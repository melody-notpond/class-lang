open Util

type token =
  | TEof
  | TSemicolon
  | TParenL
  | TParenR
  | TComma
  | TRightArrow
  | TBar
  | TEq
  | TBackslash
  | TDot
  | TColon
  | TUnderscore
  | TForall
  | TLet
  | TRec
  | TAnd
  | TIn
  | TMatch
  | TWith
  | TEnd
  | TData
  | TSym of string

type parser_state = {s: string; pos: int}
type 'a parser = parser_state -> (('a * parser_state), string) result

module ParserMonadInst = struct
  type 'a t = 'a parser

  let (>>=) x f =
    fun p ->
      match x p with
      | Ok (v, p') -> f v p'
      | Error s -> Error s

  let return x = fun p -> Ok (x, p)
end

let error s : 'a parser = fun _ -> Error s
let get: parser_state parser = fun p -> Ok (p, p)
let set p' : unit parser = fun _ -> Ok ((), p')

open Monad(ParserMonadInst)

let eof: bool parser =
  let* p = get in
  return (p.pos >= String.length p.s)

let next_char: char parser =
  let* p = get in
  if String.length p.s > p.pos then
    let* () = set {p with pos = p.pos + 1} in
    return p.s.[p.pos]
  else
    error "expected next character"

let backtrack_char: unit parser =
  let* p = get in
  if p.pos > 0 then
    set {p with pos = p.pos - 1}
  else
    return ()

type lex_state =
  | SInit

let lex: token parser =
  let rec helper acc state =
    let* stop = eof in
    if stop then
      return acc
    else let* c = next_char in
      match state with
      | SInit -> begin
        match c with
        | ' ' | '\t' | '\n' | '\r' ->
          if String.length acc = 0 then
            helper acc state
          else
            backtrack_char >> return acc
        | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> helper (acc ^ String.make 1 c) state
        | _ when String.length acc > 0 ->
          backtrack_char >> return acc
        | _ when String.contains "(),|=\\.:" c -> return @@ String.make 1 c
        | '-' ->
          let* c = next_char in
          if c = '>' then
            return "->"
          else
            error "expected ->"
        | _ -> error @@ "invalid character" ^ String.escaped @@ String.make 1 c
      end
  in let* s = helper "" SInit in return @@
  match s with
  | "(" -> TParenL
  | ")" -> TParenR
  | "," -> TComma
  | "->" -> TRightArrow
  | "|" -> TBar
  | "=" -> TEq
  | "\\" -> TBackslash
  | "." -> TDot
  | ":" -> TColon
  | "_" -> TUnderscore
  | "forall" -> TForall
  | "let" -> TLet
  | "rec" -> TRec
  | "and" -> TAnd
  | "in" -> TIn
  | "match" -> TMatch
  | "with" -> TWith
  | "end" -> TEnd
  | "data" -> TData
  | "" -> TEof
  | s -> TSym s

let consume_exact (t: token) : unit parser =
  let* t' = lex in
  if t' = t then
    return ()
  else
    error "did not get expected token"

let consume_sym : string parser =
  let* t = lex in
  match t with
  | TSym s -> return s
  | _ -> error "expected symbol"

let (|?) (p: 'a parser) (p': 'a parser) : 'a parser =
  fun s ->
    match p s with
    | Ok v -> Ok v
    | Error _ -> p' s

let optional (p: 'a parser) : 'a option parser =
  fun s ->
    match p s with
    | Ok (v, s') -> Ok (Some v, s')
    | Error _ -> Ok (None, s)

let rec many_zero (p: 'a parser) : 'a list parser =
  let* x = optional p in
  match x with
  | Some v ->
    let* vs = many_zero p in
    return @@ v :: vs
  | None -> return []

let many_one (p: 'a parser) : 'a list parser =
  let* x = p in
  let* xs = many_zero p in
  return @@ x :: xs

open Ast

let rec parse_pat: pat parser =
  fun s -> s |> begin
    parse_or |?
    parse_subpat
  end
and parse_subpat =
  fun s -> s |> begin
    parse_wild |?
    parse_ident |?
    parse_paren_pat
  end
and parse_wild =
  let* () = consume_exact TUnderscore in
  return PWild
and parse_ident =
  fun s -> s |> begin
    let* s = consume_sym in
    let* xs = many_zero parse_subpat in
    return @@ PIdent (s, xs)
  end
and parse_or = 
  fun s -> s |> begin
    let* p = parse_subpat in
    let* () = consume_exact TBar in
    let* p' = parse_pat in
    return @@ POr (p, p')
  end
and parse_paren_pat =
  fun s -> s |> begin
    let* () = consume_exact TParenL in
    let* p = parse_pat in
    let* () = consume_exact TParenR in
    return p
  end

let rec parse_type: ty parser =
  fun s -> s |> begin
    parse_forall |?
    parse_nonquant_type
  end
and parse_forall =
  fun s -> s |> begin
    let* () = consume_exact TForall in
    let* a = many_one consume_sym in
    let* () = consume_exact TComma in
    let* t = parse_nonquant_type in
      return @@ TyForall (a, t)
  end
and parse_nonquant_type =
  fun s -> s |> begin
    parse_type_func |?
    parse_nonquant_sub
  end
and parse_nonquant_sub =
  fun s -> s |> begin
    parse_type_app |?
    parse_paren_type
  end
and parse_type_name =
  let* n = consume_sym in
  return @@ TyName n
and parse_type_app =
  fun s -> s |> begin
    let* f = parse_type_name in
    let* args = many_zero (parse_type_name |? parse_paren_type) in
    return @@ List.fold_left (fun a b -> TyApp (a, b)) f args
  end
and parse_type_func =
  fun s -> s |> begin
    let* a = parse_nonquant_sub in
    let* () = consume_exact TRightArrow in
    let* r = parse_nonquant_type in
    return @@ TyFunc (a, r)
  end
and parse_paren_type =
  fun s -> s |> begin
    let* () = consume_exact TParenL in
    let* t = parse_type in
    let* () = consume_exact TParenR in
    return t
  end

let parse_let_pat: let_bind parser =
    let* () = consume_exact TParenL in
    let* p = parse_pat in
    let* t = optional (consume_exact TColon >> parse_type) in
    let* () = consume_exact TParenR in
    return @@ LPat (p, t)

let parse_let_func: let_bind parser =
  let* f = consume_sym in
  let* args = many_zero begin
    (consume_sym >>= (fun v -> return (PIdent (v, []), None))) |?
    (parse_let_pat >>= function
        | (LPat (v, t)) -> return (v, t)
        | _ -> error "impossible")
  end in
  let* t = optional (consume_exact TColon >> parse_type) in
  return @@ LFunc (f, args, t)

let parse_let_bind: let_bind parser = parse_let_pat |? parse_let_func

let rec parse_expr: expr parser =
  fun s -> s |> begin
    let* e = parse_app |?
      parse_let |?
      parse_rec |?
      parse_match |?
      parse_lambda |?
      parse_paren_expr
    in let* t = optional (consume_exact TColon >> parse_type) in
    match t with
    | Some t -> return @@ EAnn (e, t)
    | None -> return e
  end
and parse_ident =
  let* s = consume_sym in
  return @@ EId s
and parse_let =
  fun s -> s |> begin
    let* () = consume_exact TLet in
    let* bind = parse_let_bind in
    let* () = consume_exact TEq in
    let* bound = parse_expr in
    let* result = optional (consume_exact TIn >> parse_expr) in
    return @@ ELet {bind; bound; result}
  end
and parse_rec =
  fun s -> s |> begin
    let* () = consume_exact TLet in
    let* () = consume_exact TRec in
    let* bind = parse_let_bind in
    let* () = consume_exact TEq in
    let* bound = parse_expr in
    let* and_binds = many_zero begin
      let* () = consume_exact TAnd in
      let* p = parse_let_bind in
      let* () = consume_exact TEq in
      let* e = parse_expr in
      return (p, e)
    end in
    let* result = optional (consume_exact TIn >> parse_expr) in
    return @@ ELetRec {bind; bound; and_binds; result}
  end
and parse_match =
  fun s -> s |> begin
    let* () = consume_exact TMatch in
    let* e = parse_expr in
    let* () = consume_exact TWith in
    let* branches = many_one begin
      let* () = consume_exact TBar in
      let* p = parse_pat in
      let* () = consume_exact TRightArrow in
      let* e = parse_expr in
      return (p, e)
    end in
    let* () = consume_exact TEnd in
    return @@ EMatch (e, branches)
  end
and parse_lambda =
  fun s -> s |> begin
    let* () = consume_exact TBackslash in
    let* x = consume_sym in
    let* t = optional (consume_exact TColon >> parse_type) in
    let* () = consume_exact TDot in
    let* e = parse_expr in
    return @@ ELam (x, t, e)
  end
and parse_app =
  fun s -> s |> begin
    let* f = parse_ident |? parse_paren_expr in
    let* args = many_zero (parse_ident |? parse_paren_expr) in
    return @@ List.fold_left (fun a b -> EApp (a, b)) f args
  end
and parse_paren_expr =
  fun s -> s |> begin
    let* () = consume_exact TParenL in
    let* e = parse_expr in
    let* () = consume_exact TParenR in
    return e
  end

let parse_type_def: type_def parser =
  let* () = consume_exact TData in
  let* name = consume_sym in
  let* params = many_zero consume_sym in
  let* () = consume_exact TEq in
  let* variants = many_zero begin
    let* () = consume_exact TBar in
    let* constr = consume_sym in
    let* () = consume_exact TColon in
    let* t = parse_type in
    return (constr, t)
  end in
  return {name; params; variants}

let parse_top: ast parser =
  begin
    let* v = parse_expr in
    return @@ AExpr v
  end |? begin
    let* v = parse_type_def in
    return @@ ATypeDef v
  end

let parse (s: string) : (ast list, string) result =
  match begin
    let* all = many_zero parse_top in
    let* () = consume_exact TEof in
    return all
  end {s; pos = 0} with
  | Ok (v, _) -> Ok v
  | Error s -> Error s

(*
Expr := Ident
      | let LetBind = Expr in Expr
      | let rec LetBind = Expr (and LetBind = Expr)* in Expr
      | match Expr with ("|" Pat -> Expr)+ end
      | "\\" ident . Expr
      | Expr Expr
      | "(" Expr ")"

LetBind := Ident Pat*
         | Pat

Pat := Ident Pat*
     | "_"
     | (Pat)
     | Pat "|" Pat

TypeDef := data Ident Ident* = ("|" Ident ":" Type)*

Top := TypeDef | Expr

Ast := Top*
*)
