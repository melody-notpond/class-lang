open Ast
open Util

type value =
  | VAdtInst of string * value list * ty
  | VLam of string * env * expr
and env = (string * value) list

(*
let rec string_of_type t =
  match t with
  | TyVar i -> "$" ^ string_of_int i
  | TyParam a -> a
  | TyName n -> n
  | TyApp (f, a) -> "(" ^ string_of_type f ^ " " ^ string_of_type a ^ ")"
  | TyFunc (a, r) -> "(" ^ string_of_type a ^ " -> " ^ string_of_type r ^ ")"
  | TyForall (xs, t) -> "(forall " ^ String.concat " " xs ^ ", " ^ string_of_type t ^ ")"
*)

let rec string_of_let_bind _b = ""
and string_of_expr e =
  match e with
  | EId x -> x
  | ELet {bind; bound; result} ->
    "let " ^ string_of_let_bind bind ^ " = " ^ string_of_expr bound ^ begin
      match result with
      | Some v -> " in " ^ string_of_expr v
      | None -> ""
    end
  | ELetRec {bind; bound; and_binds; result} ->
    "let rec " ^ string_of_let_bind bind ^ " = " ^ string_of_expr bound ^
    (if List.for_all (fun _ -> false) and_binds then "" else " and ") ^
    String.concat " and " (List.map (fun (b, e) -> string_of_let_bind b ^ " = " ^ string_of_expr e) and_binds) ^ begin
      match result with
      | Some v -> " in " ^ string_of_expr v
      | None -> ""
    end
  | EMatch (_e, _branches) -> "match"
  | ELam (x, _, e) -> "(\\" ^ x ^ ". " ^ string_of_expr e ^ ")"
  | EApp (f, e) -> "(" ^ string_of_expr f ^ " " ^ string_of_expr e ^ ")"
  | EAnn (e, _) -> string_of_expr e

(*
and string_of_env e =
  let s = List.map (fun (s, v) -> s ^ " = " ^ string_of_value v) e in
  "{" ^ String.concat "; " s ^ "}"
and string_of_value v =
  match v with
  | VAdtInst (n, a, _) -> "(" ^ n ^ " " ^ String.concat " " (List.map string_of_value a) ^ ")"
  | VLam (x, env, e) ->
    "(\\" ^ x ^ ". " ^ string_of_expr e ^ ")@" ^ string_of_env env
*)

open ReaderResult(struct type t = env type e = string end)
open Monad
type 'a interper = 'a t

let lookup x : value interper =
  let* e = ask in
  match List.assoc_opt x e with
  | Some v -> return v
  | None -> error @@ "variable `" ^ x ^ "` does not exist"

let add_env x v e : env = (x, v) :: e
let add_env_list xs e : env = xs @ e

let destruct_type_func t : (ty * ty) interper =
  match t with
  | TyFunc (a, r) -> return (a, r)
  | _ -> error "expected a function"

let type_is_func t : bool =
  match t with
  | TyFunc _ -> true
  | _ -> false

let adt_of_value v : (string * value list * ty) interper=
  match v with
  | VAdtInst (name, args, t) -> return (name, args, t)
  | _ -> error "expected adt"

let (|?) x y : 'a option interper =
  fun e ->
  match x e with
  | Ok (Some _ as v) -> Ok v
  | Ok None -> y e
  | Error e -> Error e

let rec eval_expr e : value interper =
  match e with
  | EId x -> lookup x
  | ELet {bind = _; bound = _; result = _} -> failwith "unimplemented"
  | ELetRec {bind = _; bound = _; and_binds = _; result = _} -> failwith "unimplemented"
  | EMatch (e, branches) ->
    let* v = eval_expr e in
    let rec find_branch branches : value interper =
      match branches with
      | [] -> error "unsatisfied pattern"
      | (p, e) :: xs ->
        let* (name, args, t) = adt_of_value v in
        if type_is_func t then
          error "cannot pattern match on partial constructor application"
        else let rec pat_matches p v : env option interper =
          match p with
          | PWild -> return (Some [])
          | PIdent (x, []) ->
            let* env = ask in begin
            match List.assoc_opt x env with
            | Some (VAdtInst (n, [], _)) when n = x ->
              if n = name then
                return (Some [])
              else
                return None
            | _ -> return (Some [(x, v)])
            end
          | PIdent (cons, ps) ->
            if cons = name then
              let rec helper ps args : env option interper =
                match ps, args with
                | p :: ps, x :: xs ->
                  let* env = pat_matches p x in
                  let* env' = helper ps xs in
                  return @@
                  let open! Option.Monad in
                  let* env'' = env in
                  let* env''' = env' in
                  return @@ env'' @ env'''
                | [], [] -> return @@ Some []
                | _, _ -> return None
              in helper ps args
            else
              error "invalid variant"
          | POr (p1, p2) ->
            pat_matches p1 v |? pat_matches p2 v
        in
        let* branch = pat_matches p v in
        match branch with
        | Some env' ->
          let* env = ask in
          local (return @@ add_env_list env' env) begin
            eval_expr e
          end
        | None -> find_branch xs
    in find_branch branches
  | ELam (x, _t, e) ->
    let* env = ask in
    return @@ VLam (x, env, e)
  | EApp (f, e) ->
    let* f' = eval_expr f in begin
      match f' with
      | VLam (x, env, e') ->
        let* v = eval_expr e in
        local (return @@ add_env x v env) begin
          eval_expr e'
        end
      | VAdtInst (n, xs, t) ->
        let* (_, r) = destruct_type_func t in
        let* v = eval_expr e in
        return @@ VAdtInst (n, xs @ [v], r)
    end
  | EAnn (e, _t) -> eval_expr e

let interp (a: ast list) : (value list, string) result =
  let rec f a =
    match a with
    | [] -> return []
    | AExpr e :: xs ->
      let* v = eval_expr e in
      let* vs = f xs in
      return (v :: vs)
    | ATypeDef {variants; _} :: xs ->
      let* e = ask in
      let vars = List.map (fun (n, t) -> (n, VAdtInst (n, [], t))) variants in
      local (return @@ add_env_list vars e) begin
        f xs
      end
  in f a [("tt", VAdtInst ("tt", [], TyName "unit"))]
