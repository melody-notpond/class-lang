open Ast
open Util

type value =
  | VAdtInst of string * value list * ty
  | VLam of string * env * expr
and env = (string * value) list

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

let rec eval_expr e : value interper =
  match e with
  | EId x -> lookup x
  | ELet {bind = _; bound = _; result = _} -> failwith "unimplemented"
  | ELetRec {bind = _; bound = _; and_binds = _; result = _} -> failwith "unimplemented"

  | EMatch (e, branches) ->
    let* v = eval_expr e in
    let* (name, args, t) = adt_of_value v in
    if type_is_func t then
      error "cannot pattern match on partial constructor application"
    else let rec find_branch branches : value interper =
      match branches with
      | [] -> error "unsatisfied pattern"
      | (p, e) :: xs ->
        let rec pat_matches p v : env option interper =
          match p with
          | PWild -> return (Some [])
          | PIdent (x, []) ->
            let* env = ask in begin
            match List.assoc_opt x env with
            | Some (VAdtInst (n, [], t)) when n = x ->
              if n = name then
                return (Some [])
              else
                return (Some [(x, v)])
            | _ -> return (Some [(x, v)])
            end
          | PIdent (cons, ps) ->
            if cons = name then
              let rec helper ps args =
                match ps, args with
                | p :: ps, x :: xs ->
                  let ()
          | POr (p1, p2) ->
            return @@
            let (b, env) = pat_matches p1 v in
            if b then
              return (b, env)
            else
              pat_matches p2 v
        in
        let* (matches, env) = pat_matches p v in
        if matches then
          local (return @@ env) begin
            eval_expr e
          end
        else
          find_branch xs
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
