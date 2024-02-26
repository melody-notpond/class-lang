open Ast
open Util

type value =
  | VUnit
  | VLam of string * env * expr
and env = (string * value) list

type 'a interper = env -> ('a, string) result

module InterpMonadInstance = struct
  type 'a t = env -> ('a, string) result

  let (>>=) g f =
    fun e ->
      match g e with
      | Ok v -> f v e
      | Error s -> Error s

  let return x = fun _ -> Ok x
end

open Monad(InterpMonadInstance)

let error s : 'a interper =
  fun _ -> Error s

let ask : env interper = fun e -> Ok e
let local f m =
  fun e ->
  match f e with
  | Ok e' -> m e'
  | Error s -> Error s
 
let lookup x : value interper =
  let* e = ask in
  match List.assoc_opt x e with
  | Some v -> return v
  | None -> error @@ "variable `" ^ x ^ "` does not exist"

let add_env x v e : env =
  ((x, v) :: e)

let func_of_value v : (string * env * expr) interper =
  match v with
  | VLam (x, env, e) -> return (x, env, e)
  | _ -> error "expected function"

let rec eval_expr e : value interper =
  match e with
  | EId x ->
    if x = "tt" then
      return VUnit
    else
      lookup x
  | ELet {bind = _; bound = _; result = _} -> failwith "unimplemented"
  | ELetRec {bind = _; bound = _; and_binds = _; result = _} -> failwith "unimplemented"
  | EMatch (_e, _branches) -> failwith "unimplemented"
  | ELam (x, _t, e) ->
    let* env = ask in
    return @@ VLam (x, env, e)
  | EApp (f, e) ->
    let* f' = eval_expr f in
    let* (x, env, e') = func_of_value f' in
    let* v = eval_expr e in
    local (return @@ add_env x v env) begin
      eval_expr e'
    end
  | EAnn (e, _t) -> eval_expr e

let interp (a: ast list) : (value list, string) result =
  let condense xs x =
  match x with
  | AExpr e ->
    let* v = eval_expr e in
    return (v :: xs)
  | ATypeDef _ -> failwith "unimplemented"
  in foldM condense [] a []
