open Ast
open Util

type env = (string * ty) list

open ReaderResult(struct type t = env type e = string end)
open Monad
type 'a checker = 'a t

let lookup x : ty checker =
  let* e = ask in
  match List.assoc_opt x e with
  | Some t -> return (t)
  | None -> error @@ "variable `" ^ x ^ "` does not exist"

let add_env x t e : env =
  ((x, t) :: e)

(*
type ty =
  | TyVar of int
  | TyParam of string
  | TyName of string
  | TyApp of ty * ty
  | TyFunc of ty * ty
  | TyForall of string list * ty
*)

let rec unify t t' : unit checker =
  match t, t' with
  | TyName "unit", TyName "unit" -> return ()
  | TyFunc (a, r), TyFunc (a', r') -> unify a a' >> unify r r'
  | _ -> error "incompatible or unimplemented types"

let destruct_type_func t : (ty * ty) checker =
  match t with
  | TyFunc (a, r) -> return (a, r)
  | _ -> error "expected a function"

let rec type_expr e : ty checker =
  match e with
  | EId "tt" -> return @@ TyName "unit"
  | EId x -> lookup x
  | ELam (x, Some t, e) ->
    let* e' = ask in
    let* r = local (return @@ add_env x t e') begin
      type_expr e
    end in
    return @@ TyFunc (t, r)
  | ELam (_, _, _) -> failwith "type inference unimplemented"
  | EApp (f, a) ->
    let* f' = type_expr f in
    let* a' = type_expr a in
    let* (a'', r) = destruct_type_func f' in
    let* () = unify a' a'' in
    return r
  | EAnn (e, t) ->
    let* t' = type_expr e in
    let* () = unify t' t in
    return t
  | _ -> failwith "unimplemented"

let check a : (ty list, string) result =
  let f xs a =
    match a with
    | AExpr e ->
      let* e' = type_expr e in
      return (e' :: xs)
    | ATypeDef _ -> return xs
  in foldM f [] a []
