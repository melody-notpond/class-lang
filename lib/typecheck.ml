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

let add_env x t e : env = (x, t) :: e
let add_env_list xs e : env = xs @ e
let _add_env_lists xs ts e : env = List.combine xs ts @ e

let rec unify t t' : unit checker =
  match t, t' with
  | TyName s, TyName s' ->
    if s = s' then
      return ()
    else
      error @@ "incompatible types " ^ s ^ " and " ^ s'
  | TyFunc (a, r), TyFunc (a', r') -> unify a a' >> unify r r'
  | _ -> error "incompatible or unimplemented types"

let rec verify_type t : unit checker =
  match t with
  | TyVar _ -> error "type variable cant be verified"
  | TyParam _ -> error "system omega has no type parameters"
  | TyName n ->
    let* t = lookup n in begin
      match t with
      | TyName "*" -> return ()
      | _ -> error "dependent types are unimplemented"
    end
  | TyApp (_, _) -> error "system omega has no applications"
  | TyFunc (a, r) -> verify_type a >> verify_type r
  | TyForall (_, _) -> error "system omega has no forall"

let destruct_type_func t : (ty * ty) checker =
  match t with
  | TyFunc (a, r) -> return (a, r)
  | _ -> error "expected a function"

let rec type_expr e : ty checker =
  match e with
  | EId x -> lookup x
  | ELam (x, Some t, e) ->
    let* () = verify_type t in
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
    let* () = verify_type t in
    let* t' = type_expr e in
    let* () = unify t' t in
    return t
  | _ -> failwith "unimplemented"

let rec verify_variants n vs : unit checker =
  match vs with
  | [] -> return ()
  | (_, vt) :: vs' ->
    let* () = verify_type vt in
    let rec get_ret t =
      match t with
      | TyFunc (_, r) -> get_ret r
      | TyApp(f, _) -> get_appf f
      | _ -> t
    and get_appf t =
      match t with
      | TyApp(f, _) -> get_appf f
      | _ -> t
    in match get_ret vt with
    | TyName n' when n' = n ->
      verify_variants n vs'
    | _ -> error "invalid variant"

let check a : (ty list, string) result =
  let rec f a =
    match a with
    | [] -> return []
    | AExpr e :: xs ->
      let* e' = type_expr e in
      let* xs' = f xs in
      return (e' :: xs')
    | ATypeDef {name; params; variants} :: xs ->
      if List.exists (fun _ -> true) params then
        error "system omega has no parametrised types"
      else let* e = ask in
      let e' = add_env name (TyName "*") e
      in local (return e') begin
        let* () = verify_variants name variants in
        local (return @@ add_env_list variants e) begin
          f xs
        end
      end
  in f a [("unit", TyName "*"); ("tt", TyName "unit")]
