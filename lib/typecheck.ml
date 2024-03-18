open Ast
open Util

type env = (string * (ty * bool)) list

open ReaderResult(struct type t = env type e = string end)
open Monad
type 'a checker = 'a t

let lookup x : (ty * bool) checker =
  let* e = ask in
  match List.assoc_opt x e with
  | Some t -> return (t)
  | None -> error @@ "variable `" ^ x ^ "` does not exist"

let add_env x t e : env = (x, t) :: e
let add_env_list xs e : env = xs @ e

let rec unify t t' : ty checker =
  match t, t' with
  | TyVar _, t | t, TyVar _ -> return t
  | TyName s as t, TyName s' ->
    if s = s' then
      return t
    else
      error @@ "incompatible types " ^ s ^ " and " ^ s'
  | TyFunc (a, r), TyFunc (a', r') ->
    let* a'' = unify a a' in
    let* r'' = unify r r' in
    return @@ TyFunc (a'', r'')
  | _ -> error "incompatible or unimplemented types"

let rec verify_type t : unit checker =
  match t with
  | TyVar _ -> error "type variable cant be verified"
  | TyParam _ -> error "system omega has no type parameters"
  | TyName n ->
    let* (t, _) = lookup n in begin
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

let rec get_arg_count t =
  match t with
  | TyFunc (_, r) -> 1 + get_arg_count r
  | _ -> 0

let rec get_ret t =
  match t with
  | TyFunc (_, r) -> get_ret r
  | _ -> t

let rec _unfold_func with_args t : ty list =
  match t with
  | TyFunc (a, r) -> a  :: _unfold_func with_args r
  | _ when with_args -> [t]
  | _ -> []

let rec type_pat p : ty checker =
  match p with
  | PWild -> return @@ TyVar 0
  | PIdent(c, ps) ->
    if List.for_all (fun _ -> false) ps then
      let* e = ask in
      match List.assoc_opt c e with
      | Some (t, true) ->
        let r = get_ret t in
        let* _ = unify r t in
        return t
      | _ -> return @@ TyVar 0
    else
      let* (t, p) = lookup c in
      if p then
        if get_arg_count t = List.length ps then
          let unify_fields t p =
            let* t' = type_pat p in
            unify t t'
          in foldM unify_fields (TyVar 0) ps
        else
          error "pattern argument count should match variant field count"
      else
        error "expected pattern"
  | POr (l, r) ->
    let* l' = type_pat l in
    let* r' = type_pat r in
    let* t = unify l' r' in
    return t

let rec type_expr e : ty checker =
  match e with
  | EId x ->
    let* (t, _) = lookup x in
    return t
  | EMatch (e, branches) ->
    let* t_val = type_expr e in
    let check_branches t (p, e) =
      let* tp = type_pat p in
      let* _ = unify t_val tp in
      let* t' = type_expr e in
      let* t'' = unify t t' in
      return t''
    in foldM check_branches (TyVar 0) branches
  | ELam (x, Some t, e) ->
    let* () = verify_type t in
    let* e' = ask in
    let* r = local (return @@ add_env x (t, false) e') begin
      type_expr e
    end in
    return @@ TyFunc (t, r)
  | ELam (_, _, _) -> failwith "type inference unimplemented"
  | EApp (f, a) ->
    let* f' = type_expr f in
    let* a' = type_expr a in
    let* (a'', r) = destruct_type_func f' in
    let* _ = unify a' a'' in
    return r
  | EAnn (e, t) ->
    let* () = verify_type t in
    let* t' = type_expr e in
    let* t'' = unify t' t in
    return t''
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
      let e' = add_env name (TyName "*", true) e
      in local (return e') begin
        let* () = verify_variants name variants in
        local (return @@ add_env_list (List.map (fun (s, t) -> (s, (t, true))) variants) e) begin
          f xs
        end
      end
  in f a [("unit", (TyName "*", false)); ("tt", (TyName "unit", true))]
