open Ast
open Interp
open Parse
open Typecheck

open Util.Result(struct type t = string end)
open Monad

(*
type ty =
  | TyVar of int
  | TyParam of string
  | TyName of string
  | TyApp of ty * ty
  | TyFunc of ty * ty
  | TyForall of string list * ty
*)
let rec string_of_type t =
  match t with
  | TyVar i -> "$" ^ string_of_int i
  | TyParam a -> a
  | TyName n -> n
  | TyApp (f, a) -> "(" ^ string_of_type f ^ " " ^ string_of_type a ^ ")"
  | TyFunc (a, r) -> "(" ^ string_of_type a ^ " -> " ^ string_of_type r ^ ")"
  | TyForall (xs, t) -> "(forall " ^ String.concat " " xs ^ ", " ^ string_of_type t ^ ")"

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

and string_of_env e =
  let s = List.map (fun (s, v) -> s ^ " = " ^ string_of_value v) e in
  "{" ^ String.concat "; " s ^ "}"
and string_of_value v =
  match v with
  | VUnit -> "()"
  | VLam (x, env, e) ->
    "(\\" ^ x ^ ". " ^ string_of_expr e ^ ")@" ^ string_of_env env

let typecheck s : (ty list, string) result =
  let* a = parse s in
  check a

let typecheck_string s : (string list, string) result =
  match typecheck s with
  | Ok t -> Ok (List.map string_of_type t)
  | Error e -> Error e

let run s : (value list, string) result =
  let* a = parse s in
  let* _ = check a in
  interp a

let run_string s : (string list, string) result =
  match run s with
  | Ok v -> Ok (List.map string_of_value v)
  | Error e -> Error e

