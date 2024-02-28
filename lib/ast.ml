type ty =
  | TyVar of int
  | TyParam of string
  | TyName of string
  | TyApp of ty * ty
  | TyFunc of ty * ty
  | TyForall of string list * ty

type pat =
  | PWild
  | PIdent of string * pat list
  | POr of pat * pat

type let_bind =
  | LFunc of string * (pat * ty option) list * ty option
  | LPat of pat * ty option

type expr =
  | EId of string
  | ELet of {bind: let_bind; bound: expr; result: expr option}
  | ELetRec of {bind: let_bind; bound: expr; and_binds: (let_bind * expr) list; result: expr option}
  | EMatch of expr * (pat * expr) list
  | ELam of string * ty option * expr
  | EApp of expr * expr
  | EAnn of expr * ty

type type_def = {
  name: string;
  params: string list;
  variants: (string * ty) list
}

type ast =
  | AExpr of expr
  | ATypeDef of type_def
