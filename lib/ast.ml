type ty =
  | TyVar of int
  | TyParam of string
  | TyName of string
  | TyApp of ty * ty
  | TyFunc of ty * ty
  | TyForall of string * ty

type pat =
  | PWild
  | PIdent of string
  | POr of pat * pat

type let_bind =
  | LFunc of string * (pat * ty option) list
  | LPat of pat * ty option

type expr =
  | AId of string
  | ALet of {bind: let_bind; bound: expr; result: expr option}
  | ALetRec of {bind: let_bind; bound: expr; and_binds: (let_bind * expr) list; result: expr option}
  | AMatch of expr * (pat * expr) list
  | ALam of string * expr
  | AApp of expr * expr
  | AAnn of expr * ty

type type_def = TyDef of {
  name: string;
  params: string list;
  variants: (string * ty) list
}
