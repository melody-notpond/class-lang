type value =
  | VAdtInst of string * value list * Ast.ty
  | VLam of string * env * Ast.expr
and env = (string * value) list

val interp : Ast.ast list -> (value list, string) result
