type value =
  | VUnit
  | VLam of string * env * Ast.expr
and env = (string * value) list

val interp : Ast.ast list -> (value list, string) result
