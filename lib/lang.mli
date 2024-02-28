val string_of_expr : Ast.expr -> string
val string_of_env : Interp.env -> string
val string_of_value : Interp.value -> string
val string_of_type : Ast.ty -> string

val typecheck : string -> (Ast.ty list, string) result
val typecheck_string : string -> (string list, string) result
val run : string -> (Interp.value list, string) result
val run_string : string -> (string list, string) result
