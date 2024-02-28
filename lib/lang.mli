val run : string -> (Interp.value list, string) result
val string_of_expr : Ast.expr -> string
val string_of_env : Interp.env -> string
val string_of_value : Interp.value -> string
