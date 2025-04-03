open Ast

type data_obj =
  | IntegerObj of int
  | StringObj of string
  | BooleanObj of bool
  | ArrayObj of data_obj list
  | NullObj
  | ReturnValueObj of data_obj
  | ErrorObj of string
  | FunctionObj of expression list * statement list * enviroment
  | BuiltinFnObj of builtinFn
  | PLACEHOLDER_OBJ

and enviroment = {
  store : (string, data_obj) Hashtbl.t;
  outer : enviroment option;
}

and builtinFn = data_obj list -> data_obj

val new_enviroment : unit -> enviroment
val enclose_enviroment : enviroment -> enviroment
val get_from_env : enviroment -> string -> data_obj option
val new_error : ('a, unit, string, data_obj) format4 -> 'a
val string_of_object : data_obj -> string
val string_of_object_repl : data_obj -> string
val string_of_object_deb : data_obj -> string
val type_string_of_object : data_obj -> string
val unpack_return_object : data_obj -> data_obj
