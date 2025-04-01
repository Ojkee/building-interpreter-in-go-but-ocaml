open Ast

type data_obj =
  | IntegerObj of int
  | StringObj of string
  | BooleanObj of bool
  | NullObj
  | ReturnValueObj of data_obj
  | ErrorObj of string
  | FunctionObj of expression list * statement list * enviroment
  | PLACEHOLDER_OBJ

and enviroment = {
  store : (string, data_obj) Hashtbl.t;
  outer : enviroment option;
}

val new_enviroment : unit -> enviroment
val enclose_enviroment : enviroment -> enviroment
val get_from_env : enviroment -> string -> data_obj option
val string_of_object : data_obj -> string
val string_of_object_repl : data_obj -> string
val string_of_object_deb : data_obj -> string
val type_string_of_object : data_obj -> string
val unpack_return_object : data_obj -> data_obj
