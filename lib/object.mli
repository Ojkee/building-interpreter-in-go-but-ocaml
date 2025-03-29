type data_obj =
  | IntegerObj of int
  | BooleanObj of bool
  | NullObj
  | ReturnValueObj of data_obj
  | PLACEHOLDER_OBJ

val string_of_object : data_obj -> string
val string_of_object_deb : data_obj -> string
