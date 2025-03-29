type data_obj =
  | IntegerObj of int
  | BooleanObj of bool
  | NullObj
  | PLACEHOLDER_OBJ

val string_of_object : data_obj -> string
