type data_obj = IntegerObj of int | BooleanObj of bool | NullObj

let string_of_object = function
  | IntegerObj x -> string_of_int x
  | BooleanObj x -> string_of_bool x
  | NullObj -> "NULL"
