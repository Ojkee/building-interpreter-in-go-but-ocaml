type data_obj =
  | IntegerObj of int
  | BooleanObj of bool
  | NullObj
  | ReturnValueObj of data_obj
  | PLACEHOLDER_OBJ

let rec string_of_object = function
  | IntegerObj x -> string_of_int x
  | BooleanObj x -> string_of_bool x
  | NullObj -> "NULL"
  | ReturnValueObj x -> string_of_object x
  | PLACEHOLDER_OBJ -> "PLACEHOLDER_OBJ"

let rec string_of_object_deb = function
  | IntegerObj x -> "obj(" ^ string_of_int x ^ ")"
  | BooleanObj x -> "obj(" ^ string_of_bool x ^ ")"
  | NullObj -> "NULL"
  | ReturnValueObj x -> "ret(" ^ string_of_object_deb x ^ ")"
  | PLACEHOLDER_OBJ -> "PLACEHOLDER_OBJ"
