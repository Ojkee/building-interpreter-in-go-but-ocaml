type data_obj =
  | IntegerObj of int
  | BooleanObj of bool
  | NullObj
  | ReturnValueObj of data_obj
  | ErrorObj of string
  | PLACEHOLDER_OBJ

let rec string_of_object = function
  | IntegerObj x -> string_of_int x
  | BooleanObj x -> string_of_bool x
  | NullObj -> "NULL"
  | ReturnValueObj x -> string_of_object x
  | ErrorObj x -> "Err:\t" ^ x
  | PLACEHOLDER_OBJ -> "PLACEHOLDER_OBJ"

let rec string_of_object_deb = function
  | IntegerObj x -> "obj(" ^ string_of_int x ^ ")"
  | BooleanObj x -> "obj(" ^ string_of_bool x ^ ")"
  | NullObj -> "NULL"
  | ReturnValueObj x -> "ret(" ^ string_of_object_deb x ^ ")"
  | ErrorObj x -> "err(" ^ x ^ ")"
  | PLACEHOLDER_OBJ -> "PLACEHOLDER_OBJ"

let rec type_string_of_object = function
  | IntegerObj _ -> "INTEGER"
  | BooleanObj _ -> "BOOLEAN"
  | NullObj -> "NULL"
  | ReturnValueObj x -> "(RETURN (" ^ type_string_of_object x ^ ")"
  | ErrorObj x -> "ERROR(" ^ x ^ ")"
  | PLACEHOLDER_OBJ -> "PLACEHOLDER"

let unpack_return_object = function
  | ReturnValueObj x -> x
  | not_ret_obj -> not_ret_obj
