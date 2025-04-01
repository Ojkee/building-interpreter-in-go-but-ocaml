open Ast

type data_obj =
  | IntegerObj of int
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

let new_enviroment () : enviroment = { store = Hashtbl.create 10; outer = None }

let enclose_enviroment (outer_env : enviroment) : enviroment =
  { store = Hashtbl.create 10; outer = Some outer_env }

let rec get_from_env (env : enviroment) (key : string) : data_obj option =
  match (Hashtbl.find_opt env.store key, env.outer) with
  | (Some _ as obj), _ -> obj
  | None, Some outer -> get_from_env outer key
  | None, None -> None

let rec string_of_object = function
  | IntegerObj x -> string_of_int x
  | BooleanObj x -> string_of_bool x
  | NullObj -> "NULL"
  | ReturnValueObj x -> string_of_object x
  | ErrorObj x -> "Err:\t" ^ x
  | FunctionObj (idents, stmts, _) ->
      "fn("
      ^ (List.map string_of_expression idents |> String.concat ", ")
      ^ ") {\n" ^ string_of_statements stmts ^ "\n}"
  | PLACEHOLDER_OBJ -> "PLACEHOLDER_OBJ"

let string_of_object_repl = function
  | IntegerObj x -> string_of_int x
  | BooleanObj x -> string_of_bool x
  | NullObj -> "NULL"
  | ReturnValueObj x -> string_of_object x
  | ErrorObj x -> "Err:\t" ^ x
  | _ -> ""

let rec string_of_object_deb = function
  | IntegerObj x -> "obj(" ^ string_of_int x ^ ")"
  | BooleanObj x -> "obj(" ^ string_of_bool x ^ ")"
  | NullObj -> "NULL"
  | ReturnValueObj x -> "ret(" ^ string_of_object_deb x ^ ")"
  | ErrorObj x -> "err(" ^ x ^ ")"
  | FunctionObj (idents, stmts, _) ->
      "function(fn("
      ^ (List.map string_of_expression idents |> String.concat ", ")
      ^ ") {\n" ^ string_of_statements stmts ^ "\n})"
  | PLACEHOLDER_OBJ -> "PLACEHOLDER_OBJ"

let rec type_string_of_object = function
  | IntegerObj _ -> "INTEGER"
  | BooleanObj _ -> "BOOLEAN"
  | NullObj -> "NULL"
  | ReturnValueObj x -> "(RETURN (" ^ type_string_of_object x ^ ")"
  | ErrorObj x -> "ERROR(" ^ x ^ ")"
  | FunctionObj _ -> "FUNCTION"
  | PLACEHOLDER_OBJ -> "PLACEHOLDER"

let unpack_return_object = function
  | ReturnValueObj x -> x
  | not_ret_obj -> not_ret_obj
