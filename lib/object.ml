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

let new_enviroment () : enviroment = { store = Hashtbl.create 10; outer = None }

let enclose_enviroment (outer_env : enviroment) : enviroment =
  { store = Hashtbl.create 10; outer = Some outer_env }

let rec get_from_env (env : enviroment) (key : string) : data_obj option =
  match (Hashtbl.find_opt env.store key, env.outer) with
  | (Some _ as obj), _ -> obj
  | None, Some outer -> get_from_env outer key
  | None, None -> None

let new_error fmt = Printf.ksprintf (fun msg -> ErrorObj msg) fmt

let rec string_of_object = function
  | IntegerObj x -> string_of_int x
  | StringObj x -> "\"" ^ x ^ "\""
  | BooleanObj x -> string_of_bool x
  | ArrayObj elements ->
      elements |> List.map string_of_object |> String.concat ", " |> fun x ->
      "[" ^ x ^ "]"
  | NullObj -> "NULL"
  | ReturnValueObj x -> string_of_object x
  | ErrorObj x -> "Err:\t" ^ x
  | FunctionObj (idents, stmts, _) ->
      "fn("
      ^ (List.map string_of_expression idents |> String.concat ", ")
      ^ ") {\n" ^ string_of_statements stmts ^ "\n}"
  | BuiltinFnObj _ -> "builtin function"
  | PLACEHOLDER_OBJ -> "PLACEHOLDER_OBJ"

let string_of_object_repl = function
  | IntegerObj x -> string_of_int x
  | StringObj x -> x
  | BooleanObj x -> string_of_bool x
  | ArrayObj _ as obj -> string_of_object obj
  | NullObj -> "NULL"
  | ReturnValueObj x -> string_of_object x
  | ErrorObj x -> "Err:\t" ^ x
  | _ -> ""

let rec string_of_object_deb = function
  | IntegerObj x -> "obj(" ^ string_of_int x ^ ")"
  | StringObj _ as str -> "str(" ^ string_of_object str ^ ")"
  | BooleanObj x -> "obj(" ^ string_of_bool x ^ ")"
  | ArrayObj elements ->
      elements |> List.map string_of_object |> String.concat ", " |> fun x ->
      "ary([" ^ x ^ "])"
  | NullObj -> "NULL"
  | ReturnValueObj x -> "ret(" ^ string_of_object_deb x ^ ")"
  | ErrorObj x -> "err(" ^ x ^ ")"
  | FunctionObj (idents, stmts, _) ->
      "function(fn("
      ^ (List.map string_of_expression idents |> String.concat ", ")
      ^ ") {\n" ^ string_of_statements stmts ^ "\n})"
  | BuiltinFnObj _ -> "builtinFn"
  | PLACEHOLDER_OBJ -> "PLACEHOLDER_OBJ"

let rec type_string_of_object = function
  | IntegerObj _ -> "INTEGER"
  | StringObj _ -> "STRING"
  | BooleanObj _ -> "BOOLEAN"
  | ArrayObj _ -> "ARRAY"
  | NullObj -> "NULL"
  | ReturnValueObj x -> "(RETURN (" ^ type_string_of_object x ^ ")"
  | ErrorObj x -> "ERROR(" ^ x ^ ")"
  | FunctionObj _ -> "FUNCTION"
  | BuiltinFnObj _ -> "BUILTIN_FUNCTION"
  | PLACEHOLDER_OBJ -> "PLACEHOLDER"

let unpack_return_object = function
  | ReturnValueObj x -> x
  | not_ret_obj -> not_ret_obj
