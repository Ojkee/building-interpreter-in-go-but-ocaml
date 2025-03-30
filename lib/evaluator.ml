open Ast
open Lexer
open Object

type node_type = [ `Prog of program | `Stmt of statement | `Expr of expression ]

let same_type_obj (a : data_obj) (b : data_obj) : bool =
  match (a, b) with
  | IntegerObj _, IntegerObj _ | BooleanObj _, BooleanObj _ -> true
  | _, _ -> false

let new_error fmt = Printf.ksprintf (fun msg -> ErrorObj msg) fmt

let rec eval (env : enviroment) (node : node_type) : data_obj =
  match node with
  | `Prog { statements = _; errors = errs } when List.length errs > 0 ->
      String.concat "\n" errs |> print_endline;
      NullObj
  | `Prog prog -> eval_program prog env
  | `Stmt (ExpressionStatement expr) -> eval env (`Expr expr)
  | `Stmt (ReturnStatement expr) -> eval env (`Expr expr)
  | `Stmt (LetStatement { ident = Identifier (IDENT x); value = v }) -> (
      match eval env (`Expr v) with
      | ErrorObj _ as err -> err
      | obj ->
          Hashtbl.add env x obj;
          obj)
  | `Stmt stmt ->
      new_error "Unimplemented statement type in eval: %s"
        (string_of_statement stmt)
  | `Expr (IntegerLiteral (_, value)) -> IntegerObj value
  | `Expr (Boolean (_, value)) -> BooleanObj value
  | `Expr (Prefix (prefix_tok, _, value)) -> (
      match eval env (`Expr value) with
      | ErrorObj _ as err -> err
      | eval_value -> (
          match (prefix_tok, eval_value) with
          | OPERATOR BANG, BooleanObj x -> BooleanObj (not x)
          | OPERATOR BANG, NullObj -> BooleanObj true
          | OPERATOR BANG, _ -> BooleanObj false
          | OPERATOR MINUS, IntegerObj x -> IntegerObj (-x)
          | op, obj ->
              new_error "unknown operator: %s%s" (string_of_token op)
                (type_string_of_object obj)))
  | `Expr (Infix (left, op_tok, _, right)) -> (
      match (eval env (`Expr left), op_tok, eval env (`Expr right)) with
      | (ErrorObj _ as err_left), _, _ -> err_left
      | _, _, (ErrorObj _ as err_right) -> err_right
      | x, OPERATOR op, y when not (same_type_obj x y) ->
          new_error "type mismatch: %s %s %s" (type_string_of_object x)
            (string_of_operator op) (type_string_of_object y)
      | IntegerObj x, OPERATOR PLUS, IntegerObj y -> IntegerObj (x + y)
      | IntegerObj x, OPERATOR MINUS, IntegerObj y -> IntegerObj (x - y)
      | IntegerObj x, OPERATOR ASTERISK, IntegerObj y -> IntegerObj (x * y)
      | IntegerObj x, OPERATOR SLASH, IntegerObj y when y = 0 ->
          new_error "can't devide by 0 (%d / 0)" x
      | IntegerObj x, OPERATOR SLASH, IntegerObj y -> IntegerObj (x / y)
      | IntegerObj x, OPERATOR EQ, IntegerObj y -> BooleanObj (x == y)
      | IntegerObj x, OPERATOR NOT_EQ, IntegerObj y -> BooleanObj (x != y)
      | IntegerObj x, OPERATOR GT, IntegerObj y -> BooleanObj (x > y)
      | IntegerObj x, OPERATOR LT, IntegerObj y -> BooleanObj (x < y)
      | BooleanObj x, OPERATOR EQ, BooleanObj y -> BooleanObj (x == y)
      | BooleanObj x, OPERATOR NOT_EQ, BooleanObj y -> BooleanObj (x != y)
      | x, op, y ->
          new_error "unknown operator: %s %s %s" (type_string_of_object x)
            (string_of_token op) (type_string_of_object y))
  | `Expr (IfExpression (_, cond, Block (_, cons), alter_block)) -> (
      match (eval env (`Expr cond), alter_block) with
      | (ErrorObj _ as err), _ -> err
      | BooleanObj true, _ | IntegerObj _, _ -> eval_block_statements cons env
      | BooleanObj false, Some (Block (_, alter)) ->
          eval_block_statements alter env
      | _ -> NullObj)
  | `Expr (Identifier (IDENT x)) -> (
      match Hashtbl.find_opt env x with
      | Some obj -> obj
      | None -> new_error "identifier not found: %s" x)
  | `Expr (FunctionLiteral (_, idents, Block (_, stmts))) ->
      ignore (idents, stmts);
      failwith "TODO: FunctionLiteral evaluation"
  | `Expr expr ->
      new_error "Unimplemented expression type in eval: %s"
        (string_of_expression expr)

and eval_block_statements (stmts : statement list) (env : enviroment) : data_obj
    =
  match stmts with
  | [] -> NullObj
  | ReturnStatement expr :: _ -> ReturnValueObj (eval env (`Expr expr))
  | [ x ] -> eval env (`Stmt x)
  | h :: t -> (
      match eval env (`Stmt h) with
      | ReturnValueObj x -> x
      | ErrorObj _ as err_obj -> err_obj
      | _ -> eval_block_statements t env)

and eval_program (prog : program) env : data_obj =
  eval_block_statements prog.statements env

let evaluate ?(env = new_enviroment ()) (prog : program) : data_obj =
  eval env (`Prog prog) |> unpack_return_object
