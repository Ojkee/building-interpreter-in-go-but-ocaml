open Ast
open Lexer
open Object

type node_type = [ `Prog of program | `Stmt of statement | `Expr of expression ]

let rec eval (node : node_type) : data_obj =
  match node with
  | `Prog { statements = _; errors = errs } when List.length errs > 0 ->
      String.concat "\n" errs |> print_endline;
      NullObj
  | `Prog prog -> eval_program prog
  | `Stmt (ExpressionStatement expr) -> eval (`Expr expr)
  | `Stmt (ReturnStatement expr) -> eval (`Expr expr)
  | `Stmt _ -> failwith "Unimplemented statement type in eval"
  | `Expr (IntegerLiteral (_, value)) -> IntegerObj value
  | `Expr (Boolean (_, value)) -> BooleanObj value
  | `Expr (Prefix (OPERATOR BANG, _, value)) -> (
      match eval (`Expr value) with
      | BooleanObj x -> BooleanObj (not x)
      | NullObj -> BooleanObj true
      | _ -> BooleanObj false)
  | `Expr (Prefix (OPERATOR MINUS, _, value)) -> (
      match eval (`Expr value) with
      | IntegerObj x -> IntegerObj (-x)
      | _ -> NullObj)
  | `Expr (Infix (left, op_tok, _, right)) -> (
      match (eval (`Expr left), op_tok, eval (`Expr right)) with
      | IntegerObj x, OPERATOR PLUS, IntegerObj y -> IntegerObj (x + y)
      | IntegerObj x, OPERATOR MINUS, IntegerObj y -> IntegerObj (x - y)
      | IntegerObj x, OPERATOR ASTERISK, IntegerObj y -> IntegerObj (x * y)
      | IntegerObj x, OPERATOR SLASH, IntegerObj y when y <> 0 ->
          IntegerObj (x / y)
      | IntegerObj x, OPERATOR EQ, IntegerObj y -> BooleanObj (x == y)
      | IntegerObj x, OPERATOR NOT_EQ, IntegerObj y -> BooleanObj (x != y)
      | IntegerObj x, OPERATOR GT, IntegerObj y -> BooleanObj (x > y)
      | IntegerObj x, OPERATOR LT, IntegerObj y -> BooleanObj (x < y)
      | BooleanObj x, OPERATOR EQ, BooleanObj y -> BooleanObj (x == y)
      | BooleanObj x, OPERATOR NOT_EQ, BooleanObj y -> BooleanObj (x != y)
      | _, _, _ -> NullObj)
  | `Expr (IfExpression (_, cond, Block (_, cons), alter_block)) -> (
      match (eval (`Expr cond), alter_block) with
      | BooleanObj true, _ -> eval_block_statements cons
      | IntegerObj _, _ -> eval_block_statements cons
      | BooleanObj false, Some (Block (_, alter)) -> eval_block_statements alter
      | _ -> NullObj)
  | `Expr expr ->
      failwith
        (Printf.sprintf "Unimplemented expression type in eval: %s"
           (string_of_expression expr))

and eval_block_statements (stmts : statement list) : data_obj =
  match stmts with
  | [] -> NullObj
  | ReturnStatement expr :: _ -> ReturnValueObj (eval (`Expr expr))
  | [ x ] -> eval (`Stmt x)
  | h :: t -> (
      match eval (`Stmt h) with
      | ReturnValueObj x -> x
      | _ -> eval_block_statements t)

and eval_program (prog : program) : data_obj =
  match prog with
  | { statements = []; _ } -> NullObj
  | { statements = ReturnStatement expr :: _; _ } ->
      ReturnValueObj (eval (`Expr expr))
  | { statements = [ ExpressionStatement last_expr ]; _ } ->
      eval (`Expr last_expr)
  | { statements = h :: t; errors = err } -> (
      match eval (`Stmt h) with
      | ReturnValueObj x -> x
      | _ -> eval_program { statements = t; errors = err })

and unpack_return_object = function
  | ReturnValueObj x -> x
  | not_ret_obj -> not_ret_obj

let evaluate (prog : program) : data_obj =
  eval (`Prog prog) |> unpack_return_object
