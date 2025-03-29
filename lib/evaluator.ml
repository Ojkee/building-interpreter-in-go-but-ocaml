open Ast
open Lexer
open Object

type node_type = [ `Prog of program | `Stmt of statement | `Expr of expression ]

let rec eval (node : node_type) : data_obj =
  match node with
  | `Prog { statements = stmts; _ } -> eval_statements stmts
  | `Stmt (ExpressionStatement x) -> eval (`Expr x)
  | `Stmt _ -> failwith "Unimplemented statement type in eval"
  | `Expr (IntegerLiteral (_, value)) -> IntegerObj value
  | `Expr (Boolean (_, value)) -> BooleanObj value
  | `Expr (Prefix (OPERATOR BANG, _, value)) -> (
      match eval (`Expr value) with
      | BooleanObj x -> BooleanObj (not x)
      (* | IntegerObj x when x <> 0 -> BooleanObj true *)
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
      | BooleanObj true, _ -> eval_statements cons
      | IntegerObj _, _ -> eval_statements cons
      | BooleanObj false, Some (Block (_, alter)) -> eval_statements alter
      | _ -> NullObj)
  | `Expr _ -> failwith "Unimplemented expression type in eval"

and eval_statements (stmts : statement list) : data_obj =
  match stmts with
  | [] -> NullObj
  | [ x ] -> eval (`Stmt x)
  | h :: t ->
      let _ = eval (`Stmt h) in
      eval_statements t

let evaluate (prog : program) : data_obj = eval (`Prog prog)
