type token = Lexer.token

type expression =
  | Identifier of token
  | IntegerLiteral of token * int
  | StringLiteral of token * string
  | Boolean of token * bool
  | ArrayLiteral of token * expression list
  | IndexExpression of token * expression * expression
  | Prefix of token * string * expression
  | Infix of expression * token * string * expression
  | IfExpression of token * expression * block * block option
  | FunctionLiteral of token * expression list * block
  | CallExpression of token * expression * expression list
  | PLACEHOLDER_EXPR

and statement =
  | LetStatement of { ident : expression; value : expression }
  | ReturnStatement of expression
  | ExpressionStatement of expression
  | PLACEHOLDER_STMT

and block = Block of token * statement list

type program = { statements : statement list; errors : string list }

type precedence =
  | LOWEST
  | EQUALS
  | LESSGREATER
  | SUM
  | PRODUCT
  | PREFIX
  | CALL
  | INDEX

val string_of_expression : expression -> string
val string_of_statement : statement -> string
val string_of_statements : statement list -> string
val string_of_program : program -> string
val precedence_value : precedence -> int
val precedence_of_token : Lexer.token -> precedence
val token_precendence_value : Lexer.token -> int

(* BUILDERS *)
val build_let_statement : string -> expression -> statement

val build_if_expr :
  expression -> statement list -> statement list option -> expression

val build_fn_literal : expression list -> statement list -> expression
val build_call : expression -> expression list -> expression
val build_array_literal : expression list -> expression
