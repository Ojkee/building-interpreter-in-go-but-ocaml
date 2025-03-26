type token = Lexer.token

type expression =
  | Identifier of token
  | IntegerLiteral of token * int
  | Boolean of token * bool
  | Prefix of token * string * expression
  | Infix of expression * token * string * expression
  | PLACEHOLDER_EXPR

type statement =
  | LetStatement of { ident : expression; value : expression }
  | ReturnStatement of expression
  | ExpressionStatement of expression
  | PLACEHOLDER_STMT

type program = { statements : statement list; errors : string list }

type precedence =
  | LOWEST
  | EQUALS
  | LESSGREATER
  | SUM
  | PRODUCT
  | PREFIX
  | CALL

val string_of_expression : expression -> string
val string_of_statement : statement -> string
val string_of_program : program -> string
val precedence_value : precedence -> int
val precedence_of_operator : Lexer.operator -> precedence
val operator_precendence_value : Lexer.operator -> int
