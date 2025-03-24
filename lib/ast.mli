type token = Lexer.token
type operator = string

type expression =
  | Identifier of token
  | IntegerLiteral of token * int
  | Prefix of token * operator * expression
  | PLACEHOLDER_EXPR

type statement =
  | LetStatement of { ident : expression; value : expression }
  | ReturnStatement of expression
  | ExpressionStatement of expression

type node = Statement of statement | Expression of expression
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
val string_of_node : node -> string
val string_of_program : program -> string
val precedence_value : precedence -> int
