type token = Lexer.token

type expression =
  | Identifier of token
  | IntegerLiteral of int
  | PLACEHOLDER_EXPR

type statement =
  | LetStatement of { ident : expression; value : expression }
  | ReturnStatement of expression
  | ExpressionStatement of token * expression

type node = Statement of statement | Expression of expression
type program = { statements : statement list; errors : string list }

val string_of_expression : expression -> string
val string_of_statement : statement -> string
val string_of_node : node -> string
val string_of_program : program -> string
