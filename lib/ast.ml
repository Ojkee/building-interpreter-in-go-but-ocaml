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
  | ExpressionStatement of expression (* first token of expression *)

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

let rec string_of_expression = function
  | Identifier (IDENT x) -> x
  | Identifier tok -> "Identifier(" ^ Lexer.string_of_token tok ^ ")"
  | IntegerLiteral (_, x) -> string_of_int x
  | Prefix (_, op, x) -> op ^ "(" ^ string_of_expression x ^ ")"
  | PLACEHOLDER_EXPR -> "PLACEHOLDER_EXRP"

let string_of_statement = function
  | LetStatement { ident; value } -> (
      match ident with
      | Identifier (Lexer.IDENT x) ->
          "let " ^ x ^ " = " ^ string_of_expression value ^ ";"
      | _ -> "")
  | ReturnStatement x -> "return " ^ string_of_expression x ^ ";"
  | ExpressionStatement e ->
      "ExpressionStatement {" ^ string_of_expression e ^ "}"

let string_of_node = function
  | Statement stmt -> "Statement(" ^ string_of_statement stmt ^ ")"
  | Expression expr -> "Expression(" ^ string_of_expression expr ^ ")"

let string_of_program (prog : program) : string =
  "stetements = [\n"
  ^ String.concat "\n\t" (List.map string_of_statement prog.statements)
  ^ "\n]\nerrors = [\n"
  ^ String.concat "\n\t" prog.errors
  ^ "\n]"

let precedence_value = function
  | LOWEST -> 0
  | EQUALS -> 1
  | LESSGREATER -> 2
  | SUM -> 3
  | PRODUCT -> 4
  | PREFIX -> 5
  | CALL -> 6
