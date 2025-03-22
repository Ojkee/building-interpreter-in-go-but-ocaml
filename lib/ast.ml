type token = Lexer.token
type expression = Identifier of token
type statement = LetStatement of { ident : expression; value : expression }
type node = Statement of statement | Expression of expression
type program = { statements : statement list; errors : string list }

let string_of_expression = function
  | Identifier tok -> "Identifier(" ^ Lexer.string_of_token tok ^ ")"

let string_of_statement = function
  | LetStatement { ident; value } ->
      "Let { " ^ string_of_expression ident ^ "; " ^ string_of_expression value
      ^ " }"

let string_of_node = function
  | Statement stmt -> "Statement(" ^ string_of_statement stmt ^ ")"
  | Expression expr -> "Expression(" ^ string_of_expression expr ^ ")"

let string_of_program (prog : program) : string =
  "stetements = [\n"
  ^ String.concat "\n\t" (List.map string_of_statement prog.statements)
  ^ "\n]\nerrors = [\n"
  ^ String.concat "\n\t" prog.errors
  ^ "\n]"
