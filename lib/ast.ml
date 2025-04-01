type token = Lexer.token

type expression =
  | Identifier of token
  | IntegerLiteral of token * int
  | Boolean of token * bool
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

let rec string_of_expression = function
  | Identifier (IDENT x) -> x
  | Identifier tok -> "Identifier(" ^ Lexer.string_of_token tok ^ ")"
  | IntegerLiteral (_, x) -> string_of_int x
  | Boolean (_, x) -> string_of_bool x
  | Prefix (_, op, x) -> "(" ^ op ^ string_of_expression x ^ ")"
  | Infix (x1, _, op, x2) ->
      "(" ^ string_of_expression x1 ^ op ^ string_of_expression x2 ^ ")"
  | IfExpression (_, cond, Block (_, stmts), Some (Block (_, alt_stmts))) ->
      "if " ^ string_of_expression cond ^ " {\n" ^ string_of_statements stmts
      ^ "\n} else {\n"
      ^ string_of_statements alt_stmts
      ^ "\n}"
  | IfExpression (_, cond, Block (_, stmts), None) ->
      "if " ^ string_of_expression cond ^ " {\n" ^ string_of_statements stmts
      ^ "\n}"
  | FunctionLiteral (_, idents, Block (_, stmts)) ->
      "fn("
      ^ (idents |> List.map string_of_expression |> String.concat ", ")
      ^ ") {\n" ^ string_of_statements stmts ^ "\n}"
  | CallExpression (_, idnet_fn, args) ->
      string_of_expression idnet_fn
      ^ ( args |> List.map string_of_expression |> String.concat ", " |> fun x ->
          "(" ^ x ^ ")" )
  | PLACEHOLDER_EXPR -> "PLACEHOLDER_EXRP"

and string_of_statement = function
  | LetStatement { ident; value } -> (
      match ident with
      | Identifier (Lexer.IDENT x) ->
          "let " ^ x ^ " = " ^ string_of_expression value ^ ";"
      | _ -> "")
  | ReturnStatement x -> "return " ^ string_of_expression x ^ ";"
  | ExpressionStatement e -> string_of_expression e
  | PLACEHOLDER_STMT -> "PLACEHOLDER_STMT"

and string_of_statements (stmts : statement list) : string =
  stmts |> List.map string_of_statement |> String.concat "\n"

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

let precedence_of_token = function
  | Lexer.(OPERATOR EQ) -> EQUALS
  | Lexer.(OPERATOR NOT_EQ) -> EQUALS
  | Lexer.(OPERATOR LT) -> LESSGREATER
  | Lexer.(OPERATOR GT) -> LESSGREATER
  | Lexer.(OPERATOR PLUS) -> SUM
  | Lexer.(OPERATOR MINUS) -> SUM
  | Lexer.(OPERATOR SLASH) -> PRODUCT
  | Lexer.(OPERATOR ASTERISK) -> PRODUCT
  | Lexer.(PAREN LBRACE) -> CALL
  | _ -> LOWEST

let token_precendence_value (tok : Lexer.token) : int =
  tok |> precedence_of_token |> precedence_value

let build_if_expr (cond : expression) (cons : statement list)
    (alter : statement list option) : expression =
  match alter with
  | Some alt_stmts ->
      IfExpression
        ( KEYWORD IF,
          cond,
          Block (PAREN LBRACE, cons),
          Some (Block (PAREN LBRACE, alt_stmts)) )
  | None -> IfExpression (KEYWORD IF, cond, Block (PAREN LBRACE, cons), None)

let build_fn_literal (params : expression list) (body_stmts : statement list) :
    expression =
  FunctionLiteral (KEYWORD FUNCTION, params, Block (PAREN LBRACE, body_stmts))

let build_call (ident : expression) (call_params : expression list) : expression
    =
  CallExpression (PAREN LPAREN, ident, call_params)
