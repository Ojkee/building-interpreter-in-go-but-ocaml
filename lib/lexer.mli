type operator =
  | ASSIGN
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH
  | LT
  | GT
  | EQ
  | NOT_EQ

type delimiter = COMMA | SEMICOLON
type keyword = FUNCTION | LET | TRUE | FALSE | IF | ELSE | RETURN
type parenthesis = LPAREN | RPAREN | LBRACE | RBRACE

type token =
  | ILLEGAL of string
  | EOF
  | IDENT of string
  | INT of string
  | FLOAT of string
  | OPERATOR of operator
  | PAREN of parenthesis
  | DELIMITER of delimiter
  | KEYWORD of keyword

val string_of_token : token -> string
val string_of_tokens : token list -> string
val tokenize : string -> token list
