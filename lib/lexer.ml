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
type parenthesis = LPAREN | RPAREN | LBRACE | RBRACE | LBRACKET | RBRACKET

type token =
  | ILLEGAL of string
  | EOF
  | IDENT of string
  | INT of string
  | FLOAT of string
  | STRING of string
  | OPERATOR of operator
  | PAREN of parenthesis
  | DELIMITER of delimiter
  | KEYWORD of keyword

let string_of_operator = function
  | ASSIGN -> "="
  | PLUS -> "+"
  | MINUS -> "-"
  | BANG -> "!"
  | ASTERISK -> "*"
  | SLASH -> "/"
  | LT -> "<"
  | GT -> ">"
  | EQ -> "=="
  | NOT_EQ -> "!="

let string_of_delimiter = function COMMA -> "," | SEMICOLON -> ";"

let string_of_keyword = function
  | FUNCTION -> "fn"
  | LET -> "let"
  | TRUE -> "true"
  | FALSE -> "false"
  | IF -> "if"
  | ELSE -> "else"
  | RETURN -> "return"

let string_of_parenthesis = function
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | LBRACKET -> "["
  | RBRACKET -> "]"

let string_of_token = function
  | ILLEGAL x -> "ILLEGAL(" ^ x ^ ")"
  | EOF -> "EOF"
  | IDENT x -> x
  | INT i -> i
  | FLOAT f -> f
  | STRING s -> s
  | OPERATOR op -> string_of_operator op
  | PAREN p -> string_of_parenthesis p
  | DELIMITER d -> string_of_delimiter d
  | KEYWORD k -> string_of_keyword k

let string_of_tokens (tokens : token list) : string =
  tokens |> List.map (fun t -> string_of_token t) |> String.concat "\n"

module KeywordsMap = Map.Make (String)

let keywords_lookup : token KeywordsMap.t =
  KeywordsMap.(
    empty |> add "let" (KEYWORD LET)
    |> add "fn" (KEYWORD FUNCTION)
    |> add "true" (KEYWORD TRUE)
    |> add "false" (KEYWORD FALSE)
    |> add "if" (KEYWORD IF) |> add "else" (KEYWORD ELSE)
    |> add "return" (KEYWORD RETURN))

let char_list_of_string s = s |> String.to_seq |> List.of_seq
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_whitespace = function ' ' | '\n' | '\t' | '\r' -> true | _ -> false

let is_infix_operator = function
  | OPERATOR PLUS
  | OPERATOR MINUS
  | OPERATOR SLASH
  | OPERATOR ASTERISK
  | OPERATOR EQ
  | OPERATOR NOT_EQ
  | OPERATOR LT
  | OPERATOR GT
  | PAREN LBRACKET ->
      true
  | _ -> false

let string_rev (s : string) : string =
  s |> String.to_seq |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq

let read_identifier (chars : char list) : string * char list =
  let rec read_identifier' ident lst =
    match lst with
    | c :: rest when is_alpha c || is_digit c ->
        read_identifier' (String.make 1 c ^ ident) rest
    | _ -> (string_rev ident, lst)
  in
  read_identifier' "" chars

let read_number (chars : char list) : string * char list =
  let rec read_number' num rest =
    match rest with
    | c :: tail when is_digit c -> read_number' (num ^ String.make 1 c) tail
    | _ -> (num, rest)
  in
  read_number' "" chars

let get_keyword_or_ident (word : string) : token =
  match KeywordsMap.find_opt word keywords_lookup with
  | Some keywordType -> keywordType
  | None -> IDENT word

let read_string (chars : char list) : string * char list =
  let rec read_string' value rest =
    match rest with
    | [] -> (value, [])
    | h :: t when h = '"' -> (value, t)
    | h :: t -> read_string' (h :: value) t
  in
  match read_string' [] chars with
  | value, rest -> (value |> List.rev |> List.to_seq |> String.of_seq, rest)

let run (chars : char list) : token list =
  let rec advance chars dst =
    match chars with
    | [] -> EOF :: dst |> List.rev
    | '=' :: '=' :: tail -> advance tail (OPERATOR EQ :: dst)
    | '!' :: '=' :: tail -> advance tail (OPERATOR NOT_EQ :: dst)
    | '=' :: tail -> advance tail (OPERATOR ASSIGN :: dst)
    | '+' :: tail -> advance tail (OPERATOR PLUS :: dst)
    | '-' :: tail -> advance tail (OPERATOR MINUS :: dst)
    | '!' :: tail -> advance tail (OPERATOR BANG :: dst)
    | '*' :: tail -> advance tail (OPERATOR ASTERISK :: dst)
    | '/' :: tail -> advance tail (OPERATOR SLASH :: dst)
    | '<' :: tail -> advance tail (OPERATOR LT :: dst)
    | '>' :: tail -> advance tail (OPERATOR GT :: dst)
    | '(' :: tail -> advance tail (PAREN LPAREN :: dst)
    | ')' :: tail -> advance tail (PAREN RPAREN :: dst)
    | '{' :: tail -> advance tail (PAREN LBRACE :: dst)
    | '}' :: tail -> advance tail (PAREN RBRACE :: dst)
    | '[' :: tail -> advance tail (PAREN LBRACKET :: dst)
    | ']' :: tail -> advance tail (PAREN RBRACKET :: dst)
    | ',' :: tail -> advance tail (DELIMITER COMMA :: dst)
    | ';' :: tail -> advance tail (DELIMITER SEMICOLON :: dst)
    | '"' :: tail -> (
        match read_string tail with
        | value, rest -> advance rest (STRING value :: dst))
    | h :: tail when is_whitespace h -> advance tail dst
    | h :: _ when is_alpha h -> (
        match read_identifier chars with
        | word, rest -> advance rest (get_keyword_or_ident word :: dst))
    | h :: _ when is_digit h -> (
        match read_number chars with num, rest -> advance rest (INT num :: dst))
    | h :: tail -> advance tail (ILLEGAL (String.make 1 h) :: dst)
  in
  advance chars []

let tokenize (content : string) : token list =
  content |> char_list_of_string |> run
