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
  | INDENT of string
  | INT of int
  | FLOAT of float
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
  | ELSE -> "ELSE"
  | RETURN -> "return"

let string_of_parenthesis = function
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACE -> "{"
  | RBRACE -> "}"

let string_of_token = function
  | ILLEGAL x -> "ILLEGAL(" ^ x ^ ")"
  | EOF -> "EOF"
  | INDENT x -> "INDENT(" ^ x ^ ")"
  | INT i -> string_of_int i
  | FLOAT f -> string_of_float f
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

(* let string_of_char_list ch = ch |> List.to_seq |> String.of_seq *)
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_whitespace = function ' ' | '\n' | '\t' | '\r' -> true | _ -> false

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
  | None -> INDENT word

let run (chars : char list) : token list =
  let rec run' chars dst =
    match chars with
    | [] -> EOF :: dst |> List.rev
    | '=' :: '=' :: tail -> run' tail (OPERATOR EQ :: dst)
    | '!' :: '=' :: tail -> run' tail (OPERATOR NOT_EQ :: dst)
    | '=' :: tail -> run' tail (OPERATOR ASSIGN :: dst)
    | '+' :: tail -> run' tail (OPERATOR PLUS :: dst)
    | '-' :: tail -> run' tail (OPERATOR MINUS :: dst)
    | '!' :: tail -> run' tail (OPERATOR BANG :: dst)
    | '*' :: tail -> run' tail (OPERATOR ASTERISK :: dst)
    | '/' :: tail -> run' tail (OPERATOR SLASH :: dst)
    | '<' :: tail -> run' tail (OPERATOR LT :: dst)
    | '>' :: tail -> run' tail (OPERATOR GT :: dst)
    | '(' :: tail -> run' tail (PAREN LPAREN :: dst)
    | ')' :: tail -> run' tail (PAREN RPAREN :: dst)
    | '{' :: tail -> run' tail (PAREN LBRACE :: dst)
    | '}' :: tail -> run' tail (PAREN RBRACE :: dst)
    | ',' :: tail -> run' tail (DELIMITER COMMA :: dst)
    | ';' :: tail -> run' tail (DELIMITER SEMICOLON :: dst)
    | h :: tail when is_whitespace h -> run' tail dst
    | h :: _ when is_alpha h ->
        let word, rest = read_identifier chars in
        run' rest (get_keyword_or_ident word :: dst)
    | h :: _ when is_digit h ->
        let num, rest = read_number chars in
        run' rest (INT (int_of_string num) :: dst)
    | h :: tail -> run' tail (ILLEGAL (String.make 1 h) :: dst)
  in
  run' chars []

let tokenize (content : string) : token list =
  content |> char_list_of_string |> run
