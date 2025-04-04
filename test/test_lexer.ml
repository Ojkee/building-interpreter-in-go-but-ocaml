open Alcotest
open Lib.Lexer

let test_tokenize () =
  let cases =
    [
      ("", [ EOF ]);
      ( "=+(){},;",
        [
          OPERATOR ASSIGN;
          OPERATOR PLUS;
          PAREN LPAREN;
          PAREN RPAREN;
          PAREN LBRACE;
          PAREN RBRACE;
          DELIMITER COMMA;
          DELIMITER SEMICOLON;
          EOF;
        ] );
      ( "let five = 5;\n\
        \ let ten = 10;\n\n\
        \ let add = fn(x, y) {\n\
        \  x + y \n\
        \ }\n\n\
        \ let result = add(five, ten);",
        [
          KEYWORD LET;
          IDENT "five";
          OPERATOR ASSIGN;
          INT "5";
          DELIMITER SEMICOLON;
          KEYWORD LET;
          IDENT "ten";
          OPERATOR ASSIGN;
          INT "10";
          DELIMITER SEMICOLON;
          KEYWORD LET;
          IDENT "add";
          OPERATOR ASSIGN;
          KEYWORD FUNCTION;
          PAREN LPAREN;
          IDENT "x";
          DELIMITER COMMA;
          IDENT "y";
          PAREN RPAREN;
          PAREN LBRACE;
          IDENT "x";
          OPERATOR PLUS;
          IDENT "y";
          PAREN RBRACE;
          KEYWORD LET;
          IDENT "result";
          OPERATOR ASSIGN;
          IDENT "add";
          PAREN LPAREN;
          IDENT "five";
          DELIMITER COMMA;
          IDENT "ten";
          PAREN RPAREN;
          DELIMITER SEMICOLON;
          EOF;
        ] );
      ( "!-/*5;\n5 < 10 > 5;",
        [
          OPERATOR BANG;
          OPERATOR MINUS;
          OPERATOR SLASH;
          OPERATOR ASTERISK;
          INT "5";
          DELIMITER SEMICOLON;
          INT "5";
          OPERATOR LT;
          INT "10";
          OPERATOR GT;
          INT "5";
          DELIMITER SEMICOLON;
          EOF;
        ] );
      ( "if (5 < 10) {\n   return true;\n } else {\n   return false;\n }\n ",
        [
          KEYWORD IF;
          PAREN LPAREN;
          INT "5";
          OPERATOR LT;
          INT "10";
          PAREN RPAREN;
          PAREN LBRACE;
          KEYWORD RETURN;
          KEYWORD TRUE;
          DELIMITER SEMICOLON;
          PAREN RBRACE;
          KEYWORD ELSE;
          PAREN LBRACE;
          KEYWORD RETURN;
          KEYWORD FALSE;
          DELIMITER SEMICOLON;
          PAREN RBRACE;
          EOF;
        ] );
      ( "10 == 10;\n10 != 9;",
        [
          INT "10";
          OPERATOR EQ;
          INT "10";
          DELIMITER SEMICOLON;
          INT "10";
          OPERATOR NOT_EQ;
          INT "9";
          DELIMITER SEMICOLON;
          EOF;
        ] );
      ("\"foobar\"", [ STRING "foobar"; EOF ]);
      ("\"foo bar\"", [ STRING "foo bar"; EOF ]);
      ( "[1, 2];",
        [
          PAREN LBRACKET;
          INT "1";
          DELIMITER COMMA;
          INT "2";
          PAREN RBRACKET;
          DELIMITER SEMICOLON;
          EOF;
        ] );
    ]
  in
  let test_fn = function
    | input, expected ->
        check
          (list (testable (Fmt.of_to_string string_of_token) ( = )))
          ("Lexing:\n" ^ input) expected (tokenize input)
  in
  List.iter test_fn cases

let () =
  run "Lexer Test" [ ("Tokenize", [ test_case "Basic" `Quick test_tokenize ]) ]
