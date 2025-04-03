open Alcotest
open Lib.Lexer
open Lib.Ast
open Lib.Parser

(* let print_stmts statements = *)
(*   statements *)
(*   |> string_of_statements *)
(*   |> (fun x -> "\027[34m[\n" ^ x ^ "\n]\027[0m") *)
(*   |> print_endline *)

let test_string () =
  let cases =
    [
      ( {
          statements =
            [
              LetStatement
                {
                  ident = Identifier (IDENT "var");
                  value = Identifier (IDENT "anotherVar");
                };
            ];
          errors = [];
        },
        [ "let var = anotherVar;" ] );
      ( {
          statements = [ ReturnStatement (Identifier (IDENT "var")) ];
          errors = [];
        },
        [ "return var;" ] );
    ]
  in
  let test_fn = function
    | input, expected ->
        check
          (testable (Fmt.of_to_string (fun x -> String.concat "\n" x)) ( = ))
          ("Parsing:\n" ^ string_of_program input)
          expected
          (List.map string_of_statement input.statements)
  in
  List.iter test_fn cases

let test_let_statement_idents () =
  let cases =
    [
      ( "let x = 5;\nlet y = true;\nlet foobar = y;",
        {
          statements =
            [
              LetStatement
                {
                  ident = Identifier (IDENT "x");
                  value = IntegerLiteral (INT "5", 5);
                };
              LetStatement
                {
                  ident = Identifier (IDENT "y");
                  value = Boolean (KEYWORD TRUE, true);
                };
              LetStatement
                {
                  ident = Identifier (IDENT "foobar");
                  value = Identifier (IDENT "y");
                };
            ];
          errors = [];
        } );
      ("", { statements = []; errors = [] });
      ( "let x 5;\nlet = 10;\nlet 838383;",
        {
          statements = [];
          errors =
            [
              "expected '='; got '5'";
              "expected 'IDENT'; got '='";
              "expected 'IDENT'; got '838383'";
            ];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable (Fmt.of_to_string string_of_program) ( = ))
          ("Parsing:\n" ^ input) expected program
  in
  List.iter test_fn cases

let test_return_statement () =
  let cases =
    [
      ( "return 5;\nreturn 10;\nreturn 993322;",
        {
          statements =
            [
              ReturnStatement (IntegerLiteral (INT "5", 5));
              ReturnStatement (IntegerLiteral (INT "10", 10));
              ReturnStatement (IntegerLiteral (INT "993322", 993322));
            ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable (Fmt.of_to_string string_of_program) ( = ))
          ("Parsing:\n" ^ input) expected program
  in
  List.iter test_fn cases

let test_identfier_expression () =
  let cases =
    [
      ( "foobar;",
        {
          statements = [ ExpressionStatement (Identifier (IDENT "foobar")) ];
          errors = [];
        } );
      ( "5;",
        {
          statements = [ ExpressionStatement (IntegerLiteral (INT "5", 5)) ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let parsed_program = input |> tokenize |> parse in
        let expr_stmts prog =
          List.map
            (function
              | ExpressionStatement x -> ExpressionStatement x
              | _ -> failwith "Wrong statement type")
            prog.statements
        in
        check
          (testable
             (Fmt.of_to_string (fun x ->
                  String.concat "\n" (List.map string_of_statement x)))
             ( = ))
          ("Parsing:\n" ^ input) (expr_stmts expected)
          (expr_stmts parsed_program);
        check
          (testable (Fmt.of_to_string (fun x -> String.concat "\n" x)) ( = ))
          ("Parsing:\n" ^ input) expected.errors parsed_program.errors
  in
  List.iter test_fn cases

let test_prefix_expressions () =
  let cases =
    [
      ( "!5",
        {
          statements =
            [
              ExpressionStatement
                (Prefix (OPERATOR BANG, "!", IntegerLiteral (INT "5", 5)));
            ];
          errors = [];
        } );
      ( "-15",
        {
          statements =
            [
              ExpressionStatement
                (Prefix (OPERATOR MINUS, "-", IntegerLiteral (INT "15", 15)));
            ];
          errors = [];
        } );
      ( "!true",
        {
          statements =
            [
              ExpressionStatement
                (Prefix (OPERATOR BANG, "!", Boolean (KEYWORD TRUE, true)));
            ];
          errors = [];
        } );
      ( "!false",
        {
          statements =
            [
              ExpressionStatement
                (Prefix (OPERATOR BANG, "!", Boolean (KEYWORD FALSE, false)));
            ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let parsed_program = input |> tokenize |> parse in
        let expr_stmts prog =
          List.map
            (function
              | ExpressionStatement x -> ExpressionStatement x
              | _ -> failwith "Wrong statement type")
            prog.statements
        in
        check
          (testable
             (Fmt.of_to_string (fun x ->
                  String.concat "\n" (List.map string_of_statement x)))
             ( = ))
          ("Parsing:\n" ^ input) (expr_stmts expected)
          (expr_stmts parsed_program)
  in
  List.iter test_fn cases

let test_infix_epressins () =
  let cases =
    [
      ( "6 + 5",
        {
          statements =
            [
              ExpressionStatement
                (Infix
                   ( IntegerLiteral (INT "6", 6),
                     OPERATOR PLUS,
                     "+",
                     IntegerLiteral (INT "5", 5) ));
            ];
          errors = [];
        } );
      ( "6 - 5",
        {
          statements =
            [
              ExpressionStatement
                (Infix
                   ( IntegerLiteral (INT "6", 6),
                     OPERATOR MINUS,
                     "-",
                     IntegerLiteral (INT "5", 5) ));
            ];
          errors = [];
        } );
      ( "6 * 5",
        {
          statements =
            [
              ExpressionStatement
                (Infix
                   ( IntegerLiteral (INT "6", 6),
                     OPERATOR ASTERISK,
                     "*",
                     IntegerLiteral (INT "5", 5) ));
            ];
          errors = [];
        } );
      ( "6 / 5",
        {
          statements =
            [
              ExpressionStatement
                (Infix
                   ( IntegerLiteral (INT "6", 6),
                     OPERATOR SLASH,
                     "/",
                     IntegerLiteral (INT "5", 5) ));
            ];
          errors = [];
        } );
      ( "6 > 5",
        {
          statements =
            [
              ExpressionStatement
                (Infix
                   ( IntegerLiteral (INT "6", 6),
                     OPERATOR GT,
                     ">",
                     IntegerLiteral (INT "5", 5) ));
            ];
          errors = [];
        } );
      ( "6 < 5",
        {
          statements =
            [
              ExpressionStatement
                (Infix
                   ( IntegerLiteral (INT "6", 6),
                     OPERATOR LT,
                     "<",
                     IntegerLiteral (INT "5", 5) ));
            ];
          errors = [];
        } );
      ( "6 == 5",
        {
          statements =
            [
              ExpressionStatement
                (Infix
                   ( IntegerLiteral (INT "6", 6),
                     OPERATOR EQ,
                     "==",
                     IntegerLiteral (INT "5", 5) ));
            ];
          errors = [];
        } );
      ( "6 != 5",
        {
          statements =
            [
              ExpressionStatement
                (Infix
                   ( IntegerLiteral (INT "6", 6),
                     OPERATOR NOT_EQ,
                     "!=",
                     IntegerLiteral (INT "5", 5) ));
            ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let parsed_program = input |> tokenize |> parse in
        let expr_stmts prog =
          List.map
            (function
              | ExpressionStatement x -> ExpressionStatement x
              | _ -> failwith "Wrong statement type")
            prog.statements
        in
        check
          (testable
             (Fmt.of_to_string (fun x ->
                  String.concat "\n" (List.map string_of_statement x)))
             ( = ))
          ("Parsing:\n" ^ input) (expr_stmts expected)
          (expr_stmts parsed_program)
  in
  List.iter test_fn cases

let test_operator_precendence () =
  let cases =
    [
      ("-a*b", "((-a)*b)");
      ("!-a", "(!(-a))");
      ("a + b + c", "((a+b)+c)");
      ("a + b - c", "((a+b)-c)");
      ("a * b * c", "((a*b)*c)");
      ("a * b / c", "((a*b)/c)");
      ("a + b / c", "(a+(b/c))");
      ("a + b * c + d / e - f", "(((a+(b*c))+(d/e))-f)");
      ("3 + 4;-5 * 5", "(3+4); ((-5)*5)");
      ("5 > 4 == 3 < 4", "((5>4)==(3<4))");
      ("5 < 4 != 3 > 4", "((5<4)!=(3>4))");
      ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3+(4*5))==((3*1)+(4*5)))");
      ("true", "true");
      ("false", "false");
      ("3 > 5 == false", "((3>5)==false)");
      ("3 < 5 == true", "((3<5)==true)");
      ("a * [1, 2, 3, 4][b * c] * d", "((a*([1, 2, 3, 4][(b*c)]))*d)");
      ( "add(a * b[2], b[1], 2 * [1, 2][1])",
        "add((a*(b[2])), (b[1]), (2*([1, 2][1])))" );
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable Fmt.string ( = ))
          ("Parsing:\n" ^ input) expected
          (String.concat "; " (List.map string_of_statement program.statements))
  in
  List.iter test_fn cases

let test_boolean () =
  let cases =
    [
      ( "true == true",
        {
          statements =
            [
              ExpressionStatement
                (Infix
                   ( Boolean (KEYWORD TRUE, true),
                     OPERATOR EQ,
                     "==",
                     Boolean (KEYWORD TRUE, true) ));
            ];
          errors = [];
        } );
      ( "false == false",
        {
          statements =
            [
              ExpressionStatement
                (Infix
                   ( Boolean (KEYWORD FALSE, false),
                     OPERATOR EQ,
                     "==",
                     Boolean (KEYWORD FALSE, false) ));
            ];
          errors = [];
        } );
      ( "true != false",
        {
          statements =
            [
              ExpressionStatement
                (Infix
                   ( Boolean (KEYWORD TRUE, true),
                     OPERATOR NOT_EQ,
                     "!=",
                     Boolean (KEYWORD FALSE, false) ));
            ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable (Fmt.of_to_string (fun x -> string_of_program x)) ( = ))
          ("Parsing:\n" ^ input) expected program
  in
  List.iter test_fn cases

let test_operator_precendence_group () =
  let cases =
    [
      ("1+(2+3)+4", "((1+(2+3))+4)");
      ("(5 + 5) * 2", "((5+5)*2)");
      ("2 / (5 + 5)", "(2/(5+5))");
      ("-(5 + 5)", "(-(5+5))");
      ("!(true == true)", "(!(true==true))");
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable Fmt.string ( = ))
          ("Parsing:\n" ^ input) expected
          (String.concat "; " (List.map string_of_statement program.statements))
  in
  List.iter test_fn cases

let test_if_expressions () =
  let cases =
    [
      ( "if (x < y) { x }",
        {
          statements =
            [
              ExpressionStatement
                (IfExpression
                   ( KEYWORD IF,
                     Infix
                       ( Identifier (IDENT "x"),
                         OPERATOR LT,
                         "<",
                         Identifier (IDENT "y") ),
                     Block
                       ( PAREN LBRACE,
                         [ ExpressionStatement (Identifier (IDENT "x")) ] ),
                     None ));
            ];
          errors = [];
        } );
      ( "if (x < y) { x } else { y }",
        {
          statements =
            [
              ExpressionStatement
                (IfExpression
                   ( KEYWORD IF,
                     Infix
                       ( Identifier (IDENT "x"),
                         OPERATOR LT,
                         "<",
                         Identifier (IDENT "y") ),
                     Block
                       ( PAREN LBRACE,
                         [ ExpressionStatement (Identifier (IDENT "x")) ] ),
                     Some
                       (Block
                          ( PAREN LBRACE,
                            [ ExpressionStatement (Identifier (IDENT "y")) ] ))
                   ));
            ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable (Fmt.of_to_string (fun x -> string_of_program x)) ( = ))
          ("Parsing:\n" ^ input) expected program
  in
  List.iter test_fn cases

let test_function_literal () =
  let cases =
    [
      ( "fn(x, y) { x + y; }",
        {
          statements =
            [
              ExpressionStatement
                (FunctionLiteral
                   ( KEYWORD FUNCTION,
                     [ Identifier (IDENT "x"); Identifier (IDENT "y") ],
                     Block
                       ( PAREN LBRACE,
                         [
                           ExpressionStatement
                             (Infix
                                ( Identifier (IDENT "x"),
                                  OPERATOR PLUS,
                                  "+",
                                  Identifier (IDENT "y") ));
                         ] ) ));
            ];
          errors = [];
        } );
      ( "fn() {}",
        {
          statements =
            [
              ExpressionStatement
                (FunctionLiteral (KEYWORD FUNCTION, [], Block (PAREN LBRACE, [])));
            ];
          errors = [];
        } );
      ( "fn(x) {}",
        {
          statements =
            [
              ExpressionStatement
                (FunctionLiteral
                   ( KEYWORD FUNCTION,
                     [ Identifier (IDENT "x") ],
                     Block (PAREN LBRACE, []) ));
            ];
          errors = [];
        } );
      ( "fn(x, y, z) {}",
        {
          statements =
            [
              ExpressionStatement
                (FunctionLiteral
                   ( KEYWORD FUNCTION,
                     [
                       Identifier (IDENT "x");
                       Identifier (IDENT "y");
                       Identifier (IDENT "z");
                     ],
                     Block (PAREN LBRACE, []) ));
            ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable (Fmt.of_to_string (fun x -> string_of_program x)) ( = ))
          ("Parsing:\n" ^ input) expected program
  in
  List.iter test_fn cases

let test_call () =
  let cases =
    [
      ( "add(1, 2 * 3, 4 + 5);",
        {
          statements =
            [
              ExpressionStatement
                (CallExpression
                   ( PAREN LPAREN,
                     Identifier (IDENT "add"),
                     [
                       IntegerLiteral (INT "1", 1);
                       Infix
                         ( IntegerLiteral (INT "2", 2),
                           OPERATOR ASTERISK,
                           "*",
                           IntegerLiteral (INT "3", 3) );
                       Infix
                         ( IntegerLiteral (INT "4", 4),
                           OPERATOR PLUS,
                           "+",
                           IntegerLiteral (INT "5", 5) );
                     ] ));
            ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable (Fmt.of_to_string string_of_program) ( = ))
          ("Parsing:\n" ^ input) expected program
  in
  List.iter test_fn cases

let test_call_string () =
  let cases =
    [
      ("a + add(b * c) + d", "((a+add((b*c)))+d)");
      ( "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
        "add(a, b, 1, (2*3), (4+5), add(6, (7*8)))" );
      ("add(a + b + c * d / f + g)", "add((((a+b)+((c*d)/f))+g))");
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable Fmt.string ( = ))
          ("Parsing:\n" ^ input) expected
          (String.concat "; " (List.map string_of_statement program.statements))
  in
  List.iter test_fn cases

let test_parser_additional () =
  let cases =
    [
      ( "let x = if (10 > 4) { 10; } else { 4; } ;",
        {
          statements =
            [
              LetStatement
                {
                  ident = Identifier (IDENT "x");
                  value =
                    IfExpression
                      ( KEYWORD IF,
                        Infix
                          ( IntegerLiteral (INT "10", 10),
                            OPERATOR GT,
                            ">",
                            IntegerLiteral (INT "4", 4) ),
                        Block
                          ( PAREN LBRACE,
                            [
                              ExpressionStatement
                                (IntegerLiteral (INT "10", 10));
                            ] ),
                        Some
                          (Block
                             ( PAREN LBRACE,
                               [
                                 ExpressionStatement
                                   (IntegerLiteral (INT "4", 4));
                               ] )) );
                };
            ];
          errors = [];
        } );
      ( "let x = if (10 > 4) { let a = if (10 < 22) { 3; }; } else { 4; } ;",
        {
          statements =
            [
              LetStatement
                {
                  ident = Identifier (IDENT "x");
                  value =
                    IfExpression
                      ( KEYWORD IF,
                        Infix
                          ( IntegerLiteral (INT "10", 10),
                            OPERATOR GT,
                            ">",
                            IntegerLiteral (INT "4", 4) ),
                        Block
                          ( PAREN LBRACE,
                            [
                              LetStatement
                                {
                                  ident = Identifier (IDENT "a");
                                  value =
                                    IfExpression
                                      ( KEYWORD IF,
                                        Infix
                                          ( IntegerLiteral (INT "10", 10),
                                            OPERATOR LT,
                                            "<",
                                            IntegerLiteral (INT "22", 22) ),
                                        Block
                                          ( PAREN LBRACE,
                                            [
                                              ExpressionStatement
                                                (IntegerLiteral (INT "3", 3));
                                            ] ),
                                        None );
                                };
                            ] ),
                        Some
                          (Block
                             ( PAREN LBRACE,
                               [
                                 ExpressionStatement
                                   (IntegerLiteral (INT "4", 4));
                               ] )) );
                };
            ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable (Fmt.of_to_string string_of_program) ( = ))
          ("Parsing:\n" ^ input) expected program
  in
  List.iter test_fn cases

let test_string_literal_expressions () =
  let cases =
    [
      ( "\"hello world\";",
        {
          statements =
            [
              ExpressionStatement
                (StringLiteral (STRING "hello world", "hello world"));
            ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable (Fmt.of_to_string string_of_program) ( = ))
          ("Parsing:\n" ^ input) expected program
  in
  List.iter test_fn cases

let test_array_literals () =
  let cases =
    [
      ( "[1, 2 * 2, 3 + 3]",
        {
          statements =
            [
              ExpressionStatement
                (ArrayLiteral
                   ( PAREN LBRACKET,
                     [
                       IntegerLiteral (INT "1", 1);
                       Infix
                         ( IntegerLiteral (INT "2", 2),
                           OPERATOR ASTERISK,
                           "*",
                           IntegerLiteral (INT "2", 2) );
                       Infix
                         ( IntegerLiteral (INT "3", 3),
                           OPERATOR PLUS,
                           "+",
                           IntegerLiteral (INT "3", 3) );
                     ] ));
            ];
          errors = [];
        } );
      ( "let x = [1, 2 * 2, 3 + 3]",
        {
          statements =
            [
              LetStatement
                {
                  ident = Identifier (IDENT "x");
                  value =
                    ArrayLiteral
                      ( PAREN LBRACKET,
                        [
                          IntegerLiteral (INT "1", 1);
                          Infix
                            ( IntegerLiteral (INT "2", 2),
                              OPERATOR ASTERISK,
                              "*",
                              IntegerLiteral (INT "2", 2) );
                          Infix
                            ( IntegerLiteral (INT "3", 3),
                              OPERATOR PLUS,
                              "+",
                              IntegerLiteral (INT "3", 3) );
                        ] );
                };
            ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable (Fmt.of_to_string string_of_program) ( = ))
          ("Parsing:\n" ^ input) expected program
  in
  List.iter test_fn cases

let test_index_expressions () =
  let cases =
    [
      ( "myArray[1+1]",
        {
          statements =
            [
              ExpressionStatement
                (IndexExpression
                   ( PAREN LBRACKET,
                     Identifier (IDENT "myArray"),
                     Infix
                       ( IntegerLiteral (INT "1", 1),
                         OPERATOR PLUS,
                         "+",
                         IntegerLiteral (INT "1", 1) ) ));
            ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let program = input |> tokenize |> parse in
        check
          (testable (Fmt.of_to_string string_of_program) ( = ))
          ("Parsing:\n" ^ input) expected program
  in
  List.iter test_fn cases

let () =
  run "Parser Test"
    [
      ("Testing String", [ test_case "Basic" `Quick test_string ]);
      ("Parsing let", [ test_case "Basic" `Quick test_let_statement_idents ]);
      ("Parsing return", [ test_case "Basic" `Quick test_return_statement ]);
      ( "Parsing identifier",
        [ test_case "Basic" `Quick test_identfier_expression ] );
      ("Parsing prefix", [ test_case "Basic" `Quick test_prefix_expressions ]);
      ("Parsing infix", [ test_case "Basic" `Quick test_infix_epressins ]);
      ( "Parsing operator precendence",
        [ test_case "Basic" `Quick test_operator_precendence ] );
      ("Parsing boolean", [ test_case "Basic" `Quick test_boolean ]);
      ( "Parsing operator precendence group",
        [ test_case "Basic" `Quick test_operator_precendence_group ] );
      ("Parsing if", [ test_case "Basic" `Quick test_if_expressions ]);
      ( "Parsing function literal",
        [ test_case "Basic" `Quick test_function_literal ] );
      ("Parsing call", [ test_case "Basic" `Quick test_call ]);
      ("Parsing call string", [ test_case "Basic" `Quick test_call_string ]);
      ( "Parsing additional tests",
        [ test_case "Basic" `Quick test_parser_additional ] );
      ( "Parsing string literal",
        [ test_case "Basic" `Quick test_string_literal_expressions ] );
      ("Parsing array literal", [ test_case "Basic" `Quick test_array_literals ]);
      ( "Parsing index expressions",
        [ test_case "Basic" `Quick test_index_expressions ] );
    ]
