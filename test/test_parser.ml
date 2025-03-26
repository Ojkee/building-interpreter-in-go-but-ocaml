open Alcotest
open Lib.Lexer
open Lib.Ast
open Lib.Parser

(* let print_stmts statements = *)
(*   statements *)
(*   |> List.map (fun x -> string_of_statement x) *)
(*   |> String.concat "\n" *)
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
      ( "let x = 5;\nlet y = 10;\nlet foobar = 838383;",
        {
          statements =
            [
              LetStatement
                { ident = Identifier (IDENT "x"); value = PLACEHOLDER_EXPR };
              LetStatement
                { ident = Identifier (IDENT "y"); value = PLACEHOLDER_EXPR };
              LetStatement
                {
                  ident = Identifier (IDENT "foobar");
                  value = PLACEHOLDER_EXPR;
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
        let parsed_program = input |> tokenize |> parse in
        (* parsed_program |> string_of_program |> print_endline; *)
        let idents prog =
          List.map
            (function
              | LetStatement { ident; _ } -> ident
              | x ->
                  failwith
                    (Printf.sprintf "Wrong statement type: %s"
                       (string_of_statement x)))
            prog.statements
        in
        check
          (testable
             (Fmt.of_to_string (fun x ->
                  String.concat "\n" (List.map string_of_expression x)))
             ( = ))
          ("Parsing:\n" ^ input) (idents expected) (idents parsed_program);
        check
          (testable (Fmt.of_to_string (fun x -> String.concat "\n" x)) ( = ))
          ("Parsing:\n" ^ input) expected.errors parsed_program.errors
  in
  List.iter test_fn cases

let test_return_statement () =
  let cases =
    [
      ( "return 5;\nreturn 10;\nreturn 993322;",
        {
          statements =
            [
              ReturnStatement PLACEHOLDER_EXPR;
              ReturnStatement PLACEHOLDER_EXPR;
              ReturnStatement PLACEHOLDER_EXPR;
            ];
          errors = [];
        } );
    ]
  in
  let test_fn = function
    | input, expected ->
        let parsed_program = input |> tokenize |> parse in
        let rets prog =
          List.map
            (function
              | ReturnStatement _ -> "return"
              | _ -> failwith "Wrong statement type")
            prog.statements
        in
        check
          (testable (Fmt.of_to_string (fun x -> String.concat "\n" x)) ( = ))
          ("Parsing:\n" ^ input) (rets expected) (rets parsed_program);
        check
          (testable (Fmt.of_to_string (fun x -> String.concat "\n" x)) ( = ))
          ("Parsing:\n" ^ input) expected.errors parsed_program.errors
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
      ("5 < 4 != 3 > 4", "((5<3)!=(3>4))");
      ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3+(4*5))==((3*1)+(4*5))");
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

let () =
  run "Parser Test"
    [
      ("Testing String", [ test_case "Basic" `Quick test_string ]);
      ( "Parsing let statement",
        [ test_case "Basic" `Quick test_let_statement_idents ] );
      ( "Parsing return statement",
        [ test_case "Basic" `Quick test_return_statement ] );
      ( "Parsing identifier expression statements",
        [ test_case "Basic" `Quick test_identfier_expression ] );
      ( "Parsing prefix expression",
        [ test_case "Basic" `Quick test_prefix_expressions ] );
      ( "Parsing infix expression",
        [ test_case "Basic" `Quick test_infix_epressins ] );
      ( "Parsing operator precendence",
        [ test_case "Basic" `Quick test_operator_precendence ] );
    ]
