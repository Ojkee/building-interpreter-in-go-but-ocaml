open Alcotest
open Lib.Lexer
open Lib.Ast
open Lib.Parser

let test_let_statement_idents_errors () =
  let cases =
    [
      ( "let x = 5;\nlet y = 10;\nlet foobar = 838383;",
        {
          statements =
            [
              LetStatement
                { ident = Identifier (IDENT "x"); value = Identifier (INT "_") };
              LetStatement
                { ident = Identifier (IDENT "y"); value = Identifier (INT "_") };
              LetStatement
                {
                  ident = Identifier (IDENT "foobar");
                  value = Identifier (INT "_");
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
        let idents prog =
          List.map
            (function LetStatement { ident; _ } -> ident)
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

let () =
  run "Parser Test"
    [
      ( "Parsing let statement",
        [ test_case "Basic" `Quick test_let_statement_idents_errors ] );
    ]
