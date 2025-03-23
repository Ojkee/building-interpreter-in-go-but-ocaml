open Alcotest
open Lib.Lexer
open Lib.Ast
open Lib.Parser

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
        let idents prog =
          List.map
            (function
              | LetStatement { ident; _ } -> ident
              | _ -> failwith "Wrong statement type")
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
              ReturnStatement (Identifier (IDENT ""));
              ReturnStatement (Identifier (IDENT ""));
              ReturnStatement (Identifier (IDENT ""));
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
  let cases = [] in
  ignore cases;
  failwith "TODO"

let () =
  run "Parser Test"
    [
      ("Testing String", [ test_case "Basic" `Quick test_string ]);
      ( "Parsing let statement",
        [ test_case "Basic" `Quick test_let_statement_idents ] );
      ( "Parsing return statement",
        [ test_case "Basic" `Quick test_return_statement ] );
      ( "Parsing identifier expression",
        [ test_case "Basic" `Quick test_identfier_expression ] );
    ]
