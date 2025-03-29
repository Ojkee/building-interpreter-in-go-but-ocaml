open Alcotest
open Lib.Object
open Lib.Lexer
open Lib.Parser
open Lib.Evaluator

let test_eval_integer_expression () =
  let cases =
    [
      ("5", IntegerObj 5);
      ("10", IntegerObj 10);
      ("-5", IntegerObj (-5));
      ("-10", IntegerObj (-10));
      ("5 + 5 + 5 + 5 - 10", IntegerObj 10);
      ("2 * 2 * 2 * 2 * 2", IntegerObj 32);
      ("-50 + 100 + -50", IntegerObj 0);
      ("5 * 2 + 10", IntegerObj 20);
      ("5 + 2 * 10", IntegerObj 25);
      ("20 + 2 * -10", IntegerObj 0);
      ("50 / 2 * 2 + 10", IntegerObj 60);
      ("2 * (5 + 10)", IntegerObj 30);
      ("3 * 3 * 3 + 10", IntegerObj 37);
      ("3 * (3 * 3) + 10", IntegerObj 37);
      ("(5 + 10 * 2 + 15 / 3) * 2 + -10", IntegerObj 50);
    ]
  in
  let test_fn = function
    | input, expected ->
        let obj = input |> tokenize |> parse |> evaluate in
        check
          (testable (Fmt.of_to_string string_of_object) ( = ))
          ("Parsing:\n" ^ input) expected obj
  in
  List.iter test_fn cases

let test_eval_boolean_expression () =
  let cases =
    [
      ("true", BooleanObj true);
      ("false", BooleanObj false);
      ("1 < 2", BooleanObj true);
      ("1 > 2", BooleanObj false);
      ("1 < 1", BooleanObj false);
      ("1 > 1", BooleanObj false);
      ("1 == 1", BooleanObj true);
      ("1 != 1", BooleanObj false);
      ("1 == 2", BooleanObj false);
      ("1 != 2", BooleanObj true);
      ("true == true", BooleanObj true);
      ("false == false", BooleanObj true);
      ("true == false", BooleanObj false);
      ("true != false", BooleanObj true);
      ("false != true", BooleanObj true);
      ("(1 < 2) == true", BooleanObj true);
      ("(1 < 2) == false", BooleanObj false);
      ("(1 > 2) == true", BooleanObj false);
      ("(1 > 2) == false", BooleanObj true);
    ]
  in
  let test_fn = function
    | input, expected ->
        let obj = input |> tokenize |> parse |> evaluate in
        check
          (testable (Fmt.of_to_string string_of_object) ( = ))
          ("Parsing:\n" ^ input) expected obj
  in
  List.iter test_fn cases

let test_eval_bang_operator () =
  let cases =
    [
      ("!true", BooleanObj false);
      ("!false", BooleanObj true);
      ("!5", BooleanObj false);
      ("!!true", BooleanObj true);
      ("!!false", BooleanObj false);
      ("!!5", BooleanObj true);
    ]
  in
  let test_fn = function
    | input, expected ->
        let obj = input |> tokenize |> parse |> evaluate in
        check
          (testable (Fmt.of_to_string string_of_object) ( = ))
          ("Parsing:\n" ^ input) expected obj
  in
  List.iter test_fn cases

let test_eval_if_else_expression () =
  let cases =
    [
      ("if (true) { 10 }", IntegerObj 10);
      ("if (false) { 10 }", NullObj);
      ("if (1) { 10 } ", IntegerObj 10);
      ("if (1 < 2) { 10 } ", IntegerObj 10);
      ("if (1 > 2) { 10 } ", NullObj);
      ("if (1 > 2) { 10 } else { 20 }", IntegerObj 20);
      ("if (1 < 2) { 10 } else { 20 }", IntegerObj 10);
    ]
  in
  let test_fn = function
    | input, expected ->
        let obj = input |> tokenize |> parse |> evaluate in
        check
          (testable (Fmt.of_to_string string_of_object) ( = ))
          ("Parsing:\n" ^ input) expected obj
  in
  List.iter test_fn cases

let test_eval_return_statements () =
  let cases =
    [
      ("return 10;", IntegerObj 10);
      ("return 10; 9;", IntegerObj 10);
      ("return 2 * 5; 9;", IntegerObj 10);
      ("9; return 2 * 5; 9;", IntegerObj 10);
      ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", IntegerObj 10);
    ]
  in
  let test_fn = function
    | input, expected ->
        let obj = input |> tokenize |> parse |> evaluate in
        check
          (testable (Fmt.of_to_string string_of_object_deb) ( = ))
          ("Parsing:\n" ^ input) expected obj
  in
  List.iter test_fn cases

let test_eval_error_handling () =
  let cases =
    [
      ("5 + true", ErrorObj "type mismatch: INTEGER + BOOLEAN");
      ("5 + true; 5;", ErrorObj "type mismatch: INTEGER + BOOLEAN");
      ("-true", ErrorObj "unknown operator: -BOOLEAN");
      ("true + false;", ErrorObj "unknown operator: BOOLEAN + BOOLEAN");
      ("5; true + false; 5; ", ErrorObj "unknown operator: BOOLEAN + BOOLEAN");
      ( "if (10 > 1) { true + false; }",
        ErrorObj "unknown operator: BOOLEAN + BOOLEAN" );
      ( "if (10 > 1) { if ( 10 > 1 ) { true + false; } return 1; }",
        ErrorObj "unknown operator: BOOLEAN + BOOLEAN" );
    ]
  in
  let test_fn = function
    | input, expected ->
        let obj = input |> tokenize |> parse |> evaluate in
        check
          (testable (Fmt.of_to_string string_of_object_deb) ( = ))
          ("Parsing:\n" ^ input) expected obj
  in
  List.iter test_fn cases

let () =
  run "Parser Test"
    [
      ( "Testing eval integer expression",
        [ test_case "Basic" `Quick test_eval_integer_expression ] );
      ( "Testing eval boolean expression",
        [ test_case "Basic" `Quick test_eval_boolean_expression ] );
      ( "Testing eval bang operator",
        [ test_case "Basic" `Quick test_eval_bang_operator ] );
      ( "Testing eval if else expression",
        [ test_case "Basic" `Quick test_eval_if_else_expression ] );
      ( "Testing eval return statements",
        [ test_case "Basic" `Quick test_eval_return_statements ] );
      ( "Testing eval error handling",
        [ test_case "Basic" `Quick test_eval_error_handling ] );
    ]
