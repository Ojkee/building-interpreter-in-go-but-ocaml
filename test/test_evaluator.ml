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
      ("foobar", ErrorObj "identifier not found: foobar");
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

let test_eval_let_statements () =
  let cases =
    [
      ("let a = 5; a;", IntegerObj 5);
      ("let a = 5 * 5; a;", IntegerObj 25);
      ("let a = 5; let b = a; b;", IntegerObj 5);
      ("let a = 5; let b = a; let c = a + b + 5; c;", IntegerObj 15);
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

let test_eval_function_object () =
  let cases =
    [
      ( "fn(x) { x + 2; };",
        let h : enviroment = new_enviroment () in
        (* POPULATE *)
        FunctionObj
          ( [ Identifier (IDENT "x") ],
            [
              ExpressionStatement
                (Infix
                   ( Identifier (IDENT "x"),
                     OPERATOR PLUS,
                     "+",
                     IntegerLiteral (INT "2", 2) ));
            ],
            h ) );
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

let test_eval_function_application () =
  let cases =
    [
      ("let identity = fn(x) { x; } identity(5);", IntegerObj 5);
      ("let identity = fn(x) { return x; } identity(5);", IntegerObj 5);
      ("let double = fn(x) { x * 2; } double(5);", IntegerObj 10);
      ("let add = fn(x, y) { x + y; } add(5, 5);", IntegerObj 10);
      ("let add = fn(x, y) { x + y; } add(5 + 5, add(5, 5));", IntegerObj 20);
      ("fn (x) { x + 1; }(5)", IntegerObj 6);
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

let test_eval_closures () =
  let cases =
    [
      ( "let newAdder = fn(x) { fn(y) { x + y; }; }; let addTwo = newAdder(2); \
         addTwo(2);",
        IntegerObj 4 );
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

let test_eval_string () =
  let cases =
    [
      ("\"Hello World!\"", StringObj "Hello World!");
      ( "\"Hello\" + \" \" + \"other\" + \" \" + \"World!\"",
        StringObj "Hello other World!" );
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

let test_eval_builtin_functions () =
  let cases =
    [
      ("len(\"\")", IntegerObj 0);
      ("len(\"four\")", IntegerObj 4);
      ("len(\"hello world\")", IntegerObj 11);
      ("len(1)", ErrorObj "argument to `len` not supported, got: INTEGER");
      ( "len(\"one\", \"two\")",
        ErrorObj "wrong number of arguments. (want: 1, got: 2)" );
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

let test_eval_array_literals () =
  let cases =
    [
      ("[1, 2 * 2, 3 + 3", ArrayObj [ IntegerObj 1; IntegerObj 4; IntegerObj 6 ]);
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

let test_eval_index_expressions () =
  let cases =
    [
      ("[1, 2, 3][0]", IntegerObj 1);
      ("[1, 2, 3][1]", IntegerObj 2);
      ("[1, 2, 3][2]", IntegerObj 3);
      ("let i = 0; [1][i]", IntegerObj 1);
      ("[1, 2, 3][1+1]", IntegerObj 3);
      ("let myArray = [1, 2, 3]; myArray[2]", IntegerObj 3);
      ( "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
        IntegerObj 6 );
      ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", IntegerObj 2);
      ("[1, 2, 3][3]", NullObj);
      ("[1, 2, 3][-1]", NullObj);
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

let test_eval_complex_programs () =
  let cases = [] in
  let test_fn = function
    | input, expected ->
        let obj = input |> tokenize |> parse |> evaluate in
        check
          (testable (Fmt.of_to_string string_of_object_deb) ( = ))
          ("Parsing:\n" ^ input) expected obj
  in
  List.iter test_fn cases;
  failwith "TODO: write tests page: 180"

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
      ( "Testing eval let statements",
        [ test_case "Basic" `Quick test_eval_let_statements ] );
      ( "Testing eval function object",
        [ test_case "Basic" `Quick test_eval_function_object ] );
      ( "Testing eval function application",
        [ test_case "Basic" `Quick test_eval_function_application ] );
      ("Testing eval closures", [ test_case "Basic" `Quick test_eval_closures ]);
      ("Testing eval string", [ test_case "Basic" `Quick test_eval_string ]);
      ( "Testing eval builtin functions",
        [ test_case "Basic" `Quick test_eval_builtin_functions ] );
      ( "Testing eval array literars",
        [ test_case "Basic" `Quick test_eval_array_literals ] );
      ( "Testing eval index expressions",
        [ test_case "Basic" `Quick test_eval_index_expressions ] );
      ( "Testing eval complex programs",
        [ test_case "Basic" `Quick test_eval_complex_programs ] );
    ]
