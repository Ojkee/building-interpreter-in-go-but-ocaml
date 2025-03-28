let prompt = ">> "

let rec repl () =
  print_endline "Monkey REPL";
  print_string prompt;
  match read_line () with
  | "exit" -> exit 0
  | line ->
      line |> Lexer.tokenize |> Parser.parse |> Evaluator.evaluate
      |> Object.string_of_object |> print_endline;
      (* line |> Lexer.tokenize |> Parser.parse |> Ast.string_of_program *)
      (* |> print_endline; *)
      repl ()
