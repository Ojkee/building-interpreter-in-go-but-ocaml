let prompt = ">> "

let rec repl () =
  print_endline "Monkey REPL (Lexer)";
  print_string prompt;
  match read_line () with
  | "exit" -> exit 0
  | line ->
      line |> Lexer.tokenize |> Parser.parse |> Ast.string_of_program
      |> print_endline;
      repl ()
