let prompt = ">> "

let rec repl () =
  print_endline "Enter lines";
  print_string prompt;
  match read_line () with
  | "exit" -> exit 0
  | line ->
      Lexer.(tokenize line |> string_of_tokens) |> print_endline;
      repl ()
