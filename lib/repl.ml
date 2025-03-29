let prompt = ">> "

let repl () =
  let repl_env = Object.new_enviroment () in
  let rec repl' () =
    print_endline "Monkey REPL";
    print_string prompt;
    match read_line () with
    | "exit" -> exit 0
    | line ->
        line |> Lexer.tokenize |> Parser.parse
        |> Evaluator.evaluate ~env:repl_env
        |> Object.string_of_object |> print_endline;
        repl' ()
  in
  repl' ()
