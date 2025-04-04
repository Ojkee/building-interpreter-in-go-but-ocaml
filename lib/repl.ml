let prompt = ">> "

let repl () =
  print_endline "Monkey REPL";
  let repl_env = Object.new_enviroment () in
  let rec repl' () =
    print_string prompt;
    match read_line () with
    | "exit" -> exit 0
    | line ->
        line |> Lexer.tokenize |> Parser.parse
        |> Evaluator.evaluate ~env:repl_env
        |> Object.string_of_object_repl |> print_endline;

        (* DEBUG *)
        (* let prog = line |> Lexer.tokenize |> Parser.parse in *)
        (* prog |> Ast.string_of_program |> print_endline; *)
        (* prog *)
        (* |> Evaluator.evaluate ~env:repl_env *)
        (* |> Object.string_of_object |> print_endline; *)
        repl' ()
  in
  repl' ()
