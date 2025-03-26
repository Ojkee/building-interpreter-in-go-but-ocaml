open Lexer
open Ast

let rec skip_till_semicolon = function
  | [] -> []
  | [ EOF ] -> []
  | DELIMITER SEMICOLON :: t -> t
  | _ :: t -> skip_till_semicolon t

let expected_err f s = Printf.sprintf "expected '%s'; got '%s'" f s

let infixable prec op : bool =
  Lexer.is_infix_operator op
  && precedence_value prec < operator_precendence_value op

let rec parse_expression (tokens : token list) (prec : precedence) :
    (expression * token list) option =
  match parse_prefix tokens with
  | Some (lexpr, OPERATOR op :: rest) when infixable prec op -> (
      match parse_infix rest with
      | Some (rexpr, rrest) ->
          Some (Infix (lexpr, OPERATOR op, string_of_operator op, rexpr), rrest)
      | None -> Some (lexpr, OPERATOR op :: rest))
  | result -> result

and parse_prefix (tokens : token list) : (expression * token list) option =
  print_endline "IN PREFIX";
  tokens |> List.map string_of_token |> String.concat " " |> print_endline;
  print_endline "";
  match tokens with
  | IDENT x :: t -> Some (Identifier (IDENT x), t)
  | INT x :: t -> Some (IntegerLiteral (INT x, int_of_string x), t)
  | OPERATOR op :: t when match op with MINUS | BANG -> true | _ -> false -> (
      match parse_expression t PREFIX with
      | Some (expr, rest) ->
          Some (Prefix (OPERATOR op, Lexer.string_of_operator op, expr), rest)
      | None -> None)
  | _ -> None

and parse_infix (tokens : token list) : (expression * token list) option =
  tokens |> List.map string_of_token |> String.concat " " |> print_endline;
  match parse_prefix tokens with
  | Some (lexpr, OPERATOR h :: rest) when Lexer.is_infix_operator h -> (
      let cur_prec = precedence_of_operator h in
      match parse_expression rest cur_prec with
      | Some (rexpr, rrest) ->
          Some (Infix (lexpr, OPERATOR h, string_of_operator h, rexpr), rrest)
      | None -> None)
  | result -> result

let parse (tokens : token list) : program =
  let rec advance tokens stmts errs =
    match tokens with
    | [] -> { statements = List.rev stmts; errors = List.rev errs }
    | [ EOF ] -> { statements = List.rev stmts; errors = List.rev errs }
    | KEYWORD LET :: IDENT x :: OPERATOR ASSIGN :: t ->
        advance
          (skip_till_semicolon t) (* TODO: PARSE BODY *)
          (LetStatement
             { ident = Identifier (IDENT x); value = PLACEHOLDER_EXPR }
          :: stmts)
          errs
    | KEYWORD LET :: h1 :: h2 :: _ -> (
        match (h1, h2) with
        | IDENT _, h2 ->
            advance
              (skip_till_semicolon tokens)
              stmts
              (expected_err "=" (string_of_token h2) :: errs)
        | h1, _ ->
            advance
              (skip_till_semicolon tokens)
              stmts
              (expected_err "IDENT" (string_of_token h1) :: errs))
    | KEYWORD RETURN :: t ->
        advance
          (skip_till_semicolon t) (* TODO: PARSE BODY *)
          (ReturnStatement PLACEHOLDER_EXPR :: stmts)
          errs
    | h :: t
      when match h with OPERATOR MINUS | OPERATOR BANG -> true | _ -> false -> (
        match parse_expression tokens PREFIX with
        | Some (expr, rest) ->
            advance rest (ExpressionStatement expr :: stmts) errs
        | None -> advance t stmts ("Parse minus/bang err" :: errs))
    | h :: t when match h with INT _ | IDENT _ -> true | _ -> false -> (
        match parse_expression tokens LOWEST with
        | Some (expr, rest) ->
            advance rest (ExpressionStatement expr :: stmts) errs
        | None -> advance t stmts ("Parse int/ident err" :: errs))
    | _ :: t -> advance t stmts errs
  in
  advance tokens [] []
