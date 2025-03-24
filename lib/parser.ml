open Lexer
open Ast

let rec skip_till_semicolon = function
  | [] -> []
  | [ EOF ] -> []
  | DELIMITER SEMICOLON :: t -> t
  | _ :: t -> skip_till_semicolon t

let expected_err f s = Printf.sprintf "expected '%s'; got '%s'" f s

let rec parse_expression (tokens : token list) (prec : precedence) :
    (expression * token list) option =
  ignore prec;
  match parse_prefix tokens with
  | Some (_, OPERATOR x :: _) when Lexer.is_infix_operator x ->
      parse_infix tokens
  | Some _ as res -> res
  | None -> None

and parse_prefix (tokens : token list) : (expression * token list) option =
  match tokens with
  | IDENT x :: DELIMITER SEMICOLON :: t -> Some (Identifier (IDENT x), t)
  | INT x :: t -> Some (IntegerLiteral (INT x, int_of_string x), t)
  | OPERATOR op :: t when match op with MINUS | BANG -> true | _ -> false -> (
      match parse_expression t PREFIX with
      | Some (expr, rest) ->
          Some (Prefix (OPERATOR op, Lexer.string_of_operator op, expr), rest)
      | None -> None)
  | _ -> None

and parse_infix (tokens : token list) : (expression * token list) option =
  match parse_prefix tokens with
  | Some (lexpr, OPERATOR h :: rest) when Lexer.is_infix_operator h -> (
      let cur_prec = precedence_of_operator h in
      match parse_expression rest cur_prec with
      | Some (rexpr, rrest) ->
          Some (Infix (lexpr, OPERATOR h, string_of_operator h, rexpr), rrest)
      | None -> None)
  | _ -> None

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
      when match h with
           | INT _ | IDENT _ | OPERATOR MINUS | OPERATOR BANG -> true
           | _ -> false -> (
        match parse_expression tokens PREFIX with
        | Some (expr, rest) ->
            advance rest (ExpressionStatement expr :: stmts) errs
        | None -> advance t stmts ("Parse int err/ident" :: errs))
    | _ :: t -> advance t stmts errs
  in
  advance tokens [] []
