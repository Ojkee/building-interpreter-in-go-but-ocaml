open Lexer
open Ast

let rec skip_till_semicolon = function
  | [] -> []
  | [ EOF ] -> []
  | DELIMITER SEMICOLON :: t -> t
  | _ :: t -> skip_till_semicolon t

let expected_err f s = Printf.sprintf "expected '%s'; got '%s'" f s

let rec parse_expression (tokens : token list) :
    (expression * token list) option =
  match parse_prefix tokens with
  | Some (_, _) as res -> res
  | None -> (
      match tokens with
      | IDENT x :: DELIMITER SEMICOLON :: t -> Some (Identifier (IDENT x), t)
      | IDENT _ :: t -> Some (PLACEHOLDER_EXPR, t) (* TODO: PARSE BODY *)
      | INT x :: t -> Some (IntegerLiteral (INT x, int_of_string x), t)
      | _ -> None)

and parse_prefix (tokens : token list) : (expression * token list) option =
  match tokens with
  | OPERATOR MINUS :: t -> (
      match parse_expression t with
      | Some (expr, rest) -> Some (Prefix (OPERATOR MINUS, "-", expr), rest)
      | None -> None)
  | OPERATOR BANG :: t -> (
      match parse_expression t with
      | Some (expr, rest) -> Some (Prefix (OPERATOR BANG, "!", expr), rest)
      | None -> None)
  | _ -> None

let is_ident_or_int = function INT _ | IDENT _ -> true | _ -> false

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
    | h :: t when match h with INT _ | IDENT _ -> true | _ -> false -> (
        match parse_expression tokens with
        | Some (expr, rest) ->
            advance rest (ExpressionStatement expr :: stmts) errs
        | None -> advance t stmts ("Parse int err/ident" :: errs))
    | h :: t when h = OPERATOR MINUS || h = OPERATOR BANG -> (
        match parse_prefix tokens with
        | Some (expr, rest) ->
            advance rest (ExpressionStatement expr :: stmts) errs
        | None -> advance t stmts ("Parse prefix err" :: errs))
    | _ :: t -> advance t stmts errs
  in
  advance tokens [] []
