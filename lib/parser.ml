open Lexer
open Ast

(* type precedence = *)
(*   | LOWEST *)
(*   | EQUALS *)
(*   | LESSGREATER *)
(*   | SUM *)
(*   | PRODUCT *)
(*   | PREFIX *)
(*   | CALL *)

(* let precedence_value = function *)
(*   | LOWEST -> 0 *)
(*   | EQUALS -> 1 *)
(*   | LESSGREATER -> 2 *)
(*   | SUM -> 3 *)
(*   | PRODUCT -> 4 *)
(*   | PREFIX -> 5 *)
(*   | CALL -> 6 *)

let rec skip_till_semicolon = function
  | [] -> []
  | [ EOF ] -> []
  | DELIMITER SEMICOLON :: t -> t
  | _ :: t -> skip_till_semicolon t

let expected_err f s = Printf.sprintf "expected '%s'; got '%s'" f s

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
    | IDENT x :: DELIMITER SEMICOLON :: t ->
        advance t
          (ExpressionStatement (IDENT x, Identifier (IDENT x)) :: stmts)
          errs
    | IDENT x :: t ->
        advance t
          (ExpressionStatement (IDENT x, Identifier (IDENT "a")) :: stmts)
          errs
    | INT x :: t ->
        advance t
          (ExpressionStatement (INT x, IntegerLiteral (int_of_string x))
          :: stmts)
          errs
    | _ :: t -> advance t stmts errs
  in
  advance tokens [] []
