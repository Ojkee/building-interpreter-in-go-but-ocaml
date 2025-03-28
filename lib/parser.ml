open Lexer
open Ast

let rec skip_till_semicolon = function
  | [] -> [ EOF ]
  | [ EOF ] -> [ EOF ]
  | DELIMITER SEMICOLON :: t -> t
  | _ :: t -> skip_till_semicolon t

let expected_err f s = Printf.sprintf "expected '%s'; got '%s'" f s

let infixable (prec : precedence) (op : operator) : bool =
  is_infix_operator op
  && precedence_value prec < token_precendence_value (OPERATOR op)

let build_if_statement (cond : expression) (cons : statement list)
    (alter : statement list option) : statement =
  match alter with
  | Some alt_stmts ->
      ExpressionStatement
        (IfExpression
           ( KEYWORD IF,
             cond,
             Block (PAREN LBRACE, cons),
             Some (Block (PAREN LBRACE, alt_stmts)) ))
  | None ->
      ExpressionStatement
        (IfExpression (KEYWORD IF, cond, Block (PAREN LBRACE, cons), None))

let parse_function_parameters (tokens : token list) :
    token list * token list * string list =
  let rec parse_function_parameters' t params errs =
    match t with
    | [] -> (List.rev params, [ EOF ], errs)
    | [ EOF ] -> (List.rev params, [ EOF ], errs)
    | IDENT x :: PAREN RPAREN :: tail ->
        (List.rev (IDENT x :: params), tail, errs)
    | IDENT x :: DELIMITER COMMA :: (IDENT _ :: _ as tail) ->
        parse_function_parameters' tail (IDENT x :: params) errs
    | PAREN RPAREN :: tail -> (List.rev params, tail, errs)
    | _ -> (List.rev params, t, "Parsing function parameters err" :: errs)
  in
  parse_function_parameters' tokens [] []

let rec parse_expression (tokens : token list) (prec : precedence) :
    (expression * token list) option =
  match parse_prefix tokens with
  | Some (lexpr, rest) -> parse_infix lexpr rest prec
  | None -> None

and parse_prefix (tokens : token list) : (expression * token list) option =
  match tokens with
  | IDENT x :: t -> Some (Identifier (IDENT x), t)
  | INT x :: t -> Some (IntegerLiteral (INT x, int_of_string x), t)
  | KEYWORD TRUE :: t -> Some (Boolean (KEYWORD TRUE, true), t)
  | KEYWORD FALSE :: t -> Some (Boolean (KEYWORD FALSE, false), t)
  | OPERATOR op :: t when match op with MINUS | BANG -> true | _ -> false -> (
      match parse_expression t PREFIX with
      | Some (expr, rest) ->
          Some (Prefix (OPERATOR op, string_of_operator op, expr), rest)
      | None -> None)
  | PAREN LPAREN :: t -> (
      match parse_expression t LOWEST with
      | Some (expr, rest) -> (
          match rest with
          | PAREN RPAREN :: rrest -> Some (expr, rrest)
          | _ -> None)
      | None -> None)
  | _ -> None

and parse_infix (lexpr : expression) (tokens : token list) (prec : precedence) :
    (expression * token list) option =
  match tokens with
  | (OPERATOR op as op_token) :: rest when infixable prec op -> (
      let cur_prec = precedence_of_token op_token in
      match parse_expression rest cur_prec with
      | Some (rexpr, rrest) ->
          parse_infix
            (Infix (lexpr, op_token, string_of_operator op, rexpr))
            rrest prec
      | None -> Some (lexpr, tokens))
  | PAREN LPAREN :: rest ->
      let exprs, after_args, _ (* args_errs *) = parse_call_arguments rest in
      Some (CallExpression (PAREN LPAREN, lexpr, exprs), after_args)
  | _ -> Some (lexpr, tokens)

and parse_call_arguments (tokens : token list) :
    expression list * token list * string list =
  let rec parse_call_arguments' toks exprs errs =
    match toks with
    | [] -> (exprs, toks, errs)
    | [ EOF ] -> (exprs, toks, errs)
    | PAREN RPAREN :: rest -> (exprs, rest, errs)
    | DELIMITER COMMA :: rest -> (
        match parse_expression rest LOWEST with
        | Some (expr, after_arg) ->
            parse_call_arguments' after_arg (expr :: exprs) errs
        | None -> (exprs, toks, "Parse call arguments err" :: errs))
    | _ -> (
        match parse_expression toks LOWEST with
        | Some (expr, after_arg) ->
            parse_call_arguments' after_arg (expr :: exprs) errs
        | None -> (exprs, toks, "Parse call arguments err" :: errs))
  in
  match parse_call_arguments' tokens [] [] with
  | exprs, rest, errs -> (List.rev exprs, rest, errs)

let parse (tokens : token list) : program =
  let rec advance tokens stmts errs =
    match tokens with
    | [] -> ({ statements = List.rev stmts; errors = List.rev errs }, [ EOF ])
    | [ EOF ] ->
        ({ statements = List.rev stmts; errors = List.rev errs }, tokens)
    | PAREN RBRACE :: t ->
        ({ statements = List.rev stmts; errors = List.rev errs }, t)
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
           | INT _ | IDENT _
           | OPERATOR MINUS
           | OPERATOR BANG
           | KEYWORD TRUE
           | KEYWORD FALSE
           | PAREN LPAREN ->
               true
           | _ -> false -> (
        match parse_expression tokens LOWEST with
        | Some (expr, rest) ->
            advance rest (ExpressionStatement expr :: stmts) errs
        | None -> advance t stmts ("Parse int/ident/-/! err" :: errs))
    | KEYWORD IF :: PAREN LPAREN :: t -> (
        match parse_expression t LOWEST with
        | Some (cond, tokens_after_cond) -> (
            match advance tokens_after_cond [] [] with
            | ( { statements = cons; errors = cons_errs },
                tokens_after_consequence ) -> (
                match tokens_after_consequence with
                | KEYWORD ELSE :: PAREN LBRACE :: tokens_after_else -> (
                    match advance tokens_after_else [] [] with
                    | ( { statements = alter; errors = alter_errs },
                        tokens_after_alternative ) ->
                        advance tokens_after_alternative
                          (build_if_statement cond cons (Some alter) :: stmts)
                          (alter_errs @ errs))
                | rest ->
                    advance rest
                      (build_if_statement cond cons None :: stmts)
                      (cons_errs @ errs)))
        | None -> advance t stmts ("Parse if err" :: errs))
    | KEYWORD IF :: h :: t ->
        advance t stmts (expected_err "'('" (string_of_token h) :: errs)
    | KEYWORD FUNCTION :: PAREN LPAREN :: t -> (
        let params, after_params, param_errs = parse_function_parameters t in
        match after_params with
        | PAREN LBRACE :: rest -> (
            match advance rest [] [] with
            | { statements = body_stmts; errors = body_errs }, after_body ->
                advance after_body
                  (ExpressionStatement
                     (FunctionLiteral
                        ( KEYWORD FUNCTION,
                          params,
                          Block (PAREN LBRACE, body_stmts) ))
                  :: stmts)
                  (param_errs @ body_errs @ errs))
        | rest -> advance rest stmts errs)
    | _ :: t -> advance t stmts errs
  in
  match advance tokens [] [] with prog, _ -> prog
