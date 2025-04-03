open Lexer
open Ast

let rec skip_till_semicolon = function
  | [] | [ EOF ] -> [ EOF ]
  | DELIMITER SEMICOLON :: t -> t
  | _ :: t -> skip_till_semicolon t

let expected_err f s = Printf.sprintf "expected '%s'; got '%s'" f s

let infixable (prec : precedence) (op : token) : bool =
  is_infix_operator op && precedence_value prec < token_precendence_value op

let split_idents_by_comma (tokens : token list) (endToken : token) :
    expression list * token list * string list =
  let rec parse_function_parameters' t params errs =
    match t with
    | [] | [ EOF ] -> (params, [ EOF ], errs)
    | IDENT x :: p :: tail when p = endToken ->
        (Identifier (IDENT x) :: params, tail, errs)
    | IDENT x :: DELIMITER COMMA :: (IDENT _ :: _ as tail) ->
        parse_function_parameters' tail (Identifier (IDENT x) :: params) errs
    | p :: tail when p = endToken -> (params, tail, errs)
    | _ -> (params, t, "Parsing function parameters err" :: errs)
  in
  match parse_function_parameters' tokens [] [] with
  | params, rest, errs -> (List.rev params, rest, errs)

let rec parse_expression (tokens : token list) (prec : precedence) :
    (expression * token list) option =
  match parse_prefix tokens with
  | Some (lexpr, rest) -> parse_infix lexpr rest prec
  | None -> None

and parse_prefix (tokens : token list) : (expression * token list) option =
  match tokens with
  | IDENT x :: t -> Some (Identifier (IDENT x), t)
  | INT x :: t -> Some (IntegerLiteral (INT x, int_of_string x), t)
  | STRING x :: t -> Some (StringLiteral (STRING x, x), t)
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
  | PAREN LBRACKET :: t ->
      let elements, after_params, _ (* Errs *) =
        parse_call_arguments t (PAREN RBRACKET)
      in
      Some (build_array_literal elements, after_params)
  | _ -> None

and parse_infix (lexpr : expression) (tokens : token list) (prec : precedence) :
    (expression * token list) option =
  match tokens with
  | (OPERATOR op as op_token) :: rest when infixable prec op_token -> (
      let cur_prec = precedence_of_token op_token in
      match parse_expression rest cur_prec with
      | Some (rexpr, rrest) ->
          parse_infix
            (Infix (lexpr, op_token, string_of_operator op, rexpr))
            rrest prec
      | None -> Some (lexpr, tokens))
  | PAREN LPAREN :: rest ->
      let exprs, after_args, _ (* args_errs *) =
        parse_call_arguments rest (PAREN RPAREN)
      in
      Some (CallExpression (PAREN LPAREN, lexpr, exprs), after_args)
  | PAREN LBRACKET :: rest -> (
      match parse_expression rest LOWEST with
      | Some (rexpr, PAREN RBRACKET :: rrest) ->
          parse_infix
            (IndexExpression (PAREN LBRACKET, lexpr, rexpr))
            rrest prec
      | None -> Some (lexpr, tokens)
      | _ -> None)
  | _ -> Some (lexpr, tokens)

and parse_call_arguments (tokens : token list) (endToken : token) :
    expression list * token list * string list =
  let rec parse_call_arguments' toks exprs errs =
    match toks with
    | [] | [ EOF ] -> (exprs, toks, errs)
    | p :: rest when p = endToken -> (exprs, rest, errs)
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
    | [] | [ EOF ] ->
        ({ statements = List.rev stmts; errors = List.rev errs }, [ EOF ])
    | PAREN RBRACE :: t ->
        ({ statements = List.rev stmts; errors = List.rev errs }, t)
    | KEYWORD LET
      :: IDENT ident
      :: OPERATOR ASSIGN
      :: KEYWORD IF
      :: PAREN LPAREN
      :: t -> (
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
                          (build_let_statement ident
                             (build_if_expr cond cons (Some alter))
                          :: stmts)
                          (alter_errs @ errs))
                | rest ->
                    advance rest
                      (build_let_statement ident (build_if_expr cond cons None)
                      :: stmts)
                      (cons_errs @ errs)))
        | None -> advance t stmts ("Parse if err" :: errs))
    | KEYWORD LET
      :: IDENT ident
      :: OPERATOR ASSIGN
      :: KEYWORD FUNCTION
      :: PAREN LPAREN
      :: t -> (
        let params, after_params, param_errs =
          split_idents_by_comma t (PAREN RPAREN)
        in
        match after_params with
        | PAREN LBRACE :: rest -> (
            match advance rest [] [] with
            | ( { statements = body_stmts; errors = body_errs },
                PAREN LPAREN :: after_body ) ->
                let call_params, after_call, call_errs =
                  parse_call_arguments after_body (PAREN RPAREN)
                in
                advance after_call
                  (build_let_statement ident
                     (build_call
                        (build_fn_literal params body_stmts)
                        call_params)
                  :: stmts)
                  (body_errs @ call_errs @ errs)
            | { statements = body_stmts; errors = body_errs }, after_body ->
                advance after_body
                  (build_let_statement ident
                     (build_fn_literal params body_stmts)
                  :: stmts)
                  (param_errs @ body_errs @ errs))
        | rest -> advance rest stmts errs)
    | KEYWORD LET :: IDENT ident :: OPERATOR ASSIGN :: PAREN LBRACKET :: t ->
        let elements, after_params, param_errs =
          parse_call_arguments t (PAREN RBRACKET)
        in
        advance after_params
          (build_let_statement ident (build_array_literal elements) :: stmts)
          (param_errs @ errs)
    | KEYWORD LET :: IDENT ident :: OPERATOR ASSIGN :: t -> (
        match parse_expression t LOWEST with
        | Some (expr, after_exrp) ->
            advance after_exrp (build_let_statement ident expr :: stmts) errs
        | None ->
            advance (skip_till_semicolon t) stmts
              ("Parsing let body err" :: errs))
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
    | KEYWORD RETURN :: t -> (
        match parse_expression t LOWEST with
        | Some (expr, after_exrp) ->
            advance after_exrp (ReturnStatement expr :: stmts) errs
        | None ->
            advance (skip_till_semicolon t) stmts
              ("Parsing return body err" :: errs))
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
                          (ExpressionStatement
                             (build_if_expr cond cons (Some alter))
                          :: stmts)
                          (alter_errs @ errs))
                | rest ->
                    advance rest
                      (ExpressionStatement (build_if_expr cond cons None)
                      :: stmts)
                      (cons_errs @ errs)))
        | None -> advance t stmts ("Parse if err" :: errs))
    | KEYWORD IF :: h :: t ->
        advance t stmts (expected_err "'('" (string_of_token h) :: errs)
    | KEYWORD FUNCTION :: PAREN LPAREN :: t -> (
        let params, after_params, param_errs =
          split_idents_by_comma t (PAREN RPAREN)
        in
        match after_params with
        | PAREN LBRACE :: rest -> (
            match advance rest [] [] with
            | ( { statements = body_stmts; errors = body_errs },
                PAREN LPAREN :: after_body ) ->
                let call_params, after_call, call_errs =
                  parse_call_arguments after_body (PAREN RPAREN)
                in
                advance after_call
                  (ExpressionStatement
                     (build_call
                        (build_fn_literal params body_stmts)
                        call_params)
                  :: stmts)
                  (body_errs @ call_errs @ errs)
            | { statements = body_stmts; errors = body_errs }, after_body ->
                advance after_body
                  (ExpressionStatement (build_fn_literal params body_stmts)
                  :: stmts)
                  (param_errs @ body_errs @ errs))
        | rest -> advance rest stmts errs)
    | h :: t
      when match h with
           | INT _ | IDENT _ | STRING _
           | OPERATOR MINUS
           | OPERATOR BANG
           | KEYWORD TRUE
           | KEYWORD FALSE
           | PAREN LPAREN
           | PAREN LBRACKET ->
               true
           | _ -> false -> (
        match parse_expression tokens LOWEST with
        | Some (expr, rest) ->
            advance rest (ExpressionStatement expr :: stmts) errs
        | None -> advance t stmts ("Parse int/ident/-/! err" :: errs))
    | _ :: t -> advance t stmts errs
  in
  match advance tokens [] [] with prog, _ -> prog
