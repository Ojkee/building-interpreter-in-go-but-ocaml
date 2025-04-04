open Lexer
open Ast

let expected_err f s = Printf.sprintf "expected '%s'; got '%s'" f s

let infixable (prec : precedence) (op : token) : bool =
  is_infix_operator op && precedence_value prec < token_precendence_value op

let is_prefix_op = function BANG | MINUS -> true | _ -> false

let rec skip_till_semicolon (tokens : token list) : token list =
  match tokens with
  | [] | [ EOF ] -> [ EOF ]
  | DELIMITER SEMICOLON :: t -> t
  | _ :: t -> skip_till_semicolon t

let split_idents_by_comma (endToken : token) (tokens : token list) :
    (expression list, string) result * token list =
  let rec split_idents_by_comma' idents tokens =
    match tokens with
    | [] | [ EOF ] -> (Ok idents, [ EOF ])
    | (IDENT _ as ident) :: p :: tail when p = endToken ->
        (Ok (Identifier ident :: idents), tail)
    | (IDENT _ as ident) :: DELIMITER COMMA :: (IDENT _ :: _ as tail) ->
        split_idents_by_comma' (Identifier ident :: idents) tail
    | p :: tail when p = endToken -> (Ok idents, tail)
    | _ -> (Error "split_idents_by_comma err", tokens)
  in
  match split_idents_by_comma' [] tokens with
  | Ok idents, rest -> (Ok (List.rev idents), rest)
  | err -> err

let rec parse_let_statement (tokens : token list) :
    (statement, string) result * token list =
  match tokens with
  | [] | [ EOF ] -> (Error "parse let statement: EOF", [ EOF ])
  | KEYWORD LET :: IDENT ident :: OPERATOR ASSIGN :: rest -> (
      match parse_expression LOWEST rest with
      | Ok expr, after_expr_tokens ->
          (Ok (build_let_statement ident expr), after_expr_tokens)
      | (Error _ as err), _ -> (err, skip_till_semicolon rest))
  | KEYWORD LET :: (a :: b :: _ as rest) -> (
      match (a, b) with
      | IDENT _, b ->
          ( Error (expected_err "=" (string_of_token b)),
            skip_till_semicolon rest )
      | a, _ ->
          ( Error (expected_err "IDENT" (string_of_token a)),
            skip_till_semicolon rest ))
  | _ -> (Error "parse let: Unreachable", skip_till_semicolon tokens)

and parse_return_statement (tokens : token list) :
    (statement, string) result * token list =
  let parse_return_statement' tokens =
    match tokens with
    | [] | [ EOF ] -> (Error "Parsing return statement err", [ EOF ])
    | KEYWORD RETURN :: rest -> (
        match parse_expression LOWEST rest with
        | Ok expr, after_expr_tokens ->
            (Ok (ReturnStatement expr), after_expr_tokens)
        | (Error _ as err), _ -> (err, skip_till_semicolon rest))
    | _ -> (Error "parse return: Unreachable", skip_till_semicolon tokens)
  in
  match parse_return_statement' tokens with
  | (Ok _ as expr), DELIMITER SEMICOLON :: rest -> (expr, rest)
  | res -> res

and parse_prefix (tokens : token list) :
    (expression, string) result * token list =
  match tokens with
  | [] | [ EOF ] -> (Error "parse prefix: EOF", [ EOF ])
  | IDENT x :: t -> (Ok (Identifier (IDENT x)), t)
  | INT x :: t -> (Ok (IntegerLiteral (INT x, int_of_string x)), t)
  | STRING x :: t -> (Ok (StringLiteral (STRING x, x)), t)
  | KEYWORD TRUE :: t -> (Ok (Boolean (KEYWORD TRUE, true)), t)
  | KEYWORD FALSE :: t -> (Ok (Boolean (KEYWORD FALSE, false)), t)
  | PAREN LPAREN :: t -> (
      match parse_expression LOWEST t with
      | Ok expr, PAREN RPAREN :: rest -> (Ok expr, rest)
      | Ok _, rest -> (Error "Should be `)` after expression", rest)
      | (Error _ as err), rest -> (err, rest))
  | PAREN LBRACKET :: t -> (
      match split_expressions_by_camma (PAREN RBRACKET) t with
      | Ok exprs, rest -> (Ok (build_array_literal exprs), rest)
      | (Error _ as err), rest -> (err, rest))
  | OPERATOR op :: t when is_prefix_op op -> (
      match parse_expression PREFIX t with
      | Ok expr, rest ->
          (Ok (Prefix (OPERATOR op, string_of_operator op, expr)), rest)
      | no_expr -> no_expr)
  | KEYWORD IF :: PAREN LPAREN :: t -> parse_if t
  | KEYWORD IF :: t -> (Error "Should be `(` after `if`", t)
  | KEYWORD FUNCTION :: PAREN LPAREN :: t -> parse_fn t
  | KEYWORD FUNCTION :: t -> (Error "Should be `(` after `fn`", t)
  | DELIMITER SEMICOLON :: t -> parse_prefix t
  | h :: t ->
      ( Error
          (Printf.sprintf "parse prefix: Unimplemented token `%s`"
             (string_of_token h)),
        t )

and parse_infix (lhs : expression) (prec : precedence) (tokens : token list) :
    (expression, string) result * token list =
  match tokens with
  | [] | [ EOF ] -> (Ok lhs, [ EOF ])
  | DELIMITER SEMICOLON :: _ -> (Ok lhs, tokens)
  | PAREN LPAREN :: t -> (
      match split_expressions_by_camma (PAREN RPAREN) t with
      | Ok exprs, rest -> parse_infix (build_call lhs exprs) prec rest
      | (Error _ as err), rest -> (err, rest))
  | PAREN LBRACKET :: t -> (
      match parse_expression LOWEST t with
      | Ok idx, PAREN RBRACKET :: rest ->
          parse_infix (build_index_expression lhs idx) prec rest
      | Ok _, rest -> (Error "Should be `]` after index expression", rest)
      | (Error _ as err), rest -> (err, rest))
  | h :: t when infixable prec h -> (
      let cur_prec = precedence_of_token h in
      match parse_expression cur_prec t with
      | Ok rhs, rest ->
          parse_infix (Infix (lhs, h, string_of_token h, rhs)) prec rest
      | Error _, rest -> (Ok lhs, rest))
  | _ -> (Ok lhs, tokens)

and parse_expression (prec : precedence) (tokens : token list) :
    (expression, string) result * token list =
  match parse_prefix tokens with
  | Ok expr, rest -> parse_infix expr prec rest
  | (Error _ as err), rest -> (err, rest)

and parse_if (tokens : token list) : (expression, string) result * token list =
  let parse_if' tokens =
    match parse_expression LOWEST tokens with
    | Ok cond, PAREN RPAREN :: PAREN LBRACE :: after_cond -> (
        match parse_block_statements after_cond with
        | Ok cons_stmts, KEYWORD ELSE :: PAREN LBRACE :: after_cons -> (
            match parse_block_statements after_cons with
            | Ok alter_stmts, after_alter ->
                ( Ok (build_if_expr cond cons_stmts (Some alter_stmts)),
                  after_alter )
            | (Error _ as err), after_alter -> (err, after_alter))
        | Ok cons_stmts, after_cons ->
            (Ok (build_if_expr cond cons_stmts None), after_cons)
        | (Error _ as err), after_cons -> (err, after_cons))
    | Ok _, after_cond ->
        (Error "Should be `) {` after if condition", after_cond)
    | no_expr -> no_expr
  in
  match parse_if' tokens with
  | (Ok _ as res), DELIMITER SEMICOLON :: rest -> (res, rest)
  | res -> res

and parse_fn (tokens : token list) : (expression, string) result * token list =
  match split_idents_by_comma (PAREN RPAREN) tokens with
  | Ok idents, after_params -> (
      match after_params with
      | PAREN LBRACE :: after_lbrace -> (
          match parse_block_statements after_lbrace with
          | Ok stmts, after_fn_body ->
              (Ok (build_fn_literal idents stmts), after_fn_body)
          | (Error _ as err), after_fn_body -> (err, after_fn_body))
      | _ ->
          (Error "Should be `{` after parameters of the function", after_params)
      )
  | (Error _ as err), after_params -> (err, after_params)

and parse_block_statements (tokens : token list) :
    (statement list, string) result * token list =
  let rec parse_block_statements' stmts tokens =
    match (tokens, parse_statement tokens) with
    | PAREN RBRACE :: t, _ -> (Ok stmts, t)
    | [], _ | [ EOF ], _ -> (Ok stmts, [ EOF ])
    | _, ((Error _ as err), rest) -> (err, rest)
    | _, (Ok stmt, rest) -> parse_block_statements' (stmt :: stmts) rest
  in
  match parse_block_statements' [] tokens with
  | Ok stmts, rest -> (Ok (List.rev stmts), rest)
  | err -> err

and split_expressions_by_camma (endToken : token) (tokens : token list) :
    (expression list, string) result * token list =
  let rec split_expressions_by_camma' exprs tokens =
    match tokens with
    | [] | [ EOF ] -> (Error "split expression by comma: EOF", [ EOF ])
    | p :: t when p = endToken -> (Ok exprs, t)
    | DELIMITER COMMA :: t | t -> (
        match parse_expression LOWEST t with
        | Ok expr, rest -> split_expressions_by_camma' (expr :: exprs) rest
        | (Error _ as err), rest -> (err, rest))
  in
  match split_expressions_by_camma' [] tokens with
  | Ok exprs, rest -> (Ok (List.rev exprs), rest)
  | err -> err

and parse_expression_statement (tokens : token list) :
    (statement, string) result * token list =
  match parse_expression LOWEST tokens with
  | Ok expr, DELIMITER SEMICOLON :: rest | Ok expr, rest ->
      (Ok (ExpressionStatement expr), rest)
  | (Error _ as err), rest -> (err, rest)

and parse_statement (tokens : token list) :
    (statement, string) result * token list =
  match tokens with
  | KEYWORD LET :: _ -> parse_let_statement tokens
  | KEYWORD RETURN :: _ -> parse_return_statement tokens
  | _ -> parse_expression_statement tokens

and parse_statements (stmts : statement list) (errs : string list)
    (tokens : token list) : program =
  match tokens with
  | [] | [ EOF ] -> { statements = List.rev stmts; errors = List.rev errs }
  | DELIMITER SEMICOLON :: t -> parse_statements stmts errs t
  | _ -> (
      match parse_statement tokens with
      | Ok stmt, rest -> parse_statements (stmt :: stmts) errs rest
      | Error err, rest -> parse_statements stmts (err :: errs) rest)

let parse (tokens : token list) : program = parse_statements [] [] tokens
