open Ast

type parser = {
  tokens : Scanner.token_info Queue.t;
  mutable peek : Scanner.token_info;
  mutable prev : Scanner.token_info;
}

(* Utility functions *)

let make_parser tokens =
  let peek = Queue.peek tokens in
  let prev = Queue.peek tokens in
  { tokens; peek; prev }

let advance p =
  if (Queue.peek p.tokens).token <> Token.EOF then (
    p.prev <- Queue.pop p.tokens;
    p.peek <- Queue.peek p.tokens)

let check p tok = if p.peek.token = Token.EOF then false else p.peek.token = tok

let parser_error p message =
  Common.token_error p.peek.token p.peek.line p.peek.lexeme message

let consume p token message =
  if check p token then advance p
  else (
    parser_error p message;
    failwith "Parse error")

let match_tokens p toks =
  if List.exists (fun tok -> check p tok) toks then (
    advance p;
    true)
  else false

(* Expressions *)

let rec parse_expr p = parse_assign p

and parse_assign p =
  let expr = parse_logic_or p in
  if match_tokens p [ Token.Equal ] then (
    let rval = parse_assign p in
    match expr with
    | EXPR_Variable name -> EXPR_Assign (name, rval)
    | _ ->
        parser_error p "Invalid assignment target.";
        expr)
  else expr

and parse_logic_or p =
  let lhs = parse_logic_and p in
  let rec step lhs =
    if match_tokens p [ Token.KWOr ] then
      let rhs = parse_logic_and p in
      step (EXPR_Logical (lhs, LOGOP_or, rhs))
    else lhs
  in
  step lhs

and parse_logic_and p =
  let lhs = parse_equality p in
  let rec step lhs =
    if match_tokens p [ Token.KWAnd ] then
      let rhs = parse_equality p in
      step (EXPR_Logical (lhs, LOGOP_and, rhs))
    else lhs
  in
  step lhs

and parse_equality p =
  let lhs = parse_comparison p in
  let rec step lhs =
    if match_tokens p [ Token.BangEqual; Token.EqualEqual ] then
      let op = binop_of_tok p.prev.token in
      let rhs = parse_comparison p in
      step (EXPR_Binary (lhs, op, rhs))
    else lhs
  in
  step lhs

and parse_comparison p =
  let lhs = parse_term p in
  let rec step lhs =
    if
      match_tokens p
        [ Token.Greater; Token.GreaterEqual; Token.Less; Token.LessEqual ]
    then
      let op = binop_of_tok p.prev.token in
      let rhs = parse_term p in
      step (EXPR_Binary (lhs, op, rhs))
    else lhs
  in
  step lhs

and parse_term p =
  let lhs = parse_factor p in
  let rec step lhs =
    if match_tokens p [ Token.Minus; Token.Plus ] then
      let op = binop_of_tok p.prev.token in
      let rhs = parse_factor p in
      step (EXPR_Binary (lhs, op, rhs))
    else lhs
  in
  step lhs

and parse_factor p =
  let lhs = parse_unary p in
  let rec step lhs =
    if match_tokens p [ Token.Slash; Token.Star ] then
      let op = binop_of_tok p.prev.token in
      let rhs = parse_unary p in
      step (EXPR_Binary (lhs, op, rhs))
    else lhs
  in
  step lhs

and parse_unary p =
  if match_tokens p [ Token.Bang; Token.Minus ] then
    let op = unop_of_tok p.prev.token in
    let inner_expr = parse_unary p in
    EXPR_Unary (op, inner_expr)
  else parse_call p

and finish_call p callee =
  let rec step args =
    let arg = parse_expr p in
    if match_tokens p [ Token.Comma ] then step (arg :: args) else arg :: args
  in
  let args = if check p Token.RightParen then [] else List.rev (step []) in
  consume p Token.RightParen "Expect ')' after arguments.";
  if List.length args > 255 then
    parser_error p "Can't have more than 255 arguments.";
  EXPR_Call (callee, args)

and parse_call p =
  let expr = parse_primary p in
  let rec step callee =
    if match_tokens p [ Token.LeftParen ] then step (finish_call p callee)
    else callee
  in
  step expr

and parse_primary p =
  let tok = p.peek.token in
  let () = advance p in
  match tok with
  | Token.Number num -> EXPR_Literal (LIT_number num)
  | Token.String str -> EXPR_Literal (LIT_string str)
  | Token.KWTrue -> EXPR_Literal (LIT_bool true)
  | Token.KWFalse -> EXPR_Literal (LIT_bool false)
  | Token.KWNil -> EXPR_Literal LIT_nil
  | Token.LeftParen ->
      let expr = parse_expr p in
      consume p Token.RightParen "Expect ')' after expression.";
      expr
  | Token.Identifier s -> EXPR_Variable s
  | _ ->
      parser_error p "Expect expression.";
      failwith "Parse error"

(* Statements *)

let rec parse_stmt p =
  if match_tokens p [ Token.KWPrint ] then parse_print_stmt p
  else if match_tokens p [ Token.KWBreak ] then
    let _ = consume p Token.Semicolon "Expect ';' after break." in
    STMT_Break
  else if match_tokens p [ Token.KWFor ] then parse_for_stmt p
  else if match_tokens p [ Token.KWIf ] then parse_if_stmt p
  else if match_tokens p [ Token.KWWhile ] then parse_while_stmt p
  else if match_tokens p [ Token.KWReturn ] then parse_return_stmt p
  else if match_tokens p [ Token.LeftBrace ] then STMT_Block (parse_block p)
  else parse_expr_stmt p

and parse_return_stmt p =
  let expr =
    match p.peek.token with
    | Token.Semicolon -> None
    | _ -> Some (parse_expr p)
  in
  let _ = consume p Token.Semicolon "Expect ';' after return value." in
  STMT_Return expr

and parse_expr_stmt p =
  let expr = parse_expr p in
  let _ = consume p Token.Semicolon "Expect ';' after expression." in
  STMT_Expression expr

and parse_if_stmt p =
  let _ = consume p Token.LeftParen "Expect '(' after 'if'." in
  let condition = parse_expr p in
  let _ = consume p Token.RightParen "Expect ')' after if condition." in
  let then_branch = parse_stmt p in
  let else_branch =
    if match_tokens p [ Token.KWElse ] then Some (parse_stmt p) else None
  in
  STMT_If (condition, then_branch, else_branch)

and parse_for_stmt p =
  let _ = consume p Token.LeftParen "Expect '(' after 'for'." in
  let init =
    if match_tokens p [ Token.Semicolon ] then None
    else if match_tokens p [ Token.KWVar ] then Some (parse_var_decl p)
    else Some (parse_expr_stmt p)
  in
  let condition =
    if not (check p Token.Semicolon) then Some (parse_expr p) else None
  in
  let _ = consume p Token.Semicolon "Expect ';' after loop condition." in
  let inc =
    if not (check p Token.RightParen) then Some (parse_expr p) else None
  in
  let _ = consume p Token.RightParen "Expect ')' after for clauses." in
  let body = parse_stmt p in
  let body =
    match inc with
    | Some expr -> STMT_Block [ body; STMT_Expression expr ]
    | None -> body
  in
  let body =
    match condition with
    | Some expr -> STMT_While (expr, body)
    | None -> STMT_While (EXPR_Literal (LIT_bool true), body)
  in
  match init with
  | Some stmt -> STMT_Block [ stmt; body ]
  | None -> body

and parse_while_stmt p =
  let _ = consume p Token.LeftParen "Expect '(' after 'while'." in
  let condition = parse_expr p in
  let _ = consume p Token.RightParen "Expect ')' after while condition." in
  let body = parse_stmt p in
  STMT_While (condition, body)

and parse_print_stmt p =
  let expr = parse_expr p in
  let _ = consume p Token.Semicolon "Expect ';' after value." in
  STMT_Print expr

and parse_block p =
  let rec step stmts =
    match p.peek.token with
    | Token.RightBrace | Token.EOF -> stmts
    | _ -> step (parse_declaration p :: stmts)
  in
  let stmts = step [] in
  let _ = consume p Token.RightBrace "Expect '}' after block." in
  List.rev stmts

and parse_ident p msg =
  match p.peek.token with
  | Token.Identifier s ->
      advance p;
      s
  | _ -> failwith msg

and parse_fun_decl p =
  let ident = parse_ident p "Expect function name." in
  consume p Token.LeftParen "Expect '(' after function name.";
  let rec parse_params params =
    let param = parse_ident p "Expect parameter name." in
    if match_tokens p [ Token.Comma ] then parse_params (param :: params)
    else param :: params
  in
  let params =
    if check p Token.RightParen then [] else List.rev (parse_params [])
  in
  consume p Token.RightParen "Expect ')' after parameters.";
  consume p Token.LeftBrace "Expect '{' before function body.";
  let body = parse_block p in
  if List.length params > 255 then
    parser_error p "Can't have more than 255 parameters.";
  STMT_Fun (ident, params, body)

and parse_var_decl p =
  let ident = parse_ident p "Expect variable name." in
  let init_expr =
    if match_tokens p [ Token.Equal ] then Some (parse_expr p) else None
  in
  consume p Token.Semicolon "Expect ';' after variable declaration.";
  STMT_Var (ident, init_expr)

and parse_declaration p =
  if match_tokens p [ Token.KWFun ] then parse_fun_decl p
  else if match_tokens p [ Token.KWVar ] then parse_var_decl p
  else parse_stmt p

let parse p =
  let rec step stmts =
    if p.peek.token = Token.EOF then stmts
    else step (parse_declaration p :: stmts)
  in
  try List.rev (step []) with Failure _e -> []

let _synchronize p =
  advance p;
  let rec step p =
    if p.prev.token = Token.Semicolon then ()
    else
      match p.peek.token with
      | Token.EOF | Token.KWClass | Token.KWFun | Token.KWVar | Token.KWFor
      | Token.KWIf | Token.KWWhile | Token.KWPrint | Token.KWReturn ->
          ()
      | _ ->
          advance p;
          step p
  in
  step p
