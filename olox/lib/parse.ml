open Ast

type parse_state = {
  lex_state : Lex.lex_state;
  mutable peek : Token.t;
  mutable prev : Token.t;
  mutable node_id_counter : int;
}

(* Utility functions *)

let init lex_state =
  let token = Lex.next_token lex_state in
  { lex_state; peek = token; prev = token; node_id_counter = 1 }

let next_id ps =
  let id = ps.node_id_counter in
  ps.node_id_counter <- id + 1;
  id

let advance ps =
  let token = ps.peek in
  if token = Token.EOF then ()
  else (
    ps.prev <- token;
    ps.peek <- Lex.next_token ps.lex_state)

let check ps tok = if ps.peek = Token.EOF then false else ps.peek = tok

let parser_error ps message =
  let token = ps.peek in
  let line = ps.lex_state.line in
  let lexeme = Lex.lexeme ps.lex_state in
  Common.compile_error token line lexeme message

let consume ps token message =
  if check ps token then advance ps
  else (
    parser_error ps message;
    failwith "Parse error")

let match_tokens ps toks =
  if List.exists (fun tok -> check ps tok) toks then (
    advance ps;
    true)
  else false

(* Expressions *)

let rec parse_expr ps = parse_assign ps

and parse_assign ps =
  let expr = parse_logic_or ps in
  let token = ps.peek in
  let line = ps.lex_state.line in
  let lexeme = Lex.lexeme ps.lex_state in
  if match_tokens ps [ Token.Equal ] then (
    let rval = parse_assign ps in
    match expr with
    | EXPR_Variable name -> EXPR_Assign (name, rval)
    | _ ->
        Common.compile_error token line lexeme "Invalid assignment target.";
        expr)
  else expr

and parse_logic_or ps =
  let lhs = parse_logic_and ps in
  let rec step lhs =
    if match_tokens ps [ Token.KWOr ] then
      let rhs = parse_logic_and ps in
      step (EXPR_Logical (lhs, LOGOP_or, rhs))
    else lhs
  in
  step lhs

and parse_logic_and ps =
  let lhs = parse_equality ps in
  let rec step lhs =
    if match_tokens ps [ Token.KWAnd ] then
      let rhs = parse_equality ps in
      step (EXPR_Logical (lhs, LOGOP_and, rhs))
    else lhs
  in
  step lhs

and parse_equality ps =
  let lhs = parse_comparison ps in
  let rec step lhs =
    if match_tokens ps [ Token.BangEqual; Token.EqualEqual ] then
      let op = binop_of_tok ps.prev in
      let rhs = parse_comparison ps in
      step (EXPR_Binary (lhs, op, rhs))
    else lhs
  in
  step lhs

and parse_comparison ps =
  let lhs = parse_term ps in
  let rec step lhs =
    if
      match_tokens ps
        [ Token.Greater; Token.GreaterEqual; Token.Less; Token.LessEqual ]
    then
      let op = binop_of_tok ps.prev in
      let rhs = parse_term ps in
      step (EXPR_Binary (lhs, op, rhs))
    else lhs
  in
  step lhs

and parse_term ps =
  let lhs = parse_factor ps in
  let rec step lhs =
    if match_tokens ps [ Token.Minus; Token.Plus ] then
      let op = binop_of_tok ps.prev in
      let rhs = parse_factor ps in
      step (EXPR_Binary (lhs, op, rhs))
    else lhs
  in
  step lhs

and parse_factor ps =
  let lhs = parse_unary ps in
  let rec step lhs =
    if match_tokens ps [ Token.Slash; Token.Star ] then
      let op = binop_of_tok ps.prev in
      let rhs = parse_unary ps in
      step (EXPR_Binary (lhs, op, rhs))
    else lhs
  in
  step lhs

and parse_unary ps =
  if match_tokens ps [ Token.Bang; Token.Minus ] then
    let op = unop_of_tok ps.prev in
    let inner_expr = parse_unary ps in
    EXPR_Unary (op, inner_expr)
  else parse_call ps

and finish_call ps callee =
  let rec step args =
    let arg = parse_expr ps in
    if match_tokens ps [ Token.Comma ] then step (arg :: args) else arg :: args
  in
  let args = if check ps Token.RightParen then [] else List.rev (step []) in
  consume ps Token.RightParen "Expect ')' after arguments.";
  if List.length args > 255 then
    parser_error ps "Can't have more than 255 arguments.";
  EXPR_Call (callee, args)

and parse_call ps =
  let expr = parse_primary ps in
  let rec step callee =
    if match_tokens ps [ Token.LeftParen ] then step (finish_call ps callee)
    else callee
  in
  step expr

and parse_primary ps =
  let tok = ps.peek in
  let () = advance ps in
  match tok with
  | Token.Number num -> EXPR_Literal (LIT_number num)
  | Token.String str -> EXPR_Literal (LIT_string str)
  | Token.KWTrue -> EXPR_Literal (LIT_bool true)
  | Token.KWFalse -> EXPR_Literal (LIT_bool false)
  | Token.KWNil -> EXPR_Literal LIT_nil
  | Token.LeftParen ->
      let expr = parse_expr ps in
      consume ps Token.RightParen "Expect ')' after expression.";
      EXPR_Grouping expr
  | Token.Identifier s -> EXPR_Variable { name = s; id = next_id ps }
  | _ ->
      parser_error ps "Expect expression.";
      failwith "Parse error"

(* Statements *)

let rec parse_stmt ps =
  if match_tokens ps [ Token.KWPrint ] then parse_print_stmt ps
  else if match_tokens ps [ Token.KWBreak ] then
    let _ = consume ps Token.Semicolon "Expect ';' after break." in
    STMT_Break
  else if match_tokens ps [ Token.KWFor ] then parse_for_stmt ps
  else if match_tokens ps [ Token.KWIf ] then parse_if_stmt ps
  else if match_tokens ps [ Token.KWWhile ] then parse_while_stmt ps
  else if match_tokens ps [ Token.KWReturn ] then parse_return_stmt ps
  else if match_tokens ps [ Token.LeftBrace ] then STMT_Block (parse_block ps)
  else parse_expr_stmt ps

and parse_return_stmt ps =
  let expr =
    match ps.peek with
    | Token.Semicolon -> None
    | _ -> Some (parse_expr ps)
  in
  let _ = consume ps Token.Semicolon "Expect ';' after return value." in
  STMT_Return expr

and parse_expr_stmt ps =
  let expr = parse_expr ps in
  let _ = consume ps Token.Semicolon "Expect ';' after expression." in
  STMT_Expression expr

and parse_if_stmt ps =
  let _ = consume ps Token.LeftParen "Expect '(' after 'if'." in
  let condition = parse_expr ps in
  let _ = consume ps Token.RightParen "Expect ')' after if condition." in
  let then_branch = parse_stmt ps in
  let else_branch =
    if match_tokens ps [ Token.KWElse ] then Some (parse_stmt ps) else None
  in
  STMT_If (condition, then_branch, else_branch)

and parse_for_stmt ps =
  let _ = consume ps Token.LeftParen "Expect '(' after 'for'." in
  let init =
    if match_tokens ps [ Token.Semicolon ] then None
    else if match_tokens ps [ Token.KWVar ] then Some (parse_var_decl ps)
    else Some (parse_expr_stmt ps)
  in
  let condition =
    if not (check ps Token.Semicolon) then Some (parse_expr ps) else None
  in
  let _ = consume ps Token.Semicolon "Expect ';' after loop condition." in
  let inc =
    if not (check ps Token.RightParen) then Some (parse_expr ps) else None
  in
  let _ = consume ps Token.RightParen "Expect ')' after for clauses." in
  let body = parse_stmt ps in
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

and parse_while_stmt ps =
  let _ = consume ps Token.LeftParen "Expect '(' after 'while'." in
  let condition = parse_expr ps in
  let _ = consume ps Token.RightParen "Expect ')' after while condition." in
  let body = parse_stmt ps in
  STMT_While (condition, body)

and parse_print_stmt ps =
  let expr = parse_expr ps in
  let _ = consume ps Token.Semicolon "Expect ';' after value." in
  STMT_Print expr

and parse_block ps =
  let rec step stmts =
    match ps.peek with
    | Token.RightBrace | Token.EOF -> stmts
    | _ -> step (parse_declaration ps :: stmts)
  in
  let stmts = step [] in
  let _ = consume ps Token.RightBrace "Expect '}' after block." in
  List.rev stmts

and parse_ident ps msg =
  match ps.peek with
  | Token.Identifier s ->
      advance ps;
      s
  | _ ->
      parser_error ps msg;
      failwith "Parser error."

and parse_fun_decl ps =
  let ident = parse_ident ps "Expect function name." in
  consume ps Token.LeftParen "Expect '(' after function name.";
  let rec parse_params params =
    let param = parse_ident ps "Expect parameter name." in
    if match_tokens ps [ Token.Comma ] then parse_params (param :: params)
    else param :: params
  in
  let params =
    if check ps Token.RightParen then [] else List.rev (parse_params [])
  in
  consume ps Token.RightParen "Expect ')' after parameters.";
  consume ps Token.LeftBrace "Expect '{' before function body.";
  let body = parse_block ps in
  if List.length params > 255 then
    parser_error ps "Can't have more than 255 parameters.";
  STMT_Fun (ident, params, body)

and parse_var_decl ps =
  let ident = parse_ident ps "Expect variable name." in
  let init_expr =
    if match_tokens ps [ Token.Equal ] then Some (parse_expr ps) else None
  in
  consume ps Token.Semicolon "Expect ';' after variable declaration.";
  STMT_Var (ident, init_expr)

and parse_declaration ps =
  if match_tokens ps [ Token.KWFun ] then parse_fun_decl ps
  else if match_tokens ps [ Token.KWVar ] then parse_var_decl ps
  else parse_stmt ps

let synchronize ps =
  advance ps;
  let rec step ps =
    if ps.prev = Token.Semicolon then ()
    else
      match ps.peek with
      | Token.EOF | Token.KWClass | Token.KWFun | Token.KWVar | Token.KWFor
      | Token.KWIf | Token.KWWhile | Token.KWPrint | Token.KWReturn ->
          ()
      | _ ->
          advance ps;
          step ps
  in
  step ps

let parse ls =
  let ps = init ls in
  let rec step stmts =
    if ps.peek = Token.EOF then stmts
    else
      try step (parse_declaration ps :: stmts)
      with Failure _ ->
        synchronize ps;
        step stmts
  in
  List.rev (step [])
