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

let advance parser =
  if (Queue.peek parser.tokens).token <> Token.EOF then (
    parser.prev <- Queue.pop parser.tokens;
    parser.peek <- Queue.peek parser.tokens)

let check parser tok = if parser.peek.token = Token.EOF then false else parser.peek.token = tok

let parser_error parser message =
  Common.token_error parser.peek.token parser.peek.line parser.peek.lexeme message

let consume parser token message =
  if check parser token then advance parser
  else (
    parser_error parser message;
    failwith "Parse error")

let match_tokens parser toks =
  if List.exists (fun tok -> check parser tok) toks then (
    advance parser;
    true)
  else false

(* 

Grammar
-------
program    → statement* EOF ;
statement  → exprStmt | printStmt ;
exprStmt   → expression ";" ;
printStmt  → "print" expression ";" ;
expression → equality ;
equality   → comparison ( ( "!=" | "==" ) comparison )* ;
comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term       → factor ( ( "-" | "+" ) factor )* ;
factor     → unary ( ( "/" | "*" ) unary )* ;
unary      → ( "!" | "-" ) unary | primary ;
primary    → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

*)

(* Parsing actions *)

(* expression → equality ; *)
let rec parse_expr parser = parse_equality parser

(* equality → comparison ( ( "!=" | "==" ) comparison )* ; *)
and parse_equality parser =
  let lhs = parse_comparison parser in
  let rec step parser lhs =
    if match_tokens parser [ Token.BangEqual; Token.EqualEqual ] then
      let op = binop_of_tok parser.prev.token in
      let rhs = parse_comparison parser in
      step parser (EXPR_Binary (lhs, op, rhs))
    else lhs
  in
  step parser lhs

(* comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ; *)
and parse_comparison parser =
  let lhs = parse_term parser in
  let rec step parser lhs =
    if match_tokens parser [ Token.Greater; Token.GreaterEqual; Token.Less; Token.LessEqual ] then
      let op = binop_of_tok parser.prev.token in
      let rhs = parse_term parser in
      step parser (EXPR_Binary (lhs, op, rhs))
    else lhs
  in
  step parser lhs

(* term → factor ( ( "-" | "+" ) factor )* ; *)
and parse_term parser =
  let lhs = parse_factor parser in
  let rec step parser lhs =
    if match_tokens parser [ Token.Minus; Token.Plus ] then
      let op = binop_of_tok parser.prev.token in
      let rhs = parse_factor parser in
      step parser (EXPR_Binary (lhs, op, rhs))
    else lhs
  in
  step parser lhs

(* factor → unary ( ( "/" | "*" ) unary )* ; *)
and parse_factor parser =
  let lhs = parse_unary parser in
  let rec step parser lhs =
    if match_tokens parser [ Token.Slash; Token.Star ] then
      let op = binop_of_tok parser.prev.token in
      let rhs = parse_unary parser in
      step parser (EXPR_Binary (lhs, op, rhs))
    else lhs
  in
  step parser lhs

(* unary → ( "!" | "-" ) unary | primary ; *)
and parse_unary parser =
  if match_tokens parser [ Token.Bang; Token.Minus ] then
    let op = unop_of_tok parser.prev.token in
    let inner_expr = parse_unary parser in
    EXPR_Unary (op, inner_expr)
  else parse_primary parser

(* primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ; *)
and parse_primary parser =
  let tok = parser.peek.token in
  let () = advance parser in
  match tok with
  | Token.Number num -> EXPR_Literal (LIT_number num)
  | Token.String str -> EXPR_Literal (LIT_string str)
  | Token.KWTrue -> EXPR_Literal (LIT_bool true)
  | Token.KWFalse -> EXPR_Literal (LIT_bool false)
  | Token.KWNil -> EXPR_Literal LIT_nil
  | Token.LeftParen ->
      let expr = parse_expr parser in
      consume parser Token.RightParen "Expect ')' after expression.";
      expr
  | _ ->
      parser_error parser "Expect expression.";
      failwith "Parse error"

(* printStmt → "print" expression ";" ; *)
let parse_print_stmt parser =
  let expr = parse_expr parser in
  let _ = consume parser Token.Semicolon "Expect ';' after value." in
  STMT_Print expr

(* exprStmt → expression ";" ; *)
let parse_expr_stmt parser =
  let expr = parse_expr parser in
  let _ = consume parser Token.Semicolon "Expect ';' after expression." in
  STMT_Expression expr

(* statement → exprStmt | printStmt ; *)
let parse_stmt parser =
  if match_tokens parser [ Token.KWPrint ] then parse_print_stmt parser else parse_expr_stmt parser

(* program → statement* EOF ; *)
let parse parser =
  let rec step stmts =
    if parser.peek.token = Token.EOF then stmts else step (parse_stmt parser :: stmts)
  in
  try List.rev (step []) with Failure _e -> []

let _synchronize parser =
  advance parser;
  let rec step parser =
    if parser.prev.token = Token.Semicolon then ()
    else
      match parser.peek.token with
      | Token.EOF | Token.KWClass | Token.KWFun | Token.KWVar | Token.KWFor | Token.KWIf
      | Token.KWWhile | Token.KWPrint | Token.KWReturn ->
          ()
      | _ ->
          advance parser;
          step parser
  in
  step parser
