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

let check parser tok =
  if parser.peek.token = Token.EOF then false else parser.peek.token = tok

let parser_error parser message =
  Common.token_error parser.peek.token parser.peek.line parser.peek.lexeme
    message

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
program     → declaration* EOF ;

declaration → varDecl | statement ;
varDecl     → "var" IDENTIFIER ( "=" expression )? ";" ;
statement   → exprStmt | printStmt | block ;
exprStmt    → expression ";" ;
printStmt   → "print" expression ";" ;
block       → "{" declaration* "}" ;

expression  → assignment ;
assignment  → IDENTIFIER "=" assignment | equality ;
equality    → comparison ( ( "!=" | "==" ) comparison )* ;
comparison  → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term        → factor ( ( "-" | "+" ) factor )* ;
factor      → unary ( ( "/" | "*" ) unary )* ;
unary       → ( "!" | "-" ) unary | primary ;
primary     → "true" | "false" | "nil"
            | NUMBER | STRING
            | "(" expression ")"
            | IDENTIFIER ;

*)

(* Parsing actions *)

(* expression → assignment ; *)
let rec parse_expr parser = parse_assign parser

(* assignment → IDENTIFIER "=" assignment | equality ; *)
and parse_assign parser =
  let expr = parse_equality parser in
  if match_tokens parser [ Token.Equal ] then (
    let rval = parse_assign parser in
    match expr with
    | EXPR_Variable name -> EXPR_Assign (name, rval)
    | _ ->
        parser_error parser "Invalid assignment target.";
        expr)
  else expr

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
    if
      match_tokens parser
        [ Token.Greater; Token.GreaterEqual; Token.Less; Token.LessEqual ]
    then
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
  | Token.Identifier s -> EXPR_Variable s
  | _ ->
      parser_error parser "Expect expression.";
      failwith "Parse error"

(* statement → exprStmt | printStmt | block ; *)
let rec parse_stmt parser =
  if match_tokens parser [ Token.KWPrint ] then parse_print_stmt parser
  else if match_tokens parser [ Token.LeftBrace ] then
    STMT_Block (parse_block parser)
  else parse_expr_stmt parser

(* printStmt → "print" expression ";" ; *)
and parse_print_stmt parser =
  let expr = parse_expr parser in
  let _ = consume parser Token.Semicolon "Expect ';' after value." in
  STMT_Print expr

(* block → "{" declaration* "}" ; *)
and parse_block parser =
  let rec step stmts =
    match parser.peek.token with
    | Token.RightBrace | Token.EOF -> stmts
    | _ -> step (parse_declaration parser :: stmts)
  in
  let stmts = step [] in
  let _ = consume parser Token.RightBrace "Expect '}' after block." in
  List.rev stmts

(* exprStmt → expression ";" ; *)
and parse_expr_stmt parser =
  let expr = parse_expr parser in
  let _ = consume parser Token.Semicolon "Expect ';' after expression." in
  STMT_Expression expr

(* varDecl → "var" IDENTIFIER ( "=" expression )? ";" ; *)
and parse_var_decl parser =
  let ident =
    match parser.peek.token with
    | Token.Identifier s ->
        advance parser;
        s
    | _ -> failwith "Expect variable name."
  in
  let init_expr =
    if match_tokens parser [ Token.Equal ] then Some (parse_expr parser)
    else None
  in
  consume parser Token.Semicolon "Expect ';' after variable declaration.";
  STMT_Var (ident, init_expr)

(* declaration → varDecl | statement ; *)
and parse_declaration parser =
  if match_tokens parser [ Token.KWVar ] then parse_var_decl parser
  else parse_stmt parser

(* program → declaration* EOF ; *)
let parse parser =
  let rec step stmts =
    if parser.peek.token = Token.EOF then stmts
    else step (parse_declaration parser :: stmts)
  in
  try List.rev (step []) with Failure _e -> []

let _synchronize parser =
  advance parser;
  let rec step parser =
    if parser.prev.token = Token.Semicolon then ()
    else
      match parser.peek.token with
      | Token.EOF | Token.KWClass | Token.KWFun | Token.KWVar | Token.KWFor
      | Token.KWIf | Token.KWWhile | Token.KWPrint | Token.KWReturn ->
          ()
      | _ ->
          advance parser;
          step parser
  in
  step parser
