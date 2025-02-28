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
statement   → exprStmt | forStmt | ifStmt | whileStmt | printStmt | block ;
exprStmt    → expression ";" ;
forStmt     → "for" "(" ( varDecl | exprStmt | ";" )
            expression? ";"
            expression? ")" statement ;
ifStmt      → "if" "(" expression ")" statement
            ( "else" statement )? ;
whileStmt   → "while" "(" expression ")" statement ;
printStmt   → "print" expression ";" ;
block       → "{" declaration* "}" ;


expression  → assignment ;
assignment  → IDENTIFIER "=" assignment | logic_or ;
logic_or    → logic_and ( "or" logic_and )* ;
logic_and   → equality ( "and" equality )* ;
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

(* assignment → IDENTIFIER "=" assignment | logic_or ; *)
and parse_assign parser =
  let expr = parse_logic_or parser in
  if match_tokens parser [ Token.Equal ] then (
    let rval = parse_assign parser in
    match expr with
    | EXPR_Variable name -> EXPR_Assign (name, rval)
    | _ ->
        parser_error parser "Invalid assignment target.";
        expr)
  else expr

(* logic_or    → logic_and ( "or" logic_and )* ; *)
and parse_logic_or parser =
  let lhs = parse_logic_and parser in
  let rec step parser lhs =
    if match_tokens parser [ Token.KWOr ] then
      let rhs = parse_logic_and parser in
      step parser (EXPR_Logical (lhs, LOGOP_or, rhs))
    else lhs
  in
  step parser lhs

(* logic_and   → equality ( "and" equality )* ; *)
and parse_logic_and parser =
  let lhs = parse_equality parser in
  let rec step parser lhs =
    if match_tokens parser [ Token.KWAnd ] then
      let rhs = parse_equality parser in
      step parser (EXPR_Logical (lhs, LOGOP_and, rhs))
    else lhs
  in
  step parser lhs

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

(* statement → exprStmt | forStmt | ifStmt | whileStmt| printStmt | block ; *)
let rec parse_stmt parser =
  if match_tokens parser [ Token.KWPrint ] then parse_print_stmt parser
  else if match_tokens parser [ Token.KWBreak ] then
    let _ = consume parser Token.Semicolon "Expect ';' after break." in
    STMT_Break
  else if match_tokens parser [ Token.KWFor ] then parse_for_stmt parser
  else if match_tokens parser [ Token.KWIf ] then parse_if_stmt parser
  else if match_tokens parser [ Token.KWWhile ] then parse_while_stmt parser
  else if match_tokens parser [ Token.LeftBrace ] then
    STMT_Block (parse_block parser)
  else parse_expr_stmt parser

(* exprStmt → expression ";" ; *)
and parse_expr_stmt parser =
  let expr = parse_expr parser in
  let _ = consume parser Token.Semicolon "Expect ';' after expression." in
  STMT_Expression expr

(* ifStmt → "if" "(" expression ")" statement ( "else" statement )? ; *)
and parse_if_stmt parser =
  let _ = consume parser Token.LeftParen "Expect '(' after 'if'." in
  let condition = parse_expr parser in
  let _ = consume parser Token.RightParen "Expect ')' after if condition." in
  let then_branch = parse_stmt parser in
  let else_branch =
    if match_tokens parser [ Token.KWElse ] then Some (parse_stmt parser)
    else None
  in
  STMT_If (condition, then_branch, else_branch)

(* forStmt → "for" "(" ( varDecl | exprStmt | ";" ) *)
(* expression? ";" *)
(* expression? ")" statement ; *)
and parse_for_stmt parser =
  let _ = consume parser Token.LeftParen "Expect '(' after 'for'." in
  let init =
    if match_tokens parser [ Token.Semicolon ] then None
    else if match_tokens parser [ Token.KWVar ] then
      Some (parse_var_decl parser)
    else Some (parse_expr_stmt parser)
  in
  let condition =
    if not (check parser Token.Semicolon) then Some (parse_expr parser)
    else None
  in
  let _ = consume parser Token.Semicolon "Expect ';' after loop condition." in
  let inc =
    if not (check parser Token.RightParen) then Some (parse_expr parser)
    else None
  in
  let _ = consume parser Token.RightParen "Expect ')' after for clauses." in
  let body = parse_stmt parser in
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

(* whileStmt → "while" "(" expression ")" statement ; *)
and parse_while_stmt parser =
  let _ = consume parser Token.LeftParen "Expect '(' after 'while'." in
  let condition = parse_expr parser in
  let _ = consume parser Token.RightParen "Expect ')' after while condition." in
  let body = parse_stmt parser in
  STMT_While (condition, body)

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
