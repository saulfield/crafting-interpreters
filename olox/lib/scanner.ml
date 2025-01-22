type token_info = { token : Token.t; lexeme : string; line : int } [@@deriving show]

type scanner = {
  src : string;
  tokens : token_info Queue.t;
  mutable start : int;
  mutable current : int;
  mutable line : int;
}

let get_identifier (s : string) : Token.t =
  match s with
  | "and" -> KWAnd
  | "class" -> KWClass
  | "else" -> KWElse
  | "false" -> KWFalse
  | "for" -> KWFor
  | "fun" -> KWFun
  | "if" -> KWIf
  | "nil" -> KWNil
  | "or" -> KWOr
  | "print" -> KWPrint
  | "return" -> KWReturn
  | "super" -> KWSuper
  | "this" -> KWThis
  | "true" -> KWTrue
  | "var" -> KWVar
  | "while" -> KWWhile
  | s -> Identifier s

let make_scanner src = { src; tokens = Queue.create (); start = 0; current = 0; line = 1 }

let add_token scanner token =
  let line = scanner.line in
  let lexeme = String.sub scanner.src scanner.start (scanner.current - scanner.start) in
  let token_info = { token; lexeme; line } in
  Queue.push token_info scanner.tokens

let is_at_end scanner = scanner.current >= String.length scanner.src
let peek scanner = if is_at_end scanner then '\000' else scanner.src.[scanner.current]
let advance scanner = scanner.current <- scanner.current + 1

let match_char scanner c =
  if is_at_end scanner then false
  else if peek scanner <> c then false
  else (
    advance scanner;
    true)

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alphanumeric c = is_alpha c || is_digit c

let scan_comment scanner =
  while peek scanner <> '\n' && not (is_at_end scanner) do
    advance scanner
  done

let scan_string scanner =
  let rec aux () =
    match peek scanner with
    | '"' -> advance scanner
    | '\n' -> scanner.line <- scanner.line + 1
    | '\000' -> Common.error scanner.line "Unterminated string"
    | _ ->
        advance scanner;
        aux ()
  in
  let () = aux () in
  let start_pos = scanner.start + 1 in
  let end_pos = scanner.current - 1 in
  let str_val = String.sub scanner.src start_pos (end_pos - start_pos) in
  add_token scanner (String str_val)

let scan_digits scanner =
  while is_digit (peek scanner) do
    advance scanner
  done

let scan_number scanner =
  scan_digits scanner;
  if match_char scanner '.' then scan_digits scanner;
  let s = String.sub scanner.src scanner.start (scanner.current - scanner.start) in
  try
    let num_val = float_of_string s in
    add_token scanner (Number num_val)
  with Failure _ ->
    Printf.printf "Failed on '%s'\n" s;
    exit (-1)

let scan_identifier scanner =
  while is_alphanumeric (peek scanner) do
    advance scanner
  done;
  let id = String.sub scanner.src scanner.start (scanner.current - scanner.start) in
  let token = get_identifier id in
  add_token scanner token

let scan_token scanner =
  let c = peek scanner in
  let () = advance scanner in
  match c with
  | '(' -> add_token scanner LeftParen
  | ')' -> add_token scanner RightParen
  | '{' -> add_token scanner LeftBrace
  | '}' -> add_token scanner RightBrace
  | ',' -> add_token scanner Comma
  | '.' -> add_token scanner Dot
  | '-' -> add_token scanner Minus
  | '+' -> add_token scanner Plus
  | ';' -> add_token scanner Semicolon
  | '*' -> add_token scanner Star
  | '!' -> add_token scanner (if match_char scanner '=' then BangEqual else Bang)
  | '=' -> add_token scanner (if match_char scanner '=' then EqualEqual else Equal)
  | '<' -> add_token scanner (if match_char scanner '=' then LessEqual else Less)
  | '>' -> add_token scanner (if match_char scanner '=' then GreaterEqual else Greater)
  | '/' -> if peek scanner = '/' then scan_comment scanner else add_token scanner Slash
  | ' ' | '\r' | '\t' -> ()
  | '\n' -> scanner.line <- scanner.line + 1
  | '"' -> scan_string scanner
  | '0' .. '9' -> scan_number scanner
  | c when is_alpha c -> scan_identifier scanner
  | _ -> Common.error scanner.line "Unexpected character."

let scan_tokens scanner =
  while not (is_at_end scanner) do
    scanner.start <- scanner.current;
    scan_token scanner
  done;
  add_token scanner Token.EOF;
  scanner.tokens
