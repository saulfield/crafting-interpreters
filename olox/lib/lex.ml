open Token
open Printf

type lex_state = {
  src : string;
  mutable start : int;
  mutable current : int;
  mutable line : int;
}

let init src = { src; start = 0; current = 0; line = 1 }
let is_at_end ls = ls.current >= String.length ls.src
let peek ls = if is_at_end ls then '\000' else ls.src.[ls.current]
let advance ls = ls.current <- ls.current + 1
let lexeme ls = String.sub ls.src ls.start (ls.current - ls.start)

let match_char ls c =
  if is_at_end ls then false
  else if peek ls <> c then false
  else (
    advance ls;
    true)

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alphanumeric c = is_alpha c || is_digit c

let lex_comment ls =
  while peek ls <> '\n' && not (is_at_end ls) do
    advance ls
  done

let lex_string ls =
  let rec aux () =
    match peek ls with
    | '"' -> advance ls
    | '\n' ->
        ls.line <- ls.line + 1;
        advance ls;
        aux ()
    | '\000' -> Common.error ls.line "Unterminated string"
    | _ ->
        advance ls;
        aux ()
  in
  let () = aux () in
  let start_pos = ls.start + 1 in
  let end_pos = ls.current - 1 in
  let str_val = String.sub ls.src start_pos (end_pos - start_pos) in
  String str_val

let lex_digits ls =
  while is_digit (peek ls) do
    advance ls
  done

let lex_number ls =
  lex_digits ls;
  if match_char ls '.' then lex_digits ls;
  let s = String.sub ls.src ls.start (ls.current - ls.start) in
  try
    let num_val = float_of_string s in
    Number num_val
  with Failure _ ->
    printf "Failed on '%s'\n" s;
    exit (-1)

let lex_identifier ls =
  while is_alphanumeric (peek ls) do
    advance ls
  done;
  let id = String.sub ls.src ls.start (ls.current - ls.start) in
  from_identifier id

let rec next_token ls =
  if is_at_end ls then EOF
  else
    let _ = ls.start <- ls.current in
    let c = peek ls in
    let _ = advance ls in
    match c with
    | '(' -> LeftParen
    | ')' -> RightParen
    | '{' -> LeftBrace
    | '}' -> RightBrace
    | ',' -> Comma
    | '.' -> Dot
    | '-' -> Minus
    | '+' -> Plus
    | ';' -> Semicolon
    | '*' -> Star
    | '!' -> if match_char ls '=' then BangEqual else Bang
    | '=' -> if match_char ls '=' then EqualEqual else Equal
    | '<' -> if match_char ls '=' then LessEqual else Less
    | '>' -> if match_char ls '=' then GreaterEqual else Greater
    | '/' ->
        if peek ls = '/' then (
          lex_comment ls;
          next_token ls)
        else Slash
    | '"' -> lex_string ls
    | '0' .. '9' -> lex_number ls
    | c when is_alpha c -> lex_identifier ls
    | ' ' | '\r' | '\t' -> next_token ls
    | '\n' ->
        ls.line <- ls.line + 1;
        next_token ls
    | c -> failwith (sprintf "[Line %d] Unexpected character: '%c'" ls.line c)
