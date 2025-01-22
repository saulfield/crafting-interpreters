type token_info = { token : Token.t; lexeme : string; line : int }
[@@deriving show]

type scanner = {
  src : string;
  tokens : token_info list;
  start : int;
  current : int;
  line : int;
}
[@@deriving show]

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

let make_scanner src = { src; tokens = []; start = 0; current = 0; line = 1 }

let add_token scanner token =
  let line = scanner.line in
  let lexeme =
    String.sub scanner.src scanner.start (scanner.current - scanner.start)
  in
  let token_info = { token; lexeme; line } in
  { scanner with tokens = token_info :: scanner.tokens }

let is_at_end scanner = scanner.current >= String.length scanner.src

let peek scanner =
  if is_at_end scanner then '\000' else scanner.src.[scanner.current]

let advance scanner = { scanner with current = scanner.current + 1 }

let match_char scanner c =
  if is_at_end scanner then None
  else if peek scanner <> c then None
  else Some (advance scanner)

let add_choice s c kind1 kind2 =
  match match_char s c with
  | Some s' -> add_token s' kind2
  | None -> add_token s kind1

let scan_comment scanner =
  let rec scan_line s =
    if peek s = '\n' || is_at_end s then s else scan_line (advance s)
  in
  scan_line scanner

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alphanumeric c = is_alpha c || is_digit c

let scan_string scanner =
  let rec scan_string' s =
    match peek s with
    | '"' -> advance s
    | '\n' -> { s with line = s.line + 1 }
    | '\000' ->
        Common.error s.line "Unterminated string.";
        s
    | _ -> scan_string' (advance s)
  in
  let s' = scan_string' scanner in
  let start_pos = s'.start + 1 in
  let end_pos = s'.current - 1 in
  let str_val = String.sub s'.src start_pos (end_pos - start_pos) in
  add_token s' (String str_val)

let scan_number scanner =
  let rec scan_digits s =
    if is_digit (peek s) then scan_digits (advance s)
    else if peek s = '.' && is_digit (peek (advance s)) then
      scan_digits (advance (advance s))
    else
      let num_val =
        float_of_string (String.sub s.src s.start (s.current - s.start))
      in
      add_token s (Number num_val)
  in
  scan_digits scanner

let scan_identifier scanner =
  let rec scan_ident s =
    if is_alphanumeric (peek s) then scan_ident (advance s)
    else
      let id = String.sub s.src s.start (s.current - s.start) in
      let token = get_identifier id in
      add_token s token
  in
  scan_ident scanner

let scan_token scanner =
  let c = peek scanner in
  let s' = advance scanner in
  match c with
  | '(' -> add_token s' LeftParen
  | ')' -> add_token s' RightParen
  | '{' -> add_token s' LeftBrace
  | '}' -> add_token s' RightBrace
  | ',' -> add_token s' Comma
  | '.' -> add_token s' Dot
  | '-' -> add_token s' Minus
  | '+' -> add_token s' Plus
  | ';' -> add_token s' Semicolon
  | '*' -> add_token s' Star
  | '!' -> add_choice s' '=' BangEqual Bang
  | '=' -> add_choice s' '=' EqualEqual Equal
  | '<' -> add_choice s' '=' LessEqual Less
  | '>' -> add_choice s' '=' GreaterEqual Greater
  | '/' -> if peek s' = '/' then scan_comment s' else add_token s' Slash
  | ' ' | '\r' | '\t' -> s'
  | '\n' -> { s' with line = s'.line + 1 }
  | '"' -> scan_string s'
  | '0' .. '9' -> scan_number s'
  | c when is_alpha c -> scan_identifier s'
  | _ ->
      Common.error scanner.line "Unexpected character.";
      s'

let scan_tokens scanner =
  let rec scan s =
    if is_at_end s then add_token s EOF
    else
      let s' = scan_token { s with start = s.current } in
      scan s'
  in
  let scanner_final = scan scanner in
  List.rev scanner_final.tokens
