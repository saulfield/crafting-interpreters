type t =
  (* Single character tokens *)
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  (* One or two character tokens *)
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  (* Literals *)
  | Identifier of string
  | String of string
  | Number of float
  (* Keywords *)
  | KWAnd
  | KWClass
  | KWElse
  | KWFalse
  | KWFun
  | KWFor
  | KWIf
  | KWNil
  | KWOr
  | KWPrint
  | KWReturn
  | KWSuper
  | KWThis
  | KWTrue
  | KWVar
  | KWWhile
  | KWBreak
  (* Special *)
  | EOF
[@@deriving show]
