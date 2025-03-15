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
  | KWBreak
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
  (* Special *)
  | EOF
[@@deriving show]

let from_identifier s =
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
  | "break" -> KWBreak
  | s -> Identifier s
