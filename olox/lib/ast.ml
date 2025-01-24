type literal = LIT_string of string | LIT_number of float | LIT_bool of bool | LIT_nil
[@@deriving show]

type unop = UNOP_neg | UNOP_not [@@deriving show]

type binop =
  | BINOP_eq
  | BINOP_ne
  | BINOP_lt
  | BINOP_le
  | BINOP_gt
  | BINOP_ge
  | BINOP_add
  | BINOP_sub
  | BINOP_mul
  | BINOP_div
[@@deriving show]

type expr =
  | EXPR_Binary of expr * binop * expr
  | EXPR_Grouping of expr
  | EXPR_Literal of literal
  | EXPR_Unary of unop * expr
[@@deriving show]

(* Conversion *)

let binop_of_tok tok =
  match tok with
  | Token.EqualEqual -> BINOP_eq
  | Token.BangEqual -> BINOP_ne
  | Token.Less -> BINOP_lt
  | Token.LessEqual -> BINOP_le
  | Token.Greater -> BINOP_gt
  | Token.GreaterEqual -> BINOP_ge
  | Token.Plus -> BINOP_add
  | Token.Minus -> BINOP_sub
  | Token.Star -> BINOP_mul
  | Token.Slash -> BINOP_div
  | _ -> failwith "Invalid binop token"

let unop_of_tok tok =
  match tok with
  | Token.Minus -> UNOP_neg
  | Token.Bang -> UNOP_not
  | _ -> failwith "Invalid unop token"

(* Pretty printing *)

let string_of_binop binop =
  match binop with
  | BINOP_eq -> "=="
  | BINOP_ne -> "!="
  | BINOP_lt -> "<"
  | BINOP_le -> "<="
  | BINOP_gt -> ">"
  | BINOP_ge -> ">="
  | BINOP_add -> "+"
  | BINOP_sub -> "-"
  | BINOP_mul -> "*"
  | BINOP_div -> "/"

let string_of_unop unop =
  match unop with
  | UNOP_neg -> "-"
  | UNOP_not -> "!"
