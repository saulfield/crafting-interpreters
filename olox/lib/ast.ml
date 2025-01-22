type literal = LIT_string of string | LIT_number of float
type unop = UNOP_neg | UNOP_not

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

type expr =
  | EXPR_Binary of expr * binop * expr
  | EXPR_Grouping of expr
  | EXPR_Literal of literal
  | EXPR_Unary of unop * expr

(* Pretty printing *)

let string_of_binop binop =
  match binop with
  | BINOP_eq -> "=="
  | BINOP_ne -> "!="
  | BINOP_lt -> "<"
  | BINOP_le -> "<="
  | BINOP_ge -> ">="
  | BINOP_gt -> ">"
  | BINOP_add -> "+"
  | BINOP_sub -> "-"
  | BINOP_mul -> "*"
  | BINOP_div -> "/"

let string_of_unop unop = match unop with UNOP_neg -> "-" | UNOP_not -> "!"
