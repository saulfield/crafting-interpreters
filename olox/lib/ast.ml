type literal =
  | LIT_string of string
  | LIT_number of float
  | LIT_bool of bool
  | LIT_nil
[@@deriving show]

type unop = UNOP_neg | UNOP_not [@@deriving show]
type logop = LOGOP_or | LOGOP_and [@@deriving show]

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

(* Guarantee unique var IDs for each location *)
(* type node_id = Node of int [@@deriving show] *)
type var = { name : string; id : int } [@@deriving show]
(* type var = Var of string * int  *)

type expr =
  | EXPR_Assign of var * expr
  | EXPR_Binary of expr * binop * expr
  | EXPR_Call of expr * expr list
  | EXPR_Grouping of expr
  | EXPR_Logical of expr * logop * expr
  | EXPR_Literal of literal
  | EXPR_Unary of unop * expr
  | EXPR_Variable of var
[@@deriving show]

type stmt =
  | STMT_Block of stmt list
  | STMT_Break
  | STMT_Expression of expr
  | STMT_Fun of string * string list * stmt list
  | STMT_If of expr * stmt * stmt option
  | STMT_While of expr * stmt
  | STMT_Return of expr option
  | STMT_Print of expr
  | STMT_VarDecl of string * expr option
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

let string_of_literal lit =
  match lit with
  | LIT_string s -> Printf.sprintf "'%s'" s
  | LIT_number n -> Printf.sprintf "%g" n
  | LIT_bool b -> string_of_bool b
  | LIT_nil -> "nil"
