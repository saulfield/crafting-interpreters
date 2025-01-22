open Ast

let print_literal lit =
  match lit with LIT_number num -> string_of_float num | LIT_string str -> str

let rec print_expr (expr : expr) : string =
  match expr with
  | EXPR_Binary (left, binop, right) -> print_sexp (string_of_binop binop) [ left; right ]
  | EXPR_Grouping expr -> print_sexp "group" [ expr ]
  | EXPR_Literal lit -> print_literal lit
  | EXPR_Unary (unop, expr) -> print_sexp (string_of_unop unop) [ expr ]

and print_sexp (name : string) (exprs : expr list) =
  let args = List.fold_left (fun acc expr -> acc ^ " " ^ print_expr expr) "" exprs in
  Printf.sprintf "(%s%s)" name args
