open Printf
open Olox
open Ast

let read_file filename =
  let ch = open_in_bin filename in
  let src = really_input_string ch (in_channel_length ch) in
  close_in ch;
  src

let parse src =
  let ls = Lex.init src in
  let stmts = Parse.parse ls in
  if !Common.had_error = false then Resolve.run stmts |> ignore;
  stmts

let emit_const ch lit = fprintf ch "CONST %s\n" (string_of_literal lit)

let emit_unary ch op =
  let op_str =
    match op with
    | UNOP_neg -> "NEG"
    | _ -> failwith ("Unimplemented unary op: " ^ show_unop op)
  in
  fprintf ch "%s\n" op_str

let emit_binary ch op =
  let op_str =
    match op with
    | BINOP_add -> "ADD"
    | BINOP_sub -> "SUB"
    | BINOP_mul -> "MUL"
    | BINOP_div -> "DIV"
    | _ -> failwith ("Unimplemented binary op: " ^ show_binop op)
  in
  fprintf ch "%s\n" op_str

let rec compile_expr ch expr =
  match expr with
  (* | EXPR_Assign (var, expr) -> failwith "" *)
  | EXPR_Binary (lhs, op, rhs) ->
      compile_expr ch lhs;
      compile_expr ch rhs;
      emit_binary ch op
  (* | EXPR_Call (callee_expr, arg_exprs) -> failwith "" *)
  | EXPR_Grouping inner -> compile_expr ch inner
  (* | EXPR_Logical (lhs, op, rhs) -> failwith "" *)
  | EXPR_Literal lit -> emit_const ch lit
  | EXPR_Unary (op, inner) ->
      compile_expr ch inner;
      emit_unary ch op
  (* | EXPR_Variable var -> failwith "" *)
  | _ -> failwith ("Unimplemented expr" ^ show_expr expr)

let compile_stmt ch stmt =
  match stmt with
  | STMT_Print expr -> compile_expr ch expr
  | _ -> failwith ("Unimplemented stmt" ^ show_stmt stmt)

let compile ast =
  let filename = "out.byte" in
  let ch = open_out filename in
  List.iter (fun stmt -> compile_stmt ch stmt) ast;
  fprintf ch "RET\n";
  close_out ch;
  print_endline ("Compiled to: " ^ filename)

let run filename =
  let src = read_file filename in
  let ast = parse src in
  (* List.iter (fun stmt -> print_endline (Ast.show_stmt stmt)) ast; *)
  compile ast

let main args =
  match Array.length args with
  | 1 -> run args.(0)
  | _ ->
      print_endline "Usage: compiler [script]";
      exit 64

(* Main entry point *)
let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1)
let () = main args
