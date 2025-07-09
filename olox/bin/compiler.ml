open Printf
open Olox
open Ast

type opcode =
  | OP_CONSTANT of float
  | OP_NIL
  | OP_TRUE
  | OP_FALSE
  | OP_EQUAL
  | OP_GREATER
  | OP_LESS
  | OP_ADD
  | OP_SUB
  | OP_MUL
  | OP_DIV
  | OP_NOT
  | OP_NEG
  | OP_RETURN
[@@deriving show]

let string_of_opcode binop =
  match binop with
  | OP_CONSTANT f -> Printf.sprintf "CONST %g" f
  | OP_NIL -> "PUSH_NIL"
  | OP_TRUE -> "PUSH_TRUE"
  | OP_FALSE -> "PUSH_FALSE"
  | OP_EQUAL -> "EQUAL"
  | OP_GREATER -> "GREATER"
  | OP_LESS -> "LESS"
  | OP_ADD -> "ADD"
  | OP_SUB -> "SUB"
  | OP_MUL -> "MUL"
  | OP_DIV -> "DIV"
  | OP_NOT -> "NOT"
  | OP_NEG -> "NEG"
  | OP_RETURN -> "RET"

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

let emit_const ch lit =
  let opcode =
    match lit with
    | LIT_number n -> OP_CONSTANT n
    | LIT_bool true -> OP_TRUE
    | LIT_bool false -> OP_FALSE
    | LIT_nil -> OP_NIL
    | LIT_string _s -> failwith "Strings not yet implemented."
  in
  fprintf ch "%s\n" (string_of_opcode opcode)

let emit_unary ch op =
  let opcode =
    match op with
    | UNOP_neg -> OP_NEG
    | UNOP_not -> OP_NOT
  in
  let op_str = string_of_opcode opcode in
  fprintf ch "%s\n" op_str

let emit_binary ch op =
  let opcode =
    match op with
    | BINOP_add -> OP_ADD
    | BINOP_sub -> OP_SUB
    | BINOP_mul -> OP_MUL
    | BINOP_div -> OP_DIV
    | BINOP_eq -> OP_EQUAL
    | BINOP_gt -> OP_GREATER
    | BINOP_lt -> OP_LESS
    | BINOP_ne -> failwith ""
    | BINOP_le -> failwith ""
    | BINOP_ge -> failwith ""
  in
  let op_str = string_of_opcode opcode in
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
  fprintf ch "%s" (string_of_opcode OP_RETURN);
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
