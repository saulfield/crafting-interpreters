(* Intermediate representation (three-address code) *)
open Ast
open Printf

type arg = ARG_const of Ast.literal | ARG_var of string [@@deriving show]

type inst =
  | INST_return
  | INST_print of arg
  | INST_copy of arg * arg
  | INST_unary of Ast.unop * arg * arg
  | INST_binary of Ast.binop * arg * arg * arg
[@@deriving show]

type ir_state = { mutable instrs : inst Queue.t; mutable id : int }

let string_of_arg arg =
  match arg with
  | ARG_const lit -> string_of_literal lit
  | ARG_var name -> name

let string_of_inst inst =
  match inst with
  | INST_return -> "return"
  | INST_print arg -> "print " ^ string_of_arg arg
  | INST_copy (v1, dst) ->
      sprintf "%s := %s" (string_of_arg dst) (string_of_arg v1)
  | INST_unary (op, v1, dst) ->
      sprintf "%s := %s%s" (string_of_arg dst) (Ast.string_of_unop op)
        (string_of_arg v1)
  | INST_binary (op, v1, v2, dst) ->
      sprintf "%s := %s %s %s" (string_of_arg dst) (string_of_arg v1)
        (Ast.string_of_binop op) (string_of_arg v2)

let temp_var ir =
  let id = ir.id in
  ir.id <- id + 1;
  ARG_var ("v" ^ string_of_int id)

let add_instr ir inst = Queue.add inst ir.instrs

let rec compile_expr ir expr =
  match expr with
  | EXPR_Literal lit -> ARG_const lit
  | EXPR_Grouping inner -> compile_expr ir inner
  | EXPR_Unary (op, inner) ->
      let v1 = compile_expr ir inner in
      let dst = temp_var ir in
      add_instr ir (INST_unary (op, v1, dst));
      dst
  | EXPR_Binary (lhs, op, rhs) ->
      let v1 = compile_expr ir lhs in
      let v2 = compile_expr ir rhs in
      let dst = temp_var ir in
      add_instr ir (INST_binary (op, v1, v2, dst));
      dst
  | _ -> failwith ("Unimplemented expr" ^ Ast.show_expr expr)

let compile_stmt ir stmt =
  match stmt with
  | STMT_Print expr ->
      let dst = compile_expr ir expr in
      add_instr ir (INST_print dst)
  | _ -> failwith ("Unimplemented stmt" ^ Ast.show_stmt stmt)

let queue_to_list q = Queue.fold (fun acc item -> item :: acc) [] q |> List.rev

let compile ast =
  let ir = { instrs = Queue.create (); id = 0 } in
  List.iter (fun stmt -> compile_stmt ir stmt) ast;
  add_instr ir INST_return;
  Queue.fold (fun acc item -> item :: acc) [] ir.instrs |> List.rev
