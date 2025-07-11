open Printf
open Olox
open Ast

type bytecode_op =
  | OP_CONST_NUM of float
  | OP_CONST_STR of string
  | OP_NIL
  | OP_TRUE
  | OP_FALSE
  | OP_POP
  | OP_DEFINE_GLOBAL of string
  | OP_GET_GLOBAL of string
  | OP_SET_GLOBAL of string
  | OP_EQUAL
  | OP_GREATER
  | OP_LESS
  | OP_ADD
  | OP_SUB
  | OP_MUL
  | OP_DIV
  | OP_NOT
  | OP_NEG
  | OP_PRINT
  | OP_RET
[@@deriving show]

let string_of_op op =
  match op with
  | OP_CONST_NUM f -> Printf.sprintf "CONST %g" f
  | OP_CONST_STR s -> Printf.sprintf "CONST \"%s\"" s
  | OP_NIL -> "PUSH_NIL"
  | OP_TRUE -> "PUSH_TRUE"
  | OP_FALSE -> "PUSH_FALSE"
  | OP_POP -> "POP"
  | OP_DEFINE_GLOBAL s -> Printf.sprintf "DEFINE_GLOBAL \"%s\"" s
  | OP_GET_GLOBAL s -> Printf.sprintf "GET_GLOBAL \"%s\"" s
  | OP_SET_GLOBAL s -> Printf.sprintf "SET_GLOBAL \"%s\"" s
  | OP_EQUAL -> "EQ"
  | OP_GREATER -> "GT"
  | OP_LESS -> "LT"
  | OP_ADD -> "ADD"
  | OP_SUB -> "SUB"
  | OP_MUL -> "MUL"
  | OP_DIV -> "DIV"
  | OP_NOT -> "NOT"
  | OP_NEG -> "NEG"
  | OP_PRINT -> "PRINT"
  | OP_RET -> "RET"

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

let compile_literal lit =
  match lit with
  | LIT_number n -> OP_CONST_NUM n
  | LIT_string s -> OP_CONST_STR s
  | LIT_bool true -> OP_TRUE
  | LIT_bool false -> OP_FALSE
  | LIT_nil -> OP_NIL

let compile_unary op =
  match op with
  | UNOP_neg -> OP_NEG
  | UNOP_not -> OP_NOT

let compile_binary op =
  match op with
  | BINOP_add -> [ OP_ADD ]
  | BINOP_sub -> [ OP_SUB ]
  | BINOP_mul -> [ OP_MUL ]
  | BINOP_div -> [ OP_DIV ]
  | BINOP_eq -> [ OP_EQUAL ]
  | BINOP_gt -> [ OP_GREATER ]
  | BINOP_lt -> [ OP_LESS ]
  | BINOP_ne -> [ OP_EQUAL; OP_NOT ]
  | BINOP_le -> [ OP_GREATER; OP_NOT ]
  | BINOP_ge -> [ OP_LESS; OP_NOT ]

let rec compile_expr expr =
  match expr with
  | EXPR_Literal lit -> [ compile_literal lit ]
  | EXPR_Grouping inner -> compile_expr inner
  | EXPR_Unary (op, inner) -> compile_expr inner @ [ compile_unary op ]
  | EXPR_Binary (lhs, op, rhs) ->
      compile_expr lhs @ compile_expr rhs @ compile_binary op
  | EXPR_Assign (var, expr) -> compile_expr expr @ [ OP_SET_GLOBAL var.name ]
  | EXPR_Variable var -> [ OP_GET_GLOBAL var.name ]
  (* | EXPR_Call (callee_expr, arg_exprs) -> failwith "" *)
  (* | EXPR_Logical (lhs, op, rhs) -> failwith "" *)
  | _ -> failwith ("Unimplemented expr" ^ show_expr expr)

let compile_stmt stmt =
  match stmt with
  | STMT_Print expr -> compile_expr expr @ [ OP_PRINT ]
  | STMT_Expression expr -> compile_expr expr @ [ OP_POP ]
  | STMT_VarDecl (name, init_expr) ->
      let init_val =
        match init_expr with
        | None -> [ OP_NIL ]
        | Some expr -> compile_expr expr
      in
      init_val @ [ OP_DEFINE_GLOBAL name ]
  (* | STMT_Break -> _ *)
  (* | STMT_Block -> _ *)
  (* | STMT_Fun (_, _, _ -> _ *)
  (* | STMT_If (_, _, _ -> _ *)
  (* | STMT_While (_, _ -> _ *)
  (* | STMT_Return -> _ *)
  | _ -> failwith ("Unimplemented stmt" ^ show_stmt stmt)

let compile ast =
  let rec step ops stmts =
    match stmts with
    | [] -> ops @ [ OP_RET ]
    | stmt :: rest -> ops @ step (compile_stmt stmt) rest
  in
  step [] ast

let emit ch op = fprintf ch "%s\n" (string_of_op op)

let emit_bytecode ops =
  let filename = "out.byte" in
  let ch = open_out filename in
  List.iter (fun op -> emit ch op) ops;
  close_out ch
(* print_endline ("Compiled to: " ^ filename) *)

let run filename =
  let src = read_file filename in
  let ast = parse src in
  let bytecode = compile ast in
  (* List.iter (fun stmt -> print_endline (Ast.show_stmt stmt)) ast; *)
  (* List.iter (fun op -> print_endline (string_of_op op)) bytecode; *)
  emit_bytecode bytecode

let main args =
  match Array.length args with
  | 1 -> run args.(0)
  | _ ->
      print_endline "Usage: compiler [script]";
      exit 64

(* Main entry point *)
let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1)
let () = main args
