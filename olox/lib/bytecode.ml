(* Bytecode for a stack-based VM *)

open Ast

type local = { name : string; depth : int }

type compile_state = {
  scopes : (string, bool) Hashtbl.t Stack.t;
  locals : local Stack.t;
  mutable depth : int;
}

let global_cs =
  { scopes = Stack.create (); locals = Stack.create (); depth = 0 }

let begin_scope () =
  Stack.push (Hashtbl.create 16) global_cs.scopes;
  global_cs.depth <- global_cs.depth + 1

let rec pop_locals i =
  match Stack.top_opt global_cs.locals with
  | Some local when local.depth = global_cs.depth ->
      Stack.pop global_cs.locals |> ignore;
      pop_locals i + 1
  | _ -> i

let end_scope () =
  Stack.pop global_cs.scopes |> ignore;
  let pop_count = pop_locals 0 in
  global_cs.depth <- global_cs.depth - 1;
  pop_count

type bytecode_op =
  | OP_CONST_NUM of float
  | OP_CONST_STR of string
  | OP_NIL
  | OP_TRUE
  | OP_FALSE
  | OP_POP
  | OP_GET_LOCAL of int
  | OP_SET_LOCAL of int
  | OP_GET_GLOBAL of string
  | OP_SET_GLOBAL of string
  | OP_DEFINE_GLOBAL of string
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
  | OP_JUMP of int
  | OP_JUMP_IF_FALSE of int
  | OP_LOOP of int
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
  | OP_GET_LOCAL i -> Printf.sprintf "GET_LOCAL %d" i
  | OP_SET_LOCAL i -> Printf.sprintf "SET_LOCAL %d" i
  | OP_GET_GLOBAL s -> Printf.sprintf "GET_GLOBAL \"%s\"" s
  | OP_SET_GLOBAL s -> Printf.sprintf "SET_GLOBAL \"%s\"" s
  | OP_DEFINE_GLOBAL s -> Printf.sprintf "DEFINE_GLOBAL \"%s\"" s
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
  | OP_JUMP i -> Printf.sprintf "JUMP %d" i
  | OP_JUMP_IF_FALSE i -> Printf.sprintf "JUMP_IF_FALSE %d" i
  | OP_LOOP i -> Printf.sprintf "LOOP %d" i
  | OP_RET -> "RET"

let op_length op =
  match op with
  | OP_CONST_NUM _ -> 2
  | OP_CONST_STR _ -> 2
  | OP_NIL -> 1
  | OP_TRUE -> 1
  | OP_FALSE -> 1
  | OP_POP -> 1
  | OP_GET_LOCAL _ -> 2
  | OP_SET_LOCAL _ -> 2
  | OP_GET_GLOBAL _ -> 2
  | OP_SET_GLOBAL _ -> 2
  | OP_DEFINE_GLOBAL _ -> 2
  | OP_EQUAL -> 1
  | OP_GREATER -> 1
  | OP_LESS -> 1
  | OP_ADD -> 1
  | OP_SUB -> 1
  | OP_MUL -> 1
  | OP_DIV -> 1
  | OP_NOT -> 1
  | OP_NEG -> 1
  | OP_PRINT -> 1
  | OP_JUMP _ -> 3
  | OP_JUMP_IF_FALSE _ -> 3
  | OP_LOOP _ -> 3
  | OP_RET -> 1

let code_length ops = List.fold_left (fun acc op -> acc + op_length op) 0 ops

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

let resolve_local name =
  let n = Stack.length global_cs.locals in
  let slot = ref (n - 1) in
  let found = ref false in
  let find_slot local =
    if !found = true then ()
    else
      match local.name with
      | loc_name when loc_name = name -> found := true
      | _ -> slot := !slot - 1
  in
  Stack.iter find_slot global_cs.locals;
  !slot

let rec compile_expr expr =
  match expr with
  | EXPR_Literal lit -> [ compile_literal lit ]
  | EXPR_Grouping inner -> compile_expr inner
  | EXPR_Unary (op, inner) -> compile_expr inner @ [ compile_unary op ]
  | EXPR_Binary (lhs, op, rhs) ->
      compile_expr lhs @ compile_expr rhs @ compile_binary op
  | EXPR_Assign (var, expr) -> (
      let rval = compile_expr expr in
      match resolve_local var.name with
      | -1 -> rval @ [ OP_SET_GLOBAL var.name ]
      | slot -> rval @ [ OP_SET_LOCAL slot ])
  | EXPR_Variable var -> (
      match resolve_local var.name with
      | -1 -> [ OP_GET_GLOBAL var.name ]
      | slot -> [ OP_GET_LOCAL slot ])
  | EXPR_Logical (lhs, op, rhs) -> (
      match op with
      | LOGOP_and -> compile_and lhs rhs
      | LOGOP_or -> compile_or lhs rhs)
  (* | EXPR_Call (callee_expr, arg_exprs) -> failwith "" *)
  | _ -> failwith ("Unimplemented expr" ^ show_expr expr)

and compile_and lhs rhs =
  let lhs_code = compile_expr lhs in
  let rhs_code = [ OP_POP ] @ compile_expr rhs in
  let jump_code = [ OP_JUMP_IF_FALSE (code_length rhs_code) ] in
  lhs_code @ jump_code @ rhs_code

and compile_or lhs rhs =
  let lhs_code = compile_expr lhs in
  let rhs_code = [ OP_POP ] @ compile_expr rhs in
  let jump_true = [ OP_JUMP (code_length rhs_code) ] in
  let jump_false = [ OP_JUMP_IF_FALSE (code_length jump_true) ] in
  lhs_code @ jump_false @ jump_true @ rhs_code

let add_local name =
  if Stack.length global_cs.locals > 256 then
    failwith "Too many local variables in function."
  else Stack.push { name; depth = global_cs.depth } global_cs.locals

let declare name =
  if Stack.is_empty global_cs.scopes then () else add_local name

let define name =
  if Stack.is_empty global_cs.scopes then ()
  else
    let scope = Stack.top global_cs.scopes in
    Hashtbl.add scope name true

let compile_initializer expr =
  match expr with
  | None -> [ OP_NIL ]
  | Some expr -> compile_expr expr

let compile_var_decl name init_expr =
  declare name;
  let code = compile_initializer init_expr in
  define name;
  match global_cs.depth with
  | 0 -> code @ [ OP_DEFINE_GLOBAL name ]
  | _ -> code

let compile_pops n =
  let rec f acc i =
    match i with
    | 0 -> acc
    | _ -> f (OP_POP :: acc) (i - 1)
  in
  f [] n

let rec compile_stmt stmt =
  match stmt with
  | STMT_Print expr -> compile_expr expr @ [ OP_PRINT ]
  | STMT_Expression expr -> compile_expr expr @ [ OP_POP ]
  | STMT_VarDecl (name, init_expr) -> compile_var_decl name init_expr
  | STMT_Block stmts ->
      let _ = begin_scope () in
      let code =
        List.fold_left (fun acc stmt -> acc @ compile_stmt stmt) [] stmts
      in
      let pop_count = end_scope () in
      let pop_instrs = compile_pops pop_count in
      code @ pop_instrs
  | STMT_If (cond, then_stmt, else_stmt) -> compile_if cond then_stmt else_stmt
  | STMT_While (cond, body) -> compile_while cond body
  (* | STMT_Fun (_, _, _ -> _ *)
  (* | STMT_Return -> _ *)
  (* | STMT_Break -> _ *)
  | _ -> failwith ("Unimplemented stmt" ^ show_stmt stmt)

and compile_if cond then_stmt else_stmt =
  let cond_code = compile_expr cond in
  let then_code = [ OP_POP ] @ compile_stmt then_stmt in
  let else_code =
    match else_stmt with
    | Some stmt -> [ OP_POP ] @ compile_stmt stmt
    | None -> []
  in
  let else_jump =
    match else_stmt with
    | Some _ -> [ OP_JUMP (code_length else_code) ]
    | None -> []
  in
  let then_jump =
    [ OP_JUMP_IF_FALSE (code_length then_code + code_length else_jump) ]
  in
  cond_code @ then_jump @ then_code @ else_jump @ else_code

and compile_while cond body =
  let cond_code = compile_expr cond in
  let body_code = [ OP_POP ] @ compile_stmt body in
  let jump_start =
    [ OP_LOOP (code_length body_code + 6 + code_length cond_code) ]
  in
  let jump_end =
    [ OP_JUMP_IF_FALSE (code_length body_code + code_length jump_start) ]
  in
  cond_code @ jump_end @ body_code @ jump_start @ [ OP_POP ]

let compile ast =
  let rec step ops stmts =
    match stmts with
    | [] -> ops @ [ OP_RET ]
    | stmt :: rest -> ops @ step (compile_stmt stmt) rest
  in
  step [] ast

let emit_op ch op = Printf.fprintf ch "%s\n" (string_of_op op)

let emit ops =
  let filename = "out.byte" in
  let ch = open_out filename in
  List.iter (fun op -> emit_op ch op) ops;
  close_out ch
(* print_endline ("Compiled to: " ^ filename) *)
