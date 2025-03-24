open Ast

type function_type = NoFunction | Function

type res_state = {
  scopes : (string, bool) Hashtbl.t Stack.t;
  locals : (var, int) Hashtbl.t;
  mutable current_function : function_type;
}

let begin_scope rs =
  let scope = Hashtbl.create 4 in
  Stack.push scope rs.scopes

let end_scope rs = Stack.pop rs.scopes |> ignore

let declare rs name =
  if Stack.is_empty rs.scopes then ()
  else
    let scope = Stack.top rs.scopes in
    match Hashtbl.find_opt scope name with
    | None -> Hashtbl.add scope name false
    | Some _ ->
        Common.resolve_error name
          "Already a variable with this name in this scope."

let define rs name =
  if Stack.is_empty rs.scopes then ()
  else
    let scope = Stack.top rs.scopes in
    Hashtbl.add scope name true

let check_initializer rs name =
  if Stack.is_empty rs.scopes then ()
  else
    let scope = Stack.top rs.scopes in
    match Hashtbl.find_opt scope name with
    | Some false ->
        Common.resolve_error name
          "Can't read local variable in its own initializer."
    | _ -> ()

let resolve_local rs var =
  (* Printf.printf "Resolving local: %s\n" (show_var var); *)
  let f i scope =
    (* Hashtbl.iter (fun name b -> Printf.printf "%s -> %b\n" name b) scope; *)
    if i < 0 then i
    else
      match Hashtbl.find_opt scope var.name with
      | Some _ ->
          Hashtbl.add rs.locals var i;
          -1
      | None -> i + 1
  in
  Stack.fold f 0 rs.scopes |> ignore;
  ()

let rec resolve_expr rs expr =
  (* Printf.printf "Resolving expr: %s\n" (show_expr expr); *)
  match expr with
  | EXPR_Assign (var, rhs) ->
      resolve_expr rs rhs;
      resolve_local rs var
  | EXPR_Binary (lhs, _, rhs) ->
      resolve_expr rs lhs;
      resolve_expr rs rhs
  | EXPR_Call (callee_expr, arg_exprs) ->
      resolve_expr rs callee_expr;
      List.iter (fun e -> resolve_expr rs e) arg_exprs
  | EXPR_Grouping inner -> resolve_expr rs inner
  | EXPR_Literal _ -> ()
  | EXPR_Logical (lhs, _, rhs) ->
      resolve_expr rs lhs;
      resolve_expr rs rhs
  | EXPR_Unary (_, inner) -> resolve_expr rs inner
  | EXPR_Variable var ->
      check_initializer rs var.name;
      resolve_local rs var

let rec resolve_stmt rs stmt =
  (* Printf.printf "Resolving stmt: %s\n" (show_stmt stmt); *)
  match stmt with
  | STMT_Block stmts ->
      begin_scope rs;
      resolve_stmts rs stmts;
      end_scope rs
  | STMT_Expression expr -> resolve_expr rs expr
  | STMT_Fun (name, params, body) ->
      declare rs name;
      define rs name;
      resolve_function rs params body Function
  | STMT_If (condition, then_branch, else_branch) -> (
      resolve_expr rs condition;
      resolve_stmt rs then_branch;
      match else_branch with
      | Some else_stmt -> resolve_stmt rs else_stmt
      | None -> ())
  | STMT_While (condition, body) ->
      resolve_expr rs condition;
      resolve_stmt rs body
  | STMT_Print expr -> resolve_expr rs expr
  | STMT_Return expr -> (
      if rs.current_function == NoFunction then
        Common.resolve_error "return" "Can't return from top-level code.";
      match expr with
      | Some expr -> resolve_expr rs expr
      | None -> ())
  | STMT_Var (name, init) ->
      declare rs name;
      (match init with
      | Some expr -> resolve_expr rs expr
      | None -> ());
      define rs name
  | STMT_Break -> ()

and resolve_stmts rs stmts = List.iter (fun s -> resolve_stmt rs s) stmts

and resolve_function rs params body ftype =
  let enclosing_ftype = rs.current_function in
  rs.current_function <- ftype;
  begin_scope rs;
  List.iter
    (fun param ->
      declare rs param;
      define rs param)
    params;
  resolve_stmts rs body;
  end_scope rs;
  rs.current_function <- enclosing_ftype

let run stmts =
  let res_state =
    {
      scopes = Stack.create ();
      locals = Hashtbl.create 16;
      current_function = NoFunction;
    }
  in
  resolve_stmts res_state stmts;
  res_state.locals

let run_expr expr =
  let res_state =
    {
      scopes = Stack.create ();
      locals = Hashtbl.create 16;
      current_function = NoFunction;
    }
  in
  resolve_expr res_state expr;
  res_state.locals
