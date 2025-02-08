open Printf
open Ast

type value = Nil | Bool of bool | Number of float | String of string
[@@deriving show]

type env = { parent : env option; values : (string, value) Hashtbl.t }

let env_create parent = { parent; values = Hashtbl.create 16 }

(* Environment *)
let env_define env name value = Hashtbl.add env.values name value

let rec env_get env name =
  match Hashtbl.find_opt env.values name with
  | Some value -> value
  | None -> (
      match env.parent with
      | Some parent_env -> env_get parent_env name
      | None -> failwith (sprintf "Undefined variable '%s'." name))

let rec env_assign env name value =
  match Hashtbl.find_opt env.values name with
  | Some _ -> Hashtbl.add env.values name value
  | None -> (
      match env.parent with
      | Some parent_env -> env_assign parent_env name value
      | None -> failwith (sprintf "Undefined variable '%s'." name))

(* Evaluation *)
let string_of_value v =
  match v with
  | Nil -> "nil"
  | Bool b -> string_of_bool b
  | Number n -> string_of_float n
  | String s -> "\"" ^ s ^ "\""

let is_equal v1 v2 =
  match (v1, v2) with
  | Nil, Nil -> true
  | Bool b1, Bool b2 -> b1 == b2
  | Number n1, Number n2 -> n1 == n2
  | String s1, String s2 -> s1 == s2
  | _ -> false

let is_truthy v =
  match v with
  | Nil -> false
  | Bool b -> b
  | _ -> true

let check_num v =
  match v with
  | Number n -> n
  | _ -> failwith "Expected type 'Number'"

let eval_literal lit =
  match lit with
  | LIT_nil -> Nil
  | LIT_bool b -> Bool b
  | LIT_number n -> Number n
  | LIT_string s -> String s

let rec eval_expr env expr =
  match expr with
  | EXPR_Literal lit -> eval_literal lit
  | EXPR_Grouping inner -> eval_expr env inner
  | EXPR_Binary (lhs, op, rhs) -> eval_binary env lhs op rhs
  | EXPR_Unary (op, inner) -> eval_unary env op inner
  | EXPR_Variable name -> env_get env name
  | EXPR_Assign (name, expr) ->
      let value = eval_expr env expr in
      env_assign env name value;
      value

and eval_unary env op expr =
  let v = eval_expr env expr in
  match op with
  | UNOP_neg -> Number (Float.neg (check_num v))
  | UNOP_not -> Bool (not (is_truthy v))

and num_op f lval rval = f (check_num lval) (check_num rval)

and eval_add lval rval =
  match (lval, rval) with
  | Number lnum, Number rnum -> Number (Float.add lnum rnum)
  | String lstr, String rstr -> String (lstr ^ rstr)
  | String lstr, other -> String (lstr ^ string_of_value other)
  | other, String rstr -> String (string_of_value other ^ rstr)
  | left, right ->
      failwith
        (sprintf "Operands must be of same type (left=%s, right=%s)"
           (string_of_value left) (string_of_value right))

and eval_binary env lhs op rhs =
  let lval = eval_expr env lhs in
  let rval = eval_expr env rhs in
  match op with
  | BINOP_add -> eval_add lval rval
  | BINOP_sub -> Number (num_op Float.sub lval rval)
  | BINOP_div -> Number (num_op Float.div lval rval)
  | BINOP_mul -> Number (num_op Float.mul lval rval)
  | BINOP_gt -> Bool (check_num lval > check_num lval)
  | BINOP_ge -> Bool (check_num lval >= check_num lval)
  | BINOP_lt -> Bool (check_num lval < check_num lval)
  | BINOP_le -> Bool (check_num lval <= check_num lval)
  | BINOP_eq -> Bool (is_equal lval rval)
  | BINOP_ne -> Bool (not (is_equal lval rval))

let eval_var_stmt env name init_expr =
  let init_value =
    match init_expr with
    | Some expr -> eval_expr env expr
    | None -> Nil
  in
  env_define env name init_value |> ignore

let rec eval_stmt env stmt =
  match stmt with
  | STMT_Block stmts ->
      let new_env = env_create (Some env) in
      List.iter (eval_stmt new_env) stmts
  | STMT_Expression expr -> eval_expr env expr |> ignore
  | STMT_Print expr -> expr |> eval_expr env |> string_of_value |> print_endline
  | STMT_Var (name, init_expr) -> eval_var_stmt env name init_expr

let interpret stmts =
  let global_env = env_create None in
  let rec step env stmts =
    match stmts with
    | stmt :: rest ->
        (try eval_stmt env stmt with Failure e -> Common.runtime_error e 1);
        step env rest
    | [] -> ()
  in
  step global_env stmts
