open Ast

type value = Nil | Bool of bool | Number of float | String of string [@@deriving show]

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

let rec eval expr =
  match expr with
  | EXPR_Literal lit -> eval_literal lit
  | EXPR_Grouping inner -> eval inner
  | EXPR_Binary (lhs, op, rhs) -> eval_binary lhs op rhs
  | EXPR_Unary (op, inner) -> eval_unary op inner

and eval_unary op expr =
  let v = eval expr in
  match op with
  | UNOP_neg -> Number (Float.neg (check_num v))
  | UNOP_not -> Bool (not (is_truthy v))

and num_op f lval rval = f (check_num lval) (check_num rval)

and eval_add lval rval =
  match (lval, rval) with
  | Number lnum, Number rnum -> Number (Float.add lnum rnum)
  | String lstr, String rstr -> String (lstr ^ rstr)
  | left, right ->
      failwith
        (Printf.sprintf "Operands must be of same type (left=%s, right=%s)" (string_of_value left)
           (string_of_value right))

and eval_binary lhs op rhs =
  let lval = eval lhs in
  let rval = eval rhs in
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
