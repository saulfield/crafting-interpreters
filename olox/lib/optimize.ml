open Ir

let fold_constants inst =
  match inst with
  | INST_unary (op, ARG_const value, dst) ->
      let v = Interpret.eval_unary_val op value in
      INST_copy (ARG_const v, dst)
  | INST_binary (op, ARG_const v1, ARG_const v2, dst) ->
      let v = Interpret.eval_binary_val op v1 v2 in
      INST_copy (ARG_const v, dst)
  | _ -> inst

let optimize instrs = List.map fold_constants instrs
