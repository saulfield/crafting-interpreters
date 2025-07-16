open Olox

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

let run filename =
  let src = read_file filename in
  let ast = parse src in
  (* List.iter (fun stmt -> print_endline (Ast.show_stmt stmt)) ast; *)
  let bytecode = Bytecode.compile ast in
  (* List.iter (fun op -> print_endline (Bytecode.string_of_op op)) bytecode; *)
  Bytecode.emit bytecode;
  (* let ir = Ir.compile ast in *)
  (* List.iter (fun inst -> print_endline (Ir.string_of_inst inst)) ir; *)
  (* let ir_optimized = Optimize.optimize ir in *)
  (* List.iter (fun inst -> print_endline (Ir.string_of_inst inst)) ir_optimized; *)
  ()

let main args =
  match Array.length args with
  | 1 -> run args.(0)
  | _ ->
      print_endline "Usage: compiler [script]";
      exit 64

(* Main entry point *)
let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1)
let () = main args
