open Printf
open Olox
open Scanner
open Ast
open AstPrinter

let print_ast () =
  let expr =
    EXPR_Binary
      ( EXPR_Unary (UNOP_neg, EXPR_Literal (LIT_number 123.)),
        BINOP_mul,
        EXPR_Grouping (EXPR_Literal (LIT_number 45.67)) )
  in
  expr |> print_expr |> print_endline

let run src =
  let scanner = make_scanner src in
  let tokens = scan_tokens scanner in
  Queue.iter (fun t -> printf "%s\n" (show_token_info t)) tokens;
  printf "%d tokens\n" (Queue.length tokens);
  print_ast ()

let run_file filename =
  let ch = open_in_bin filename in
  let src = really_input_string ch (in_channel_length ch) in
  close_in ch;
  run src;
  if !Common.had_error then exit 65

let run_prompt () =
  let continue = ref true in
  while !continue do
    try
      print_string "> ";
      let line = read_line () in
      run line;
      Common.had_error := false
    with End_of_file ->
      continue := false;
      printf "\n"
  done

let main args =
  let n_args = Array.length args in
  match n_args with
  | 0 -> run_prompt ()
  | 1 -> run_file args.(0)
  | _ ->
      print_endline "Usage: olox [script]";
      exit 64

(* Main entry point *)
let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1)
let () = main args
