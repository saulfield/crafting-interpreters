open Printf
open Olox
open Scanner
open Parser

let run src =
  let scanner = make_scanner src in
  let tokens = scan_tokens scanner in
  let parser = make_parser tokens in
  let ast = parse parser in
  match ast with
  | Some expr -> expr |> AstPrinter.print_expr |> print_endline
  | None -> ()

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
