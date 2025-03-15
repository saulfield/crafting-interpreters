open Printf
open Olox

(* open Lex *)
(* open Parse *)
(* open Interpret *)

(* let rec print_tokens ls =
  let token = Lex.next_token ls in
  printf "%d %s\n" ls.line (Token.show token);
  if token == Token.EOF then () else print_tokens ls *)

let run src =
  let lex_state = Lex.init src in
  (* let _ = print_tokens lex_state in *)
  let parse_state = Parse.init lex_state in
  let stmts = Parse.parse parse_state in
  (* let _ = List.iter (fun stmt -> print_endline (Ast.show_stmt stmt)) stmts in *)
  Interpret.interpret stmts |> ignore

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
