open OUnit2
open Olox

let pp_comma ppf () = Format.fprintf ppf ", "
let pp_tok_list = Format.(pp_print_list ~pp_sep:pp_comma) Token.pp
let str toks = Format.asprintf "[%a]" pp_tok_list toks

let _pp_diff fmt (actual, expected) =
  Format.fprintf fmt "Expected [%a] but got [%a]" pp_tok_list expected
    pp_tok_list actual

(* let extract ts = List.map (fun t -> t.token) ts *)
let rec lex_all ls tokens =
  let token = Lex.next_token ls in
  let tokens' = token :: tokens in
  if token == Token.EOF then tokens' else lex_all ls tokens'

let lex src =
  let ls = Lex.init src in
  let tokens = lex_all ls [] in
  List.rev tokens

let make_test src expected =
 fun _ -> assert_equal expected (lex src) ~printer:str

let tests =
  [
    "Test empty" >:: make_test "" [ Token.EOF ];
    "Test ops" >:: make_test "+-" [ Token.Plus; Token.Minus; Token.EOF ];
    "Test numbers" >:: make_test "1" [ Token.Number 1.0; Token.EOF ];
    "Test strings"
    >:: make_test "\"a string\"" [ Token.String "a string"; Token.EOF ];
    "Test identifiers"
    >:: make_test "or organ" [ Token.KWOr; Token.Identifier "organ"; Token.EOF ];
    "Test comments" >:: make_test "// this is a comment" [ Token.EOF ];
  ]

let test_suite = "Scanner tests" >::: tests
let _ = run_test_tt_main test_suite
