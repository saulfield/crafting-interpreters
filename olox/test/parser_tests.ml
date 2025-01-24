open OUnit2
open Olox
open Scanner
open Parser

let scan src = src |> make_scanner |> scan_tokens

let parse src =
  let parser = make_parser (scan src) in
  parse_expr parser

let make_test src expected = fun _ -> assert_equal expected (parse src) ~printer:Ast.show_expr

let tests =
  [
    "Test empty"
    >:: make_test "1 == -2"
          (Ast.EXPR_Binary
             ( Ast.EXPR_Literal (Ast.LIT_number 1.),
               Ast.BINOP_eq,
               Ast.EXPR_Unary (Ast.UNOP_neg, Ast.EXPR_Literal (Ast.LIT_number 2.)) ));
  ]

let test_suite = "Parser tests" >::: tests
let _ = run_test_tt_main test_suite
