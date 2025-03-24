open Printf

let silence = ref false
let had_error = ref false
let had_runtime_error = ref false

let report line where message =
  if !silence = false then printf "[line %d] Error%s: %s\n" line where message;
  had_error := true

let resolve_error lexeme message =
  let where = " at '" ^ lexeme ^ "'" in
  if !silence = false then printf "Error%s: %s\n" where message;
  had_error := true

let error line message = report line "" message

let compile_error token line lexeme message =
  if token = Token.EOF then report line " at end" message
  else report line (" at '" ^ lexeme ^ "'") message

let runtime_error message =
  printf "Runtime Error: %s\n" message;
  had_runtime_error := true
