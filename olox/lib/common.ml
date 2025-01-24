open Printf

let had_error = ref false

let report line where message =
  printf "[line %d] Error%s: %s\n" line where message;
  had_error := true

let error line message = report line "" message

let token_error token line lexeme message =
  if token = Token.EOF then report line " at end" message
  else report line (" at '" ^ lexeme ^ "'") message
