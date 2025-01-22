open Printf

let report line where message =
  printf "[line %d] Error%s: %s\n" line where message

let had_error = ref false

let error line message =
  report line "" message;
  had_error := true
