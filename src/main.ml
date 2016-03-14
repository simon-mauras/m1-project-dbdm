let doc = []
let usage_msg = "Usage: ./tinySQL input_file"

let arg_input = ref ""
let set_arg_input s = arg_input := s

let main () =
  Arg.parse doc set_arg_input usage_msg;
  if !arg_input = ""
  then (prerr_string "No input file provided\n"; Arg.usage doc usage_msg)
  else begin
    try
      let input = open_in !arg_input in
      let lexbuf = Lexing.from_channel input in
      let source, target, mapping = Parser.main Lexer.main lexbuf in
      let skolemized_mapping = Skolemizer.skolemize(mapping) in ()
    with
      Sys_error s -> prerr_endline s (* no such file or directory, ... *)
  end

let _ = main ()

