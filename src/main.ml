open Sigs

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
      (*prerr_endline "----------------------------";
      prerr_endline (string_of_tgds mapping);*)
      let skolemized_mapping = Skolemizer.skolemize(mapping) in
      (*prerr_endline "----------------------------";
      prerr_endline (string_of_tgds skolemized_mapping);*)
      let sql = Sql_generator.generate (source, target, skolemized_mapping) in
      List.iter (Printf.printf "%s;\n") sql
    with
      Sys_error s -> prerr_endline s (* no such file or directory, ... *)
  end

let _ = main ()

