open Sigs

let arg_input = ref ""
let arg_output = ref ""
let arg_sqlite3 = ref ""

let doc = [("-input", Arg.Set_string(arg_input), "Specify input file");
           ("-output", Arg.Set_string(arg_output), "Specify output file");
           ("-sqlite3", Arg.Set_string(arg_sqlite3), "Specify database file")]
let usage_msg = "Usage: ./DataExchangeSQL <options>"

let main () =
  (* Parse command line *)
  Arg.parse doc (fun _ -> ()) usage_msg;
  
  (* Lexing and parsing *)
  let input = if !arg_input = "" then stdin else open_in !arg_input in
  let lexbuf = Lexing.from_channel input in
  let source, target, mapping = Parser.main Lexer.main lexbuf in
  if !arg_input <> "" then close_in(input);
  
  (*prerr_endline "----------------------------";
  prerr_endline (string_of_tgds mapping);*)
  let skolemized_mapping = Skolemizer.skolemize(mapping) in
  (*prerr_endline "----------------------------";
  prerr_endline (string_of_tgds skolemized_mapping);*)
  
  let sql = Sql_generator.generate (source, target, skolemized_mapping) in
  if !arg_sqlite3 <> "" then begin
    (* If option -sqlite3, then execute SQL *)
    let db = Sqlite3.db_open !arg_sqlite3 in
    let res = List.map (Sqlite3.exec db) sql in
    let res_msgs = List.map Sqlite3.Rc.to_string res in
    List.iter (Printf.eprintf "Execute query... %s\n") res_msgs;
    ignore (Sqlite3.db_close db);
  end else begin
    (* Otherwise, print SQL queries *)
    let output = if !arg_output = "" then stdout else open_out !arg_output in
    List.iter (Printf.fprintf output "%s;\n") sql;
    if !arg_output <> "" then close_out(output);
  end

(* Execute main *)
let _ = main ()
