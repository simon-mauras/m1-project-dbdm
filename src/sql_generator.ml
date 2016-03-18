open Sigs

module StringMap = Map.Make(String)

(* Get the id of the tables involved in the source part of the tgd *)
let get_source_tables (query_s : parse_query) =
  List.mapi (fun i (s,_) -> Printf.sprintf "%s T%d" s i) query_s

(* Get a mapping var -> list of table.field in which var appears *)
let get_source_vars (schema_s : parse_schema) (query_s : parse_query) =
  let columns_from_value acc = function
    | name, Var(s) ->
      let l = try StringMap.find s acc with Not_found -> [] in
      StringMap.add s (name :: l) acc
    | _ -> acc
  in
  let columns_from_atom acc (id, name, args) =
    let l = List.map (Printf.sprintf "T%d.%s" id) (List.assoc name schema_s) in
    List.fold_left columns_from_value acc (List.combine l args)
  in
  let query_s = List.mapi (fun i (s,l) -> (i, s, l)) query_s in
  List.fold_left columns_from_atom StringMap.empty query_s

(* Get source constraints based on constants *)
let get_const_constraints (schema_s : parse_schema) (query_s : parse_query) =
  let constraints_from_value acc = function
    | name, Cst(x) -> (Printf.sprintf "%s = %d" name x) :: acc
    | _ -> acc
  in
  let constraints_from_atom acc (id, name, args) =
    let l = List.map (Printf.sprintf "T%d.%s" id) (List.assoc name schema_s) in
    List.fold_left constraints_from_value acc (List.combine l args)
  in
  let query_s = List.mapi (fun i (s,l) -> (i, s, l)) query_s in
  List.fold_left constraints_from_atom [] query_s

(* Get the constraints for the join using the mapping var -> fields *)
let get_join_constraints columns =
  let columns = StringMap.fold (fun _ l acc -> l::acc) columns [] in
  let rec pairs acc = function
    | a :: b :: l -> pairs ((a,b) :: acc) (b :: l)
    | x -> acc
  in
  let constraints = List.fold_left pairs [] columns in
  List.map (fun (x,y) -> Printf.sprintf "%s = %s" x y) constraints

(* Get target values *)
let get_target_tables (schema_t : parse_schema) (query_t : parse_query) =
  let get_table (name,_) =
    let atts = List.assoc name schema_t in
    Printf.sprintf "%s(%s)" name (String.concat "," atts)
  in
  List.map get_table query_t

(* The the values to insert in the target table *)
let get_target_columns columns (query_t : parse_query) =
  let column_of_var s = List.hd (StringMap.find s columns) in
  let get_value = function
    | Cst(x) -> string_of_int x
    | Var(s) -> column_of_var s
    | Skolem(i,s,lst) ->
      match List.map column_of_var lst with
        | [] -> Printf.sprintf "'Skolem[m%d,%s]()'" i s
        | l  -> Printf.sprintf
                  "'Skolem[m%d,%s](' || %s || ')'"
                  i s (String.concat " || ',' || " l)
  in
  List.map (fun (_,args) -> List.map get_value args) query_t


(* Generate the sql requests for one tgd *)
let generate_request schema_s schema_t tgd =
  let query_s, query_t = tgd in
  
  let source_tables = get_source_tables query_s in
  let sql_from =
    Printf.sprintf "\nFROM %s" (String.concat " INNER JOIN " source_tables)
  in
  (*prerr_string "------------------------";
  prerr_endline sql_from;*)
  
  let source_vars = get_source_vars schema_s query_s in
  (*prerr_endline "------------------------";
  let dump = StringMap.iter (fun k v -> Printf.eprintf "%s -> %s\n" k (String.concat "," v)) in
  dump source_vars;*)
  
  let sql_where = match get_const_constraints schema_s query_s with
    | [] -> ""
    | l -> Printf.sprintf "\nWHERE %s" (String.concat " AND " l)
  in
  (*prerr_string "------------------------";
  prerr_endline sql_where;*)
  
  let sql_on = match get_join_constraints source_vars with
    | [] -> ""
    | l -> Printf.sprintf "\nON %s" (String.concat " AND " l)
  in
  (*prerr_string "------------------------";
  prerr_endline sql_on;*)
  
  let target_tables = get_target_tables schema_t query_t in
  (*prerr_endline "------------------------";
  List.iter prerr_endline target_tables;*)
  
  let target_columns =
    List.map (String.concat ", ") (get_target_columns source_vars query_t)
  in
  (*prerr_endline "------------------------";
  List.iter prerr_endline target_columns;*)
  
  let sql_query (t_table, t_columns) =
    Printf.sprintf "INSERT INTO %s\nSELECT %s%s%s%s"
      t_table
      t_columns
      sql_from
      sql_on
      sql_where
  in
  List.map sql_query (List.combine target_tables target_columns)

(* Generate a set of requests for a list of tgds *)
let generate (schema_mapping : parse_main) =
  let schema_s, schema_t, mapping = schema_mapping in
  List.concat (List.map (generate_request schema_s schema_t) mapping)

