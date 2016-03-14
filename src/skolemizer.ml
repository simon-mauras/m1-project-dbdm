open Sigs

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

let get_vars_from_value acc = function
  | Var(s) -> StringSet.add s acc
  | _ -> acc

let get_vars_from_atom acc ((_,args) : parse_atom) =
  List.fold_left get_vars_from_value acc args
  
let get_vars_from_query =
  List.fold_left get_vars_from_atom StringSet.empty
  
let rename_vars_in_value rename = function
  | Var(s) when StringMap.mem s rename -> Var(StringMap.find s rename)
  | x -> x

let rename_vars_in_atom rename ((name,args) : parse_atom) =
  (name, List.map (rename_vars_in_value rename) args)
  
let rename_vars_in_query rename =
  List.map (rename_vars_in_atom rename)

let skolem_string m z ys =
  Printf.sprintf "skolem[%d,%s|%s]" m z (String.concat "," (StringSet.elements ys))

let rec skolemize_tgd rule_id ((q_source, q_target) : parse_tgd) =
  let vars_source = get_vars_from_query q_source in
  let vars_target = get_vars_from_query q_target in
  let vars_shared = StringSet.inter vars_target vars_source in
  let vars_created = StringSet.diff vars_target vars_source in
  let add_map s = StringMap.add s (skolem_string rule_id s vars_shared) in
  let rename = StringSet.fold add_map vars_created StringMap.empty in
  StringMap.iter (Printf.printf "m%d %s -> %s\n" rule_id) rename;
  (q_source, rename_vars_in_query rename q_target)

let skolemize (tgds : parse_tgds) =
  List.mapi skolemize_tgd tgds
