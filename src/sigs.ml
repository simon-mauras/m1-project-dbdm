type parse_main = parse_schema * parse_schema * parse_tgds
and parse_schema = parse_relation list
and parse_relation = parse_name * parse_atts
and parse_atts = parse_name list
and parse_tgds = parse_tgd list
and parse_tgd = parse_query * parse_query
and parse_query = parse_atom list
and parse_atom = parse_name * parse_args
and parse_args = parse_value list
and parse_value = Cst of parse_constant
                | Var of parse_name
                | Skolem of int * parse_name * parse_name list
and parse_name = string
and parse_constant = int

let string_of_value = function
  | Cst(c) -> Printf.sprintf "Cst(%d)" c
  | Var(s) -> Printf.sprintf "Var(%s)" s
  | Skolem(m, v, l) ->
    Printf.sprintf "Skolem[m%d,%s](%s)" m v (String.concat "," l)

let string_of_args args =
  String.concat "," (List.map string_of_value args)
  
let string_of_atom (name, args) =
  Printf.sprintf "%s(%s)" name (string_of_args args)

let string_of_query query =
  String.concat " /\\ " (List.map string_of_atom query)

let string_of_tgd (source, target) =
  Printf.sprintf "%s -> %s" (string_of_query source) (string_of_query target)

let string_of_tgds tgds =
  let f = fun i t -> Printf.sprintf "m%d : %s" i (string_of_tgd t) in
  String.concat "\n" (List.mapi f tgds)

