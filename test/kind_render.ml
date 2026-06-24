open Pidgin

let nothing_string = "nothing"
let bool_string = "bool"
let int_string = "int"
let float_string = "float"
let string_string = "string"
let opt_string r = r ^ "?"
let string_literal s = "\"" ^ s ^ "\""
let list_string l = "[" ^ String.concat "," l ^ "]"
let record_string r = "{" ^ String.concat "," r ^ "}"
let attribute_string k s = "\"" ^ k ^ "\":" ^ s

let rec to_string e =
  let open Kind.Deconstruct in
  fold
    ~nothing:(fun () -> nothing_string)
    ~opt:(fun t -> opt_string (to_string t))
    ~bool:(fun () -> bool_string)
    ~int:(fun () -> int_string)
    ~float:(fun () -> float_string)
    ~string:(fun () -> string_string)
    ~list:(fun l -> list_string (List.map to_string l))
    ~record:(fun r ->
      record_string
        (List.map (fun (k, e) -> attribute_string k (to_string e)) r))
    e
;;
