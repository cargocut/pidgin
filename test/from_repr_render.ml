open Pidgin

let null_string = "null"
let string_literal s = "\"" ^ s ^ "\""
let list_string l = "[" ^ String.concat "," l ^ "]"
let record_string r = "{" ^ String.concat "," r ^ "}"
let attribute_string k s = "\"" ^ k ^ "\":" ^ s

let rec to_string = function
  | Ok t -> "Ok"
  | Error (Repr.Deconstruct.Invalid_kind { expecting; given }) ->
    "Expecting "
    ^ Kind_render.to_string expecting
    ^ " receives "
    ^ Kind_render.to_string given
;;
