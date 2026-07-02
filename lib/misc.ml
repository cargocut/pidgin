(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

let strim subject = subject |> String.trim |> String.lowercase_ascii

let find_assoc ?(normalize_keys = true) assoc key =
  List.find_map
    (fun (k, v) ->
       let eq =
         if normalize_keys
         then String.equal (strim key) (strim k)
         else String.equal key k
       in
       if eq then Some v else None)
    assoc
;;

let escape_spaces =
  String.fold_left
    (fun acc -> function
       | ('\n' | '\t' | ' ') as c -> acc ^ "\\" ^ String.make 1 c
       | c -> acc ^ String.make 1 c)
    ""
;;

let concat_buffer_with ~sep buf f l =
  List.iteri
    (fun i x ->
       let sep = if Int.equal i 0 then "" else sep in
       Buffer.add_string buf sep;
       f buf x)
    l
;;

let concat_with ~sep f l =
  let buf = Buffer.create 256 in
  concat_buffer_with ~sep buf (fun buf elt -> Buffer.add_string buf (f elt)) l;
  Buffer.contents buf
;;
