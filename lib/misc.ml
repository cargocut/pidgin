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
