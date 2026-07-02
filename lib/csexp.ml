(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

type t = Sexp.t =
  | Atom of string
  | Node of t list

type parsed = (t, Error.Csexp.t) result

let equal = Sexp.equal

let rec to_buffer buf = function
  | Atom x ->
    let len = x |> String.length |> string_of_int in
    Buffer.add_string buf len;
    Buffer.add_char buf ':';
    Buffer.add_string buf x
  | Node x ->
    Buffer.add_char buf '(';
    List.iter (to_buffer buf) x;
    Buffer.add_char buf ')'
;;

let to_string sexp =
  (* MAYBE: In YOCaml, the buffer size is precalculated, but it seems
     to me like an unnecessary pass. *)
  let buf = Buffer.create 256 in
  to_buffer buf sexp;
  Buffer.contents buf
;;

(* let collect_string expected_length seq = *)
(*   let buf = Buffer.create expected_length in *)
(*   let rec aux given_length seq = *)
(*     if given_length = expected_length *)
(*     then ( *)
(*       let content = Buffer.contents buf in *)
(*       Ok (Sexp.atom content, seq)) *)
(*     else ( *)
(*       match Seq.uncons seq with *)
(*       | Some (c, xs) -> *)
(*         Buffer.add_char buf c; *)
(*         aux (given_length + 1) xs *)
(*       | None -> *)
(*         Error (Error.premature_end_of_atom ~given_length ~expected_length)) *)
(*   in *)
(*   aux 0 seq *)
(* ;; *)

(* let parse_atom pos seq = *)
(*   let rec aux acc pos seq = *)
(*     match Seq.uncons seq, acc with *)
(*     | None, _ -> assert false *)
(*     | _ -> assert false *)
(*   in *)
(*   aux None pos seq *)
(* ;; *)
