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

let collect_string pos expected_length seq =
  let buf = Buffer.create expected_length in
  let rec aux given_length seq =
    if given_length = expected_length
    then (
      let content = Buffer.contents buf in
      Ok (Sexp.atom content, seq))
    else (
      match Seq.uncons seq with
      | Some (c, xs) ->
        Buffer.add_char buf c;
        aux (given_length + 1) xs
      | None ->
        Error.Csexp.premature_end_of_atom
          ~given_length
          ~expected_length
          (pos + given_length - 1))
  in
  aux 0 seq
;;

let char_to_int c = int_of_char c - int_of_char '0'

let parse_atom pos seq =
  let rec aux acc pos seq =
    match Seq.uncons seq, acc with
    | None, _ -> Error.Csexp.expected_atom pos
    | Some (':', xs), Some len ->
      Result.map
        (fun (atom, xs) -> atom, pos + len, xs)
        (collect_string (pos + 1) len xs)
    | Some (('0' .. '9' as i), xs), acc ->
      let acc = Option.value ~default:0 acc * 10 in
      aux (Some (acc + char_to_int i)) (pos + 1) xs
    | Some _, Some _ ->
      (* NOTE: Regular character before [:] *)
      Error.Csexp.expected_number_or_column pos
      (* NOTE: Regular character without any number *)
    | Some _, None -> Error.Csexp.expected_number pos
  in
  aux None pos seq
;;

let from_seq seq =
  let rec aux level pos acc seq =
    match Seq.uncons seq with
    | None ->
      if Int.equal 0 level
      then Ok (List.rev acc, pos, level, Seq.empty)
      else
        (* NOTE: The expression is valid only if we haven't entered a
           node yet. Otherwise, it means the node hasn't been
           completed.*)
        Error.Csexp.non_terminated_node (pos - 1)
    | Some (('0' .. '9' as c), xs) ->
      Result.bind
        (parse_atom pos (Seq.cons c xs))
        (fun (atom, pos, xs) -> aux level (pos + 1) (atom :: acc) xs)
    | Some (')', xs) ->
      (* MAYBE: The YOCaml implementation does not maintain the level
         counter and relies solely on recursion, which makes it
         impossible to detect closed cases without an opening, for
         example: [foo)bar].*)
      if level > 0
      then Ok (List.rev acc, pos + 1, level - 1, xs)
      else Error.Csexp.non_opened_node pos
    | Some ('(', xs) ->
      Result.bind
        (aux (level + 1) (pos + 1) [] xs)
        (fun (node, pos, level, xs) -> aux level pos (Sexp.node node :: acc) xs)
    | _ -> assert false
  in
  Result.map
    (fun (r, _, _, _) ->
       (* NOTE: Only one expression is accepted; if there are multiple
          expressions, they are wrapped in a node, which allows for
          sequential lexing of source code (for example). *)
       match r with
       | [ e ] -> e
       | r -> Sexp.node r)
    (aux 0 0 [] seq)
;;

let from_string str = str |> String.to_seq |> from_seq
