(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(* NOTE: The code mostly come from
   https://github.com/xhtmlboi/yocaml *)

type t =
  (* TODO: It seems there's a big discussion in the OCaml community
     about how to pool types for S-Expressions:
     https://discuss.ocaml.org/t/m/10153 I have no objection to relying
     on a library of "declarations" for common types, but I haven't been
     able to find that minimal definition. So for now, I'll leave it as
     is.*)
  | Atom of string
  | Node of t list

type parsing_error =
  | Non_terminated_node of int
  | Non_opened_node of int

type parsed = (t, parsing_error) result

let atom x = Atom x
let node x = Node x

let rec equal a b =
  match a, b with
  | Atom a, Atom b -> String.equal a b
  | Node a, Node b -> List.equal equal a b
  | Atom _, _ | Node _, _ -> false
;;

let rec to_buffer buf = function
  | Atom s -> Buffer.add_string buf (Misc.escape_spaces s)
  | Node x ->
    Buffer.add_char buf '(';
    Misc.concat_buffer_with ~sep:" " buf to_buffer x;
    Buffer.add_char buf ')'
;;

let to_string sexp =
  let buf = Buffer.create 256 in
  to_buffer buf sexp;
  Buffer.contents buf
;;

let parse_atom pos seq =
  let buf = Buffer.create 256 in
  let rec aux is_escaped pos seq =
    match Seq.uncons seq with
    | None -> Buffer.contents buf, pos, Seq.empty
    | Some ('\\', xs) when not is_escaped ->
      (* MAYBE: The YOCaml implementation doesn't seem to care about
         escape characters (if they are escaped). This appears to be a
         bug, but it hasn't been reported.*)
      aux true (pos + 1) xs
    | Some (((' ' | '\t' | '\n' | ')' | '(') as c), xs) when not is_escaped ->
      Buffer.contents buf, pos, Seq.cons c xs
    | Some (c, xs) ->
      Buffer.add_char buf c;
      aux false (pos + 1) xs
  in
  aux false pos seq
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
        Error (Non_terminated_node (pos - 1))
    | Some (('\t' | ' ' | '\n'), xs) -> aux level (pos + 1) acc xs
    | Some (')', xs) ->
      (* MAYBE: The YOCaml implementation does not maintain the level
         counter and relies solely on recursion, which makes it
         impossible to detect closed cases without an opening, for
         example: [foo)bar].*)
      if level > 0
      then Ok (List.rev acc, pos + 1, level - 1, xs)
      else Error (Non_opened_node pos)
    | Some ('(', xs) ->
      Result.bind
        (aux (level + 1) (pos + 1) [] xs)
        (fun (n, pos, level, xs) ->
           (* NOTE: We go from the previous level (to avoid to
              maintain level decrement) *)
           aux level pos (node n :: acc) xs)
    | Some (c, xs) ->
      let a, pos, xs = parse_atom pos (Seq.cons c xs) in
      (* NOTE: We do not increase the position counter here because it
         was already handled by [parse_atom]*)
      aux level pos (atom a :: acc) xs
  in
  Result.map
    (fun (r, _, _, _) ->
       (* NOTE: Only one expression is accepted; if there are multiple
          expressions, they are wrapped in a node, which allows for
          sequential lexing of source code (for example). *)
       match r with
       | [ e ] -> e
       | r -> node r)
    (aux 0 0 [] seq)
;;

let from_string str = str |> String.to_seq |> from_seq
