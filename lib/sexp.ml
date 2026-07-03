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

let rec translate_from_pidgin = function
  | Repr.Null -> atom "null"
  | Repr.Bool r -> atom (if r then "true" else "false")
  | Repr.Int i -> atom (string_of_int i)
  | Repr.Float f -> atom (string_of_float f)
  | Repr.String s -> atom s
  | Repr.List xs -> node (List.map translate_from_pidgin xs)
  | Repr.Record xs ->
    node
      (List.map
         (fun (k, value) -> node [ atom k; translate_from_pidgin value ])
         xs)
;;

let translate_atom x =
  (* NOTE: Since S-expressions carry far less information than the
     Pidgin representation, we have to do a bit of magic to correctly
     assign Pidgin data types. *)
  match int_of_string_opt x with
  | Some i -> Repr.int i
  | None ->
    (* KLUDGE: YOCaml use a fancy alt combinator <|> but that means
       evaluating all the branches, even if it's successful, and I
       didn't feel comfortable going with the "trunk" [unit -> option]. *)
    (match float_of_string_opt x with
     | Some f -> Repr.float f
     | None -> Repr.string x)
;;

let rec translate_to_pidgin = function
  | Atom x when String.equal (Misc.strim x) "null" -> Repr.null ()
  | Atom x when String.equal (Misc.strim x) "true" -> Repr.bool true
  | Atom x when String.equal (Misc.strim x) "false" -> Repr.bool false
  | Atom x -> translate_atom x
  | Node [ Atom key; value ] -> render_pair key (translate_to_pidgin value)
  | Node xs -> list_or_record xs

and render_pair key value =
  (* NOTE: to be consistent with the way we handle pair in
     [list_or_record]. Instead of building a pair, we build a list of
     two element, since it can be checked as pair. *)
  Repr.(list [ string key; value ])

and list_or_record xs =
  (* NOTE: FMPOV (@mspwn) The processing in YOCaml is a bit too
     resource-intensive; it first checks every element in the list to
     determine whether it is a record or not. Here, we construct the
     list and the record simultaneously, and as soon as it becomes
     clear that it is not a record, we fall back to a list. *)
  let rec aux list_acc record_acc = function
    | [] ->
      (match record_acc with
       | [] ->
         (* If the record is empty, we assume a list *)
         Repr.list []
       | xs ->
         (* KLUDGE: Maybe the reverse order isn't very useful
            (just like the order in record validation, which
            isn't very meaningful). *)
         Repr.record (List.rev xs))
    | Node [ Atom key; value ] :: xs ->
      let value = translate_to_pidgin value in
      aux (render_pair key value :: list_acc) ((key, value) :: record_acc) xs
    | xs ->
      (* If we have something other than what looks a bit like a pair
         of records, we decide that, actually... it was a list. *)
      Repr.list (List.rev_append list_acc (List.map translate_to_pidgin xs))
  in
  aux [] [] xs
;;
