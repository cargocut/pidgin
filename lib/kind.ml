(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

(* KLUDGE: This approach is probably a little too complicated and ad
   hoc for reporting purposes. *)

(* NOTE: Kinds are a little more complicated than one might expect
   (to provide more feedback on errors). *)
type t =
  | Any
  (* NOTE: It's a little annoying, but it is possible to
     generate values that cannot be unified. For example, an
     empty list. *)
  | Or of t list (* Empty or leads to any. *)
  | Null
  | Bool
  | Int
  | Float
  | String
  | List of t
  (* NOTE: Since we have [Or], we can perform unification on
     complicated list. *)
  | Branch of string * t
  (* NOTE: A special case for handling sum-types. *)
  | Pair of t * t
  (* NOTE: A special case for handling pairs *)
  | Record of (string * t) list

let rec equal a b =
  match a, b with
  | Any, Any
  | Null, Null
  | Bool, Bool
  | Int, Int
  | Float, Float
  | String, String -> true
  | Or a, Or b -> List.equal equal a b
  | Pair (a, x), Pair (b, y) -> equal a b && equal x y
  | Branch (ka, va), Branch (kb, vb) -> equal_with_keys (ka, va) (kb, vb)
  | List a, List b -> equal a b
  | Record a, Record b -> List.equal equal_with_keys a b
  (* NOTE: Ensure that the pattern matching is exhaustive. *)
  | Any, _
  | Or _, _
  | Null, _
  | Bool, _
  | Int, _
  | Float, _
  | String, _
  | List _, _
  | Branch (_, _), _
  | Pair (_, _), _
  | Record _, _ -> false

and equal_with_keys (ka, va) (kb, vb) =
  let c = String.equal ka kb in
  if c then equal va vb else c
;;

let weight
  =
  (* NOTE: The function is internal and just useful for having a
     consistent compare function. Values are discarded because in the
     case of composite kind, order is checked at the compare function
     level. *)
  function
  | Any -> 0
  | Null -> 1
  | Bool -> 2
  | Int -> 3
  | Float -> 4
  | String -> 5
  | Or _ -> 6
  | List _ -> 7
  | Branch (_, _) -> 8
  | Pair (_, _) -> 9
  | Record _ -> 10
;;

let rec compare a b =
  match a, b with
  | List a, List b -> compare a b
  | Or a, Or b -> List.compare compare a b
  | Pair (a, b), Pair (x, y) ->
    (* NOTE: Lexicographic order is sufficient here, and already
       implemented in List. *)
    List.compare compare [ a; b ] [ x; y ]
  | Branch (a, b), Branch (x, y) -> compare_with_keys (a, b) (x, y)
  | Record a, Record b -> List.compare compare_with_keys a b
  | a, b ->
    (* NOTE: here we are using weight. *)
    Int.compare (weight a) (weight b)

and compare_with_keys (ka, va) (kb, vb) =
  let c = String.compare ka kb in
  if Int.equal c 0 then compare va vb else c
;;

(* NOTE: The set is just used to ensure that [or] expression contains
   only uniq kinds. *)
module S = Stdlib.Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

let uniq x = x |> S.of_list |> S.to_list

let or_ a b =
  (* NOTE: we try to keep the smallest shape. i,e: or a a = a. *)
  match a, b with
  | Any, _ | _, Any ->
    (* We lose the value, Any absorb *)
    Any
  | a, b when equal a b -> a
  | Or a, Or b -> Or (List.append a b |> uniq)
  | a, Or b | Or b, a -> Or (List.cons a b |> uniq)
  | a, b ->
    (* NOTE: be consistent with the set *)
    Or [ a; b ]
;;

let from_list_aux f = function
  | [] -> (* We lose the kind information *) Any
  | [ x ] -> f x
  | terms ->
    let s =
      terms
      |>
      (* NOTE: Here we do not use [S.of_list] in order to reuse the
         function for term inference. *)
      List.fold_left (fun set t -> S.add (f t) set) S.empty
    in
    (match S.find_first_opt (fun _ -> true) s with
     | None -> Any
     | Some t ->
       (* It is ok to iter on the full set, since [t] will be
          collapsed. *)
       S.fold or_ s t)
;;

let or_ a b = from_list_aux Fun.id [ a; b ]
let any = Any
let null = Null
let bool = Bool
let int = Int
let float = Float
let string = String
let list t = List t

let record ?(normalize_keys = true) assoc =
  (* NOTE: Empty records are allowed. *)
  let assoc =
    if normalize_keys
    then List.map (fun (k, v) -> Misc.strim k, v) assoc
    else assoc
  in
  Record assoc
;;

let unify Nel.(x :: xs) =
  match xs with
  | [] -> x
  | _ -> from_list_aux (fun x -> x) (x :: xs)
;;

let branch k v = Branch (Misc.strim k, v)
let sum Nel.(x :: xs) = from_list_aux (fun (k, v) -> branch k v) (x :: xs)
let pair a b = Pair (a, b)
let triple a b c = pair a (pair b c)

let tuple Nel.(x :: xs) =
  (* KLUDGE: Non tailtrecursive, sorry, but hey CPS. *)
  let rec aux = function
    | List.[ x ] -> x
    | List.(x :: xs) -> pair x (aux xs)
    | _ ->
      (* NOTE: Never reached because of the Nonempty nature of the
         input. *)
      assert false
  in
  aux (x :: xs)
;;

let rec infer
  =
  (* KLUDGE: the term "infer" is maybe extrem but since we unify types
     for lists... *)
  function
  | Repr.Null -> null
  | Repr.Bool _ -> bool
  | Repr.Int _ -> int
  | Repr.Float _ -> float
  | Repr.String _ -> string
  | Repr.List xs -> list (from_list_aux infer xs)
  | Repr.Record [ ("constr", String k); ("value", v) ]
  | Repr.Record [ ("value", v); ("constr", String k) ] -> branch k (infer v)
  | Repr.Record [ ("first", a); ("second", b) ]
  | Repr.Record [ ("second", b); ("first", a) ] -> Pair (infer a, infer b)
  | Repr.Record fields -> record (List.map (fun (k, v) -> k, infer v) fields)
;;

let rec to_string
  =
  (* NOTE: To string is mostly for inspection (and test unit
     purpose). *)
  function
  | Any -> "?any"
  | Null -> "null"
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Or l ->
    let l = l |> List.map to_string |> String.concat " | " in
    "(" ^ l ^ ")"
  | Pair (a, b) -> "(" ^ to_string a ^ ", " ^ to_string b ^ ")"
  | List a -> "[" ^ to_string a ^ "]"
  | Branch (constr, kind) ->
    "#" ^ Misc.strim constr ^ "<" ^ to_string kind ^ ">"
  | Record r ->
    "{"
    ^ (r
       |> List.map (fun (k, v) -> {|"|} ^ k ^ {|": |} ^ to_string v)
       |> String.concat ", ")
    ^ "}"
;;
