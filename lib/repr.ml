(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Record of (string * t) list

let rec equal a b =
  match a, b with
  | Null, Null -> true
  | Bool a, Bool b -> Bool.equal a b
  | Int a, Int b -> Int.equal a b
  | Float a, Float b -> Float.equal a b
  | String a, String b -> String.equal a b
  | List a, List b -> List.equal equal a b
  | Record a, Record b ->
    List.equal (fun (ka, va) (kb, vb) -> String.equal ka kb && equal va vb) a b
  (* NOTE: Ensure that the pattern matching is exhaustive. *)
  | Null, _
  | Bool _, _
  | Int _, _
  | Float _, _
  | String _, _
  | List _, _
  | Record _, _ -> false
;;

type 'a conv = 'a -> t

module type PROJECTABLE = sig
  type t

  val to_pidgin : t conv
end

let into (type a) (module P : PROJECTABLE with type t = a) x = P.to_pidgin x
let using f conv x = conv (f x)
let replace n = using (fun _ -> n)
let null _ = Null
let bool b = Bool b
let int i = Int i
let float f = Float f
let string s = String s
let list l = List l
let list_of conv l = list @@ List.map conv l
let nel l = list (Nel.to_list l)
let nel_of conv l = list_of conv (Nel.to_list l)

let record ?(normalize_keys = true) assoc =
  (* NOTE: Empty records are allowed. *)
  let assoc =
    if normalize_keys
    then List.map (fun (k, v) -> Misc.strim k, v) assoc
    else assoc
  in
  Record assoc
;;

let option conv = function
  | None -> null ()
  | Some x -> conv x
;;

let sum f x =
  let constr, value = f x in
  record
    ~normalize_keys:true
    [ "constr", constr |> Misc.strim |> string; "value", value ]
;;

let pair fst snd (a, b) =
  record ~normalize_keys:true [ "first", fst a; "second", snd b ]
;;

let result ~ok ~error =
  sum (function
    | Ok x -> "ok", ok x
    | Error x -> "error", error x)
;;

let either ~left ~right =
  sum (function
    | Either.Left x -> "left", left x
    | Either.Right x -> "right", right x)
;;

let triple f g h (a, b, c) = pair f (pair g h) (a, (b, c))
let int32 = sum (fun x -> "int32", x |> Int32.to_string |> string)
let int64 = sum (fun x -> "int64", x |> Int64.to_string |> string)

let fold ~null ~bool ~int ~float ~string ~list ~record = function
  | Null -> null ()
  | Bool b -> bool b
  | Int i -> int i
  | Float f -> float f
  | String s -> string s
  | List l -> list l
  | Record f -> record f
;;

let fold_partial ?null ?bool ?int ?float ?string ?list ?record default x =
  (* NOTE: Allocate just one closure for every cases. *)
  let default _ = default x in
  let opt_or = function
    | None -> default
    | Some f -> f
  in
  fold
    ~null:(opt_or null)
    ~bool:(opt_or bool)
    ~int:(opt_or int)
    ~float:(opt_or float)
    ~string:(opt_or string)
    ~list:(opt_or list)
    ~record:(opt_or record)
    x
;;

module Infix = struct
  let ( <$> ) = using
end

include Infix
