(* Copyright (c) 2026, Cargocut and the Pidgin developers.
   All rights reserved.

   SPDX-License-Identifier: BSD-3-Clause *)

module Gender = struct
  type t =
    | Male
    | Female
    | Other of string

  let male = Male
  let female = Female
  let other x = Other x

  let to_pidgin x =
    Repr.string
      (match x with
       | Other s -> s
       | Male -> "male"
       | Female -> "female")
  ;;

  let from_pidgin =
    let open Check in
    string
    $ Misc.strim
    $ function
    | "male" | "m" -> Male
    | "female" | "f" -> Female
    | other -> Other other
  ;;

  let equal a b =
    match a, b with
    | Male, Male -> true
    | Female, Female -> true
    | Other a, Male | Male, Other a -> String.equal (Misc.strim a) "male"
    | Other a, Female | Female, Other a -> String.equal (Misc.strim a) "female"
    | Other a, Other b -> String.equal a b
    | Female, Male | Male, Female -> false
  ;;

  let pp ppf = function
    | Male -> Format.pp_print_string ppf "male"
    | Female -> Format.pp_print_string ppf "female"
    | Other s -> Format.pp_print_string ppf s
  ;;

  let testable = Alcotest.testable pp equal
end

module Human = struct
  type t =
    { nickname : string
    ; firstname : string option
    ; lastname : string option
    ; age : int option
    ; gender : Gender.t
    }

  let make ?firstname ?lastname ?age ~nickname ~gender () =
    { nickname; firstname; lastname; age; gender }
  ;;

  let to_pidgin { nickname; firstname; lastname; age; gender } =
    let open Repr in
    record
      [ "nickname", string nickname
      ; "firstname", option string firstname
      ; "lastname", option string lastname
      ; "age", option int age
      ; "gender", Gender.to_pidgin gender
      ]
  ;;

  let from_pidgin =
    let open Check in
    record (fun fields ->
      let+ nickname = req ~alt:[ "nick"; "pseudo" ] fields "nickname" string
      and+ gender = req fields "gender" Gender.from_pidgin
      and+ firstname = opt ~alt:[ "first_name" ] fields "firstname" string
      and+ lastname = opt ~alt:[ "last_name"; "name" ] fields "lastname" string
      and+ age = opt fields "age" int in
      make ?firstname ?lastname ?age ~nickname ~gender ())
  ;;

  let pp ppf human = Format.fprintf ppf "%a" Pp.repr (to_pidgin human)

  let equal { nickname; firstname; lastname; age; gender } b =
    String.equal nickname b.nickname
    && Option.equal String.equal firstname b.firstname
    && Option.equal String.equal lastname b.lastname
    && Option.equal Int.equal age b.age
    && Gender.equal gender b.gender
  ;;

  let testable = Alcotest.testable pp equal
end

module User = struct
  type t =
    { human : Human.t
    ; email : string
    ; level : int
    }

  let make ~email ?(level = 0) ~human () = { email; level; human }

  let to_pidgin { email; level; human } =
    let open Repr in
    record
      [ "human", Human.to_pidgin human
      ; "level", int level
      ; "email", string email
      ]
  ;;

  let from_pidgin =
    let open Check in
    record (fun fields ->
      let+ human = use_record fields Human.from_pidgin
      and+ level = opt fields "level" int
      and+ email = req fields "email" ~alt:[ "mail" ] string in
      make ?level ~human ~email ())
  ;;

  let pp ppf user = Format.fprintf ppf "%a" Pp.repr (to_pidgin user)

  let equal { email; level; human } b =
    Human.equal human b.human
    && String.equal email b.email
    && Int.equal level b.level
  ;;

  let testable = Alcotest.testable pp equal
end
