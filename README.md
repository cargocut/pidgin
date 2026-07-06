> [!WARNING]  
> This project is still **highly experimental**, but we would be
> delighted to receive feedback (but please be careful with
> production).

# pidgin

> **Pidgin** is a generic key-value data structure description
> language that enables fine-grained validation to deal with format
> like
> [JSON](https://en.wikipedia.org/wiki/JavaScript_Object_Notation),
> [S-expression](https://en.wikipedia.org/wiki/S-expression),
> [Yaml](https://en.wikipedia.org/wiki/YAML),
> [ToML](https://en.wikipedia.org/wiki/TOML) etc.

The main idea is to present a minimal representation (very similar to
that of JSON) and to provide:

- A DSL for describing arbitrary data structures in this language.
- Validation functions that operate on data described using this DSL
- A bidirectional conversion approach, imposing a cost due to the
  indirect nature of the generic format (though it is viable in many
  scenarios).
  
**Pidgin** does not statically preserve the type of expressions;
instead, it hides them, which allows expressions written in this
language to be treated as an untyped runtime representation of
arbitrary OCaml values (enabling the derivation of pretty-printers and
equality functions, for example).

## A simple example

Here is a very short example that demonstrates how to serialize and
deserialize Pidgin expressions: 

```ocaml
module Gender = struct
  type t =
    | Male
    | Female
    | Other of string

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
    $ function
    | "male" | "m" -> Male
    | "female" | "f" -> Female
    | other -> Other other
  ;;
end
```

```ocaml
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
end
```

