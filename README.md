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
