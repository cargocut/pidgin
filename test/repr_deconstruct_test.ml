open Pidgin

let%expect_test "deconstruct: null" =
  let t = Repr.Construct.null () in
  let v = Repr.Deconstruct.null t in
  print_string (From_repr_render.to_string v);
  [%expect {| Ok |}]
;;

let%expect_test "deconstruct: null with int" =
  let t = Repr.Construct.int 1 in
  let v = Repr.Deconstruct.null t in
  print_string (From_repr_render.to_string v);
  [%expect {| Expecting nothing receives int |}]
;;

let%expect_test "deconstruct: int" =
  let t = Repr.Construct.int 42 in
  let v = Repr.Deconstruct.int t in
  print_string (From_repr_render.to_string v);
  [%expect {| Ok |}]
;;

let%expect_test "deconstruct: int with float" =
  let t = Repr.Construct.float 4.5 in
  let v = Repr.Deconstruct.int t in
  print_string (From_repr_render.to_string v);
  [%expect {| Expecting int receives float |}]
;;

let%expect_test "deconstruct: float" =
  let t = Repr.Construct.float 42.0 in
  let v = Repr.Deconstruct.float t in
  print_string (From_repr_render.to_string v);
  [%expect {| Ok |}]
;;

let%expect_test "deconstruct: float with string" =
  let t = Repr.Construct.float 4.5 in
  let v = Repr.Deconstruct.string t in
  print_string (From_repr_render.to_string v);
  [%expect {| Expecting string receives float |}]
;;

let%expect_test "deconstruct: string" =
  let t = Repr.Construct.string "4.5" in
  let v = Repr.Deconstruct.string t in
  print_string (From_repr_render.to_string v);
  [%expect {| Ok |}]
;;

let%expect_test "deconstruct: string with null" =
  let t = Repr.Construct.string "4.5" in
  let v = Repr.Deconstruct.null t in
  print_string (From_repr_render.to_string v);
  [%expect {| Expecting nothing receives string |}]
;;

let%expect_test "deconstruct: list" =
  let t = Repr.Construct.list [] in
  let v = Repr.Deconstruct.list t in
  print_string (From_repr_render.to_string v);
  [%expect {| Ok |}]
;;

let%expect_test "deconstruct: list with null" =
  let t = Repr.Construct.list [] in
  let v = Repr.Deconstruct.null t in
  print_string (From_repr_render.to_string v);
  [%expect {| Expecting nothing receives [] |}]
;;

let%expect_test "deconstruct: record" =
  let t = Repr.Construct.record [] in
  let v = Repr.Deconstruct.record t in
  print_string (From_repr_render.to_string v);
  [%expect {| Ok |}]
;;

let%expect_test "deconstruct: list with null" =
  let t = Repr.Construct.record [] in
  let v = Repr.Deconstruct.null t in
  print_string (From_repr_render.to_string v);
  [%expect {| Expecting nothing receives {} |}]
;;
