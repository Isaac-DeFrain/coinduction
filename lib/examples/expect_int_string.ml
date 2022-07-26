open! Int_string

let print_bool = Caml.Printf.printf "%b"

let ti1 = IntStream.make ~value:0 ~next:(fun x -> x + 1)

let ts1 = StringStream.make ~value:"hello + world" ~next:(fun x -> x ^ " + world")

let%expect_test "int_stream_coeq_test" =
  print_bool IntStream.(co_equal ti ti1);
  [%expect {| true |}]

let%expect_test "int_stream_coneq_test1" =
  print_bool @@ not IntStream.(co_equal (next ti) ti1);
  [%expect {| true |}]

let%expect_test "int_stream_coneq_test2" =
  print_bool @@ not IntStream.(co_equal ti (next ti1));
  [%expect {| true |}]

let%expect_test "string_stream_coeq_test" =
  print_bool StringStream.(co_equal (next ts) ts1);
  [%expect {| true |}]

let%expect_test "string_stream_coneq_test" =
  print_bool @@ not StringStream.(co_equal ts ts1);
  [%expect {| true |}]
