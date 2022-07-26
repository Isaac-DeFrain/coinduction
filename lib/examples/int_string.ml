open! Base
open! Coinduction.Stream
module IntStream = Make (Int)
module StringStream = Make (String)

let ti = IntStream.make ~value:0 ~next:(fun x -> x + 1)

let ts = StringStream.make ~value:"hello" ~next:(fun x -> x ^ " + world")
