open! Base

module type S = sig
  type s

  type _ t

  val make : value:s -> next:(s -> s) -> s t

  val peek : s t -> s

  val next : ?n:int -> s t -> s t

  val take : ?n:int -> s t -> s list

  val co_equal : s t -> s t -> bool

  val co_equal_relation : s t -> s t -> bool * (s * s) list

  val combine : f:(s -> s -> s) -> s t -> s t -> s t

  val map : s t -> f:(s -> s) -> s t
end
