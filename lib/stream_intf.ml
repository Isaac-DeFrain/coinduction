open! Base

module type S = sig
  type s

  type t

  val make : value:s -> next:(s -> s) -> t

  val peek : t -> s

  val next : ?n:int -> t -> t

  val take : ?n:int -> t -> s list

  val co_equal : t -> t -> bool

  val co_equal_relation : t -> t -> bool * (s * s) list

  val combine : f:(s -> s -> s) -> t -> t -> t

  val map : t -> f:(s -> s) -> t
end
