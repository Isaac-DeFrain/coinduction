open! Base

module type S = sig
  type s

  type 'a t

  val make : value:s -> next:(s -> s) -> s t

  val peek : s t -> s

  val next : s t -> s t

  val combine : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

module Make (X : sig
  type t
end) : S with type s := X.t = struct
  type 'a t =
    { value : 'a
    ; next : 'a -> 'a
    }

  let make ~value ~next = { value; next }

  let peek t = t.value

  let next t = { value = t.next t.value; next = t.next }

  let combine ~f ta tb =
    let a = ref ta in
    let b = ref tb in
    { value = f ta.value tb.value
    ; next =
        (fun _ ->
          ignore (a := next !a);
          ignore (b := next !b);
          f (peek !a) (peek !b))
    }
end
