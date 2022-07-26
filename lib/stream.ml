open! Base

module Make (X : sig
  type t [@@deriving eq]
end) : Stream_intf.S with type s := X.t = struct
  type t =
    { value : X.t
    ; next : X.t -> X.t
    }

  let make ~value ~next = { value; next }

  let peek t = t.value

  let next ?(n = 1) =
    let rec aux rem { value; next } =
      if rem < 1 then { value; next }
      else aux (rem - 1) { value = next value; next }
    in
    aux n

  let take ?(n = 1) =
    let rec aux rem acc { value; next } =
      if rem < 1 then acc
      else aux (rem - 1) (value :: acc) { value = next value; next }
    in
    aux n []

  let co_equal_relation ta tb =
    let r = ref [] in
    let todo = ref [] in
    todo := (ta, tb) :: !todo;
    let rec loop () =
      match List.hd !todo with
      | None -> (true, !r)
      | Some (p1, p2) ->
        if
          List.mem !r (p1.value, p2.value) ~equal:(fun (a1, a2) (b1, b2) ->
              X.equal a1 a2 && X.equal b1 b2)
        then (true, !r)
        else if not @@ X.equal p1.value p2.value then (false, !r)
        else (
          todo := (next p1, next p2) :: !todo;
          r := (p1.value, p2.value) :: !r;
          loop ())
    in
    loop ()

  let co_equal ta tb = fst @@ co_equal_relation ta tb

  let combine ~f ta tb =
    let a = ref ta.value in
    let b = ref tb.value in
    { value = f !a !b
    ; next =
        (fun _ ->
          a := ta.next !a;
          b := tb.next !b;
          f !a !b)
    }

  let map t ~f =
    let v = ref t.value in
    { value = f t.value
    ; next =
        (fun _ ->
          v := t.next !v;
          f !v)
    }
end
