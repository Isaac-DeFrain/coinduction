open! Base

module Make (X : sig
  type t [@@deriving eq]
end) : Stream_intf.S with type s := X.t = struct
  type u =
    { value : X.t
    ; next : X.t -> X.t
    }

  type _ t = Stream : u -> X.t t

  let make ~value ~next = Stream { value; next }

  let peek (Stream u) = u.value

  let next ?(n = 1) (Stream u) =
    let rec aux rem { value; next } =
      if rem < 1 then { value; next }
      else aux (rem - 1) { value = next value; next }
    in
    Stream (aux n u)

  let take ?(n = 1) (Stream u) =
    let rec aux rem acc { value; next } =
      if rem < 1 then acc
      else aux (rem - 1) (value :: acc) { value = next value; next }
    in
    aux n [] u

  let co_equal_relation (Stream ua) (Stream ub) =
    let r = ref [] in
    let todo = ref [] in
    todo := (ua, ub) :: !todo;
    let rec loop () =
      match List.hd !todo with
      | None -> (true, !r)
      | Some (p1, p2) ->
        if
          List.mem !r (p1.value, p2.value) ~equal:(fun (a1, a2) (b1, b2) ->
              X.equal a1 a2 && X.equal b1 b2)
        then (true, !r)
        else if not (X.equal p1.value p2.value) then (false, !r)
        else
          let (Stream n1) = next (Stream p1) in
          let (Stream n2) = next (Stream p2) in
          todo := (n1, n2) :: !todo;
          r := (p1.value, p2.value) :: !r;
          loop ()
    in
    loop ()

  let co_equal ta tb = fst (co_equal_relation ta tb)

  let combine ~f (Stream ua) (Stream ub) =
    let a = ref ua.value in
    let b = ref ub.value in
    Stream
      { value = f !a !b
      ; next =
          (fun _ ->
            a := ua.next !a;
            b := ub.next !b;
            f !a !b)
      }

  let map (Stream u) ~f =
    let v = ref u.value in
    Stream
      { value = f u.value
      ; next =
          (fun _ ->
            v := u.next !v;
            f !v)
      }
end
