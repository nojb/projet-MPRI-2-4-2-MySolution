(* Exception raising, uncaught exception *)

type exception =
  | Exn1
  | Exn2

type t =
  | A | B

program

let f x =
  match x with
  | A -> B
  | B -> raise Exn2
  end
in

  try raise Exn1 with x -> raise Exn2 end

(* Expected result:
Uncaught exception Exn2
*)
