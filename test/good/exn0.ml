(* Exception raising, uncaught exception *)

type exception =
  | Exn1
  | Exn2

program

  try raise Exn1 with x -> raise Exn2 end

(* Expected result:
Uncaught exception Exn2
*)
