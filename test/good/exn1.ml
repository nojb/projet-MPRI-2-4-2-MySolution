(* Exception raising and catching *)

type ('a, 'b) pair =
  | Pair of 'a * 'b

type nat =
  | Zero
  | Succ of nat

type 'a list =
  | Nil
  | Cons of 'a * 'a list

type exception =
  | Exn_nat of nat
  | Exn_list of nat list

program

(* Raise at top *)

let x =
  try Exn_nat(raise (Exn_list (Cons(Zero, Nil))))
  with x -> x
  end in

(* Raise within a recursive function *)

let rec f n = fun m ->
  match n with
  | Zero -> raise (Exn_nat m)
  | Succ n' -> f n' (Succ m)
  end in

let y =
  try f (Succ(Succ Zero)) Zero 
  with x -> match x with | Exn_nat n -> n | Exn_list l -> raise x end
  end in

(* Do not raise *)

let z = try Succ Zero with x -> Zero end in

Pair(x, Pair(y, z))

(* Expected result:
Pair(Exn_list(Cons(Zero, Nil)), Pair(Succ(Succ(Zero)), Succ(Zero)))
*)
