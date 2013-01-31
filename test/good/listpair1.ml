type ('a, 'b) pair =
  | Pair of 'a * 'b

type nat =
  | Zero
  | Succ of nat

type 'a list =
  | Nil
  | Cons of 'a * 'a list

program

let fst p =
  match p with
  | Pair (x, y) ->
      x
  end
in

let snd p =
  match p with
  | Pair (x, y) ->
      y
  end
in

let rec map f = fun xs ->
  match xs with
  | Nil ->
      Nil
  | Cons (x, xs) ->
      Cons (f x, map f xs)
  end
in

fun xs -> map fst xs

(* attendu: ('a22, 'a6) pair list -> 'a22 list *)

