type ('a, 'b) pair =
  | Pair of 'a * 'b

type nat =
  | Zero
  | Succ of nat

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

let p = Pair (Zero, Succ Zero) in

Pair (snd p, fst p)

