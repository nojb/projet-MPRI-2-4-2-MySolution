type nat =
  | Zero
  | Succ of nat

program

let rec add x = fun y ->
  match y with
  | Zero ->
      x
  | Succ y ->
      add (Succ x) y
  end
in

let rec mul x = fun y ->
  match y with
  | Zero -> Zero
  | Succ y -> add x (mul x y)
  end

in

let rec fact x =
  match x with
  | Zero -> Succ Zero
  | Succ n ->
      mul (Succ n) (fact n)
      end

in

(* add (Succ Zero) (Succ (Succ Zero)) *)

fact (Succ (Succ (Succ Zero)))

