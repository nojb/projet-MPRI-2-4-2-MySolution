type nat =
  | Zero
  | Succ of nat

type bool =
  | True
  | False

program

let rec add x = fun y ->
  match y with
  | Zero ->
      x
  | Succ y ->
      add (Succ x) y
  end
in

add (Succ True) (Succ (Succ Zero))

