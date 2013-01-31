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

add (fun x -> Succ x) Zero

