type nat =
  | Zero
  | Succ of nat

program

let rec add x = fun y ->
  match y with
  | Zero ->
      x
  | Succ y ->
      add y
  end
in

add Zero Zero

