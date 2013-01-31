type bool =
  | True
  | False

type nat =
  | Zero
  | Succ of nat

type unit =
  | Unit

program

let id = fun x -> x in
let true = id True in
let zero = id Zero in
Unit

