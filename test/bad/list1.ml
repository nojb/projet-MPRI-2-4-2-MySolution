type 'a list =
  | Nil
  | Cons of 'a * 'a list

program

let rec map f = fun xs ->
  match xs with
  | Nil ->
      Nil
  | Cons (x, xs) ->
      Cons (f x, map f xs)
  end
in

exists 'a . (map : 'a list -> 'a list) (* wrong type *)

