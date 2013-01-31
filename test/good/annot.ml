type 'a list =
  | Nil
  | Cons of 'a * 'a list

program

let rec map f = fun xs ->
  match xs with
  | Nil ->
      Nil
  | Cons (x, xs) ->
      let y = exists 'a . (f (x : 'a) : 'a) in (* force [x] and [y] to have same type *)
      Cons (y, map f xs)
  end
in

map

