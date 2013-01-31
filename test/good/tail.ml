type 'a list =
  | Nil
  | Cons of 'a * 'a list

program

let rec fold_left f = fun a -> fun lst ->
  match lst with
  | Nil -> a
  | Cons (x1, l) -> fold_left f (f a x1) l
  end

in

fold_left 
