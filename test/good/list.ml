type 'a list =
  | Nil
  | Cons of 'a * 'a list

type bool =
  | True
  | False

program

let rec map f = fun xs ->
  match xs with
  | Nil ->
      Nil
  | Cons (x, xs) ->
      Cons (f x, map f xs)
  end
in

let rec fold f = fun xs -> fun accu ->
  match xs with
  | Nil ->
      accu
  | Cons (x, xs) ->
      f x (fold f xs accu)
  end
in

let neg = fun b ->
  match b with
  | True ->
      False
  | False ->
      True
  end
in

let conj = fun b1 -> fun b2 ->
  match b1 with
  | True ->
      (b2 : bool)
  | False ->
      False
  end
in

(fold conj (map neg (Cons (True, (Cons (False, Nil))))) True : bool)

