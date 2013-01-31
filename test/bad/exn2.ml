(* A type error caused by exception handling:
   raise an exception of a type [t1] and try to use the exception
   value with type [t2]. *)

type t1 = | A1
type t2 = | A2

program

  try raise A1
  with x -> match x with A2 -> A2 end
  end

