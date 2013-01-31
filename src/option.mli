val map: ('a -> 'b) -> 'a option -> 'b option
val iter: ('a -> unit) -> 'a option -> unit
val fold: ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
val concat: ('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option
