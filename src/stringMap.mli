include Map.S with type key = string

exception StrictAdd of string

val strict_add: key -> 'a -> 'a t -> 'a t

val union: 'a t -> 'a t -> 'a t

