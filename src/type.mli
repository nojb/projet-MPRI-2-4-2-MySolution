(* Basic operations on types. *)

open Format
open AbstractSyntax

(* If [theta] is a substitution of types for type variables, then
   [lift theta] is a type homomorphism. *)

val lift: ('a -> 'b ptyp) -> 'a ptyp -> 'b ptyp

(* This turns a type variable printer into a type printer. *)

val print: (formatter -> 'a -> unit) -> formatter -> 'a ptyp -> unit
