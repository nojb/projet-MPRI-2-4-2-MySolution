open AbstractSyntax

(* [check_program p] checks that the program [p] is well-formed and returns a
   data constructor table. *)

(* A program is well-formed if every name is properly bound, every
   multiple-name-binding construct is linear, and every data
   constructor and type constructor is applied to an appropriate
   number of arguments. *)

(* A data constructor table maps every data constructor to a closed
   type scheme, where a type scheme is a triple of a vector of
   quantifiers, a vector of domain types, and a codomain type. *)

type scheme =
  | Scheme of type_variable list * typ list * typ

type data_constructor_table =
    scheme StringMap.t

val check_program: program -> data_constructor_table

