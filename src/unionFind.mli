(* This module offers a purely functional implementation of Tarjan's
   data structure for solving the union-find problem. *)

(* ------------------------------------------------------------------------- *)

(* Items. *)

module type Item = sig

  type t

  val equal: t -> t -> bool

  module Map : sig
    type key = t
    type 'a t
    val empty: 'a t
    val find: key -> 'a t -> 'a
    val add: key -> 'a -> 'a t -> 'a t
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

end

(* ------------------------------------------------------------------------- *)

(* Descriptors. *)

module type Desc = sig

  type descriptor

  val default: descriptor

  type accumulator

  val union: descriptor -> descriptor -> accumulator -> descriptor * accumulator

  (* [default] should be a neutral element for [union]. [union] should
     be commutative and associative. *)

end

(* ------------------------------------------------------------------------- *)

(* Here are the operations provided by this module. *)

module type S = sig

  type item

  type descriptor

  type accumulator

  (* A state consists of a partition of the items (or, in other words,
     of an equivalence relation over items), together with a total
     mapping of the partition blocks (or, in other words, of the
     equivalence classes) to descriptors. *)

  type state

  (* In the initial state, each item forms its own block, and each
     block is mapped to the [default] descriptor. *)

  val initial: state

  (* [representative] maps each item to a representative member of its
     block. *)

  val representative: item -> state -> item

  (* [equivalent] tells whether two items are members of the same
     block. *)

  val equivalent: item -> item -> state -> bool

  (* [descriptor] maps (items to) blocks to descriptors. *)

  val descriptor: item -> state -> descriptor

  (* [set] changes the descriptor associated with a block. *)

  val set: item -> descriptor -> state -> state

  (* [union] merges two blocks. The new block's descriptor is obtained
     as the union of the two original descriptors. Computing the union
     of two descriptors can have side effects, reflected via the
     [accumulator] monad. *)

  val union: item -> item -> state -> accumulator -> state * accumulator

end

(* ------------------------------------------------------------------------- *)

(* Invocation. *)

module Make (Item : Item) (Desc : Desc)
    : S with type item = Item.t
         and type descriptor = Desc.descriptor
         and type accumulator = Desc.accumulator

