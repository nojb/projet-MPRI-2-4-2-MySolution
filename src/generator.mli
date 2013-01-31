open AbstractSyntax

(* ------------------------------------------------------------------------- *)

(* The constraint generator is parameterized over a data constructor
   table and a main expression. It produces a unification problem with
   a distinguished variable (which represents the type of the main
   expression). *)

module Run (G : sig

  val dcenv: Wf.data_constructor_table
  val main: expression

end) : sig

  val problem: Unification.problem
  val root: Unification.variable

end

