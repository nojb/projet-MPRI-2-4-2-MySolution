(* This file defines compilation from our programming language
   to our abstract machine. *)

exception CompilationError of string
      (* Exception used to report compilation errors *)

val compile_program : AbstractSyntax.program -> Machine.code
      (* Return the machine code corresponding to the given source program. *)
