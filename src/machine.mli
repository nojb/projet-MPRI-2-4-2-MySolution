(* This file defines an abstract machine based on the Modern SECD. *)

open AbstractSyntax

(* The instruction set for this machine *)

type instruction =
    IAccess of int
      (* [IAccess n] pushes the [n]-th entry of the environment on the stack.
         The first entry of the environment has [n] = 0. *)
  | IClosure of code
      (* [IClosure c] pushes on the stack a closure of code [c]
         with the current environment *)
  | IRecClosure of code
      (* [IRecClosure c] pushes on the stack a recursive closure of code [c]
         with the current environment.  
         This closure is of the form v = c[v.e], that is, the first entry
         of its environment is the closure itself. *)
  | ITailapply
      (* Pop an argument [v], a closure [c[e]] and apply this closure to
         this environment.  No return frame is pushed. *)
  | IApply
      (* Pop an argument [v], a closure [c[e]] and apply this closure to
         this environment.  A return frame is pushed, enabling the next
         [IReturn] instruction to branch back to the code following this
         [IApply]. *)
  | IReturn
      (* Pop a return frame and branch to it *)
  | ILet
      (* Pop a value and add it to the environment *)
  | IEndlet of int
      (* [IEndlet n] discards the first [n] entries of the environment. *)
  | IDup
      (* [IDup] duplicates the top of the stack: 
           stack before [v.s]    --->  stack after [v.v.s] *)
  | IDrop
      (* [IDrop] discards the top of the stack: 
           stack before [v.s]    --->  stack after [s] *)
  | IConstr of data_constructor * int
      (* [IConstr(cstr, N)] pops [N] values [vN, ..., v1] off the stack
         and pushes back the constructed value [C(v1, ..., vN)]. *)
  | IField of int
      (* [IField(N)] pops a constructed value [C(v0, ..., vM)]
         off the stack and pushes back the value [vN] of its [N]-th
         argument.  Arguments are counted starting from 0. *)
  | IIfconstr of data_constructor * code * code
      (* [Iifconstr(cstr, c1, c2)] pops a constructed value [C(...)]
         off the stack.  If the constructor [C] of the value is equal
         to the constructor [cstr] given in the instruction,
         execution proceeds with code [c1], then the code following
         the [Iifconstr].  If the constructor [C] is different from [cstr],
         execution proceeds with code [c2], then the code following
         the [Iifconstr]. *)
  | IHalt
      (* Abort the machine on an error. *)

and code = instruction list

val print_program : code -> unit
      (* Print the given machine code to standard output *)

val trace : bool ref
      (* When set to [true], the machine will print its current state
         before each transition.  Very useful to debug code generated
         by a compiler. *)

val execute_program : code -> unit
      (* Execute a code and print its result value. *)
