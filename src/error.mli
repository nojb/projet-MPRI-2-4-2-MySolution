(* This module helps report errors or informational messages. *)

(* This the type of source code locations. A location delimits a range of
   text in a source file. *)

type location =
    (Lexing.position * Lexing.position) option

(* [error locs msg] displays the error message [msg], referring to the
   locations [locs], and stops the program. [signal locs msg] is
   analogous, but does not stop the program. *)

val error: location list -> string -> 'a
val signal: location list -> string -> unit

(* [signaled()] stops the program if [signal] or [signalv] was
   previously called. *)

val signaled: unit -> unit

