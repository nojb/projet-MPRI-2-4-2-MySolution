(* This file implements the monadic translation for exceptions. *)

open AbstractSyntax

(* The translation uses the following algebraic data type:

     type ('a, 'b) %mon = | %V of 'a | %E of 'b

Results of the form %V(v) denote normal termination with value v.
Results of the form %E(v) denote abrupt termination on an uncaught exception
whose value is v.

% signs are used in the names of the type constructor and data
constructors so that they cannot conflict with names already present
in the source program.
*)

let montype : type_constructor = "%mon"
let vconstr : data_constructor = "%V"
let econstr : data_constructor = "%E"

let mondecl : data_type_definition =
  DefDataType(["a"; "b"], montype,
              [DefCon(vconstr, [TVar "a"]);
               DefCon(econstr, [TVar "b"])])

(* A generator for fresh variable names.  Again, we use a % so that
   these names cannot appear in the source code. *)

let gensym_counter = ref 0

let gensym() =
  incr gensym_counter; Printf.sprintf "%%%d" !gensym_counter

(* We provide optimizing implementations of some of the basic monadic
   operations.   Notice how the implementation of [bind] avoids
   generating administrative redexes. *)

let return (v : expression) = EConApp(vconstr, [v])

(* "bind a f" is equivalent to
      match a with
      | %V(x) -> f x
      | %E(x) -> %E(x)
   where "x" is a fresh variable.
   It avoids generating the "match" when the shape of "a" allows so. *)

let bind (a : expression) (f : expression -> expression) =
  match a with
  | EConApp(cstr, [v]) when cstr = vconstr ->
     f v
  | EConApp(cstr, [v]) when cstr = econstr ->
     EConApp(econstr, [v])
  | _ ->
     let x = gensym() in
     EMatch(a, [Branch(PConApp(vconstr, [x]), f (EVar x));
                Branch(PConApp(econstr, [x]), EConApp(econstr, [EVar x]))])

(* "bindlist al f" is similar, for a list al = [a1; ...; aN] of expressions:
      match a1 with
      | %E(x1) -> %E(x1)
      | %V(x1) ->
          ...
          match aN with
          | %E(xN) -> %E(xN)
          | %V(xN) -> f [x1, ..., xN]
*)

let rec bindlist (al : expression list) (f : expression list -> expression) =
  match al with
  | [] -> f []
  | hd :: tl ->
      bind hd (fun hd' -> bindlist tl (fun tl' -> f (hd' :: tl')))

let rec transl_expr = function
  | EVar v ->
      return (EVar v)
  | EFun (v, e) ->
      return (EFun (v, transl_expr e))
  | ERecFun (f, v, e) ->
      return (ERecFun (f, v, transl_expr e))
  | EApp (e1, e2) ->
      bind (transl_expr e1) (fun e1 ->
        bind (transl_expr e2)
          (fun e2 -> EApp (e1, e2)))
  | EConApp (constr, al) ->
      bindlist (List.map transl_expr al)
        (fun al -> return (EConApp (constr, al)))
  | ELet (v, e1, e2) ->
      bind (transl_expr e1)
        (fun e1 -> ELet (v, e1, transl_expr e2))
  | EMatch (a, branches) ->
      transl_match a branches
  | ERaise (e) ->
      bind (transl_expr e)
        (fun e -> EConApp (econstr, [e]))
  | ETryWith (e1, v, e2) ->
      let x = gensym() in
      EMatch (transl_expr e1,
        [Branch (PConApp (vconstr, [x]), return (EVar x));
        Branch (PConApp (econstr, [v]), transl_expr e2)])
  | EExists (ts, e) ->
      EExists (ts, transl_expr e)
      (* bind (transl_expr e)
        (fun e -> return (EExists (ts, e))) *)
  | ETypeAnnotation (e, t) ->
      bind (transl_expr e)
        (fun e -> return (ETypeAnnotation (e, t)))

and transl_match a branches =
  let transl_branch (Branch (pat, e)) =
    Branch (pat, transl_expr e) in
  bind (transl_expr a)
    (fun a -> EMatch (a, List.map transl_branch branches))

(* [transl_program p] takes a program [p] written in full mini-ML,
   including the [raise] and [try...with] constructs, and returns
   a program [p'] in pure mini-ML, without the [raise] and [try...with]
   constructs. *)

let transl_program (Program(datatypes, body)) =
  Program(mondecl :: datatypes, transl_expr body) (* translation of [body] to be implemented *)


