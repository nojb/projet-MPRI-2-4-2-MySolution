(* This file defines an interpreter for our small programming language *)

open Printf
open AbstractSyntax

(* This exception is raised when the interpreter encounters a fatal error *)
exception EvaluationError of string

(* Finite maps from value_variables to some data type *)
module Env =
  Map.Make(struct type t = value_variable let compare = compare end)

(* The representation of values (results of evaluation) *)
type value =
  | Con of data_constructor * value list
  | Closure of value_variable * value Env.t * expression

(* Environments are finite maps from value_variables to values *)
and environment = value Env.t

(* Return the value associated with [var] in [env]. *)
let find_var var env =
  try Env.find var env
  with Not_found -> raise (EvaluationError ("unbound variable " ^ var))

(* Return an environment that extends [env] with a binding from 
   [var] to [value] *)
let bind_var var value env =
  Env.add var value env

(* Print a value on standard output *)
let rec print_value v =
  match v with
  | Con(cstr, []) ->
      printf "%s" cstr
  | Con(cstr, v1 :: vl) ->
      printf "%s(" cstr;
      print_value v1;
      List.iter (fun v -> printf ", "; print_value v) vl;
      printf ")"
  | Closure _ ->
      printf "<fun>"

(* Exception raised by [eval env expr] if the evaluation of [expr]
   raises an exception and does not catch it.  The argument is the
   value of the exception. *)

exception UncaughtException of value

(* Y combinator to express recursion *)

(* Y = (\f. \x. f (\v. (x x) v)) (\x. f (\v. (x x) v)) *)

let y f =
  EFun (f,
    EApp (EFun ("x", EApp (EVar "x", EVar "x")),
      EFun ("x", EApp (EVar f,
        EFun ("y", EApp (EApp (EVar "x", EVar "x"), EVar "y"))))))

(* [eval env expr] returns the value of expression [expr] in environment [env] *)
let rec eval env expr =
  match expr with
  | EVar v ->
      Env.find v env
  | EFun (v, e) ->
      Closure (v, env, e)
  | ERecFun (f, x, a) ->
      eval env (EApp (EFun ("g", EApp (y "g", EVar "g")),
        EFun (f, EFun (x, a))))
  | EApp (e1, e2) -> begin
      let v2 = eval env e2 in
      match eval env e1 with
      | Closure (v, env, a) ->
          eval (Env.add v v2 env) a
      | _ -> raise (EvaluationError "bad lambda")
      end
  | EConApp (d, el) ->
      Con (d, List.map (eval env) el)
  | ELet (v, e1, e2) ->
      eval (Env.add v (eval env e1) env) e2
  | EMatch (e, bl) -> begin
      match eval env e with
      | Con (cstr, args) -> eval_match env cstr args bl
      | _ -> raise (EvaluationError "bad match")
      end
  | ERaise e ->
      raise (UncaughtException (eval env e))
  | ETryWith (e1, v, e2) ->
      begin try
        eval env e1
      with
        UncaughtException x -> eval (Env.add v x env) e2
      end
  | EExists (_, e)
  | ETypeAnnotation (e, _) ->
      eval env e

(* [eval_match env cstr args branches] evaluates a pattern-matching.
   The value being matched is [Con(cstr, args)].  *)

and eval_match env cstr args branches =
  match branches with
  | [] -> raise (EvaluationError "bad match")
  | Branch (PConApp (d, vs), e) :: branches ->
      if d = cstr then
        eval (List.fold_right2 Env.add vs args env) e
      else
        eval_match env cstr args branches

(* Evaluate the given program and print its value. *)
let eval_program (Program(datatypes, body)) =
  try
    print_value (eval Env.empty body); print_newline()
  with
  | UncaughtException vexn ->
      print_string "Uncaught exception "; print_value vexn; print_newline()
  | EvaluationError msg ->
      printf "Error during evaluation: %s\n" msg
