(* This file defines a compiler from our programming language
   to our abstract machine. *)

open AbstractSyntax
open Machine

(* Exception used to report compilation errors *)
exception CompilationError of string

let index v l =
  let rec loop n = function
  | [] -> raise (CompilationError ("variable not found: " ^ v))
  | v1 :: vs ->
      if v1 = v then n
      else loop (n + 1) vs
  in loop 0 l

(* Optimizes away the sequence IDup; IDrop *)
let dup k =
  match k with
  | IDrop :: k -> k
  | _ -> IDup :: k

let rec compile (env : value_variable list) e k =
  match e with
  | EVar v ->
      IAccess (index v env) :: k
  | EFun (v, e) ->
      IClosure (compile (v :: env) e [IReturn]) :: k
  | ERecFun (f, v, e) ->
      IRecClosure (compile (v :: f :: env) e [IReturn]) :: k
  | EApp (f, e) ->
      compile env f (compile env e (IApply :: k))
  | EConApp (d, el) ->
      List.fold_right (compile env) el
        (IConstr (d, List.length el) :: k)
  | ELet (v, e1, e2) ->
      compile_let env v e1 e2 k
  | EMatch (e, branches) ->
      compile_match env e branches k
  | ERaise _
  | ETryWith _ ->
      raise (CompilationError "direct compilation of [Raise] and [TryWith] is not supported")
  | EExists (_, e)
  | ETypeAnnotation (e, _) ->
      compile env e k

      (* [compile_match] generates a cascade of [IIfconstr] instructions
       * to check each branch of the match expression. Note that
       * if no branch succeeds we halt the machine (and we leave the offending
       * expression to be matched in top of the stack) *)
and compile_match env e branches k =
  let rec compile_branch (Branch (PConApp (d, vl), e)) ifnot =
    let rec loop n env = function
      | [] -> IDrop :: compile env e []
      | v :: [] ->
          IField n :: ILet :: compile (v :: env) e [IEndlet (n + 1)]
      | v :: vl ->
          IDup :: IField n :: ILet :: loop (n + 1) (v :: env) vl
    in
    IIfconstr (d, loop 0 env vl, dup [ifnot])
  in
  compile env e
    (IDup :: List.fold_right compile_branch branches IHalt :: k)

and compile_let env v e1 e2 k =
  let rec loop n env = function
    | ELet (v1, e1, e2) ->
        compile env e1
          (ILet :: loop (n + 1) (v1 :: env) e2)
    | _ as e  ->
        compile env e (IEndlet n :: k)
  in
  compile env e1 (ILet :: loop 1 (v :: env) e2)

(* Return the machine code (of type [Machine.code]) corresponding to
   the given source program. *)

let compile_program (Program(datatypes, body)) =
  compile [] body []
