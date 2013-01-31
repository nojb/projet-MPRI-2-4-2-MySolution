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

end = struct

open G

(* ------------------------------------------------------------------------- *)

(* The constraint generator builds a unification problem. *)

module B =
  Unification.Build (struct end)

open B

(* ------------------------------------------------------------------------- *)

(* The constraint generator maintains a mapping [vvenv] of value
   variables to internal types (that is, unification terms) and a
   mapping [tvenv] of external type variables to internal type
   variables (that is, unification variables). *)

type vvenv =
   Unification.term StringMap.t

type tvenv =
   Unification.variable StringMap.t

(* ------------------------------------------------------------------------- *)
(* All exception values manipulated by the program must have the
   same type.  However, this can be any type.  We introduce a
   global type variable which will be unified with the types of
   exception values at the [raise] and [try...with] constructs. *)

let type_of_exn = TVar (fresh())

let lift_type tvenv t =
  Type.lift (fun v -> TVar (StringMap.find v tvenv)) t

(* The constraint generator accepts two environments [vvenv] and
   [tvenv], an expression [e], and an expected type [expected]. It
   (implicitly) emits a constraint and returns no explicit result. *)

let rec type_expr
    (vvenv : vvenv) (tvenv : tvenv)
    (e : expression) (expected : Unification.term) =
  match e with
  | EVar v ->
      StringMap.find v vvenv =?= expected
  | EFun (v, e) ->
      let t1 = TVar (fresh ()) in
      let t2 = TVar (fresh ()) in
      type_expr (StringMap.add v t1 vvenv) tvenv e t2;
      expected =?= TArrow (t1, t2)
  | ERecFun (f, v, e) ->
      let t1 = TVar (fresh ()) in
      let t2 = TVar (fresh ()) in
      let vvenv = StringMap.add v t1
        (StringMap.add f (TArrow (t1, t2)) vvenv) in
      type_expr vvenv tvenv e t2;
      expected =?= (TArrow (t1, t2))
  | EApp (e1, e2) ->
      let t = TVar (fresh ()) in
      type_expr vvenv tvenv e1 (TArrow (t, expected));
      type_expr vvenv tvenv e2 t
  | EConApp (cnstr, el) ->
      let Wf.Scheme (xs, ts, t) = StringMap.find cnstr dcenv in
      let tvenv' = List.fold_left
        (fun tvenv x -> StringMap.add x (fresh ()) tvenv) tvenv xs in
      List.iter2 (fun e t -> type_expr vvenv tvenv e (lift_type tvenv' t)) el ts;
      expected =?= lift_type tvenv' t
  | ELet (v, e1, e2) ->
      let t = TVar (fresh ()) in
      type_expr vvenv tvenv e1 t;
      type_expr (StringMap.add v t vvenv) tvenv e2 expected
  | EMatch (e, branches) ->
      type_match vvenv tvenv e branches expected
  | ERaise e ->
      type_expr vvenv tvenv e type_of_exn
  | ETryWith (e1, x, e2) ->
      type_expr vvenv tvenv e1 expected;
      type_expr (StringMap.add x type_of_exn vvenv) tvenv e2 expected
  | EExists (vl, e) ->
      let tvenv = List.fold_left (fun tvenv v ->
        StringMap.add v (fresh ()) tvenv) tvenv vl in
      type_expr vvenv tvenv e expected
  | ETypeAnnotation (e, t) ->
      expected =?= lift_type tvenv t;
      type_expr vvenv tvenv e expected

and type_match vvenv tvenv e branches expected =
  let t = TVar (fresh ()) in
  type_expr vvenv tvenv e t;
  List.iter (fun (Branch (PConApp (cnstr, vl), e)) ->
    let Wf.Scheme (xs, ts, t') = StringMap.find cnstr dcenv in
    let tvenv' = List.fold_left
      (fun tvenv x -> StringMap.add x (fresh ()) tvenv) tvenv xs in
    lift_type tvenv' t' =?= t;
    let vvenv = List.fold_left2 (fun vvenv v t ->
      StringMap.add v (lift_type tvenv' t) vvenv) vvenv vl ts in
    type_expr vvenv tvenv e expected) branches

(* ------------------------------------------------------------------------- *)

(* To conclude, apply the generator to the main expression and extract the
   resulting unification problem. *)

let root =
  fresh()

let problem =
  type_expr StringMap.empty StringMap.empty main (TVar root);
  finished()

end
