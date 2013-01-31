(* This file defines the abstract syntax of our small programming language. *)

(* ------------------------------------------------------------------------- *)

(* We begin with four kinds of identifiers: value variables, type variables,
   data constructors, and type constructors. All of them are represented as
   strings. *)

(* Value variables. *)

type value_variable =
    string

(* Type variables. *)

type type_variable =
    string

(* Data constructors. *)

type data_constructor =
    string

(* Algebraic data type constructors. *)

type type_constructor =
    string

(* ------------------------------------------------------------------------- *)

(* Types. *)

(* Types can appear within algebraic data type definitions and within
   expressions. *)

(* A type can be: a type variable; a function type; an application of
   a type constructor to an appropriate number of type arguments. *)

(* Note: [TArrow] is really superfluous, since it is a particular case
   of [TConApp]. We keep it, for no good reason. *)

(* In the abstract syntax, type variables are represented by values of
   type [type_variable], that is, strings. However, when performing
   first-order unification, we will use an internal representation for
   type variables. For this reason, we define a parameterized type
   ['var ptyp] of types whose variables are represented by values of
   type ['var]. *)

type 'var ptyp =
  | TVar of 'var
  | TArrow of 'var ptyp * 'var ptyp              (* function type t1 -> t2 *)
  | TConApp of type_constructor * 'var ptyp list (* (t1, ..., tn) typeconstr *)

type typ =
    type_variable ptyp

(* ------------------------------------------------------------------------- *)

(* Case analysis patterns. *)

type pattern =
  | PConApp of data_constructor * value_variable list

(* Case analysis branches. *)

type branch =
  | Branch of pattern * expression

(* ------------------------------------------------------------------------- *)

(* Expressions. *)

(* An expression can be: a value variable; an abstraction (that is, an
   anonymous function); a recursive abstraction; a function
   application; a tuple (of arbitrary arity); an application of a data
   constructor to a single argument; a case analysis; an introduction
   of an existentially quantified type variable; a type annotation. *)

and expression =
  | EVar of value_variable
  | EFun of value_variable * expression                      (* lambda x. a *)
  | ERecFun of value_variable * value_variable * expression  (* mu f. lambda x. a *)
  | EApp of expression * expression                          (* a b *)
  | EConApp of data_constructor * expression list            (* Cstr(a1,...,aN) *)
  | ELet of value_variable * expression * expression         (* let x = a in b *)
  | EMatch of expression * branch list                       (* match a with ... *)
  | ERaise of expression                                     (* raise a *)
  | ETryWith of expression * value_variable * expression     (* try a with x -> b *)
  | EExists of type_variable list * expression               (* exists v1...vn. a *)
  | ETypeAnnotation of expression * typ                      (* (a : typ) *)

(* ------------------------------------------------------------------------- *)

(* A data constructor definition consists of the data constructor's name,
   together with the types of its arguments. *)

type data_constructor_definition =
  | DefCon of data_constructor * typ list

(* A data type definition consists of a list of formal type
   parameters, the type constructor's name, and a list of data
   constructor definitions. *)

type data_type_definition =
  | DefDataType of type_variable list * type_constructor * data_constructor_definition list

(* A complete program consists of a list of data type definitions and
   an expression. *)

type program =
  | Program of data_type_definition list * expression

