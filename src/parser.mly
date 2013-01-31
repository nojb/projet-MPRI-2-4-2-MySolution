(* In this simple language, we attempt to stick mostly to ocaml syntax.
   There are a few departures, though: we use MATCH ... WITH ... END
   instead of MATCH ... WITH; tuples must be explicitly parenthesized;
   and the keyword PROGRAM is used to announce the program body. *)

%{

open AbstractSyntax

%}

%token <string> IDENTIFIER CONSTRUCTOR
%token LPAR COMMA RPAR
%token TYPE DEFEQ BAR
%token QUOTE ARROW STAR
%token LET REC FUN MATCH WITH END OF IN EXISTS DOT COLON RAISE TRY 
%token PROGRAM EOF
%start <AbstractSyntax.program> program

(* In ocaml syntax, constructor application (EConApp) and function
   application (EApp) have the same syntax, which makes the grammar
   ambiguous. We work around this issue by giving appropriate priority
   declarations. Here, we must prefer shifting LPAR, CONSTRUCTOR, and
   IDENTIFIER over reducing the production expression0 ->
   CONSTRUCTOR. *)

%nonassoc lone_constructor
%nonassoc LPAR CONSTRUCTOR IDENTIFIER

%%

(* ------------------------------------------------------------------------- *)

(* Parenthesized tuples. *)

parenthesized_tuple(X):
| LPAR xs = separated_list(COMMA, X) RPAR
    { xs }

(* ------------------------------------------------------------------------- *)

(* This parameterized non-terminal symbol recognizes separated lists of at
   least two elements -- which means that at least one separator is
   explicit. The definition is inlined, so as to make the first separator
   visible at the instantiation site. *)

%inline separated_list_of_two_or_more(separator, X):
| head = X separator tail = separated_nonempty_list(separator, X)
    { head :: tail }

(* ------------------------------------------------------------------------- *)

(* In ocaml syntax, BAR can be used either as a delimiter or as an opening
   symbol. That is, the first BAR is optional. Here is a way of saying so. *)

bar(X):
| xs = preceded(BAR, X)+
| xs = separated_nonempty_list(BAR, X)
    { xs }

(* ------------------------------------------------------------------------- *)

(* In ocaml syntax, the parameters of a data constructor or type
   constructor can be absent, a single parameter (without
   parentheses), or a tuple of parameters (with parentheses and
   commas). *)

parameters(X):
| (* epsilon *)
    { [] }
| x = X
    { [ x ] }
| LPAR xs = separated_list_of_two_or_more(COMMA, X) RPAR
    { xs }

(* ------------------------------------------------------------------------- *)

(* Types. *)

type_variable:
| QUOTE x = IDENTIFIER
    { x }

typ0:
| x = type_variable
    { TVar x }
| ts = parameters(typ0) t = IDENTIFIER
    { TConApp (t, ts) }
| LPAR t = typ RPAR
    { t }

typ:
| t = typ0
    { t }
| t1 = typ0 ARROW t2 = typ
    { TArrow (t1, t2) }

(* ------------------------------------------------------------------------- *)

(* Patterns. *)

pattern:
| tag = CONSTRUCTOR xs = parameters(IDENTIFIER)
    { PConApp (tag, xs) }

(* ------------------------------------------------------------------------- *)

(* Expressions. *)

expression0:
| x = IDENTIFIER
    { EVar x }
| tag = CONSTRUCTOR %prec lone_constructor
    { EConApp (tag, []) }
| LPAR e = expression RPAR
   { e }
| LPAR e = expression COLON t = typ RPAR
    { ETypeAnnotation (e, t) }

expression1:
| e = expression0
    { e }
| e1 = expression1 e2 = expression0
    { EApp (e1, e2) }
| tag = CONSTRUCTOR e = expression0
    { EConApp (tag, [ e ]) }
| tag = CONSTRUCTOR LPAR es = separated_list_of_two_or_more(COMMA, expression) RPAR
    { EConApp (tag, es) }

expression:
| e = expression1
    { e }
| FUN x = IDENTIFIER ARROW e = expression
    { EFun (x, e) }
| MATCH e = expression WITH bs = bar(branch) END
    { EMatch (e, bs) }
| LET x = IDENTIFIER DEFEQ e1 = expression IN e2 = expression
    { ELet (x, e1, e2) }
| LET p = pattern DEFEQ e1 = expression IN e2 = expression
    { EMatch (e1, [ Branch (p, e2) ]) }
| LET f = IDENTIFIER x = IDENTIFIER DEFEQ e1 = expression IN e2 = expression
    { ELet (f, EFun (x, e1), e2) }
| LET REC f = IDENTIFIER x = IDENTIFIER DEFEQ e1 = expression IN e2 = expression
    { ELet (f, ERecFun (f, x, e1), e2) }
| EXISTS xs = type_variable+ DOT e = expression
    { EExists (xs, e) }
| RAISE e = expression
    { ERaise e }
| TRY e1 = expression WITH x = IDENTIFIER ARROW e2 = expression END
    { ETryWith(e1, x, e2) }

branch:
| p = pattern ARROW e = expression
    { Branch (p, e) }

(* ------------------------------------------------------------------------- *)

(* Data type definitions. *)

constructor_parameters:
| (* epsilon *)
    { [] }
| OF ts = separated_nonempty_list(STAR, typ)
    { ts }

data_constructor_definition:
| tag = CONSTRUCTOR ts = constructor_parameters
    { DefCon (tag, ts) }

data_type_definition:
| TYPE xs = parameters(type_variable) x = IDENTIFIER
  DEFEQ cs = bar(data_constructor_definition)
    { DefDataType (xs, x, cs) }

(* ------------------------------------------------------------------------- *)

(* Programs. *)

program:
| defs = data_type_definition*
  PROGRAM e = expression EOF
    { Program (defs, e) }

