(* Default settings (from HsToCoq.Coq.Preamble) *)

Generalizable All Variables.

Unset Implicit Arguments.
Set Maximal Implicit Insertion.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Require Coq.Program.Tactics.
Require Coq.Program.Wf.

(* Preamble *)

Require Import Prelude.
Import ListNotations.

(* Converted imports: *)

Require Coq.Init.Datatypes.
Require Coq.Lists.List.
Require Data.Traversable.
Require Data.Functor.
Require Control.Monad.
Require GHC.Base.
Require GHC.Err.
Require GHC.List.
Require GHC.Num.
Import GHC.Base.Notations.

(* Converted type declarations: *)

Definition Vertex :=
  GHC.Num.Int%type.

Definition Edges :=
  (list (Vertex * Vertex)%type)%type.

Inductive Graph : Type
  := | MkGraph (getVertices : list Vertex) (getEdges : Edges) : Graph.

Inductive Color : Type := | R : Color |  G : Color |  B : Color.

Definition Coloring :=
  (list Color)%type.

Inductive Var : Type := | MkVar : Vertex -> Color -> Var.

Inductive Literal : Type := | Lit : Var -> Literal |  NLit : Var -> Literal.

Definition Solution :=
  (list (Var * bool)%type)%type.

Definition Clause :=
  (list Literal)%type.

Definition CNF :=
  (list Clause)%type.

Definition SatSolver :=
  (CNF -> option Solution)%type.

Instance Default__Graph : GHC.Err.Default Graph :=
  GHC.Err.Build_Default _ (MkGraph GHC.Err.default GHC.Err.default).

Instance Default__Color : GHC.Err.Default Color := GHC.Err.Build_Default _ R.

Definition getEdges (arg_0__ : Graph) :=
  let 'MkGraph _ getEdges := arg_0__ in
  getEdges.

Definition getVertices (arg_0__ : Graph) :=
  let 'MkGraph getVertices _ := arg_0__ in
  getVertices.

(* Converted value declarations: *)

Definition verticesColorUniqueness : Graph -> CNF :=
  fun g =>
    getVertices g GHC.Base.>>=
    (fun v =>
       let diffCols :=
         fun c1 c2 => cons (NLit (MkVar v c1)) (cons (NLit (MkVar v c2)) nil) in
       cons (diffCols R B) (cons (diffCols B G) (cons (diffCols R G) nil))).

Definition vertexHasColor : Graph -> CNF :=
  fun g =>
    getVertices g GHC.Base.>>=
    (fun v =>
       GHC.Base.return_ (Coq.Lists.List.flat_map (fun c => cons (Lit (MkVar v c)) nil)
                                                 (cons R (cons G (cons B nil))))).

Definition op_zlzbzg__ {a} : option a -> option a -> option a :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | Some x, _ => Some x
    | _, Some x => Some x
    | _, _ => None
    end.

Notation "'_<|>_'" := (op_zlzbzg__).

Infix "<|>" := (_<|>_) (at level 99).

Definition guard : bool -> option unit :=
  fun arg_0__ => match arg_0__ with | true => Some tt | false => None end.

Definition recoverAnswer : Graph -> option Solution -> option Coloring :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | _, None => None
    | graph, Some xs =>
        let f :=
          fun v =>
            let col :=
              fun c =>
                GHC.List.lookup (MkVar v c) xs GHC.Base.>>=
                (fun val => guard val GHC.Base.>> GHC.Base.return_ c) in
            (col R <|> col G) <|> col B in
        Data.Traversable.mapM f (getVertices graph)
    end.

Definition adjVerticesHaveDiffColors : Graph -> CNF :=
  fun g =>
    let cont_0__ arg_1__ :=
      let 'pair u v := arg_1__ in
      Coq.Lists.List.flat_map (fun c =>
                                 cons (cons (NLit (MkVar u c)) (cons (NLit (MkVar v c)) nil)) nil) (cons R (cons
                                                                                                          G (cons B
                                                                                                                  nil))) in
    getEdges g GHC.Base.>>= cont_0__.

Definition generateCNF : Graph -> CNF :=
  fun g =>
    Coq.Init.Datatypes.app (vertexHasColor g) (Coq.Init.Datatypes.app
                            (verticesColorUniqueness g) (adjVerticesHaveDiffColors g)).

Definition solve : SatSolver -> Graph -> option Coloring :=
  fun solver graph => recoverAnswer graph (solver (generateCNF graph)).

(* Skipping all instances of class `GHC.Enum.Enum', including
   `Reduction.Enum__Color' *)

Local Definition Eq___Color_op_zeze__ : Color -> Color -> bool :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | R, R => true
    | G, G => true
    | B, B => true
    | _, _ => false
    end.

Local Definition Eq___Color_op_zsze__ : Color -> Color -> bool :=
  fun x y => negb (Eq___Color_op_zeze__ x y).

Program Instance Eq___Color : GHC.Base.Eq_ Color :=
  fun _ k__ =>
    k__ {| GHC.Base.op_zeze____ := Eq___Color_op_zeze__ ;
           GHC.Base.op_zsze____ := Eq___Color_op_zsze__ |}.

Local Definition Eq___Var_op_zeze__ : Var -> Var -> bool :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | MkVar a1 a2, MkVar b1 b2 => (andb ((a1 GHC.Base.== b1)) ((a2 GHC.Base.== b2)))
    end.

Local Definition Eq___Var_op_zsze__ : Var -> Var -> bool :=
  fun x y => negb (Eq___Var_op_zeze__ x y).

Program Instance Eq___Var : GHC.Base.Eq_ Var :=
  fun _ k__ =>
    k__ {| GHC.Base.op_zeze____ := Eq___Var_op_zeze__ ;
           GHC.Base.op_zsze____ := Eq___Var_op_zsze__ |}.

Module Notations.
Notation "'_Reduction.<|>_'" := (op_zlzbzg__).
Infix "Reduction.<|>" := (_<|>_) (at level 99).
End Notations.

(* External variables:
     None Some andb bool cons false list negb nil op_zt__ option pair true tt unit
     Coq.Init.Datatypes.app Coq.Lists.List.flat_map Data.Traversable.mapM
     GHC.Base.Eq_ GHC.Base.op_zeze__ GHC.Base.op_zeze____ GHC.Base.op_zgzg__
     GHC.Base.op_zgzgze__ GHC.Base.op_zsze____ GHC.Base.return_ GHC.Err.Build_Default
     GHC.Err.Default GHC.Err.default GHC.List.lookup GHC.Num.Int
*)
