Require Import List.
Require Import Omega.
Require Import pastis.Utils.

Open Scope Z.


Inductive rewrite_ast :=
  | F_check_ge (x y: Z)
  | F_one
  | F_max0_ge_0 (x: Z)
  | F_max0_ge_arg (x: Z)
  | F_max0_le_arg (p: rewrite_ast)
  | F_max0_monotonic (p: rewrite_ast)
  | F_max0_sublinear (p: rewrite_ast)
  | F_max0_pre_decrement (k: nat) (x y: Z)
  | F_max0_pre_increment (k: nat) (x y: Z)
  | F_binom_monotonic (k: nat) (p q: rewrite_ast)
  | F_product (p q: rewrite_ast)
.


(* A simple datatype to represent inequalities
   between integers together with its semantics. *)
Inductive ineq :=
  | ige (a: Z) (b: Z)
  | ige0 (a: Z)
.
Notation "a '>= b" := (ige a b) (at level 70).
Notation "a '>= 0" := (ige0 a) (at level 70).

Definition ineq_prop (i: ineq) :=
  match i with
    | a '>= b => a >= b
    | a '>= 0 => a >= 0
  end.


(* [ineq_ge] and [ineq_ge0] take an inequality and
   ``project'' it in the form [a >= b] or [a >= 0]
   respectively. *)
Definition ineq_ge (i: ineq) :=
  match i with a '>= b => (a, b) | a '>= 0 => (a, 0) end.
Definition ineq_ge0 (i: ineq) :=
  match i with a '>= b => a - b | a '>= 0 => a end.

Lemma ineq_ge_semantics:
  forall p x y, ineq_ge p = (x, y) -> ineq_prop p -> x >= y.
Proof.
  destruct p; intros; simpl in *; congruence.
Qed.
Lemma ineq_ge0_semantics:
  forall p x, ineq_ge0 p = x -> ineq_prop p -> x >= 0.
Proof.
  destruct p; intros; simpl in *; omega.
Qed.
Local Hint Resolve ineq_ge_semantics.
Local Hint Resolve ineq_ge0_semantics.


(* The interpretation type for the syntax [rewrite_ast]
   using the [ineq] type.  The type is a pair whose
   second member is a conclusion true if all the
   hypotheses in the first member are true. *)
Definition interp := (list ineq * ineq)%type.
Notation "[ a |- c ]" := (a, c) (only parsing).

(* Interpretation function. *)
Import ListNotations.
Fixpoint interpret (p: rewrite_ast): interp :=
  match p with

    | F_check_ge x y =>
      [ [x '>= y] |- x '>= y ]

    | F_one =>
      [ [] |- 1 '>= 0 ]

    | F_max0_ge_0 x =>
      [ [] |- max0 x '>= 0 ]

    | F_max0_ge_arg x =>
      [ [] |- max0 x '>= x ]

    | F_max0_le_arg q =>
      let (a, c) := interpret q in
      let x := ineq_ge0 c in
      [ a |- x '>= max0 x ]

    | F_max0_monotonic q =>
      let (a, c) := interpret q in
      let (x, y) := ineq_ge c in
      [ a |- max0 x '>= max0 y ]

    | F_max0_sublinear q =>
      let (a, c) := interpret q in
      let (x, y) := ineq_ge c in
      [ a |- max0 y + x - y '>= max0 x ]

    | F_max0_pre_decrement k x y =>
      [ [x '>= y; y '>= 0] |-
        prodn k (max0 x) 0 '>= prodn k (y + max0 (x - y)) 0 ]

    | F_max0_pre_increment k x y =>
      [ [y '>= 0] |-
        prodn k (y + max0 x) 0 '>= prodn k (max0 (x + y)) 0 ]

    | F_binom_monotonic k q r =>
      let (aq, cq) := interpret q in
      let (x, y) := ineq_ge cq in
      let (ar, cr) := interpret r in
      let y' := ineq_ge0 cr in
      [ app aq ar |-
        prodn k (x - y + y') 0 '>= prodn k y' 0 ]

    | F_product q r =>
      let (aq, cq) := interpret q in
      let (x, y) := ineq_ge cq in
      let (ar, cr) := interpret r in
      let m := ineq_ge0 cr in
      [ app aq ar |-
        m * x '>= m * y ]
  end.


Theorem rewrite_ast_semantics:
  forall (p: rewrite_ast)
         a c (INT: interpret p = (a, c))
         (HYPS: Forall ineq_prop a),
    ineq_prop c.
Proof.
  induction p; intros until 0; simpl;
  repeat match goal with
    | [ |- (let (_, _) := ?x in _) = _ -> _ ] =>
      case_eq x; intros until 1
    | [ |- Forall _ (app _ _) -> _ ] =>
      intro HYPS; apply Forall_app in HYPS; destruct HYPS
    | [ |- (_, _) = (_, _) -> _ ] => injection 1
    | [ |- _ = _ -> _ ] => destruct 1
  end;
  intros; simpl in *.
  + tauto.
  + auto with zarith.
  + apply ZLemmas.lem_max0_ge_0.
  + apply ZLemmas.lem_max0_ge_arg.
  + apply ZLemmas.lem_max0_le_arg; eauto.
  + apply ZLemmas.lem_max0_monotonic; eauto.
  + apply ZLemmas.lem_max0_sublinear; eauto.
  + apply ZLemmas.lem_max0_pre_decrement; tauto.
  + apply ZLemmas.lem_max0_pre_increment; tauto.
  + assert (z >= z0) by eauto.
    assert (ineq_ge0 i0 >= 0) by eauto.
    apply ZLemmas.lem_prodn_monotonic; omega.
  + apply Zmult_ge_compat_l; eauto.
Qed.
