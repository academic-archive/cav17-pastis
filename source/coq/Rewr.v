Require Import List.
Require Import Omega.

Open Scope Z.


Inductive rewrite_ast :=
  | F_check_ge (x: Z) (y: Z)
  | F_max0_ge_0 (x: Z)
  | F_max0_ge_arg (x: Z)
  | F_max0_le_arg (p: rewrite_ast)
  | F_max0_monotonic (p: rewrite_ast)
  | F_max0_sublinear (p: rewrite_ast)
  | F_max0_pre_decrement (x: Z) (y: Z)
  | F_max0_pre_increment (x: Z) (y: Z)
.


(* A simple datatype to represent inequalities
 * between integers together with its semantics.
 *)
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
 * ``project'' it in the form [a >= b] or [a >= 0]
 * respectively.
 *)
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
 * using the [ineq] type.  The type is a pair whose
 * second member is a conclusion true if all the
 * hypotheses in the first member are true.
 *)
Definition interp := (list ineq * ineq)%type.
Notation "[ a |- c ]" := (a, c) (only parsing).

Definition max0 x := Z.max 0 x.

(* Interpretation function. *)
Import ListNotations.
Fixpoint interpret (p: rewrite_ast): interp :=
  match p with

    | F_check_ge x y =>
      [ [x '>= y] |- x '>= y ]

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

    | F_max0_pre_decrement x y =>
      [ [x '>= y; y '>= 0] |-
        max0 x '>= y + max0 (x - y) ]

    | F_max0_pre_increment x y =>
      [ [y '>= 0] |-
        y + max0 x '>= max0 (x + y) ]

  end.


Section ZLemmas.

  Lemma lem_max0_ge_0:
    forall x, max0 x >= 0.
  Proof.
    intros. apply Z.le_ge, Z.le_max_l.
  Qed.

  Lemma lem_max0_ge_arg:
    forall x, max0 x >= x.
  Proof.
    intros. apply Z.le_ge, Z.le_max_r.
  Qed.

  Lemma lem_max0_le_arg:
    forall x, x >= 0 -> x >= max0 x.
  Proof.
    intros. apply Z.le_ge, Z.max_lub; auto with zarith.
  Qed.

  Lemma lem_max0_monotonic:
    forall x y, x >= y -> max0 x >= max0 y.
  Proof.
    intros. apply Z.le_ge, Z.max_le_compat_l. auto with zarith.
  Qed.

  Lemma lem_max0_sublinear:
    forall x y, x >= y -> max0 y + x - y >= max0 x.
  Proof.
    intros x y Hxy. apply Z.le_ge, Z.max_lub.
    + generalize (lem_max0_ge_0 y). omega.
    + generalize (lem_max0_ge_arg y). omega.
  Qed.

  Lemma lem_max0_pre_decrement:
    forall x y, x >= y -> y >= 0 -> max0 x >= y + max0 (x - y).
  Proof.
    intros x y Hxy Hy0. apply Z.le_ge.
    replace (max0 x) with x
      by (symmetry; apply Z.max_r; auto with zarith).
    generalize (lem_max0_le_arg (x - y)).
    omega.
  Qed.

  Lemma lem_max0_pre_increment:
    forall x y, y >= 0 -> y + max0 x >= max0 (x + y).
  Proof.
    intros x y Hxy. apply Z.le_ge, Z.max_lub.
    + generalize (lem_max0_ge_0 x). omega.
    + generalize (lem_max0_ge_arg x). omega.
  Qed.

End ZLemmas.

Fixpoint Forall {A: Type} (P: A -> Prop) (l: list A): Prop :=
  match l with
    | x :: l' => P x /\ Forall P l'
    | [] => True
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
    | [ |- (_, _) = (_, _) -> _ ] => injection 1
  end;
  intros; subst; simpl in *.
  + tauto.
  + apply lem_max0_ge_0.
  + apply lem_max0_ge_arg.
  + apply lem_max0_le_arg; eauto.
  + apply lem_max0_monotonic; eauto.
  + apply lem_max0_sublinear; eauto.
  + apply lem_max0_pre_decrement; tauto.
  + apply lem_max0_pre_increment; tauto.
Qed.