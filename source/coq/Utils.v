Require Import List.
Require Import ZArith.
Require Import QArith.


Section Forall.

  Variable A: Type.
  Variable P: A -> Prop.

  Fixpoint Forall (l: list A): Prop :=
    match l with
      | x :: l' => P x /\ Forall l'
      | nil => True
    end.
  Global Implicit Arguments Forall.

  Theorem Forall_In:
    forall l, Forall l -> forall x, In x l -> P x.
  Proof.
    induction l.
    + now auto.
    + simpl; intuition.
      congruence.
  Qed.

End Forall.


Global Coercion inject_Z: Z >-> Q.

Definition max0 x := Z.max 0 x.

(* Some convenient lemmas about the max0 functions
   and Z in general.  They are used in the proof
   of soundness of the various building blocks for
   rewrite functions.
 *)
Module ZLemmas.

  Require Import Omega.
  Open Scope Z.

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
