Require Import List.
Require Import ZArith.
Require Import QArith.
Require Import Omega.


Section Forall.

  Variable A: Type.
  Variable P: A -> Prop.

  Fixpoint Forall (l: list A): Prop :=
    match l with
      | x :: l' => P x /\ Forall l'
      | nil => True
    end.

  Theorem Forall_In:
    forall l, Forall l -> forall x, In x l -> P x.
  Proof.
    induction l.
    + now auto.
    + simpl; intuition.
      congruence.
  Qed.

  Theorem In_Forall:
    forall l, (forall x, In x l -> P x) -> Forall l.
  Proof.
    induction l.
    + intros; constructor.
    + split. apply H. left. auto.
      apply IHl. intros. apply H. right. auto.
  Qed.

  Theorem Forall_app:
    forall l l', Forall (app l l') -> (Forall l /\ Forall l').
  Proof.
    induction l; intros; simpl in *.
    + tauto.
    + intuition.
      - refine (proj1 (IHl l' _)); assumption.
      - refine (proj2 (IHl l' _)); assumption.
  Qed.

End Forall.
Global Arguments Forall {A} P l.


Open Scope Z.

Global Coercion inject_Z: Z >-> Q.

Definition max0 x := Z.max 0 x.

Fixpoint prodn (n: nat) (k l: Z) :=
  match n with
  | O => 1
  | S n' => (k - l) * prodn n' k (l + 1)
  end.

(* Some convenient lemmas about the max0 functions
   and Z in general.  They are used in the proof
   of soundness of the various building blocks for
   rewrite functions.
 *)
Module ZLemmas.

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

  Lemma lem_prodn_nonneg:
    forall k x l, 0 <= l -> l <= x -> 0 <= prodn k x l.
  Proof.
    induction k.
    + intros; now auto with zarith.
    + intros x l X0 L0. simpl.
      assert (XDISJ: x = l \/ l + 1 <= x) by omega.
      destruct XDISJ as [XEQL | XGTL].
      - rewrite XEQL, Z.sub_diag, Z.mul_0_l.
        now apply Z.le_refl.
      - apply Z.mul_nonneg_nonneg;
        [| apply IHk ]; omega.
  Qed.

  Lemma lem_prodn_monotonic:
    forall k x y l, x >= y -> y >= l -> l >= 0 ->
                    prodn k x l >= prodn k y l.
  Proof.
    induction k.
    + intros; apply Z.le_ge, Z.le_refl.
    + intros x y l XY YL L0. simpl.
      assert (YDISJ: y = l \/ y >= l + 1) by omega.
      destruct YDISJ as [YEQL | YGTL].
      - rewrite YEQL, Z.sub_diag, Z.mul_0_l.
        assert (XDISJ: x = l \/ x >= l + 1) by omega.
        destruct XDISJ as [XEQL | XGTL].
        * rewrite XEQL, Z.sub_diag, Z.mul_0_l.
          now apply Z.le_ge, Z.le_refl.
        * apply Z.le_ge, Z.mul_nonneg_nonneg;
          [| apply lem_prodn_nonneg ]; omega.
      - apply Z.le_ge, Z.mul_le_mono_nonneg; try omega.
        apply lem_prodn_nonneg; omega.
        apply Z.ge_le, IHk; omega.
  Qed.

  Lemma lem_max0_pre_decrement:
    forall k x y, x >= y -> y >= 0 ->
                  prodn k (max0 x) 0 >= prodn k (y + max0 (x - y)) 0.
  Proof.
    intros k x y Hxy Hy0. apply lem_prodn_monotonic.
    - apply Z.le_ge.
      replace (max0 x) with x
        by (symmetry; apply Z.max_r; auto with zarith).
      generalize (lem_max0_le_arg (x - y)). omega.
    - generalize (lem_max0_ge_0 (x - y)). omega.
    - auto with zarith.
  Qed.

  Lemma lem_max0_pre_increment:
    forall k x y, y >= 0 ->
                  prodn k (y + max0 x) 0 >= prodn k (max0 (x + y)) 0.
  Proof.
    intros k x y Hxy. apply lem_prodn_monotonic.
    - apply Z.le_ge, Z.max_lub.
      + generalize (lem_max0_ge_0 x). omega.
      + generalize (lem_max0_ge_arg x). omega.
    - apply lem_max0_ge_0.
    - auto with zarith.
  Qed.

End ZLemmas.
