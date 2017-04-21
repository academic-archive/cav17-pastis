Require Import ZArith.
Require Import QArith.
Require Import Psatz.
Require Import pasta.Utils.
Require Import pasta.CFG.
Require Import pasta.RewriteF.

Close Scope Z.
Close Scope Q.

Definition hints {A B: Type} (a: A) (b: B) := b.

(* This tactic simplifies goals that contain max0 terms. *)
Ltac simplify_max0 :=
  repeat match goal with
  | [ |- context[ max0 ?x ] ] => set (max0 x)
  end;
  repeat match goal with
  | [ m := max0 ?a |- _ ] =>
    (* Uniformize contents of max0 terms. *)
    repeat match goal with
    | [ |- context[ max0 ?b ] ] =>
      let H := fresh "H" in
      (assert (H: b = a) by reflexivity; fail 1) ||
      (assert (H: b = a) by ring; rewrite H; clear H)
    end;
    (* Reduce max0 when the argument is a constant. *)
    revert m; ring_simplify a
  end;
  repeat match goal with
  | [ |- context[ max0 (Zpos ?x) ] ] =>
    change (max0 (Zpos x)) with (Zpos x)
  | [ |- context[ max0 Z0 ] ] =>
    change (max0 Z0) with Z0
  | [ |- context[ max0 (Z.opp (Zpos ?x)) ] ] =>
    change (max0 (Z.opp (Zpos x))) with Z0
  | [ |- context[ max0 (Zneg ?x) ] ] =>
    change (max0 (Zneg x)) with Z0
  end.

(* These two definitions and the lemma below allow to use
   rewrite functions emitted in the Coq generation
   simply. *)
Definition rewrite_conclusion (a: rewrite_ast) :=
  (ineq_ge0 (snd (interpret a)) >= 0)%Z.
Definition rewrite_assumptions (a: rewrite_ast) :=
  Forall ineq_prop (fst (interpret a)).

Lemma use_rewrites:
  forall hints,
    Forall rewrite_assumptions hints ->
    Forall rewrite_conclusion hints.
Proof.
  induction 0.
  + tauto.
  + simpl; intuition.
    eapply ineq_ge0_semantics.
    reflexivity.
    case_eq (interpret a); intros.
    eapply rewrite_ast_semantics.
    eassumption.
    unfold rewrite_assumptions in H0.
    rewrite H2 in H0.
    assumption.
Qed.

(* Results of the Abstract Interpretation are formulas
   in Presburger arithmetic, so should be provable using
   Coq's lia. *)
Ltac prove_ai := repeat apply conj; auto; lia.

Ltac prove_weak :=
  match goal with
  | [ |- hints ?hintlist _ -> _ ] =>
    apply Qle_trans; refine (_ (use_rewrites hintlist _));
    unfold Qle, rewrite_conclusion, rewrite_assumptions;
    simpl in *; simplify_max0; lia
  | [ |- _ ] => apply Qle_trans, Qle_refl
  end.

(* Some handy lemmas to split goals. *)
Lemma split_conj (A B C D: Prop): (A -> C) -> (A -> B -> D) -> (A /\ B) -> (C /\ D).
Proof. intuition. Qed.
Lemma split_conj' (A B C D: Prop): (A -> (C /\ (B -> D))) -> (A /\ B) -> (C /\ D).
Proof. intuition. Qed.

(* When proving the VC of a call edge, we have
   as assumption all the specifications of the
   callee.  Here, we put them into useful form
   for the linear arithmetic tactics. *)
Ltac use_specs :=
  repeat match goal with
  | [ H: True |- _ ] => destruct H
  | [ H: (forall _, _ -> _) /\ _ |- _ ] => destruct H
  | [ H: forall z, ?AI /\ (?PRE <= z)%Q -> ?POST |- _ ] =>
    let ai := fresh "AIOK" in
    assert (ai: AI) by prove_ai;
    specialize (H PRE (conj ai (Qle_refl _)));
    clear ai
  end.

(* Prove the VC associated to an edge. *)
Ltac prove_edge :=
  match goal with
  | [ |- True ] => exact I

  (* Assignments, should be provable with ring. *)
  | [ |- forall _ _, step _ (AAssign _ _) _ -> _ ] =>
    inversion_clear 1; simpl; apply split_conj; intro;
    [ prove_ai
    | apply Qle_trans, Z.eq_le_incl;
      simpl; simplify_max0; ring (* Hopefully that does it... *)  
    ]

  (* Weakenings. *)
  | [ |- forall _ _, step _ AWeaken _ -> _ ] =>
    inversion_clear 1; apply split_conj; intro;
      [prove_ai | try prove_weak]

  (* No change in the potential on guards and empty edges. *)
  | [ |- forall _ _, step _ (AGuard _) _ -> _ ] =>
    inversion_clear 1; apply split_conj; unfold hints; intro;
      [prove_ai | apply Qle_trans, Qle_refl]
  | [ |- forall _ _, step _ ANone _ -> _ ] =>
    inversion_clear 1; apply split_conj; unfold hints; intro;
      [prove_ai | apply Qle_trans, Qle_refl]

  (* Procedure Calls. *)
  | [ |- forall _ _, step _ _ _ -> _ ] => idtac
  | [ |- _ ] =>
    (* Put the abstract state of s1 in the context. *)
    intros ? ? ?; apply split_conj'; unfold hints, exit;
    (* Specialize the PAs of the callee to the pre-call state (s1). *)
    simpl; intro; use_specs; split;
    [ prove_ai
    | apply Qle_trans; unfold Qle in *;
      simpl in *; simplify_max0; lia
    ]

  | [ |- _ ] => idtac
  end.

(* Prove that an IPA satisfies the VC. *)
Ltac prove_ipa_vc :=
  let P := fresh "Proc" in
  intro P; unfold PA_VC; destruct P; simpl;
  repeat apply conj; (* One goal per PA. *)
  match goal with
  | [ |- True ] => exact I
  | [ |- _ ] =>
    intro; repeat apply conj; (* One goal per edge. *)
      prove_edge
  end.

Ltac prove_bound ipa admissible_ipa Proc :=
  match eval hnf in (ipa Proc) with
  | cons ?pa _ =>
    let STEPS := fresh "STEPS" in
    intros ? ? STEPS;
    apply (ipa_sound ipa admissible_ipa _ _ _ _ _ STEPS pa);
    [ left; reflexivity | simpl; now auto using Qle_refl ]
  end.
