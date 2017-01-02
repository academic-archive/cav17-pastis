Require Import ZArith.
Require Import QArith.
Require Import Psatz.
Require Import Utils.
Require Import CFG.
Require Import RewriteF.

Close Scope Z.
Close Scope Q.

(* This tactic is used to prove the bounds deduced by the 
   Presburger arithmetic abstract interpreter. It basically
   just calls Coq's omega. *)
Ltac check_ai :=

  intros until 0;
  apply reachable_ind;
  simpl;

  [ (* step case *)
    repeat apply conj;
    intros;
    try (match goal with [ H : step _ _ _ |- _] => inversion H end; subst);
    simpl;
    try unfold update;
    simpl in *;
    auto;
    try omega
  | (* base case *)
    auto
  ].

(* This tactic simplifies goals that contain max0 terms.
   It first makes sure that all pairs of max0 applications
   with equal arguments (using ring) have syntactically
   equal arguments.
   Then it simplifies max0 applications with a constant
   argument. *)
Ltac simplify_max0 :=

  (* Uniformize contents of max0 terms. *)
  repeat match goal with
  | [ |- context[ max0 ?a ] ] =>
    match goal with
    | [ |- context[ max0 ?b ] ] =>
      let H := fresh "H" in
      (assert (H: a = b) by reflexivity; fail 1) ||
      (assert (H: a = b) by ring; rewrite H; clear H)
    end
  end;

  (* Reduce max0 when the argument is a constant. *)
  repeat match goal with
  | [ |- context[ max0 ?x ] ] => set (max0 x)
  end;
  repeat match goal with
  | [ m := max0 ?x |- _ ] => revert m; ring_simplify x
  end;
  repeat match goal with
  | [ |- context[ max0 (Zpos ?x) ] ] =>
    change (max0 (Zpos x)) with (Zpos x)
  | [ |- context[ max0 0 ] ] =>
    change (max0 0) with 0
  | [ |- context[ max0 (Zneg ?x) ] ] =>
    change (max0 (Zneg x)) with 0
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
  induction hints.
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

Ltac check_lp ai_theorem rewrites :=
  
  intros until 0;
  match goal with
  | [ |- _ -> (?annots (g_start ?f) ?s >= ?annots ?p' ?s')%Q ] =>
    apply reachable_ind with
    (P := fun p' s' => (annots (g_start f) s >= annots p' s')%Q);
    auto with qarith;
      
    repeat apply conj;
      
    try match goal with
    | [ |- context[ AAssign _ _ ] ] =>
      inversion_clear 2;
      unfold update;
      apply Qle_trans, Z.eq_le_incl;
      simpl; simplify_max0;
      ring (* Hopefully that does it... *)
            
    | [ |- context[ AWeaken ] ] =>
      intros ? ? REACHABLE;
      refine (_ (ai_theorem _ _ _ REACHABLE));
      intro; inversion 1; subst;
      clear REACHABLE; apply Qle_trans;
      match goal with
      | [ |- (_ <= _ ?n ?s)%Q ] =>
        refine (_ (use_rewrites (rewrites n s) _));
        unfold Qle, rewrite_conclusion, rewrite_assumptions;
        simpl in *; simplify_max0; lia
      end
                
    | [ |- context[ AGuard _ ] ] =>
      inversion_clear 2; now apply Qle_trans, Qle_refl
    | [ |- context[ ANone ] ] =>
      inversion_clear 2; now apply Qle_trans, Qle_refl
                                     
    | [ |- True ] => constructor
                       
    end
  end.

