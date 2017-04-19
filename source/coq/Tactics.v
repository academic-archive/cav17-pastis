Require Import ZArith.
Require Import QArith.
Require Import Psatz.
Require Import pasta.Utils.
Require Import pasta.CFG.
Require Import pasta.RewriteF.

Close Scope Z.
Close Scope Q.

(*

(* This tactic is used to prove the bounds deduced by the 
   Presburger arithmetic abstract interpreter. It basically
   just calls Coq's lia. *)
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
    try lia
  | (* base case *)
    auto
  ].

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

*)
