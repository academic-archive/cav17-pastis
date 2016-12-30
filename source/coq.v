Add LoadPath "coq".
Require Import List.
Require Import Arith.
Require Import ZArith.
Require Import QArith.
Require Import CFG.
Require Import Rewr.
Close Scope Z.
Close Scope Q.
Opaque Zminus.
Opaque Zplus.
Opaque Zmult.
Import ListNotations.

Coercion inject_Z: Z >-> Q.

Notation IDfunc0_t := 0.
Notation IDfunc0_z := 1.
Definition func0 : graph := {|
  g_start := 0;
  g_end := 11;
  g_edges := (0,(AAssign IDfunc0_t (ENum 1)),1)::(1,AWeaken,2)::
             (2,(AGuard (fun s => (~ ((~ ((eval (EVar IDfunc0_t) s) >=
             (eval (ENum 10) s))%Z) /\ ((eval (EVar IDfunc0_t) s) >=
             (eval (ENum 0) s))%Z)))),8)::
             (2,(AGuard (fun s => ((~ ((eval (EVar IDfunc0_t) s) >=
             (eval (ENum 10) s))%Z) /\ ((eval (EVar IDfunc0_t) s) >=
             (eval (ENum 0) s))%Z))),3)::(3,AWeaken,4)::
             (4,(AAssign IDfunc0_t (EAdd (EVar IDfunc0_t) (ENum 2))),5)::
             (5,(AAssign IDfunc0_z (EAdd (EVar IDfunc0_z) (ENum 1))),6)::
             (6,ANone,7)::(7,AWeaken,2)::(8,AWeaken,9)::(9,ANone,10)::
             (10,AWeaken,11)::nil
|}.

Definition func0_bounds (p : node) (s : state) := 
  match p with
    | 0 => (True)%Z
    | 1 => (1 * (s IDfunc0_t) + -1 <= 0 /\ -1 * (s IDfunc0_t) + 1 <= 0)%Z
    | 2 => (-1 * (s IDfunc0_t) + 1 <= 0 /\ 1 * (s IDfunc0_t) + -11 <= 0)%Z
    | 3 => (-1 * (s IDfunc0_t) + 1 <= 0 /\ 1 * (s IDfunc0_t) + -9 <= 0)%Z
    | 4 => (1 * (s IDfunc0_t) + -9 <= 0 /\ -1 * (s IDfunc0_t) + 1 <= 0)%Z
    | 5 => (1 * (s IDfunc0_t) + -11 <= 0 /\ -1 * (s IDfunc0_t) + 3 <= 0)%Z
    | 6 => (-1 * (s IDfunc0_t) + 3 <= 0 /\ 1 * (s IDfunc0_t) + -11 <= 0)%Z
    | 7 => (1 * (s IDfunc0_t) + -11 <= 0 /\ -1 * (s IDfunc0_t) + 3 <= 0)%Z
    | 8 => (1 * (s IDfunc0_t) + -11 <= 0 /\ -1 * (s IDfunc0_t) + 10 <= 0)%Z
    | 9 => (-1 * (s IDfunc0_t) + 10 <= 0 /\ 1 * (s IDfunc0_t) + -11 <= 0)%Z
    | 10 => (1 * (s IDfunc0_t) + -11 <= 0 /\ -1 * (s IDfunc0_t) + 10 <= 0)%Z
    | 11 => (-1 * (s IDfunc0_t) + 10 <= 0 /\ 1 * (s IDfunc0_t) + -11 <= 0)%Z
    | _ => False
  end.
Theorem func0_bounds_corrects:
  forall s p' s', steps (g_start func0) s (g_edges func0) p' s' -> func0_bounds p' s'.
Proof. prove_ai_bounds_correct. Qed.

Definition func0_annots (p : node) (s : state) := 
  match p with
    | 0 => ((5 # 1) + inject_Z (s IDfunc0_z))%Q
    | 1 => (inject_Z (s IDfunc0_z) + (1 # 2) * max0(11 - (s IDfunc0_t)))%Q
    | 2 => (inject_Z (s IDfunc0_z) + (1 # 2) * max0(11 - (s IDfunc0_t)))%Q
    | 3 => (inject_Z (s IDfunc0_z) + (1 # 2) * max0(11 - (s IDfunc0_t)))%Q
    | 4 => ((1 # 1) + inject_Z (s IDfunc0_z)
            + (1 # 2) * max0(9 - (s IDfunc0_t)))%Q
    | 5 => ((1 # 1) + inject_Z (s IDfunc0_z)
            + (1 # 2) * max0(11 - (s IDfunc0_t)))%Q
    | 6 => (inject_Z (s IDfunc0_z) + (1 # 2) * max0(11 - (s IDfunc0_t)))%Q
    | 7 => (inject_Z (s IDfunc0_z) + (1 # 2) * max0(11 - (s IDfunc0_t)))%Q
    | 8 => (inject_Z (s IDfunc0_z) + (1 # 2) * max0(11 - (s IDfunc0_t)))%Q
    | 9 => (inject_Z (s IDfunc0_z) + (1 # 2) * max0(11 - (s IDfunc0_t)))%Q
    | 10 => (inject_Z (s IDfunc0_z) + (1 # 2) * max0(11 - (s IDfunc0_t)))%Q
    | 11 => (inject_Z (s IDfunc0_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition func0_hints (p : node) (s : state) := 
  match p with
    | 0 => []
    | 1 => []
    | 2 => []
    | 3 => [(*-0.5 0*) F_max0_pre_decrement (11 - (s IDfunc0_t)) (2)]
    | 4 => []
    | 5 => []
    | 6 => []
    | 7 => []
    | 8 => []
    | 9 => []
    | 10 => [(*-0.5 0*) F_max0_monotonic (F_check_ge (11 - (s IDfunc0_t)) (9 - (s IDfunc0_t)));
             (*-0.5 0*) F_max0_ge_0 (9 - (s IDfunc0_t))]
    | 11 => []
    | _ => []
  end.

Require Import Psatz.

Definition rewrite_conclusion (a: rewrite_ast) :=
  (ineq_ge0 (snd (interpret a)) >= 0)%Z.
Definition rewrite_assumptions (a: rewrite_ast) :=
  Forall ineq_prop (fst (interpret a)).

Lemma use_hints:
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


Theorem foo:
  forall s p' s',
    steps (g_start func0) s (g_edges func0) p' s' ->
    (func0_annots (g_start func0) s >= func0_annots p' s')%Q.
Proof.
  intros until 0.
  apply reachable_ind with
  (P := fun p' s' => (func0_annots (g_start func0) s >= func0_annots p' s')%Q);
    auto with qarith.

  repeat apply conj;
    
  try match goal with
  | [ |- context[ AAssign _ _ ] ] =>
    inversion_clear 2;
    unfold update;
    apply Qle_trans, Z.eq_le_incl;
    simpl; simplify_max0;
    ring (* Hopefully that does it... *)

  | [ |- context[ AWeaken ] ] =>
    intros until 1;
    refine (_ (func0_bounds_corrects _ _ _ REACHABLE));
    intro; inversion 1; subst;
    clear REACHABLE; apply Qle_trans;
    match goal with
    | [ |- (_ <= _ ?n ?s)%Q ] =>
      refine (_ (use_hints (func0_hints n s) _));
      unfold Qle, rewrite_conclusion, rewrite_assumptions;
      simpl in *; simplify_max0; try lia
    end
                     
  | [ |- context[ AGuard _ ] ] =>
    inversion_clear 2;
    now apply Qle_trans, Qle_refl

  | [ |- context[ ANone ] ] =>
    inversion_clear 2;
    now apply Qle_trans, Qle_refl

  | [ |- True ] => constructor

  end.

Qed.
