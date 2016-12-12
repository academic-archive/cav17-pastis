Require Import List.
Require Import ZArith.
Require Import Omega.

Import ListNotations.
Open Scope Z.


Inductive rf_ast :=
| rf_check (x: Z) (y: Z)
| rf_max0_ge_0 (x: Z)
| rf_max0_ge_arg (x: Z)
| rf_max0_le_arg (x: Z)
| rf_max0_monotonic (p: rf_ast)
| rf_max0_sublinear (p: rf_ast)
| rf_max0_pre_decrement (x: Z) (y: Z)
| rf_max0_pre_increment (x: Z) (y: Z)
.

Inductive rf_ineq :=
| rfi_ge (a: Z) (b: Z)
| rfi_ge0 (a: Z)
.

Definition rf_ineq_proj_ge (i: rf_ineq) :=
  match i with
    | rfi_ge a b => (a, b)
    | rfi_ge0 a => (a, 0)
  end.
Definition rf_ineq_proj_ge0 (i: rf_ineq) :=
  match i with
    | rfi_ge a b => a - b
    | rfi_ge0 a => a
  end.

Definition rf_int :=
  (* First interpretation of the syntax defined
   * in rf_ast. *)
  ( list rf_ineq (* assumptions *)
    * rf_ineq    (* conclusion *)
  ) % type.

Definition max0 x := Z.max 0 x.

Fixpoint rf_interpret (p: rf_ast): rf_int :=
  match p with
    | rf_check x y => ([rfi_ge x y], rfi_ge x y)
    | rf_max0_ge_0 x => ([], rfi_ge0 (max0 x))
    | rf_max0_ge_arg x => ([], rfi_ge (max0 x) x)
    | rf_max0_le_arg x => ([rfi_ge0 x], rfi_ge x (max0 x))
    | rf_max0_monotonic q =>
      let (a, c) := rf_interpret q in
      let (x, y) := rf_ineq_proj_ge c in
      (a, rfi_ge (max0 x) (max0 y))
    | rf_max0_sublinear q =>
      let (a, c) := rf_interpret q in
      let (x, y) := rf_ineq_proj_ge c in
      (a, rfi_ge (max0 y + x - y) (max0 x))
    | rf_max0_pre_decrement x y =>
      ([rfi_ge x y; rfi_ge0 y], rfi_ge (max0 x) (y + max0 (x - y)))
    | rf_max0_pre_increment x y =>
      ([rfi_ge0 y], rfi_ge (y + max0 x) (max0 (x + y)))
  end.

Definition rf_ineq_prop (i: rf_ineq) :=
  match i with
    | rfi_ge a b => a >= b
    | rfi_ge0 a => a >= 0
  end.

(* Simple max0 lemmas ---------------------------------------------- *)
Lemma lem_max0_ge_0: forall x, max0 x >= 0.
Proof.
  intros. apply Z.le_ge, Z.le_max_l.
Qed.

Lemma lem_max0_ge_arg: forall x, max0 x >= x.
Proof.
  intros. apply Z.le_ge, Z.le_max_r.
Qed.

Lemma lem_max0_le_arg: forall x, x >= 0 -> x >= max0 x.
Proof.
  intros. apply Z.le_ge, Z.max_lub; auto with zarith.
Qed.

Lemma lem_max0_monotonic: forall x y, x >= y -> max0 x >= max0 y.
Proof.
  intros. apply Z.le_ge, Z.max_le_compat_l. auto with zarith.
Qed.

Lemma lem_max0_sublinear: forall x y, x >= y -> max0 y + x - y >= max0 x.
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
(* ----------------------------------------------------------------- *)

Fixpoint list_forall {A: Type} (P: A -> Prop) (l: list A): Prop :=
  match l with
    | x :: l' => P x /\ list_forall P l'
    | [] => True
  end.

Lemma rf_ineq_proj_ge_semantics:
  forall p x y (PROJ: rf_ineq_proj_ge p = (x, y))
         (PROP: rf_ineq_prop p),
    x >= y.
Proof.
  destruct p; intros; simpl in *; congruence.
Qed.

Theorem rf_ast_semantics:
  forall (p: rf_ast)
         a c (INT: rf_interpret p = (a, c))
         (HYPS: list_forall rf_ineq_prop a),
    rf_ineq_prop c.
Proof.
  induction p; intros;
  try (injection INT; intros ASSUMP CONCL; subst);
  simpl in *.
  + apply (proj1 HYPS).
  + apply lem_max0_ge_0.
  + apply lem_max0_ge_arg.
  + apply lem_max0_le_arg; tauto.
  + revert INT.
    case_eq (rf_interpret p). intros a' c' REC.
    case_eq (rf_ineq_proj_ge c'). intros x y PROJ.
    injection 1; intros; subst.
    apply lem_max0_monotonic.
    eapply rf_ineq_proj_ge_semantics; eauto.
  + revert INT.
    case_eq (rf_interpret p). intros a' c' REC.
    case_eq (rf_ineq_proj_ge c'). intros x y PROJ.
    injection 1; intros; subst.
    apply lem_max0_sublinear.
    eapply rf_ineq_proj_ge_semantics; eauto.    
  + apply lem_max0_pre_decrement; tauto.
  + apply lem_max0_pre_increment; tauto.
Qed.
