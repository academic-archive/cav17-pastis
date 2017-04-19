Require Import ZArith.
Require Import List.
Require Import pasta.Utils.

Definition id: Type := positive.
Definition node: Type := positive.
Definition state := id -> Z.

Definition update (x : id)  (v : Z) (s : state) :=
  fun y => if (Pos.eq_dec x y) then v else (s y).

Inductive expr :=
| EVar : id -> expr
| ENum : Z -> expr
| EAdd : expr -> expr -> expr
| ESub : expr -> expr -> expr
| EMul : expr -> expr -> expr
.

Inductive action: Type :=
| ANone
| AWeaken
| AGuard (guard: state -> Prop)
| AAssign (var: id) (oexpr: option expr)
.

Fixpoint eval (e : expr) (s : state) :=
  (match e with
    | EVar x => s x
    | ENum n => n
    | EAdd e1 e2 => eval e1 s + eval e2 s
    | ESub e1 e2 => eval e1 s - eval e2 s
    | EMul e1 e2 => eval e1 s * eval e2 s
  end)%Z.

Inductive step: state -> action -> state -> Prop :=
| SNone       : forall s, step s ANone s
| SWeaken     : forall s, step s AWeaken s
| SGuard      : forall s (P : state -> Prop),  P s -> step s (AGuard P) s
| SAssignExpr : forall s x e, step s (AAssign x (Some e)) (update x (eval e s) s)
| SAssignRand : forall s x n, step s (AAssign x None) (update x n s)
.

Inductive edge (proc: Type) :=
| EA (n1: node) (a: action) (n2: node)
| EC (n1: node) (P: proc) (n2: node)
.
Global Arguments EA {proc} n1%positive a n2%positive.
Global Arguments EC {proc} n1%positive P n2%positive.

Class Program (proc: Type) :=
  { proc_edges: proc -> list (edge proc)
  ; proc_start: proc -> node
  ; proc_end:   proc -> node
  ; var_global: id -> bool
  }.

Section WithProgram.
  Context `{prog: Program}.

  Definition exit (s1 s2: state) :=
    fun x => if var_global x then s2 x else s1 x.

  Inductive steps: proc -> node -> state -> node -> state -> Prop :=

  | SStart P p s:
      steps P p s p s

  | SStep P p s p1 s1 a p2 s2:
      steps P p s p1 s1 ->
      In (EA p1 a p2) (proc_edges P) ->
      step s1 a s2 ->
      steps P p s p2 s2

  | SCall P p s p1 s1 Q p2 s2:
      steps P p s p1 s1 ->
      In (EC p1 Q p2) (proc_edges P) ->
      steps Q (proc_start Q) s1 (proc_end Q) s2 ->
      steps P p s p2 (exit s1 s2)
  .

  (* Procedure annotation. *)
  Inductive PA :=
    { pa_aux: Type
    ; pa_spec: node -> pa_aux -> state -> Prop
    ; pa_call: node -> nat
    }.

  (* Inter-procedural annotation. *)
  Definition IPA :=
    proc -> list PA.

  Definition PA_VC (ipa: IPA) (P: proc) (pa: PA) :=
    forall (z: pa_aux pa),
    Forall
      (fun e => match e with
           
         | EA n1 a n2 =>
           forall s1 s2,
             pa_spec pa n1 z s1 ->
             step s1 a s2 ->
             pa_spec pa n2 z s2
                     
         | EC n1 Q n2 =>
           match nth_error (ipa Q) (pa_call pa n1) with
           | Some qpa =>
             exists z',
             forall s1,
               pa_spec pa n1 z s1 ->
               pa_spec qpa (proc_start Q) z' s1 /\
               forall s2,
                 pa_spec qpa (proc_end Q) z' s2 ->
                 pa_spec pa n2 z (exit s1 s2)
           | None => False
           end

         end)
      (proc_edges P).

  Definition IPA_VC (ipa: IPA) :=
    forall (P: proc), Forall (PA_VC ipa P) (ipa P).

  Transparent PA_VC IPA_VC.

  Theorem ipa_sound (ipa: IPA) (VC: IPA_VC ipa):
    forall (P: proc) p1 s1 p2 s2 (STEPS: steps P p1 s1 p2 s2),
    forall (pa: PA) z (InIPA: In pa (ipa P)), pa_spec pa p1 z s1 -> pa_spec pa p2 z s2.
  Proof.
    intros ? ? ? ? ? STEPS.
    induction STEPS; intros ? ? ? INIT.
    - assumption.
    - assert (paVC: PA_VC ipa P pa).
      { eapply Forall_In; eauto using VC. }
      eapply (Forall_In _ _ _ (paVC z) _ H).	
      auto using IHSTEPS.
      assumption.
    - assert (paVC: PA_VC ipa P pa).
      { eapply Forall_In; eauto using VC. }
      generalize (Forall_In _ _ _ (paVC z) _ H).
      case_eq (nth_error (ipa Q) (pa_call pa p1)).
      2: intros _ [].
      intros qpa qpaInIPA.
      apply nth_error_In in qpaInIPA.
      intros (z' & CALL).
      apply CALL.
      + auto using IHSTEPS1.
      + apply IHSTEPS2; auto.
	apply CALL.
	auto using IHSTEPS1.
  Qed.

End WithProgram.
