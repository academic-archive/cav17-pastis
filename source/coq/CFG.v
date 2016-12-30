Require Import ZArith.
Require Import List.

Definition id : Type := nat.

(* Conceivably we could also shallowly embed these? *)
Inductive expr :=
(*  | ERandom *)
  | EVar : id -> expr
  | ENum : Z -> expr
  | EAdd : expr -> expr -> expr
  | ESub : expr -> expr -> expr
  | EMul : expr -> expr -> expr.

Definition node : Type := nat.

Definition state := id -> Z.

Definition update (x : id)  (v : Z) (s : state) :=
  fun y => if (eq_nat_dec x y) then v else (s y).

Inductive action :=
  | ANone
  | AWeaken
  | AGuard : (state -> Prop) -> action 
  | AAssign : id -> expr -> action
  | ACall  : list id -> id -> list expr -> action.

Definition edge := (node * action * node)%type.

Inductive graph :=
  { g_start: node
  ; g_end: node
  ; g_edges: list edge
  }.

(* type func = (Focus.focus, graph) func_ *)


Fixpoint eval (e : expr) (s : state) :=
  (match e with
    | EVar x => s x
    | ENum n => n
    | EAdd e1 e2 => eval e1 s + eval e2 s
    | ESub e1 e2 => eval e1 s - eval e2 s
    | EMul e1 e2 => eval e1 s * eval e2 s
  end)%Z.

Inductive step : state -> action -> state -> Prop :=
| SNone   : forall s, step s ANone s
| SWeaken : forall s, step s AWeaken s
| SGuard  : forall s (P : state -> Prop),  P s -> step s (AGuard P) s
| SAssign : forall s x e, step s (AAssign x e) (update x (eval e s) s)
(* TODO: ACall *)
.

Inductive steps (p : node) (s : state) (es : list edge) : node -> state -> Prop :=
| SStart : steps p s es p s
| SStep p1 s1 a p2 s2 :
    steps p s es p1 s1 -> In (p1,a,p2) es -> step s1 a s2 ->
    steps p s es p2 s2.

Lemma reachable_ind' :
  forall (g: graph) (P: node -> state -> Prop) s_i
         (Hstep : forall p s a p' s'
                         (PAP'EDGE:  In (p,a,p') (g_edges g))
                         (REACHABLE: steps (g_start g) s_i (g_edges g) p s)
                         (STEP:      step s a s')
                         (HIND:      P p s),
                    P p' s'),
  forall p s, P (g_start g) s_i -> steps (g_start g) s_i (g_edges g) p s -> P p s.
Proof.
  intros g P s_i Hstep p s Hinit Hsteps.
  induction Hsteps.
  + auto.
  + eapply Hstep; eauto.
Qed.

Fixpoint reachable_ind_VC
         (p_i: node) (s_i: state) (es: list edge)
         (P : node -> state -> Prop) (ns : list edge) : Prop :=
  match ns with
  | nil => True
  | (p,a,p') :: ns =>
    (forall s s'
            (REACHABLE: steps p_i s_i es p s)
            (STEP:      step s a s')
            (HIND:      P p s),
       P p' s') /\
    reachable_ind_VC p_i s_i es P ns
  end.

Lemma reachable_ind_VC_spec :
  forall p_i s_i es P ns, reachable_ind_VC p_i s_i es P ns ->
    forall p s a p' s',
      In (p,a,p') ns ->
      steps p_i s_i es p s ->
      step s a s' ->
      P p s ->
      P p' s'.
Proof.
 induction ns.
 + simpl; intros; tauto.
 + destruct a as [[p a] p'].
   simpl.
   intuition.
    - inversion H6.
      subst.
      eauto.
   - eauto.
Qed.

Lemma reachable_ind :
  forall (g : graph) (P : node -> state -> Prop) s_i
         (STEP : reachable_ind_VC (g_start g) s_i (g_edges g) P (g_edges g)),
  forall p' s', P (g_start g) s_i -> steps (g_start g) s_i (g_edges g) p' s' -> P p' s'.
Proof.
  intros g P s_i STEP p' s' INIT STEPS.
  refine (reachable_ind' g P s_i _ p' s' INIT STEPS).
  apply reachable_ind_VC_spec; assumption.
Qed.

Opaque Zplus.
Opaque Zminus.
Opaque Zmult.

(* This tactic is used to prove the bounds deduced by the 
   Presburger arithmetic abstract interpreter. It basically
   just calls Coq's omega. *)
Ltac prove_ai_bounds_correct :=
  intros until 0;
  apply reachable_ind;
  [ (* step case *)
    simpl;
    repeat apply conj;
    intros;
    try (match goal with [ H : step _ _ _ |- _] => inversion H end; subst);
    simpl;
    try unfold update;
    simpl;
    auto;
    try omega
  | (* base case *) simpl; auto ].
