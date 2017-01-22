Require Import ZArith.
Require Import List.
Require Import pasta.Utils.

Definition id : Type := positive.

(* Conceivably we could also shallowly embed these? *)
Inductive expr :=
(*  | ERandom *)
  | EVar : id -> expr
  | ENum : Z -> expr
  | EAdd : expr -> expr -> expr
  | ESub : expr -> expr -> expr
  | EMul : expr -> expr -> expr.

Definition node : Type := positive.

Definition state := id -> Z.

Definition update (x : id)  (v : Z) (s : state) :=
  fun y => if (Pos.eq_dec x y) then v else (s y).

Inductive action :=
  | ANone
  | AWeaken
  | AGuard : (state -> Prop) -> action 
  | AAssign : id -> option expr -> action
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
| SNone       : forall s, step s ANone s
| SWeaken     : forall s, step s AWeaken s
| SGuard      : forall s (P : state -> Prop),  P s -> step s (AGuard P) s
| SAssignExpr : forall s x e, step s (AAssign x (Some e)) (update x (eval e s) s)
| SAssignRand : forall s x n, step s (AAssign x None) (update x n s)
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

Definition reachable_ind_VC
           (p_i: node) (s_i: state) (es: list edge)
           (P: node -> state -> Prop) :=
  Forall (fun e =>
            match e with (p,a,p') =>
              forall s s'
                     (REACHABLE: steps p_i s_i es p s)
                     (STEP:      step s a s')
                     (HIND:      P p s),
                P p' s'
            end)
         es.

Lemma reachable_ind_VC_spec :
  forall p_i s_i es P, reachable_ind_VC p_i s_i es P ->
    forall p s a p' s',
      In (p,a,p') es ->
      steps p_i s_i es p s ->
      step s a s' ->
      P p s ->
      P p' s'.
Proof.
  intros p_i s_i es P REACH p s a p' s' IN.
  now apply (Forall_In _ _ _ REACH (p,a,p') IN s s').
Qed.

Lemma reachable_ind :
  forall (g : graph) (P : node -> state -> Prop) s_i
         (STEP : reachable_ind_VC (g_start g) s_i (g_edges g) P),
  forall p' s', P (g_start g) s_i -> steps (g_start g) s_i (g_edges g) p' s' -> P p' s'.
Proof.
  intros g P s_i STEP p' s' INIT STEPS.
  refine (reachable_ind' g P s_i _ p' s' INIT STEPS).
  apply reachable_ind_VC_spec; assumption.
Qed.
