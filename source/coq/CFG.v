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

(*
type free_expr =
  | FBase of expr
  | FApply of id * free_expr list * position

type ('a, 'b) func_ =
  { fun_name: id
  ; fun_vars: id list
  ; fun_args: id list
  ; fun_rets: id list
  ; fun_focus: 'a list
  ; fun_body: 'b
  ; fun_start_p: position
  ; fun_end_p: position
  }
*)

(*
Inductive cmp  := Le | Lt | Ge | Gt | Eq | Ne.

Inductive logic :=
  | LTrue
  | LFalse
  | LRandom
  | LCmp : expr -> cmp -> expr -> logic
  | LAnd : logic -> logic -> logic
  | LOr  : logic -> logic -> logic
  | LNot : logic -> logic.
*)

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

Inductive graph :=
  { g_start: node
  ; g_end: node
  ; g_edges: (list (node * action * node))
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

Inductive steps (p : node) (s : state) (g : graph) : node -> state -> Prop :=
| SStart : steps p s g p s
| SStep : forall p1 s1 a p2 s2 ,
            steps p s g p1 s1 -> In (p1,a,p2) (g_edges g) -> step s1 a s2 -> steps p s g p2 s2.

Lemma reachable_ind' : forall (g : graph)
                               (P : node -> state -> Prop)
                               (Hinit : forall s, P (g_start g) s)
                               (Hstep : forall p s a p' s',
                                         In (p,a,p') (g_edges g) -> step s a s' -> P p s -> P p' s'),
                        forall s p' s', steps (g_start g) s g p' s' -> P p' s'.
Proof.
  intros g P Hinit Hstep s p' s' Hsteps.
  induction Hsteps.
  + auto.
  + eapply Hstep; eauto.
Qed.

Fixpoint reachable_ind_VC  (P : node -> state -> Prop) (ns : list (node * action * node)) : Prop :=
  match ns with
    | nil => True
    | (p,a,p') :: ns => (forall s s', step s a s' -> P p s -> P p' s') /\ reachable_ind_VC P ns
  end.

Lemma reachable_ind_VC_spec : forall P ns,
                                reachable_ind_VC P ns
                                -> forall p s a p' s', In (p,a,p') ns -> step s a s' -> P p s -> P p' s'.
Proof.
 induction ns.
 + simpl; intros; tauto.
 + destruct a as [[p a] p'].
   simpl.
   intuition.
    - inversion H5.
      subst.
      eauto.
   - eauto.
Qed.

Lemma reachable_ind : forall (g : graph)
                             (P : node -> state -> Prop)
                             (Hinit : forall s, P (g_start g) s)
                             (Hstep : reachable_ind_VC P (g_edges g)),
                        forall s p' s', steps (g_start g) s g p' s' -> P p' s'.
Proof.
  intros.
  refine (reachable_ind' g P Hinit _ s p' s' H).
  apply reachable_ind_VC_spec; assumption.
Qed.


Opaque Zplus.
Opaque Zmult.

(* This tactic is used to prove the bounds deduced by the 
   Presburger arithmetic abstract interpreter. It basically
   just calls Coq's omega. *)
Ltac prove_ai_bounds_correct :=
  apply reachable_ind;
  [ (* base case *) intros s; simpl; auto
  | (* step case *)
    simpl;
    repeat apply conj;
    intros;
    try (match goal with [ H : step _ _ _ |- _] => inversion H end; subst);
    simpl;
    try unfold update;
    simpl;
    auto;
    try omega].
