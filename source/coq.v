Add LoadPath "coq".
Require Import Pasta.

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
Proof. check_ai. Qed.


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


Theorem foo:
  forall s p' s',
    steps (g_start func0) s (g_edges func0) p' s' ->
    (func0_annots (g_start func0) s >= func0_annots p' s')%Q.
Proof. check_lp func0_bounds_corrects func0_hints. Qed.
