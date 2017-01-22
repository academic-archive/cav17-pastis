Require Import pasta.Pasta.

Notation IDpf_push_z := 1%positive.
Notation IDpf_push__tmp := 2%positive.
Notation IDpf_push_n := 3%positive.
Notation IDpf_push_op := 4%positive.
Notation IDpf_push_ppts := 5%positive.
Definition pf_push : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDpf_push_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDpf_push__tmp (Some (EVar IDpf_push_n))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDpf_push__tmp
             (Some (EAdd (EVar IDpf_push__tmp) (ENum (-1))))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDpf_push__tmp) s) <>
             (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDpf_push__tmp) s) =
             (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDpf_push_z (Some (EAdd (ENum (1))
             (EVar IDpf_push_z)))),4%positive)::nil
|}.

Definition pf_push_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpf_push_z) <= 0 /\ -1 * (s IDpf_push_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpf_push_z) <= 0 /\ 1 * (s IDpf_push_z) <= 0)%Z
    | 4%positive => (-1 * (s IDpf_push_z) <= 0)%Z
    | 5%positive => (-1 * (s IDpf_push_z) <= 0)%Z
    | 6%positive => (-1 * (s IDpf_push_z) <= 0)%Z
    | 7%positive => (-1 * (s IDpf_push_z) <= 0 /\ 1 * (s IDpf_push__tmp) <= 0 /\ -1 * (s IDpf_push__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDpf_push__tmp) <= 0 /\ 1 * (s IDpf_push__tmp) <= 0 /\ -1 * (s IDpf_push_z) <= 0)%Z
    | 9%positive => (-1 * (s IDpf_push_z) <= 0)%Z
    | 10%positive => (-1 * (s IDpf_push_z) <= 0)%Z
    | 11%positive => (-1 * (s IDpf_push_z) <= 0)%Z
    | 12%positive => (-1 * (s IDpf_push_z) <= 0)%Z
    | _ => False
  end.

Definition pf_push_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDpf_push_n))%Q
    | 2%positive => ((s IDpf_push_n) + (s IDpf_push_z))%Q
    | 3%positive => ((s IDpf_push__tmp) + (s IDpf_push_z))%Q
    | 4%positive => ((s IDpf_push__tmp) + (s IDpf_push_z))%Q
    | 5%positive => ((1 # 1) + (s IDpf_push__tmp) + (s IDpf_push_z))%Q
    | 6%positive => ((1 # 1) + (s IDpf_push__tmp) + (s IDpf_push_z))%Q
    | 7%positive => ((1 # 1) + (s IDpf_push__tmp) + (s IDpf_push_z))%Q
    | 8%positive => ((s IDpf_push_z))%Q
    | 9%positive => ((1 # 1) + (s IDpf_push__tmp) + (s IDpf_push_z))%Q
    | 10%positive => ((1 # 1) + (s IDpf_push__tmp) + (s IDpf_push_z))%Q
    | 11%positive => ((1 # 1) + (s IDpf_push__tmp) + (s IDpf_push_z))%Q
    | 12%positive => ((1 # 1) + (s IDpf_push__tmp) + (s IDpf_push_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition pf_push_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_one;
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDpf_push__tmp))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDpf_push__tmp)) (0))) (F_max0_ge_0 ((s IDpf_push__tmp)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | _ => []
  end.


Theorem pf_push_ai_correct:
  forall s p' s', steps (g_start pf_push) s (g_edges pf_push) p' s' -> pf_push_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem pf_push_pot_correct:
  forall s p' s',
    steps (g_start pf_push) s (g_edges pf_push) p' s' ->
    (pf_push_pot (g_start pf_push) s >= pf_push_pot p' s')%Q.
Proof.
  check_lp pf_push_ai_correct pf_push_hints.
Qed.

