Require Import pasta.Pasta.

Notation IDxorbytes_z := 1%positive.
Notation IDxorbytes__tmp := 2%positive.
Notation IDxorbytes_dest := 3%positive.
Notation IDxorbytes_len := 4%positive.
Notation IDxorbytes_src := 5%positive.
Definition xorbytes : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDxorbytes_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDxorbytes__tmp
             (Some (EVar IDxorbytes_len))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDxorbytes__tmp
             (Some (EAdd (EVar IDxorbytes__tmp) (ENum (-1))))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDxorbytes__tmp)
             s) <> (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDxorbytes__tmp) s) =
             (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDxorbytes_z (Some (EAdd (ENum (1))
             (EVar IDxorbytes_z)))),4%positive)::nil
|}.

Definition xorbytes_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDxorbytes_z) <= 0 /\ -1 * (s IDxorbytes_z) <= 0)%Z
    | 3%positive => (-1 * (s IDxorbytes_z) <= 0 /\ 1 * (s IDxorbytes_z) <= 0)%Z
    | 4%positive => (-1 * (s IDxorbytes_z) <= 0)%Z
    | 5%positive => (-1 * (s IDxorbytes_z) <= 0)%Z
    | 6%positive => (-1 * (s IDxorbytes_z) <= 0)%Z
    | 7%positive => (-1 * (s IDxorbytes_z) <= 0 /\ 1 * (s IDxorbytes__tmp) <= 0 /\ -1 * (s IDxorbytes__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDxorbytes__tmp) <= 0 /\ 1 * (s IDxorbytes__tmp) <= 0 /\ -1 * (s IDxorbytes_z) <= 0)%Z
    | 9%positive => (-1 * (s IDxorbytes_z) <= 0)%Z
    | 10%positive => (-1 * (s IDxorbytes_z) <= 0)%Z
    | 11%positive => (-1 * (s IDxorbytes_z) <= 0)%Z
    | 12%positive => (-1 * (s IDxorbytes_z) <= 0)%Z
    | _ => False
  end.

Definition xorbytes_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDxorbytes_len))%Q
    | 2%positive => ((s IDxorbytes_len) + (s IDxorbytes_z))%Q
    | 3%positive => ((s IDxorbytes__tmp) + (s IDxorbytes_z))%Q
    | 4%positive => ((s IDxorbytes__tmp) + (s IDxorbytes_z))%Q
    | 5%positive => ((1 # 1) + (s IDxorbytes__tmp) + (s IDxorbytes_z))%Q
    | 6%positive => ((1 # 1) + (s IDxorbytes__tmp) + (s IDxorbytes_z))%Q
    | 7%positive => ((1 # 1) + (s IDxorbytes__tmp) + (s IDxorbytes_z))%Q
    | 8%positive => ((s IDxorbytes_z))%Q
    | 9%positive => ((1 # 1) + (s IDxorbytes__tmp) + (s IDxorbytes_z))%Q
    | 10%positive => ((1 # 1) + (s IDxorbytes__tmp) + (s IDxorbytes_z))%Q
    | 11%positive => ((1 # 1) + (s IDxorbytes__tmp) + (s IDxorbytes_z))%Q
    | 12%positive => ((1 # 1) + (s IDxorbytes__tmp) + (s IDxorbytes_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition xorbytes_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_one;
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDxorbytes__tmp))) (F_check_ge (0) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDxorbytes__tmp)) (0))) (F_max0_ge_0 ((s IDxorbytes__tmp)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | _ => []
  end.


Theorem xorbytes_ai_correct:
  forall s p' s', steps (g_start xorbytes) s (g_edges xorbytes) p' s' -> xorbytes_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem xorbytes_pot_correct:
  forall s p' s',
    steps (g_start xorbytes) s (g_edges xorbytes) p' s' ->
    (xorbytes_pot (g_start xorbytes) s >= xorbytes_pot p' s')%Q.
Proof.
  check_lp xorbytes_ai_correct xorbytes_hints.
Qed.

