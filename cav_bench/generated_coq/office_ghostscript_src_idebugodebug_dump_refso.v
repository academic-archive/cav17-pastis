Require Import pasta.Pasta.

Notation IDdebug_dump_refs_z := 1%positive.
Notation IDdebug_dump_refs__tmp := 2%positive.
Notation IDdebug_dump_refs_count := 3%positive.
Notation IDdebug_dump_refs_from := 4%positive.
Notation IDdebug_dump_refs_msg := 5%positive.
Notation IDdebug_dump_refs_size := 6%positive.
Definition debug_dump_refs : graph := {|
  g_start := 1%positive;
  g_end := 15%positive;
  g_edges := (1%positive,(AAssign IDdebug_dump_refs_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDdebug_dump_refs__tmp
             (Some (EVar IDdebug_dump_refs_size))),3%positive)::
             (3%positive,(AAssign IDdebug_dump_refs_count
             (Some (EVar IDdebug_dump_refs__tmp))),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_dump_refs__tmp) s) <>
             (eval (ENum (0)) s))%Z)),7%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_dump_refs__tmp) s) =
             (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,10%positive)::
             (7%positive,AWeaken,8%positive)::(8%positive,ANone,9%positive)::
             (8%positive,ANone,10%positive)::(9%positive,ANone,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDdebug_dump_refs_count
             (Some (EAdd (EVar IDdebug_dump_refs_count) (ENum (-1))))),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_dump_refs_count) s) <>
             (eval (ENum (0)) s))%Z)),16%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDdebug_dump_refs_count) s) =
             (eval (ENum (0)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDdebug_dump_refs_z (Some (EAdd (ENum (1))
             (EVar IDdebug_dump_refs_z)))),11%positive)::nil
|}.

Definition debug_dump_refs_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDdebug_dump_refs_z) <= 0 /\ -1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 3%positive => (-1 * (s IDdebug_dump_refs_z) <= 0 /\ 1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 4%positive => (1 * (s IDdebug_dump_refs_z) <= 0 /\ -1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 5%positive => (-1 * (s IDdebug_dump_refs_z) <= 0 /\ 1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 6%positive => (1 * (s IDdebug_dump_refs_z) <= 0 /\ -1 * (s IDdebug_dump_refs_z) <= 0 /\ 1 * (s IDdebug_dump_refs__tmp) <= 0 /\ -1 * (s IDdebug_dump_refs__tmp) <= 0)%Z
    | 7%positive => (1 * (s IDdebug_dump_refs_z) <= 0 /\ -1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 8%positive => (-1 * (s IDdebug_dump_refs_z) <= 0 /\ 1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 9%positive => (1 * (s IDdebug_dump_refs_z) <= 0 /\ -1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 10%positive => (-1 * (s IDdebug_dump_refs_z) <= 0 /\ 1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 11%positive => (-1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 12%positive => (-1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 13%positive => (-1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 14%positive => (-1 * (s IDdebug_dump_refs_z) <= 0 /\ 1 * (s IDdebug_dump_refs_count) <= 0 /\ -1 * (s IDdebug_dump_refs_count) <= 0)%Z
    | 15%positive => (-1 * (s IDdebug_dump_refs_count) <= 0 /\ 1 * (s IDdebug_dump_refs_count) <= 0 /\ -1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 16%positive => (-1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 17%positive => (-1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 18%positive => (-1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | 19%positive => (-1 * (s IDdebug_dump_refs_z) <= 0)%Z
    | _ => False
  end.

Definition debug_dump_refs_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDdebug_dump_refs_size))%Q
    | 2%positive => ((s IDdebug_dump_refs_size) + (s IDdebug_dump_refs_z))%Q
    | 3%positive => ((s IDdebug_dump_refs__tmp) + (s IDdebug_dump_refs_z))%Q
    | 4%positive => ((s IDdebug_dump_refs_count) + (s IDdebug_dump_refs_z))%Q
    | 5%positive => ((s IDdebug_dump_refs_count) + (s IDdebug_dump_refs_z))%Q
    | 6%positive => ((s IDdebug_dump_refs_count) + (s IDdebug_dump_refs_z))%Q
    | 7%positive => ((s IDdebug_dump_refs_count) + (s IDdebug_dump_refs_z))%Q
    | 8%positive => ((s IDdebug_dump_refs_count) + (s IDdebug_dump_refs_z))%Q
    | 9%positive => ((s IDdebug_dump_refs_count) + (s IDdebug_dump_refs_z))%Q
    | 10%positive => ((s IDdebug_dump_refs_count) + (s IDdebug_dump_refs_z))%Q
    | 11%positive => ((s IDdebug_dump_refs_count) + (s IDdebug_dump_refs_z))%Q
    | 12%positive => ((1 # 1) + (s IDdebug_dump_refs_count)
                      + (s IDdebug_dump_refs_z))%Q
    | 13%positive => ((1 # 1) + (s IDdebug_dump_refs_count)
                      + (s IDdebug_dump_refs_z))%Q
    | 14%positive => ((1 # 1) + (s IDdebug_dump_refs_count)
                      + (s IDdebug_dump_refs_z))%Q
    | 15%positive => ((s IDdebug_dump_refs_z))%Q
    | 16%positive => ((1 # 1) + (s IDdebug_dump_refs_count)
                      + (s IDdebug_dump_refs_z))%Q
    | 17%positive => ((1 # 1) + (s IDdebug_dump_refs_count)
                      + (s IDdebug_dump_refs_z))%Q
    | 18%positive => ((1 # 1) + (s IDdebug_dump_refs_count)
                      + (s IDdebug_dump_refs_z))%Q
    | 19%positive => ((1 # 1) + (s IDdebug_dump_refs_count)
                      + (s IDdebug_dump_refs_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition debug_dump_refs_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDdebug_dump_refs_count))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDdebug_dump_refs_count)) (0))) (F_max0_ge_0 ((s IDdebug_dump_refs_count)))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | _ => []
  end.


Theorem debug_dump_refs_ai_correct:
  forall s p' s', steps (g_start debug_dump_refs) s (g_edges debug_dump_refs) p' s' -> debug_dump_refs_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem debug_dump_refs_pot_correct:
  forall s p' s',
    steps (g_start debug_dump_refs) s (g_edges debug_dump_refs) p' s' ->
    (debug_dump_refs_pot (g_start debug_dump_refs) s >= debug_dump_refs_pot p' s')%Q.
Proof.
  check_lp debug_dump_refs_ai_correct debug_dump_refs_hints.
Qed.

