Require Import pasta.Pasta.

Notation IDialloc_init_z := 1%positive.
Notation IDialloc_init__tmp := 2%positive.
Notation IDialloc_init__tmp1 := 3%positive.
Notation IDialloc_init_i := 4%positive.
Notation IDialloc_init_chunk_size := 5%positive.
Notation IDialloc_init_level2 := 6%positive.
Notation IDialloc_init_mem := 7%positive.
Definition ialloc_init : graph := {|
  g_start := 1%positive;
  g_end := 17%positive;
  g_edges := (1%positive,(AAssign IDialloc_init_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDialloc_init_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDialloc_init__tmp1
             (Some (EVar IDialloc_init_chunk_size))),5%positive)::
             (5%positive,(AAssign IDialloc_init__tmp
             (Some (EVar IDialloc_init_level2))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDialloc_init__tmp)
             s) <> (eval (ENum (0)) s))%Z)),10%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDialloc_init__tmp)
             s) = (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,12%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDialloc_init_i (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDialloc_init_i)
             s) < (eval (ENum (4)) s))%Z)),18%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDialloc_init_i)
             s) >= (eval (ENum (4)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDialloc_init_i
             (Some (EAdd (EVar IDialloc_init_i) (ENum (1))))),21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDialloc_init_z (Some (EAdd (ENum (1))
             (EVar IDialloc_init_z)))),24%positive)::
             (24%positive,AWeaken,15%positive)::nil
|}.

Definition ialloc_init_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_z) <= 0)%Z
    | 3%positive => (-1 * (s IDialloc_init_z) <= 0 /\ 1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_i) <= 0)%Z
    | 4%positive => (-1 * (s IDialloc_init_i) <= 0 /\ 1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_z) <= 0)%Z
    | 5%positive => (-1 * (s IDialloc_init_z) <= 0 /\ 1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_i) <= 0)%Z
    | 6%positive => (-1 * (s IDialloc_init_i) <= 0 /\ 1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_z) <= 0)%Z
    | 7%positive => (-1 * (s IDialloc_init_z) <= 0 /\ 1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_i) <= 0)%Z
    | 8%positive => (-1 * (s IDialloc_init_i) <= 0 /\ 1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_z) <= 0 /\ 1 * (s IDialloc_init__tmp) <= 0 /\ -1 * (s IDialloc_init__tmp) <= 0)%Z
    | 9%positive => (-1 * (s IDialloc_init__tmp) <= 0 /\ 1 * (s IDialloc_init__tmp) <= 0 /\ -1 * (s IDialloc_init_z) <= 0 /\ 1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_i) <= 0)%Z
    | 10%positive => (-1 * (s IDialloc_init_i) <= 0 /\ 1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_z) <= 0)%Z
    | 11%positive => (-1 * (s IDialloc_init_z) <= 0 /\ 1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_i) <= 0)%Z
    | 12%positive => (-1 * (s IDialloc_init_i) <= 0 /\ 1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_z) <= 0)%Z
    | 13%positive => (-1 * (s IDialloc_init_z) <= 0 /\ 1 * (s IDialloc_init_z) <= 0 /\ 1 * (s IDialloc_init_i) <= 0 /\ -1 * (s IDialloc_init_i) <= 0)%Z
    | 14%positive => (-1 * (s IDialloc_init_i) <= 0 /\ 1 * (s IDialloc_init_i) <= 0 /\ 1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_z) <= 0)%Z
    | 15%positive => (-1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_i) <= 0 /\ 1 * (s IDialloc_init_i) + -4 <= 0)%Z
    | 16%positive => (1 * (s IDialloc_init_i) + -4 <= 0 /\ -1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_i) + 4 <= 0)%Z
    | 17%positive => (-1 * (s IDialloc_init_i) + 4 <= 0 /\ -1 * (s IDialloc_init_z) <= 0 /\ 1 * (s IDialloc_init_i) + -4 <= 0)%Z
    | 18%positive => (-1 * (s IDialloc_init_i) <= 0 /\ -1 * (s IDialloc_init_z) <= 0 /\ 1 * (s IDialloc_init_i) + -3 <= 0)%Z
    | 19%positive => (1 * (s IDialloc_init_i) + -3 <= 0 /\ -1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_i) <= 0)%Z
    | 20%positive => (-1 * (s IDialloc_init_i) <= 0 /\ -1 * (s IDialloc_init_z) <= 0 /\ 1 * (s IDialloc_init_i) + -3 <= 0)%Z
    | 21%positive => (-1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_i) + 1 <= 0 /\ 1 * (s IDialloc_init_i) + -4 <= 0)%Z
    | 22%positive => (1 * (s IDialloc_init_i) + -4 <= 0 /\ -1 * (s IDialloc_init_i) + 1 <= 0 /\ -1 * (s IDialloc_init_z) <= 0)%Z
    | 23%positive => (-1 * (s IDialloc_init_z) <= 0 /\ -1 * (s IDialloc_init_i) + 1 <= 0 /\ 1 * (s IDialloc_init_i) + -4 <= 0)%Z
    | 24%positive => (1 * (s IDialloc_init_i) + -4 <= 0 /\ -1 * (s IDialloc_init_i) + 1 <= 0 /\ -1 * (s IDialloc_init_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition ialloc_init_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((4 # 1))%Q
    | 2%positive => ((4 # 1) + (s IDialloc_init_z))%Q
    | 3%positive => ((4 # 1) + (s IDialloc_init_z))%Q
    | 4%positive => ((4 # 1) + (s IDialloc_init_z))%Q
    | 5%positive => ((4 # 1) + (s IDialloc_init_z))%Q
    | 6%positive => ((4 # 1) + (s IDialloc_init_z))%Q
    | 7%positive => ((4 # 1) + (s IDialloc_init_z))%Q
    | 8%positive => ((4 # 1) + (s IDialloc_init_z))%Q
    | 9%positive => ((4 # 1) + (s IDialloc_init_z))%Q
    | 10%positive => ((4 # 1) + (s IDialloc_init_z))%Q
    | 11%positive => ((4 # 1) + (s IDialloc_init_z))%Q
    | 12%positive => ((4 # 1) + (s IDialloc_init_z))%Q
    | 13%positive => ((s IDialloc_init_z) + max0(4 - (s IDialloc_init_i)))%Q
    | 14%positive => ((s IDialloc_init_z) + max0(4 - (s IDialloc_init_i)))%Q
    | 15%positive => ((s IDialloc_init_z) + max0(4 - (s IDialloc_init_i)))%Q
    | 16%positive => ((s IDialloc_init_z) + max0(4 - (s IDialloc_init_i)))%Q
    | 17%positive => ((s IDialloc_init_z))%Q
    | 18%positive => ((s IDialloc_init_z) + max0(4 - (s IDialloc_init_i)))%Q
    | 19%positive => ((1 # 1) + (s IDialloc_init_z)
                      + max0(3 - (s IDialloc_init_i)))%Q
    | 20%positive => ((1 # 1) + (s IDialloc_init_z)
                      + max0(3 - (s IDialloc_init_i)))%Q
    | 21%positive => ((1 # 1) + (s IDialloc_init_z)
                      + max0(4 - (s IDialloc_init_i)))%Q
    | 22%positive => ((1 # 1) + (s IDialloc_init_z)
                      + max0(4 - (s IDialloc_init_i)))%Q
    | 23%positive => ((1 # 1) + (s IDialloc_init_z)
                      + max0(4 - (s IDialloc_init_i)))%Q
    | 24%positive => ((s IDialloc_init_z) + max0(4 - (s IDialloc_init_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition ialloc_init_hints (p : node) (s : state) := 
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
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDialloc_init_i)) (3
                                                                    - (s IDialloc_init_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDialloc_init_i))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_pre_decrement (4 - (s IDialloc_init_i)) (1)]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | _ => []
  end.


Theorem ialloc_init_ai_correct:
  forall s p' s', steps (g_start ialloc_init) s (g_edges ialloc_init) p' s' -> ialloc_init_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ialloc_init_pot_correct:
  forall s p' s',
    steps (g_start ialloc_init) s (g_edges ialloc_init) p' s' ->
    (ialloc_init_pot (g_start ialloc_init) s >= ialloc_init_pot p' s')%Q.
Proof.
  check_lp ialloc_init_ai_correct ialloc_init_hints.
Qed.

