Require Import pasta.Pasta.

Notation IDpop_estack_z := 1%positive.
Notation IDpop_estack__tmp := 2%positive.
Notation IDpop_estack_idx := 3%positive.
Notation IDpop_estack_popped := 4%positive.
Notation IDpop_estack_count := 5%positive.
Definition pop_estack : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDpop_estack_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDpop_estack_idx)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDpop_estack__tmp)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDpop_estack__tmp
             (Some (EVar IDpop_estack_count))),6%positive)::
             (6%positive,(AAssign IDpop_estack_idx (Some (ENum (0)))),
             7%positive)::
             (7%positive,(AAssign IDpop_estack_popped (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDpop_estack_idx)
             s) < (eval (EVar IDpop_estack__tmp) s))%Z)),13%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDpop_estack_idx)
             s) >= (eval (EVar IDpop_estack__tmp) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (14%positive,ANone,17%positive)::
             (15%positive,(AAssign IDpop_estack_popped
             (Some (EAdd (EVar IDpop_estack_idx) (ENum (1))))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDpop_estack_idx
             (Some (EAdd (EVar IDpop_estack_idx) (ENum (1))))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDpop_estack_z (Some (EAdd (ENum (1))
             (EVar IDpop_estack_z)))),22%positive)::
             (22%positive,AWeaken,10%positive)::nil
|}.

Definition pop_estack_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpop_estack_z) <= 0 /\ 1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_idx) <= 0)%Z
    | 4%positive => (-1 * (s IDpop_estack_idx) <= 0 /\ 1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDpop_estack__tmp) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ 1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_idx) <= 0)%Z
    | 6%positive => (-1 * (s IDpop_estack_idx) <= 0 /\ 1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_z) <= 0)%Z
    | 7%positive => (-1 * (s IDpop_estack_z) <= 0 /\ 1 * (s IDpop_estack_z) <= 0 /\ 1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack_idx) <= 0)%Z
    | 8%positive => (-1 * (s IDpop_estack_idx) <= 0 /\ 1 * (s IDpop_estack_idx) <= 0 /\ 1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ 1 * (s IDpop_estack_popped) <= 0 /\ -1 * (s IDpop_estack_popped) <= 0)%Z
    | 9%positive => (-1 * (s IDpop_estack_popped) <= 0 /\ 1 * (s IDpop_estack_popped) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ 1 * (s IDpop_estack_z) <= 0 /\ 1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack_idx) <= 0)%Z
    | 10%positive => (-1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_popped) <= 0)%Z
    | 11%positive => (-1 * (s IDpop_estack_popped) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_idx) <= 0 /\ 1 * (s IDpop_estack__tmp)+ -1 * (s IDpop_estack_idx) <= 0)%Z
    | 12%positive => (1 * (s IDpop_estack__tmp)+ -1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_popped) <= 0)%Z
    | 13%positive => (-1 * (s IDpop_estack_popped) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack__tmp)+ 1 * (s IDpop_estack_idx) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDpop_estack__tmp)+ 1 * (s IDpop_estack_idx) + 1 <= 0 /\ -1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_popped) <= 0)%Z
    | 15%positive => (-1 * (s IDpop_estack_popped) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack__tmp)+ 1 * (s IDpop_estack_idx) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDpop_estack__tmp)+ 1 * (s IDpop_estack_idx) + 1 <= 0 /\ -1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack__tmp)+ 1 * (s IDpop_estack_popped) <= 0 /\ -1 * (s IDpop_estack_popped) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDpop_estack_popped) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack__tmp)+ 1 * (s IDpop_estack_idx) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDpop_estack__tmp)+ 1 * (s IDpop_estack_idx) + 1 <= 0 /\ -1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_popped) <= 0)%Z
    | 19%positive => (-1 * (s IDpop_estack_popped) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack__tmp)+ 1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack_idx) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDpop_estack_idx) + 1 <= 0 /\ -1 * (s IDpop_estack__tmp)+ 1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack_popped) <= 0)%Z
    | 21%positive => (-1 * (s IDpop_estack_popped) <= 0 /\ -1 * (s IDpop_estack_z) <= 0 /\ -1 * (s IDpop_estack__tmp)+ 1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack_idx) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDpop_estack_idx) + 1 <= 0 /\ -1 * (s IDpop_estack__tmp)+ 1 * (s IDpop_estack_idx) <= 0 /\ -1 * (s IDpop_estack_popped) <= 0 /\ -1 * (s IDpop_estack_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition pop_estack_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDpop_estack_count)))%Q
    | 2%positive => ((s IDpop_estack_z) + max0((s IDpop_estack_count)))%Q
    | 3%positive => ((s IDpop_estack_z) + max0((s IDpop_estack_count)))%Q
    | 4%positive => ((s IDpop_estack_z) + max0((s IDpop_estack_count)))%Q
    | 5%positive => ((s IDpop_estack_z) + max0((s IDpop_estack_count)))%Q
    | 6%positive => ((s IDpop_estack_z) + max0((s IDpop_estack__tmp)))%Q
    | 7%positive => ((s IDpop_estack_z)
                     + max0((s IDpop_estack__tmp) - (s IDpop_estack_idx)))%Q
    | 8%positive => ((s IDpop_estack_z)
                     + max0((s IDpop_estack__tmp) - (s IDpop_estack_idx)))%Q
    | 9%positive => ((s IDpop_estack_z)
                     + max0((s IDpop_estack__tmp) - (s IDpop_estack_idx)))%Q
    | 10%positive => ((s IDpop_estack_z)
                      + max0((s IDpop_estack__tmp) - (s IDpop_estack_idx)))%Q
    | 11%positive => ((s IDpop_estack_z)
                      + max0((s IDpop_estack__tmp) - (s IDpop_estack_idx)))%Q
    | 12%positive => ((s IDpop_estack_z))%Q
    | 13%positive => ((s IDpop_estack_z)
                      + max0((s IDpop_estack__tmp) - (s IDpop_estack_idx)))%Q
    | 14%positive => ((1 # 1) + (s IDpop_estack_z)
                      + max0(-1 + (s IDpop_estack__tmp)
                             - (s IDpop_estack_idx)))%Q
    | 15%positive => ((1 # 1) + (s IDpop_estack_z)
                      + max0(-1 + (s IDpop_estack__tmp)
                             - (s IDpop_estack_idx)))%Q
    | 16%positive => ((1 # 1) + (s IDpop_estack_z)
                      + max0(-1 + (s IDpop_estack__tmp)
                             - (s IDpop_estack_idx)))%Q
    | 17%positive => ((1 # 1) + (s IDpop_estack_z)
                      + max0(-1 + (s IDpop_estack__tmp)
                             - (s IDpop_estack_idx)))%Q
    | 18%positive => ((1 # 1) + (s IDpop_estack_z)
                      + max0(-1 + (s IDpop_estack__tmp)
                             - (s IDpop_estack_idx)))%Q
    | 19%positive => ((1 # 1) + (s IDpop_estack_z)
                      + max0((s IDpop_estack__tmp) - (s IDpop_estack_idx)))%Q
    | 20%positive => ((1 # 1) + (s IDpop_estack_z)
                      + max0((s IDpop_estack__tmp) - (s IDpop_estack_idx)))%Q
    | 21%positive => ((1 # 1) + (s IDpop_estack_z)
                      + max0((s IDpop_estack__tmp) - (s IDpop_estack_idx)))%Q
    | 22%positive => ((s IDpop_estack_z)
                      + max0((s IDpop_estack__tmp) - (s IDpop_estack_idx)))%Q
    | _ => (0 # 1)%Q
  end.

Definition pop_estack_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDpop_estack__tmp)
                                                             - (s IDpop_estack_idx)) (-1
                                                                    + (s IDpop_estack__tmp)
                                                                    - (s IDpop_estack_idx)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDpop_estack__tmp)
                                            - (s IDpop_estack_idx))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_pre_decrement ((s IDpop_estack__tmp)
                                                     - (s IDpop_estack_idx)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | _ => []
  end.


Theorem pop_estack_ai_correct:
  forall s p' s', steps (g_start pop_estack) s (g_edges pop_estack) p' s' -> pop_estack_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem pop_estack_pot_correct:
  forall s p' s',
    steps (g_start pop_estack) s (g_edges pop_estack) p' s' ->
    (pop_estack_pot (g_start pop_estack) s >= pop_estack_pot p' s')%Q.
Proof.
  check_lp pop_estack_ai_correct pop_estack_hints.
Qed.

