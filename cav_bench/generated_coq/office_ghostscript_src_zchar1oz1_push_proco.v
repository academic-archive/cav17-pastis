Require Import pasta.Pasta.

Notation IDz1_push_proc_z := 1%positive.
Notation IDz1_push_proc__tmp := 2%positive.
Notation IDz1_push_proc__tmp1 := 3%positive.
Notation IDz1_push_proc_i := 4%positive.
Notation IDz1_push_proc_count := 5%positive.
Notation IDz1_push_proc_ignore := 6%positive.
Notation IDz1_push_proc_pf := 7%positive.
Definition z1_push_proc : graph := {|
  g_start := 1%positive;
  g_end := 23%positive;
  g_edges := (1%positive,(AAssign IDz1_push_proc_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDz1_push_proc__tmp
             (Some (EVar IDz1_push_proc_count))),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,ANone,20%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDz1_push_proc_i (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDz1_push_proc_i)
             s) < (eval (EVar IDz1_push_proc__tmp) s))%Z)),13%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDz1_push_proc_i)
             s) >= (eval (EVar IDz1_push_proc__tmp) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDz1_push_proc__tmp1 (Some (ENum (0)))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,AWeaken,23%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDz1_push_proc_i
             (Some (EAdd (EVar IDz1_push_proc_i) (ENum (1))))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDz1_push_proc_z (Some (EAdd (ENum (1))
             (EVar IDz1_push_proc_z)))),19%positive)::
             (19%positive,AWeaken,8%positive)::
             (20%positive,(AAssign IDz1_push_proc__tmp1 (Some (ENum (-16)))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::nil
|}.

Definition z1_push_proc_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0)%Z
    | 3%positive => (-1 * (s IDz1_push_proc_z) <= 0 /\ 1 * (s IDz1_push_proc_z) <= 0)%Z
    | 4%positive => (1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0)%Z
    | 5%positive => (-1 * (s IDz1_push_proc_z) <= 0 /\ 1 * (s IDz1_push_proc_z) <= 0)%Z
    | 6%positive => (1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0 /\ 1 * (s IDz1_push_proc_i) <= 0 /\ -1 * (s IDz1_push_proc_i) <= 0)%Z
    | 7%positive => (-1 * (s IDz1_push_proc_i) <= 0 /\ 1 * (s IDz1_push_proc_i) <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0 /\ 1 * (s IDz1_push_proc_z) <= 0)%Z
    | 8%positive => (-1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc_i) <= 0)%Z
    | 9%positive => (-1 * (s IDz1_push_proc_i) <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0 /\ 1 * (s IDz1_push_proc__tmp)+ -1 * (s IDz1_push_proc_i) <= 0)%Z
    | 10%positive => (1 * (s IDz1_push_proc__tmp)+ -1 * (s IDz1_push_proc_i) <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc_i) <= 0)%Z
    | 11%positive => (-1 * (s IDz1_push_proc_i) <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0 /\ 1 * (s IDz1_push_proc__tmp)+ -1 * (s IDz1_push_proc_i) <= 0 /\ 1 * (s IDz1_push_proc__tmp1) <= 0 /\ -1 * (s IDz1_push_proc__tmp1) <= 0)%Z
    | 12%positive => (-1 * (s IDz1_push_proc__tmp1) <= 0 /\ 1 * (s IDz1_push_proc__tmp1) <= 0 /\ 1 * (s IDz1_push_proc__tmp)+ -1 * (s IDz1_push_proc_i) <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc_i) <= 0)%Z
    | 13%positive => (-1 * (s IDz1_push_proc_i) <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc__tmp)+ 1 * (s IDz1_push_proc_i) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDz1_push_proc__tmp)+ 1 * (s IDz1_push_proc_i) + 1 <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc_i) <= 0)%Z
    | 15%positive => (-1 * (s IDz1_push_proc_i) <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc__tmp)+ 1 * (s IDz1_push_proc_i) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc_i) + 1 <= 0 /\ -1 * (s IDz1_push_proc__tmp)+ 1 * (s IDz1_push_proc_i) <= 0)%Z
    | 17%positive => (-1 * (s IDz1_push_proc__tmp)+ 1 * (s IDz1_push_proc_i) <= 0 /\ -1 * (s IDz1_push_proc_i) + 1 <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0)%Z
    | 18%positive => (-1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc_i) + 1 <= 0 /\ -1 * (s IDz1_push_proc__tmp)+ 1 * (s IDz1_push_proc_i) <= 0)%Z
    | 19%positive => (-1 * (s IDz1_push_proc__tmp)+ 1 * (s IDz1_push_proc_i) <= 0 /\ -1 * (s IDz1_push_proc_i) + 1 <= 0 /\ -1 * (s IDz1_push_proc_z) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDz1_push_proc_z) <= 0 /\ 1 * (s IDz1_push_proc_z) <= 0)%Z
    | 21%positive => (1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0 /\ 1 * (s IDz1_push_proc__tmp1) + 16 <= 0 /\ -1 * (s IDz1_push_proc__tmp1) + -16 <= 0)%Z
    | 22%positive => (-1 * (s IDz1_push_proc__tmp1) + -16 <= 0 /\ 1 * (s IDz1_push_proc__tmp1) + 16 <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0 /\ 1 * (s IDz1_push_proc_z) <= 0)%Z
    | 23%positive => (1 * (s IDz1_push_proc__tmp1) <= 0 /\ -1 * (s IDz1_push_proc_z) <= 0 /\ -1 * (s IDz1_push_proc__tmp1) + -16 <= 0)%Z
    | _ => False
  end.

Definition z1_push_proc_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDz1_push_proc_count)))%Q
    | 2%positive => ((s IDz1_push_proc_z) + max0((s IDz1_push_proc_count)))%Q
    | 3%positive => ((s IDz1_push_proc_z) + max0((s IDz1_push_proc__tmp)))%Q
    | 4%positive => ((s IDz1_push_proc_z) + max0((s IDz1_push_proc__tmp)))%Q
    | 5%positive => ((s IDz1_push_proc_z) + max0((s IDz1_push_proc__tmp)))%Q
    | 6%positive => ((s IDz1_push_proc_z)
                     + max0((s IDz1_push_proc__tmp) - (s IDz1_push_proc_i)))%Q
    | 7%positive => ((s IDz1_push_proc_z)
                     + max0((s IDz1_push_proc__tmp) - (s IDz1_push_proc_i)))%Q
    | 8%positive => ((s IDz1_push_proc_z)
                     + max0((s IDz1_push_proc__tmp) - (s IDz1_push_proc_i)))%Q
    | 9%positive => ((s IDz1_push_proc_z)
                     + max0((s IDz1_push_proc__tmp) - (s IDz1_push_proc_i)))%Q
    | 10%positive => ((s IDz1_push_proc_z)
                      + max0((s IDz1_push_proc__tmp) - (s IDz1_push_proc_i)))%Q
    | 11%positive => ((s IDz1_push_proc_z)
                      + max0((s IDz1_push_proc__tmp) - (s IDz1_push_proc_i)))%Q
    | 12%positive => ((s IDz1_push_proc_z)
                      + max0((s IDz1_push_proc__tmp) - (s IDz1_push_proc_i)))%Q
    | 13%positive => ((s IDz1_push_proc_z)
                      + max0((s IDz1_push_proc__tmp) - (s IDz1_push_proc_i)))%Q
    | 14%positive => ((1 # 1) + (s IDz1_push_proc_z)
                      + max0(-1 + (s IDz1_push_proc__tmp)
                             - (s IDz1_push_proc_i)))%Q
    | 15%positive => ((1 # 1) + (s IDz1_push_proc_z)
                      + max0(-1 + (s IDz1_push_proc__tmp)
                             - (s IDz1_push_proc_i)))%Q
    | 16%positive => ((1 # 1) + (s IDz1_push_proc_z)
                      + max0((s IDz1_push_proc__tmp) - (s IDz1_push_proc_i)))%Q
    | 17%positive => ((1 # 1) + (s IDz1_push_proc_z)
                      + max0((s IDz1_push_proc__tmp) - (s IDz1_push_proc_i)))%Q
    | 18%positive => ((1 # 1) + (s IDz1_push_proc_z)
                      + max0((s IDz1_push_proc__tmp) - (s IDz1_push_proc_i)))%Q
    | 19%positive => ((s IDz1_push_proc_z)
                      + max0((s IDz1_push_proc__tmp) - (s IDz1_push_proc_i)))%Q
    | 20%positive => ((s IDz1_push_proc_z) + max0((s IDz1_push_proc__tmp)))%Q
    | 21%positive => ((s IDz1_push_proc_z) + max0((s IDz1_push_proc__tmp)))%Q
    | 22%positive => ((s IDz1_push_proc_z) + max0((s IDz1_push_proc__tmp)))%Q
    | 23%positive => ((s IDz1_push_proc_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition z1_push_proc_hints (p : node) (s : state) := 
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
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDz1_push_proc__tmp)
                                                             - (s IDz1_push_proc_i)) (-1
                                                                    + (s IDz1_push_proc__tmp)
                                                                    - (s IDz1_push_proc_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDz1_push_proc__tmp)
                                            - (s IDz1_push_proc_i))]
    | 13%positive => [(*0 1*) F_max0_pre_decrement ((s IDz1_push_proc__tmp)
                                                    - (s IDz1_push_proc_i)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_max0_ge_0 ((s IDz1_push_proc__tmp))]
    | 23%positive => []
    | _ => []
  end.


Theorem z1_push_proc_ai_correct:
  forall s p' s', steps (g_start z1_push_proc) s (g_edges z1_push_proc) p' s' -> z1_push_proc_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem z1_push_proc_pot_correct:
  forall s p' s',
    steps (g_start z1_push_proc) s (g_edges z1_push_proc) p' s' ->
    (z1_push_proc_pot (g_start z1_push_proc) s >= z1_push_proc_pot p' s')%Q.
Proof.
  check_lp z1_push_proc_ai_correct z1_push_proc_hints.
Qed.

