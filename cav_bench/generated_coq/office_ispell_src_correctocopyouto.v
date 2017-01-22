Require Import pasta.Pasta.

Notation IDcopyout_z := 1%positive.
Notation IDcopyout__tmp := 2%positive.
Notation IDcopyout_aflag := 3%positive.
Notation IDcopyout_lflag := 4%positive.
Notation IDcopyout_cc := 5%positive.
Notation IDcopyout_cnt := 6%positive.
Definition copyout : graph := {|
  g_start := 1%positive;
  g_end := 23%positive;
  g_edges := (1%positive,(AAssign IDcopyout_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDcopyout__tmp
             (Some (EVar IDcopyout_cnt))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDcopyout__tmp
             (Some (EAdd (EVar IDcopyout__tmp) (ENum (-1))))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EAdd (EVar IDcopyout__tmp)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),8%positive)::
             (6%positive,(AGuard (fun s => ((eval (EAdd (EVar IDcopyout__tmp)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,23%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,21%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDcopyout_aflag)
             s) <> (eval (ENum (0)) s))%Z)),17%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDcopyout_aflag)
             s) = (eval (ENum (0)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDcopyout_lflag)
             s) <> (eval (ENum (0)) s))%Z)),16%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDcopyout_lflag)
             s) = (eval (ENum (0)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,18%positive)::
             (16%positive,AWeaken,18%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDcopyout_z (Some (EAdd (ENum (1))
             (EVar IDcopyout_z)))),4%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::nil
|}.

Definition copyout_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcopyout_z) <= 0 /\ -1 * (s IDcopyout_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcopyout_z) <= 0 /\ 1 * (s IDcopyout_z) <= 0)%Z
    | 4%positive => (-1 * (s IDcopyout_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcopyout_z) <= 0)%Z
    | 6%positive => (-1 * (s IDcopyout_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcopyout_z) <= 0 /\ 1 * (s IDcopyout__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDcopyout_z) <= 0 /\ -1 * (s IDcopyout__tmp) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDcopyout__tmp) + 1 <= 0 /\ -1 * (s IDcopyout_z) <= 0)%Z
    | 10%positive => (-1 * (s IDcopyout_z) <= 0 /\ -1 * (s IDcopyout__tmp) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDcopyout__tmp) + 1 <= 0 /\ -1 * (s IDcopyout_z) <= 0)%Z
    | 12%positive => (-1 * (s IDcopyout_z) <= 0 /\ -1 * (s IDcopyout__tmp) + 1 <= 0 /\ 1 * (s IDcopyout_aflag) <= 0 /\ -1 * (s IDcopyout_aflag) <= 0)%Z
    | 13%positive => (-1 * (s IDcopyout_aflag) <= 0 /\ 1 * (s IDcopyout_aflag) <= 0 /\ -1 * (s IDcopyout__tmp) + 1 <= 0 /\ -1 * (s IDcopyout_z) <= 0)%Z
    | 14%positive => (-1 * (s IDcopyout_z) <= 0 /\ -1 * (s IDcopyout__tmp) + 1 <= 0 /\ 1 * (s IDcopyout_aflag) <= 0 /\ -1 * (s IDcopyout_aflag) <= 0 /\ 1 * (s IDcopyout_lflag) <= 0 /\ -1 * (s IDcopyout_lflag) <= 0)%Z
    | 15%positive => (-1 * (s IDcopyout_lflag) <= 0 /\ 1 * (s IDcopyout_lflag) <= 0 /\ -1 * (s IDcopyout_aflag) <= 0 /\ 1 * (s IDcopyout_aflag) <= 0 /\ -1 * (s IDcopyout__tmp) + 1 <= 0 /\ -1 * (s IDcopyout_z) <= 0)%Z
    | 16%positive => (-1 * (s IDcopyout_z) <= 0 /\ -1 * (s IDcopyout__tmp) + 1 <= 0 /\ 1 * (s IDcopyout_aflag) <= 0 /\ -1 * (s IDcopyout_aflag) <= 0)%Z
    | 17%positive => (-1 * (s IDcopyout_z) <= 0 /\ -1 * (s IDcopyout__tmp) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDcopyout__tmp) + 1 <= 0 /\ -1 * (s IDcopyout_z) <= 0)%Z
    | 19%positive => (-1 * (s IDcopyout_z) <= 0 /\ -1 * (s IDcopyout__tmp) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDcopyout__tmp) + 1 <= 0 /\ -1 * (s IDcopyout_z) <= 0)%Z
    | 21%positive => (-1 * (s IDcopyout_z) <= 0 /\ -1 * (s IDcopyout__tmp) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDcopyout__tmp) + 1 <= 0 /\ -1 * (s IDcopyout_z) <= 0)%Z
    | 23%positive => (-1 * (s IDcopyout_z) <= 0)%Z
    | _ => False
  end.

Definition copyout_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDcopyout_cnt)))%Q
    | 2%positive => ((s IDcopyout_z) + max0(-1 + (s IDcopyout_cnt)))%Q
    | 3%positive => ((s IDcopyout_z) + max0(-1 + (s IDcopyout__tmp)))%Q
    | 4%positive => ((s IDcopyout_z) + max0(-1 + (s IDcopyout__tmp)))%Q
    | 5%positive => ((s IDcopyout_z) + max0((s IDcopyout__tmp)))%Q
    | 6%positive => ((s IDcopyout_z) + max0((s IDcopyout__tmp)))%Q
    | 7%positive => ((s IDcopyout_z) + max0((s IDcopyout__tmp)))%Q
    | 8%positive => ((s IDcopyout_z) + max0((s IDcopyout__tmp)))%Q
    | 9%positive => ((1 # 1) + (s IDcopyout_z)
                     + max0(-1 + (s IDcopyout__tmp)))%Q
    | 10%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 11%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 12%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 13%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 14%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 17%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 18%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 19%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 20%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 21%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 22%positive => ((1 # 1) + (s IDcopyout_z)
                      + max0(-1 + (s IDcopyout__tmp)))%Q
    | 23%positive => ((s IDcopyout_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition copyout_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcopyout__tmp)) (-1
                                                                    + (s IDcopyout__tmp)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDcopyout__tmp))]
    | 8%positive => [(*0 1*) F_max0_pre_decrement ((s IDcopyout__tmp)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDcopyout__tmp))]
    | 23%positive => []
    | _ => []
  end.


Theorem copyout_ai_correct:
  forall s p' s', steps (g_start copyout) s (g_edges copyout) p' s' -> copyout_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem copyout_pot_correct:
  forall s p' s',
    steps (g_start copyout) s (g_edges copyout) p' s' ->
    (copyout_pot (g_start copyout) s >= copyout_pot p' s')%Q.
Proof.
  check_lp copyout_ai_correct copyout_hints.
Qed.

