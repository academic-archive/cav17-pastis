Require Import pasta.Pasta.

Notation IDcheckcmap_z := 1%positive.
Notation IDcheckcmap__tmp := 2%positive.
Notation IDcheckcmap__tmp1 := 3%positive.
Notation IDcheckcmap_b := 4%positive.
Notation IDcheckcmap_g := 5%positive.
Notation IDcheckcmap_n := 6%positive.
Notation IDcheckcmap_r := 7%positive.
Notation IDcheckcmap_tif := 8%positive.
Definition checkcmap : graph := {|
  g_start := 1%positive;
  g_end := 23%positive;
  g_edges := (1%positive,(AAssign IDcheckcmap_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDcheckcmap__tmp
             (Some (EVar IDcheckcmap_n))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDcheckcmap__tmp
             (Some (EAdd (EVar IDcheckcmap__tmp) (ENum (-1))))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDcheckcmap__tmp)
             s) > (eval (ENum (0)) s))%Z)),11%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDcheckcmap__tmp)
             s) <= (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDcheckcmap__tmp1 (Some (ENum (8)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,23%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,20%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,20%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,20%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDcheckcmap_z (Some (EAdd (ENum (1))
             (EVar IDcheckcmap_z)))),4%positive)::
             (20%positive,(AAssign IDcheckcmap__tmp1 (Some (ENum (16)))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::nil
|}.

Definition checkcmap_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcheckcmap_z) <= 0 /\ -1 * (s IDcheckcmap_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcheckcmap_z) <= 0 /\ 1 * (s IDcheckcmap_z) <= 0)%Z
    | 4%positive => (-1 * (s IDcheckcmap_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcheckcmap_z) <= 0)%Z
    | 6%positive => (-1 * (s IDcheckcmap_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcheckcmap_z) <= 0 /\ 1 * (s IDcheckcmap__tmp) <= 0)%Z
    | 8%positive => (1 * (s IDcheckcmap__tmp) <= 0 /\ -1 * (s IDcheckcmap_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcheckcmap_z) <= 0 /\ 1 * (s IDcheckcmap__tmp) <= 0 /\ 1 * (s IDcheckcmap__tmp1) + -8 <= 0 /\ -1 * (s IDcheckcmap__tmp1) + 8 <= 0)%Z
    | 10%positive => (-1 * (s IDcheckcmap__tmp1) + 8 <= 0 /\ 1 * (s IDcheckcmap__tmp1) + -8 <= 0 /\ 1 * (s IDcheckcmap__tmp) <= 0 /\ -1 * (s IDcheckcmap_z) <= 0)%Z
    | 11%positive => (-1 * (s IDcheckcmap_z) <= 0 /\ -1 * (s IDcheckcmap__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDcheckcmap__tmp) + 1 <= 0 /\ -1 * (s IDcheckcmap_z) <= 0)%Z
    | 13%positive => (-1 * (s IDcheckcmap_z) <= 0 /\ -1 * (s IDcheckcmap__tmp) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDcheckcmap__tmp) + 1 <= 0 /\ -1 * (s IDcheckcmap_z) <= 0)%Z
    | 15%positive => (-1 * (s IDcheckcmap_z) <= 0 /\ -1 * (s IDcheckcmap__tmp) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDcheckcmap__tmp) + 1 <= 0 /\ -1 * (s IDcheckcmap_z) <= 0)%Z
    | 17%positive => (-1 * (s IDcheckcmap_z) <= 0 /\ -1 * (s IDcheckcmap__tmp) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDcheckcmap__tmp) + 1 <= 0 /\ -1 * (s IDcheckcmap_z) <= 0)%Z
    | 19%positive => (-1 * (s IDcheckcmap_z) <= 0 /\ -1 * (s IDcheckcmap__tmp) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDcheckcmap_z) <= 0 /\ -1 * (s IDcheckcmap__tmp) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDcheckcmap__tmp) + 1 <= 0 /\ -1 * (s IDcheckcmap_z) <= 0 /\ 1 * (s IDcheckcmap__tmp1) + -16 <= 0 /\ -1 * (s IDcheckcmap__tmp1) + 16 <= 0)%Z
    | 22%positive => (-1 * (s IDcheckcmap__tmp1) + 16 <= 0 /\ 1 * (s IDcheckcmap__tmp1) + -16 <= 0 /\ -1 * (s IDcheckcmap_z) <= 0 /\ -1 * (s IDcheckcmap__tmp) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDcheckcmap__tmp1) + 8 <= 0 /\ -1 * (s IDcheckcmap_z) <= 0 /\ 1 * (s IDcheckcmap__tmp1) + -16 <= 0)%Z
    | _ => False
  end.

Definition checkcmap_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDcheckcmap_n)))%Q
    | 2%positive => ((s IDcheckcmap_z) + max0(-1 + (s IDcheckcmap_n)))%Q
    | 3%positive => ((s IDcheckcmap_z) + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 4%positive => ((s IDcheckcmap_z) + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 5%positive => ((s IDcheckcmap_z) + max0((s IDcheckcmap__tmp)))%Q
    | 6%positive => ((s IDcheckcmap_z) + max0((s IDcheckcmap__tmp)))%Q
    | 7%positive => ((s IDcheckcmap_z) + max0((s IDcheckcmap__tmp)))%Q
    | 8%positive => ((s IDcheckcmap_z) + max0((s IDcheckcmap__tmp)))%Q
    | 9%positive => ((s IDcheckcmap_z) + max0((s IDcheckcmap__tmp)))%Q
    | 10%positive => ((s IDcheckcmap_z) + max0((s IDcheckcmap__tmp)))%Q
    | 11%positive => ((s IDcheckcmap_z) + max0((s IDcheckcmap__tmp)))%Q
    | 12%positive => ((1 # 1) + (s IDcheckcmap_z)
                      + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 13%positive => ((1 # 1) + (s IDcheckcmap_z)
                      + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 14%positive => ((1 # 1) + (s IDcheckcmap_z)
                      + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDcheckcmap_z)
                      + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDcheckcmap_z)
                      + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 17%positive => ((1 # 1) + (s IDcheckcmap_z)
                      + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 18%positive => ((1 # 1) + (s IDcheckcmap_z)
                      + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 19%positive => ((1 # 1) + (s IDcheckcmap_z)
                      + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 20%positive => ((1 # 1) + (s IDcheckcmap_z)
                      + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 21%positive => ((s IDcheckcmap_z)
                      + (1 # 8) * max0(-8 + (s IDcheckcmap__tmp1))
                      + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 22%positive => ((s IDcheckcmap_z)
                      + (1 # 8) * max0(-8 + (s IDcheckcmap__tmp1))
                      + max0(-1 + (s IDcheckcmap__tmp)))%Q
    | 23%positive => ((s IDcheckcmap_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition checkcmap_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcheckcmap__tmp)) (-1
                                                                    + (s IDcheckcmap__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDcheckcmap__tmp))]
    | 11%positive => [(*-1 0*) F_max0_pre_decrement ((s IDcheckcmap__tmp)) (1)]
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
    | 22%positive => [(*-1 0*) F_max0_ge_0 (-1 + (s IDcheckcmap__tmp));
                      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (-8
                                                                    + 
                                                                    (s IDcheckcmap__tmp1))) (F_check_ge (0) (0))]
    | 23%positive => []
    | _ => []
  end.


Theorem checkcmap_ai_correct:
  forall s p' s', steps (g_start checkcmap) s (g_edges checkcmap) p' s' -> checkcmap_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem checkcmap_pot_correct:
  forall s p' s',
    steps (g_start checkcmap) s (g_edges checkcmap) p' s' ->
    (checkcmap_pot (g_start checkcmap) s >= checkcmap_pot p' s')%Q.
Proof.
  check_lp checkcmap_ai_correct checkcmap_hints.
Qed.

