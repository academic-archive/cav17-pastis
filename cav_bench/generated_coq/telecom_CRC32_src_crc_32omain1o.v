Require Import pasta.Pasta.

Notation IDmain1_z := 1%positive.
Notation IDmain1__tmp := 2%positive.
Notation IDmain1__tmp1 := 3%positive.
Notation IDmain1_errors := 4%positive.
Notation IDmain1_argc := 5%positive.
Notation IDmain1_argv := 6%positive.
Notation IDmain1_print := 7%positive.
Definition main1 : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDmain1_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDmain1__tmp (Some (EVar IDmain1_argc))),
             3%positive)::
             (3%positive,(AAssign IDmain1__tmp1 (Some (EVar IDmain1_print))),
             4%positive)::
             (4%positive,(AAssign IDmain1_errors (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDmain1__tmp
             (Some (EAdd (EVar IDmain1__tmp) (ENum (-1))))),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EAdd (EVar IDmain1__tmp)
             (ENum (-1))) s) > (eval (ENum (0)) s))%Z)),11%positive)::
             (8%positive,(AGuard (fun s => ((eval (EAdd (EVar IDmain1__tmp)
             (ENum (-1))) s) <= (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDmain1_errors None),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDmain1__tmp1) s) <>
             (eval (ENum (0)) s))%Z)),16%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDmain1__tmp1) s) =
             (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,18%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDmain1_z (Some (EAdd (ENum (1))
             (EVar IDmain1_z)))),6%positive)::nil
|}.

Definition main1_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0)%Z
    | 4%positive => (1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1_errors) <= 0 /\ -1 * (s IDmain1_errors) <= 0)%Z
    | 6%positive => (-1 * (s IDmain1_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmain1_z) <= 0)%Z
    | 8%positive => (-1 * (s IDmain1_z) <= 0)%Z
    | 9%positive => (-1 * (s IDmain1_z) <= 0 /\ 1 * (s IDmain1__tmp) + -1 <= 0)%Z
    | 10%positive => (1 * (s IDmain1__tmp) + -1 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 11%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | 12%positive => (-1 * (s IDmain1__tmp) + 2 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 13%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | 14%positive => (-1 * (s IDmain1__tmp) + 2 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 15%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0 /\ 1 * (s IDmain1__tmp1) <= 0 /\ -1 * (s IDmain1__tmp1) <= 0)%Z
    | 16%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | 17%positive => (-1 * (s IDmain1__tmp) + 2 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 18%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | 19%positive => (-1 * (s IDmain1__tmp) + 2 <= 0 /\ -1 * (s IDmain1_z) <= 0)%Z
    | 20%positive => (-1 * (s IDmain1_z) <= 0 /\ -1 * (s IDmain1__tmp) + 2 <= 0)%Z
    | _ => False
  end.

Definition main1_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-2 + (s IDmain1_argc)))%Q
    | 2%positive => ((s IDmain1_z) + max0(-2 + (s IDmain1_argc)))%Q
    | 3%positive => ((s IDmain1_z) + max0(-2 + (s IDmain1__tmp)))%Q
    | 4%positive => ((s IDmain1_z) + max0(-2 + (s IDmain1__tmp)))%Q
    | 5%positive => ((s IDmain1_z) + max0(-2 + (s IDmain1__tmp)))%Q
    | 6%positive => ((s IDmain1_z) + max0(-2 + (s IDmain1__tmp)))%Q
    | 7%positive => ((s IDmain1_z) + max0(-1 + (s IDmain1__tmp)))%Q
    | 8%positive => ((s IDmain1_z) + max0(-1 + (s IDmain1__tmp)))%Q
    | 9%positive => ((s IDmain1_z) + max0(-1 + (s IDmain1__tmp)))%Q
    | 10%positive => ((s IDmain1_z))%Q
    | 11%positive => ((s IDmain1_z) + max0(-1 + (s IDmain1__tmp)))%Q
    | 12%positive => ((s IDmain1_z) + max0(-1 + (s IDmain1__tmp)))%Q
    | 13%positive => ((s IDmain1_z) + max0(-1 + (s IDmain1__tmp)))%Q
    | 14%positive => ((1 # 1) + (s IDmain1_z) + max0(-2 + (s IDmain1__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDmain1_z) + max0(-2 + (s IDmain1__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDmain1_z) + max0(-2 + (s IDmain1__tmp)))%Q
    | 17%positive => ((1 # 1) + (s IDmain1_z) + max0(-2 + (s IDmain1__tmp)))%Q
    | 18%positive => ((1 # 1) + (s IDmain1_z) + max0(-2 + (s IDmain1__tmp)))%Q
    | 19%positive => ((1 # 1) + (s IDmain1_z) + max0(-2 + (s IDmain1__tmp)))%Q
    | 20%positive => ((1 # 1) + (s IDmain1_z) + max0(-2 + (s IDmain1__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition main1_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-1
                                                            + (s IDmain1__tmp)) (-2
                                                                    + (s IDmain1__tmp)));
                     (*-1 0*) F_max0_ge_0 (-2 + (s IDmain1__tmp))]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*0 1*) F_max0_pre_decrement (-1 + (s IDmain1__tmp)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | _ => []
  end.


Theorem main1_ai_correct:
  forall s p' s', steps (g_start main1) s (g_edges main1) p' s' -> main1_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem main1_pot_correct:
  forall s p' s',
    steps (g_start main1) s (g_edges main1) p' s' ->
    (main1_pot (g_start main1) s >= main1_pot p' s')%Q.
Proof.
  check_lp main1_ai_correct main1_hints.
Qed.

