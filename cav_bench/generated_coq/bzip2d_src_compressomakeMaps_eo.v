Require Import pasta.Pasta.

Notation IDmakeMaps_e_z := 1%positive.
Notation IDmakeMaps_e_i := 2%positive.
Notation IDmakeMaps_e_s_dref_off124 := 3%positive.
Notation IDmakeMaps_e_s := 4%positive.
Definition makeMaps_e : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDmakeMaps_e_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmakeMaps_e_s_dref_off124
             (Some (ENum (0)))),3%positive)::
             (3%positive,(AAssign IDmakeMaps_e_i (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDmakeMaps_e_i) s) <
             (eval (ENum (256)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDmakeMaps_e_i) s) >=
             (eval (ENum (256)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (10%positive,ANone,13%positive)::
             (11%positive,(AAssign IDmakeMaps_e_s_dref_off124
             (Some (EAdd (EVar IDmakeMaps_e_s_dref_off124) (ENum (1))))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDmakeMaps_e_i
             (Some (EAdd (EVar IDmakeMaps_e_i) (ENum (1))))),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDmakeMaps_e_z (Some (EAdd (ENum (1))
             (EVar IDmakeMaps_e_z)))),18%positive)::
             (18%positive,AWeaken,6%positive)::nil
|}.

Definition makeMaps_e_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmakeMaps_e_z) <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmakeMaps_e_z) <= 0 /\ 1 * (s IDmakeMaps_e_z) <= 0 /\ 1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ -1 * (s IDmakeMaps_e_s_dref_off124) <= 0)%Z
    | 4%positive => (-1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ 1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ 1 * (s IDmakeMaps_e_z) <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ 1 * (s IDmakeMaps_e_i) <= 0 /\ -1 * (s IDmakeMaps_e_i) <= 0)%Z
    | 5%positive => (-1 * (s IDmakeMaps_e_i) <= 0 /\ 1 * (s IDmakeMaps_e_i) <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ 1 * (s IDmakeMaps_e_z) <= 0 /\ 1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ -1 * (s IDmakeMaps_e_s_dref_off124) <= 0)%Z
    | 6%positive => (-1 * (s IDmakeMaps_e_z) <= 0 /\ -1 * (s IDmakeMaps_e_i) <= 0 /\ -1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ 1 * (s IDmakeMaps_e_i) + -256 <= 0)%Z
    | 7%positive => (1 * (s IDmakeMaps_e_i) + -256 <= 0 /\ -1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ -1 * (s IDmakeMaps_e_i) + 256 <= 0)%Z
    | 8%positive => (-1 * (s IDmakeMaps_e_i) + 256 <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ -1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ 1 * (s IDmakeMaps_e_i) + -256 <= 0)%Z
    | 9%positive => (-1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ -1 * (s IDmakeMaps_e_i) <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ 1 * (s IDmakeMaps_e_i) + -255 <= 0)%Z
    | 10%positive => (1 * (s IDmakeMaps_e_i) + -255 <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ -1 * (s IDmakeMaps_e_i) <= 0 /\ -1 * (s IDmakeMaps_e_s_dref_off124) <= 0)%Z
    | 11%positive => (-1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ -1 * (s IDmakeMaps_e_i) <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ 1 * (s IDmakeMaps_e_i) + -255 <= 0)%Z
    | 12%positive => (1 * (s IDmakeMaps_e_i) + -255 <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ -1 * (s IDmakeMaps_e_i) <= 0 /\ -1 * (s IDmakeMaps_e_s_dref_off124) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ -1 * (s IDmakeMaps_e_i) <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ 1 * (s IDmakeMaps_e_i) + -255 <= 0)%Z
    | 14%positive => (1 * (s IDmakeMaps_e_i) + -255 <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ -1 * (s IDmakeMaps_e_i) <= 0 /\ -1 * (s IDmakeMaps_e_s_dref_off124) <= 0)%Z
    | 15%positive => (-1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ 1 * (s IDmakeMaps_e_i) + -256 <= 0 /\ -1 * (s IDmakeMaps_e_i) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDmakeMaps_e_i) + 1 <= 0 /\ 1 * (s IDmakeMaps_e_i) + -256 <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ -1 * (s IDmakeMaps_e_s_dref_off124) <= 0)%Z
    | 17%positive => (-1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ -1 * (s IDmakeMaps_e_z) <= 0 /\ 1 * (s IDmakeMaps_e_i) + -256 <= 0 /\ -1 * (s IDmakeMaps_e_i) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDmakeMaps_e_i) + 1 <= 0 /\ 1 * (s IDmakeMaps_e_i) + -256 <= 0 /\ -1 * (s IDmakeMaps_e_s_dref_off124) <= 0 /\ -1 * (s IDmakeMaps_e_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition makeMaps_e_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((256 # 1))%Q
    | 2%positive => ((256 # 1) + (s IDmakeMaps_e_z))%Q
    | 3%positive => ((256 # 1) + (s IDmakeMaps_e_z))%Q
    | 4%positive => ((s IDmakeMaps_e_z) + max0(256 - (s IDmakeMaps_e_i)))%Q
    | 5%positive => ((s IDmakeMaps_e_z) + max0(256 - (s IDmakeMaps_e_i)))%Q
    | 6%positive => ((s IDmakeMaps_e_z) + max0(256 - (s IDmakeMaps_e_i)))%Q
    | 7%positive => ((s IDmakeMaps_e_z) + max0(256 - (s IDmakeMaps_e_i)))%Q
    | 8%positive => ((s IDmakeMaps_e_z))%Q
    | 9%positive => ((s IDmakeMaps_e_z) + max0(256 - (s IDmakeMaps_e_i)))%Q
    | 10%positive => ((1 # 1) + (s IDmakeMaps_e_z)
                      + max0(255 - (s IDmakeMaps_e_i)))%Q
    | 11%positive => ((1 # 1) + (s IDmakeMaps_e_z)
                      + max0(255 - (s IDmakeMaps_e_i)))%Q
    | 12%positive => ((1 # 1) + (s IDmakeMaps_e_z)
                      + max0(255 - (s IDmakeMaps_e_i)))%Q
    | 13%positive => ((1 # 1) + (s IDmakeMaps_e_z)
                      + max0(255 - (s IDmakeMaps_e_i)))%Q
    | 14%positive => ((1 # 1) + (s IDmakeMaps_e_z)
                      + max0(255 - (s IDmakeMaps_e_i)))%Q
    | 15%positive => ((1 # 1) + (s IDmakeMaps_e_z)
                      + max0(256 - (s IDmakeMaps_e_i)))%Q
    | 16%positive => ((1 # 1) + (s IDmakeMaps_e_z)
                      + max0(256 - (s IDmakeMaps_e_i)))%Q
    | 17%positive => ((1 # 1) + (s IDmakeMaps_e_z)
                      + max0(256 - (s IDmakeMaps_e_i)))%Q
    | 18%positive => ((s IDmakeMaps_e_z) + max0(256 - (s IDmakeMaps_e_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition makeMaps_e_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (256
                                                            - (s IDmakeMaps_e_i)) (255
                                                                    - (s IDmakeMaps_e_i)));
                     (*-1 0*) F_max0_ge_0 (255 - (s IDmakeMaps_e_i))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_pre_decrement (256 - (s IDmakeMaps_e_i)) (1)]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | _ => []
  end.


Theorem makeMaps_e_ai_correct:
  forall s p' s', steps (g_start makeMaps_e) s (g_edges makeMaps_e) p' s' -> makeMaps_e_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem makeMaps_e_pot_correct:
  forall s p' s',
    steps (g_start makeMaps_e) s (g_edges makeMaps_e) p' s' ->
    (makeMaps_e_pot (g_start makeMaps_e) s >= makeMaps_e_pot p' s')%Q.
Proof.
  check_lp makeMaps_e_ai_correct makeMaps_e_hints.
Qed.

