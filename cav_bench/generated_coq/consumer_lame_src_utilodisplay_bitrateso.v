Require Import pasta.Pasta.

Notation IDdisplay_bitrates_z := 1%positive.
Notation IDdisplay_bitrates_index := 2%positive.
Notation IDdisplay_bitrates_version := 3%positive.
Notation IDdisplay_bitrates_out_fh := 4%positive.
Definition display_bitrates : graph := {|
  g_start := 1%positive;
  g_end := 14%positive;
  g_edges := (1%positive,(AAssign IDdisplay_bitrates_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDdisplay_bitrates_version
             (Some (ENum (1)))),3%positive)::
             (3%positive,(AAssign IDdisplay_bitrates_index
             (Some (ENum (1)))),4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDdisplay_bitrates_index) s) <
             (eval (ENum (15)) s))%Z)),22%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDdisplay_bitrates_index) s) >=
             (eval (ENum (15)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDdisplay_bitrates_version
             (Some (ENum (0)))),9%positive)::
             (9%positive,(AAssign IDdisplay_bitrates_index
             (Some (ENum (1)))),10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDdisplay_bitrates_index) s) <
             (eval (ENum (15)) s))%Z)),15%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDdisplay_bitrates_index) s) >=
             (eval (ENum (15)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDdisplay_bitrates_index
             (Some (EAdd (EVar IDdisplay_bitrates_index) (ENum (1))))),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDdisplay_bitrates_z
             (Some (EAdd (ENum (1)) (EVar IDdisplay_bitrates_z)))),
             21%positive)::(21%positive,AWeaken,12%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDdisplay_bitrates_index
             (Some (EAdd (EVar IDdisplay_bitrates_index) (ENum (1))))),
             25%positive)::(25%positive,ANone,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDdisplay_bitrates_z
             (Some (EAdd (ENum (1)) (EVar IDdisplay_bitrates_z)))),
             28%positive)::(28%positive,AWeaken,6%positive)::nil
|}.

Definition display_bitrates_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0)%Z
    | 3%positive => (-1 * (s IDdisplay_bitrates_z) <= 0 /\ 1 * (s IDdisplay_bitrates_z) <= 0 /\ 1 * (s IDdisplay_bitrates_version) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_version) + 1 <= 0)%Z
    | 4%positive => (-1 * (s IDdisplay_bitrates_version) + 1 <= 0 /\ 1 * (s IDdisplay_bitrates_version) + -1 <= 0 /\ 1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 1 <= 0)%Z
    | 5%positive => (-1 * (s IDdisplay_bitrates_index) + 1 <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ 1 * (s IDdisplay_bitrates_z) <= 0 /\ 1 * (s IDdisplay_bitrates_version) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_version) + 1 <= 0)%Z
    | 6%positive => (-1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_version) + 1 <= 0 /\ 1 * (s IDdisplay_bitrates_version) + -1 <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -15 <= 0)%Z
    | 7%positive => (1 * (s IDdisplay_bitrates_index) + -15 <= 0 /\ 1 * (s IDdisplay_bitrates_version) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_version) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 15 <= 0)%Z
    | 8%positive => (-1 * (s IDdisplay_bitrates_index) + 15 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_version) + 1 <= 0 /\ 1 * (s IDdisplay_bitrates_version) + -1 <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -15 <= 0)%Z
    | 9%positive => (1 * (s IDdisplay_bitrates_index) + -15 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 15 <= 0 /\ 1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_version) <= 0)%Z
    | 10%positive => (-1 * (s IDdisplay_bitrates_version) <= 0 /\ 1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDdisplay_bitrates_index) + 1 <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ 1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_version) <= 0)%Z
    | 12%positive => (-1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_version) <= 0 /\ 1 * (s IDdisplay_bitrates_version) <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -15 <= 0)%Z
    | 13%positive => (1 * (s IDdisplay_bitrates_index) + -15 <= 0 /\ 1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 15 <= 0)%Z
    | 14%positive => (-1 * (s IDdisplay_bitrates_index) + 15 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_version) <= 0 /\ 1 * (s IDdisplay_bitrates_version) <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -15 <= 0)%Z
    | 15%positive => (1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -14 <= 0)%Z
    | 16%positive => (1 * (s IDdisplay_bitrates_index) + -14 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_version) <= 0 /\ 1 * (s IDdisplay_bitrates_version) <= 0)%Z
    | 17%positive => (1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -14 <= 0)%Z
    | 18%positive => (-1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_version) <= 0 /\ 1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 2 <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -15 <= 0)%Z
    | 19%positive => (1 * (s IDdisplay_bitrates_index) + -15 <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 2 <= 0 /\ 1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0)%Z
    | 20%positive => (-1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_version) <= 0 /\ 1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 2 <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -15 <= 0)%Z
    | 21%positive => (1 * (s IDdisplay_bitrates_index) + -15 <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 2 <= 0 /\ 1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_version) <= 0 /\ -1 * (s IDdisplay_bitrates_z) + 1 <= 0)%Z
    | 22%positive => (1 * (s IDdisplay_bitrates_version) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_version) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -14 <= 0)%Z
    | 23%positive => (1 * (s IDdisplay_bitrates_index) + -14 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_version) + 1 <= 0 /\ 1 * (s IDdisplay_bitrates_version) + -1 <= 0)%Z
    | 24%positive => (1 * (s IDdisplay_bitrates_version) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_version) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -14 <= 0)%Z
    | 25%positive => (-1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_version) + 1 <= 0 /\ 1 * (s IDdisplay_bitrates_version) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 2 <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -15 <= 0)%Z
    | 26%positive => (1 * (s IDdisplay_bitrates_index) + -15 <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 2 <= 0 /\ 1 * (s IDdisplay_bitrates_version) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_version) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_z) <= 0)%Z
    | 27%positive => (-1 * (s IDdisplay_bitrates_z) <= 0 /\ -1 * (s IDdisplay_bitrates_version) + 1 <= 0 /\ 1 * (s IDdisplay_bitrates_version) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 2 <= 0 /\ 1 * (s IDdisplay_bitrates_index) + -15 <= 0)%Z
    | 28%positive => (1 * (s IDdisplay_bitrates_index) + -15 <= 0 /\ -1 * (s IDdisplay_bitrates_index) + 2 <= 0 /\ 1 * (s IDdisplay_bitrates_version) + -1 <= 0 /\ -1 * (s IDdisplay_bitrates_version) + 1 <= 0 /\ -1 * (s IDdisplay_bitrates_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition display_bitrates_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((28 # 1))%Q
    | 2%positive => ((28 # 1) + (s IDdisplay_bitrates_z))%Q
    | 3%positive => ((28 # 1) + (s IDdisplay_bitrates_z))%Q
    | 4%positive => ((14 # 1) + (s IDdisplay_bitrates_z)
                     + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 5%positive => ((14 # 1) + (s IDdisplay_bitrates_z)
                     + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 6%positive => ((14 # 1) + (s IDdisplay_bitrates_z)
                     + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 7%positive => ((14 # 1) + (s IDdisplay_bitrates_z)
                     + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 8%positive => ((14 # 1) + (s IDdisplay_bitrates_z))%Q
    | 9%positive => ((14 # 1) + (s IDdisplay_bitrates_z))%Q
    | 10%positive => ((s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 11%positive => ((s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 12%positive => ((s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 13%positive => ((s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 14%positive => ((s IDdisplay_bitrates_z))%Q
    | 15%positive => ((s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 16%positive => ((1 # 1) + (s IDdisplay_bitrates_z)
                      + max0(14 - (s IDdisplay_bitrates_index)))%Q
    | 17%positive => ((1 # 1) + (s IDdisplay_bitrates_z)
                      + max0(14 - (s IDdisplay_bitrates_index)))%Q
    | 18%positive => ((1 # 1) + (s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 19%positive => ((1 # 1) + (s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 20%positive => ((1 # 1) + (s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 21%positive => ((s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 22%positive => ((14 # 1) + (s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 23%positive => ((15 # 1) + (s IDdisplay_bitrates_z)
                      + max0(14 - (s IDdisplay_bitrates_index)))%Q
    | 24%positive => ((15 # 1) + (s IDdisplay_bitrates_z)
                      + max0(14 - (s IDdisplay_bitrates_index)))%Q
    | 25%positive => ((15 # 1) + (s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 26%positive => ((15 # 1) + (s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 27%positive => ((15 # 1) + (s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | 28%positive => ((14 # 1) + (s IDdisplay_bitrates_z)
                      + max0(15 - (s IDdisplay_bitrates_index)))%Q
    | _ => (0 # 1)%Q
  end.

Definition display_bitrates_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (15
                                                            - (s IDdisplay_bitrates_index)) (14
                                                                    - (s IDdisplay_bitrates_index)));
                     (*-1 0*) F_max0_ge_0 (14 - (s IDdisplay_bitrates_index))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (15
                                                             - (s IDdisplay_bitrates_index)) (14
                                                                    - (s IDdisplay_bitrates_index)));
                      (*-1 0*) F_max0_ge_0 (14 - (s IDdisplay_bitrates_index))]
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_pre_decrement (15
                                                     - (s IDdisplay_bitrates_index)) (1)]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_max0_pre_decrement (15
                                                     - (s IDdisplay_bitrates_index)) (1)]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | _ => []
  end.


Theorem display_bitrates_ai_correct:
  forall s p' s', steps (g_start display_bitrates) s (g_edges display_bitrates) p' s' -> display_bitrates_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem display_bitrates_pot_correct:
  forall s p' s',
    steps (g_start display_bitrates) s (g_edges display_bitrates) p' s' ->
    (display_bitrates_pot (g_start display_bitrates) s >= display_bitrates_pot p' s')%Q.
Proof.
  check_lp display_bitrates_ai_correct display_bitrates_hints.
Qed.

