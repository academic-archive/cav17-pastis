Require Import pasta.Pasta.

Notation IDprepare_new_block_z := 1%positive.
Notation IDprepare_new_block_i := 2%positive.
Notation IDprepare_new_block_s_dref_off108 := 3%positive.
Notation IDprepare_new_block_s_dref_off116 := 4%positive.
Notation IDprepare_new_block_s_dref_off120 := 5%positive.
Notation IDprepare_new_block_s_dref_off648 := 6%positive.
Notation IDprepare_new_block_s_dref_off660 := 7%positive.
Notation IDprepare_new_block_s := 8%positive.
Definition prepare_new_block : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDprepare_new_block_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDprepare_new_block_s_dref_off108
             (Some (ENum (0)))),3%positive)::
             (3%positive,(AAssign IDprepare_new_block_s_dref_off116
             (Some (ENum (0)))),4%positive)::
             (4%positive,(AAssign IDprepare_new_block_s_dref_off120
             (Some (ENum (0)))),5%positive)::
             (5%positive,(AAssign IDprepare_new_block_s_dref_off648
             (Some (ENum (-1)))),6%positive)::
             (6%positive,(AAssign IDprepare_new_block_i (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDprepare_new_block_i) s) <
             (eval (ENum (256)) s))%Z)),14%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDprepare_new_block_i) s) >=
             (eval (ENum (256)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDprepare_new_block_s_dref_off660
             (Some (EAdd (EVar IDprepare_new_block_s_dref_off660)
             (ENum (1))))),12%positive)::(12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDprepare_new_block_i
             (Some (EAdd (EVar IDprepare_new_block_i) (ENum (1))))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDprepare_new_block_z
             (Some (EAdd (ENum (1)) (EVar IDprepare_new_block_z)))),
             20%positive)::(20%positive,AWeaken,9%positive)::nil
|}.

Definition prepare_new_block_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDprepare_new_block_z) <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0)%Z
    | 3%positive => (-1 * (s IDprepare_new_block_z) <= 0 /\ 1 * (s IDprepare_new_block_z) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0)%Z
    | 4%positive => (-1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_z) <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0)%Z
    | 5%positive => (-1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0 /\ 1 * (s IDprepare_new_block_z) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0)%Z
    | 6%positive => (-1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_z) <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0)%Z
    | 7%positive => (-1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0 /\ 1 * (s IDprepare_new_block_z) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ 1 * (s IDprepare_new_block_i) <= 0 /\ -1 * (s IDprepare_new_block_i) <= 0)%Z
    | 8%positive => (-1 * (s IDprepare_new_block_i) <= 0 /\ 1 * (s IDprepare_new_block_i) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_z) <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0)%Z
    | 9%positive => (-1 * (s IDprepare_new_block_z) <= 0 /\ -1 * (s IDprepare_new_block_i) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ 1 * (s IDprepare_new_block_i) + -256 <= 0)%Z
    | 10%positive => (1 * (s IDprepare_new_block_i) + -256 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0 /\ -1 * (s IDprepare_new_block_i) + 256 <= 0)%Z
    | 11%positive => (-1 * (s IDprepare_new_block_i) + 256 <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ 1 * (s IDprepare_new_block_i) + -256 <= 0)%Z
    | 12%positive => (1 * (s IDprepare_new_block_i) + -256 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0 /\ -1 * (s IDprepare_new_block_i) + 256 <= 0)%Z
    | 13%positive => (-1 * (s IDprepare_new_block_i) + 256 <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ 1 * (s IDprepare_new_block_i) + -256 <= 0)%Z
    | 14%positive => (-1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ -1 * (s IDprepare_new_block_i) <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0 /\ 1 * (s IDprepare_new_block_i) + -255 <= 0)%Z
    | 15%positive => (1 * (s IDprepare_new_block_i) + -255 <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0 /\ -1 * (s IDprepare_new_block_i) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0)%Z
    | 16%positive => (-1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ -1 * (s IDprepare_new_block_i) <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0 /\ 1 * (s IDprepare_new_block_i) + -255 <= 0)%Z
    | 17%positive => (-1 * (s IDprepare_new_block_z) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_i) + 1 <= 0 /\ 1 * (s IDprepare_new_block_i) + -256 <= 0)%Z
    | 18%positive => (1 * (s IDprepare_new_block_i) + -256 <= 0 /\ -1 * (s IDprepare_new_block_i) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ -1 * (s IDprepare_new_block_z) <= 0)%Z
    | 19%positive => (-1 * (s IDprepare_new_block_z) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_i) + 1 <= 0 /\ 1 * (s IDprepare_new_block_i) + -256 <= 0)%Z
    | 20%positive => (1 * (s IDprepare_new_block_i) + -256 <= 0 /\ -1 * (s IDprepare_new_block_i) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off120) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off108) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off116) <= 0 /\ 1 * (s IDprepare_new_block_s_dref_off648) + 1 <= 0 /\ -1 * (s IDprepare_new_block_s_dref_off648) + -1 <= 0 /\ -1 * (s IDprepare_new_block_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition prepare_new_block_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((256 # 1))%Q
    | 2%positive => ((256 # 1) + (s IDprepare_new_block_z))%Q
    | 3%positive => ((256 # 1) + (s IDprepare_new_block_z))%Q
    | 4%positive => ((256 # 1) + (s IDprepare_new_block_z))%Q
    | 5%positive => ((256 # 1) + (s IDprepare_new_block_z))%Q
    | 6%positive => ((256 # 1) + (s IDprepare_new_block_z))%Q
    | 7%positive => ((s IDprepare_new_block_z)
                     + max0(256 - (s IDprepare_new_block_i)))%Q
    | 8%positive => ((s IDprepare_new_block_z)
                     + max0(256 - (s IDprepare_new_block_i)))%Q
    | 9%positive => ((s IDprepare_new_block_z)
                     + max0(256 - (s IDprepare_new_block_i)))%Q
    | 10%positive => ((s IDprepare_new_block_z)
                      + max0(256 - (s IDprepare_new_block_i)))%Q
    | 11%positive => ((s IDprepare_new_block_z)
                      + max0(256 - (s IDprepare_new_block_i)))%Q
    | 12%positive => ((s IDprepare_new_block_z)
                      + max0(256 - (s IDprepare_new_block_i)))%Q
    | 13%positive => ((s IDprepare_new_block_z))%Q
    | 14%positive => ((s IDprepare_new_block_z)
                      + max0(256 - (s IDprepare_new_block_i)))%Q
    | 15%positive => ((1 # 1) + (s IDprepare_new_block_z)
                      + max0(255 - (s IDprepare_new_block_i)))%Q
    | 16%positive => ((1 # 1) + (s IDprepare_new_block_z)
                      + max0(255 - (s IDprepare_new_block_i)))%Q
    | 17%positive => ((1 # 1) + (s IDprepare_new_block_z)
                      + max0(256 - (s IDprepare_new_block_i)))%Q
    | 18%positive => ((1 # 1) + (s IDprepare_new_block_z)
                      + max0(256 - (s IDprepare_new_block_i)))%Q
    | 19%positive => ((1 # 1) + (s IDprepare_new_block_z)
                      + max0(256 - (s IDprepare_new_block_i)))%Q
    | 20%positive => ((s IDprepare_new_block_z)
                      + max0(256 - (s IDprepare_new_block_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition prepare_new_block_hints (p : node) (s : state) := 
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
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (256
                                                             - (s IDprepare_new_block_i)) (255
                                                                    - (s IDprepare_new_block_i)));
                      (*-1 0*) F_max0_ge_0 (255 - (s IDprepare_new_block_i))]
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_pre_decrement (256
                                                     - (s IDprepare_new_block_i)) (1)]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | _ => []
  end.


Theorem prepare_new_block_ai_correct:
  forall s p' s', steps (g_start prepare_new_block) s (g_edges prepare_new_block) p' s' -> prepare_new_block_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem prepare_new_block_pot_correct:
  forall s p' s',
    steps (g_start prepare_new_block) s (g_edges prepare_new_block) p' s' ->
    (prepare_new_block_pot (g_start prepare_new_block) s >= prepare_new_block_pot p' s')%Q.
Proof.
  check_lp prepare_new_block_ai_correct prepare_new_block_hints.
Qed.

