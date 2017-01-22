Require Import pasta.Pasta.

Notation IDIII_freqinver_z := 1%positive.
Notation IDIII_freqinver__tmp := 2%positive.
Notation IDIII_freqinver_i := 3%positive.
Notation IDIII_freqinver_tmp1 := 4%positive.
Notation IDIII_freqinver_tmp2 := 5%positive.
Notation IDIII_freqinver_sample := 6%positive.
Notation IDIII_freqinver_sb := 7%positive.
Definition III_freqinver : graph := {|
  g_start := 1%positive;
  g_end := 14%positive;
  g_edges := (1%positive,(AAssign IDIII_freqinver_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDIII_freqinver_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDIII_freqinver__tmp
             (Some (EVar IDIII_freqinver_sb))),5%positive)::
             (5%positive,(AAssign IDIII_freqinver_tmp1 None),6%positive)::
             (6%positive,(AAssign IDIII_freqinver_tmp2 None),7%positive)::
             (7%positive,(AAssign IDIII_freqinver_i (Some (ENum (1)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDIII_freqinver_i)
             s) < (eval (ENum (13)) s))%Z)),15%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDIII_freqinver_i)
             s) >= (eval (ENum (13)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDIII_freqinver_tmp1 None),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AAssign IDIII_freqinver_tmp1 None),17%positive)::
             (17%positive,(AAssign IDIII_freqinver_tmp2 None),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDIII_freqinver_i
             (Some (EAdd (EVar IDIII_freqinver_i) (ENum (4))))),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDIII_freqinver_z (Some (EAdd (ENum (1))
             (EVar IDIII_freqinver_z)))),23%positive)::
             (23%positive,AWeaken,10%positive)::nil
|}.

Definition III_freqinver_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0)%Z
    | 3%positive => (-1 * (s IDIII_freqinver_z) <= 0 /\ 1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_i) <= 0)%Z
    | 4%positive => (-1 * (s IDIII_freqinver_i) <= 0 /\ 1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0)%Z
    | 5%positive => (-1 * (s IDIII_freqinver_z) <= 0 /\ 1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_i) <= 0)%Z
    | 6%positive => (-1 * (s IDIII_freqinver_i) <= 0 /\ 1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0)%Z
    | 7%positive => (-1 * (s IDIII_freqinver_z) <= 0 /\ 1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_i) <= 0)%Z
    | 8%positive => (1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0 /\ 1 * (s IDIII_freqinver_i) + -1 <= 0 /\ -1 * (s IDIII_freqinver_i) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDIII_freqinver_i) + 1 <= 0 /\ 1 * (s IDIII_freqinver_i) + -1 <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0 /\ 1 * (s IDIII_freqinver_z) <= 0)%Z
    | 10%positive => (-1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_i) + 1 <= 0 /\ 1 * (s IDIII_freqinver_i) + -16 <= 0)%Z
    | 11%positive => (1 * (s IDIII_freqinver_i) + -16 <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_i) + 13 <= 0)%Z
    | 12%positive => (-1 * (s IDIII_freqinver_i) + 13 <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0 /\ 1 * (s IDIII_freqinver_i) + -16 <= 0)%Z
    | 13%positive => (1 * (s IDIII_freqinver_i) + -16 <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_i) + 13 <= 0)%Z
    | 14%positive => (-1 * (s IDIII_freqinver_i) + 13 <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0 /\ 1 * (s IDIII_freqinver_i) + -16 <= 0)%Z
    | 15%positive => (-1 * (s IDIII_freqinver_i) + 1 <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0 /\ 1 * (s IDIII_freqinver_i) + -12 <= 0)%Z
    | 16%positive => (1 * (s IDIII_freqinver_i) + -12 <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDIII_freqinver_i) + 1 <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0 /\ 1 * (s IDIII_freqinver_i) + -12 <= 0)%Z
    | 18%positive => (1 * (s IDIII_freqinver_i) + -12 <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDIII_freqinver_i) + 1 <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0 /\ 1 * (s IDIII_freqinver_i) + -12 <= 0)%Z
    | 20%positive => (-1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_i) + 5 <= 0 /\ 1 * (s IDIII_freqinver_i) + -16 <= 0)%Z
    | 21%positive => (1 * (s IDIII_freqinver_i) + -16 <= 0 /\ -1 * (s IDIII_freqinver_i) + 5 <= 0 /\ -1 * (s IDIII_freqinver_z) <= 0)%Z
    | 22%positive => (-1 * (s IDIII_freqinver_z) <= 0 /\ -1 * (s IDIII_freqinver_i) + 5 <= 0 /\ 1 * (s IDIII_freqinver_i) + -16 <= 0)%Z
    | 23%positive => (1 * (s IDIII_freqinver_i) + -16 <= 0 /\ -1 * (s IDIII_freqinver_i) + 5 <= 0 /\ -1 * (s IDIII_freqinver_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition III_freqinver_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((15 # 4))%Q
    | 2%positive => ((15 # 4) + (s IDIII_freqinver_z))%Q
    | 3%positive => ((15 # 4) + (s IDIII_freqinver_z))%Q
    | 4%positive => ((15 # 4) + (s IDIII_freqinver_z))%Q
    | 5%positive => ((15 # 4) + (s IDIII_freqinver_z))%Q
    | 6%positive => ((15 # 4) + (s IDIII_freqinver_z))%Q
    | 7%positive => ((15 # 4) + (s IDIII_freqinver_z))%Q
    | 8%positive => ((4 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                     + (s IDIII_freqinver_z))%Q
    | 9%positive => ((4 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                     + (s IDIII_freqinver_z))%Q
    | 10%positive => ((4 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | 11%positive => ((4 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | 12%positive => ((4 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | 13%positive => ((4 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | 14%positive => ((s IDIII_freqinver_z))%Q
    | 15%positive => ((4 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | 16%positive => ((4 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | 17%positive => ((4 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | 18%positive => ((4 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | 19%positive => ((4 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | 20%positive => ((5 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | 21%positive => ((5 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | 22%positive => ((5 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | 23%positive => ((4 # 1) - (1 # 4) * (s IDIII_freqinver_i)
                      + (s IDIII_freqinver_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition III_freqinver_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-0.25 0*) F_max0_monotonic (F_check_ge (16
                                                                - (s IDIII_freqinver_i)) (12
                                                                    - (s IDIII_freqinver_i)));
                      (*-0.25 0*) F_max0_ge_0 (12 - (s IDIII_freqinver_i));
                      (*0 0.25*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                    - (s IDIII_freqinver_i)) (0))) (F_max0_ge_0 (16
                                                                    - (s IDIII_freqinver_i)))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | _ => []
  end.


Theorem III_freqinver_ai_correct:
  forall s p' s', steps (g_start III_freqinver) s (g_edges III_freqinver) p' s' -> III_freqinver_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem III_freqinver_pot_correct:
  forall s p' s',
    steps (g_start III_freqinver) s (g_edges III_freqinver) p' s' ->
    (III_freqinver_pot (g_start III_freqinver) s >= III_freqinver_pot p' s')%Q.
Proof.
  check_lp III_freqinver_ai_correct III_freqinver_hints.
Qed.

