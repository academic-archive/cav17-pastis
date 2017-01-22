Require Import pasta.Pasta.

Notation IDmdct_short_z := 1%positive.
Notation IDmdct_short_l := 2%positive.
Notation IDmdct_short_m := 3%positive.
Notation IDmdct_short_in := 4%positive.
Notation IDmdct_short_out := 5%positive.
Definition mdct_short : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDmdct_short_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmdct_short_m (Some (ENum (5)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDmdct_short_m) s) >=
             (eval (ENum (0)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDmdct_short_m) s) <
             (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDmdct_short_l (Some (ENum (2)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDmdct_short_l)
             s) >= (eval (ENum (0)) s))%Z)),20%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDmdct_short_l) s) <
             (eval (ENum (0)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDmdct_short_m
             (Some (EAdd (EVar IDmdct_short_m) (ENum (-1))))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDmdct_short_z (Some (EAdd (ENum (1))
             (EVar IDmdct_short_z)))),19%positive)::
             (19%positive,AWeaken,5%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDmdct_short_l
             (Some (EAdd (EVar IDmdct_short_l) (ENum (-1))))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDmdct_short_z (Some (EAdd (ENum (1))
             (EVar IDmdct_short_z)))),26%positive)::
             (26%positive,AWeaken,12%positive)::nil
|}.

Definition mdct_short_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmdct_short_z) <= 0 /\ -1 * (s IDmdct_short_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0 /\ -1 * (s IDmdct_short_m) + 5 <= 0)%Z
    | 4%positive => (-1 * (s IDmdct_short_m) + 5 <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0 /\ 1 * (s IDmdct_short_z) <= 0 /\ -1 * (s IDmdct_short_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0)%Z
    | 6%positive => (-1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_m) + 1 <= 0)%Z
    | 7%positive => (1 * (s IDmdct_short_m) + 1 <= 0 /\ -1 * (s IDmdct_short_z) <= 0)%Z
    | 8%positive => (1 * (s IDmdct_short_m) + -5 <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ -1 * (s IDmdct_short_m) <= 0)%Z
    | 9%positive => (-1 * (s IDmdct_short_m) <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0)%Z
    | 10%positive => (1 * (s IDmdct_short_m) + -5 <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ -1 * (s IDmdct_short_m) <= 0 /\ 1 * (s IDmdct_short_l) + -2 <= 0 /\ -1 * (s IDmdct_short_l) + 2 <= 0)%Z
    | 11%positive => (-1 * (s IDmdct_short_l) + 2 <= 0 /\ 1 * (s IDmdct_short_l) + -2 <= 0 /\ -1 * (s IDmdct_short_m) <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0)%Z
    | 12%positive => (-1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_l) + -2 <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0 /\ -1 * (s IDmdct_short_l) + -1 <= 0)%Z
    | 13%positive => (-1 * (s IDmdct_short_l) + -1 <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_l) + 1 <= 0)%Z
    | 14%positive => (1 * (s IDmdct_short_l) + 1 <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0 /\ -1 * (s IDmdct_short_l) + -1 <= 0)%Z
    | 15%positive => (-1 * (s IDmdct_short_l) + -1 <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_l) + 1 <= 0)%Z
    | 16%positive => (1 * (s IDmdct_short_l) + 1 <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ -1 * (s IDmdct_short_l) + -1 <= 0 /\ 1 * (s IDmdct_short_m) + -4 <= 0)%Z
    | 17%positive => (1 * (s IDmdct_short_m) + -4 <= 0 /\ -1 * (s IDmdct_short_l) + -1 <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_l) + 1 <= 0)%Z
    | 18%positive => (1 * (s IDmdct_short_l) + 1 <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ -1 * (s IDmdct_short_l) + -1 <= 0 /\ 1 * (s IDmdct_short_m) + -4 <= 0)%Z
    | 19%positive => (1 * (s IDmdct_short_m) + -4 <= 0 /\ -1 * (s IDmdct_short_l) + -1 <= 0 /\ 1 * (s IDmdct_short_l) + 1 <= 0 /\ -1 * (s IDmdct_short_z) + 1 <= 0)%Z
    | 20%positive => (1 * (s IDmdct_short_m) + -5 <= 0 /\ 1 * (s IDmdct_short_l) + -2 <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ -1 * (s IDmdct_short_l) <= 0)%Z
    | 21%positive => (-1 * (s IDmdct_short_l) <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_l) + -2 <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0)%Z
    | 22%positive => (1 * (s IDmdct_short_m) + -5 <= 0 /\ 1 * (s IDmdct_short_l) + -2 <= 0 /\ -1 * (s IDmdct_short_z) <= 0 /\ -1 * (s IDmdct_short_l) <= 0)%Z
    | 23%positive => (-1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0 /\ 1 * (s IDmdct_short_l) + -1 <= 0 /\ -1 * (s IDmdct_short_l) + -1 <= 0)%Z
    | 24%positive => (-1 * (s IDmdct_short_l) + -1 <= 0 /\ 1 * (s IDmdct_short_l) + -1 <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0 /\ -1 * (s IDmdct_short_z) <= 0)%Z
    | 25%positive => (-1 * (s IDmdct_short_z) <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0 /\ 1 * (s IDmdct_short_l) + -1 <= 0 /\ -1 * (s IDmdct_short_l) + -1 <= 0)%Z
    | 26%positive => (-1 * (s IDmdct_short_l) + -1 <= 0 /\ 1 * (s IDmdct_short_l) + -1 <= 0 /\ 1 * (s IDmdct_short_m) + -5 <= 0 /\ -1 * (s IDmdct_short_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition mdct_short_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((24 # 1))%Q
    | 2%positive => ((24 # 1) + (s IDmdct_short_z))%Q
    | 3%positive => ((s IDmdct_short_z)
                     + (4 # 1) * max0(1 + (s IDmdct_short_m)))%Q
    | 4%positive => ((s IDmdct_short_z)
                     + (4 # 1) * max0(1 + (s IDmdct_short_m)))%Q
    | 5%positive => ((s IDmdct_short_z)
                     + (4 # 1) * max0(1 + (s IDmdct_short_m)))%Q
    | 6%positive => ((s IDmdct_short_z)
                     + (4 # 1) * max0(1 + (s IDmdct_short_m)))%Q
    | 7%positive => ((s IDmdct_short_z))%Q
    | 8%positive => ((s IDmdct_short_z)
                     + (4 # 1) * max0(1 + (s IDmdct_short_m)))%Q
    | 9%positive => ((s IDmdct_short_z)
                     + (4 # 1) * max0(1 + (s IDmdct_short_m)))%Q
    | 10%positive => (-(3 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0(1 + (s IDmdct_short_m)))%Q
    | 11%positive => (-(3 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0(1 + (s IDmdct_short_m)))%Q
    | 12%positive => ((1 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0((s IDmdct_short_m)))%Q
    | 13%positive => ((1 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0((s IDmdct_short_m)))%Q
    | 14%positive => ((1 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0((s IDmdct_short_m)))%Q
    | 15%positive => ((1 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0((s IDmdct_short_m)))%Q
    | 16%positive => ((1 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0(1 + (s IDmdct_short_m)))%Q
    | 17%positive => ((1 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0(1 + (s IDmdct_short_m)))%Q
    | 18%positive => ((1 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0(1 + (s IDmdct_short_m)))%Q
    | 19%positive => ((s IDmdct_short_z) + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0(1 + (s IDmdct_short_m)))%Q
    | 20%positive => ((1 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0((s IDmdct_short_m)))%Q
    | 21%positive => ((2 # 1) + (s IDmdct_short_z) + max0((s IDmdct_short_l))
                      + (4 # 1) * max0((s IDmdct_short_m)))%Q
    | 22%positive => ((2 # 1) + (s IDmdct_short_z) + max0((s IDmdct_short_l))
                      + (4 # 1) * max0((s IDmdct_short_m)))%Q
    | 23%positive => ((2 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0((s IDmdct_short_m)))%Q
    | 24%positive => ((2 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0((s IDmdct_short_m)))%Q
    | 25%positive => ((2 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0((s IDmdct_short_m)))%Q
    | 26%positive => ((1 # 1) + (s IDmdct_short_z)
                      + max0(1 + (s IDmdct_short_l))
                      + (4 # 1) * max0((s IDmdct_short_m)))%Q
    | _ => (0 # 1)%Q
  end.

Definition mdct_short_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-4 0*) F_max0_monotonic (F_check_ge (1
                                                            + (s IDmdct_short_m)) ((s IDmdct_short_m)));
                     (*-4 0*) F_max0_ge_0 ((s IDmdct_short_m))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*0 4*) F_max0_pre_decrement (1 + (s IDmdct_short_m)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*0 1*) F_max0_ge_0 (1 + (s IDmdct_short_l))]
    | 20%positive => [(*-1 0*) F_max0_pre_decrement (1 + (s IDmdct_short_l)) (1)]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | _ => []
  end.


Theorem mdct_short_ai_correct:
  forall s p' s', steps (g_start mdct_short) s (g_edges mdct_short) p' s' -> mdct_short_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mdct_short_pot_correct:
  forall s p' s',
    steps (g_start mdct_short) s (g_edges mdct_short) p' s' ->
    (mdct_short_pot (g_start mdct_short) s >= mdct_short_pot p' s')%Q.
Proof.
  check_lp mdct_short_ai_correct mdct_short_hints.
Qed.

