Require Import pasta.Pasta.

Notation IDTIFFUnlinkDirectory_z := 1%positive.
Notation IDTIFFUnlinkDirectory__tmp := 2%positive.
Notation IDTIFFUnlinkDirectory__tmp1 := 3%positive.
Notation IDTIFFUnlinkDirectory_n := 4%positive.
Notation IDTIFFUnlinkDirectory_dirn := 5%positive.
Notation IDTIFFUnlinkDirectory_tif := 6%positive.
Definition TIFFUnlinkDirectory : graph := {|
  g_start := 1%positive;
  g_end := 50%positive;
  g_edges := (1%positive,(AAssign IDTIFFUnlinkDirectory_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDTIFFUnlinkDirectory__tmp1
             (Some (EVar IDTIFFUnlinkDirectory_dirn))),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,ANone,47%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDTIFFUnlinkDirectory_n
             (Some (ESub (EVar IDTIFFUnlinkDirectory__tmp1) (ENum (1))))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFUnlinkDirectory_n) s) >
             (eval (ENum (0)) s))%Z)),31%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFUnlinkDirectory_n) s) <=
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,14%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDTIFFUnlinkDirectory__tmp
             (Some (ENum (0)))),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,50%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,17%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,19%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,23%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDTIFFUnlinkDirectory__tmp
             (Some (ENum (0)))),21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,50%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,25%positive)::
             (24%positive,ANone,28%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (26%positive,ANone,28%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDTIFFUnlinkDirectory__tmp
             (Some (ENum (1)))),29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,50%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,44%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,38%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDTIFFUnlinkDirectory__tmp
             (Some (ENum (0)))),36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,AWeaken,50%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDTIFFUnlinkDirectory_n
             (Some (EAdd (EVar IDTIFFUnlinkDirectory_n) (ENum (-1))))),
             40%positive)::(40%positive,ANone,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDTIFFUnlinkDirectory_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFUnlinkDirectory_z)))),
             43%positive)::(43%positive,AWeaken,8%positive)::
             (44%positive,(AAssign IDTIFFUnlinkDirectory__tmp
             (Some (ENum (0)))),45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,AWeaken,50%positive)::
             (47%positive,(AAssign IDTIFFUnlinkDirectory__tmp
             (Some (ENum (0)))),48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,AWeaken,50%positive)::nil
|}.

Definition TIFFUnlinkDirectory_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 3%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 4%positive => (1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 5%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 6%positive => (1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 7%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 8%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 9%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 10%positive => (1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 11%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 12%positive => (1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory__tmp) <= 0)%Z
    | 13%positive => (-1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 14%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 15%positive => (1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 16%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 17%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 18%positive => (1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 19%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 20%positive => (1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 21%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory__tmp) <= 0)%Z
    | 22%positive => (-1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 23%positive => (1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 24%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 25%positive => (1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 26%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 27%positive => (1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 28%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 29%positive => (1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory__tmp) + -1 <= 0 /\ -1 * (s IDTIFFUnlinkDirectory__tmp) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDTIFFUnlinkDirectory__tmp) + 1 <= 0 /\ 1 * (s IDTIFFUnlinkDirectory__tmp) + -1 <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 31%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_n) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDTIFFUnlinkDirectory_n) + 1 <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 33%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_n) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDTIFFUnlinkDirectory_n) + 1 <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 35%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_n) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDTIFFUnlinkDirectory_n) + 1 <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory__tmp) <= 0)%Z
    | 37%positive => (-1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_n) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_n) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDTIFFUnlinkDirectory_n) + 1 <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 40%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 41%positive => (-1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 42%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_n) <= 0)%Z
    | 43%positive => (-1 * (s IDTIFFUnlinkDirectory_n) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) + 1 <= 0)%Z
    | 44%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_n) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDTIFFUnlinkDirectory_n) + 1 <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory__tmp) <= 0)%Z
    | 46%positive => (-1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_n) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 48%positive => (1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory__tmp) <= 0)%Z
    | 49%positive => (-1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory__tmp) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ 1 * (s IDTIFFUnlinkDirectory_z) <= 0)%Z
    | 50%positive => (1 * (s IDTIFFUnlinkDirectory__tmp) + -1 <= 0 /\ -1 * (s IDTIFFUnlinkDirectory_z) <= 0 /\ -1 * (s IDTIFFUnlinkDirectory__tmp) <= 0)%Z
    | _ => False
  end.

Definition TIFFUnlinkDirectory_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDTIFFUnlinkDirectory_dirn)))%Q
    | 2%positive => ((s IDTIFFUnlinkDirectory_z)
                     + max0(-1 + (s IDTIFFUnlinkDirectory_dirn)))%Q
    | 3%positive => ((s IDTIFFUnlinkDirectory_z)
                     + max0(-1 + (s IDTIFFUnlinkDirectory__tmp1)))%Q
    | 4%positive => ((s IDTIFFUnlinkDirectory_z)
                     + max0(-1 + (s IDTIFFUnlinkDirectory__tmp1)))%Q
    | 5%positive => ((s IDTIFFUnlinkDirectory_z)
                     + max0(-1 + (s IDTIFFUnlinkDirectory__tmp1)))%Q
    | 6%positive => ((s IDTIFFUnlinkDirectory_z)
                     + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 7%positive => ((s IDTIFFUnlinkDirectory_z)
                     + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 8%positive => ((s IDTIFFUnlinkDirectory_z)
                     + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 9%positive => ((s IDTIFFUnlinkDirectory_z)
                     + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 10%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 11%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 12%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 13%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 14%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 15%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 16%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 17%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 18%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 19%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 20%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 21%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 22%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 23%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 24%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 25%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 26%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 27%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 28%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 29%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 30%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 31%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 32%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 33%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 34%positive => ((s IDTIFFUnlinkDirectory_n)
                      + (s IDTIFFUnlinkDirectory_z))%Q
    | 35%positive => ((s IDTIFFUnlinkDirectory_n)
                      + (s IDTIFFUnlinkDirectory_z))%Q
    | 36%positive => ((s IDTIFFUnlinkDirectory_n)
                      + (s IDTIFFUnlinkDirectory_z))%Q
    | 37%positive => ((s IDTIFFUnlinkDirectory_n)
                      + (s IDTIFFUnlinkDirectory_z))%Q
    | 38%positive => ((s IDTIFFUnlinkDirectory_n)
                      + (s IDTIFFUnlinkDirectory_z))%Q
    | 39%positive => ((s IDTIFFUnlinkDirectory_n)
                      + (s IDTIFFUnlinkDirectory_z))%Q
    | 40%positive => ((1 # 1) + (s IDTIFFUnlinkDirectory_n)
                      + (s IDTIFFUnlinkDirectory_z))%Q
    | 41%positive => ((1 # 1) + (s IDTIFFUnlinkDirectory_n)
                      + (s IDTIFFUnlinkDirectory_z))%Q
    | 42%positive => ((1 # 1) + (s IDTIFFUnlinkDirectory_n)
                      + (s IDTIFFUnlinkDirectory_z))%Q
    | 43%positive => ((s IDTIFFUnlinkDirectory_n)
                      + (s IDTIFFUnlinkDirectory_z))%Q
    | 44%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 45%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 46%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0((s IDTIFFUnlinkDirectory_n)))%Q
    | 47%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0(-1 + (s IDTIFFUnlinkDirectory__tmp1)))%Q
    | 48%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0(-1 + (s IDTIFFUnlinkDirectory__tmp1)))%Q
    | 49%positive => ((s IDTIFFUnlinkDirectory_z)
                      + max0(-1 + (s IDTIFFUnlinkDirectory__tmp1)))%Q
    | 50%positive => ((s IDTIFFUnlinkDirectory_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition TIFFUnlinkDirectory_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFUnlinkDirectory_n)) (-1
                                                                    + (s IDTIFFUnlinkDirectory_n)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFUnlinkDirectory_n))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFUnlinkDirectory_n)) (-1
                                                                    + (s IDTIFFUnlinkDirectory_n)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFUnlinkDirectory_n))]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFUnlinkDirectory_n)) (-1
                                                                    + (s IDTIFFUnlinkDirectory_n)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFUnlinkDirectory_n))]
    | 31%positive => []
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTIFFUnlinkDirectory_n))) (F_check_ge ((s IDTIFFUnlinkDirectory_n)) (0))]
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDTIFFUnlinkDirectory_n)) (1);
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFUnlinkDirectory_n));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDTIFFUnlinkDirectory_n)) (0))) (F_max0_ge_0 ((s IDTIFFUnlinkDirectory_n)))]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDTIFFUnlinkDirectory_n)) (0))) (F_max0_ge_0 ((s IDTIFFUnlinkDirectory_n)))]
    | 44%positive => []
    | 45%positive => []
    | 46%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDTIFFUnlinkDirectory_n)) (1);
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFUnlinkDirectory_n))]
    | 47%positive => []
    | 48%positive => []
    | 49%positive => [(*-1 0*) F_max0_ge_0 (-1
                                            + (s IDTIFFUnlinkDirectory__tmp1))]
    | 50%positive => []
    | _ => []
  end.


Theorem TIFFUnlinkDirectory_ai_correct:
  forall s p' s', steps (g_start TIFFUnlinkDirectory) s (g_edges TIFFUnlinkDirectory) p' s' -> TIFFUnlinkDirectory_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem TIFFUnlinkDirectory_pot_correct:
  forall s p' s',
    steps (g_start TIFFUnlinkDirectory) s (g_edges TIFFUnlinkDirectory) p' s' ->
    (TIFFUnlinkDirectory_pot (g_start TIFFUnlinkDirectory) s >= TIFFUnlinkDirectory_pot p' s')%Q.
Proof.
  check_lp TIFFUnlinkDirectory_ai_correct TIFFUnlinkDirectory_hints.
Qed.

