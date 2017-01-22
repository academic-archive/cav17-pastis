Require Import pasta.Pasta.

Notation IDTIFFSetDirectory_z := 1%positive.
Notation IDTIFFSetDirectory__tmp := 2%positive.
Notation IDTIFFSetDirectory__tmp1 := 3%positive.
Notation IDTIFFSetDirectory_n := 4%positive.
Notation IDTIFFSetDirectory_dirn := 5%positive.
Notation IDTIFFSetDirectory_tif := 6%positive.
Definition TIFFSetDirectory : graph := {|
  g_start := 1%positive;
  g_end := 21%positive;
  g_edges := (1%positive,(AAssign IDTIFFSetDirectory_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDTIFFSetDirectory__tmp1
             (Some (EVar IDTIFFSetDirectory_dirn))),3%positive)::
             (3%positive,(AAssign IDTIFFSetDirectory_n
             (Some (EVar IDTIFFSetDirectory__tmp1))),4%positive)::
             (4%positive,ANone,5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDTIFFSetDirectory_n)
             s) > (eval (ENum (0)) s))%Z)),8%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDTIFFSetDirectory_n)
             s) <= (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,13%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => True)),16%positive)::
             (11%positive,(AGuard (fun s => True)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDTIFFSetDirectory__tmp None),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,21%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,22%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDTIFFSetDirectory__tmp
             (Some (ENum (0)))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDTIFFSetDirectory_n
             (Some (EAdd (EVar IDTIFFSetDirectory_n) (ENum (-1))))),
             24%positive)::(24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDTIFFSetDirectory_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFSetDirectory_z)))),
             27%positive)::(27%positive,AWeaken,6%positive)::nil
|}.

Definition TIFFSetDirectory_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDTIFFSetDirectory_z) <= 0 /\ -1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 3%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0 /\ 1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 4%positive => (1 * (s IDTIFFSetDirectory_z) <= 0 /\ -1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 5%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0 /\ 1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 6%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 7%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0 /\ 1 * (s IDTIFFSetDirectory_n) <= 0)%Z
    | 8%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0 /\ -1 * (s IDTIFFSetDirectory_n) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDTIFFSetDirectory_n) + 1 <= 0 /\ -1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 10%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0 /\ -1 * (s IDTIFFSetDirectory_n) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDTIFFSetDirectory_n) + 1 <= 0 /\ -1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 12%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0 /\ -1 * (s IDTIFFSetDirectory_n) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 14%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 15%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 16%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0 /\ -1 * (s IDTIFFSetDirectory_n) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDTIFFSetDirectory_n) + 1 <= 0 /\ -1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 18%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0 /\ -1 * (s IDTIFFSetDirectory_n) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDTIFFSetDirectory_n) + 1 <= 0 /\ -1 * (s IDTIFFSetDirectory_z) <= 0 /\ 1 * (s IDTIFFSetDirectory__tmp) <= 0 /\ -1 * (s IDTIFFSetDirectory__tmp) <= 0)%Z
    | 20%positive => (-1 * (s IDTIFFSetDirectory__tmp) <= 0 /\ 1 * (s IDTIFFSetDirectory__tmp) <= 0 /\ -1 * (s IDTIFFSetDirectory_z) <= 0 /\ -1 * (s IDTIFFSetDirectory_n) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 22%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0 /\ -1 * (s IDTIFFSetDirectory_n) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDTIFFSetDirectory_n) + 1 <= 0 /\ -1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 24%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0 /\ -1 * (s IDTIFFSetDirectory_n) <= 0)%Z
    | 25%positive => (-1 * (s IDTIFFSetDirectory_n) <= 0 /\ -1 * (s IDTIFFSetDirectory_z) <= 0)%Z
    | 26%positive => (-1 * (s IDTIFFSetDirectory_z) <= 0 /\ -1 * (s IDTIFFSetDirectory_n) <= 0)%Z
    | 27%positive => (-1 * (s IDTIFFSetDirectory_n) <= 0 /\ -1 * (s IDTIFFSetDirectory_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition TIFFSetDirectory_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDTIFFSetDirectory_dirn)))%Q
    | 2%positive => (max0((s IDTIFFSetDirectory_dirn))
                     + max0((s IDTIFFSetDirectory_z)))%Q
    | 3%positive => (max0((s IDTIFFSetDirectory__tmp1))
                     + max0((s IDTIFFSetDirectory_z)))%Q
    | 4%positive => (max0((s IDTIFFSetDirectory_n))
                     + max0((s IDTIFFSetDirectory_z)))%Q
    | 5%positive => (max0((s IDTIFFSetDirectory_n))
                     + max0((s IDTIFFSetDirectory_z)))%Q
    | 6%positive => (max0((s IDTIFFSetDirectory_n))
                     + max0((s IDTIFFSetDirectory_z)))%Q
    | 7%positive => (max0((s IDTIFFSetDirectory_n))
                     + max0((s IDTIFFSetDirectory_z)))%Q
    | 8%positive => (max0((s IDTIFFSetDirectory_n))
                     + max0((s IDTIFFSetDirectory_z)))%Q
    | 9%positive => (max0((s IDTIFFSetDirectory_n))
                     + max0((s IDTIFFSetDirectory_z)))%Q
    | 10%positive => (max0((s IDTIFFSetDirectory_n))
                      + max0((s IDTIFFSetDirectory_z)))%Q
    | 11%positive => ((s IDTIFFSetDirectory_n) + (s IDTIFFSetDirectory_z))%Q
    | 12%positive => ((s IDTIFFSetDirectory_n) + (s IDTIFFSetDirectory_z))%Q
    | 13%positive => ((s IDTIFFSetDirectory_z))%Q
    | 14%positive => ((s IDTIFFSetDirectory_z))%Q
    | 15%positive => ((s IDTIFFSetDirectory_z))%Q
    | 16%positive => ((s IDTIFFSetDirectory_n) + (s IDTIFFSetDirectory_z))%Q
    | 17%positive => ((s IDTIFFSetDirectory_n) + (s IDTIFFSetDirectory_z))%Q
    | 18%positive => ((s IDTIFFSetDirectory_n) + (s IDTIFFSetDirectory_z))%Q
    | 19%positive => ((s IDTIFFSetDirectory_n) + (s IDTIFFSetDirectory_z))%Q
    | 20%positive => ((s IDTIFFSetDirectory_n) + (s IDTIFFSetDirectory_z))%Q
    | 21%positive => ((s IDTIFFSetDirectory_z))%Q
    | 22%positive => ((s IDTIFFSetDirectory_n) + (s IDTIFFSetDirectory_z))%Q
    | 23%positive => ((s IDTIFFSetDirectory_n) + (s IDTIFFSetDirectory_z))%Q
    | 24%positive => ((1 # 1) + (s IDTIFFSetDirectory_n)
                      + (s IDTIFFSetDirectory_z))%Q
    | 25%positive => ((1 # 1) + (s IDTIFFSetDirectory_n)
                      + (s IDTIFFSetDirectory_z))%Q
    | 26%positive => ((1 # 1) + (s IDTIFFSetDirectory_n)
                      + (s IDTIFFSetDirectory_z))%Q
    | 27%positive => ((s IDTIFFSetDirectory_n) + (s IDTIFFSetDirectory_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition TIFFSetDirectory_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDTIFFSetDirectory_n)) (-1
                                                                    + (s IDTIFFSetDirectory_n)));
                     (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTIFFSetDirectory_z))) (F_check_ge ((s IDTIFFSetDirectory_z)) (0));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                + (s IDTIFFSetDirectory_n))) (F_check_ge (0) (0))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTIFFSetDirectory_z))) (F_check_ge ((s IDTIFFSetDirectory_z)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDTIFFSetDirectory_n))) (F_check_ge ((s IDTIFFSetDirectory_n)) (0))]
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDTIFFSetDirectory_n)) (1);
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFSetDirectory_n));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDTIFFSetDirectory_n)) (0))) (F_max0_ge_0 ((s IDTIFFSetDirectory_n)))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDTIFFSetDirectory_n)) (1);
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDTIFFSetDirectory_n));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDTIFFSetDirectory_n)) (0))) (F_max0_ge_0 ((s IDTIFFSetDirectory_n)))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDTIFFSetDirectory_z)) (0))) (F_max0_ge_0 ((s IDTIFFSetDirectory_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDTIFFSetDirectory_n)) (0))) (F_max0_ge_0 ((s IDTIFFSetDirectory_n)))]
    | _ => []
  end.


Theorem TIFFSetDirectory_ai_correct:
  forall s p' s', steps (g_start TIFFSetDirectory) s (g_edges TIFFSetDirectory) p' s' -> TIFFSetDirectory_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem TIFFSetDirectory_pot_correct:
  forall s p' s',
    steps (g_start TIFFSetDirectory) s (g_edges TIFFSetDirectory) p' s' ->
    (TIFFSetDirectory_pot (g_start TIFFSetDirectory) s >= TIFFSetDirectory_pot p' s')%Q.
Proof.
  check_lp TIFFSetDirectory_ai_correct TIFFSetDirectory_hints.
Qed.

