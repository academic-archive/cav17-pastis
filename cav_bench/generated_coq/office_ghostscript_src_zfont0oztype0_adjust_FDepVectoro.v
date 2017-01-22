Require Import pasta.Pasta.

Notation IDztype0_adjust_FDepVector_z := 1%positive.
Notation IDztype0_adjust_FDepVector__tmp := 2%positive.
Notation IDztype0_adjust_FDepVector_code := 3%positive.
Notation IDztype0_adjust_FDepVector_fdep_size := 4%positive.
Notation IDztype0_adjust_FDepVector_i := 5%positive.
Notation IDztype0_adjust_FDepVector_pfont_dref_off280_off56 := 6%positive.
Notation IDztype0_adjust_FDepVector_pfont := 7%positive.
Definition ztype0_adjust_FDepVector : graph := {|
  g_start := 1%positive;
  g_end := 29%positive;
  g_edges := (1%positive,(AAssign IDztype0_adjust_FDepVector_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDztype0_adjust_FDepVector_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDztype0_adjust_FDepVector_fdep_size)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDztype0_adjust_FDepVector_fdep_size
             (Some (EVar IDztype0_adjust_FDepVector_pfont_dref_off280_off56))),
             6%positive)::
             (6%positive,(AAssign IDztype0_adjust_FDepVector_code None),
             7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDztype0_adjust_FDepVector_code) s) <
             (eval (ENum (0)) s))%Z)),25%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDztype0_adjust_FDepVector_code) s) >=
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDztype0_adjust_FDepVector_i
             (Some (ENum (0)))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDztype0_adjust_FDepVector_i) s) <
             (eval (EVar IDztype0_adjust_FDepVector_fdep_size) s))%Z)),
             18%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDztype0_adjust_FDepVector_i) s) >=
             (eval (EVar IDztype0_adjust_FDepVector_fdep_size) s))%Z)),
             14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDztype0_adjust_FDepVector__tmp None),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,AWeaken,29%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDztype0_adjust_FDepVector_i
             (Some (EAdd (EVar IDztype0_adjust_FDepVector_i) (ENum (1))))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDztype0_adjust_FDepVector_z
             (Some (EAdd (ENum (1)) (EVar IDztype0_adjust_FDepVector_z)))),
             24%positive)::(24%positive,AWeaken,13%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDztype0_adjust_FDepVector__tmp
             (Some (EVar IDztype0_adjust_FDepVector_code))),27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,AWeaken,29%positive)::nil
|}.

Definition ztype0_adjust_FDepVector_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0)%Z
    | 3%positive => (-1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0)%Z
    | 4%positive => (-1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_fdep_size) <= 0)%Z
    | 5%positive => (-1 * (s IDztype0_adjust_FDepVector_fdep_size) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0)%Z
    | 6%positive => (-1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0)%Z
    | 7%positive => (-1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0)%Z
    | 8%positive => (-1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0)%Z
    | 9%positive => (-1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_code) <= 0)%Z
    | 10%positive => (-1 * (s IDztype0_adjust_FDepVector_code) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0)%Z
    | 11%positive => (-1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_code) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0)%Z
    | 12%positive => (-1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_code) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0)%Z
    | 13%positive => (-1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_code) <= 0)%Z
    | 14%positive => (-1 * (s IDztype0_adjust_FDepVector_code) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_fdep_size)+ -1 * (s IDztype0_adjust_FDepVector_i) <= 0)%Z
    | 15%positive => (1 * (s IDztype0_adjust_FDepVector_fdep_size)+ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_code) <= 0)%Z
    | 16%positive => (-1 * (s IDztype0_adjust_FDepVector_code) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_fdep_size)+ -1 * (s IDztype0_adjust_FDepVector_i) <= 0)%Z
    | 17%positive => (1 * (s IDztype0_adjust_FDepVector_fdep_size)+ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_code) <= 0)%Z
    | 18%positive => (-1 * (s IDztype0_adjust_FDepVector_code) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_fdep_size)+ 1 * (s IDztype0_adjust_FDepVector_i) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDztype0_adjust_FDepVector_fdep_size)+ 1 * (s IDztype0_adjust_FDepVector_i) + 1 <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_code) <= 0)%Z
    | 20%positive => (-1 * (s IDztype0_adjust_FDepVector_code) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_fdep_size)+ 1 * (s IDztype0_adjust_FDepVector_i) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_code) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) + 1 <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_fdep_size)+ 1 * (s IDztype0_adjust_FDepVector_i) <= 0)%Z
    | 22%positive => (-1 * (s IDztype0_adjust_FDepVector_fdep_size)+ 1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) + 1 <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_code) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0)%Z
    | 23%positive => (-1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_code) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) + 1 <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_fdep_size)+ 1 * (s IDztype0_adjust_FDepVector_i) <= 0)%Z
    | 24%positive => (-1 * (s IDztype0_adjust_FDepVector_fdep_size)+ 1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) + 1 <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_code) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_code) + 1 <= 0)%Z
    | 26%positive => (1 * (s IDztype0_adjust_FDepVector_code) + 1 <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0)%Z
    | 27%positive => (-1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_code) + 1 <= 0 /\ 1 * (s IDztype0_adjust_FDepVector__tmp) + 1 <= 0)%Z
    | 28%positive => (1 * (s IDztype0_adjust_FDepVector__tmp) + 1 <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_code) + 1 <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0 /\ 1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_z) <= 0)%Z
    | 29%positive => (-1 * (s IDztype0_adjust_FDepVector_z) <= 0 /\ -1 * (s IDztype0_adjust_FDepVector_i) <= 0)%Z
    | _ => False
  end.

Definition ztype0_adjust_FDepVector_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDztype0_adjust_FDepVector_pfont_dref_off280_off56)))%Q
    | 2%positive => ((s IDztype0_adjust_FDepVector_z)
                     + max0((s IDztype0_adjust_FDepVector_pfont_dref_off280_off56)))%Q
    | 3%positive => ((s IDztype0_adjust_FDepVector_z)
                     + max0((s IDztype0_adjust_FDepVector_pfont_dref_off280_off56)))%Q
    | 4%positive => ((s IDztype0_adjust_FDepVector_z)
                     + max0((s IDztype0_adjust_FDepVector_pfont_dref_off280_off56)))%Q
    | 5%positive => ((s IDztype0_adjust_FDepVector_z)
                     + max0((s IDztype0_adjust_FDepVector_pfont_dref_off280_off56)))%Q
    | 6%positive => ((s IDztype0_adjust_FDepVector_z)
                     + max0((s IDztype0_adjust_FDepVector_fdep_size)))%Q
    | 7%positive => ((s IDztype0_adjust_FDepVector_z)
                     + max0((s IDztype0_adjust_FDepVector_fdep_size)))%Q
    | 8%positive => ((s IDztype0_adjust_FDepVector_z)
                     + max0((s IDztype0_adjust_FDepVector_fdep_size)))%Q
    | 9%positive => ((s IDztype0_adjust_FDepVector_z)
                     + max0((s IDztype0_adjust_FDepVector_fdep_size)))%Q
    | 10%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)))%Q
    | 11%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 12%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 13%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 14%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 15%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 16%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 17%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 18%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 19%positive => ((1 # 1) + (s IDztype0_adjust_FDepVector_z)
                      + max0(-1 + (s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 20%positive => ((1 # 1) + (s IDztype0_adjust_FDepVector_z)
                      + max0(-1 + (s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 21%positive => ((1 # 1) + (s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 22%positive => ((1 # 1) + (s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 23%positive => ((1 # 1) + (s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 24%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)
                             - (s IDztype0_adjust_FDepVector_i)))%Q
    | 25%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)))%Q
    | 26%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)))%Q
    | 27%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)))%Q
    | 28%positive => ((s IDztype0_adjust_FDepVector_z)
                      + max0((s IDztype0_adjust_FDepVector_fdep_size)))%Q
    | 29%positive => ((s IDztype0_adjust_FDepVector_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition ztype0_adjust_FDepVector_hints (p : node) (s : state) := 
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
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDztype0_adjust_FDepVector_fdep_size)
                                                             - (s IDztype0_adjust_FDepVector_i)) (-1
                                                                    + (s IDztype0_adjust_FDepVector_fdep_size)
                                                                    - (s IDztype0_adjust_FDepVector_i)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDztype0_adjust_FDepVector_fdep_size)
                                            - (s IDztype0_adjust_FDepVector_i))]
    | 18%positive => [(*-1 0*) F_max0_pre_decrement ((s IDztype0_adjust_FDepVector_fdep_size)
                                                     - (s IDztype0_adjust_FDepVector_i)) (1)]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => [(*-1 0*) F_max0_ge_0 ((s IDztype0_adjust_FDepVector_fdep_size))]
    | 29%positive => []
    | _ => []
  end.


Theorem ztype0_adjust_FDepVector_ai_correct:
  forall s p' s', steps (g_start ztype0_adjust_FDepVector) s (g_edges ztype0_adjust_FDepVector) p' s' -> ztype0_adjust_FDepVector_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ztype0_adjust_FDepVector_pot_correct:
  forall s p' s',
    steps (g_start ztype0_adjust_FDepVector) s (g_edges ztype0_adjust_FDepVector) p' s' ->
    (ztype0_adjust_FDepVector_pot (g_start ztype0_adjust_FDepVector) s >= ztype0_adjust_FDepVector_pot p' s')%Q.
Proof.
  check_lp ztype0_adjust_FDepVector_ai_correct ztype0_adjust_FDepVector_hints.
Qed.

