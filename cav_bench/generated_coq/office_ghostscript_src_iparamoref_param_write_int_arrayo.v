Require Import pasta.Pasta.

Notation IDref_param_write_int_array_z := 1%positive.
Notation IDref_param_write_int_array__tmp := 2%positive.
Notation IDref_param_write_int_array_code := 3%positive.
Notation IDref_param_write_int_array_n := 4%positive.
Notation IDref_param_write_int_array_pvalue_dref_off8 := 5%positive.
Notation IDref_param_write_int_array_pkey := 6%positive.
Notation IDref_param_write_int_array_plist := 7%positive.
Notation IDref_param_write_int_array_pvalue := 8%positive.
Definition ref_param_write_int_array : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDref_param_write_int_array_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDref_param_write_int_array_n) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDref_param_write_int_array_n
             (Some (EVar IDref_param_write_int_array_pvalue_dref_off8))),
             5%positive)::
             (5%positive,(AAssign IDref_param_write_int_array_code None),
             6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,ANone,22%positive)::(7%positive,ANone,8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDref_param_write_int_array_n) s) >
             (eval (ENum (0)) s))%Z)),15%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDref_param_write_int_array_n) s) <=
             (eval (ENum (0)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDref_param_write_int_array__tmp None),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,25%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDref_param_write_int_array_n
             (Some (EAdd (EVar IDref_param_write_int_array_n) (ENum (-1))))),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDref_param_write_int_array_z
             (Some (EAdd (ENum (1)) (EVar IDref_param_write_int_array_z)))),
             21%positive)::(21%positive,AWeaken,10%positive)::
             (22%positive,(AAssign IDref_param_write_int_array__tmp
             (Some (EVar IDref_param_write_int_array_code))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::nil
|}.

Definition ref_param_write_int_array_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDref_param_write_int_array_z) <= 0 /\ -1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 3%positive => (-1 * (s IDref_param_write_int_array_z) <= 0 /\ 1 * (s IDref_param_write_int_array_z) <= 0 /\ -1 * (s IDref_param_write_int_array_n) <= 0)%Z
    | 4%positive => (-1 * (s IDref_param_write_int_array_n) <= 0 /\ 1 * (s IDref_param_write_int_array_z) <= 0 /\ -1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 5%positive => (-1 * (s IDref_param_write_int_array_z) <= 0 /\ 1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 6%positive => (1 * (s IDref_param_write_int_array_z) <= 0 /\ -1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 7%positive => (-1 * (s IDref_param_write_int_array_z) <= 0 /\ 1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 8%positive => (1 * (s IDref_param_write_int_array_z) <= 0 /\ -1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 9%positive => (-1 * (s IDref_param_write_int_array_z) <= 0 /\ 1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 10%positive => (-1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 11%positive => (-1 * (s IDref_param_write_int_array_z) <= 0 /\ 1 * (s IDref_param_write_int_array_n) <= 0)%Z
    | 12%positive => (1 * (s IDref_param_write_int_array_n) <= 0 /\ -1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 13%positive => (-1 * (s IDref_param_write_int_array_z) <= 0 /\ 1 * (s IDref_param_write_int_array_n) <= 0)%Z
    | 14%positive => (1 * (s IDref_param_write_int_array_n) <= 0 /\ -1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 15%positive => (-1 * (s IDref_param_write_int_array_z) <= 0 /\ -1 * (s IDref_param_write_int_array_n) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDref_param_write_int_array_n) + 1 <= 0 /\ -1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 17%positive => (-1 * (s IDref_param_write_int_array_z) <= 0 /\ -1 * (s IDref_param_write_int_array_n) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDref_param_write_int_array_z) <= 0 /\ -1 * (s IDref_param_write_int_array_n) <= 0)%Z
    | 19%positive => (-1 * (s IDref_param_write_int_array_n) <= 0 /\ -1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 20%positive => (-1 * (s IDref_param_write_int_array_z) <= 0 /\ -1 * (s IDref_param_write_int_array_n) <= 0)%Z
    | 21%positive => (-1 * (s IDref_param_write_int_array_n) <= 0 /\ -1 * (s IDref_param_write_int_array_z) + 1 <= 0)%Z
    | 22%positive => (1 * (s IDref_param_write_int_array_z) <= 0 /\ -1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 23%positive => (-1 * (s IDref_param_write_int_array_z) <= 0 /\ 1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 24%positive => (1 * (s IDref_param_write_int_array_z) <= 0 /\ -1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | 25%positive => (-1 * (s IDref_param_write_int_array_z) <= 0)%Z
    | _ => False
  end.

Definition ref_param_write_int_array_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDref_param_write_int_array_pvalue_dref_off8)))%Q
    | 2%positive => ((s IDref_param_write_int_array_z)
                     + max0((s IDref_param_write_int_array_pvalue_dref_off8)))%Q
    | 3%positive => ((s IDref_param_write_int_array_z)
                     + max0((s IDref_param_write_int_array_pvalue_dref_off8)))%Q
    | 4%positive => ((s IDref_param_write_int_array_z)
                     + max0((s IDref_param_write_int_array_pvalue_dref_off8)))%Q
    | 5%positive => ((s IDref_param_write_int_array_z)
                     + max0((s IDref_param_write_int_array_n)))%Q
    | 6%positive => ((s IDref_param_write_int_array_z)
                     + max0((s IDref_param_write_int_array_n)))%Q
    | 7%positive => ((s IDref_param_write_int_array_z)
                     + max0((s IDref_param_write_int_array_n)))%Q
    | 8%positive => ((s IDref_param_write_int_array_z)
                     + max0((s IDref_param_write_int_array_n)))%Q
    | 9%positive => ((s IDref_param_write_int_array_z)
                     + max0((s IDref_param_write_int_array_n)))%Q
    | 10%positive => ((s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 11%positive => ((s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 12%positive => ((s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 13%positive => ((s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 14%positive => ((s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 15%positive => ((s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 16%positive => ((1 # 1) + (s IDref_param_write_int_array_z)
                      + max0(-1 + (s IDref_param_write_int_array_n)))%Q
    | 17%positive => ((1 # 1) + (s IDref_param_write_int_array_z)
                      + max0(-1 + (s IDref_param_write_int_array_n)))%Q
    | 18%positive => ((1 # 1) + (s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 19%positive => ((1 # 1) + (s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 20%positive => ((1 # 1) + (s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 21%positive => ((s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 22%positive => ((s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 23%positive => ((s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 24%positive => ((s IDref_param_write_int_array_z)
                      + max0((s IDref_param_write_int_array_n)))%Q
    | 25%positive => ((s IDref_param_write_int_array_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition ref_param_write_int_array_hints (p : node) (s : state) := 
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
    | 14%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDref_param_write_int_array_n)) (-1
                                                                    + (s IDref_param_write_int_array_n)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDref_param_write_int_array_n))]
    | 15%positive => [(*-1 0*) F_max0_pre_decrement ((s IDref_param_write_int_array_n)) (1)]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDref_param_write_int_array_n)) (-1
                                                                    + (s IDref_param_write_int_array_n)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDref_param_write_int_array_n))]
    | 25%positive => []
    | _ => []
  end.


Theorem ref_param_write_int_array_ai_correct:
  forall s p' s', steps (g_start ref_param_write_int_array) s (g_edges ref_param_write_int_array) p' s' -> ref_param_write_int_array_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ref_param_write_int_array_pot_correct:
  forall s p' s',
    steps (g_start ref_param_write_int_array) s (g_edges ref_param_write_int_array) p' s' ->
    (ref_param_write_int_array_pot (g_start ref_param_write_int_array) s >= ref_param_write_int_array_pot p' s')%Q.
Proof.
  check_lp ref_param_write_int_array_ai_correct ref_param_write_int_array_hints.
Qed.

