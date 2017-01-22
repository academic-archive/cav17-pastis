Require Import pasta.Pasta.

Notation IDbi_reverse_z := 1%positive.
Notation IDbi_reverse__tmp := 2%positive.
Notation IDbi_reverse__tmp1 := 3%positive.
Notation IDbi_reverse_res := 4%positive.
Notation IDbi_reverse_code := 5%positive.
Notation IDbi_reverse_len := 6%positive.
Definition bi_reverse : graph := {|
  g_start := 1%positive;
  g_end := 14%positive;
  g_edges := (1%positive,(AAssign IDbi_reverse_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDbi_reverse__tmp1
             (Some (EVar IDbi_reverse_code))),3%positive)::
             (3%positive,(AAssign IDbi_reverse__tmp
             (Some (EVar IDbi_reverse_len))),4%positive)::
             (4%positive,(AAssign IDbi_reverse_res (Some (ENum (0)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDbi_reverse_res None),7%positive)::
             (7%positive,(AAssign IDbi_reverse__tmp1 None),8%positive)::
             (8%positive,(AAssign IDbi_reverse_res None),9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDbi_reverse__tmp
             (Some (EAdd (EVar IDbi_reverse__tmp) (ENum (-1))))),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbi_reverse__tmp) (ENum (-1)))
             s) > (eval (ENum (0)) s))%Z)),15%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDbi_reverse__tmp) (ENum (-1)))
             s) <= (eval (ENum (0)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDbi_reverse_z (Some (EAdd (ENum (1))
             (EVar IDbi_reverse_z)))),6%positive)::nil
|}.

Definition bi_reverse_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDbi_reverse_z) <= 0 /\ -1 * (s IDbi_reverse_z) <= 0)%Z
    | 3%positive => (-1 * (s IDbi_reverse_z) <= 0 /\ 1 * (s IDbi_reverse_z) <= 0)%Z
    | 4%positive => (1 * (s IDbi_reverse_z) <= 0 /\ -1 * (s IDbi_reverse_z) <= 0)%Z
    | 5%positive => (-1 * (s IDbi_reverse_z) <= 0 /\ 1 * (s IDbi_reverse_z) <= 0 /\ 1 * (s IDbi_reverse_res) <= 0 /\ -1 * (s IDbi_reverse_res) <= 0)%Z
    | 6%positive => (-1 * (s IDbi_reverse_z) <= 0)%Z
    | 7%positive => (-1 * (s IDbi_reverse_z) <= 0)%Z
    | 8%positive => (-1 * (s IDbi_reverse_z) <= 0)%Z
    | 9%positive => (-1 * (s IDbi_reverse_z) <= 0)%Z
    | 10%positive => (-1 * (s IDbi_reverse_z) <= 0)%Z
    | 11%positive => (-1 * (s IDbi_reverse_z) <= 0)%Z
    | 12%positive => (-1 * (s IDbi_reverse_z) <= 0)%Z
    | 13%positive => (-1 * (s IDbi_reverse_z) <= 0 /\ 1 * (s IDbi_reverse__tmp) + -1 <= 0)%Z
    | 14%positive => (1 * (s IDbi_reverse__tmp) + -1 <= 0 /\ -1 * (s IDbi_reverse_z) <= 0)%Z
    | 15%positive => (-1 * (s IDbi_reverse_z) <= 0 /\ -1 * (s IDbi_reverse__tmp) + 2 <= 0)%Z
    | 16%positive => (-1 * (s IDbi_reverse__tmp) + 2 <= 0 /\ -1 * (s IDbi_reverse_z) <= 0)%Z
    | 17%positive => (-1 * (s IDbi_reverse_z) <= 0 /\ -1 * (s IDbi_reverse__tmp) + 2 <= 0)%Z
    | _ => False
  end.

Definition bi_reverse_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-2 + (s IDbi_reverse_len)))%Q
    | 2%positive => ((s IDbi_reverse_z) + max0(-2 + (s IDbi_reverse_len)))%Q
    | 3%positive => ((s IDbi_reverse_z) + max0(-2 + (s IDbi_reverse_len)))%Q
    | 4%positive => ((s IDbi_reverse_z) + max0(-2 + (s IDbi_reverse__tmp)))%Q
    | 5%positive => ((s IDbi_reverse_z) + max0(-2 + (s IDbi_reverse__tmp)))%Q
    | 6%positive => ((s IDbi_reverse_z) + max0(-2 + (s IDbi_reverse__tmp)))%Q
    | 7%positive => ((s IDbi_reverse_z) + max0(-2 + (s IDbi_reverse__tmp)))%Q
    | 8%positive => ((s IDbi_reverse_z) + max0(-2 + (s IDbi_reverse__tmp)))%Q
    | 9%positive => ((s IDbi_reverse_z) + max0(-2 + (s IDbi_reverse__tmp)))%Q
    | 10%positive => ((s IDbi_reverse_z) + max0(-2 + (s IDbi_reverse__tmp)))%Q
    | 11%positive => ((s IDbi_reverse_z) + max0(-1 + (s IDbi_reverse__tmp)))%Q
    | 12%positive => ((s IDbi_reverse_z) + max0(-1 + (s IDbi_reverse__tmp)))%Q
    | 13%positive => ((s IDbi_reverse_z) + max0(-1 + (s IDbi_reverse__tmp)))%Q
    | 14%positive => ((s IDbi_reverse_z))%Q
    | 15%positive => ((s IDbi_reverse_z) + max0(-1 + (s IDbi_reverse__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDbi_reverse_z)
                      + max0(-2 + (s IDbi_reverse__tmp)))%Q
    | 17%positive => ((1 # 1) + (s IDbi_reverse_z)
                      + max0(-2 + (s IDbi_reverse__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition bi_reverse_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-1
                                                             + (s IDbi_reverse__tmp)) (-2
                                                                    + (s IDbi_reverse__tmp)));
                      (*-1 0*) F_max0_ge_0 (-2 + (s IDbi_reverse__tmp))]
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_pre_decrement (-1
                                                     + (s IDbi_reverse__tmp)) (1)]
    | 16%positive => []
    | 17%positive => []
    | _ => []
  end.


Theorem bi_reverse_ai_correct:
  forall s p' s', steps (g_start bi_reverse) s (g_edges bi_reverse) p' s' -> bi_reverse_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem bi_reverse_pot_correct:
  forall s p' s',
    steps (g_start bi_reverse) s (g_edges bi_reverse) p' s' ->
    (bi_reverse_pot (g_start bi_reverse) s >= bi_reverse_pot p' s')%Q.
Proof.
  check_lp bi_reverse_ai_correct bi_reverse_hints.
Qed.

