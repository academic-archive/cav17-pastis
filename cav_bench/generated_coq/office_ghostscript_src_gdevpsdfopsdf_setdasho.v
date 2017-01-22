Require Import pasta.Pasta.

Notation IDpsdf_setdash_z := 1%positive.
Notation IDpsdf_setdash__tmp := 2%positive.
Notation IDpsdf_setdash_i := 3%positive.
Notation IDpsdf_setdash_count := 4%positive.
Notation IDpsdf_setdash_offset := 5%positive.
Notation IDpsdf_setdash_pattern := 6%positive.
Notation IDpsdf_setdash_vdev := 7%positive.
Definition psdf_setdash : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDpsdf_setdash_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDpsdf_setdash_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDpsdf_setdash__tmp)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDpsdf_setdash__tmp
             (Some (EVar IDpsdf_setdash_count))),6%positive)::
             (6%positive,(AAssign IDpsdf_setdash_i (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDpsdf_setdash_i)
             s) < (eval (EVar IDpsdf_setdash__tmp) s))%Z)),12%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDpsdf_setdash_i)
             s) >= (eval (EVar IDpsdf_setdash__tmp) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDpsdf_setdash_i
             (Some (EAdd (EVar IDpsdf_setdash_i) (ENum (1))))),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDpsdf_setdash_z (Some (EAdd (ENum (1))
             (EVar IDpsdf_setdash_z)))),18%positive)::
             (18%positive,AWeaken,9%positive)::nil
|}.

Definition psdf_setdash_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpsdf_setdash_z) <= 0 /\ 1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash_i) <= 0)%Z
    | 4%positive => (-1 * (s IDpsdf_setdash_i) <= 0 /\ 1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDpsdf_setdash__tmp) <= 0 /\ -1 * (s IDpsdf_setdash_z) <= 0 /\ 1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash_i) <= 0)%Z
    | 6%positive => (-1 * (s IDpsdf_setdash_i) <= 0 /\ 1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash_z) <= 0)%Z
    | 7%positive => (-1 * (s IDpsdf_setdash_z) <= 0 /\ 1 * (s IDpsdf_setdash_z) <= 0 /\ 1 * (s IDpsdf_setdash_i) <= 0 /\ -1 * (s IDpsdf_setdash_i) <= 0)%Z
    | 8%positive => (-1 * (s IDpsdf_setdash_i) <= 0 /\ 1 * (s IDpsdf_setdash_i) <= 0 /\ 1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash_z) <= 0)%Z
    | 9%positive => (-1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash_i) <= 0)%Z
    | 10%positive => (-1 * (s IDpsdf_setdash_i) <= 0 /\ -1 * (s IDpsdf_setdash_z) <= 0 /\ 1 * (s IDpsdf_setdash__tmp)+ -1 * (s IDpsdf_setdash_i) <= 0)%Z
    | 11%positive => (1 * (s IDpsdf_setdash__tmp)+ -1 * (s IDpsdf_setdash_i) <= 0 /\ -1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash_i) <= 0)%Z
    | 12%positive => (-1 * (s IDpsdf_setdash_i) <= 0 /\ -1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash__tmp)+ 1 * (s IDpsdf_setdash_i) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDpsdf_setdash__tmp)+ 1 * (s IDpsdf_setdash_i) + 1 <= 0 /\ -1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash_i) <= 0)%Z
    | 14%positive => (-1 * (s IDpsdf_setdash_i) <= 0 /\ -1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash__tmp)+ 1 * (s IDpsdf_setdash_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash_i) + 1 <= 0 /\ -1 * (s IDpsdf_setdash__tmp)+ 1 * (s IDpsdf_setdash_i) <= 0)%Z
    | 16%positive => (-1 * (s IDpsdf_setdash__tmp)+ 1 * (s IDpsdf_setdash_i) <= 0 /\ -1 * (s IDpsdf_setdash_i) + 1 <= 0 /\ -1 * (s IDpsdf_setdash_z) <= 0)%Z
    | 17%positive => (-1 * (s IDpsdf_setdash_z) <= 0 /\ -1 * (s IDpsdf_setdash_i) + 1 <= 0 /\ -1 * (s IDpsdf_setdash__tmp)+ 1 * (s IDpsdf_setdash_i) <= 0)%Z
    | 18%positive => (-1 * (s IDpsdf_setdash__tmp)+ 1 * (s IDpsdf_setdash_i) <= 0 /\ -1 * (s IDpsdf_setdash_i) + 1 <= 0 /\ -1 * (s IDpsdf_setdash_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition psdf_setdash_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDpsdf_setdash_count)))%Q
    | 2%positive => ((s IDpsdf_setdash_z) + max0((s IDpsdf_setdash_count)))%Q
    | 3%positive => ((s IDpsdf_setdash_z) + max0((s IDpsdf_setdash_count)))%Q
    | 4%positive => ((s IDpsdf_setdash_z) + max0((s IDpsdf_setdash_count)))%Q
    | 5%positive => ((s IDpsdf_setdash_z) + max0((s IDpsdf_setdash_count)))%Q
    | 6%positive => ((s IDpsdf_setdash_z) + max0((s IDpsdf_setdash__tmp)))%Q
    | 7%positive => ((s IDpsdf_setdash_z)
                     + max0((s IDpsdf_setdash__tmp) - (s IDpsdf_setdash_i)))%Q
    | 8%positive => ((s IDpsdf_setdash_z)
                     + max0((s IDpsdf_setdash__tmp) - (s IDpsdf_setdash_i)))%Q
    | 9%positive => ((s IDpsdf_setdash_z)
                     + max0((s IDpsdf_setdash__tmp) - (s IDpsdf_setdash_i)))%Q
    | 10%positive => ((s IDpsdf_setdash_z)
                      + max0((s IDpsdf_setdash__tmp) - (s IDpsdf_setdash_i)))%Q
    | 11%positive => ((s IDpsdf_setdash_z))%Q
    | 12%positive => ((s IDpsdf_setdash_z)
                      + max0((s IDpsdf_setdash__tmp) - (s IDpsdf_setdash_i)))%Q
    | 13%positive => ((1 # 1) + (s IDpsdf_setdash_z)
                      + max0(-1 + (s IDpsdf_setdash__tmp)
                             - (s IDpsdf_setdash_i)))%Q
    | 14%positive => ((1 # 1) + (s IDpsdf_setdash_z)
                      + max0(-1 + (s IDpsdf_setdash__tmp)
                             - (s IDpsdf_setdash_i)))%Q
    | 15%positive => ((1 # 1) + (s IDpsdf_setdash_z)
                      + max0((s IDpsdf_setdash__tmp) - (s IDpsdf_setdash_i)))%Q
    | 16%positive => ((1 # 1) + (s IDpsdf_setdash_z)
                      + max0((s IDpsdf_setdash__tmp) - (s IDpsdf_setdash_i)))%Q
    | 17%positive => ((1 # 1) + (s IDpsdf_setdash_z)
                      + max0((s IDpsdf_setdash__tmp) - (s IDpsdf_setdash_i)))%Q
    | 18%positive => ((s IDpsdf_setdash_z)
                      + max0((s IDpsdf_setdash__tmp) - (s IDpsdf_setdash_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition psdf_setdash_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDpsdf_setdash__tmp)
                                                             - (s IDpsdf_setdash_i)) (-1
                                                                    + (s IDpsdf_setdash__tmp)
                                                                    - (s IDpsdf_setdash_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDpsdf_setdash__tmp)
                                            - (s IDpsdf_setdash_i))]
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_pre_decrement ((s IDpsdf_setdash__tmp)
                                                     - (s IDpsdf_setdash_i)) (1)]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | _ => []
  end.


Theorem psdf_setdash_ai_correct:
  forall s p' s', steps (g_start psdf_setdash) s (g_edges psdf_setdash) p' s' -> psdf_setdash_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem psdf_setdash_pot_correct:
  forall s p' s',
    steps (g_start psdf_setdash) s (g_edges psdf_setdash) p' s' ->
    (psdf_setdash_pot (g_start psdf_setdash) s >= psdf_setdash_pot p' s')%Q.
Proof.
  check_lp psdf_setdash_ai_correct psdf_setdash_hints.
Qed.

