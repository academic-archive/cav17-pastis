Require Import pasta.Pasta.

Notation IDemit_buffered_bits_z := 1%positive.
Notation IDemit_buffered_bits__tmp := 2%positive.
Notation IDemit_buffered_bits_bufstart := 3%positive.
Notation IDemit_buffered_bits_entropy := 4%positive.
Notation IDemit_buffered_bits_nbits := 5%positive.
Definition emit_buffered_bits : graph := {|
  g_start := 1%positive;
  g_end := 19%positive;
  g_edges := (1%positive,(AAssign IDemit_buffered_bits_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDemit_buffered_bits__tmp) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDemit_buffered_bits__tmp
             (Some (EVar IDemit_buffered_bits_nbits))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,ANone,17%positive)::(6%positive,ANone,7%positive)::
             (7%positive,ANone,8%positive)::(8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDemit_buffered_bits__tmp) s) >
             (eval (ENum (0)) s))%Z)),11%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDemit_buffered_bits__tmp) s) <=
             (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,19%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDemit_buffered_bits__tmp
             (Some (EAdd (EVar IDemit_buffered_bits__tmp) (ENum (-1))))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDemit_buffered_bits_z
             (Some (EAdd (ENum (1)) (EVar IDemit_buffered_bits_z)))),
             16%positive)::(16%positive,AWeaken,9%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::nil
|}.

Definition emit_buffered_bits_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDemit_buffered_bits_z) <= 0 /\ -1 * (s IDemit_buffered_bits_z) <= 0)%Z
    | 3%positive => (-1 * (s IDemit_buffered_bits_z) <= 0 /\ 1 * (s IDemit_buffered_bits_z) <= 0 /\ -1 * (s IDemit_buffered_bits__tmp) <= 0)%Z
    | 4%positive => (-1 * (s IDemit_buffered_bits__tmp) <= 0 /\ 1 * (s IDemit_buffered_bits_z) <= 0 /\ -1 * (s IDemit_buffered_bits_z) <= 0)%Z
    | 5%positive => (-1 * (s IDemit_buffered_bits_z) <= 0 /\ 1 * (s IDemit_buffered_bits_z) <= 0)%Z
    | 6%positive => (1 * (s IDemit_buffered_bits_z) <= 0 /\ -1 * (s IDemit_buffered_bits_z) <= 0)%Z
    | 7%positive => (-1 * (s IDemit_buffered_bits_z) <= 0 /\ 1 * (s IDemit_buffered_bits_z) <= 0)%Z
    | 8%positive => (1 * (s IDemit_buffered_bits_z) <= 0 /\ -1 * (s IDemit_buffered_bits_z) <= 0)%Z
    | 9%positive => (-1 * (s IDemit_buffered_bits_z) <= 0)%Z
    | 10%positive => (-1 * (s IDemit_buffered_bits_z) <= 0 /\ 1 * (s IDemit_buffered_bits__tmp) <= 0)%Z
    | 11%positive => (-1 * (s IDemit_buffered_bits_z) <= 0 /\ -1 * (s IDemit_buffered_bits__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDemit_buffered_bits__tmp) + 1 <= 0 /\ -1 * (s IDemit_buffered_bits_z) <= 0)%Z
    | 13%positive => (-1 * (s IDemit_buffered_bits_z) <= 0 /\ -1 * (s IDemit_buffered_bits__tmp) <= 0)%Z
    | 14%positive => (-1 * (s IDemit_buffered_bits__tmp) <= 0 /\ -1 * (s IDemit_buffered_bits_z) <= 0)%Z
    | 15%positive => (-1 * (s IDemit_buffered_bits_z) <= 0 /\ -1 * (s IDemit_buffered_bits__tmp) <= 0)%Z
    | 16%positive => (-1 * (s IDemit_buffered_bits__tmp) <= 0 /\ -1 * (s IDemit_buffered_bits_z) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDemit_buffered_bits_z) <= 0 /\ 1 * (s IDemit_buffered_bits_z) <= 0)%Z
    | 18%positive => (1 * (s IDemit_buffered_bits_z) <= 0 /\ -1 * (s IDemit_buffered_bits_z) <= 0)%Z
    | 19%positive => (-1 * (s IDemit_buffered_bits_z) <= 0)%Z
    | _ => False
  end.

Definition emit_buffered_bits_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDemit_buffered_bits_nbits)))%Q
    | 2%positive => ((s IDemit_buffered_bits_z)
                     + max0((s IDemit_buffered_bits_nbits)))%Q
    | 3%positive => ((s IDemit_buffered_bits_z)
                     + max0((s IDemit_buffered_bits_nbits)))%Q
    | 4%positive => ((s IDemit_buffered_bits_z)
                     + max0((s IDemit_buffered_bits_nbits)))%Q
    | 5%positive => ((s IDemit_buffered_bits_z)
                     + max0((s IDemit_buffered_bits__tmp)))%Q
    | 6%positive => ((s IDemit_buffered_bits_z)
                     + max0((s IDemit_buffered_bits__tmp)))%Q
    | 7%positive => ((s IDemit_buffered_bits_z)
                     + max0((s IDemit_buffered_bits__tmp)))%Q
    | 8%positive => ((s IDemit_buffered_bits_z)
                     + max0((s IDemit_buffered_bits__tmp)))%Q
    | 9%positive => (max0((s IDemit_buffered_bits__tmp))
                     + max0((s IDemit_buffered_bits_z)))%Q
    | 10%positive => (max0((s IDemit_buffered_bits__tmp))
                      + max0((s IDemit_buffered_bits_z)))%Q
    | 11%positive => (max0((s IDemit_buffered_bits__tmp))
                      + max0((s IDemit_buffered_bits_z)))%Q
    | 12%positive => ((s IDemit_buffered_bits__tmp)
                      + max0((s IDemit_buffered_bits_z)))%Q
    | 13%positive => ((1 # 1) + (s IDemit_buffered_bits__tmp)
                      + max0((s IDemit_buffered_bits_z)))%Q
    | 14%positive => ((1 # 1) + (s IDemit_buffered_bits__tmp)
                      + max0((s IDemit_buffered_bits_z)))%Q
    | 15%positive => ((1 # 1) + (s IDemit_buffered_bits__tmp)
                      + max0((s IDemit_buffered_bits_z)))%Q
    | 16%positive => ((1 # 1) + (s IDemit_buffered_bits__tmp)
                      + max0(-1 + (s IDemit_buffered_bits_z)))%Q
    | 17%positive => ((s IDemit_buffered_bits_z)
                      + max0((s IDemit_buffered_bits__tmp)))%Q
    | 18%positive => ((s IDemit_buffered_bits_z)
                      + max0((s IDemit_buffered_bits__tmp)))%Q
    | 19%positive => ((s IDemit_buffered_bits_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition emit_buffered_bits_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDemit_buffered_bits_z)) (0))) (F_max0_ge_0 ((s IDemit_buffered_bits_z)))]
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDemit_buffered_bits__tmp)) (-1
                                                                    + (s IDemit_buffered_bits__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDemit_buffered_bits__tmp));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDemit_buffered_bits_z))) (F_check_ge ((s IDemit_buffered_bits_z)) (0))]
    | 11%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDemit_buffered_bits__tmp))) (F_check_ge ((s IDemit_buffered_bits__tmp)) (0))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDemit_buffered_bits_z)) (0))) (F_max0_ge_0 ((s IDemit_buffered_bits_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDemit_buffered_bits__tmp)) (0))) (F_max0_ge_0 ((s IDemit_buffered_bits__tmp)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDemit_buffered_bits_z))) (F_check_ge (-1
                                                                    + (s IDemit_buffered_bits_z)) (0))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDemit_buffered_bits__tmp)) (-1
                                                                    + (s IDemit_buffered_bits__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDemit_buffered_bits__tmp))]
    | 19%positive => []
    | _ => []
  end.


Theorem emit_buffered_bits_ai_correct:
  forall s p' s', steps (g_start emit_buffered_bits) s (g_edges emit_buffered_bits) p' s' -> emit_buffered_bits_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem emit_buffered_bits_pot_correct:
  forall s p' s',
    steps (g_start emit_buffered_bits) s (g_edges emit_buffered_bits) p' s' ->
    (emit_buffered_bits_pot (g_start emit_buffered_bits) s >= emit_buffered_bits_pot p' s')%Q.
Proof.
  check_lp emit_buffered_bits_ai_correct emit_buffered_bits_hints.
Qed.

