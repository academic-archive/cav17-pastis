Require Import pasta.Pasta.

Notation IDTIFFDefaultRefBlackWhite_z := 1%positive.
Notation IDTIFFDefaultRefBlackWhite_i := 2%positive.
Notation IDTIFFDefaultRefBlackWhite_td := 3%positive.
Definition TIFFDefaultRefBlackWhite : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDTIFFDefaultRefBlackWhite_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDTIFFDefaultRefBlackWhite_i
             (Some (ENum (0)))),3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFDefaultRefBlackWhite_i) s) <
             (eval (ENum (3)) s))%Z)),8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFDefaultRefBlackWhite_i) s) >=
             (eval (ENum (3)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDTIFFDefaultRefBlackWhite_i
             (Some (EAdd (EVar IDTIFFDefaultRefBlackWhite_i) (ENum (1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDTIFFDefaultRefBlackWhite_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFDefaultRefBlackWhite_z)))),
             14%positive)::(14%positive,AWeaken,5%positive)::nil
|}.

Definition TIFFDefaultRefBlackWhite_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0)%Z
    | 3%positive => (-1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0 /\ 1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0 /\ 1 * (s IDTIFFDefaultRefBlackWhite_i) <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_i) <= 0)%Z
    | 4%positive => (-1 * (s IDTIFFDefaultRefBlackWhite_i) <= 0 /\ 1 * (s IDTIFFDefaultRefBlackWhite_i) <= 0 /\ 1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0)%Z
    | 5%positive => (-1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_i) <= 0 /\ 1 * (s IDTIFFDefaultRefBlackWhite_i) + -3 <= 0)%Z
    | 6%positive => (1 * (s IDTIFFDefaultRefBlackWhite_i) + -3 <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_i) + 3 <= 0)%Z
    | 7%positive => (-1 * (s IDTIFFDefaultRefBlackWhite_i) + 3 <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0 /\ 1 * (s IDTIFFDefaultRefBlackWhite_i) + -3 <= 0)%Z
    | 8%positive => (-1 * (s IDTIFFDefaultRefBlackWhite_i) <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0 /\ 1 * (s IDTIFFDefaultRefBlackWhite_i) + -2 <= 0)%Z
    | 9%positive => (1 * (s IDTIFFDefaultRefBlackWhite_i) + -2 <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_i) <= 0)%Z
    | 10%positive => (-1 * (s IDTIFFDefaultRefBlackWhite_i) <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0 /\ 1 * (s IDTIFFDefaultRefBlackWhite_i) + -2 <= 0)%Z
    | 11%positive => (-1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_i) + 1 <= 0 /\ 1 * (s IDTIFFDefaultRefBlackWhite_i) + -3 <= 0)%Z
    | 12%positive => (1 * (s IDTIFFDefaultRefBlackWhite_i) + -3 <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_i) + 1 <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0)%Z
    | 13%positive => (-1 * (s IDTIFFDefaultRefBlackWhite_z) <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_i) + 1 <= 0 /\ 1 * (s IDTIFFDefaultRefBlackWhite_i) + -3 <= 0)%Z
    | 14%positive => (1 * (s IDTIFFDefaultRefBlackWhite_i) + -3 <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_i) + 1 <= 0 /\ -1 * (s IDTIFFDefaultRefBlackWhite_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition TIFFDefaultRefBlackWhite_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((3 # 1))%Q
    | 2%positive => ((3 # 1) + (s IDTIFFDefaultRefBlackWhite_z))%Q
    | 3%positive => ((s IDTIFFDefaultRefBlackWhite_z)
                     + max0(3 - (s IDTIFFDefaultRefBlackWhite_i)))%Q
    | 4%positive => ((s IDTIFFDefaultRefBlackWhite_z)
                     + max0(3 - (s IDTIFFDefaultRefBlackWhite_i)))%Q
    | 5%positive => ((s IDTIFFDefaultRefBlackWhite_z)
                     + max0(3 - (s IDTIFFDefaultRefBlackWhite_i)))%Q
    | 6%positive => ((s IDTIFFDefaultRefBlackWhite_z)
                     + max0(3 - (s IDTIFFDefaultRefBlackWhite_i)))%Q
    | 7%positive => ((s IDTIFFDefaultRefBlackWhite_z))%Q
    | 8%positive => ((s IDTIFFDefaultRefBlackWhite_z)
                     + max0(3 - (s IDTIFFDefaultRefBlackWhite_i)))%Q
    | 9%positive => ((1 # 1) + (s IDTIFFDefaultRefBlackWhite_z)
                     + max0(2 - (s IDTIFFDefaultRefBlackWhite_i)))%Q
    | 10%positive => ((1 # 1) + (s IDTIFFDefaultRefBlackWhite_z)
                      + max0(2 - (s IDTIFFDefaultRefBlackWhite_i)))%Q
    | 11%positive => ((1 # 1) + (s IDTIFFDefaultRefBlackWhite_z)
                      + max0(3 - (s IDTIFFDefaultRefBlackWhite_i)))%Q
    | 12%positive => ((1 # 1) + (s IDTIFFDefaultRefBlackWhite_z)
                      + max0(3 - (s IDTIFFDefaultRefBlackWhite_i)))%Q
    | 13%positive => ((1 # 1) + (s IDTIFFDefaultRefBlackWhite_z)
                      + max0(3 - (s IDTIFFDefaultRefBlackWhite_i)))%Q
    | 14%positive => ((s IDTIFFDefaultRefBlackWhite_z)
                      + max0(3 - (s IDTIFFDefaultRefBlackWhite_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition TIFFDefaultRefBlackWhite_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                                            - (s IDTIFFDefaultRefBlackWhite_i)) (2
                                                                    - (s IDTIFFDefaultRefBlackWhite_i)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (2
                                                                - (s IDTIFFDefaultRefBlackWhite_i))) (F_check_ge (0) (0))]
    | 7%positive => []
    | 8%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (3
                                                                 - (s IDTIFFDefaultRefBlackWhite_i))) (F_check_ge (3
                                                                    - (s IDTIFFDefaultRefBlackWhite_i)) (0));
                     (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                    - (s IDTIFFDefaultRefBlackWhite_i)) (0))) (F_max0_ge_0 (2
                                                                    - (s IDTIFFDefaultRefBlackWhite_i)))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem TIFFDefaultRefBlackWhite_ai_correct:
  forall s p' s', steps (g_start TIFFDefaultRefBlackWhite) s (g_edges TIFFDefaultRefBlackWhite) p' s' -> TIFFDefaultRefBlackWhite_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem TIFFDefaultRefBlackWhite_pot_correct:
  forall s p' s',
    steps (g_start TIFFDefaultRefBlackWhite) s (g_edges TIFFDefaultRefBlackWhite) p' s' ->
    (TIFFDefaultRefBlackWhite_pot (g_start TIFFDefaultRefBlackWhite) s >= TIFFDefaultRefBlackWhite_pot p' s')%Q.
Proof.
  check_lp TIFFDefaultRefBlackWhite_ai_correct TIFFDefaultRefBlackWhite_hints.
Qed.

