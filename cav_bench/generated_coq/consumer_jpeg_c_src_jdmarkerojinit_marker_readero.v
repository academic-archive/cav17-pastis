Require Import pasta.Pasta.

Notation IDjinit_marker_reader_z := 1%positive.
Notation IDjinit_marker_reader_i := 2%positive.
Notation IDjinit_marker_reader_cinfo := 3%positive.
Definition jinit_marker_reader : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDjinit_marker_reader_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDjinit_marker_reader_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDjinit_marker_reader_i) s) <
             (eval (ENum (16)) s))%Z)),8%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDjinit_marker_reader_i) s) >=
             (eval (ENum (16)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDjinit_marker_reader_i
             (Some (EAdd (EVar IDjinit_marker_reader_i) (ENum (1))))),
             11%positive)::(11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDjinit_marker_reader_z
             (Some (EAdd (ENum (1)) (EVar IDjinit_marker_reader_z)))),
             14%positive)::(14%positive,AWeaken,5%positive)::nil
|}.

Definition jinit_marker_reader_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjinit_marker_reader_z) <= 0 /\ -1 * (s IDjinit_marker_reader_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjinit_marker_reader_z) <= 0 /\ 1 * (s IDjinit_marker_reader_z) <= 0 /\ 1 * (s IDjinit_marker_reader_i) <= 0 /\ -1 * (s IDjinit_marker_reader_i) <= 0)%Z
    | 4%positive => (-1 * (s IDjinit_marker_reader_i) <= 0 /\ 1 * (s IDjinit_marker_reader_i) <= 0 /\ 1 * (s IDjinit_marker_reader_z) <= 0 /\ -1 * (s IDjinit_marker_reader_z) <= 0)%Z
    | 5%positive => (-1 * (s IDjinit_marker_reader_z) <= 0 /\ -1 * (s IDjinit_marker_reader_i) <= 0 /\ 1 * (s IDjinit_marker_reader_i) + -16 <= 0)%Z
    | 6%positive => (1 * (s IDjinit_marker_reader_i) + -16 <= 0 /\ -1 * (s IDjinit_marker_reader_z) <= 0 /\ -1 * (s IDjinit_marker_reader_i) + 16 <= 0)%Z
    | 7%positive => (-1 * (s IDjinit_marker_reader_i) + 16 <= 0 /\ -1 * (s IDjinit_marker_reader_z) <= 0 /\ 1 * (s IDjinit_marker_reader_i) + -16 <= 0)%Z
    | 8%positive => (-1 * (s IDjinit_marker_reader_i) <= 0 /\ -1 * (s IDjinit_marker_reader_z) <= 0 /\ 1 * (s IDjinit_marker_reader_i) + -15 <= 0)%Z
    | 9%positive => (1 * (s IDjinit_marker_reader_i) + -15 <= 0 /\ -1 * (s IDjinit_marker_reader_z) <= 0 /\ -1 * (s IDjinit_marker_reader_i) <= 0)%Z
    | 10%positive => (-1 * (s IDjinit_marker_reader_i) <= 0 /\ -1 * (s IDjinit_marker_reader_z) <= 0 /\ 1 * (s IDjinit_marker_reader_i) + -15 <= 0)%Z
    | 11%positive => (-1 * (s IDjinit_marker_reader_z) <= 0 /\ -1 * (s IDjinit_marker_reader_i) + 1 <= 0 /\ 1 * (s IDjinit_marker_reader_i) + -16 <= 0)%Z
    | 12%positive => (1 * (s IDjinit_marker_reader_i) + -16 <= 0 /\ -1 * (s IDjinit_marker_reader_i) + 1 <= 0 /\ -1 * (s IDjinit_marker_reader_z) <= 0)%Z
    | 13%positive => (-1 * (s IDjinit_marker_reader_z) <= 0 /\ -1 * (s IDjinit_marker_reader_i) + 1 <= 0 /\ 1 * (s IDjinit_marker_reader_i) + -16 <= 0)%Z
    | 14%positive => (1 * (s IDjinit_marker_reader_i) + -16 <= 0 /\ -1 * (s IDjinit_marker_reader_i) + 1 <= 0 /\ -1 * (s IDjinit_marker_reader_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jinit_marker_reader_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((16 # 1))%Q
    | 2%positive => ((16 # 1) + (s IDjinit_marker_reader_z))%Q
    | 3%positive => ((s IDjinit_marker_reader_z)
                     + max0(16 - (s IDjinit_marker_reader_i)))%Q
    | 4%positive => ((s IDjinit_marker_reader_z)
                     + max0(16 - (s IDjinit_marker_reader_i)))%Q
    | 5%positive => ((s IDjinit_marker_reader_z)
                     + max0(16 - (s IDjinit_marker_reader_i)))%Q
    | 6%positive => ((s IDjinit_marker_reader_z)
                     + max0(16 - (s IDjinit_marker_reader_i)))%Q
    | 7%positive => ((s IDjinit_marker_reader_z))%Q
    | 8%positive => ((s IDjinit_marker_reader_z)
                     + max0(16 - (s IDjinit_marker_reader_i)))%Q
    | 9%positive => ((1 # 1) + (s IDjinit_marker_reader_z)
                     + max0(15 - (s IDjinit_marker_reader_i)))%Q
    | 10%positive => ((1 # 1) + (s IDjinit_marker_reader_z)
                      + max0(15 - (s IDjinit_marker_reader_i)))%Q
    | 11%positive => ((1 # 1) + (s IDjinit_marker_reader_z)
                      + max0(16 - (s IDjinit_marker_reader_i)))%Q
    | 12%positive => ((1 # 1) + (s IDjinit_marker_reader_z)
                      + max0(16 - (s IDjinit_marker_reader_i)))%Q
    | 13%positive => ((1 # 1) + (s IDjinit_marker_reader_z)
                      + max0(16 - (s IDjinit_marker_reader_i)))%Q
    | 14%positive => ((s IDjinit_marker_reader_z)
                      + max0(16 - (s IDjinit_marker_reader_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition jinit_marker_reader_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (16
                                                            - (s IDjinit_marker_reader_i)) (15
                                                                    - (s IDjinit_marker_reader_i)));
                     (*-1 0*) F_max0_ge_0 (15 - (s IDjinit_marker_reader_i))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (16
                                                    - (s IDjinit_marker_reader_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem jinit_marker_reader_ai_correct:
  forall s p' s', steps (g_start jinit_marker_reader) s (g_edges jinit_marker_reader) p' s' -> jinit_marker_reader_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jinit_marker_reader_pot_correct:
  forall s p' s',
    steps (g_start jinit_marker_reader) s (g_edges jinit_marker_reader) p' s' ->
    (jinit_marker_reader_pot (g_start jinit_marker_reader) s >= jinit_marker_reader_pot p' s')%Q.
Proof.
  check_lp jinit_marker_reader_ai_correct jinit_marker_reader_hints.
Qed.

