Require Import pasta.Pasta.

Notation IDReadColorMap_z := 1%positive.
Notation IDReadColorMap__tmp := 2%positive.
Notation IDReadColorMap_i := 3%positive.
Notation IDReadColorMap_cmap := 4%positive.
Notation IDReadColorMap_cmaplen := 5%positive.
Notation IDReadColorMap_sinfo := 6%positive.
Definition ReadColorMap : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDReadColorMap_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDReadColorMap__tmp
             (Some (EVar IDReadColorMap_cmaplen))),3%positive)::
             (3%positive,(AAssign IDReadColorMap_i (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDReadColorMap_i)
             s) < (eval (EVar IDReadColorMap__tmp) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDReadColorMap_i)
             s) >= (eval (EVar IDReadColorMap__tmp) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDReadColorMap_i
             (Some (EAdd (EVar IDReadColorMap_i) (ENum (1))))),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDReadColorMap_z (Some (EAdd (ENum (1))
             (EVar IDReadColorMap_z)))),15%positive)::
             (15%positive,AWeaken,6%positive)::nil
|}.

Definition ReadColorMap_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDReadColorMap_z) <= 0 /\ -1 * (s IDReadColorMap_z) <= 0)%Z
    | 3%positive => (-1 * (s IDReadColorMap_z) <= 0 /\ 1 * (s IDReadColorMap_z) <= 0)%Z
    | 4%positive => (1 * (s IDReadColorMap_z) <= 0 /\ -1 * (s IDReadColorMap_z) <= 0 /\ 1 * (s IDReadColorMap_i) <= 0 /\ -1 * (s IDReadColorMap_i) <= 0)%Z
    | 5%positive => (-1 * (s IDReadColorMap_i) <= 0 /\ 1 * (s IDReadColorMap_i) <= 0 /\ -1 * (s IDReadColorMap_z) <= 0 /\ 1 * (s IDReadColorMap_z) <= 0)%Z
    | 6%positive => (-1 * (s IDReadColorMap_z) <= 0 /\ -1 * (s IDReadColorMap_i) <= 0)%Z
    | 7%positive => (-1 * (s IDReadColorMap_i) <= 0 /\ -1 * (s IDReadColorMap_z) <= 0 /\ 1 * (s IDReadColorMap__tmp)+ -1 * (s IDReadColorMap_i) <= 0)%Z
    | 8%positive => (1 * (s IDReadColorMap__tmp)+ -1 * (s IDReadColorMap_i) <= 0 /\ -1 * (s IDReadColorMap_z) <= 0 /\ -1 * (s IDReadColorMap_i) <= 0)%Z
    | 9%positive => (-1 * (s IDReadColorMap_i) <= 0 /\ -1 * (s IDReadColorMap_z) <= 0 /\ -1 * (s IDReadColorMap__tmp)+ 1 * (s IDReadColorMap_i) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDReadColorMap__tmp)+ 1 * (s IDReadColorMap_i) + 1 <= 0 /\ -1 * (s IDReadColorMap_z) <= 0 /\ -1 * (s IDReadColorMap_i) <= 0)%Z
    | 11%positive => (-1 * (s IDReadColorMap_i) <= 0 /\ -1 * (s IDReadColorMap_z) <= 0 /\ -1 * (s IDReadColorMap__tmp)+ 1 * (s IDReadColorMap_i) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDReadColorMap_z) <= 0 /\ -1 * (s IDReadColorMap_i) + 1 <= 0 /\ -1 * (s IDReadColorMap__tmp)+ 1 * (s IDReadColorMap_i) <= 0)%Z
    | 13%positive => (-1 * (s IDReadColorMap__tmp)+ 1 * (s IDReadColorMap_i) <= 0 /\ -1 * (s IDReadColorMap_i) + 1 <= 0 /\ -1 * (s IDReadColorMap_z) <= 0)%Z
    | 14%positive => (-1 * (s IDReadColorMap_z) <= 0 /\ -1 * (s IDReadColorMap_i) + 1 <= 0 /\ -1 * (s IDReadColorMap__tmp)+ 1 * (s IDReadColorMap_i) <= 0)%Z
    | 15%positive => (-1 * (s IDReadColorMap__tmp)+ 1 * (s IDReadColorMap_i) <= 0 /\ -1 * (s IDReadColorMap_i) + 1 <= 0 /\ -1 * (s IDReadColorMap_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition ReadColorMap_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDReadColorMap_cmaplen)))%Q
    | 2%positive => ((s IDReadColorMap_z) + max0((s IDReadColorMap_cmaplen)))%Q
    | 3%positive => ((s IDReadColorMap_z) + max0((s IDReadColorMap__tmp)))%Q
    | 4%positive => ((s IDReadColorMap_z)
                     + max0((s IDReadColorMap__tmp) - (s IDReadColorMap_i)))%Q
    | 5%positive => ((s IDReadColorMap_z)
                     + max0((s IDReadColorMap__tmp) - (s IDReadColorMap_i)))%Q
    | 6%positive => ((s IDReadColorMap_z)
                     + max0((s IDReadColorMap__tmp) - (s IDReadColorMap_i)))%Q
    | 7%positive => ((s IDReadColorMap_z)
                     + max0((s IDReadColorMap__tmp) - (s IDReadColorMap_i)))%Q
    | 8%positive => ((s IDReadColorMap_z))%Q
    | 9%positive => ((s IDReadColorMap_z)
                     + max0((s IDReadColorMap__tmp) - (s IDReadColorMap_i)))%Q
    | 10%positive => ((1 # 1) + (s IDReadColorMap_z)
                      + max0(-1 + (s IDReadColorMap__tmp)
                             - (s IDReadColorMap_i)))%Q
    | 11%positive => ((1 # 1) + (s IDReadColorMap_z)
                      + max0(-1 + (s IDReadColorMap__tmp)
                             - (s IDReadColorMap_i)))%Q
    | 12%positive => ((1 # 1) + (s IDReadColorMap_z)
                      + max0((s IDReadColorMap__tmp) - (s IDReadColorMap_i)))%Q
    | 13%positive => ((1 # 1) + (s IDReadColorMap_z)
                      + max0((s IDReadColorMap__tmp) - (s IDReadColorMap_i)))%Q
    | 14%positive => ((1 # 1) + (s IDReadColorMap_z)
                      + max0((s IDReadColorMap__tmp) - (s IDReadColorMap_i)))%Q
    | 15%positive => ((s IDReadColorMap_z)
                      + max0((s IDReadColorMap__tmp) - (s IDReadColorMap_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition ReadColorMap_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDReadColorMap__tmp)
                                                            - (s IDReadColorMap_i)) (-1
                                                                    + (s IDReadColorMap__tmp)
                                                                    - (s IDReadColorMap_i)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                + (s IDReadColorMap__tmp)
                                                                - (s IDReadColorMap_i))) (F_check_ge (0) (0))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_pre_decrement ((s IDReadColorMap__tmp)
                                                    - (s IDReadColorMap_i)) (1)]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | _ => []
  end.


Theorem ReadColorMap_ai_correct:
  forall s p' s', steps (g_start ReadColorMap) s (g_edges ReadColorMap) p' s' -> ReadColorMap_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ReadColorMap_pot_correct:
  forall s p' s',
    steps (g_start ReadColorMap) s (g_edges ReadColorMap) p' s' ->
    (ReadColorMap_pot (g_start ReadColorMap) s >= ReadColorMap_pot p' s')%Q.
Proof.
  check_lp ReadColorMap_ai_correct ReadColorMap_hints.
Qed.

