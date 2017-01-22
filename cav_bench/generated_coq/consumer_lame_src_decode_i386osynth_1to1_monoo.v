Require Import pasta.Pasta.

Notation IDsynth_1to1_mono_z := 1%positive.
Notation IDsynth_1to1_mono_i := 2%positive.
Notation IDsynth_1to1_mono_pnt_dref := 3%positive.
Notation IDsynth_1to1_mono_ret := 4%positive.
Notation IDsynth_1to1_mono_bandPtr := 5%positive.
Notation IDsynth_1to1_mono_pnt := 6%positive.
Notation IDsynth_1to1_mono_samples := 7%positive.
Definition synth_1to1_mono : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDsynth_1to1_mono_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDsynth_1to1_mono_ret None),3%positive)::
             (3%positive,(AAssign IDsynth_1to1_mono_i (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDsynth_1to1_mono_i)
             s) < (eval (ENum (32)) s))%Z)),11%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDsynth_1to1_mono_i)
             s) >= (eval (ENum (32)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDsynth_1to1_mono_pnt_dref
             (Some (EAdd (EVar IDsynth_1to1_mono_pnt_dref) (ENum (64))))),
             9%positive)::(9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDsynth_1to1_mono_i
             (Some (EAdd (EVar IDsynth_1to1_mono_i) (ENum (1))))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDsynth_1to1_mono_z (Some (EAdd (ENum (1))
             (EVar IDsynth_1to1_mono_z)))),17%positive)::
             (17%positive,AWeaken,6%positive)::nil
|}.

Definition synth_1to1_mono_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsynth_1to1_mono_z) <= 0 /\ -1 * (s IDsynth_1to1_mono_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsynth_1to1_mono_z) <= 0 /\ 1 * (s IDsynth_1to1_mono_z) <= 0)%Z
    | 4%positive => (1 * (s IDsynth_1to1_mono_z) <= 0 /\ -1 * (s IDsynth_1to1_mono_z) <= 0 /\ 1 * (s IDsynth_1to1_mono_i) <= 0 /\ -1 * (s IDsynth_1to1_mono_i) <= 0)%Z
    | 5%positive => (-1 * (s IDsynth_1to1_mono_i) <= 0 /\ 1 * (s IDsynth_1to1_mono_i) <= 0 /\ -1 * (s IDsynth_1to1_mono_z) <= 0 /\ 1 * (s IDsynth_1to1_mono_z) <= 0)%Z
    | 6%positive => (-1 * (s IDsynth_1to1_mono_z) <= 0 /\ -1 * (s IDsynth_1to1_mono_i) <= 0 /\ 1 * (s IDsynth_1to1_mono_i) + -32 <= 0)%Z
    | 7%positive => (1 * (s IDsynth_1to1_mono_i) + -32 <= 0 /\ -1 * (s IDsynth_1to1_mono_z) <= 0 /\ -1 * (s IDsynth_1to1_mono_i) + 32 <= 0)%Z
    | 8%positive => (-1 * (s IDsynth_1to1_mono_i) + 32 <= 0 /\ -1 * (s IDsynth_1to1_mono_z) <= 0 /\ 1 * (s IDsynth_1to1_mono_i) + -32 <= 0)%Z
    | 9%positive => (1 * (s IDsynth_1to1_mono_i) + -32 <= 0 /\ -1 * (s IDsynth_1to1_mono_z) <= 0 /\ -1 * (s IDsynth_1to1_mono_i) + 32 <= 0)%Z
    | 10%positive => (-1 * (s IDsynth_1to1_mono_i) + 32 <= 0 /\ -1 * (s IDsynth_1to1_mono_z) <= 0 /\ 1 * (s IDsynth_1to1_mono_i) + -32 <= 0)%Z
    | 11%positive => (-1 * (s IDsynth_1to1_mono_i) <= 0 /\ -1 * (s IDsynth_1to1_mono_z) <= 0 /\ 1 * (s IDsynth_1to1_mono_i) + -31 <= 0)%Z
    | 12%positive => (1 * (s IDsynth_1to1_mono_i) + -31 <= 0 /\ -1 * (s IDsynth_1to1_mono_z) <= 0 /\ -1 * (s IDsynth_1to1_mono_i) <= 0)%Z
    | 13%positive => (-1 * (s IDsynth_1to1_mono_i) <= 0 /\ -1 * (s IDsynth_1to1_mono_z) <= 0 /\ 1 * (s IDsynth_1to1_mono_i) + -31 <= 0)%Z
    | 14%positive => (-1 * (s IDsynth_1to1_mono_z) <= 0 /\ -1 * (s IDsynth_1to1_mono_i) + 1 <= 0 /\ 1 * (s IDsynth_1to1_mono_i) + -32 <= 0)%Z
    | 15%positive => (1 * (s IDsynth_1to1_mono_i) + -32 <= 0 /\ -1 * (s IDsynth_1to1_mono_i) + 1 <= 0 /\ -1 * (s IDsynth_1to1_mono_z) <= 0)%Z
    | 16%positive => (-1 * (s IDsynth_1to1_mono_z) <= 0 /\ -1 * (s IDsynth_1to1_mono_i) + 1 <= 0 /\ 1 * (s IDsynth_1to1_mono_i) + -32 <= 0)%Z
    | 17%positive => (1 * (s IDsynth_1to1_mono_i) + -32 <= 0 /\ -1 * (s IDsynth_1to1_mono_i) + 1 <= 0 /\ -1 * (s IDsynth_1to1_mono_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition synth_1to1_mono_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((32 # 1))%Q
    | 2%positive => ((32 # 1) + (s IDsynth_1to1_mono_z))%Q
    | 3%positive => ((32 # 1) + (s IDsynth_1to1_mono_z))%Q
    | 4%positive => ((s IDsynth_1to1_mono_z)
                     + max0(32 - (s IDsynth_1to1_mono_i)))%Q
    | 5%positive => ((s IDsynth_1to1_mono_z)
                     + max0(32 - (s IDsynth_1to1_mono_i)))%Q
    | 6%positive => ((s IDsynth_1to1_mono_z)
                     + max0(32 - (s IDsynth_1to1_mono_i)))%Q
    | 7%positive => ((s IDsynth_1to1_mono_z)
                     + max0(32 - (s IDsynth_1to1_mono_i)))%Q
    | 8%positive => ((s IDsynth_1to1_mono_z)
                     + max0(32 - (s IDsynth_1to1_mono_i)))%Q
    | 9%positive => ((s IDsynth_1to1_mono_z)
                     + max0(32 - (s IDsynth_1to1_mono_i)))%Q
    | 10%positive => ((s IDsynth_1to1_mono_z))%Q
    | 11%positive => ((s IDsynth_1to1_mono_z)
                      + max0(32 - (s IDsynth_1to1_mono_i)))%Q
    | 12%positive => ((1 # 1) + (s IDsynth_1to1_mono_z)
                      + max0(31 - (s IDsynth_1to1_mono_i)))%Q
    | 13%positive => ((1 # 1) + (s IDsynth_1to1_mono_z)
                      + max0(31 - (s IDsynth_1to1_mono_i)))%Q
    | 14%positive => ((1 # 1) + (s IDsynth_1to1_mono_z)
                      + max0(32 - (s IDsynth_1to1_mono_i)))%Q
    | 15%positive => ((1 # 1) + (s IDsynth_1to1_mono_z)
                      + max0(32 - (s IDsynth_1to1_mono_i)))%Q
    | 16%positive => ((1 # 1) + (s IDsynth_1to1_mono_z)
                      + max0(32 - (s IDsynth_1to1_mono_i)))%Q
    | 17%positive => ((s IDsynth_1to1_mono_z)
                      + max0(32 - (s IDsynth_1to1_mono_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition synth_1to1_mono_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (32
                                                            - (s IDsynth_1to1_mono_i)) (31
                                                                    - (s IDsynth_1to1_mono_i)));
                     (*-1 0*) F_max0_ge_0 (31 - (s IDsynth_1to1_mono_i))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_pre_decrement (32
                                                     - (s IDsynth_1to1_mono_i)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | _ => []
  end.


Theorem synth_1to1_mono_ai_correct:
  forall s p' s', steps (g_start synth_1to1_mono) s (g_edges synth_1to1_mono) p' s' -> synth_1to1_mono_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem synth_1to1_mono_pot_correct:
  forall s p' s',
    steps (g_start synth_1to1_mono) s (g_edges synth_1to1_mono) p' s' ->
    (synth_1to1_mono_pot (g_start synth_1to1_mono) s >= synth_1to1_mono_pot p' s')%Q.
Proof.
  check_lp synth_1to1_mono_ai_correct synth_1to1_mono_hints.
Qed.

