Require Import pasta.Pasta.

Notation IDspell_out_z := 1%positive.
Notation IDspell_out__tmp := 2%positive.
Notation IDspell_out_nph := 3%positive.
Notation IDspell_out_n := 4%positive.
Notation IDspell_out_phone := 5%positive.
Notation IDspell_out_word := 6%positive.
Definition spell_out : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDspell_out_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDspell_out__tmp
             (Some (EVar IDspell_out_n))),3%positive)::
             (3%positive,(AAssign IDspell_out_nph (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDspell_out__tmp
             (Some (EAdd (EVar IDspell_out__tmp) (ENum (-1))))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDspell_out__tmp)
             s) > (eval (ENum (0)) s))%Z)),10%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDspell_out__tmp)
             s) <= (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDspell_out_nph None),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDspell_out_z (Some (EAdd (ENum (1))
             (EVar IDspell_out_z)))),5%positive)::nil
|}.

Definition spell_out_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDspell_out_z) <= 0 /\ -1 * (s IDspell_out_z) <= 0)%Z
    | 3%positive => (-1 * (s IDspell_out_z) <= 0 /\ 1 * (s IDspell_out_z) <= 0)%Z
    | 4%positive => (1 * (s IDspell_out_z) <= 0 /\ -1 * (s IDspell_out_z) <= 0 /\ 1 * (s IDspell_out_nph) <= 0 /\ -1 * (s IDspell_out_nph) <= 0)%Z
    | 5%positive => (-1 * (s IDspell_out_z) <= 0)%Z
    | 6%positive => (-1 * (s IDspell_out_z) <= 0)%Z
    | 7%positive => (-1 * (s IDspell_out_z) <= 0)%Z
    | 8%positive => (-1 * (s IDspell_out_z) <= 0 /\ 1 * (s IDspell_out__tmp) <= 0)%Z
    | 9%positive => (1 * (s IDspell_out__tmp) <= 0 /\ -1 * (s IDspell_out_z) <= 0)%Z
    | 10%positive => (-1 * (s IDspell_out_z) <= 0 /\ -1 * (s IDspell_out__tmp) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDspell_out__tmp) + 1 <= 0 /\ -1 * (s IDspell_out_z) <= 0)%Z
    | 12%positive => (-1 * (s IDspell_out_z) <= 0 /\ -1 * (s IDspell_out__tmp) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDspell_out__tmp) + 1 <= 0 /\ -1 * (s IDspell_out_z) <= 0)%Z
    | 14%positive => (-1 * (s IDspell_out_z) <= 0 /\ -1 * (s IDspell_out__tmp) + 1 <= 0)%Z
    | _ => False
  end.

Definition spell_out_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDspell_out_n)))%Q
    | 2%positive => ((s IDspell_out_z) + max0(-1 + (s IDspell_out_n)))%Q
    | 3%positive => ((s IDspell_out_z) + max0(-1 + (s IDspell_out__tmp)))%Q
    | 4%positive => ((s IDspell_out_z) + max0(-1 + (s IDspell_out__tmp)))%Q
    | 5%positive => ((s IDspell_out_z) + max0(-1 + (s IDspell_out__tmp)))%Q
    | 6%positive => ((s IDspell_out_z) + max0((s IDspell_out__tmp)))%Q
    | 7%positive => ((s IDspell_out_z) + max0((s IDspell_out__tmp)))%Q
    | 8%positive => ((s IDspell_out_z) + max0((s IDspell_out__tmp)))%Q
    | 9%positive => ((s IDspell_out_z))%Q
    | 10%positive => ((s IDspell_out_z) + max0((s IDspell_out__tmp)))%Q
    | 11%positive => ((1 # 1) + (s IDspell_out_z)
                      + max0(-1 + (s IDspell_out__tmp)))%Q
    | 12%positive => ((1 # 1) + (s IDspell_out_z)
                      + max0(-1 + (s IDspell_out__tmp)))%Q
    | 13%positive => ((1 # 1) + (s IDspell_out_z)
                      + max0(-1 + (s IDspell_out__tmp)))%Q
    | 14%positive => ((1 # 1) + (s IDspell_out_z)
                      + max0(-1 + (s IDspell_out__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition spell_out_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDspell_out__tmp)) (-1
                                                                    + (s IDspell_out__tmp)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDspell_out__tmp))]
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_max0_pre_decrement ((s IDspell_out__tmp)) (1)]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem spell_out_ai_correct:
  forall s p' s', steps (g_start spell_out) s (g_edges spell_out) p' s' -> spell_out_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem spell_out_pot_correct:
  forall s p' s',
    steps (g_start spell_out) s (g_edges spell_out) p' s' ->
    (spell_out_pot (g_start spell_out) s >= spell_out_pot p' s')%Q.
Proof.
  check_lp spell_out_ai_correct spell_out_hints.
Qed.

