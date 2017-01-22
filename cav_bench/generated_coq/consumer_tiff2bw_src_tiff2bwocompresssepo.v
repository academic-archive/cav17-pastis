Require Import pasta.Pasta.

Notation IDcompresssep_z := 1%positive.
Notation IDcompresssep_BLUE := 2%positive.
Notation IDcompresssep_GREEN := 3%positive.
Notation IDcompresssep_RED := 4%positive.
Notation IDcompresssep__tmp := 5%positive.
Notation IDcompresssep_blue := 6%positive.
Notation IDcompresssep_green := 7%positive.
Notation IDcompresssep_red := 8%positive.
Notation IDcompresssep_b := 9%positive.
Notation IDcompresssep_g := 10%positive.
Notation IDcompresssep_n := 11%positive.
Notation IDcompresssep_out := 12%positive.
Notation IDcompresssep_r := 13%positive.
Definition compresssep : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDcompresssep_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDcompresssep__tmp)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDcompresssep__tmp
             (Some (EVar IDcompresssep_n))),5%positive)::
             (5%positive,(AAssign IDcompresssep_red
             (Some (EVar IDcompresssep_RED))),6%positive)::
             (6%positive,(AAssign IDcompresssep_green
             (Some (EVar IDcompresssep_GREEN))),7%positive)::
             (7%positive,(AAssign IDcompresssep_blue
             (Some (EVar IDcompresssep_BLUE))),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDcompresssep__tmp
             (Some (EAdd (EVar IDcompresssep__tmp) (ENum (-1))))),
             10%positive)::(10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDcompresssep__tmp)
             s) > (eval (ENum (0)) s))%Z)),14%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDcompresssep__tmp)
             s) <= (eval (ENum (0)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDcompresssep_z (Some (EAdd (ENum (1))
             (EVar IDcompresssep_z)))),9%positive)::nil
|}.

Definition compresssep_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcompresssep_z) <= 0 /\ -1 * (s IDcompresssep_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcompresssep_z) <= 0 /\ 1 * (s IDcompresssep_z) <= 0 /\ -1 * (s IDcompresssep__tmp) <= 0)%Z
    | 4%positive => (-1 * (s IDcompresssep__tmp) <= 0 /\ 1 * (s IDcompresssep_z) <= 0 /\ -1 * (s IDcompresssep_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcompresssep_z) <= 0 /\ 1 * (s IDcompresssep_z) <= 0)%Z
    | 6%positive => (1 * (s IDcompresssep_z) <= 0 /\ -1 * (s IDcompresssep_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcompresssep_z) <= 0 /\ 1 * (s IDcompresssep_z) <= 0)%Z
    | 8%positive => (1 * (s IDcompresssep_z) <= 0 /\ -1 * (s IDcompresssep_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcompresssep_z) <= 0)%Z
    | 10%positive => (-1 * (s IDcompresssep_z) <= 0)%Z
    | 11%positive => (-1 * (s IDcompresssep_z) <= 0)%Z
    | 12%positive => (-1 * (s IDcompresssep_z) <= 0 /\ 1 * (s IDcompresssep__tmp) <= 0)%Z
    | 13%positive => (1 * (s IDcompresssep__tmp) <= 0 /\ -1 * (s IDcompresssep_z) <= 0)%Z
    | 14%positive => (-1 * (s IDcompresssep_z) <= 0 /\ -1 * (s IDcompresssep__tmp) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDcompresssep__tmp) + 1 <= 0 /\ -1 * (s IDcompresssep_z) <= 0)%Z
    | 16%positive => (-1 * (s IDcompresssep_z) <= 0 /\ -1 * (s IDcompresssep__tmp) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDcompresssep__tmp) + 1 <= 0 /\ -1 * (s IDcompresssep_z) <= 0)%Z
    | _ => False
  end.

Definition compresssep_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDcompresssep_n)))%Q
    | 2%positive => ((s IDcompresssep_z) + max0(-1 + (s IDcompresssep_n)))%Q
    | 3%positive => ((s IDcompresssep_z) + max0(-1 + (s IDcompresssep_n)))%Q
    | 4%positive => ((s IDcompresssep_z) + max0(-1 + (s IDcompresssep_n)))%Q
    | 5%positive => ((s IDcompresssep_z) + max0(-1 + (s IDcompresssep__tmp)))%Q
    | 6%positive => ((s IDcompresssep_z) + max0(-1 + (s IDcompresssep__tmp)))%Q
    | 7%positive => ((s IDcompresssep_z) + max0(-1 + (s IDcompresssep__tmp)))%Q
    | 8%positive => ((s IDcompresssep_z) + max0(-1 + (s IDcompresssep__tmp)))%Q
    | 9%positive => ((s IDcompresssep_z) + max0(-1 + (s IDcompresssep__tmp)))%Q
    | 10%positive => ((s IDcompresssep_z) + max0((s IDcompresssep__tmp)))%Q
    | 11%positive => ((s IDcompresssep_z) + max0((s IDcompresssep__tmp)))%Q
    | 12%positive => ((s IDcompresssep_z) + max0((s IDcompresssep__tmp)))%Q
    | 13%positive => ((s IDcompresssep_z))%Q
    | 14%positive => ((s IDcompresssep_z) + max0((s IDcompresssep__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDcompresssep_z)
                      + max0(-1 + (s IDcompresssep__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDcompresssep_z)
                      + max0(-1 + (s IDcompresssep__tmp)))%Q
    | 17%positive => ((1 # 1) + (s IDcompresssep_z)
                      + max0(-1 + (s IDcompresssep__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition compresssep_hints (p : node) (s : state) := 
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
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcompresssep__tmp)) (-1
                                                                    + (s IDcompresssep__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDcompresssep__tmp))]
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_pre_decrement ((s IDcompresssep__tmp)) (1)]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | _ => []
  end.


Theorem compresssep_ai_correct:
  forall s p' s', steps (g_start compresssep) s (g_edges compresssep) p' s' -> compresssep_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem compresssep_pot_correct:
  forall s p' s',
    steps (g_start compresssep) s (g_edges compresssep) p' s' ->
    (compresssep_pot (g_start compresssep) s >= compresssep_pot p' s')%Q.
Proof.
  check_lp compresssep_ai_correct compresssep_hints.
Qed.

