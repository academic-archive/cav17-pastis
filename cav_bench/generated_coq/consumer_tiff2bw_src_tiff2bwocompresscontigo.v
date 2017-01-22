Require Import pasta.Pasta.

Notation IDcompresscontig_z := 1%positive.
Notation IDcompresscontig_BLUE := 2%positive.
Notation IDcompresscontig_GREEN := 3%positive.
Notation IDcompresscontig_RED := 4%positive.
Notation IDcompresscontig__tmp := 5%positive.
Notation IDcompresscontig_blue := 6%positive.
Notation IDcompresscontig_green := 7%positive.
Notation IDcompresscontig_red := 8%positive.
Notation IDcompresscontig_v := 9%positive.
Notation IDcompresscontig_n := 10%positive.
Notation IDcompresscontig_out := 11%positive.
Notation IDcompresscontig_rgb := 12%positive.
Definition compresscontig : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDcompresscontig_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDcompresscontig__tmp) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDcompresscontig__tmp
             (Some (EVar IDcompresscontig_n))),5%positive)::
             (5%positive,(AAssign IDcompresscontig_red
             (Some (EVar IDcompresscontig_RED))),6%positive)::
             (6%positive,(AAssign IDcompresscontig_green
             (Some (EVar IDcompresscontig_GREEN))),7%positive)::
             (7%positive,(AAssign IDcompresscontig_blue
             (Some (EVar IDcompresscontig_BLUE))),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDcompresscontig__tmp
             (Some (EAdd (EVar IDcompresscontig__tmp) (ENum (-1))))),
             10%positive)::(10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDcompresscontig__tmp) s) >
             (eval (ENum (0)) s))%Z)),14%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDcompresscontig__tmp) s) <=
             (eval (ENum (0)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDcompresscontig_v None),16%positive)::
             (16%positive,(AAssign IDcompresscontig_v None),17%positive)::
             (17%positive,(AAssign IDcompresscontig_v None),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDcompresscontig_z (Some (EAdd (ENum (1))
             (EVar IDcompresscontig_z)))),9%positive)::nil
|}.

Definition compresscontig_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcompresscontig_z) <= 0 /\ -1 * (s IDcompresscontig_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcompresscontig_z) <= 0 /\ 1 * (s IDcompresscontig_z) <= 0 /\ -1 * (s IDcompresscontig__tmp) <= 0)%Z
    | 4%positive => (-1 * (s IDcompresscontig__tmp) <= 0 /\ 1 * (s IDcompresscontig_z) <= 0 /\ -1 * (s IDcompresscontig_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcompresscontig_z) <= 0 /\ 1 * (s IDcompresscontig_z) <= 0)%Z
    | 6%positive => (1 * (s IDcompresscontig_z) <= 0 /\ -1 * (s IDcompresscontig_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcompresscontig_z) <= 0 /\ 1 * (s IDcompresscontig_z) <= 0)%Z
    | 8%positive => (1 * (s IDcompresscontig_z) <= 0 /\ -1 * (s IDcompresscontig_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcompresscontig_z) <= 0)%Z
    | 10%positive => (-1 * (s IDcompresscontig_z) <= 0)%Z
    | 11%positive => (-1 * (s IDcompresscontig_z) <= 0)%Z
    | 12%positive => (-1 * (s IDcompresscontig_z) <= 0 /\ 1 * (s IDcompresscontig__tmp) <= 0)%Z
    | 13%positive => (1 * (s IDcompresscontig__tmp) <= 0 /\ -1 * (s IDcompresscontig_z) <= 0)%Z
    | 14%positive => (-1 * (s IDcompresscontig_z) <= 0 /\ -1 * (s IDcompresscontig__tmp) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDcompresscontig__tmp) + 1 <= 0 /\ -1 * (s IDcompresscontig_z) <= 0)%Z
    | 16%positive => (-1 * (s IDcompresscontig_z) <= 0 /\ -1 * (s IDcompresscontig__tmp) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDcompresscontig__tmp) + 1 <= 0 /\ -1 * (s IDcompresscontig_z) <= 0)%Z
    | 18%positive => (-1 * (s IDcompresscontig_z) <= 0 /\ -1 * (s IDcompresscontig__tmp) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDcompresscontig__tmp) + 1 <= 0 /\ -1 * (s IDcompresscontig_z) <= 0)%Z
    | 20%positive => (-1 * (s IDcompresscontig_z) <= 0 /\ -1 * (s IDcompresscontig__tmp) + 1 <= 0)%Z
    | _ => False
  end.

Definition compresscontig_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDcompresscontig_n)))%Q
    | 2%positive => ((s IDcompresscontig_z)
                     + max0(-1 + (s IDcompresscontig_n)))%Q
    | 3%positive => ((s IDcompresscontig_z)
                     + max0(-1 + (s IDcompresscontig_n)))%Q
    | 4%positive => ((s IDcompresscontig_z)
                     + max0(-1 + (s IDcompresscontig_n)))%Q
    | 5%positive => ((s IDcompresscontig_z)
                     + max0(-1 + (s IDcompresscontig__tmp)))%Q
    | 6%positive => ((s IDcompresscontig_z)
                     + max0(-1 + (s IDcompresscontig__tmp)))%Q
    | 7%positive => ((s IDcompresscontig_z)
                     + max0(-1 + (s IDcompresscontig__tmp)))%Q
    | 8%positive => ((s IDcompresscontig_z)
                     + max0(-1 + (s IDcompresscontig__tmp)))%Q
    | 9%positive => ((s IDcompresscontig_z)
                     + max0(-1 + (s IDcompresscontig__tmp)))%Q
    | 10%positive => ((s IDcompresscontig_z)
                      + max0((s IDcompresscontig__tmp)))%Q
    | 11%positive => ((s IDcompresscontig_z)
                      + max0((s IDcompresscontig__tmp)))%Q
    | 12%positive => ((s IDcompresscontig_z)
                      + max0((s IDcompresscontig__tmp)))%Q
    | 13%positive => ((s IDcompresscontig_z))%Q
    | 14%positive => ((s IDcompresscontig_z)
                      + max0((s IDcompresscontig__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDcompresscontig_z)
                      + max0(-1 + (s IDcompresscontig__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDcompresscontig_z)
                      + max0(-1 + (s IDcompresscontig__tmp)))%Q
    | 17%positive => ((1 # 1) + (s IDcompresscontig_z)
                      + max0(-1 + (s IDcompresscontig__tmp)))%Q
    | 18%positive => ((1 # 1) + (s IDcompresscontig_z)
                      + max0(-1 + (s IDcompresscontig__tmp)))%Q
    | 19%positive => ((1 # 1) + (s IDcompresscontig_z)
                      + max0(-1 + (s IDcompresscontig__tmp)))%Q
    | 20%positive => ((1 # 1) + (s IDcompresscontig_z)
                      + max0(-1 + (s IDcompresscontig__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition compresscontig_hints (p : node) (s : state) := 
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
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcompresscontig__tmp)) (-1
                                                                    + (s IDcompresscontig__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDcompresscontig__tmp))]
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_pre_decrement ((s IDcompresscontig__tmp)) (1)]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | _ => []
  end.


Theorem compresscontig_ai_correct:
  forall s p' s', steps (g_start compresscontig) s (g_edges compresscontig) p' s' -> compresscontig_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem compresscontig_pot_correct:
  forall s p' s',
    steps (g_start compresscontig) s (g_edges compresscontig) p' s' ->
    (compresscontig_pot (g_start compresscontig) s >= compresscontig_pot p' s')%Q.
Proof.
  check_lp compresscontig_ai_correct compresscontig_hints.
Qed.

