Require Import pasta.Pasta.

Notation IDcompresspalette_z := 1%positive.
Notation IDcompresspalette_BLUE := 2%positive.
Notation IDcompresspalette_GREEN := 3%positive.
Notation IDcompresspalette_RED := 4%positive.
Notation IDcompresspalette__tmp := 5%positive.
Notation IDcompresspalette_blue := 6%positive.
Notation IDcompresspalette_green := 7%positive.
Notation IDcompresspalette_ix := 8%positive.
Notation IDcompresspalette_red := 9%positive.
Notation IDcompresspalette_v := 10%positive.
Notation IDcompresspalette_bmap := 11%positive.
Notation IDcompresspalette_data := 12%positive.
Notation IDcompresspalette_gmap := 13%positive.
Notation IDcompresspalette_n := 14%positive.
Notation IDcompresspalette_out := 15%positive.
Notation IDcompresspalette_rmap := 16%positive.
Definition compresspalette : graph := {|
  g_start := 1%positive;
  g_end := 13%positive;
  g_edges := (1%positive,(AAssign IDcompresspalette_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDcompresspalette__tmp) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDcompresspalette__tmp
             (Some (EVar IDcompresspalette_n))),5%positive)::
             (5%positive,(AAssign IDcompresspalette_red
             (Some (EVar IDcompresspalette_RED))),6%positive)::
             (6%positive,(AAssign IDcompresspalette_green
             (Some (EVar IDcompresspalette_GREEN))),7%positive)::
             (7%positive,(AAssign IDcompresspalette_blue
             (Some (EVar IDcompresspalette_BLUE))),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDcompresspalette__tmp
             (Some (EAdd (EVar IDcompresspalette__tmp) (ENum (-1))))),
             10%positive)::(10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDcompresspalette__tmp) s) >
             (eval (ENum (0)) s))%Z)),14%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDcompresspalette__tmp) s) <=
             (eval (ENum (0)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDcompresspalette_ix None),16%positive)::
             (16%positive,(AAssign IDcompresspalette_v None),17%positive)::
             (17%positive,(AAssign IDcompresspalette_v None),18%positive)::
             (18%positive,(AAssign IDcompresspalette_v None),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDcompresspalette_z (Some (EAdd (ENum (1))
             (EVar IDcompresspalette_z)))),9%positive)::nil
|}.

Definition compresspalette_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcompresspalette_z) <= 0 /\ -1 * (s IDcompresspalette_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcompresspalette_z) <= 0 /\ 1 * (s IDcompresspalette_z) <= 0 /\ -1 * (s IDcompresspalette__tmp) <= 0)%Z
    | 4%positive => (-1 * (s IDcompresspalette__tmp) <= 0 /\ 1 * (s IDcompresspalette_z) <= 0 /\ -1 * (s IDcompresspalette_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcompresspalette_z) <= 0 /\ 1 * (s IDcompresspalette_z) <= 0)%Z
    | 6%positive => (1 * (s IDcompresspalette_z) <= 0 /\ -1 * (s IDcompresspalette_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcompresspalette_z) <= 0 /\ 1 * (s IDcompresspalette_z) <= 0)%Z
    | 8%positive => (1 * (s IDcompresspalette_z) <= 0 /\ -1 * (s IDcompresspalette_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcompresspalette_z) <= 0)%Z
    | 10%positive => (-1 * (s IDcompresspalette_z) <= 0)%Z
    | 11%positive => (-1 * (s IDcompresspalette_z) <= 0)%Z
    | 12%positive => (-1 * (s IDcompresspalette_z) <= 0 /\ 1 * (s IDcompresspalette__tmp) <= 0)%Z
    | 13%positive => (1 * (s IDcompresspalette__tmp) <= 0 /\ -1 * (s IDcompresspalette_z) <= 0)%Z
    | 14%positive => (-1 * (s IDcompresspalette_z) <= 0 /\ -1 * (s IDcompresspalette__tmp) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDcompresspalette__tmp) + 1 <= 0 /\ -1 * (s IDcompresspalette_z) <= 0)%Z
    | 16%positive => (-1 * (s IDcompresspalette_z) <= 0 /\ -1 * (s IDcompresspalette__tmp) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDcompresspalette__tmp) + 1 <= 0 /\ -1 * (s IDcompresspalette_z) <= 0)%Z
    | 18%positive => (-1 * (s IDcompresspalette_z) <= 0 /\ -1 * (s IDcompresspalette__tmp) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDcompresspalette__tmp) + 1 <= 0 /\ -1 * (s IDcompresspalette_z) <= 0)%Z
    | 20%positive => (-1 * (s IDcompresspalette_z) <= 0 /\ -1 * (s IDcompresspalette__tmp) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDcompresspalette__tmp) + 1 <= 0 /\ -1 * (s IDcompresspalette_z) <= 0)%Z
    | _ => False
  end.

Definition compresspalette_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDcompresspalette_n)))%Q
    | 2%positive => ((s IDcompresspalette_z)
                     + max0(-1 + (s IDcompresspalette_n)))%Q
    | 3%positive => ((s IDcompresspalette_z)
                     + max0(-1 + (s IDcompresspalette_n)))%Q
    | 4%positive => ((s IDcompresspalette_z)
                     + max0(-1 + (s IDcompresspalette_n)))%Q
    | 5%positive => ((s IDcompresspalette_z)
                     + max0(-1 + (s IDcompresspalette__tmp)))%Q
    | 6%positive => ((s IDcompresspalette_z)
                     + max0(-1 + (s IDcompresspalette__tmp)))%Q
    | 7%positive => ((s IDcompresspalette_z)
                     + max0(-1 + (s IDcompresspalette__tmp)))%Q
    | 8%positive => ((s IDcompresspalette_z)
                     + max0(-1 + (s IDcompresspalette__tmp)))%Q
    | 9%positive => ((s IDcompresspalette_z)
                     + max0(-1 + (s IDcompresspalette__tmp)))%Q
    | 10%positive => ((s IDcompresspalette_z)
                      + max0((s IDcompresspalette__tmp)))%Q
    | 11%positive => ((s IDcompresspalette_z)
                      + max0((s IDcompresspalette__tmp)))%Q
    | 12%positive => ((s IDcompresspalette_z)
                      + max0((s IDcompresspalette__tmp)))%Q
    | 13%positive => ((s IDcompresspalette_z))%Q
    | 14%positive => ((s IDcompresspalette_z)
                      + max0((s IDcompresspalette__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDcompresspalette_z)
                      + max0(-1 + (s IDcompresspalette__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDcompresspalette_z)
                      + max0(-1 + (s IDcompresspalette__tmp)))%Q
    | 17%positive => ((1 # 1) + (s IDcompresspalette_z)
                      + max0(-1 + (s IDcompresspalette__tmp)))%Q
    | 18%positive => ((1 # 1) + (s IDcompresspalette_z)
                      + max0(-1 + (s IDcompresspalette__tmp)))%Q
    | 19%positive => ((1 # 1) + (s IDcompresspalette_z)
                      + max0(-1 + (s IDcompresspalette__tmp)))%Q
    | 20%positive => ((1 # 1) + (s IDcompresspalette_z)
                      + max0(-1 + (s IDcompresspalette__tmp)))%Q
    | 21%positive => ((1 # 1) + (s IDcompresspalette_z)
                      + max0(-1 + (s IDcompresspalette__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition compresspalette_hints (p : node) (s : state) := 
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
    | 12%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcompresspalette__tmp)) (-1
                                                                    + (s IDcompresspalette__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDcompresspalette__tmp))]
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_max0_pre_decrement ((s IDcompresspalette__tmp)) (1)]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | _ => []
  end.


Theorem compresspalette_ai_correct:
  forall s p' s', steps (g_start compresspalette) s (g_edges compresspalette) p' s' -> compresspalette_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem compresspalette_pot_correct:
  forall s p' s',
    steps (g_start compresspalette) s (g_edges compresspalette) p' s' ->
    (compresspalette_pot (g_start compresspalette) s >= compresspalette_pot p' s')%Q.
Proof.
  check_lp compresspalette_ai_correct compresspalette_hints.
Qed.

