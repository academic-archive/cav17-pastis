Require Import pasta.Pasta.

Notation IDtbldump_z := 1%positive.
Notation IDtbldump__tmp := 2%positive.
Notation IDtbldump_flagp := 3%positive.
Notation IDtbldump_numflags := 4%positive.
Definition tbldump : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDtbldump_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDtbldump__tmp
             (Some (EVar IDtbldump_numflags))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDtbldump__tmp
             (Some (EAdd (EVar IDtbldump__tmp) (ENum (-1))))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EAdd (EVar IDtbldump__tmp)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EAdd (EVar IDtbldump__tmp)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDtbldump_z (Some (EAdd (ENum (1))
             (EVar IDtbldump_z)))),4%positive)::nil
|}.

Definition tbldump_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDtbldump_z) <= 0 /\ -1 * (s IDtbldump_z) <= 0)%Z
    | 3%positive => (-1 * (s IDtbldump_z) <= 0 /\ 1 * (s IDtbldump_z) <= 0)%Z
    | 4%positive => (-1 * (s IDtbldump_z) <= 0)%Z
    | 5%positive => (-1 * (s IDtbldump_z) <= 0)%Z
    | 6%positive => (-1 * (s IDtbldump_z) <= 0)%Z
    | 7%positive => (-1 * (s IDtbldump_z) <= 0 /\ 1 * (s IDtbldump__tmp) <= 0)%Z
    | 8%positive => (1 * (s IDtbldump__tmp) <= 0 /\ -1 * (s IDtbldump_z) <= 0)%Z
    | 9%positive => (-1 * (s IDtbldump_z) <= 0 /\ -1 * (s IDtbldump__tmp) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDtbldump__tmp) + 1 <= 0 /\ -1 * (s IDtbldump_z) <= 0)%Z
    | 11%positive => (-1 * (s IDtbldump_z) <= 0 /\ -1 * (s IDtbldump__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDtbldump__tmp) + 1 <= 0 /\ -1 * (s IDtbldump_z) <= 0)%Z
    | _ => False
  end.

Definition tbldump_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDtbldump_numflags)))%Q
    | 2%positive => ((s IDtbldump_z) + max0(-1 + (s IDtbldump_numflags)))%Q
    | 3%positive => ((s IDtbldump_z) + max0(-1 + (s IDtbldump__tmp)))%Q
    | 4%positive => ((s IDtbldump_z) + max0(-1 + (s IDtbldump__tmp)))%Q
    | 5%positive => ((s IDtbldump_z) + max0((s IDtbldump__tmp)))%Q
    | 6%positive => ((s IDtbldump_z) + max0((s IDtbldump__tmp)))%Q
    | 7%positive => ((s IDtbldump_z) + max0((s IDtbldump__tmp)))%Q
    | 8%positive => ((s IDtbldump_z))%Q
    | 9%positive => ((s IDtbldump_z) + max0((s IDtbldump__tmp)))%Q
    | 10%positive => ((1 # 1) + (s IDtbldump_z)
                      + max0(-1 + (s IDtbldump__tmp)))%Q
    | 11%positive => ((1 # 1) + (s IDtbldump_z)
                      + max0(-1 + (s IDtbldump__tmp)))%Q
    | 12%positive => ((1 # 1) + (s IDtbldump_z)
                      + max0(-1 + (s IDtbldump__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition tbldump_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDtbldump__tmp)) (-1
                                                                    + (s IDtbldump__tmp)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDtbldump__tmp))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_pre_decrement ((s IDtbldump__tmp)) (1)]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | _ => []
  end.


Theorem tbldump_ai_correct:
  forall s p' s', steps (g_start tbldump) s (g_edges tbldump) p' s' -> tbldump_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem tbldump_pot_correct:
  forall s p' s',
    steps (g_start tbldump) s (g_edges tbldump) p' s' ->
    (tbldump_pot (g_start tbldump) s >= tbldump_pot p' s')%Q.
Proof.
  check_lp tbldump_ai_correct tbldump_hints.
Qed.

