Require Import pasta.Pasta.

Notation IDsetdump_z := 1%positive.
Notation IDsetdump__tmp := 2%positive.
Notation IDsetdump_cnum := 3%positive.
Notation IDsetdump_firstnz := 4%positive.
Notation IDsetdump_numnz := 5%positive.
Notation IDsetdump_mask := 6%positive.
Notation IDsetdump_setp := 7%positive.
Definition setdump : graph := {|
  g_start := 1%positive;
  g_end := 28%positive;
  g_edges := (1%positive,(AAssign IDsetdump_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDsetdump__tmp
             (Some (EVar IDsetdump_mask))),3%positive)::
             (3%positive,(AAssign IDsetdump_numnz (Some (ENum (0)))),
             4%positive)::
             (4%positive,(AAssign IDsetdump_firstnz (Some (ENum (0)))),
             5%positive)::
             (5%positive,(AAssign IDsetdump_cnum (Some (ENum (128)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,(AAssign IDsetdump_cnum
             (Some (EAdd (EVar IDsetdump_cnum) (ENum (-1))))),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EAdd (EVar IDsetdump_cnum)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),29%positive)::
             (9%positive,(AGuard (fun s => ((eval (EAdd (EVar IDsetdump_cnum)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDsetdump_numnz)
             s) = (eval (ENum (1)) s))%Z)),25%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDsetdump_numnz)
             s) <> (eval (ENum (1)) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDsetdump_numnz)
             s) = (eval (ENum (128)) s))%Z)),21%positive)::
             (13%positive,(AGuard (fun s => ((eval (EVar IDsetdump_numnz)
             s) <> (eval (ENum (128)) s))%Z)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDsetdump_numnz)
             s) > (eval (ENum (64)) s))%Z)),18%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDsetdump_numnz)
             s) <= (eval (ENum (64)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,20%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,23%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,28%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,31%positive)::
             (30%positive,ANone,34%positive)::
             (31%positive,(AAssign IDsetdump_numnz
             (Some (EAdd (EVar IDsetdump_numnz) (ENum (1))))),32%positive)::
             (32%positive,(AAssign IDsetdump_firstnz
             (Some (EVar IDsetdump_cnum))),33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDsetdump_z (Some (EAdd (ENum (1))
             (EVar IDsetdump_z)))),7%positive)::nil
|}.

Definition setdump_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsetdump_z) <= 0 /\ 1 * (s IDsetdump_z) <= 0)%Z
    | 4%positive => (1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ 1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0)%Z
    | 5%positive => (-1 * (s IDsetdump_numnz) <= 0 /\ 1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ 1 * (s IDsetdump_z) <= 0 /\ 1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 6%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ 1 * (s IDsetdump_firstnz) <= 0 /\ 1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ 1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ 1 * (s IDsetdump_cnum) + -128 <= 0 /\ -1 * (s IDsetdump_cnum) + 128 <= 0)%Z
    | 7%positive => (1 * (s IDsetdump_cnum) + -128 <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_cnum) + 1 <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 8%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ 1 * (s IDsetdump_cnum) + -127 <= 0 /\ -1 * (s IDsetdump_cnum) <= 0)%Z
    | 9%positive => (-1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_cnum) + -127 <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 10%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_cnum) <= 0)%Z
    | 11%positive => (1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 12%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_cnum) <= 0)%Z
    | 13%positive => (1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 14%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_cnum) <= 0)%Z
    | 15%positive => (1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 16%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_numnz) + -64 <= 0)%Z
    | 17%positive => (1 * (s IDsetdump_numnz) + -64 <= 0 /\ 1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 18%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_numnz) + 65 <= 0)%Z
    | 19%positive => (-1 * (s IDsetdump_numnz) + 65 <= 0 /\ 1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 20%positive => (-1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_cnum) <= 0)%Z
    | 21%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_numnz) + -128 <= 0 /\ -1 * (s IDsetdump_numnz) + 128 <= 0)%Z
    | 22%positive => (-1 * (s IDsetdump_numnz) + 128 <= 0 /\ 1 * (s IDsetdump_numnz) + -128 <= 0 /\ 1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 23%positive => (-1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_cnum) <= 0)%Z
    | 24%positive => (1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0)%Z
    | 25%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_numnz) + -1 <= 0 /\ -1 * (s IDsetdump_numnz) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDsetdump_numnz) + 1 <= 0 /\ 1 * (s IDsetdump_numnz) + -1 <= 0 /\ 1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 27%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_cnum) <= 0 /\ 1 * (s IDsetdump_numnz) + -1 <= 0 /\ -1 * (s IDsetdump_numnz) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDsetdump_numnz) <= 0 /\ 1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_cnum) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 29%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ 1 * (s IDsetdump_cnum) + -127 <= 0 /\ -1 * (s IDsetdump_cnum) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDsetdump_cnum) + 1 <= 0 /\ 1 * (s IDsetdump_cnum) + -127 <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 31%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ 1 * (s IDsetdump_cnum) + -127 <= 0 /\ -1 * (s IDsetdump_cnum) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDsetdump_cnum) + 1 <= 0 /\ 1 * (s IDsetdump_cnum) + -127 <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_numnz) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDsetdump_numnz) + 1 <= 0 /\ -1 * (s IDsetdump_z) <= 0 /\ 1 * (s IDsetdump_cnum) + -127 <= 0 /\ -1 * (s IDsetdump_cnum) + 1 <= 0 /\ 1 * (s IDsetdump_firstnz) + -127 <= 0 /\ -1 * (s IDsetdump_firstnz) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_cnum) + 1 <= 0 /\ 1 * (s IDsetdump_cnum) + -127 <= 0 /\ -1 * (s IDsetdump_z) <= 0)%Z
    | 35%positive => (-1 * (s IDsetdump_z) <= 0 /\ 1 * (s IDsetdump_cnum) + -127 <= 0 /\ -1 * (s IDsetdump_cnum) + 1 <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_firstnz) <= 0)%Z
    | 36%positive => (-1 * (s IDsetdump_firstnz) <= 0 /\ -1 * (s IDsetdump_numnz) <= 0 /\ -1 * (s IDsetdump_cnum) + 1 <= 0 /\ 1 * (s IDsetdump_cnum) + -127 <= 0 /\ -1 * (s IDsetdump_z) <= 0)%Z
    | _ => False
  end.

Definition setdump_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((127 # 1))%Q
    | 2%positive => ((127 # 1) + (s IDsetdump_z))%Q
    | 3%positive => ((127 # 1) + (s IDsetdump_z))%Q
    | 4%positive => ((127 # 1) + (s IDsetdump_z))%Q
    | 5%positive => ((127 # 1) + (s IDsetdump_z))%Q
    | 6%positive => ((s IDsetdump_z) + max0(-1 + (s IDsetdump_cnum)))%Q
    | 7%positive => ((s IDsetdump_z) + max0(-1 + (s IDsetdump_cnum)))%Q
    | 8%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 9%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 10%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 11%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 12%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 13%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 14%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 15%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 16%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 17%positive => ((s IDsetdump_z))%Q
    | 18%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 19%positive => ((s IDsetdump_z))%Q
    | 20%positive => ((s IDsetdump_z))%Q
    | 21%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 22%positive => ((s IDsetdump_z))%Q
    | 23%positive => ((s IDsetdump_z))%Q
    | 24%positive => ((s IDsetdump_z))%Q
    | 25%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 26%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 27%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 28%positive => ((s IDsetdump_z))%Q
    | 29%positive => ((s IDsetdump_z) + max0((s IDsetdump_cnum)))%Q
    | 30%positive => ((1 # 1) + (s IDsetdump_z)
                      + max0(-1 + (s IDsetdump_cnum)))%Q
    | 31%positive => ((1 # 1) + (s IDsetdump_z)
                      + max0(-1 + (s IDsetdump_cnum)))%Q
    | 32%positive => ((1 # 1) + (s IDsetdump_z)
                      + max0(-1 + (s IDsetdump_cnum)))%Q
    | 33%positive => ((1 # 1) + (s IDsetdump_z)
                      + max0(-1 + (s IDsetdump_cnum)))%Q
    | 34%positive => ((1 # 1) + (s IDsetdump_z)
                      + max0(-1 + (s IDsetdump_cnum)))%Q
    | 35%positive => ((1 # 1) + (s IDsetdump_z)
                      + max0(-1 + (s IDsetdump_cnum)))%Q
    | 36%positive => ((1 # 1) + (s IDsetdump_z)
                      + max0(-1 + (s IDsetdump_cnum)))%Q
    | _ => (0 # 1)%Q
  end.

Definition setdump_hints (p : node) (s : state) := 
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
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDsetdump_cnum)) (-1
                                                                    + (s IDsetdump_cnum)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDsetdump_cnum))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDsetdump_cnum)) (-1
                                                                    + (s IDsetdump_cnum)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDsetdump_cnum))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDsetdump_cnum)) (-1
                                                                    + (s IDsetdump_cnum)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDsetdump_cnum))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDsetdump_cnum)) (-1
                                                                    + (s IDsetdump_cnum)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDsetdump_cnum))]
    | 28%positive => []
    | 29%positive => [(*-1 0*) F_max0_pre_decrement ((s IDsetdump_cnum)) (1)]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | _ => []
  end.


Theorem setdump_ai_correct:
  forall s p' s', steps (g_start setdump) s (g_edges setdump) p' s' -> setdump_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem setdump_pot_correct:
  forall s p' s',
    steps (g_start setdump) s (g_edges setdump) p' s' ->
    (setdump_pot (g_start setdump) s >= setdump_pot p' s')%Q.
Proof.
  check_lp setdump_ai_correct setdump_hints.
Qed.

