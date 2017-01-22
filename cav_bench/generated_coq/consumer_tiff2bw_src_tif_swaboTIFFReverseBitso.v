Require Import pasta.Pasta.

Notation IDTIFFReverseBits_z := 1%positive.
Notation IDTIFFReverseBits__tmp := 2%positive.
Notation IDTIFFReverseBits_cp := 3%positive.
Notation IDTIFFReverseBits_n := 4%positive.
Definition TIFFReverseBits : graph := {|
  g_start := 1%positive;
  g_end := 14%positive;
  g_edges := (1%positive,(AAssign IDTIFFReverseBits_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFReverseBits__tmp) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDTIFFReverseBits__tmp
             (Some (EVar IDTIFFReverseBits_n))),5%positive)::
             (5%positive,ANone,6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFReverseBits__tmp) s) >
             (eval (ENum (8)) s))%Z)),19%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFReverseBits__tmp) s) <=
             (eval (ENum (8)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDTIFFReverseBits__tmp
             (Some (EAdd (EVar IDTIFFReverseBits__tmp) (ENum (-1))))),
             11%positive)::(11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFReverseBits__tmp) s) >
             (eval (ENum (0)) s))%Z)),15%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFReverseBits__tmp) s) <=
             (eval (ENum (0)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDTIFFReverseBits_z (Some (EAdd (ENum (1))
             (EVar IDTIFFReverseBits_z)))),10%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDTIFFReverseBits__tmp
             (Some (ESub (EVar IDTIFFReverseBits__tmp) (ENum (8))))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDTIFFReverseBits_z (Some (EAdd (ENum (1))
             (EVar IDTIFFReverseBits_z)))),25%positive)::
             (25%positive,AWeaken,7%positive)::nil
|}.

Definition TIFFReverseBits_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDTIFFReverseBits_z) <= 0 /\ -1 * (s IDTIFFReverseBits_z) <= 0)%Z
    | 3%positive => (-1 * (s IDTIFFReverseBits_z) <= 0 /\ 1 * (s IDTIFFReverseBits_z) <= 0 /\ -1 * (s IDTIFFReverseBits__tmp) <= 0)%Z
    | 4%positive => (-1 * (s IDTIFFReverseBits__tmp) <= 0 /\ 1 * (s IDTIFFReverseBits_z) <= 0 /\ -1 * (s IDTIFFReverseBits_z) <= 0)%Z
    | 5%positive => (-1 * (s IDTIFFReverseBits_z) <= 0 /\ 1 * (s IDTIFFReverseBits_z) <= 0)%Z
    | 6%positive => (1 * (s IDTIFFReverseBits_z) <= 0 /\ -1 * (s IDTIFFReverseBits_z) <= 0)%Z
    | 7%positive => (-1 * (s IDTIFFReverseBits_z) <= 0)%Z
    | 8%positive => (-1 * (s IDTIFFReverseBits_z) <= 0 /\ 1 * (s IDTIFFReverseBits__tmp) + -8 <= 0)%Z
    | 9%positive => (1 * (s IDTIFFReverseBits__tmp) + -8 <= 0 /\ -1 * (s IDTIFFReverseBits_z) <= 0)%Z
    | 10%positive => (-1 * (s IDTIFFReverseBits_z) <= 0 /\ 1 * (s IDTIFFReverseBits__tmp) + -8 <= 0)%Z
    | 11%positive => (-1 * (s IDTIFFReverseBits_z) <= 0 /\ 1 * (s IDTIFFReverseBits__tmp) + -7 <= 0)%Z
    | 12%positive => (1 * (s IDTIFFReverseBits__tmp) + -7 <= 0 /\ -1 * (s IDTIFFReverseBits_z) <= 0)%Z
    | 13%positive => (-1 * (s IDTIFFReverseBits_z) <= 0 /\ 1 * (s IDTIFFReverseBits__tmp) <= 0)%Z
    | 14%positive => (1 * (s IDTIFFReverseBits__tmp) <= 0 /\ -1 * (s IDTIFFReverseBits_z) <= 0)%Z
    | 15%positive => (-1 * (s IDTIFFReverseBits_z) <= 0 /\ 1 * (s IDTIFFReverseBits__tmp) + -7 <= 0 /\ -1 * (s IDTIFFReverseBits__tmp) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDTIFFReverseBits__tmp) + 1 <= 0 /\ 1 * (s IDTIFFReverseBits__tmp) + -7 <= 0 /\ -1 * (s IDTIFFReverseBits_z) <= 0)%Z
    | 17%positive => (-1 * (s IDTIFFReverseBits_z) <= 0 /\ 1 * (s IDTIFFReverseBits__tmp) + -7 <= 0 /\ -1 * (s IDTIFFReverseBits__tmp) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDTIFFReverseBits__tmp) + 1 <= 0 /\ 1 * (s IDTIFFReverseBits__tmp) + -7 <= 0 /\ -1 * (s IDTIFFReverseBits_z) <= 0)%Z
    | 19%positive => (-1 * (s IDTIFFReverseBits_z) <= 0 /\ -1 * (s IDTIFFReverseBits__tmp) + 9 <= 0)%Z
    | 20%positive => (-1 * (s IDTIFFReverseBits__tmp) + 9 <= 0 /\ -1 * (s IDTIFFReverseBits_z) <= 0)%Z
    | 21%positive => (-1 * (s IDTIFFReverseBits_z) <= 0 /\ -1 * (s IDTIFFReverseBits__tmp) + 9 <= 0)%Z
    | 22%positive => (-1 * (s IDTIFFReverseBits_z) <= 0 /\ -1 * (s IDTIFFReverseBits__tmp) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDTIFFReverseBits__tmp) + 1 <= 0 /\ -1 * (s IDTIFFReverseBits_z) <= 0)%Z
    | 24%positive => (-1 * (s IDTIFFReverseBits_z) <= 0 /\ -1 * (s IDTIFFReverseBits__tmp) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDTIFFReverseBits__tmp) + 1 <= 0 /\ -1 * (s IDTIFFReverseBits_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition TIFFReverseBits_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((7 # 8) * max0(-1 + (s IDTIFFReverseBits_n))
                     + (1 # 8) * max0((s IDTIFFReverseBits_n)))%Q
    | 2%positive => ((s IDTIFFReverseBits_z)
                     + (7 # 8) * max0(-1 + (s IDTIFFReverseBits_n))
                     + (1 # 8) * max0((s IDTIFFReverseBits_n)))%Q
    | 3%positive => ((s IDTIFFReverseBits_z)
                     + (7 # 8) * max0(-1 + (s IDTIFFReverseBits_n))
                     + (1 # 8) * max0((s IDTIFFReverseBits_n)))%Q
    | 4%positive => ((s IDTIFFReverseBits_z)
                     + (7 # 8) * max0(-1 + (s IDTIFFReverseBits_n))
                     + (1 # 8) * max0((s IDTIFFReverseBits_n)))%Q
    | 5%positive => ((s IDTIFFReverseBits_z)
                     + (7 # 8) * max0(-1 + (s IDTIFFReverseBits__tmp))
                     + (1 # 8) * max0((s IDTIFFReverseBits__tmp)))%Q
    | 6%positive => ((s IDTIFFReverseBits_z)
                     + (7 # 8) * max0(-1 + (s IDTIFFReverseBits__tmp))
                     + (1 # 8) * max0((s IDTIFFReverseBits__tmp)))%Q
    | 7%positive => ((s IDTIFFReverseBits_z)
                     + (7 # 8) * max0(-1 + (s IDTIFFReverseBits__tmp))
                     + (1 # 8) * max0((s IDTIFFReverseBits__tmp)))%Q
    | 8%positive => ((s IDTIFFReverseBits_z)
                     + (7 # 8) * max0(-1 + (s IDTIFFReverseBits__tmp))
                     + (1 # 8) * max0((s IDTIFFReverseBits__tmp)))%Q
    | 9%positive => ((s IDTIFFReverseBits_z)
                     + max0(-1 + (s IDTIFFReverseBits__tmp)))%Q
    | 10%positive => ((s IDTIFFReverseBits_z)
                      + max0(-1 + (s IDTIFFReverseBits__tmp)))%Q
    | 11%positive => ((s IDTIFFReverseBits_z)
                      + max0((s IDTIFFReverseBits__tmp)))%Q
    | 12%positive => ((s IDTIFFReverseBits_z)
                      + max0((s IDTIFFReverseBits__tmp)))%Q
    | 13%positive => ((s IDTIFFReverseBits_z)
                      + max0((s IDTIFFReverseBits__tmp)))%Q
    | 14%positive => ((s IDTIFFReverseBits_z))%Q
    | 15%positive => ((s IDTIFFReverseBits_z)
                      + max0((s IDTIFFReverseBits__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDTIFFReverseBits_z)
                      + max0(-1 + (s IDTIFFReverseBits__tmp)))%Q
    | 17%positive => ((1 # 1) + (s IDTIFFReverseBits_z)
                      + max0(-1 + (s IDTIFFReverseBits__tmp)))%Q
    | 18%positive => ((1 # 1) + (s IDTIFFReverseBits_z)
                      + max0(-1 + (s IDTIFFReverseBits__tmp)))%Q
    | 19%positive => ((s IDTIFFReverseBits_z)
                      + (7 # 8) * max0(-1 + (s IDTIFFReverseBits__tmp))
                      + (1 # 8) * max0((s IDTIFFReverseBits__tmp)))%Q
    | 20%positive => ((1 # 1) + (s IDTIFFReverseBits_z)
                      + (7 # 8) * max0(-9 + (s IDTIFFReverseBits__tmp))
                      + (1 # 8) * max0(-8 + (s IDTIFFReverseBits__tmp)))%Q
    | 21%positive => ((1 # 1) + (s IDTIFFReverseBits_z)
                      + (7 # 8) * max0(-9 + (s IDTIFFReverseBits__tmp))
                      + (1 # 8) * max0(-8 + (s IDTIFFReverseBits__tmp)))%Q
    | 22%positive => ((1 # 1) + (s IDTIFFReverseBits_z)
                      + (7 # 8) * max0(-1 + (s IDTIFFReverseBits__tmp))
                      + (1 # 8) * max0((s IDTIFFReverseBits__tmp)))%Q
    | 23%positive => ((1 # 1) + (s IDTIFFReverseBits_z)
                      + (7 # 8) * max0(-1 + (s IDTIFFReverseBits__tmp))
                      + (1 # 8) * max0((s IDTIFFReverseBits__tmp)))%Q
    | 24%positive => ((1 # 1) + (s IDTIFFReverseBits_z)
                      + (7 # 8) * max0(-1 + (s IDTIFFReverseBits__tmp))
                      + (1 # 8) * max0((s IDTIFFReverseBits__tmp)))%Q
    | 25%positive => ((s IDTIFFReverseBits_z)
                      + (7 # 8) * max0(-1 + (s IDTIFFReverseBits__tmp))
                      + (1 # 8) * max0((s IDTIFFReverseBits__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition TIFFReverseBits_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*0 0.125*) F_max0_monotonic (F_check_ge ((s IDTIFFReverseBits__tmp)) (-1
                                                                    + (s IDTIFFReverseBits__tmp)))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-1
                                                             + (s IDTIFFReverseBits__tmp)) (-9
                                                                    + (s IDTIFFReverseBits__tmp)));
                      (*-1 0*) F_max0_ge_0 (-9 + (s IDTIFFReverseBits__tmp));
                      (*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFReverseBits__tmp)) (-1
                                                                    + (s IDTIFFReverseBits__tmp)))]
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_pre_decrement ((s IDTIFFReverseBits__tmp)) (1)]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-0.125 0*) F_max0_pre_decrement ((s IDTIFFReverseBits__tmp)) (8);
                      (*-0.875 0*) F_max0_monotonic (F_check_ge (-1
                                                                 + (s IDTIFFReverseBits__tmp)) (-9
                                                                    + (s IDTIFFReverseBits__tmp)))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | _ => []
  end.


Theorem TIFFReverseBits_ai_correct:
  forall s p' s', steps (g_start TIFFReverseBits) s (g_edges TIFFReverseBits) p' s' -> TIFFReverseBits_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem TIFFReverseBits_pot_correct:
  forall s p' s',
    steps (g_start TIFFReverseBits) s (g_edges TIFFReverseBits) p' s' ->
    (TIFFReverseBits_pot (g_start TIFFReverseBits) s >= TIFFReverseBits_pot p' s')%Q.
Proof.
  check_lp TIFFReverseBits_ai_correct TIFFReverseBits_hints.
Qed.

