Require Import pasta.Pasta.

Notation IDTIFFSwabArrayOfDouble_z := 1%positive.
Notation IDTIFFSwabArrayOfDouble__tmp := 2%positive.
Notation IDTIFFSwabArrayOfDouble_t := 3%positive.
Notation IDTIFFSwabArrayOfDouble_dp := 4%positive.
Notation IDTIFFSwabArrayOfDouble_n := 5%positive.
Definition TIFFSwabArrayOfDouble : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDTIFFSwabArrayOfDouble_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFSwabArrayOfDouble__tmp) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDTIFFSwabArrayOfDouble__tmp
             (Some (EVar IDTIFFSwabArrayOfDouble_n))),5%positive)::
             (5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDTIFFSwabArrayOfDouble__tmp
             (Some (EAdd (EVar IDTIFFSwabArrayOfDouble__tmp) (ENum (-1))))),
             7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFSwabArrayOfDouble__tmp) s) >
             (eval (ENum (0)) s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFSwabArrayOfDouble__tmp) s) <=
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDTIFFSwabArrayOfDouble_t None),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDTIFFSwabArrayOfDouble_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFSwabArrayOfDouble_z)))),
             6%positive)::nil
|}.

Definition TIFFSwabArrayOfDouble_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDTIFFSwabArrayOfDouble_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfDouble_z) <= 0)%Z
    | 3%positive => (-1 * (s IDTIFFSwabArrayOfDouble_z) <= 0 /\ 1 * (s IDTIFFSwabArrayOfDouble_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfDouble__tmp) <= 0)%Z
    | 4%positive => (-1 * (s IDTIFFSwabArrayOfDouble__tmp) <= 0 /\ 1 * (s IDTIFFSwabArrayOfDouble_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfDouble_z) <= 0)%Z
    | 5%positive => (-1 * (s IDTIFFSwabArrayOfDouble_z) <= 0 /\ 1 * (s IDTIFFSwabArrayOfDouble_z) <= 0)%Z
    | 6%positive => (-1 * (s IDTIFFSwabArrayOfDouble_z) <= 0)%Z
    | 7%positive => (-1 * (s IDTIFFSwabArrayOfDouble_z) <= 0)%Z
    | 8%positive => (-1 * (s IDTIFFSwabArrayOfDouble_z) <= 0)%Z
    | 9%positive => (-1 * (s IDTIFFSwabArrayOfDouble_z) <= 0 /\ 1 * (s IDTIFFSwabArrayOfDouble__tmp) <= 0)%Z
    | 10%positive => (1 * (s IDTIFFSwabArrayOfDouble__tmp) <= 0 /\ -1 * (s IDTIFFSwabArrayOfDouble_z) <= 0)%Z
    | 11%positive => (-1 * (s IDTIFFSwabArrayOfDouble_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfDouble__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDTIFFSwabArrayOfDouble__tmp) + 1 <= 0 /\ -1 * (s IDTIFFSwabArrayOfDouble_z) <= 0)%Z
    | 13%positive => (-1 * (s IDTIFFSwabArrayOfDouble_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfDouble__tmp) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDTIFFSwabArrayOfDouble__tmp) + 1 <= 0 /\ -1 * (s IDTIFFSwabArrayOfDouble_z) <= 0)%Z
    | 15%positive => (-1 * (s IDTIFFSwabArrayOfDouble_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfDouble__tmp) + 1 <= 0)%Z
    | _ => False
  end.

Definition TIFFSwabArrayOfDouble_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDTIFFSwabArrayOfDouble_n)))%Q
    | 2%positive => ((s IDTIFFSwabArrayOfDouble_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfDouble_n)))%Q
    | 3%positive => ((s IDTIFFSwabArrayOfDouble_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfDouble_n)))%Q
    | 4%positive => ((s IDTIFFSwabArrayOfDouble_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfDouble_n)))%Q
    | 5%positive => ((s IDTIFFSwabArrayOfDouble_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfDouble__tmp)))%Q
    | 6%positive => ((s IDTIFFSwabArrayOfDouble_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfDouble__tmp)))%Q
    | 7%positive => ((s IDTIFFSwabArrayOfDouble_z)
                     + max0((s IDTIFFSwabArrayOfDouble__tmp)))%Q
    | 8%positive => ((s IDTIFFSwabArrayOfDouble_z)
                     + max0((s IDTIFFSwabArrayOfDouble__tmp)))%Q
    | 9%positive => ((s IDTIFFSwabArrayOfDouble_z)
                     + max0((s IDTIFFSwabArrayOfDouble__tmp)))%Q
    | 10%positive => ((s IDTIFFSwabArrayOfDouble_z))%Q
    | 11%positive => ((s IDTIFFSwabArrayOfDouble_z)
                      + max0((s IDTIFFSwabArrayOfDouble__tmp)))%Q
    | 12%positive => ((1 # 1) + (s IDTIFFSwabArrayOfDouble_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfDouble__tmp)))%Q
    | 13%positive => ((1 # 1) + (s IDTIFFSwabArrayOfDouble_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfDouble__tmp)))%Q
    | 14%positive => ((1 # 1) + (s IDTIFFSwabArrayOfDouble_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfDouble__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDTIFFSwabArrayOfDouble_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfDouble__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition TIFFSwabArrayOfDouble_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFSwabArrayOfDouble__tmp)) (-1
                                                                    + (s IDTIFFSwabArrayOfDouble__tmp)));
                     (*-1 0*) F_max0_ge_0 (-1
                                           + (s IDTIFFSwabArrayOfDouble__tmp))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_pre_decrement ((s IDTIFFSwabArrayOfDouble__tmp)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | _ => []
  end.


Theorem TIFFSwabArrayOfDouble_ai_correct:
  forall s p' s', steps (g_start TIFFSwabArrayOfDouble) s (g_edges TIFFSwabArrayOfDouble) p' s' -> TIFFSwabArrayOfDouble_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem TIFFSwabArrayOfDouble_pot_correct:
  forall s p' s',
    steps (g_start TIFFSwabArrayOfDouble) s (g_edges TIFFSwabArrayOfDouble) p' s' ->
    (TIFFSwabArrayOfDouble_pot (g_start TIFFSwabArrayOfDouble) s >= TIFFSwabArrayOfDouble_pot p' s')%Q.
Proof.
  check_lp TIFFSwabArrayOfDouble_ai_correct TIFFSwabArrayOfDouble_hints.
Qed.

