Require Import pasta.Pasta.

Notation IDTIFFSwabArrayOfShort_z := 1%positive.
Notation IDTIFFSwabArrayOfShort__tmp := 2%positive.
Notation IDTIFFSwabArrayOfShort_t := 3%positive.
Notation IDTIFFSwabArrayOfShort_n := 4%positive.
Notation IDTIFFSwabArrayOfShort_wp := 5%positive.
Definition TIFFSwabArrayOfShort : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDTIFFSwabArrayOfShort_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFSwabArrayOfShort__tmp) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDTIFFSwabArrayOfShort__tmp
             (Some (EVar IDTIFFSwabArrayOfShort_n))),5%positive)::
             (5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDTIFFSwabArrayOfShort__tmp
             (Some (EAdd (EVar IDTIFFSwabArrayOfShort__tmp) (ENum (-1))))),
             7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFSwabArrayOfShort__tmp) s) >
             (eval (ENum (0)) s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFSwabArrayOfShort__tmp) s) <=
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDTIFFSwabArrayOfShort_t None),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDTIFFSwabArrayOfShort_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFSwabArrayOfShort_z)))),
             6%positive)::nil
|}.

Definition TIFFSwabArrayOfShort_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDTIFFSwabArrayOfShort_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfShort_z) <= 0)%Z
    | 3%positive => (-1 * (s IDTIFFSwabArrayOfShort_z) <= 0 /\ 1 * (s IDTIFFSwabArrayOfShort_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfShort__tmp) <= 0)%Z
    | 4%positive => (-1 * (s IDTIFFSwabArrayOfShort__tmp) <= 0 /\ 1 * (s IDTIFFSwabArrayOfShort_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfShort_z) <= 0)%Z
    | 5%positive => (-1 * (s IDTIFFSwabArrayOfShort_z) <= 0 /\ 1 * (s IDTIFFSwabArrayOfShort_z) <= 0)%Z
    | 6%positive => (-1 * (s IDTIFFSwabArrayOfShort_z) <= 0)%Z
    | 7%positive => (-1 * (s IDTIFFSwabArrayOfShort_z) <= 0)%Z
    | 8%positive => (-1 * (s IDTIFFSwabArrayOfShort_z) <= 0)%Z
    | 9%positive => (-1 * (s IDTIFFSwabArrayOfShort_z) <= 0 /\ 1 * (s IDTIFFSwabArrayOfShort__tmp) <= 0)%Z
    | 10%positive => (1 * (s IDTIFFSwabArrayOfShort__tmp) <= 0 /\ -1 * (s IDTIFFSwabArrayOfShort_z) <= 0)%Z
    | 11%positive => (-1 * (s IDTIFFSwabArrayOfShort_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfShort__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDTIFFSwabArrayOfShort__tmp) + 1 <= 0 /\ -1 * (s IDTIFFSwabArrayOfShort_z) <= 0)%Z
    | 13%positive => (-1 * (s IDTIFFSwabArrayOfShort_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfShort__tmp) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDTIFFSwabArrayOfShort__tmp) + 1 <= 0 /\ -1 * (s IDTIFFSwabArrayOfShort_z) <= 0)%Z
    | 15%positive => (-1 * (s IDTIFFSwabArrayOfShort_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfShort__tmp) + 1 <= 0)%Z
    | _ => False
  end.

Definition TIFFSwabArrayOfShort_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDTIFFSwabArrayOfShort_n)))%Q
    | 2%positive => ((s IDTIFFSwabArrayOfShort_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfShort_n)))%Q
    | 3%positive => ((s IDTIFFSwabArrayOfShort_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfShort_n)))%Q
    | 4%positive => ((s IDTIFFSwabArrayOfShort_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfShort_n)))%Q
    | 5%positive => ((s IDTIFFSwabArrayOfShort_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfShort__tmp)))%Q
    | 6%positive => ((s IDTIFFSwabArrayOfShort_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfShort__tmp)))%Q
    | 7%positive => ((s IDTIFFSwabArrayOfShort_z)
                     + max0((s IDTIFFSwabArrayOfShort__tmp)))%Q
    | 8%positive => ((s IDTIFFSwabArrayOfShort_z)
                     + max0((s IDTIFFSwabArrayOfShort__tmp)))%Q
    | 9%positive => ((s IDTIFFSwabArrayOfShort_z)
                     + max0((s IDTIFFSwabArrayOfShort__tmp)))%Q
    | 10%positive => ((s IDTIFFSwabArrayOfShort_z))%Q
    | 11%positive => ((s IDTIFFSwabArrayOfShort_z)
                      + max0((s IDTIFFSwabArrayOfShort__tmp)))%Q
    | 12%positive => ((1 # 1) + (s IDTIFFSwabArrayOfShort_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfShort__tmp)))%Q
    | 13%positive => ((1 # 1) + (s IDTIFFSwabArrayOfShort_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfShort__tmp)))%Q
    | 14%positive => ((1 # 1) + (s IDTIFFSwabArrayOfShort_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfShort__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDTIFFSwabArrayOfShort_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfShort__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition TIFFSwabArrayOfShort_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFSwabArrayOfShort__tmp)) (-1
                                                                    + (s IDTIFFSwabArrayOfShort__tmp)));
                     (*-1 0*) F_max0_ge_0 (-1
                                           + (s IDTIFFSwabArrayOfShort__tmp))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_pre_decrement ((s IDTIFFSwabArrayOfShort__tmp)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | _ => []
  end.


Theorem TIFFSwabArrayOfShort_ai_correct:
  forall s p' s', steps (g_start TIFFSwabArrayOfShort) s (g_edges TIFFSwabArrayOfShort) p' s' -> TIFFSwabArrayOfShort_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem TIFFSwabArrayOfShort_pot_correct:
  forall s p' s',
    steps (g_start TIFFSwabArrayOfShort) s (g_edges TIFFSwabArrayOfShort) p' s' ->
    (TIFFSwabArrayOfShort_pot (g_start TIFFSwabArrayOfShort) s >= TIFFSwabArrayOfShort_pot p' s')%Q.
Proof.
  check_lp TIFFSwabArrayOfShort_ai_correct TIFFSwabArrayOfShort_hints.
Qed.

