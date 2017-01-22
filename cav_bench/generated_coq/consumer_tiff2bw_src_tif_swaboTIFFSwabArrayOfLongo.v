Require Import pasta.Pasta.

Notation IDTIFFSwabArrayOfLong_z := 1%positive.
Notation IDTIFFSwabArrayOfLong__tmp := 2%positive.
Notation IDTIFFSwabArrayOfLong_t := 3%positive.
Notation IDTIFFSwabArrayOfLong_lp := 4%positive.
Notation IDTIFFSwabArrayOfLong_n := 5%positive.
Definition TIFFSwabArrayOfLong : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDTIFFSwabArrayOfLong_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFSwabArrayOfLong__tmp) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDTIFFSwabArrayOfLong__tmp
             (Some (EVar IDTIFFSwabArrayOfLong_n))),5%positive)::
             (5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDTIFFSwabArrayOfLong__tmp
             (Some (EAdd (EVar IDTIFFSwabArrayOfLong__tmp) (ENum (-1))))),
             7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFSwabArrayOfLong__tmp) s) >
             (eval (ENum (0)) s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFSwabArrayOfLong__tmp) s) <=
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDTIFFSwabArrayOfLong_t None),13%positive)::
             (13%positive,(AAssign IDTIFFSwabArrayOfLong_t None),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDTIFFSwabArrayOfLong_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFSwabArrayOfLong_z)))),
             6%positive)::nil
|}.

Definition TIFFSwabArrayOfLong_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDTIFFSwabArrayOfLong_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfLong_z) <= 0)%Z
    | 3%positive => (-1 * (s IDTIFFSwabArrayOfLong_z) <= 0 /\ 1 * (s IDTIFFSwabArrayOfLong_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfLong__tmp) <= 0)%Z
    | 4%positive => (-1 * (s IDTIFFSwabArrayOfLong__tmp) <= 0 /\ 1 * (s IDTIFFSwabArrayOfLong_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfLong_z) <= 0)%Z
    | 5%positive => (-1 * (s IDTIFFSwabArrayOfLong_z) <= 0 /\ 1 * (s IDTIFFSwabArrayOfLong_z) <= 0)%Z
    | 6%positive => (-1 * (s IDTIFFSwabArrayOfLong_z) <= 0)%Z
    | 7%positive => (-1 * (s IDTIFFSwabArrayOfLong_z) <= 0)%Z
    | 8%positive => (-1 * (s IDTIFFSwabArrayOfLong_z) <= 0)%Z
    | 9%positive => (-1 * (s IDTIFFSwabArrayOfLong_z) <= 0 /\ 1 * (s IDTIFFSwabArrayOfLong__tmp) <= 0)%Z
    | 10%positive => (1 * (s IDTIFFSwabArrayOfLong__tmp) <= 0 /\ -1 * (s IDTIFFSwabArrayOfLong_z) <= 0)%Z
    | 11%positive => (-1 * (s IDTIFFSwabArrayOfLong_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfLong__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDTIFFSwabArrayOfLong__tmp) + 1 <= 0 /\ -1 * (s IDTIFFSwabArrayOfLong_z) <= 0)%Z
    | 13%positive => (-1 * (s IDTIFFSwabArrayOfLong_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfLong__tmp) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDTIFFSwabArrayOfLong__tmp) + 1 <= 0 /\ -1 * (s IDTIFFSwabArrayOfLong_z) <= 0)%Z
    | 15%positive => (-1 * (s IDTIFFSwabArrayOfLong_z) <= 0 /\ -1 * (s IDTIFFSwabArrayOfLong__tmp) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDTIFFSwabArrayOfLong__tmp) + 1 <= 0 /\ -1 * (s IDTIFFSwabArrayOfLong_z) <= 0)%Z
    | _ => False
  end.

Definition TIFFSwabArrayOfLong_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDTIFFSwabArrayOfLong_n)))%Q
    | 2%positive => ((s IDTIFFSwabArrayOfLong_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfLong_n)))%Q
    | 3%positive => ((s IDTIFFSwabArrayOfLong_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfLong_n)))%Q
    | 4%positive => ((s IDTIFFSwabArrayOfLong_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfLong_n)))%Q
    | 5%positive => ((s IDTIFFSwabArrayOfLong_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfLong__tmp)))%Q
    | 6%positive => ((s IDTIFFSwabArrayOfLong_z)
                     + max0(-1 + (s IDTIFFSwabArrayOfLong__tmp)))%Q
    | 7%positive => ((s IDTIFFSwabArrayOfLong_z)
                     + max0((s IDTIFFSwabArrayOfLong__tmp)))%Q
    | 8%positive => ((s IDTIFFSwabArrayOfLong_z)
                     + max0((s IDTIFFSwabArrayOfLong__tmp)))%Q
    | 9%positive => ((s IDTIFFSwabArrayOfLong_z)
                     + max0((s IDTIFFSwabArrayOfLong__tmp)))%Q
    | 10%positive => ((s IDTIFFSwabArrayOfLong_z))%Q
    | 11%positive => ((s IDTIFFSwabArrayOfLong_z)
                      + max0((s IDTIFFSwabArrayOfLong__tmp)))%Q
    | 12%positive => ((1 # 1) + (s IDTIFFSwabArrayOfLong_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfLong__tmp)))%Q
    | 13%positive => ((1 # 1) + (s IDTIFFSwabArrayOfLong_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfLong__tmp)))%Q
    | 14%positive => ((1 # 1) + (s IDTIFFSwabArrayOfLong_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfLong__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDTIFFSwabArrayOfLong_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfLong__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDTIFFSwabArrayOfLong_z)
                      + max0(-1 + (s IDTIFFSwabArrayOfLong__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition TIFFSwabArrayOfLong_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFSwabArrayOfLong__tmp)) (-1
                                                                    + (s IDTIFFSwabArrayOfLong__tmp)));
                     (*-1 0*) F_max0_ge_0 (-1
                                           + (s IDTIFFSwabArrayOfLong__tmp))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_pre_decrement ((s IDTIFFSwabArrayOfLong__tmp)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | _ => []
  end.


Theorem TIFFSwabArrayOfLong_ai_correct:
  forall s p' s', steps (g_start TIFFSwabArrayOfLong) s (g_edges TIFFSwabArrayOfLong) p' s' -> TIFFSwabArrayOfLong_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem TIFFSwabArrayOfLong_pot_correct:
  forall s p' s',
    steps (g_start TIFFSwabArrayOfLong) s (g_edges TIFFSwabArrayOfLong) p' s' ->
    (TIFFSwabArrayOfLong_pot (g_start TIFFSwabArrayOfLong) s >= TIFFSwabArrayOfLong_pot p' s')%Q.
Proof.
  check_lp TIFFSwabArrayOfLong_ai_correct TIFFSwabArrayOfLong_hints.
Qed.

