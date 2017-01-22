Require Import pasta.Pasta.

Notation IDLuv24fromXYZ_z := 1%positive.
Notation IDLuv24fromXYZ__tmp := 2%positive.
Notation IDLuv24fromXYZ_n := 3%positive.
Notation IDLuv24fromXYZ_op := 4%positive.
Notation IDLuv24fromXYZ_sp := 5%positive.
Definition Luv24fromXYZ : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDLuv24fromXYZ_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDLuv24fromXYZ__tmp
             (Some (EVar IDLuv24fromXYZ_n))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDLuv24fromXYZ__tmp
             (Some (EAdd (EVar IDLuv24fromXYZ__tmp) (ENum (-1))))),
             5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDLuv24fromXYZ__tmp)
             s) > (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDLuv24fromXYZ__tmp)
             s) <= (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDLuv24fromXYZ_z (Some (EAdd (ENum (1))
             (EVar IDLuv24fromXYZ_z)))),4%positive)::nil
|}.

Definition Luv24fromXYZ_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDLuv24fromXYZ_z) <= 0 /\ -1 * (s IDLuv24fromXYZ_z) <= 0)%Z
    | 3%positive => (-1 * (s IDLuv24fromXYZ_z) <= 0 /\ 1 * (s IDLuv24fromXYZ_z) <= 0)%Z
    | 4%positive => (-1 * (s IDLuv24fromXYZ_z) <= 0)%Z
    | 5%positive => (-1 * (s IDLuv24fromXYZ_z) <= 0)%Z
    | 6%positive => (-1 * (s IDLuv24fromXYZ_z) <= 0)%Z
    | 7%positive => (-1 * (s IDLuv24fromXYZ_z) <= 0 /\ 1 * (s IDLuv24fromXYZ__tmp) <= 0)%Z
    | 8%positive => (1 * (s IDLuv24fromXYZ__tmp) <= 0 /\ -1 * (s IDLuv24fromXYZ_z) <= 0)%Z
    | 9%positive => (-1 * (s IDLuv24fromXYZ_z) <= 0 /\ -1 * (s IDLuv24fromXYZ__tmp) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDLuv24fromXYZ__tmp) + 1 <= 0 /\ -1 * (s IDLuv24fromXYZ_z) <= 0)%Z
    | 11%positive => (-1 * (s IDLuv24fromXYZ_z) <= 0 /\ -1 * (s IDLuv24fromXYZ__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDLuv24fromXYZ__tmp) + 1 <= 0 /\ -1 * (s IDLuv24fromXYZ_z) <= 0)%Z
    | _ => False
  end.

Definition Luv24fromXYZ_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDLuv24fromXYZ_n)))%Q
    | 2%positive => ((s IDLuv24fromXYZ_z) + max0(-1 + (s IDLuv24fromXYZ_n)))%Q
    | 3%positive => ((s IDLuv24fromXYZ_z)
                     + max0(-1 + (s IDLuv24fromXYZ__tmp)))%Q
    | 4%positive => ((s IDLuv24fromXYZ_z)
                     + max0(-1 + (s IDLuv24fromXYZ__tmp)))%Q
    | 5%positive => ((s IDLuv24fromXYZ_z) + max0((s IDLuv24fromXYZ__tmp)))%Q
    | 6%positive => ((s IDLuv24fromXYZ_z) + max0((s IDLuv24fromXYZ__tmp)))%Q
    | 7%positive => ((s IDLuv24fromXYZ_z) + max0((s IDLuv24fromXYZ__tmp)))%Q
    | 8%positive => ((s IDLuv24fromXYZ_z))%Q
    | 9%positive => ((s IDLuv24fromXYZ_z) + max0((s IDLuv24fromXYZ__tmp)))%Q
    | 10%positive => ((1 # 1) + (s IDLuv24fromXYZ_z)
                      + max0(-1 + (s IDLuv24fromXYZ__tmp)))%Q
    | 11%positive => ((1 # 1) + (s IDLuv24fromXYZ_z)
                      + max0(-1 + (s IDLuv24fromXYZ__tmp)))%Q
    | 12%positive => ((1 # 1) + (s IDLuv24fromXYZ_z)
                      + max0(-1 + (s IDLuv24fromXYZ__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Luv24fromXYZ_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDLuv24fromXYZ__tmp)) (-1
                                                                    + (s IDLuv24fromXYZ__tmp)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDLuv24fromXYZ__tmp))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_pre_decrement ((s IDLuv24fromXYZ__tmp)) (1)]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | _ => []
  end.


Theorem Luv24fromXYZ_ai_correct:
  forall s p' s', steps (g_start Luv24fromXYZ) s (g_edges Luv24fromXYZ) p' s' -> Luv24fromXYZ_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Luv24fromXYZ_pot_correct:
  forall s p' s',
    steps (g_start Luv24fromXYZ) s (g_edges Luv24fromXYZ) p' s' ->
    (Luv24fromXYZ_pot (g_start Luv24fromXYZ) s >= Luv24fromXYZ_pot p' s')%Q.
Proof.
  check_lp Luv24fromXYZ_ai_correct Luv24fromXYZ_hints.
Qed.

