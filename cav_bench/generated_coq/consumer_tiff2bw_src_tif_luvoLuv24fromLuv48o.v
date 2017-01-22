Require Import pasta.Pasta.

Notation IDLuv24fromLuv48_z := 1%positive.
Notation IDLuv24fromLuv48_Ce := 2%positive.
Notation IDLuv24fromLuv48_Le := 3%positive.
Notation IDLuv24fromLuv48__tmp := 4%positive.
Notation IDLuv24fromLuv48_n := 5%positive.
Notation IDLuv24fromLuv48_op := 6%positive.
Notation IDLuv24fromLuv48_sp := 7%positive.
Definition Luv24fromLuv48 : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDLuv24fromLuv48_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDLuv24fromLuv48__tmp
             (Some (EVar IDLuv24fromLuv48_n))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDLuv24fromLuv48__tmp
             (Some (EAdd (EVar IDLuv24fromLuv48__tmp) (ENum (-1))))),
             5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDLuv24fromLuv48__tmp) s) >
             (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDLuv24fromLuv48__tmp) s) <=
             (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,18%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,15%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDLuv24fromLuv48_Le None),14%positive)::
             (14%positive,ANone,17%positive)::
             (15%positive,(AAssign IDLuv24fromLuv48_Le (Some (ENum (1023)))),
             16%positive)::(16%positive,ANone,17%positive)::
             (17%positive,ANone,20%positive)::
             (18%positive,(AAssign IDLuv24fromLuv48_Le (Some (ENum (0)))),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDLuv24fromLuv48_Ce None),21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDLuv24fromLuv48_Ce)
             s) < (eval (ENum (0)) s))%Z)),24%positive)::
             (22%positive,(AGuard (fun s => ((eval (EVar IDLuv24fromLuv48_Ce)
             s) >= (eval (ENum (0)) s))%Z)),23%positive)::
             (23%positive,AWeaken,27%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDLuv24fromLuv48_Ce None),26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDLuv24fromLuv48_z (Some (EAdd (ENum (1))
             (EVar IDLuv24fromLuv48_z)))),4%positive)::nil
|}.

Definition Luv24fromLuv48_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0)%Z
    | 3%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0 /\ 1 * (s IDLuv24fromLuv48_z) <= 0)%Z
    | 4%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0)%Z
    | 5%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0)%Z
    | 6%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0)%Z
    | 7%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0 /\ 1 * (s IDLuv24fromLuv48__tmp) <= 0)%Z
    | 8%positive => (1 * (s IDLuv24fromLuv48__tmp) <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0)%Z
    | 9%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0)%Z
    | 10%positive => (-1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0)%Z
    | 11%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0)%Z
    | 12%positive => (-1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0)%Z
    | 13%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0)%Z
    | 15%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0 /\ 1 * (s IDLuv24fromLuv48_Le) + -1023 <= 0 /\ -1 * (s IDLuv24fromLuv48_Le) + 1023 <= 0)%Z
    | 17%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0 /\ 1 * (s IDLuv24fromLuv48_Le) <= 0 /\ -1 * (s IDLuv24fromLuv48_Le) <= 0)%Z
    | 20%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0)%Z
    | 22%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48_Ce) <= 0)%Z
    | 24%positive => (-1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0 /\ 1 * (s IDLuv24fromLuv48_Ce) + 1 <= 0)%Z
    | 25%positive => (1 * (s IDLuv24fromLuv48_Ce) + 1 <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0)%Z
    | 27%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0 /\ -1 * (s IDLuv24fromLuv48_z) <= 0)%Z
    | 29%positive => (-1 * (s IDLuv24fromLuv48_z) <= 0 /\ -1 * (s IDLuv24fromLuv48__tmp) + 1 <= 0)%Z
    | _ => False
  end.

Definition Luv24fromLuv48_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDLuv24fromLuv48_n)))%Q
    | 2%positive => ((s IDLuv24fromLuv48_z)
                     + max0(-1 + (s IDLuv24fromLuv48_n)))%Q
    | 3%positive => ((s IDLuv24fromLuv48_z)
                     + max0(-1 + (s IDLuv24fromLuv48__tmp)))%Q
    | 4%positive => ((s IDLuv24fromLuv48_z)
                     + max0(-1 + (s IDLuv24fromLuv48__tmp)))%Q
    | 5%positive => ((s IDLuv24fromLuv48_z) + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 6%positive => ((s IDLuv24fromLuv48_z) + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 7%positive => ((s IDLuv24fromLuv48_z) + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 8%positive => ((s IDLuv24fromLuv48_z))%Q
    | 9%positive => ((s IDLuv24fromLuv48_z) + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 10%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 11%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 12%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 13%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 14%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 15%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 16%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 17%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 18%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 19%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 20%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 21%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 22%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 23%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 24%positive => ((s IDLuv24fromLuv48_z)
                      + max0((s IDLuv24fromLuv48__tmp)))%Q
    | 25%positive => ((1 # 1) + (s IDLuv24fromLuv48_z)
                      + max0(-1 + (s IDLuv24fromLuv48__tmp)))%Q
    | 26%positive => ((1 # 1) + (s IDLuv24fromLuv48_z)
                      + max0(-1 + (s IDLuv24fromLuv48__tmp)))%Q
    | 27%positive => ((1 # 1) + (s IDLuv24fromLuv48_z)
                      + max0(-1 + (s IDLuv24fromLuv48__tmp)))%Q
    | 28%positive => ((1 # 1) + (s IDLuv24fromLuv48_z)
                      + max0(-1 + (s IDLuv24fromLuv48__tmp)))%Q
    | 29%positive => ((1 # 1) + (s IDLuv24fromLuv48_z)
                      + max0(-1 + (s IDLuv24fromLuv48__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Luv24fromLuv48_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDLuv24fromLuv48__tmp)) (-1
                                                                    + (s IDLuv24fromLuv48__tmp)));
                     (*-1 0*) F_max0_ge_0 (-1 + (s IDLuv24fromLuv48__tmp))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_pre_decrement ((s IDLuv24fromLuv48__tmp)) (1)]
    | 24%positive => [(*-1 0*) F_max0_pre_decrement ((s IDLuv24fromLuv48__tmp)) (1)]
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | _ => []
  end.


Theorem Luv24fromLuv48_ai_correct:
  forall s p' s', steps (g_start Luv24fromLuv48) s (g_edges Luv24fromLuv48) p' s' -> Luv24fromLuv48_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Luv24fromLuv48_pot_correct:
  forall s p' s',
    steps (g_start Luv24fromLuv48) s (g_edges Luv24fromLuv48) p' s' ->
    (Luv24fromLuv48_pot (g_start Luv24fromLuv48) s >= Luv24fromLuv48_pot p' s')%Q.
Proof.
  check_lp Luv24fromLuv48_ai_correct Luv24fromLuv48_hints.
Qed.

