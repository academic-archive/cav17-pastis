Require Import pasta.Pasta.

Notation IDrandombits_z := 1%positive.
Notation IDrandombits__tmp := 2%positive.
Notation IDrandombits_nbits := 3%positive.
Notation IDrandombits_p := 4%positive.
Definition randombits : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDrandombits_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDrandombits__tmp
             (Some (EVar IDrandombits_nbits))),3%positive)::
             (3%positive,ANone,4%positive)::(4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDrandombits__tmp)
             s) >= (eval (ENum (16)) s))%Z)),13%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDrandombits__tmp)
             s) < (eval (ENum (16)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDrandombits__tmp)
             s) <> (eval (ENum (0)) s))%Z)),9%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDrandombits__tmp)
             s) = (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,12%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDrandombits__tmp
             (Some (ESub (EVar IDrandombits__tmp) (ENum (16))))),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDrandombits_z (Some (EAdd (ENum (1))
             (EVar IDrandombits_z)))),18%positive)::
             (18%positive,AWeaken,5%positive)::nil
|}.

Definition randombits_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDrandombits_z) <= 0 /\ -1 * (s IDrandombits_z) <= 0)%Z
    | 3%positive => (-1 * (s IDrandombits_z) <= 0 /\ 1 * (s IDrandombits_z) <= 0)%Z
    | 4%positive => (1 * (s IDrandombits_z) <= 0 /\ -1 * (s IDrandombits_z) <= 0)%Z
    | 5%positive => (-1 * (s IDrandombits_z) <= 0)%Z
    | 6%positive => (-1 * (s IDrandombits_z) <= 0 /\ 1 * (s IDrandombits__tmp) + -15 <= 0)%Z
    | 7%positive => (1 * (s IDrandombits__tmp) + -15 <= 0 /\ -1 * (s IDrandombits_z) <= 0)%Z
    | 8%positive => (-1 * (s IDrandombits_z) <= 0 /\ 1 * (s IDrandombits__tmp) <= 0 /\ -1 * (s IDrandombits__tmp) <= 0)%Z
    | 9%positive => (-1 * (s IDrandombits_z) <= 0 /\ 1 * (s IDrandombits__tmp) + -15 <= 0)%Z
    | 10%positive => (1 * (s IDrandombits__tmp) + -15 <= 0 /\ -1 * (s IDrandombits_z) <= 0)%Z
    | 11%positive => (-1 * (s IDrandombits_z) <= 0 /\ 1 * (s IDrandombits__tmp) + -15 <= 0)%Z
    | 12%positive => (1 * (s IDrandombits__tmp) + -15 <= 0 /\ -1 * (s IDrandombits_z) <= 0)%Z
    | 13%positive => (-1 * (s IDrandombits_z) <= 0 /\ -1 * (s IDrandombits__tmp) + 16 <= 0)%Z
    | 14%positive => (-1 * (s IDrandombits__tmp) + 16 <= 0 /\ -1 * (s IDrandombits_z) <= 0)%Z
    | 15%positive => (-1 * (s IDrandombits_z) <= 0 /\ -1 * (s IDrandombits__tmp) <= 0)%Z
    | 16%positive => (-1 * (s IDrandombits__tmp) <= 0 /\ -1 * (s IDrandombits_z) <= 0)%Z
    | 17%positive => (-1 * (s IDrandombits_z) <= 0 /\ -1 * (s IDrandombits__tmp) <= 0)%Z
    | 18%positive => (-1 * (s IDrandombits__tmp) <= 0 /\ -1 * (s IDrandombits_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition randombits_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 16) * max0((s IDrandombits_nbits)))%Q
    | 2%positive => ((s IDrandombits_z)
                     + (1 # 16) * max0((s IDrandombits_nbits)))%Q
    | 3%positive => ((s IDrandombits_z)
                     + (1 # 16) * max0((s IDrandombits__tmp)))%Q
    | 4%positive => ((s IDrandombits_z)
                     + (1 # 16) * max0((s IDrandombits__tmp)))%Q
    | 5%positive => ((s IDrandombits_z)
                     + (1 # 16) * max0((s IDrandombits__tmp)))%Q
    | 6%positive => ((s IDrandombits_z)
                     + (1 # 16) * max0((s IDrandombits__tmp)))%Q
    | 7%positive => ((s IDrandombits_z))%Q
    | 8%positive => ((s IDrandombits_z))%Q
    | 9%positive => ((s IDrandombits_z))%Q
    | 10%positive => ((s IDrandombits_z))%Q
    | 11%positive => ((s IDrandombits_z))%Q
    | 12%positive => ((s IDrandombits_z))%Q
    | 13%positive => ((s IDrandombits_z)
                      + (1 # 16) * max0((s IDrandombits__tmp)))%Q
    | 14%positive => ((1 # 1) + (s IDrandombits_z)
                      + (1 # 16) * max0(-16 + (s IDrandombits__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDrandombits_z)
                      + (1 # 16) * max0((s IDrandombits__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDrandombits_z)
                      + (1 # 16) * max0((s IDrandombits__tmp)))%Q
    | 17%positive => ((1 # 1) + (s IDrandombits_z)
                      + (1 # 16) * max0((s IDrandombits__tmp)))%Q
    | 18%positive => ((s IDrandombits_z)
                      + (1 # 16) * max0((s IDrandombits__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition randombits_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*0 0.0625*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDrandombits__tmp))) (F_check_ge (0) (0))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-0.0625 0*) F_max0_pre_decrement ((s IDrandombits__tmp)) (16)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | _ => []
  end.


Theorem randombits_ai_correct:
  forall s p' s', steps (g_start randombits) s (g_edges randombits) p' s' -> randombits_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem randombits_pot_correct:
  forall s p' s',
    steps (g_start randombits) s (g_edges randombits) p' s' ->
    (randombits_pot (g_start randombits) s >= randombits_pot p' s')%Q.
Proof.
  check_lp randombits_ai_correct randombits_hints.
Qed.

