Require Import pasta.Pasta.

Notation IDGsm_Coder_z := 1%positive.
Notation IDGsm_Coder_i := 2%positive.
Notation IDGsm_Coder_k := 3%positive.
Notation IDGsm_Coder_ltmp := 4%positive.
Notation IDGsm_Coder_LARc := 5%positive.
Notation IDGsm_Coder_Mc := 6%positive.
Notation IDGsm_Coder_Nc := 7%positive.
Notation IDGsm_Coder_S := 8%positive.
Notation IDGsm_Coder_bc := 9%positive.
Notation IDGsm_Coder_s := 10%positive.
Notation IDGsm_Coder_xMc := 11%positive.
Notation IDGsm_Coder_xmaxc := 12%positive.
Definition Gsm_Coder : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDGsm_Coder_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDGsm_Coder_k (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDGsm_Coder_k) s) <=
             (eval (ENum (3)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDGsm_Coder_k) s) >
             (eval (ENum (3)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDGsm_Coder_i (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDGsm_Coder_i) s) <=
             (eval (ENum (39)) s))%Z)),20%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDGsm_Coder_i) s) >
             (eval (ENum (39)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDGsm_Coder_k
             (Some (EAdd (EVar IDGsm_Coder_k) (ENum (1))))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDGsm_Coder_z (Some (EAdd (ENum (1))
             (EVar IDGsm_Coder_z)))),19%positive)::
             (19%positive,AWeaken,5%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDGsm_Coder_ltmp None),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,25%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,26%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDGsm_Coder_i
             (Some (EAdd (EVar IDGsm_Coder_i) (ENum (1))))),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDGsm_Coder_z (Some (EAdd (ENum (1))
             (EVar IDGsm_Coder_z)))),31%positive)::
             (31%positive,AWeaken,12%positive)::nil
|}.

Definition Gsm_Coder_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0)%Z
    | 3%positive => (-1 * (s IDGsm_Coder_z) <= 0 /\ 1 * (s IDGsm_Coder_z) <= 0 /\ 1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0)%Z
    | 4%positive => (-1 * (s IDGsm_Coder_k) <= 0 /\ 1 * (s IDGsm_Coder_k) <= 0 /\ 1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0)%Z
    | 5%positive => (-1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0)%Z
    | 6%positive => (-1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_k) + 4 <= 0)%Z
    | 7%positive => (-1 * (s IDGsm_Coder_k) + 4 <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0)%Z
    | 8%positive => (-1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ 1 * (s IDGsm_Coder_k) + -3 <= 0)%Z
    | 9%positive => (1 * (s IDGsm_Coder_k) + -3 <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0)%Z
    | 10%positive => (-1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ 1 * (s IDGsm_Coder_k) + -3 <= 0 /\ 1 * (s IDGsm_Coder_i) <= 0 /\ -1 * (s IDGsm_Coder_i) <= 0)%Z
    | 11%positive => (-1 * (s IDGsm_Coder_i) <= 0 /\ 1 * (s IDGsm_Coder_i) <= 0 /\ 1 * (s IDGsm_Coder_k) + -3 <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0)%Z
    | 12%positive => (-1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_i) <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0 /\ 1 * (s IDGsm_Coder_i) + -40 <= 0)%Z
    | 13%positive => (1 * (s IDGsm_Coder_i) + -40 <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_i) + 40 <= 0)%Z
    | 14%positive => (-1 * (s IDGsm_Coder_i) + 40 <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0 /\ 1 * (s IDGsm_Coder_i) + -40 <= 0)%Z
    | 15%positive => (1 * (s IDGsm_Coder_i) + -40 <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_i) + 40 <= 0)%Z
    | 16%positive => (-1 * (s IDGsm_Coder_i) + 40 <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ 1 * (s IDGsm_Coder_i) + -40 <= 0 /\ -1 * (s IDGsm_Coder_k) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDGsm_Coder_k) + 1 <= 0 /\ 1 * (s IDGsm_Coder_i) + -40 <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_i) + 40 <= 0)%Z
    | 18%positive => (-1 * (s IDGsm_Coder_i) + 40 <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ 1 * (s IDGsm_Coder_i) + -40 <= 0 /\ -1 * (s IDGsm_Coder_k) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDGsm_Coder_k) + 1 <= 0 /\ 1 * (s IDGsm_Coder_i) + -40 <= 0 /\ -1 * (s IDGsm_Coder_i) + 40 <= 0 /\ -1 * (s IDGsm_Coder_z) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_i) <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ 1 * (s IDGsm_Coder_i) + -39 <= 0)%Z
    | 21%positive => (1 * (s IDGsm_Coder_i) + -39 <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_i) <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0)%Z
    | 22%positive => (-1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_i) <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ 1 * (s IDGsm_Coder_i) + -39 <= 0)%Z
    | 23%positive => (1 * (s IDGsm_Coder_i) + -39 <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_i) <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0)%Z
    | 24%positive => (-1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_i) <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ 1 * (s IDGsm_Coder_i) + -39 <= 0)%Z
    | 25%positive => (-1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_i) <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ 1 * (s IDGsm_Coder_i) + -39 <= 0)%Z
    | 26%positive => (1 * (s IDGsm_Coder_i) + -39 <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_i) <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0)%Z
    | 27%positive => (-1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_i) <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0 /\ 1 * (s IDGsm_Coder_i) + -39 <= 0)%Z
    | 28%positive => (-1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_i) + 1 <= 0 /\ 1 * (s IDGsm_Coder_i) + -40 <= 0)%Z
    | 29%positive => (1 * (s IDGsm_Coder_i) + -40 <= 0 /\ -1 * (s IDGsm_Coder_i) + 1 <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_z) <= 0)%Z
    | 30%positive => (-1 * (s IDGsm_Coder_z) <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_i) + 1 <= 0 /\ 1 * (s IDGsm_Coder_i) + -40 <= 0)%Z
    | 31%positive => (1 * (s IDGsm_Coder_i) + -40 <= 0 /\ -1 * (s IDGsm_Coder_i) + 1 <= 0 /\ -1 * (s IDGsm_Coder_k) <= 0 /\ -1 * (s IDGsm_Coder_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition Gsm_Coder_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((164 # 1))%Q
    | 2%positive => ((164 # 1) + (s IDGsm_Coder_z))%Q
    | 3%positive => ((s IDGsm_Coder_z)
                     + (41 # 1) * max0(4 - (s IDGsm_Coder_k)))%Q
    | 4%positive => ((s IDGsm_Coder_z)
                     + (41 # 1) * max0(4 - (s IDGsm_Coder_k)))%Q
    | 5%positive => ((s IDGsm_Coder_z)
                     + (41 # 1) * max0(4 - (s IDGsm_Coder_k)))%Q
    | 6%positive => ((s IDGsm_Coder_z)
                     + (41 # 1) * max0(4 - (s IDGsm_Coder_k)))%Q
    | 7%positive => ((s IDGsm_Coder_z))%Q
    | 8%positive => ((s IDGsm_Coder_z)
                     + (41 # 1) * max0(4 - (s IDGsm_Coder_k)))%Q
    | 9%positive => ((s IDGsm_Coder_z)
                     + (41 # 1) * max0(4 - (s IDGsm_Coder_k)))%Q
    | 10%positive => (-(40 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(4 - (s IDGsm_Coder_k))
                      + max0(40 - (s IDGsm_Coder_i)))%Q
    | 11%positive => (-(40 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(4 - (s IDGsm_Coder_k))
                      + max0(40 - (s IDGsm_Coder_i)))%Q
    | 12%positive => ((1 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(40 - (s IDGsm_Coder_i)))%Q
    | 13%positive => ((1 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(40 - (s IDGsm_Coder_i)))%Q
    | 14%positive => ((1 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k)))%Q
    | 15%positive => ((1 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k)))%Q
    | 16%positive => ((1 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(4 - (s IDGsm_Coder_k)))%Q
    | 17%positive => ((1 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(4 - (s IDGsm_Coder_k)))%Q
    | 18%positive => ((1 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(4 - (s IDGsm_Coder_k)))%Q
    | 19%positive => ((s IDGsm_Coder_z)
                      + (41 # 1) * max0(4 - (s IDGsm_Coder_k)))%Q
    | 20%positive => ((1 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(40 - (s IDGsm_Coder_i)))%Q
    | 21%positive => ((2 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(39 - (s IDGsm_Coder_i)))%Q
    | 22%positive => ((2 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(39 - (s IDGsm_Coder_i)))%Q
    | 23%positive => ((2 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(39 - (s IDGsm_Coder_i)))%Q
    | 24%positive => ((2 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(39 - (s IDGsm_Coder_i)))%Q
    | 25%positive => ((2 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(39 - (s IDGsm_Coder_i)))%Q
    | 26%positive => ((2 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(39 - (s IDGsm_Coder_i)))%Q
    | 27%positive => ((2 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(39 - (s IDGsm_Coder_i)))%Q
    | 28%positive => ((2 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(40 - (s IDGsm_Coder_i)))%Q
    | 29%positive => ((2 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(40 - (s IDGsm_Coder_i)))%Q
    | 30%positive => ((2 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(40 - (s IDGsm_Coder_i)))%Q
    | 31%positive => ((1 # 1) + (s IDGsm_Coder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Coder_k))
                      + max0(40 - (s IDGsm_Coder_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Gsm_Coder_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-41 0*) F_max0_ge_0 (4 - (s IDGsm_Coder_k))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-41 0*) F_max0_pre_decrement (4 - (s IDGsm_Coder_k)) (1)]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_ge_0 (40 - (s IDGsm_Coder_i))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*0 1*) F_max0_pre_decrement (40 - (s IDGsm_Coder_i)) (1)]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | _ => []
  end.


Theorem Gsm_Coder_ai_correct:
  forall s p' s', steps (g_start Gsm_Coder) s (g_edges Gsm_Coder) p' s' -> Gsm_Coder_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Gsm_Coder_pot_correct:
  forall s p' s',
    steps (g_start Gsm_Coder) s (g_edges Gsm_Coder) p' s' ->
    (Gsm_Coder_pot (g_start Gsm_Coder) s >= Gsm_Coder_pot p' s')%Q.
Proof.
  check_lp Gsm_Coder_ai_correct Gsm_Coder_hints.
Qed.

