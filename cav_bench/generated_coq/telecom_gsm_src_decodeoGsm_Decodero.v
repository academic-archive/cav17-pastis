Require Import pasta.Pasta.

Notation IDGsm_Decoder_z := 1%positive.
Notation IDGsm_Decoder_j := 2%positive.
Notation IDGsm_Decoder_k := 3%positive.
Notation IDGsm_Decoder_LARcr := 4%positive.
Notation IDGsm_Decoder_Mcr := 5%positive.
Notation IDGsm_Decoder_Ncr := 6%positive.
Notation IDGsm_Decoder_S := 7%positive.
Notation IDGsm_Decoder_bcr := 8%positive.
Notation IDGsm_Decoder_s := 9%positive.
Notation IDGsm_Decoder_xMcr := 10%positive.
Notation IDGsm_Decoder_xmaxcr := 11%positive.
Definition Gsm_Decoder : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDGsm_Decoder_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDGsm_Decoder_j (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDGsm_Decoder_j)
             s) <= (eval (ENum (3)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDGsm_Decoder_j) s) >
             (eval (ENum (3)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDGsm_Decoder_k (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDGsm_Decoder_k)
             s) <= (eval (ENum (39)) s))%Z)),20%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDGsm_Decoder_k)
             s) > (eval (ENum (39)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDGsm_Decoder_j
             (Some (EAdd (EVar IDGsm_Decoder_j) (ENum (1))))),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,(AAssign IDGsm_Decoder_z (Some (EAdd (ENum (1))
             (EVar IDGsm_Decoder_z)))),19%positive)::
             (19%positive,AWeaken,5%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDGsm_Decoder_k
             (Some (EAdd (EVar IDGsm_Decoder_k) (ENum (1))))),23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDGsm_Decoder_z (Some (EAdd (ENum (1))
             (EVar IDGsm_Decoder_z)))),26%positive)::
             (26%positive,AWeaken,12%positive)::nil
|}.

Definition Gsm_Decoder_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0)%Z
    | 3%positive => (-1 * (s IDGsm_Decoder_z) <= 0 /\ 1 * (s IDGsm_Decoder_z) <= 0 /\ 1 * (s IDGsm_Decoder_j) <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0)%Z
    | 4%positive => (-1 * (s IDGsm_Decoder_j) <= 0 /\ 1 * (s IDGsm_Decoder_j) <= 0 /\ 1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0)%Z
    | 5%positive => (-1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0)%Z
    | 6%positive => (-1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_j) + 4 <= 0)%Z
    | 7%positive => (-1 * (s IDGsm_Decoder_j) + 4 <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0)%Z
    | 8%positive => (-1 * (s IDGsm_Decoder_j) <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ 1 * (s IDGsm_Decoder_j) + -3 <= 0)%Z
    | 9%positive => (1 * (s IDGsm_Decoder_j) + -3 <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0)%Z
    | 10%positive => (-1 * (s IDGsm_Decoder_j) <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ 1 * (s IDGsm_Decoder_j) + -3 <= 0 /\ 1 * (s IDGsm_Decoder_k) <= 0 /\ -1 * (s IDGsm_Decoder_k) <= 0)%Z
    | 11%positive => (-1 * (s IDGsm_Decoder_k) <= 0 /\ 1 * (s IDGsm_Decoder_k) <= 0 /\ 1 * (s IDGsm_Decoder_j) + -3 <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0)%Z
    | 12%positive => (-1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_k) <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0 /\ 1 * (s IDGsm_Decoder_k) + -40 <= 0)%Z
    | 13%positive => (1 * (s IDGsm_Decoder_k) + -40 <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_k) + 40 <= 0)%Z
    | 14%positive => (-1 * (s IDGsm_Decoder_k) + 40 <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0 /\ 1 * (s IDGsm_Decoder_k) + -40 <= 0)%Z
    | 15%positive => (1 * (s IDGsm_Decoder_k) + -40 <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_k) + 40 <= 0)%Z
    | 16%positive => (-1 * (s IDGsm_Decoder_k) + 40 <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ 1 * (s IDGsm_Decoder_k) + -40 <= 0 /\ -1 * (s IDGsm_Decoder_j) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDGsm_Decoder_j) + 1 <= 0 /\ 1 * (s IDGsm_Decoder_k) + -40 <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_k) + 40 <= 0)%Z
    | 18%positive => (-1 * (s IDGsm_Decoder_k) + 40 <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ 1 * (s IDGsm_Decoder_k) + -40 <= 0 /\ -1 * (s IDGsm_Decoder_j) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDGsm_Decoder_j) + 1 <= 0 /\ 1 * (s IDGsm_Decoder_k) + -40 <= 0 /\ -1 * (s IDGsm_Decoder_k) + 40 <= 0 /\ -1 * (s IDGsm_Decoder_z) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDGsm_Decoder_j) <= 0 /\ -1 * (s IDGsm_Decoder_k) <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ 1 * (s IDGsm_Decoder_k) + -39 <= 0)%Z
    | 21%positive => (1 * (s IDGsm_Decoder_k) + -39 <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_k) <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0)%Z
    | 22%positive => (-1 * (s IDGsm_Decoder_j) <= 0 /\ -1 * (s IDGsm_Decoder_k) <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0 /\ 1 * (s IDGsm_Decoder_k) + -39 <= 0)%Z
    | 23%positive => (-1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0 /\ -1 * (s IDGsm_Decoder_k) + 1 <= 0 /\ 1 * (s IDGsm_Decoder_k) + -40 <= 0)%Z
    | 24%positive => (1 * (s IDGsm_Decoder_k) + -40 <= 0 /\ -1 * (s IDGsm_Decoder_k) + 1 <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0 /\ -1 * (s IDGsm_Decoder_z) <= 0)%Z
    | 25%positive => (-1 * (s IDGsm_Decoder_z) <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0 /\ -1 * (s IDGsm_Decoder_k) + 1 <= 0 /\ 1 * (s IDGsm_Decoder_k) + -40 <= 0)%Z
    | 26%positive => (1 * (s IDGsm_Decoder_k) + -40 <= 0 /\ -1 * (s IDGsm_Decoder_k) + 1 <= 0 /\ -1 * (s IDGsm_Decoder_j) <= 0 /\ -1 * (s IDGsm_Decoder_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition Gsm_Decoder_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((164 # 1))%Q
    | 2%positive => ((164 # 1) + (s IDGsm_Decoder_z))%Q
    | 3%positive => ((s IDGsm_Decoder_z)
                     + (41 # 1) * max0(4 - (s IDGsm_Decoder_j)))%Q
    | 4%positive => ((s IDGsm_Decoder_z)
                     + (41 # 1) * max0(4 - (s IDGsm_Decoder_j)))%Q
    | 5%positive => ((s IDGsm_Decoder_z)
                     + (41 # 1) * max0(4 - (s IDGsm_Decoder_j)))%Q
    | 6%positive => ((s IDGsm_Decoder_z)
                     + (41 # 1) * max0(4 - (s IDGsm_Decoder_j)))%Q
    | 7%positive => ((s IDGsm_Decoder_z))%Q
    | 8%positive => ((s IDGsm_Decoder_z)
                     + (41 # 1) * max0(4 - (s IDGsm_Decoder_j)))%Q
    | 9%positive => ((s IDGsm_Decoder_z)
                     + (41 # 1) * max0(4 - (s IDGsm_Decoder_j)))%Q
    | 10%positive => (-(40 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(4 - (s IDGsm_Decoder_j))
                      + max0(40 - (s IDGsm_Decoder_k)))%Q
    | 11%positive => (-(40 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(4 - (s IDGsm_Decoder_j))
                      + max0(40 - (s IDGsm_Decoder_k)))%Q
    | 12%positive => ((1 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Decoder_j))
                      + max0(40 - (s IDGsm_Decoder_k)))%Q
    | 13%positive => ((1 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Decoder_j))
                      + max0(40 - (s IDGsm_Decoder_k)))%Q
    | 14%positive => ((1 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Decoder_j)))%Q
    | 15%positive => ((1 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Decoder_j)))%Q
    | 16%positive => ((1 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(4 - (s IDGsm_Decoder_j)))%Q
    | 17%positive => ((1 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(4 - (s IDGsm_Decoder_j)))%Q
    | 18%positive => ((1 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(4 - (s IDGsm_Decoder_j)))%Q
    | 19%positive => ((s IDGsm_Decoder_z)
                      + (41 # 1) * max0(4 - (s IDGsm_Decoder_j)))%Q
    | 20%positive => ((1 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Decoder_j))
                      + max0(40 - (s IDGsm_Decoder_k)))%Q
    | 21%positive => ((2 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Decoder_j))
                      + max0(39 - (s IDGsm_Decoder_k)))%Q
    | 22%positive => ((2 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Decoder_j))
                      + max0(39 - (s IDGsm_Decoder_k)))%Q
    | 23%positive => ((2 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Decoder_j))
                      + max0(40 - (s IDGsm_Decoder_k)))%Q
    | 24%positive => ((2 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Decoder_j))
                      + max0(40 - (s IDGsm_Decoder_k)))%Q
    | 25%positive => ((2 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Decoder_j))
                      + max0(40 - (s IDGsm_Decoder_k)))%Q
    | 26%positive => ((1 # 1) + (s IDGsm_Decoder_z)
                      + (41 # 1) * max0(3 - (s IDGsm_Decoder_j))
                      + max0(40 - (s IDGsm_Decoder_k)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Gsm_Decoder_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-41 0*) F_max0_ge_0 (4 - (s IDGsm_Decoder_j))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-41 0*) F_max0_pre_decrement (4 - (s IDGsm_Decoder_j)) (1)]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_ge_0 (40 - (s IDGsm_Decoder_k))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_pre_decrement (40 - (s IDGsm_Decoder_k)) (1)]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | _ => []
  end.


Theorem Gsm_Decoder_ai_correct:
  forall s p' s', steps (g_start Gsm_Decoder) s (g_edges Gsm_Decoder) p' s' -> Gsm_Decoder_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Gsm_Decoder_pot_correct:
  forall s p' s',
    steps (g_start Gsm_Decoder) s (g_edges Gsm_Decoder) p' s' ->
    (Gsm_Decoder_pot (g_start Gsm_Decoder) s >= Gsm_Decoder_pot p' s')%Q.
Proof.
  check_lp Gsm_Decoder_ai_correct Gsm_Decoder_hints.
Qed.

