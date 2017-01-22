Require Import pasta.Pasta.

Notation IDFax3Close_z := 1%positive.
Notation IDFax3Close_code := 2%positive.
Notation IDFax3Close_i := 3%positive.
Notation IDFax3Close_length := 4%positive.
Notation IDFax3Close_tif := 5%positive.
Definition Fax3Close : graph := {|
  g_start := 1%positive;
  g_end := 21%positive;
  g_edges := (1%positive,(AAssign IDFax3Close_z (Some (ENum (0)))),
             2%positive)::(2%positive,AWeaken,3%positive)::
             (3%positive,ANone,5%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,21%positive)::
             (5%positive,(AAssign IDFax3Close_code (Some (ENum (1)))),
             6%positive)::
             (6%positive,(AAssign IDFax3Close_length (Some (ENum (12)))),
             7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,ANone,9%positive)::(8%positive,ANone,12%positive)::
             (9%positive,(AAssign IDFax3Close_code None),10%positive)::
             (10%positive,(AAssign IDFax3Close_length
             (Some (EAdd (EVar IDFax3Close_length) (ENum (1))))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,(AAssign IDFax3Close_i (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDFax3Close_i) s) <
             (eval (ENum (6)) s))%Z)),22%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDFax3Close_i) s) >=
             (eval (ENum (6)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (17%positive,ANone,19%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDFax3Close_i
             (Some (EAdd (EVar IDFax3Close_i) (ENum (1))))),25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDFax3Close_z (Some (EAdd (ENum (1))
             (EVar IDFax3Close_z)))),28%positive)::
             (28%positive,AWeaken,15%positive)::nil
|}.

Definition Fax3Close_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_z) <= 0)%Z
    | 3%positive => (-1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_z) <= 0)%Z
    | 4%positive => (1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_z) <= 0)%Z
    | 5%positive => (1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_z) <= 0)%Z
    | 6%positive => (-1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_code) + -1 <= 0 /\ -1 * (s IDFax3Close_code) + 1 <= 0)%Z
    | 7%positive => (-1 * (s IDFax3Close_code) + 1 <= 0 /\ 1 * (s IDFax3Close_code) + -1 <= 0 /\ 1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_length) + -12 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0)%Z
    | 8%positive => (-1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_length) + -12 <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_code) + -1 <= 0 /\ -1 * (s IDFax3Close_code) + 1 <= 0)%Z
    | 9%positive => (-1 * (s IDFax3Close_code) + 1 <= 0 /\ 1 * (s IDFax3Close_code) + -1 <= 0 /\ 1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_length) + -12 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0)%Z
    | 10%positive => (-1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_length) + -12 <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_z) <= 0)%Z
    | 11%positive => (1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_length) + 13 <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0)%Z
    | 12%positive => (-1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_z) <= 0)%Z
    | 13%positive => (1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_i) <= 0 /\ -1 * (s IDFax3Close_i) <= 0)%Z
    | 14%positive => (-1 * (s IDFax3Close_i) <= 0 /\ 1 * (s IDFax3Close_i) <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_z) <= 0)%Z
    | 15%positive => (-1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_i) <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_i) + -6 <= 0)%Z
    | 16%positive => (1 * (s IDFax3Close_i) + -6 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_i) + 6 <= 0)%Z
    | 17%positive => (-1 * (s IDFax3Close_i) + 6 <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_i) + -6 <= 0)%Z
    | 18%positive => (1 * (s IDFax3Close_i) + -6 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_i) + 6 <= 0)%Z
    | 19%positive => (-1 * (s IDFax3Close_i) + 6 <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_i) + -6 <= 0)%Z
    | 20%positive => (1 * (s IDFax3Close_i) + -6 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_i) + 6 <= 0)%Z
    | 21%positive => (-1 * (s IDFax3Close_z) <= 0)%Z
    | 22%positive => (-1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_i) <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_i) + -5 <= 0)%Z
    | 23%positive => (1 * (s IDFax3Close_i) + -5 <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ -1 * (s IDFax3Close_i) <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0)%Z
    | 24%positive => (-1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_i) <= 0 /\ -1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_i) + -5 <= 0)%Z
    | 25%positive => (-1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0 /\ -1 * (s IDFax3Close_i) + 1 <= 0 /\ 1 * (s IDFax3Close_i) + -6 <= 0)%Z
    | 26%positive => (1 * (s IDFax3Close_i) + -6 <= 0 /\ -1 * (s IDFax3Close_i) + 1 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_z) <= 0)%Z
    | 27%positive => (-1 * (s IDFax3Close_z) <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0 /\ -1 * (s IDFax3Close_i) + 1 <= 0 /\ 1 * (s IDFax3Close_i) + -6 <= 0)%Z
    | 28%positive => (1 * (s IDFax3Close_i) + -6 <= 0 /\ -1 * (s IDFax3Close_i) + 1 <= 0 /\ -1 * (s IDFax3Close_length) + 12 <= 0 /\ 1 * (s IDFax3Close_length) + -13 <= 0 /\ -1 * (s IDFax3Close_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition Fax3Close_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((6 # 1))%Q
    | 2%positive => ((6 # 1) + (s IDFax3Close_z))%Q
    | 3%positive => ((6 # 1) + (s IDFax3Close_z))%Q
    | 4%positive => ((6 # 1) + (s IDFax3Close_z))%Q
    | 5%positive => ((6 # 1) + (s IDFax3Close_z))%Q
    | 6%positive => ((6 # 1) + (s IDFax3Close_z))%Q
    | 7%positive => ((6 # 1) + (s IDFax3Close_z))%Q
    | 8%positive => ((6 # 1) + (s IDFax3Close_z))%Q
    | 9%positive => ((6 # 1) + (s IDFax3Close_z))%Q
    | 10%positive => ((6 # 1) + (s IDFax3Close_z))%Q
    | 11%positive => ((6 # 1) + (s IDFax3Close_z))%Q
    | 12%positive => ((6 # 1) + (s IDFax3Close_z))%Q
    | 13%positive => ((s IDFax3Close_z) + max0(6 - (s IDFax3Close_i)))%Q
    | 14%positive => ((s IDFax3Close_z) + max0(6 - (s IDFax3Close_i)))%Q
    | 15%positive => ((s IDFax3Close_z) + max0(6 - (s IDFax3Close_i)))%Q
    | 16%positive => ((s IDFax3Close_z) + max0(6 - (s IDFax3Close_i)))%Q
    | 17%positive => ((s IDFax3Close_z) + max0(6 - (s IDFax3Close_i)))%Q
    | 18%positive => ((s IDFax3Close_z) + max0(6 - (s IDFax3Close_i)))%Q
    | 19%positive => ((s IDFax3Close_z) + max0(6 - (s IDFax3Close_i)))%Q
    | 20%positive => ((s IDFax3Close_z) + max0(6 - (s IDFax3Close_i)))%Q
    | 21%positive => ((s IDFax3Close_z))%Q
    | 22%positive => ((s IDFax3Close_z) + max0(6 - (s IDFax3Close_i)))%Q
    | 23%positive => ((1 # 1) + (s IDFax3Close_z)
                      + max0(5 - (s IDFax3Close_i)))%Q
    | 24%positive => ((1 # 1) + (s IDFax3Close_z)
                      + max0(5 - (s IDFax3Close_i)))%Q
    | 25%positive => ((1 # 1) + (s IDFax3Close_z)
                      + max0(6 - (s IDFax3Close_i)))%Q
    | 26%positive => ((1 # 1) + (s IDFax3Close_z)
                      + max0(6 - (s IDFax3Close_i)))%Q
    | 27%positive => ((1 # 1) + (s IDFax3Close_z)
                      + max0(6 - (s IDFax3Close_i)))%Q
    | 28%positive => ((s IDFax3Close_z) + max0(6 - (s IDFax3Close_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Fax3Close_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => [(*-6 0*) F_one]
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
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (6
                                                             - (s IDFax3Close_i)) (5
                                                                    - (s IDFax3Close_i)));
                      (*-1 0*) F_max0_ge_0 (5 - (s IDFax3Close_i))]
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_max0_pre_decrement (6 - (s IDFax3Close_i)) (1)]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | _ => []
  end.


Theorem Fax3Close_ai_correct:
  forall s p' s', steps (g_start Fax3Close) s (g_edges Fax3Close) p' s' -> Fax3Close_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Fax3Close_pot_correct:
  forall s p' s',
    steps (g_start Fax3Close) s (g_edges Fax3Close) p' s' ->
    (Fax3Close_pot (g_start Fax3Close) s >= Fax3Close_pot p' s')%Q.
Proof.
  check_lp Fax3Close_ai_correct Fax3Close_hints.
Qed.

