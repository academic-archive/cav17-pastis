Require Import pasta.Pasta.

Notation IDname_init_z := 1%positive.
Notation IDname_init__tmp := 2%positive.
Notation IDname_init_i := 3%positive.
Notation IDname_init_ncnt := 4%positive.
Notation IDname_init_nidx := 5%positive.
Notation IDname_init_count := 6%positive.
Notation IDname_init_mem := 7%positive.
Definition name_init : graph := {|
  g_start := 1%positive;
  g_end := 25%positive;
  g_edges := (1%positive,(AAssign IDname_init_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDname_init__tmp
             (Some (EVar IDname_init_count))),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDname_init__tmp)
             s) = (eval (ENum (0)) s))%Z)),10%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDname_init__tmp)
             s) <> (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::(6%positive,ANone,8%positive)::
             (6%positive,ANone,7%positive)::(7%positive,ANone,13%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,AWeaken,25%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDname_init__tmp None),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDname_init_i (Some (ENum (0)))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDname_init_i) s) <
             (eval (ENum (130)) s))%Z)),41%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDname_init_i) s) >=
             (eval (ENum (130)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDname_init_i (Some (ENum (-1)))),
             19%positive)::(19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDname_init_i) s) <
             (eval (ENum (128)) s))%Z)),26%positive)::
             (21%positive,(AGuard (fun s => ((eval (EVar IDname_init_i) s) >=
             (eval (ENum (128)) s))%Z)),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,25%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,(AAssign IDname_init_ncnt (Some (EAdd (ENum (2))
             (EVar IDname_init_i)))),28%positive)::
             (28%positive,(AAssign IDname_init_nidx None),29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AGuard (fun s => ((eval (EVar IDname_init_i) s) <
             (eval (ENum (0)) s))%Z)),33%positive)::
             (30%positive,(AGuard (fun s => ((eval (EVar IDname_init_i) s) >=
             (eval (ENum (0)) s))%Z)),31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,35%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDname_init_i
             (Some (EAdd (EVar IDname_init_i) (ENum (1))))),37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDname_init_z (Some (EAdd (ENum (1))
             (EVar IDname_init_z)))),40%positive)::
             (40%positive,AWeaken,21%positive)::
             (41%positive,AWeaken,42%positive)::
             (42%positive,ANone,43%positive)::
             (43%positive,(AAssign IDname_init_i
             (Some (EAdd (EVar IDname_init_i) (ENum (128))))),44%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDname_init_z (Some (EAdd (ENum (1))
             (EVar IDname_init_z)))),47%positive)::
             (47%positive,AWeaken,16%positive)::nil
|}.

Definition name_init_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_z) <= 0)%Z
    | 3%positive => (-1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_z) <= 0)%Z
    | 4%positive => (1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_z) <= 0)%Z
    | 5%positive => (-1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_z) <= 0)%Z
    | 6%positive => (1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_z) <= 0)%Z
    | 7%positive => (-1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_z) <= 0)%Z
    | 8%positive => (-1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_z) <= 0)%Z
    | 9%positive => (1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_z) <= 0)%Z
    | 10%positive => (-1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init__tmp) <= 0 /\ -1 * (s IDname_init__tmp) <= 0)%Z
    | 11%positive => (-1 * (s IDname_init__tmp) <= 0 /\ 1 * (s IDname_init__tmp) <= 0 /\ 1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_z) <= 0)%Z
    | 12%positive => (-1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_z) <= 0)%Z
    | 13%positive => (1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_z) <= 0)%Z
    | 14%positive => (-1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_i) <= 0 /\ -1 * (s IDname_init_i) <= 0)%Z
    | 15%positive => (-1 * (s IDname_init_i) <= 0 /\ 1 * (s IDname_init_i) <= 0 /\ 1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_z) <= 0)%Z
    | 16%positive => (-1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) <= 0 /\ 1 * (s IDname_init_i) + -257 <= 0)%Z
    | 17%positive => (1 * (s IDname_init_i) + -257 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) + 130 <= 0)%Z
    | 18%positive => (-1 * (s IDname_init_i) + 130 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_i) + -257 <= 0)%Z
    | 19%positive => (-1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_i) + 1 <= 0 /\ -1 * (s IDname_init_i) + -1 <= 0)%Z
    | 20%positive => (-1 * (s IDname_init_i) + -1 <= 0 /\ 1 * (s IDname_init_i) + 1 <= 0 /\ -1 * (s IDname_init_z) <= 0)%Z
    | 21%positive => (-1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) + -1 <= 0 /\ 1 * (s IDname_init_i) + -128 <= 0)%Z
    | 22%positive => (1 * (s IDname_init_i) + -128 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) + 128 <= 0)%Z
    | 23%positive => (-1 * (s IDname_init_i) + 128 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_i) + -128 <= 0)%Z
    | 24%positive => (1 * (s IDname_init_i) + -128 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) + 128 <= 0)%Z
    | 25%positive => (-1 * (s IDname_init_z) <= 0)%Z
    | 26%positive => (-1 * (s IDname_init_i) + -1 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_i) + -127 <= 0)%Z
    | 27%positive => (1 * (s IDname_init_i) + -127 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) + -1 <= 0)%Z
    | 28%positive => (-1 * (s IDname_init_i) + -1 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_i) + -127 <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ -1 * (s IDname_init_ncnt) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDname_init_ncnt) + 1 <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ 1 * (s IDname_init_i) + -127 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) + -1 <= 0)%Z
    | 30%positive => (-1 * (s IDname_init_i) + -1 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_i) + -127 <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ -1 * (s IDname_init_ncnt) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDname_init_ncnt) + 1 <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ 1 * (s IDname_init_i) + -127 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) <= 0)%Z
    | 32%positive => (-1 * (s IDname_init_i) <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_i) + -127 <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ -1 * (s IDname_init_ncnt) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDname_init_ncnt) + 1 <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) + -1 <= 0 /\ 1 * (s IDname_init_i) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDname_init_i) + 1 <= 0 /\ -1 * (s IDname_init_i) + -1 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ -1 * (s IDname_init_ncnt) + 1 <= 0)%Z
    | 35%positive => (1 * (s IDname_init_i) + -127 <= 0 /\ -1 * (s IDname_init_ncnt) + 1 <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) + -1 <= 0)%Z
    | 36%positive => (-1 * (s IDname_init_i) + -1 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ -1 * (s IDname_init_ncnt) + 1 <= 0 /\ 1 * (s IDname_init_i) + -127 <= 0)%Z
    | 37%positive => (-1 * (s IDname_init_ncnt) + 1 <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) <= 0 /\ 1 * (s IDname_init_i) + -128 <= 0)%Z
    | 38%positive => (1 * (s IDname_init_i) + -128 <= 0 /\ -1 * (s IDname_init_i) <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ -1 * (s IDname_init_ncnt) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDname_init_ncnt) + 1 <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) <= 0 /\ 1 * (s IDname_init_i) + -128 <= 0)%Z
    | 40%positive => (1 * (s IDname_init_i) + -128 <= 0 /\ -1 * (s IDname_init_i) <= 0 /\ 1 * (s IDname_init_ncnt) + -129 <= 0 /\ -1 * (s IDname_init_ncnt) + 1 <= 0 /\ -1 * (s IDname_init_z) + 1 <= 0)%Z
    | 41%positive => (-1 * (s IDname_init_i) <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_i) + -129 <= 0)%Z
    | 42%positive => (1 * (s IDname_init_i) + -129 <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) <= 0)%Z
    | 43%positive => (-1 * (s IDname_init_i) <= 0 /\ -1 * (s IDname_init_z) <= 0 /\ 1 * (s IDname_init_i) + -129 <= 0)%Z
    | 44%positive => (-1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) + 128 <= 0 /\ 1 * (s IDname_init_i) + -257 <= 0)%Z
    | 45%positive => (1 * (s IDname_init_i) + -257 <= 0 /\ -1 * (s IDname_init_i) + 128 <= 0 /\ -1 * (s IDname_init_z) <= 0)%Z
    | 46%positive => (-1 * (s IDname_init_z) <= 0 /\ -1 * (s IDname_init_i) + 128 <= 0 /\ 1 * (s IDname_init_i) + -257 <= 0)%Z
    | 47%positive => (1 * (s IDname_init_i) + -257 <= 0 /\ -1 * (s IDname_init_i) + 128 <= 0 /\ -1 * (s IDname_init_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition name_init_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((16638 # 127))%Q
    | 2%positive => ((16638 # 127) + (s IDname_init_z))%Q
    | 3%positive => ((16638 # 127) + (s IDname_init_z))%Q
    | 4%positive => ((16638 # 127) + (s IDname_init_z))%Q
    | 5%positive => ((16638 # 127) + (s IDname_init_z))%Q
    | 6%positive => ((16638 # 127) + (s IDname_init_z))%Q
    | 7%positive => ((16638 # 127) + (s IDname_init_z))%Q
    | 8%positive => ((16638 # 127) + (s IDname_init_z))%Q
    | 9%positive => ((16638 # 127) + (s IDname_init_z))%Q
    | 10%positive => ((16638 # 127) + (s IDname_init_z))%Q
    | 11%positive => ((16638 # 127) + (s IDname_init_z))%Q
    | 12%positive => ((16638 # 127) + (s IDname_init_z))%Q
    | 13%positive => ((16638 # 127) + (s IDname_init_z))%Q
    | 14%positive => ((129 # 1) + (s IDname_init_z)
                      + (1 # 127) * max0(257 - (s IDname_init_i)))%Q
    | 15%positive => ((129 # 1) + (s IDname_init_z)
                      + (1 # 127) * max0(257 - (s IDname_init_i)))%Q
    | 16%positive => ((129 # 1) + (s IDname_init_z)
                      + (1 # 127) * max0(257 - (s IDname_init_i)))%Q
    | 17%positive => ((129 # 1) + (s IDname_init_z)
                      + (1 # 127) * max0(257 - (s IDname_init_i)))%Q
    | 18%positive => ((129 # 1) + (s IDname_init_z))%Q
    | 19%positive => ((s IDname_init_z) + max0(128 - (s IDname_init_i)))%Q
    | 20%positive => ((s IDname_init_z) + max0(128 - (s IDname_init_i)))%Q
    | 21%positive => (max0(128 - (s IDname_init_i)) + max0((s IDname_init_z)))%Q
    | 22%positive => (max0(128 - (s IDname_init_i)) + max0((s IDname_init_z)))%Q
    | 23%positive => (max0(128 - (s IDname_init_i)) + max0((s IDname_init_z)))%Q
    | 24%positive => (max0(128 - (s IDname_init_i)) + max0((s IDname_init_z)))%Q
    | 25%positive => ((s IDname_init_z))%Q
    | 26%positive => (max0(128 - (s IDname_init_i)) + max0((s IDname_init_z)))%Q
    | 27%positive => ((s IDname_init_z) + max0(128 - (s IDname_init_i)))%Q
    | 28%positive => ((s IDname_init_z) + max0(128 - (s IDname_init_i)))%Q
    | 29%positive => ((s IDname_init_z) + max0(128 - (s IDname_init_i)))%Q
    | 30%positive => ((s IDname_init_z) + max0(128 - (s IDname_init_i)))%Q
    | 31%positive => ((s IDname_init_z) + max0(128 - (s IDname_init_i)))%Q
    | 32%positive => ((1 # 1) + (s IDname_init_z)
                      + max0(127 - (s IDname_init_i)))%Q
    | 33%positive => ((s IDname_init_z) + max0(128 - (s IDname_init_i)))%Q
    | 34%positive => ((1 # 1) + (s IDname_init_z)
                      + max0(127 - (s IDname_init_i)))%Q
    | 35%positive => ((1 # 1) + (s IDname_init_z)
                      + max0(127 - (s IDname_init_i)))%Q
    | 36%positive => ((1 # 1) + (s IDname_init_z)
                      + max0(127 - (s IDname_init_i)))%Q
    | 37%positive => ((1 # 1) + (s IDname_init_z)
                      + max0(128 - (s IDname_init_i)))%Q
    | 38%positive => ((1 # 1) + (s IDname_init_z)
                      + max0(128 - (s IDname_init_i)))%Q
    | 39%positive => ((1 # 1) + (s IDname_init_z)
                      + max0(128 - (s IDname_init_i)))%Q
    | 40%positive => ((s IDname_init_z) + max0(128 - (s IDname_init_i)))%Q
    | 41%positive => ((129 # 1) + (s IDname_init_z)
                      + (1 # 127) * max0(257 - (s IDname_init_i)))%Q
    | 42%positive => ((130 # 1) + (s IDname_init_z)
                      + (1 # 127) * max0(129 - (s IDname_init_i)))%Q
    | 43%positive => ((130 # 1) + (s IDname_init_z)
                      + (1 # 127) * max0(129 - (s IDname_init_i)))%Q
    | 44%positive => ((130 # 1) + (s IDname_init_z)
                      + (1 # 127) * max0(257 - (s IDname_init_i)))%Q
    | 45%positive => ((130 # 1) + (s IDname_init_z)
                      + (1 # 127) * max0(257 - (s IDname_init_i)))%Q
    | 46%positive => ((130 # 1) + (s IDname_init_z)
                      + (1 # 127) * max0(257 - (s IDname_init_i)))%Q
    | 47%positive => ((129 # 1) + (s IDname_init_z)
                      + (1 # 127) * max0(257 - (s IDname_init_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition name_init_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-131.008 0*) F_one]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-0.0078125 0*) F_binom_monotonic 1 (F_max0_ge_0 (257
                                                                    - (s IDname_init_i))) (F_check_ge (0) (0))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDname_init_z)) (0))) (F_max0_ge_0 ((s IDname_init_z)))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (128
                                                             - (s IDname_init_i)) (127
                                                                    - (s IDname_init_i)));
                      (*-1 0*) F_max0_ge_0 (127 - (s IDname_init_i));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDname_init_z))) (F_check_ge ((s IDname_init_z)) (0))]
    | 25%positive => []
    | 26%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDname_init_z))) (F_check_ge ((s IDname_init_z)) (0))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*0 1*) F_max0_pre_decrement (128 - (s IDname_init_i)) (1)]
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_max0_pre_decrement (128 - (s IDname_init_i)) (1)]
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDname_init_z)) (0))) (F_max0_ge_0 ((s IDname_init_z)))]
    | 41%positive => [(*-0.0078125 0*) F_max0_pre_decrement (257
                                                             - (s IDname_init_i)) (128)]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDname_init_z))) (F_check_ge ((s IDname_init_z)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDname_init_z)) (0))) (F_max0_ge_0 ((s IDname_init_z)))]
    | _ => []
  end.


Theorem name_init_ai_correct:
  forall s p' s', steps (g_start name_init) s (g_edges name_init) p' s' -> name_init_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem name_init_pot_correct:
  forall s p' s',
    steps (g_start name_init) s (g_edges name_init) p' s' ->
    (name_init_pot (g_start name_init) s >= name_init_pot p' s')%Q.
Proof.
  check_lp name_init_ai_correct name_init_hints.
Qed.

