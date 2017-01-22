Require Import pasta.Pasta.

Notation IDgen_codes_z := 1%positive.
Notation IDgen_codes__tmp := 2%positive.
Notation IDgen_codes_bits := 3%positive.
Notation IDgen_codes_code := 4%positive.
Notation IDgen_codes_len := 5%positive.
Notation IDgen_codes_n := 6%positive.
Notation IDgen_codes_max_code := 7%positive.
Notation IDgen_codes_tree := 8%positive.
Definition gen_codes : graph := {|
  g_start := 1%positive;
  g_end := 14%positive;
  g_edges := (1%positive,(AAssign IDgen_codes_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDgen_codes__tmp
             (Some (EVar IDgen_codes_max_code))),3%positive)::
             (3%positive,(AAssign IDgen_codes_code (Some (ENum (0)))),
             4%positive)::
             (4%positive,(AAssign IDgen_codes_bits (Some (ENum (1)))),
             5%positive)::(5%positive,ANone,6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDgen_codes_bits)
             s) <= (eval (ENum (15)) s))%Z)),28%positive)::
             (7%positive,(AGuard (fun s => ((eval (EVar IDgen_codes_bits)
             s) > (eval (ENum (15)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDgen_codes_n (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDgen_codes_n) s) <=
             (eval (EVar IDgen_codes__tmp) s))%Z)),15%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDgen_codes_n) s) >
             (eval (EVar IDgen_codes__tmp) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AAssign IDgen_codes_len None),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AGuard (fun s => ((eval (EVar IDgen_codes_len)
             s) = (eval (ENum (0)) s))%Z)),21%positive)::
             (18%positive,(AGuard (fun s => ((eval (EVar IDgen_codes_len)
             s) <> (eval (ENum (0)) s))%Z)),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,ANone,23%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDgen_codes_n
             (Some (EAdd (EVar IDgen_codes_n) (ENum (1))))),24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDgen_codes_z (Some (EAdd (ENum (1))
             (EVar IDgen_codes_z)))),27%positive)::
             (27%positive,AWeaken,12%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AAssign IDgen_codes_code None),30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDgen_codes_bits
             (Some (EAdd (EVar IDgen_codes_bits) (ENum (1))))),32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDgen_codes_z (Some (EAdd (ENum (1))
             (EVar IDgen_codes_z)))),35%positive)::
             (35%positive,AWeaken,7%positive)::nil
|}.

Definition gen_codes_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_z) <= 0)%Z
    | 3%positive => (-1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_z) <= 0)%Z
    | 4%positive => (1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_code) <= 0 /\ -1 * (s IDgen_codes_code) <= 0)%Z
    | 5%positive => (-1 * (s IDgen_codes_code) <= 0 /\ 1 * (s IDgen_codes_code) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_bits) + -1 <= 0 /\ -1 * (s IDgen_codes_bits) + 1 <= 0)%Z
    | 6%positive => (-1 * (s IDgen_codes_bits) + 1 <= 0 /\ 1 * (s IDgen_codes_bits) + -1 <= 0 /\ 1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_code) <= 0 /\ -1 * (s IDgen_codes_code) <= 0)%Z
    | 7%positive => (-1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_bits) + 1 <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0)%Z
    | 8%positive => (1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0)%Z
    | 9%positive => (-1 * (s IDgen_codes_bits) + 16 <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0)%Z
    | 10%positive => (1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0 /\ 1 * (s IDgen_codes_n) <= 0 /\ -1 * (s IDgen_codes_n) <= 0)%Z
    | 11%positive => (-1 * (s IDgen_codes_n) <= 0 /\ 1 * (s IDgen_codes_n) <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0)%Z
    | 12%positive => (-1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_n) <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0)%Z
    | 13%positive => (-1 * (s IDgen_codes_bits) + 16 <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_n) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes__tmp)+ -1 * (s IDgen_codes_n) + 1 <= 0)%Z
    | 14%positive => (1 * (s IDgen_codes__tmp)+ -1 * (s IDgen_codes_n) + 1 <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_n) <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0)%Z
    | 15%positive => (-1 * (s IDgen_codes_bits) + 16 <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_n) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) <= 0)%Z
    | 16%positive => (-1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_n) <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0)%Z
    | 17%positive => (-1 * (s IDgen_codes_bits) + 16 <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_n) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) <= 0)%Z
    | 18%positive => (-1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_n) <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0)%Z
    | 19%positive => (-1 * (s IDgen_codes_bits) + 16 <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_n) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) <= 0)%Z
    | 20%positive => (-1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_n) <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0)%Z
    | 21%positive => (-1 * (s IDgen_codes_bits) + 16 <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_n) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) <= 0 /\ 1 * (s IDgen_codes_len) <= 0 /\ -1 * (s IDgen_codes_len) <= 0)%Z
    | 22%positive => (-1 * (s IDgen_codes_len) <= 0 /\ 1 * (s IDgen_codes_len) <= 0 /\ -1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_n) <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0)%Z
    | 23%positive => (-1 * (s IDgen_codes_bits) + 16 <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_n) <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) <= 0)%Z
    | 24%positive => (-1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0 /\ -1 * (s IDgen_codes_n) + 1 <= 0 /\ -1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) + -1 <= 0)%Z
    | 25%positive => (-1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) + -1 <= 0 /\ -1 * (s IDgen_codes_n) + 1 <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_z) <= 0)%Z
    | 26%positive => (-1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0 /\ -1 * (s IDgen_codes_n) + 1 <= 0 /\ -1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) + -1 <= 0)%Z
    | 27%positive => (-1 * (s IDgen_codes__tmp)+ 1 * (s IDgen_codes_n) + -1 <= 0 /\ -1 * (s IDgen_codes_n) + 1 <= 0 /\ -1 * (s IDgen_codes_bits) + 16 <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_z) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDgen_codes_bits) + 1 <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_bits) + -15 <= 0)%Z
    | 29%positive => (1 * (s IDgen_codes_bits) + -15 <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_bits) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDgen_codes_bits) + 1 <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_bits) + -15 <= 0)%Z
    | 31%positive => (1 * (s IDgen_codes_bits) + -15 <= 0 /\ -1 * (s IDgen_codes_z) <= 0 /\ -1 * (s IDgen_codes_bits) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_bits) + 2 <= 0)%Z
    | 33%positive => (-1 * (s IDgen_codes_bits) + 2 <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_z) <= 0)%Z
    | 34%positive => (-1 * (s IDgen_codes_z) <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_bits) + 2 <= 0)%Z
    | 35%positive => (-1 * (s IDgen_codes_bits) + 2 <= 0 /\ 1 * (s IDgen_codes_bits) + -16 <= 0 /\ -1 * (s IDgen_codes_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition gen_codes_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((15 # 1) + max0(1 + (s IDgen_codes_max_code)))%Q
    | 2%positive => ((15 # 1) + (s IDgen_codes_z)
                     + max0(1 + (s IDgen_codes_max_code)))%Q
    | 3%positive => ((15 # 1) + (s IDgen_codes_z)
                     + max0(1 + (s IDgen_codes__tmp)))%Q
    | 4%positive => ((15 # 1) + (s IDgen_codes_z)
                     + max0(1 + (s IDgen_codes__tmp)))%Q
    | 5%positive => ((s IDgen_codes_z) + max0(1 + (s IDgen_codes__tmp))
                     + max0(16 - (s IDgen_codes_bits)))%Q
    | 6%positive => ((s IDgen_codes_z) + max0(1 + (s IDgen_codes__tmp))
                     + max0(16 - (s IDgen_codes_bits)))%Q
    | 7%positive => ((s IDgen_codes_z) + max0(1 + (s IDgen_codes__tmp))
                     + max0(16 - (s IDgen_codes_bits)))%Q
    | 8%positive => ((s IDgen_codes_z) + max0(1 + (s IDgen_codes__tmp))
                     + max0(16 - (s IDgen_codes_bits)))%Q
    | 9%positive => ((s IDgen_codes_z) + max0(1 + (s IDgen_codes__tmp))
                     + max0(16 - (s IDgen_codes_bits)))%Q
    | 10%positive => ((s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp) - (s IDgen_codes_n))
                      + max0(16 - (s IDgen_codes_bits)))%Q
    | 11%positive => ((s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp) - (s IDgen_codes_n))
                      + max0(16 - (s IDgen_codes_bits)))%Q
    | 12%positive => ((s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 13%positive => ((s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 14%positive => ((s IDgen_codes_z))%Q
    | 15%positive => ((s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 16%positive => ((s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 17%positive => ((s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 18%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0((s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 19%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0((s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 20%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0((s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 21%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0((s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 22%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0((s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 23%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0((s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 24%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 25%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 26%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 27%positive => ((s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp) - (s IDgen_codes_n)))%Q
    | 28%positive => ((s IDgen_codes_z) + max0(1 + (s IDgen_codes__tmp))
                      + max0(16 - (s IDgen_codes_bits)))%Q
    | 29%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp))
                      + max0(15 - (s IDgen_codes_bits)))%Q
    | 30%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp))
                      + max0(15 - (s IDgen_codes_bits)))%Q
    | 31%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp))
                      + max0(15 - (s IDgen_codes_bits)))%Q
    | 32%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp))
                      + max0(16 - (s IDgen_codes_bits)))%Q
    | 33%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp))
                      + max0(16 - (s IDgen_codes_bits)))%Q
    | 34%positive => ((1 # 1) + (s IDgen_codes_z)
                      + max0(1 + (s IDgen_codes__tmp))
                      + max0(16 - (s IDgen_codes_bits)))%Q
    | 35%positive => ((s IDgen_codes_z) + max0(1 + (s IDgen_codes__tmp))
                      + max0(16 - (s IDgen_codes_bits)))%Q
    | _ => (0 # 1)%Q
  end.

Definition gen_codes_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (16
                                                             - (s IDgen_codes_bits)) (15
                                                                    - (s IDgen_codes_bits)));
                      (*-1 0*) F_max0_ge_0 (15 - (s IDgen_codes_bits))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDgen_codes__tmp)
                                                             - (s IDgen_codes_n)) ((s IDgen_codes__tmp)
                                                                    - (s IDgen_codes_n)));
                      (*-1 0*) F_max0_ge_0 ((s IDgen_codes__tmp)
                                            - (s IDgen_codes_n))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*0 1*) F_max0_pre_decrement (1 + (s IDgen_codes__tmp)
                                                    - (s IDgen_codes_n)) (1)]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDgen_codes_z))) (F_check_ge ((s IDgen_codes_z)) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDgen_codes_z)) (0))) (F_max0_ge_0 ((s IDgen_codes_z)))]
    | 28%positive => [(*-1 0*) F_max0_pre_decrement (16
                                                     - (s IDgen_codes_bits)) (1)]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | _ => []
  end.


Theorem gen_codes_ai_correct:
  forall s p' s', steps (g_start gen_codes) s (g_edges gen_codes) p' s' -> gen_codes_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem gen_codes_pot_correct:
  forall s p' s',
    steps (g_start gen_codes) s (g_edges gen_codes) p' s' ->
    (gen_codes_pot (g_start gen_codes) s >= gen_codes_pot p' s')%Q.
Proof.
  check_lp gen_codes_ai_correct gen_codes_hints.
Qed.

