Require Import pasta.Pasta.

Notation IDnumFilename_z := 1%positive.
Notation IDnumFilename__tmp := 2%positive.
Notation IDnumFilename__tmp1 := 3%positive.
Notation IDnumFilename_len := 4%positive.
Notation IDnumFilename_offset := 5%positive.
Notation IDnumFilename_fname := 6%positive.
Notation IDnumFilename_num := 7%positive.
Notation IDnumFilename_ofnum := 8%positive.
Definition numFilename : graph := {|
  g_start := 1%positive;
  g_end := 19%positive;
  g_edges := (1%positive,(AAssign IDnumFilename_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDnumFilename__tmp1
             (Some (EVar IDnumFilename_num))),3%positive)::
             (3%positive,(AAssign IDnumFilename__tmp
             (Some (EVar IDnumFilename_ofnum))),4%positive)::
             (4%positive,(AAssign IDnumFilename_offset (Some (ENum (1)))),
             5%positive)::
             (5%positive,(AAssign IDnumFilename_len None),6%positive)::
             (6%positive,ANone,7%positive)::
             (7%positive,(AAssign IDnumFilename__tmp1 None),8%positive)::
             (8%positive,(AAssign IDnumFilename__tmp None),9%positive)::
             (9%positive,(AAssign IDnumFilename_offset
             (Some (EAdd (EVar IDnumFilename_offset) (ENum (1))))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDnumFilename__tmp)
             s) >= (eval (ENum (1)) s))%Z)),14%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDnumFilename__tmp)
             s) < (eval (ENum (1)) s))%Z)),13%positive)::
             (13%positive,AWeaken,19%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDnumFilename_offset) s) <
             (eval (ENum (4)) s))%Z)),20%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDnumFilename_offset) s) >=
             (eval (ENum (4)) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDnumFilename_z (Some (EAdd (ENum (1))
             (EVar IDnumFilename_z)))),7%positive)::nil
|}.

Definition numFilename_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename_z) <= 0)%Z
    | 3%positive => (-1 * (s IDnumFilename_z) <= 0 /\ 1 * (s IDnumFilename_z) <= 0)%Z
    | 4%positive => (1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename_z) <= 0)%Z
    | 5%positive => (-1 * (s IDnumFilename_z) <= 0 /\ 1 * (s IDnumFilename_z) <= 0 /\ 1 * (s IDnumFilename_offset) + -1 <= 0 /\ -1 * (s IDnumFilename_offset) + 1 <= 0)%Z
    | 6%positive => (-1 * (s IDnumFilename_offset) + 1 <= 0 /\ 1 * (s IDnumFilename_offset) + -1 <= 0 /\ 1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename_z) <= 0)%Z
    | 7%positive => (-1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename_offset) + 1 <= 0 /\ 1 * (s IDnumFilename_offset) + -3 <= 0)%Z
    | 8%positive => (1 * (s IDnumFilename_offset) + -3 <= 0 /\ -1 * (s IDnumFilename_offset) + 1 <= 0 /\ -1 * (s IDnumFilename_z) <= 0)%Z
    | 9%positive => (-1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename_offset) + 1 <= 0 /\ 1 * (s IDnumFilename_offset) + -3 <= 0)%Z
    | 10%positive => (-1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename_offset) + 2 <= 0 /\ 1 * (s IDnumFilename_offset) + -4 <= 0)%Z
    | 11%positive => (1 * (s IDnumFilename_offset) + -4 <= 0 /\ -1 * (s IDnumFilename_offset) + 2 <= 0 /\ -1 * (s IDnumFilename_z) <= 0)%Z
    | 12%positive => (-1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename_offset) + 2 <= 0 /\ 1 * (s IDnumFilename_offset) + -4 <= 0)%Z
    | 13%positive => (1 * (s IDnumFilename_offset) + -4 <= 0 /\ -1 * (s IDnumFilename_offset) + 2 <= 0 /\ -1 * (s IDnumFilename_z) <= 0 /\ 1 * (s IDnumFilename__tmp) <= 0)%Z
    | 14%positive => (1 * (s IDnumFilename_offset) + -4 <= 0 /\ -1 * (s IDnumFilename_offset) + 2 <= 0 /\ -1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename__tmp) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDnumFilename__tmp) + 1 <= 0 /\ -1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename_offset) + 2 <= 0 /\ 1 * (s IDnumFilename_offset) + -4 <= 0)%Z
    | 16%positive => (1 * (s IDnumFilename_offset) + -4 <= 0 /\ -1 * (s IDnumFilename_offset) + 2 <= 0 /\ -1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename__tmp) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDnumFilename__tmp) + 1 <= 0 /\ -1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename_offset) + 2 <= 0 /\ 1 * (s IDnumFilename_offset) + -4 <= 0)%Z
    | 18%positive => (1 * (s IDnumFilename_offset) + -4 <= 0 /\ -1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename__tmp) + 1 <= 0 /\ -1 * (s IDnumFilename_offset) + 4 <= 0)%Z
    | 19%positive => (-1 * (s IDnumFilename_offset) + 2 <= 0 /\ -1 * (s IDnumFilename_z) <= 0 /\ 1 * (s IDnumFilename_offset) + -4 <= 0)%Z
    | 20%positive => (-1 * (s IDnumFilename_offset) + 2 <= 0 /\ -1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename__tmp) + 1 <= 0 /\ 1 * (s IDnumFilename_offset) + -3 <= 0)%Z
    | 21%positive => (1 * (s IDnumFilename_offset) + -3 <= 0 /\ -1 * (s IDnumFilename__tmp) + 1 <= 0 /\ -1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename_offset) + 2 <= 0)%Z
    | 22%positive => (-1 * (s IDnumFilename_offset) + 2 <= 0 /\ -1 * (s IDnumFilename_z) <= 0 /\ -1 * (s IDnumFilename__tmp) + 1 <= 0 /\ 1 * (s IDnumFilename_offset) + -3 <= 0)%Z
    | _ => False
  end.

Definition numFilename_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((2 # 1))%Q
    | 2%positive => ((2 # 1) + (s IDnumFilename_z))%Q
    | 3%positive => ((2 # 1) + (s IDnumFilename_z))%Q
    | 4%positive => ((2 # 1) + (s IDnumFilename_z))%Q
    | 5%positive => ((s IDnumFilename_z) + max0(3 - (s IDnumFilename_offset)))%Q
    | 6%positive => ((s IDnumFilename_z) + max0(3 - (s IDnumFilename_offset)))%Q
    | 7%positive => ((s IDnumFilename_z) + max0(3 - (s IDnumFilename_offset)))%Q
    | 8%positive => ((s IDnumFilename_z) + max0(3 - (s IDnumFilename_offset)))%Q
    | 9%positive => ((s IDnumFilename_z) + max0(3 - (s IDnumFilename_offset)))%Q
    | 10%positive => ((s IDnumFilename_z)
                      + max0(4 - (s IDnumFilename_offset)))%Q
    | 11%positive => ((s IDnumFilename_z)
                      + max0(4 - (s IDnumFilename_offset)))%Q
    | 12%positive => ((s IDnumFilename_z)
                      + max0(4 - (s IDnumFilename_offset)))%Q
    | 13%positive => ((s IDnumFilename_z)
                      + max0(4 - (s IDnumFilename_offset)))%Q
    | 14%positive => ((s IDnumFilename_z)
                      + max0(4 - (s IDnumFilename_offset)))%Q
    | 15%positive => ((s IDnumFilename_z)
                      + max0(4 - (s IDnumFilename_offset)))%Q
    | 16%positive => ((s IDnumFilename_z)
                      + max0(4 - (s IDnumFilename_offset)))%Q
    | 17%positive => ((4 # 1) - (s IDnumFilename_offset)
                      + (s IDnumFilename_z))%Q
    | 18%positive => ((4 # 1) - (s IDnumFilename_offset)
                      + (s IDnumFilename_z))%Q
    | 19%positive => ((s IDnumFilename_z))%Q
    | 20%positive => ((4 # 1) - (s IDnumFilename_offset)
                      + (s IDnumFilename_z))%Q
    | 21%positive => ((1 # 1) + (s IDnumFilename_z)
                      + max0(3 - (s IDnumFilename_offset)))%Q
    | 22%positive => ((1 # 1) + (s IDnumFilename_z)
                      + max0(3 - (s IDnumFilename_offset)))%Q
    | _ => (0 # 1)%Q
  end.

Definition numFilename_hints (p : node) (s : state) := 
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
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDnumFilename_offset)) (3
                                                                    - (s IDnumFilename_offset)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDnumFilename_offset))]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                                  - (s IDnumFilename_offset))) (F_check_ge (4
                                                                    - (s IDnumFilename_offset)) (0))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDnumFilename_offset)) (3
                                                                    - (s IDnumFilename_offset)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDnumFilename_offset));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDnumFilename_offset)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDnumFilename_offset)))]
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDnumFilename_offset)) (1);
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - (s IDnumFilename_offset)) (0))) (F_max0_ge_0 (4
                                                                    - (s IDnumFilename_offset)))]
    | 21%positive => []
    | 22%positive => []
    | _ => []
  end.


Theorem numFilename_ai_correct:
  forall s p' s', steps (g_start numFilename) s (g_edges numFilename) p' s' -> numFilename_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem numFilename_pot_correct:
  forall s p' s',
    steps (g_start numFilename) s (g_edges numFilename) p' s' ->
    (numFilename_pot (g_start numFilename) s >= numFilename_pot p' s')%Q.
Proof.
  check_lp numFilename_ai_correct numFilename_hints.
Qed.

