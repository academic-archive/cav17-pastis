Require Import pasta.Pasta.

Notation IDid3_crc_calculate_z := 1%positive.
Notation IDid3_crc_calculate__tmp := 2%positive.
Notation IDid3_crc_calculate_crc := 3%positive.
Notation IDid3_crc_calculate_data := 4%positive.
Notation IDid3_crc_calculate_length := 5%positive.
Definition id3_crc_calculate : graph := {|
  g_start := 1%positive;
  g_end := 28%positive;
  g_edges := (1%positive,(AAssign IDid3_crc_calculate_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDid3_crc_calculate__tmp) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDid3_crc_calculate__tmp
             (Some (EVar IDid3_crc_calculate_length))),5%positive)::
             (5%positive,(AAssign IDid3_crc_calculate_crc None),6%positive)::
             (6%positive,ANone,7%positive)::(7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDid3_crc_calculate__tmp) s) >=
             (eval (ENum (8)) s))%Z)),29%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDid3_crc_calculate__tmp) s) <
             (eval (ENum (8)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,27%positive)::
             (10%positive,ANone,11%positive)::
             (10%positive,ANone,13%positive)::
             (10%positive,ANone,15%positive)::
             (10%positive,ANone,17%positive)::
             (10%positive,ANone,19%positive)::
             (10%positive,ANone,21%positive)::
             (10%positive,ANone,23%positive)::
             (10%positive,ANone,25%positive)::
             (11%positive,(AAssign IDid3_crc_calculate_crc None),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDid3_crc_calculate_crc None),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDid3_crc_calculate_crc None),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDid3_crc_calculate_crc None),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDid3_crc_calculate_crc None),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDid3_crc_calculate_crc None),22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDid3_crc_calculate_crc None),24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,AWeaken,28%positive)::
             (27%positive,AWeaken,28%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,(AAssign IDid3_crc_calculate_crc None),31%positive)::
             (31%positive,(AAssign IDid3_crc_calculate_crc None),32%positive)::
             (32%positive,(AAssign IDid3_crc_calculate_crc None),33%positive)::
             (33%positive,(AAssign IDid3_crc_calculate_crc None),34%positive)::
             (34%positive,(AAssign IDid3_crc_calculate_crc None),35%positive)::
             (35%positive,(AAssign IDid3_crc_calculate_crc None),36%positive)::
             (36%positive,(AAssign IDid3_crc_calculate_crc None),37%positive)::
             (37%positive,(AAssign IDid3_crc_calculate_crc None),38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDid3_crc_calculate__tmp
             (Some (ESub (EVar IDid3_crc_calculate__tmp) (ENum (8))))),
             40%positive)::(40%positive,ANone,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDid3_crc_calculate_z
             (Some (EAdd (ENum (1)) (EVar IDid3_crc_calculate_z)))),
             43%positive)::(43%positive,AWeaken,8%positive)::nil
|}.

Definition id3_crc_calculate_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDid3_crc_calculate_z) <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 3%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate_z) <= 0 /\ -1 * (s IDid3_crc_calculate__tmp) <= 0)%Z
    | 4%positive => (-1 * (s IDid3_crc_calculate__tmp) <= 0 /\ 1 * (s IDid3_crc_calculate_z) <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 5%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 6%positive => (1 * (s IDid3_crc_calculate_z) <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 7%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 8%positive => (-1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 9%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate__tmp) + -7 <= 0)%Z
    | 10%positive => (1 * (s IDid3_crc_calculate__tmp) + -7 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 11%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate__tmp) + -7 <= 0)%Z
    | 12%positive => (1 * (s IDid3_crc_calculate__tmp) + -7 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 13%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate__tmp) + -7 <= 0)%Z
    | 14%positive => (1 * (s IDid3_crc_calculate__tmp) + -7 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 15%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate__tmp) + -7 <= 0)%Z
    | 16%positive => (1 * (s IDid3_crc_calculate__tmp) + -7 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 17%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate__tmp) + -7 <= 0)%Z
    | 18%positive => (1 * (s IDid3_crc_calculate__tmp) + -7 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 19%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate__tmp) + -7 <= 0)%Z
    | 20%positive => (1 * (s IDid3_crc_calculate__tmp) + -7 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 21%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate__tmp) + -7 <= 0)%Z
    | 22%positive => (1 * (s IDid3_crc_calculate__tmp) + -7 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 23%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate__tmp) + -7 <= 0)%Z
    | 24%positive => (1 * (s IDid3_crc_calculate__tmp) + -7 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 25%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate__tmp) + -7 <= 0)%Z
    | 26%positive => (1 * (s IDid3_crc_calculate__tmp) + -7 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 27%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ 1 * (s IDid3_crc_calculate__tmp) + -7 <= 0)%Z
    | 28%positive => (1 * (s IDid3_crc_calculate__tmp) + -7 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 29%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ -1 * (s IDid3_crc_calculate__tmp) + 8 <= 0)%Z
    | 30%positive => (-1 * (s IDid3_crc_calculate__tmp) + 8 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 31%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ -1 * (s IDid3_crc_calculate__tmp) + 8 <= 0)%Z
    | 32%positive => (-1 * (s IDid3_crc_calculate__tmp) + 8 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 33%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ -1 * (s IDid3_crc_calculate__tmp) + 8 <= 0)%Z
    | 34%positive => (-1 * (s IDid3_crc_calculate__tmp) + 8 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 35%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ -1 * (s IDid3_crc_calculate__tmp) + 8 <= 0)%Z
    | 36%positive => (-1 * (s IDid3_crc_calculate__tmp) + 8 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 37%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ -1 * (s IDid3_crc_calculate__tmp) + 8 <= 0)%Z
    | 38%positive => (-1 * (s IDid3_crc_calculate__tmp) + 8 <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 39%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ -1 * (s IDid3_crc_calculate__tmp) + 8 <= 0)%Z
    | 40%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ -1 * (s IDid3_crc_calculate__tmp) <= 0)%Z
    | 41%positive => (-1 * (s IDid3_crc_calculate__tmp) <= 0 /\ -1 * (s IDid3_crc_calculate_z) <= 0)%Z
    | 42%positive => (-1 * (s IDid3_crc_calculate_z) <= 0 /\ -1 * (s IDid3_crc_calculate__tmp) <= 0)%Z
    | 43%positive => (-1 * (s IDid3_crc_calculate__tmp) <= 0 /\ -1 * (s IDid3_crc_calculate_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition id3_crc_calculate_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1 # 8) * max0((s IDid3_crc_calculate_length)))%Q
    | 2%positive => ((s IDid3_crc_calculate_z)
                     + (1 # 8) * max0((s IDid3_crc_calculate_length)))%Q
    | 3%positive => ((s IDid3_crc_calculate_z)
                     + (1 # 8) * max0((s IDid3_crc_calculate_length)))%Q
    | 4%positive => ((s IDid3_crc_calculate_z)
                     + (1 # 8) * max0((s IDid3_crc_calculate_length)))%Q
    | 5%positive => ((s IDid3_crc_calculate_z)
                     + (1 # 8) * max0((s IDid3_crc_calculate__tmp)))%Q
    | 6%positive => ((s IDid3_crc_calculate_z)
                     + (1 # 8) * max0((s IDid3_crc_calculate__tmp)))%Q
    | 7%positive => ((s IDid3_crc_calculate_z)
                     + (1 # 8) * max0((s IDid3_crc_calculate__tmp)))%Q
    | 8%positive => ((s IDid3_crc_calculate_z)
                     + (1 # 8) * max0((s IDid3_crc_calculate__tmp)))%Q
    | 9%positive => ((s IDid3_crc_calculate_z)
                     + (1 # 8) * max0((s IDid3_crc_calculate__tmp)))%Q
    | 10%positive => ((s IDid3_crc_calculate_z))%Q
    | 11%positive => ((s IDid3_crc_calculate_z))%Q
    | 12%positive => ((s IDid3_crc_calculate_z))%Q
    | 13%positive => ((s IDid3_crc_calculate_z))%Q
    | 14%positive => ((s IDid3_crc_calculate_z))%Q
    | 15%positive => ((s IDid3_crc_calculate_z))%Q
    | 16%positive => ((s IDid3_crc_calculate_z))%Q
    | 17%positive => ((s IDid3_crc_calculate_z))%Q
    | 18%positive => ((s IDid3_crc_calculate_z))%Q
    | 19%positive => ((s IDid3_crc_calculate_z))%Q
    | 20%positive => ((s IDid3_crc_calculate_z))%Q
    | 21%positive => ((s IDid3_crc_calculate_z))%Q
    | 22%positive => ((s IDid3_crc_calculate_z))%Q
    | 23%positive => ((s IDid3_crc_calculate_z))%Q
    | 24%positive => ((s IDid3_crc_calculate_z))%Q
    | 25%positive => ((s IDid3_crc_calculate_z))%Q
    | 26%positive => ((s IDid3_crc_calculate_z))%Q
    | 27%positive => ((s IDid3_crc_calculate_z))%Q
    | 28%positive => ((s IDid3_crc_calculate_z))%Q
    | 29%positive => ((s IDid3_crc_calculate_z)
                      + (1 # 8) * max0((s IDid3_crc_calculate__tmp)))%Q
    | 30%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0(-8 + (s IDid3_crc_calculate__tmp)))%Q
    | 31%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0(-8 + (s IDid3_crc_calculate__tmp)))%Q
    | 32%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0(-8 + (s IDid3_crc_calculate__tmp)))%Q
    | 33%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0(-8 + (s IDid3_crc_calculate__tmp)))%Q
    | 34%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0(-8 + (s IDid3_crc_calculate__tmp)))%Q
    | 35%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0(-8 + (s IDid3_crc_calculate__tmp)))%Q
    | 36%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0(-8 + (s IDid3_crc_calculate__tmp)))%Q
    | 37%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0(-8 + (s IDid3_crc_calculate__tmp)))%Q
    | 38%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0(-8 + (s IDid3_crc_calculate__tmp)))%Q
    | 39%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0(-8 + (s IDid3_crc_calculate__tmp)))%Q
    | 40%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0((s IDid3_crc_calculate__tmp)))%Q
    | 41%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0((s IDid3_crc_calculate__tmp)))%Q
    | 42%positive => ((1 # 1) + (s IDid3_crc_calculate_z)
                      + (1 # 8) * max0((s IDid3_crc_calculate__tmp)))%Q
    | 43%positive => ((s IDid3_crc_calculate_z)
                      + (1 # 8) * max0((s IDid3_crc_calculate__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition id3_crc_calculate_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*0 0.125*) F_max0_ge_0 ((s IDid3_crc_calculate__tmp))]
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
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-0.125 0*) F_max0_pre_decrement ((s IDid3_crc_calculate__tmp)) (8)]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | _ => []
  end.


Theorem id3_crc_calculate_ai_correct:
  forall s p' s', steps (g_start id3_crc_calculate) s (g_edges id3_crc_calculate) p' s' -> id3_crc_calculate_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem id3_crc_calculate_pot_correct:
  forall s p' s',
    steps (g_start id3_crc_calculate) s (g_edges id3_crc_calculate) p' s' ->
    (id3_crc_calculate_pot (g_start id3_crc_calculate) s >= id3_crc_calculate_pot p' s')%Q.
Proof.
  check_lp id3_crc_calculate_ai_correct id3_crc_calculate_hints.
Qed.

