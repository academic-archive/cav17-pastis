Require Import pasta.Pasta.

Notation IDcmd_image_data_z := 1%positive.
Notation IDcmd_image_data__tmp := 2%positive.
Notation IDcmd_image_data__tmp1 := 3%positive.
Notation IDcmd_image_data__tmp2 := 4%positive.
Notation IDcmd_image_data__tmp3 := 5%positive.
Notation IDcmd_image_data__tmp4 := 6%positive.
Notation IDcmd_image_data_i := 7%positive.
Notation IDcmd_image_data_len := 8%positive.
Notation IDcmd_image_data_nbytes := 9%positive.
Notation IDcmd_image_data_bytes_per_row := 10%positive.
Notation IDcmd_image_data_cldev := 11%positive.
Notation IDcmd_image_data_data := 12%positive.
Notation IDcmd_image_data_data_x := 13%positive.
Notation IDcmd_image_data_h := 14%positive.
Notation IDcmd_image_data_pcls := 15%positive.
Notation IDcmd_image_data_raster := 16%positive.
Definition cmd_image_data : graph := {|
  g_start := 1%positive;
  g_end := 50%positive;
  g_edges := (1%positive,(AAssign IDcmd_image_data_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDcmd_image_data__tmp
             (Some (EVar IDcmd_image_data_data_x))),3%positive)::
             (3%positive,(AAssign IDcmd_image_data__tmp4
             (Some (EVar IDcmd_image_data_raster))),4%positive)::
             (4%positive,(AAssign IDcmd_image_data__tmp3
             (Some (EVar IDcmd_image_data_bytes_per_row))),5%positive)::
             (5%positive,(AAssign IDcmd_image_data__tmp2
             (Some (EVar IDcmd_image_data_h))),6%positive)::
             (6%positive,(AAssign IDcmd_image_data_nbytes
             (Some (EMul (EVar IDcmd_image_data__tmp3)
             (EVar IDcmd_image_data__tmp2)))),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,ANone,10%positive)::(8%positive,ANone,9%positive)::
             (9%positive,ANone,16%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,(AAssign IDcmd_image_data_len None),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => True)),15%positive)::
             (13%positive,(AGuard (fun s => True)),14%positive)::
             (14%positive,AWeaken,21%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,47%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,44%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,28%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,29%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDcmd_image_data_i (Some (ENum (0)))),
             30%positive)::(30%positive,ANone,31%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard (fun s => ((eval (EVar IDcmd_image_data_i)
             s) < (eval (EVar IDcmd_image_data__tmp2) s))%Z)),37%positive)::
             (32%positive,(AGuard (fun s => ((eval (EVar IDcmd_image_data_i)
             s) >= (eval (EVar IDcmd_image_data__tmp2) s))%Z)),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AAssign IDcmd_image_data__tmp1 (Some (ENum (0)))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,AWeaken,50%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDcmd_image_data_i
             (Some (EAdd (EVar IDcmd_image_data_i) (ENum (1))))),40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDcmd_image_data_z (Some (EAdd (ENum (1))
             (EVar IDcmd_image_data_z)))),43%positive)::
             (43%positive,AWeaken,32%positive)::
             (44%positive,(AAssign IDcmd_image_data__tmp1 None),45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,AWeaken,50%positive)::
             (47%positive,(AAssign IDcmd_image_data__tmp1 None),48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,AWeaken,50%positive)::nil
|}.

Definition cmd_image_data_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 4%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 5%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 6%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 7%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 8%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 10%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 11%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 12%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 13%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 14%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 15%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 16%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 17%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 18%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 19%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 20%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 21%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 22%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 23%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 24%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 25%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 26%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 27%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 28%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 29%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 30%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_i) <= 0 /\ -1 * (s IDcmd_image_data_i) <= 0)%Z
    | 31%positive => (-1 * (s IDcmd_image_data_i) <= 0 /\ 1 * (s IDcmd_image_data_i) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 32%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_i) <= 0)%Z
    | 33%positive => (-1 * (s IDcmd_image_data_i) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data__tmp2)+ -1 * (s IDcmd_image_data_i) <= 0)%Z
    | 34%positive => (1 * (s IDcmd_image_data__tmp2)+ -1 * (s IDcmd_image_data_i) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_i) <= 0)%Z
    | 35%positive => (-1 * (s IDcmd_image_data_i) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data__tmp2)+ -1 * (s IDcmd_image_data_i) <= 0 /\ 1 * (s IDcmd_image_data__tmp1) <= 0 /\ -1 * (s IDcmd_image_data__tmp1) <= 0)%Z
    | 36%positive => (-1 * (s IDcmd_image_data__tmp1) <= 0 /\ 1 * (s IDcmd_image_data__tmp1) <= 0 /\ 1 * (s IDcmd_image_data__tmp2)+ -1 * (s IDcmd_image_data_i) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_i) <= 0)%Z
    | 37%positive => (-1 * (s IDcmd_image_data_i) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data__tmp2)+ 1 * (s IDcmd_image_data_i) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDcmd_image_data__tmp2)+ 1 * (s IDcmd_image_data_i) + 1 <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_i) <= 0)%Z
    | 39%positive => (-1 * (s IDcmd_image_data_i) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data__tmp2)+ 1 * (s IDcmd_image_data_i) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_i) + 1 <= 0 /\ -1 * (s IDcmd_image_data__tmp2)+ 1 * (s IDcmd_image_data_i) <= 0)%Z
    | 41%positive => (-1 * (s IDcmd_image_data__tmp2)+ 1 * (s IDcmd_image_data_i) <= 0 /\ -1 * (s IDcmd_image_data_i) + 1 <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 42%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_i) + 1 <= 0 /\ -1 * (s IDcmd_image_data__tmp2)+ 1 * (s IDcmd_image_data_i) <= 0)%Z
    | 43%positive => (-1 * (s IDcmd_image_data__tmp2)+ 1 * (s IDcmd_image_data_i) <= 0 /\ -1 * (s IDcmd_image_data_i) + 1 <= 0 /\ -1 * (s IDcmd_image_data_z) + 1 <= 0)%Z
    | 44%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 45%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 46%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 47%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 48%positive => (1 * (s IDcmd_image_data_z) <= 0 /\ -1 * (s IDcmd_image_data_z) <= 0)%Z
    | 49%positive => (-1 * (s IDcmd_image_data_z) <= 0 /\ 1 * (s IDcmd_image_data_z) <= 0)%Z
    | 50%positive => (-1 * (s IDcmd_image_data_z) <= 0)%Z
    | _ => False
  end.

Definition cmd_image_data_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDcmd_image_data_h)))%Q
    | 2%positive => (max0((s IDcmd_image_data_h))
                     + max0((s IDcmd_image_data_z)))%Q
    | 3%positive => (max0((s IDcmd_image_data_h))
                     + max0((s IDcmd_image_data_z)))%Q
    | 4%positive => (max0((s IDcmd_image_data_h))
                     + max0((s IDcmd_image_data_z)))%Q
    | 5%positive => (max0((s IDcmd_image_data_h))
                     + max0((s IDcmd_image_data_z)))%Q
    | 6%positive => (max0((s IDcmd_image_data__tmp2))
                     + max0((s IDcmd_image_data_z)))%Q
    | 7%positive => (max0((s IDcmd_image_data__tmp2))
                     + max0((s IDcmd_image_data_z)))%Q
    | 8%positive => ((s IDcmd_image_data_z)
                     + max0((s IDcmd_image_data__tmp2)))%Q
    | 9%positive => ((s IDcmd_image_data_z)
                     + max0((s IDcmd_image_data__tmp2)))%Q
    | 10%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 11%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 12%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 13%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 14%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 15%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 16%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 17%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 18%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 19%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 20%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 21%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 22%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 23%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 24%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 25%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 26%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 27%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 28%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 29%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 30%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 31%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 32%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 33%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 34%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 35%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 36%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 37%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 38%positive => ((1 # 1) + (s IDcmd_image_data_z)
                      + max0(-1 + (s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 39%positive => ((1 # 1) + (s IDcmd_image_data_z)
                      + max0(-1 + (s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 40%positive => ((1 # 1) + (s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 41%positive => ((1 # 1) + (s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 42%positive => ((1 # 1) + (s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 43%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)
                             - (s IDcmd_image_data_i)))%Q
    | 44%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 45%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 46%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 47%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 48%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 49%positive => ((s IDcmd_image_data_z)
                      + max0((s IDcmd_image_data__tmp2)))%Q
    | 50%positive => ((s IDcmd_image_data_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cmd_image_data_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcmd_image_data_z))) (F_check_ge ((s IDcmd_image_data_z)) (0))]
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
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcmd_image_data__tmp2)
                                                             - (s IDcmd_image_data_i)) (-1
                                                                    + (s IDcmd_image_data__tmp2)
                                                                    - (s IDcmd_image_data_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDcmd_image_data__tmp2)
                                            - (s IDcmd_image_data_i))]
    | 37%positive => [(*-1 0*) F_max0_pre_decrement ((s IDcmd_image_data__tmp2)
                                                     - (s IDcmd_image_data_i)) (1)]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => [(*-1 0*) F_max0_ge_0 ((s IDcmd_image_data__tmp2))]
    | 47%positive => []
    | 48%positive => []
    | 49%positive => [(*-1 0*) F_max0_ge_0 ((s IDcmd_image_data__tmp2))]
    | 50%positive => []
    | _ => []
  end.


Theorem cmd_image_data_ai_correct:
  forall s p' s', steps (g_start cmd_image_data) s (g_edges cmd_image_data) p' s' -> cmd_image_data_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cmd_image_data_pot_correct:
  forall s p' s',
    steps (g_start cmd_image_data) s (g_edges cmd_image_data) p' s' ->
    (cmd_image_data_pot (g_start cmd_image_data) s >= cmd_image_data_pot p' s')%Q.
Proof.
  check_lp cmd_image_data_ai_correct cmd_image_data_hints.
Qed.

