Require Import pasta.Pasta.

Notation IDmakebwmap_z := 1%positive.
Notation IDmakebwmap__tmp := 2%positive.
Notation IDmakebwmap_bitspersample := 3%positive.
Notation IDmakebwmap_c := 4%positive.
Notation IDmakebwmap_i := 5%positive.
Notation IDmakebwmap_img_dref_off28 := 6%positive.
Notation IDmakebwmap_nsamples := 7%positive.
Notation IDmakebwmap_img := 8%positive.
Definition makebwmap : graph := {|
  g_start := 1%positive;
  g_end := 44%positive;
  g_edges := (1%positive,(AAssign IDmakebwmap_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDmakebwmap_bitspersample
             (Some (EVar IDmakebwmap_img_dref_off28))),3%positive)::
             (3%positive,(AAssign IDmakebwmap_nsamples None),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,ANone,41%positive)::(5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDmakebwmap_i (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDmakebwmap_i) s) <
             (eval (ENum (256)) s))%Z)),14%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDmakebwmap_i) s) >=
             (eval (ENum (256)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDmakebwmap__tmp (Some (ENum (1)))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,44%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,35%positive)::
             (15%positive,ANone,26%positive)::
             (15%positive,ANone,21%positive)::
             (15%positive,ANone,18%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDmakebwmap_c None),17%positive)::
             (17%positive,ANone,35%positive)::
             (18%positive,(AAssign IDmakebwmap_c None),19%positive)::
             (19%positive,(AAssign IDmakebwmap_c None),20%positive)::
             (20%positive,ANone,35%positive)::
             (21%positive,(AAssign IDmakebwmap_c None),22%positive)::
             (22%positive,(AAssign IDmakebwmap_c None),23%positive)::
             (23%positive,(AAssign IDmakebwmap_c None),24%positive)::
             (24%positive,(AAssign IDmakebwmap_c None),25%positive)::
             (25%positive,ANone,35%positive)::
             (26%positive,(AAssign IDmakebwmap_c None),27%positive)::
             (27%positive,(AAssign IDmakebwmap_c None),28%positive)::
             (28%positive,(AAssign IDmakebwmap_c None),29%positive)::
             (29%positive,(AAssign IDmakebwmap_c None),30%positive)::
             (30%positive,(AAssign IDmakebwmap_c None),31%positive)::
             (31%positive,(AAssign IDmakebwmap_c None),32%positive)::
             (32%positive,(AAssign IDmakebwmap_c None),33%positive)::
             (33%positive,(AAssign IDmakebwmap_c None),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDmakebwmap_i
             (Some (EAdd (EVar IDmakebwmap_i) (ENum (1))))),37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDmakebwmap_z (Some (EAdd (ENum (1))
             (EVar IDmakebwmap_z)))),40%positive)::
             (40%positive,AWeaken,9%positive)::
             (41%positive,(AAssign IDmakebwmap__tmp (Some (ENum (0)))),
             42%positive)::(42%positive,ANone,43%positive)::
             (43%positive,AWeaken,44%positive)::nil
|}.

Definition makebwmap_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_z) <= 0)%Z
    | 4%positive => (1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0)%Z
    | 5%positive => (-1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_z) <= 0)%Z
    | 6%positive => (1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0)%Z
    | 7%positive => (-1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_i) <= 0)%Z
    | 8%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ 1 * (s IDmakebwmap_i) <= 0 /\ 1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0)%Z
    | 9%positive => (-1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) <= 0 /\ 1 * (s IDmakebwmap_i) + -256 <= 0)%Z
    | 10%positive => (1 * (s IDmakebwmap_i) + -256 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) + 256 <= 0)%Z
    | 11%positive => (-1 * (s IDmakebwmap_i) + 256 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -256 <= 0)%Z
    | 12%positive => (1 * (s IDmakebwmap_i) + -256 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) + 256 <= 0 /\ 1 * (s IDmakebwmap__tmp) + -1 <= 0 /\ -1 * (s IDmakebwmap__tmp) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDmakebwmap__tmp) + 1 <= 0 /\ 1 * (s IDmakebwmap__tmp) + -1 <= 0 /\ -1 * (s IDmakebwmap_i) + 256 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -256 <= 0)%Z
    | 14%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 15%positive => (1 * (s IDmakebwmap_i) + -255 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) <= 0)%Z
    | 16%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 17%positive => (1 * (s IDmakebwmap_i) + -255 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) <= 0)%Z
    | 18%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 19%positive => (1 * (s IDmakebwmap_i) + -255 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) <= 0)%Z
    | 20%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 21%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 22%positive => (1 * (s IDmakebwmap_i) + -255 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) <= 0)%Z
    | 23%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 24%positive => (1 * (s IDmakebwmap_i) + -255 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) <= 0)%Z
    | 25%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 26%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 27%positive => (1 * (s IDmakebwmap_i) + -255 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) <= 0)%Z
    | 28%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 29%positive => (1 * (s IDmakebwmap_i) + -255 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) <= 0)%Z
    | 30%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 31%positive => (1 * (s IDmakebwmap_i) + -255 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) <= 0)%Z
    | 32%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 33%positive => (1 * (s IDmakebwmap_i) + -255 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) <= 0)%Z
    | 34%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 35%positive => (1 * (s IDmakebwmap_i) + -255 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) <= 0)%Z
    | 36%positive => (-1 * (s IDmakebwmap_i) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_i) + -255 <= 0)%Z
    | 37%positive => (-1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) + 1 <= 0 /\ 1 * (s IDmakebwmap_i) + -256 <= 0)%Z
    | 38%positive => (1 * (s IDmakebwmap_i) + -256 <= 0 /\ -1 * (s IDmakebwmap_i) + 1 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0)%Z
    | 39%positive => (-1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_i) + 1 <= 0 /\ 1 * (s IDmakebwmap_i) + -256 <= 0)%Z
    | 40%positive => (1 * (s IDmakebwmap_i) + -256 <= 0 /\ -1 * (s IDmakebwmap_i) + 1 <= 0 /\ -1 * (s IDmakebwmap_z) + 1 <= 0)%Z
    | 41%positive => (1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0)%Z
    | 42%positive => (-1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap_z) <= 0 /\ 1 * (s IDmakebwmap__tmp) <= 0 /\ -1 * (s IDmakebwmap__tmp) <= 0)%Z
    | 43%positive => (-1 * (s IDmakebwmap__tmp) <= 0 /\ 1 * (s IDmakebwmap__tmp) <= 0 /\ 1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap_z) <= 0)%Z
    | 44%positive => (1 * (s IDmakebwmap__tmp) + -1 <= 0 /\ -1 * (s IDmakebwmap_z) <= 0 /\ -1 * (s IDmakebwmap__tmp) <= 0)%Z
    | _ => False
  end.

Definition makebwmap_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((256 # 1))%Q
    | 2%positive => ((256 # 1) + (s IDmakebwmap_z))%Q
    | 3%positive => ((256 # 1) + (s IDmakebwmap_z))%Q
    | 4%positive => ((256 # 1) + (s IDmakebwmap_z))%Q
    | 5%positive => ((256 # 1) + (s IDmakebwmap_z))%Q
    | 6%positive => ((256 # 1) + (s IDmakebwmap_z))%Q
    | 7%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 8%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 9%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 10%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 11%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 12%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 13%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 14%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 15%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 16%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 17%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 18%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 19%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 20%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 21%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 22%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 23%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 24%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 25%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 26%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 27%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 28%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 29%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 30%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 31%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 32%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 33%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 34%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 35%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 36%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 37%positive => ((257 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 38%positive => ((257 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 39%positive => ((257 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 40%positive => ((256 # 1) - (s IDmakebwmap_i) + (s IDmakebwmap_z))%Q
    | 41%positive => ((256 # 1) + (s IDmakebwmap_z))%Q
    | 42%positive => ((256 # 1) + (s IDmakebwmap_z))%Q
    | 43%positive => ((256 # 1) + (s IDmakebwmap_z))%Q
    | 44%positive => ((s IDmakebwmap_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition makebwmap_hints (p : node) (s : state) := 
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
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (256
                                                             - (s IDmakebwmap_i)) (255
                                                                    - (s IDmakebwmap_i)));
                      (*-1 0*) F_max0_ge_0 (255 - (s IDmakebwmap_i));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (256
                                                                    - (s IDmakebwmap_i)) (0))) (F_max0_ge_0 (256
                                                                    - (s IDmakebwmap_i)))]
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
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => [(*-256 0*) F_one]
    | 44%positive => []
    | _ => []
  end.


Theorem makebwmap_ai_correct:
  forall s p' s', steps (g_start makebwmap) s (g_edges makebwmap) p' s' -> makebwmap_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem makebwmap_pot_correct:
  forall s p' s',
    steps (g_start makebwmap) s (g_edges makebwmap) p' s' ->
    (makebwmap_pot (g_start makebwmap) s >= makebwmap_pot p' s')%Q.
Proof.
  check_lp makebwmap_ai_correct makebwmap_hints.
Qed.

