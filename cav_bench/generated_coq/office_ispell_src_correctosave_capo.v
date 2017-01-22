Require Import pasta.Pasta.

Notation IDsave_cap_z := 1%positive.
Notation IDsave_cap__tmp := 2%positive.
Notation IDsave_cap_hitno := 3%positive.
Notation IDsave_cap_numhits := 4%positive.
Notation IDsave_cap_preadd := 5%positive.
Notation IDsave_cap_prestrip := 6%positive.
Notation IDsave_cap_sufadd := 7%positive.
Notation IDsave_cap_sufstrip := 8%positive.
Notation IDsave_cap_pattern := 9%positive.
Notation IDsave_cap_savearea := 10%positive.
Notation IDsave_cap_word := 11%positive.
Definition save_cap : graph := {|
  g_start := 1%positive;
  g_end := 41%positive;
  g_edges := (1%positive,(AAssign IDsave_cap_z (Some (ENum (0)))),2%positive)::
             (2%positive,AWeaken,3%positive)::
             (3%positive,ANone,38%positive)::(3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDsave_cap_hitno
             (Some (EVar IDsave_cap_numhits))),5%positive)::
             (5%positive,ANone,6%positive)::
             (6%positive,(AAssign IDsave_cap_hitno
             (Some (EAdd (EVar IDsave_cap_hitno) (ENum (-1))))),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDsave_cap_hitno) (ENum (-1)))
             s) >= (eval (ENum (0)) s))%Z)),10%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDsave_cap_hitno) (ENum (-1))) s) <
             (eval (ENum (0)) s))%Z)),9%positive)::
             (9%positive,AWeaken,15%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard (fun s => True)),18%positive)::
             (13%positive,(AGuard (fun s => True)),14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AAssign IDsave_cap__tmp None),16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,41%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,24%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDsave_cap_preadd (Some (ENum (0)))),
             21%positive)::
             (21%positive,(AAssign IDsave_cap_prestrip (Some (ENum (0)))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,AWeaken,28%positive)::
             (24%positive,(AAssign IDsave_cap_prestrip None),25%positive)::
             (25%positive,(AAssign IDsave_cap_preadd None),26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,32%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDsave_cap_sufstrip (Some (ENum (0)))),
             30%positive)::
             (30%positive,(AAssign IDsave_cap_sufadd (Some (ENum (0)))),
             31%positive)::(31%positive,ANone,35%positive)::
             (32%positive,(AAssign IDsave_cap_sufstrip None),33%positive)::
             (33%positive,(AAssign IDsave_cap_sufadd None),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDsave_cap_z (Some (EAdd (ENum (1))
             (EVar IDsave_cap_z)))),6%positive)::
             (38%positive,(AAssign IDsave_cap__tmp (Some (ENum (0)))),
             39%positive)::(39%positive,ANone,40%positive)::
             (40%positive,AWeaken,41%positive)::nil
|}.

Definition save_cap_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsave_cap_z) <= 0 /\ 1 * (s IDsave_cap_z) <= 0)%Z
    | 4%positive => (1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 5%positive => (-1 * (s IDsave_cap_z) <= 0 /\ 1 * (s IDsave_cap_z) <= 0)%Z
    | 6%positive => (-1 * (s IDsave_cap_z) <= 0)%Z
    | 7%positive => (-1 * (s IDsave_cap_z) <= 0)%Z
    | 8%positive => (-1 * (s IDsave_cap_z) <= 0)%Z
    | 9%positive => (-1 * (s IDsave_cap_z) <= 0 /\ 1 * (s IDsave_cap_hitno) <= 0)%Z
    | 10%positive => (-1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0)%Z
    | 11%positive => (-1 * (s IDsave_cap_hitno) + 1 <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 12%positive => (-1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDsave_cap_hitno) + 1 <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 14%positive => (-1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDsave_cap_z) <= 0)%Z
    | 16%positive => (-1 * (s IDsave_cap_z) <= 0)%Z
    | 17%positive => (-1 * (s IDsave_cap_z) <= 0)%Z
    | 18%positive => (-1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDsave_cap_hitno) + 1 <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 20%positive => (-1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDsave_cap_hitno) + 1 <= 0 /\ -1 * (s IDsave_cap_z) <= 0 /\ 1 * (s IDsave_cap_preadd) <= 0 /\ -1 * (s IDsave_cap_preadd) <= 0)%Z
    | 22%positive => (-1 * (s IDsave_cap_preadd) <= 0 /\ 1 * (s IDsave_cap_preadd) <= 0 /\ -1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0 /\ 1 * (s IDsave_cap_prestrip) <= 0 /\ -1 * (s IDsave_cap_prestrip) <= 0)%Z
    | 23%positive => (-1 * (s IDsave_cap_prestrip) <= 0 /\ 1 * (s IDsave_cap_prestrip) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0 /\ -1 * (s IDsave_cap_z) <= 0 /\ 1 * (s IDsave_cap_preadd) <= 0 /\ -1 * (s IDsave_cap_preadd) <= 0)%Z
    | 24%positive => (-1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDsave_cap_hitno) + 1 <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 26%positive => (-1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0)%Z
    | 27%positive => (-1 * (s IDsave_cap_hitno) + 1 <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 28%positive => (-1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDsave_cap_hitno) + 1 <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 30%positive => (-1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0 /\ 1 * (s IDsave_cap_sufstrip) <= 0 /\ -1 * (s IDsave_cap_sufstrip) <= 0)%Z
    | 31%positive => (-1 * (s IDsave_cap_sufstrip) <= 0 /\ 1 * (s IDsave_cap_sufstrip) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0 /\ -1 * (s IDsave_cap_z) <= 0 /\ 1 * (s IDsave_cap_sufadd) <= 0 /\ -1 * (s IDsave_cap_sufadd) <= 0)%Z
    | 32%positive => (-1 * (s IDsave_cap_hitno) + 1 <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 33%positive => (-1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDsave_cap_hitno) + 1 <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 35%positive => (-1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDsave_cap_hitno) + 1 <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 37%positive => (-1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_hitno) + 1 <= 0)%Z
    | 38%positive => (1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 39%positive => (-1 * (s IDsave_cap_z) <= 0 /\ 1 * (s IDsave_cap_z) <= 0 /\ 1 * (s IDsave_cap__tmp) <= 0 /\ -1 * (s IDsave_cap__tmp) <= 0)%Z
    | 40%positive => (-1 * (s IDsave_cap__tmp) <= 0 /\ 1 * (s IDsave_cap__tmp) <= 0 /\ 1 * (s IDsave_cap_z) <= 0 /\ -1 * (s IDsave_cap_z) <= 0)%Z
    | 41%positive => (-1 * (s IDsave_cap_z) <= 0)%Z
    | _ => False
  end.

Definition save_cap_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDsave_cap_numhits)))%Q
    | 2%positive => ((s IDsave_cap_z) + max0(-1 + (s IDsave_cap_numhits)))%Q
    | 3%positive => ((s IDsave_cap_z) + max0(-1 + (s IDsave_cap_numhits)))%Q
    | 4%positive => ((s IDsave_cap_z) + max0(-1 + (s IDsave_cap_numhits)))%Q
    | 5%positive => ((s IDsave_cap_z) + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 6%positive => ((s IDsave_cap_z) + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 7%positive => ((s IDsave_cap_z) + max0((s IDsave_cap_hitno)))%Q
    | 8%positive => ((s IDsave_cap_z) + max0((s IDsave_cap_hitno)))%Q
    | 9%positive => ((s IDsave_cap_z) + max0((s IDsave_cap_hitno)))%Q
    | 10%positive => ((s IDsave_cap_z) + max0((s IDsave_cap_hitno)))%Q
    | 11%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 12%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 13%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 14%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 15%positive => ((s IDsave_cap_z))%Q
    | 16%positive => ((s IDsave_cap_z))%Q
    | 17%positive => ((s IDsave_cap_z))%Q
    | 18%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 19%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 20%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 21%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 22%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 23%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 24%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 25%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 26%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 27%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 28%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 29%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 30%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 31%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 32%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 33%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 34%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 35%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 36%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 37%positive => ((1 # 1) + (s IDsave_cap_z)
                      + max0(-1 + (s IDsave_cap_hitno)))%Q
    | 38%positive => ((s IDsave_cap_z) + max0(-1 + (s IDsave_cap_numhits)))%Q
    | 39%positive => ((s IDsave_cap_z) + max0(-1 + (s IDsave_cap_numhits)))%Q
    | 40%positive => ((s IDsave_cap_z) + max0(-1 + (s IDsave_cap_numhits)))%Q
    | 41%positive => ((s IDsave_cap_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition save_cap_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDsave_cap_hitno)) (-1
                                                                    + (s IDsave_cap_hitno)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                + (s IDsave_cap_hitno))) (F_check_ge (0) (0))]
    | 10%positive => [(*0 1*) F_max0_pre_decrement ((s IDsave_cap_hitno)) (1)]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDsave_cap_hitno))]
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
    | 40%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDsave_cap_numhits))) (F_check_ge (0) (0))]
    | 41%positive => []
    | _ => []
  end.


Theorem save_cap_ai_correct:
  forall s p' s', steps (g_start save_cap) s (g_edges save_cap) p' s' -> save_cap_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem save_cap_pot_correct:
  forall s p' s',
    steps (g_start save_cap) s (g_edges save_cap) p' s' ->
    (save_cap_pot (g_start save_cap) s >= save_cap_pot p' s')%Q.
Proof.
  check_lp save_cap_ai_correct save_cap_hints.
Qed.

