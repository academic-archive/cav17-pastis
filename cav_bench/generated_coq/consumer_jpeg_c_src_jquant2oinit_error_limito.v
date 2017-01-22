Require Import pasta.Pasta.

Notation IDinit_error_limit_z := 1%positive.
Notation IDinit_error_limit_in := 2%positive.
Notation IDinit_error_limit_out := 3%positive.
Notation IDinit_error_limit_cinfo := 4%positive.
Definition init_error_limit : graph := {|
  g_start := 1%positive;
  g_end := 16%positive;
  g_edges := (1%positive,(AAssign IDinit_error_limit_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDinit_error_limit_out (Some (ENum (0)))),
             3%positive)::
             (3%positive,(AAssign IDinit_error_limit_in (Some (ENum (0)))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDinit_error_limit_in) s) <
             (eval (ENum (16)) s))%Z)),32%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDinit_error_limit_in) s) >=
             (eval (ENum (16)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDinit_error_limit_in) s) <
             (eval (ENum (48)) s))%Z)),24%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDinit_error_limit_in) s) >=
             (eval (ENum (48)) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDinit_error_limit_in) s) <=
             (eval (ENum (255)) s))%Z)),17%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDinit_error_limit_in) s) >
             (eval (ENum (255)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDinit_error_limit_in
             (Some (EAdd (EVar IDinit_error_limit_in) (ENum (1))))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDinit_error_limit_z
             (Some (EAdd (ENum (1)) (EVar IDinit_error_limit_z)))),
             23%positive)::(23%positive,AWeaken,14%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,(AAssign IDinit_error_limit_in
             (Some (EAdd (EVar IDinit_error_limit_in) (ENum (1))))),
             27%positive)::
             (27%positive,(AAssign IDinit_error_limit_out None),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDinit_error_limit_z
             (Some (EAdd (ENum (1)) (EVar IDinit_error_limit_z)))),
             31%positive)::(31%positive,AWeaken,10%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDinit_error_limit_in
             (Some (EAdd (EVar IDinit_error_limit_in) (ENum (1))))),
             35%positive)::
             (35%positive,(AAssign IDinit_error_limit_out
             (Some (EAdd (EVar IDinit_error_limit_out) (ENum (1))))),
             36%positive)::(36%positive,ANone,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDinit_error_limit_z
             (Some (EAdd (ENum (1)) (EVar IDinit_error_limit_z)))),
             39%positive)::(39%positive,AWeaken,6%positive)::nil
|}.

Definition init_error_limit_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0)%Z
    | 3%positive => (-1 * (s IDinit_error_limit_z) <= 0 /\ 1 * (s IDinit_error_limit_z) <= 0 /\ 1 * (s IDinit_error_limit_out) <= 0 /\ -1 * (s IDinit_error_limit_out) <= 0)%Z
    | 4%positive => (-1 * (s IDinit_error_limit_out) <= 0 /\ 1 * (s IDinit_error_limit_out) <= 0 /\ 1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ 1 * (s IDinit_error_limit_in) <= 0 /\ -1 * (s IDinit_error_limit_in) <= 0)%Z
    | 5%positive => (-1 * (s IDinit_error_limit_in) <= 0 /\ 1 * (s IDinit_error_limit_in) <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ 1 * (s IDinit_error_limit_z) <= 0 /\ 1 * (s IDinit_error_limit_out) <= 0 /\ -1 * (s IDinit_error_limit_out) <= 0)%Z
    | 6%positive => (-1 * (s IDinit_error_limit_out) <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) <= 0 /\ 1 * (s IDinit_error_limit_in) + -16 <= 0)%Z
    | 7%positive => (1 * (s IDinit_error_limit_in) + -16 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_out) <= 0 /\ -1 * (s IDinit_error_limit_in) + 16 <= 0)%Z
    | 8%positive => (-1 * (s IDinit_error_limit_in) + 16 <= 0 /\ -1 * (s IDinit_error_limit_out) <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ 1 * (s IDinit_error_limit_in) + -16 <= 0)%Z
    | 9%positive => (1 * (s IDinit_error_limit_in) + -16 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_out) <= 0 /\ -1 * (s IDinit_error_limit_in) + 16 <= 0)%Z
    | 10%positive => (-1 * (s IDinit_error_limit_in) + 16 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ 1 * (s IDinit_error_limit_in) + -48 <= 0)%Z
    | 11%positive => (1 * (s IDinit_error_limit_in) + -48 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 48 <= 0)%Z
    | 12%positive => (-1 * (s IDinit_error_limit_in) + 48 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ 1 * (s IDinit_error_limit_in) + -48 <= 0)%Z
    | 13%positive => (1 * (s IDinit_error_limit_in) + -48 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 48 <= 0)%Z
    | 14%positive => (-1 * (s IDinit_error_limit_in) + 48 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ 1 * (s IDinit_error_limit_in) + -256 <= 0)%Z
    | 15%positive => (1 * (s IDinit_error_limit_in) + -256 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 256 <= 0)%Z
    | 16%positive => (-1 * (s IDinit_error_limit_in) + 256 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ 1 * (s IDinit_error_limit_in) + -256 <= 0)%Z
    | 17%positive => (-1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 48 <= 0 /\ 1 * (s IDinit_error_limit_in) + -255 <= 0)%Z
    | 18%positive => (1 * (s IDinit_error_limit_in) + -255 <= 0 /\ -1 * (s IDinit_error_limit_in) + 48 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0)%Z
    | 19%positive => (-1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 48 <= 0 /\ 1 * (s IDinit_error_limit_in) + -255 <= 0)%Z
    | 20%positive => (-1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 49 <= 0 /\ 1 * (s IDinit_error_limit_in) + -256 <= 0)%Z
    | 21%positive => (1 * (s IDinit_error_limit_in) + -256 <= 0 /\ -1 * (s IDinit_error_limit_in) + 49 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0)%Z
    | 22%positive => (-1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 49 <= 0 /\ 1 * (s IDinit_error_limit_in) + -256 <= 0)%Z
    | 23%positive => (1 * (s IDinit_error_limit_in) + -256 <= 0 /\ -1 * (s IDinit_error_limit_in) + 49 <= 0 /\ -1 * (s IDinit_error_limit_z) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 16 <= 0 /\ 1 * (s IDinit_error_limit_in) + -47 <= 0)%Z
    | 25%positive => (1 * (s IDinit_error_limit_in) + -47 <= 0 /\ -1 * (s IDinit_error_limit_in) + 16 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0)%Z
    | 26%positive => (-1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 16 <= 0 /\ 1 * (s IDinit_error_limit_in) + -47 <= 0)%Z
    | 27%positive => (-1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 17 <= 0 /\ 1 * (s IDinit_error_limit_in) + -48 <= 0)%Z
    | 28%positive => (1 * (s IDinit_error_limit_in) + -48 <= 0 /\ -1 * (s IDinit_error_limit_in) + 17 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0)%Z
    | 29%positive => (-1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 17 <= 0 /\ 1 * (s IDinit_error_limit_in) + -48 <= 0)%Z
    | 30%positive => (1 * (s IDinit_error_limit_in) + -48 <= 0 /\ -1 * (s IDinit_error_limit_in) + 17 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0)%Z
    | 31%positive => (-1 * (s IDinit_error_limit_in) + 17 <= 0 /\ 1 * (s IDinit_error_limit_in) + -48 <= 0 /\ -1 * (s IDinit_error_limit_z) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDinit_error_limit_in) <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_out) <= 0 /\ 1 * (s IDinit_error_limit_in) + -15 <= 0)%Z
    | 33%positive => (1 * (s IDinit_error_limit_in) + -15 <= 0 /\ -1 * (s IDinit_error_limit_out) <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) <= 0)%Z
    | 34%positive => (-1 * (s IDinit_error_limit_in) <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_out) <= 0 /\ 1 * (s IDinit_error_limit_in) + -15 <= 0)%Z
    | 35%positive => (-1 * (s IDinit_error_limit_out) <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 1 <= 0 /\ 1 * (s IDinit_error_limit_in) + -16 <= 0)%Z
    | 36%positive => (1 * (s IDinit_error_limit_in) + -16 <= 0 /\ -1 * (s IDinit_error_limit_in) + 1 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_out) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDinit_error_limit_out) + 1 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_in) + 1 <= 0 /\ 1 * (s IDinit_error_limit_in) + -16 <= 0)%Z
    | 38%positive => (1 * (s IDinit_error_limit_in) + -16 <= 0 /\ -1 * (s IDinit_error_limit_in) + 1 <= 0 /\ -1 * (s IDinit_error_limit_z) <= 0 /\ -1 * (s IDinit_error_limit_out) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDinit_error_limit_out) + 1 <= 0 /\ -1 * (s IDinit_error_limit_in) + 1 <= 0 /\ 1 * (s IDinit_error_limit_in) + -16 <= 0 /\ -1 * (s IDinit_error_limit_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition init_error_limit_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((256 # 1))%Q
    | 2%positive => ((256 # 1) + (s IDinit_error_limit_z))%Q
    | 3%positive => ((256 # 1) + (s IDinit_error_limit_z))%Q
    | 4%positive => ((256 # 1) - (s IDinit_error_limit_in)
                     + (s IDinit_error_limit_z))%Q
    | 5%positive => ((256 # 1) - (s IDinit_error_limit_in)
                     + (s IDinit_error_limit_z))%Q
    | 6%positive => ((256 # 1) - (s IDinit_error_limit_in)
                     + (s IDinit_error_limit_z))%Q
    | 7%positive => ((256 # 1) - (s IDinit_error_limit_in)
                     + (s IDinit_error_limit_z))%Q
    | 8%positive => (-(0 # 1) + (s IDinit_error_limit_z)
                     + max0(256 - (s IDinit_error_limit_in)))%Q
    | 9%positive => (-(0 # 1) + (s IDinit_error_limit_z)
                     + max0(256 - (s IDinit_error_limit_in)))%Q
    | 10%positive => (-(0 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 11%positive => (-(0 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 12%positive => (-(0 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 13%positive => (-(0 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 14%positive => ((s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 15%positive => ((s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 16%positive => ((s IDinit_error_limit_z))%Q
    | 17%positive => ((s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 18%positive => ((1 # 1) + (s IDinit_error_limit_z)
                      + max0(255 - (s IDinit_error_limit_in)))%Q
    | 19%positive => ((1 # 1) + (s IDinit_error_limit_z)
                      + max0(255 - (s IDinit_error_limit_in)))%Q
    | 20%positive => ((1 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 21%positive => ((1 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 22%positive => ((1 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 23%positive => ((s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 24%positive => (-(0 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 25%positive => ((1 # 1) + (s IDinit_error_limit_z)
                      + max0(255 - (s IDinit_error_limit_in)))%Q
    | 26%positive => ((1 # 1) + (s IDinit_error_limit_z)
                      + max0(255 - (s IDinit_error_limit_in)))%Q
    | 27%positive => ((1 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 28%positive => ((1 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 29%positive => ((1 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 30%positive => ((1 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 31%positive => (-(0 # 1) + (s IDinit_error_limit_z)
                      + max0(256 - (s IDinit_error_limit_in)))%Q
    | 32%positive => ((256 # 1) - (s IDinit_error_limit_in)
                      + (s IDinit_error_limit_z))%Q
    | 33%positive => ((256 # 1) - (s IDinit_error_limit_in)
                      + (s IDinit_error_limit_z))%Q
    | 34%positive => ((256 # 1) - (s IDinit_error_limit_in)
                      + (s IDinit_error_limit_z))%Q
    | 35%positive => ((257 # 1) - (s IDinit_error_limit_in)
                      + (s IDinit_error_limit_z))%Q
    | 36%positive => ((257 # 1) - (s IDinit_error_limit_in)
                      + (s IDinit_error_limit_z))%Q
    | 37%positive => ((257 # 1) - (s IDinit_error_limit_in)
                      + (s IDinit_error_limit_z))%Q
    | 38%positive => ((257 # 1) - (s IDinit_error_limit_in)
                      + (s IDinit_error_limit_z))%Q
    | 39%positive => ((256 # 1) - (s IDinit_error_limit_in)
                      + (s IDinit_error_limit_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition init_error_limit_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-5.40211e-09 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (256
                                                                    - (s IDinit_error_limit_in)) (0))) (F_max0_ge_0 (256
                                                                    - (s IDinit_error_limit_in)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (256
                                                             - (s IDinit_error_limit_in)) (255
                                                                    - (s IDinit_error_limit_in)));
                      (*-1 0*) F_max0_ge_0 (255 - (s IDinit_error_limit_in))]
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_pre_decrement (256
                                                     - (s IDinit_error_limit_in)) (1)]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_max0_pre_decrement (256
                                                     - (s IDinit_error_limit_in)) (1)]
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
    | _ => []
  end.


Theorem init_error_limit_ai_correct:
  forall s p' s', steps (g_start init_error_limit) s (g_edges init_error_limit) p' s' -> init_error_limit_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem init_error_limit_pot_correct:
  forall s p' s',
    steps (g_start init_error_limit) s (g_edges init_error_limit) p' s' ->
    (init_error_limit_pot (g_start init_error_limit) s >= init_error_limit_pot p' s')%Q.
Proof.
  check_lp init_error_limit_ai_correct init_error_limit_hints.
Qed.

