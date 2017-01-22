Require Import pasta.Pasta.

Notation IDjinit_2pass_quantizer_z := 1%positive.
Notation IDjinit_2pass_quantizer_desired := 2%positive.
Notation IDjinit_2pass_quantizer_i := 3%positive.
Notation IDjinit_2pass_quantizer_cinfo := 4%positive.
Definition jinit_2pass_quantizer : graph := {|
  g_start := 1%positive;
  g_end := 34%positive;
  g_edges := (1%positive,(AAssign IDjinit_2pass_quantizer_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,AWeaken,3%positive)::(3%positive,ANone,4%positive)::
             (3%positive,ANone,5%positive)::(4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDjinit_2pass_quantizer_i
             (Some (ENum (0)))),6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDjinit_2pass_quantizer_i) s) <
             (eval (ENum (32)) s))%Z)),35%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDjinit_2pass_quantizer_i) s) >=
             (eval (ENum (32)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,13%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,AWeaken,26%positive)::
             (13%positive,(AAssign IDjinit_2pass_quantizer_desired None),
             14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDjinit_2pass_quantizer_desired) s) <
             (eval (ENum (8)) s))%Z)),17%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDjinit_2pass_quantizer_desired) s) >=
             (eval (ENum (8)) s))%Z)),16%positive)::
             (16%positive,AWeaken,20%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDjinit_2pass_quantizer_desired) s) >
             (eval (ENum (256)) s))%Z)),22%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDjinit_2pass_quantizer_desired) s) <=
             (eval (ENum (256)) s))%Z)),21%positive)::
             (21%positive,AWeaken,24%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,28%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,30%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,32%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,34%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDjinit_2pass_quantizer_i
             (Some (EAdd (EVar IDjinit_2pass_quantizer_i) (ENum (1))))),
             38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDjinit_2pass_quantizer_z
             (Some (EAdd (ENum (1)) (EVar IDjinit_2pass_quantizer_z)))),
             41%positive)::(41%positive,AWeaken,8%positive)::nil
|}.

Definition jinit_2pass_quantizer_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_z) <= 0)%Z
    | 4%positive => (1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0)%Z
    | 5%positive => (-1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_z) <= 0)%Z
    | 6%positive => (1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) <= 0)%Z
    | 7%positive => (-1 * (s IDjinit_2pass_quantizer_i) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_z) <= 0)%Z
    | 8%positive => (-1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0)%Z
    | 9%positive => (1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0)%Z
    | 10%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0)%Z
    | 11%positive => (1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0)%Z
    | 12%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0)%Z
    | 13%positive => (1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0)%Z
    | 14%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0)%Z
    | 15%positive => (1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0)%Z
    | 16%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_desired) + 8 <= 0)%Z
    | 17%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ 1 * (s IDjinit_2pass_quantizer_desired) + -7 <= 0)%Z
    | 18%positive => (1 * (s IDjinit_2pass_quantizer_desired) + -7 <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0)%Z
    | 19%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ 1 * (s IDjinit_2pass_quantizer_desired) + -7 <= 0)%Z
    | 20%positive => (1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0)%Z
    | 21%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ 1 * (s IDjinit_2pass_quantizer_desired) + -256 <= 0)%Z
    | 22%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_desired) + 257 <= 0)%Z
    | 23%positive => (-1 * (s IDjinit_2pass_quantizer_desired) + 257 <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0)%Z
    | 24%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0)%Z
    | 25%positive => (1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0)%Z
    | 26%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0)%Z
    | 27%positive => (1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0)%Z
    | 28%positive => (1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0)%Z
    | 29%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0)%Z
    | 30%positive => (1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0)%Z
    | 31%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0)%Z
    | 32%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0)%Z
    | 33%positive => (1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0)%Z
    | 34%positive => (-1 * (s IDjinit_2pass_quantizer_i) + 32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0)%Z
    | 35%positive => (-1 * (s IDjinit_2pass_quantizer_i) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -31 <= 0)%Z
    | 36%positive => (1 * (s IDjinit_2pass_quantizer_i) + -31 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) <= 0)%Z
    | 37%positive => (-1 * (s IDjinit_2pass_quantizer_i) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -31 <= 0)%Z
    | 38%positive => (-1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 1 <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0)%Z
    | 39%positive => (1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 1 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) <= 0)%Z
    | 40%positive => (-1 * (s IDjinit_2pass_quantizer_z) <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 1 <= 0 /\ 1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0)%Z
    | 41%positive => (1 * (s IDjinit_2pass_quantizer_i) + -32 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_i) + 1 <= 0 /\ -1 * (s IDjinit_2pass_quantizer_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jinit_2pass_quantizer_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((32 # 1))%Q
    | 2%positive => ((32 # 1) + (s IDjinit_2pass_quantizer_z))%Q
    | 3%positive => ((32 # 1) + (s IDjinit_2pass_quantizer_z))%Q
    | 4%positive => ((32 # 1) + (s IDjinit_2pass_quantizer_z))%Q
    | 5%positive => ((32 # 1) + (s IDjinit_2pass_quantizer_z))%Q
    | 6%positive => ((s IDjinit_2pass_quantizer_z)
                     + max0(32 - (s IDjinit_2pass_quantizer_i)))%Q
    | 7%positive => ((s IDjinit_2pass_quantizer_z)
                     + max0(32 - (s IDjinit_2pass_quantizer_i)))%Q
    | 8%positive => ((s IDjinit_2pass_quantizer_z)
                     + max0(32 - (s IDjinit_2pass_quantizer_i)))%Q
    | 9%positive => ((s IDjinit_2pass_quantizer_z)
                     + max0(32 - (s IDjinit_2pass_quantizer_i)))%Q
    | 10%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 11%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 12%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 13%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 14%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 15%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 16%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 17%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 18%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 19%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 20%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 21%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 22%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 23%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 24%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 25%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 26%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 27%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 28%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 29%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 30%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 31%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 32%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 33%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 34%positive => ((s IDjinit_2pass_quantizer_z))%Q
    | 35%positive => ((s IDjinit_2pass_quantizer_z)
                      + max0(32 - (s IDjinit_2pass_quantizer_i)))%Q
    | 36%positive => ((1 # 1) + (s IDjinit_2pass_quantizer_z)
                      + max0(31 - (s IDjinit_2pass_quantizer_i)))%Q
    | 37%positive => ((1 # 1) + (s IDjinit_2pass_quantizer_z)
                      + max0(31 - (s IDjinit_2pass_quantizer_i)))%Q
    | 38%positive => ((1 # 1) + (s IDjinit_2pass_quantizer_z)
                      + max0(32 - (s IDjinit_2pass_quantizer_i)))%Q
    | 39%positive => ((1 # 1) + (s IDjinit_2pass_quantizer_z)
                      + max0(32 - (s IDjinit_2pass_quantizer_i)))%Q
    | 40%positive => ((1 # 1) + (s IDjinit_2pass_quantizer_z)
                      + max0(32 - (s IDjinit_2pass_quantizer_i)))%Q
    | 41%positive => ((s IDjinit_2pass_quantizer_z)
                      + max0(32 - (s IDjinit_2pass_quantizer_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition jinit_2pass_quantizer_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_ge_0 (32 - (s IDjinit_2pass_quantizer_i))]
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
    | 35%positive => [(*-1 0*) F_max0_pre_decrement (32
                                                     - (s IDjinit_2pass_quantizer_i)) (1)]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | _ => []
  end.


Theorem jinit_2pass_quantizer_ai_correct:
  forall s p' s', steps (g_start jinit_2pass_quantizer) s (g_edges jinit_2pass_quantizer) p' s' -> jinit_2pass_quantizer_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jinit_2pass_quantizer_pot_correct:
  forall s p' s',
    steps (g_start jinit_2pass_quantizer) s (g_edges jinit_2pass_quantizer) p' s' ->
    (jinit_2pass_quantizer_pot (g_start jinit_2pass_quantizer) s >= jinit_2pass_quantizer_pot p' s')%Q.
Proof.
  check_lp jinit_2pass_quantizer_ai_correct jinit_2pass_quantizer_hints.
Qed.

