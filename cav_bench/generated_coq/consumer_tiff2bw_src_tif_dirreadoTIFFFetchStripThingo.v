Require Import pasta.Pasta.

Notation IDTIFFFetchStripThing_z := 1%positive.
Notation IDTIFFFetchStripThing__tmp := 2%positive.
Notation IDTIFFFetchStripThing__tmp1 := 3%positive.
Notation IDTIFFFetchStripThing_status := 4%positive.
Notation IDTIFFFetchStripThing_dir := 5%positive.
Notation IDTIFFFetchStripThing_lpp := 6%positive.
Notation IDTIFFFetchStripThing_nstrips := 7%positive.
Notation IDTIFFFetchStripThing_tif := 8%positive.
Definition TIFFFetchStripThing : graph := {|
  g_start := 1%positive;
  g_end := 42%positive;
  g_edges := (1%positive,(AAssign IDTIFFFetchStripThing_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDTIFFFetchStripThing__tmp1
             (Some (EVar IDTIFFFetchStripThing_nstrips))),3%positive)::
             (3%positive,AWeaken,4%positive)::(4%positive,ANone,8%positive)::
             (4%positive,ANone,5%positive)::
             (5%positive,(AAssign IDTIFFFetchStripThing__tmp
             (Some (ENum (0)))),6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,42%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,11%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,14%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,39%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,17%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDTIFFFetchStripThing_status None),
             16%positive)::(16%positive,ANone,29%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,36%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDTIFFFetchStripThing_status None),
             20%positive)::(20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (21%positive,ANone,28%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDTIFFFetchStripThing__tmp1
             (Some (EAdd (EVar IDTIFFFetchStripThing__tmp1) (ENum (-1))))),
             24%positive)::(24%positive,AWeaken,25%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFFetchStripThing__tmp1) s) >
             (eval (ENum (0)) s))%Z)),32%positive)::
             (25%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFFetchStripThing__tmp1) s) <=
             (eval (ENum (0)) s))%Z)),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDTIFFFetchStripThing__tmp
             (Some (EVar IDTIFFFetchStripThing_status))),30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,AWeaken,42%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDTIFFFetchStripThing_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFFetchStripThing_z)))),
             23%positive)::
             (36%positive,(AAssign IDTIFFFetchStripThing__tmp
             (Some (ENum (0)))),37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,AWeaken,42%positive)::
             (39%positive,(AAssign IDTIFFFetchStripThing__tmp
             (Some (ENum (0)))),40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,AWeaken,42%positive)::nil
|}.

Definition TIFFFetchStripThing_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 3%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 4%positive => (1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 5%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 6%positive => (1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing__tmp) <= 0 /\ -1 * (s IDTIFFFetchStripThing__tmp) <= 0)%Z
    | 7%positive => (-1 * (s IDTIFFFetchStripThing__tmp) <= 0 /\ 1 * (s IDTIFFFetchStripThing__tmp) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 8%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 9%positive => (1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 10%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 11%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 12%positive => (1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 13%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 14%positive => (1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 15%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 16%positive => (1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 17%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 18%positive => (1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 19%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 20%positive => (1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 21%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 22%positive => (1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 23%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 24%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 25%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 26%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing__tmp1) <= 0)%Z
    | 27%positive => (1 * (s IDTIFFFetchStripThing__tmp1) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 28%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 29%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 30%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 31%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 32%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing__tmp1) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDTIFFFetchStripThing__tmp1) + 1 <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 34%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing__tmp1) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDTIFFFetchStripThing__tmp1) + 1 <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 36%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 37%positive => (1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing__tmp) <= 0 /\ -1 * (s IDTIFFFetchStripThing__tmp) <= 0)%Z
    | 38%positive => (-1 * (s IDTIFFFetchStripThing__tmp) <= 0 /\ 1 * (s IDTIFFFetchStripThing__tmp) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 39%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 40%positive => (1 * (s IDTIFFFetchStripThing_z) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing__tmp) <= 0 /\ -1 * (s IDTIFFFetchStripThing__tmp) <= 0)%Z
    | 41%positive => (-1 * (s IDTIFFFetchStripThing__tmp) <= 0 /\ 1 * (s IDTIFFFetchStripThing__tmp) <= 0 /\ -1 * (s IDTIFFFetchStripThing_z) <= 0 /\ 1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | 42%positive => (-1 * (s IDTIFFFetchStripThing_z) <= 0)%Z
    | _ => False
  end.

Definition TIFFFetchStripThing_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDTIFFFetchStripThing_nstrips)))%Q
    | 2%positive => ((s IDTIFFFetchStripThing_z)
                     + max0((s IDTIFFFetchStripThing_nstrips)))%Q
    | 3%positive => ((s IDTIFFFetchStripThing_z)
                     + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 4%positive => ((s IDTIFFFetchStripThing_z)
                     + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 5%positive => ((s IDTIFFFetchStripThing_z)
                     + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 6%positive => ((s IDTIFFFetchStripThing_z)
                     + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 7%positive => ((s IDTIFFFetchStripThing_z)
                     + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 8%positive => ((s IDTIFFFetchStripThing_z)
                     + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 9%positive => ((s IDTIFFFetchStripThing_z)
                     + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 10%positive => ((s IDTIFFFetchStripThing_z)
                      + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 11%positive => ((s IDTIFFFetchStripThing_z)
                      + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 12%positive => ((s IDTIFFFetchStripThing_z)
                      + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 13%positive => ((s IDTIFFFetchStripThing_z)
                      + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 14%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 15%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 16%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 17%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 18%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 19%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 20%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 21%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 22%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 23%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 24%positive => ((s IDTIFFFetchStripThing_z)
                      + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 25%positive => ((s IDTIFFFetchStripThing_z)
                      + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 26%positive => ((s IDTIFFFetchStripThing_z)
                      + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 27%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 28%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 29%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 30%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 31%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 32%positive => ((s IDTIFFFetchStripThing_z)
                      + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 33%positive => ((1 # 1) + (s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 34%positive => ((1 # 1) + (s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 35%positive => ((1 # 1) + (s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 36%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 37%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 38%positive => ((s IDTIFFFetchStripThing_z)
                      + max0(-1 + (s IDTIFFFetchStripThing__tmp1)))%Q
    | 39%positive => ((s IDTIFFFetchStripThing_z)
                      + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 40%positive => ((s IDTIFFFetchStripThing_z)
                      + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 41%positive => ((s IDTIFFFetchStripThing_z)
                      + max0((s IDTIFFFetchStripThing__tmp1)))%Q
    | 42%positive => ((s IDTIFFFetchStripThing_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition TIFFFetchStripThing_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFFetchStripThing__tmp1)) (-1
                                                                    + (s IDTIFFFetchStripThing__tmp1)));
                     (*-1 0*) F_max0_ge_0 (-1
                                           + (s IDTIFFFetchStripThing__tmp1))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFFetchStripThing__tmp1)) (-1
                                                                    + (s IDTIFFFetchStripThing__tmp1)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFFetchStripThing__tmp1)) (-1
                                                                    + (s IDTIFFFetchStripThing__tmp1)))]
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
    | 26%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFFetchStripThing__tmp1)) (-1
                                                                    + (s IDTIFFFetchStripThing__tmp1)))]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*-1 0*) F_max0_ge_0 (-1
                                            + (s IDTIFFFetchStripThing__tmp1))]
    | 32%positive => [(*-1 0*) F_max0_pre_decrement ((s IDTIFFFetchStripThing__tmp1)) (1)]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_max0_ge_0 (-1
                                            + (s IDTIFFFetchStripThing__tmp1))]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDTIFFFetchStripThing__tmp1)) (-1
                                                                    + (s IDTIFFFetchStripThing__tmp1)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDTIFFFetchStripThing__tmp1))]
    | 42%positive => []
    | _ => []
  end.


Theorem TIFFFetchStripThing_ai_correct:
  forall s p' s', steps (g_start TIFFFetchStripThing) s (g_edges TIFFFetchStripThing) p' s' -> TIFFFetchStripThing_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem TIFFFetchStripThing_pot_correct:
  forall s p' s',
    steps (g_start TIFFFetchStripThing) s (g_edges TIFFFetchStripThing) p' s' ->
    (TIFFFetchStripThing_pot (g_start TIFFFetchStripThing) s >= TIFFFetchStripThing_pot p' s')%Q.
Proof.
  check_lp TIFFFetchStripThing_ai_correct TIFFFetchStripThing_hints.
Qed.

