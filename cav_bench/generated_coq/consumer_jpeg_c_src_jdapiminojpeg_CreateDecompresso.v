Require Import pasta.Pasta.

Notation IDjpeg_CreateDecompress_z := 1%positive.
Notation IDjpeg_CreateDecompress__tmp := 2%positive.
Notation IDjpeg_CreateDecompress__tmp1 := 3%positive.
Notation IDjpeg_CreateDecompress_i := 4%positive.
Notation IDjpeg_CreateDecompress_cinfo := 5%positive.
Notation IDjpeg_CreateDecompress_structsize := 6%positive.
Notation IDjpeg_CreateDecompress_version := 7%positive.
Definition jpeg_CreateDecompress : graph := {|
  g_start := 1%positive;
  g_end := 24%positive;
  g_edges := (1%positive,(AAssign IDjpeg_CreateDecompress_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDjpeg_CreateDecompress__tmp
             (Some (EVar IDjpeg_CreateDecompress_version))),3%positive)::
             (3%positive,(AAssign IDjpeg_CreateDecompress__tmp1
             (Some (EVar IDjpeg_CreateDecompress_structsize))),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_CreateDecompress__tmp) s) <>
             (eval (ENum (61)) s))%Z)),7%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_CreateDecompress__tmp) s) =
             (eval (ENum (61)) s))%Z)),6%positive)::
             (6%positive,AWeaken,10%positive)::
             (7%positive,AWeaken,8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_CreateDecompress__tmp1) s) <>
             (eval (ENum (616)) s))%Z)),12%positive)::
             (10%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_CreateDecompress__tmp1) s) =
             (eval (ENum (616)) s))%Z)),11%positive)::
             (11%positive,AWeaken,14%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDjpeg_CreateDecompress_i
             (Some (ENum (0)))),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_CreateDecompress_i) s) <
             (eval (ENum (4)) s))%Z)),32%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_CreateDecompress_i) s) >=
             (eval (ENum (4)) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDjpeg_CreateDecompress_i
             (Some (ENum (0)))),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_CreateDecompress_i) s) <
             (eval (ENum (4)) s))%Z)),25%positive)::
             (22%positive,(AGuard
             (fun s => ((eval (EVar IDjpeg_CreateDecompress_i) s) >=
             (eval (ENum (4)) s))%Z)),23%positive)::
             (23%positive,AWeaken,24%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDjpeg_CreateDecompress_i
             (Some (EAdd (EVar IDjpeg_CreateDecompress_i) (ENum (1))))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDjpeg_CreateDecompress_z
             (Some (EAdd (ENum (1)) (EVar IDjpeg_CreateDecompress_z)))),
             31%positive)::(31%positive,AWeaken,22%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDjpeg_CreateDecompress_i
             (Some (EAdd (EVar IDjpeg_CreateDecompress_i) (ENum (1))))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDjpeg_CreateDecompress_z
             (Some (EAdd (ENum (1)) (EVar IDjpeg_CreateDecompress_z)))),
             38%positive)::(38%positive,AWeaken,17%positive)::nil
|}.

Definition jpeg_CreateDecompress_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 3%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 4%positive => (1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 5%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 6%positive => (1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress__tmp) + -61 <= 0 /\ -1 * (s IDjpeg_CreateDecompress__tmp) + 61 <= 0)%Z
    | 7%positive => (1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 8%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 9%positive => (1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 10%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 11%positive => (1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress__tmp1) + -616 <= 0 /\ -1 * (s IDjpeg_CreateDecompress__tmp1) + 616 <= 0)%Z
    | 12%positive => (1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 13%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 14%positive => (1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 15%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) <= 0)%Z
    | 16%positive => (-1 * (s IDjpeg_CreateDecompress_i) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 17%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0)%Z
    | 18%positive => (1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) + 4 <= 0)%Z
    | 19%positive => (-1 * (s IDjpeg_CreateDecompress_i) + 4 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0)%Z
    | 20%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) <= 0)%Z
    | 21%positive => (-1 * (s IDjpeg_CreateDecompress_i) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 22%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0)%Z
    | 23%positive => (1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) + 4 <= 0)%Z
    | 24%positive => (-1 * (s IDjpeg_CreateDecompress_i) + 4 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0)%Z
    | 25%positive => (-1 * (s IDjpeg_CreateDecompress_i) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) + -3 <= 0)%Z
    | 26%positive => (1 * (s IDjpeg_CreateDecompress_i) + -3 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) <= 0)%Z
    | 27%positive => (-1 * (s IDjpeg_CreateDecompress_i) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) + -3 <= 0)%Z
    | 28%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) + 1 <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0)%Z
    | 29%positive => (1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) + 1 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 30%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) + 1 <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0)%Z
    | 31%positive => (1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) + 1 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDjpeg_CreateDecompress_i) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) + -3 <= 0)%Z
    | 33%positive => (1 * (s IDjpeg_CreateDecompress_i) + -3 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) <= 0)%Z
    | 34%positive => (-1 * (s IDjpeg_CreateDecompress_i) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) + -3 <= 0)%Z
    | 35%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) + 1 <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0)%Z
    | 36%positive => (1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) + 1 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) <= 0)%Z
    | 37%positive => (-1 * (s IDjpeg_CreateDecompress_z) <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) + 1 <= 0 /\ 1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0)%Z
    | 38%positive => (1 * (s IDjpeg_CreateDecompress_i) + -4 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_i) + 1 <= 0 /\ -1 * (s IDjpeg_CreateDecompress_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition jpeg_CreateDecompress_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1))%Q
    | 2%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 3%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 4%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 5%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 6%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 7%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 8%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 9%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 10%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 11%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 12%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 13%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 14%positive => ((8 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 15%positive => ((4 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 16%positive => ((4 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 17%positive => ((4 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 18%positive => ((4 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 19%positive => ((4 # 1) + (s IDjpeg_CreateDecompress_z))%Q
    | 20%positive => ((s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 21%positive => ((s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 22%positive => ((s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 23%positive => ((s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 24%positive => ((s IDjpeg_CreateDecompress_z))%Q
    | 25%positive => ((s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 26%positive => ((1 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(3 - (s IDjpeg_CreateDecompress_i)))%Q
    | 27%positive => ((1 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(3 - (s IDjpeg_CreateDecompress_i)))%Q
    | 28%positive => ((1 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 29%positive => ((1 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 30%positive => ((1 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 31%positive => ((s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 32%positive => ((4 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 33%positive => ((5 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(3 - (s IDjpeg_CreateDecompress_i)))%Q
    | 34%positive => ((5 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(3 - (s IDjpeg_CreateDecompress_i)))%Q
    | 35%positive => ((5 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 36%positive => ((5 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 37%positive => ((5 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | 38%positive => ((4 # 1) + (s IDjpeg_CreateDecompress_z)
                      + max0(4 - (s IDjpeg_CreateDecompress_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition jpeg_CreateDecompress_hints (p : node) (s : state) := 
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
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDjpeg_CreateDecompress_i)) (3
                                                                    - (s IDjpeg_CreateDecompress_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDjpeg_CreateDecompress_i))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                                             - (s IDjpeg_CreateDecompress_i)) (3
                                                                    - (s IDjpeg_CreateDecompress_i)));
                      (*-1 0*) F_max0_ge_0 (3 - (s IDjpeg_CreateDecompress_i))]
    | 24%positive => []
    | 25%positive => [(*-1 0*) F_max0_pre_decrement (4
                                                     - (s IDjpeg_CreateDecompress_i)) (1)]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*0 1*) F_max0_pre_decrement (4
                                                    - (s IDjpeg_CreateDecompress_i)) (1)]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | _ => []
  end.


Theorem jpeg_CreateDecompress_ai_correct:
  forall s p' s', steps (g_start jpeg_CreateDecompress) s (g_edges jpeg_CreateDecompress) p' s' -> jpeg_CreateDecompress_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem jpeg_CreateDecompress_pot_correct:
  forall s p' s',
    steps (g_start jpeg_CreateDecompress) s (g_edges jpeg_CreateDecompress) p' s' ->
    (jpeg_CreateDecompress_pot (g_start jpeg_CreateDecompress) s >= jpeg_CreateDecompress_pot p' s')%Q.
Proof.
  check_lp jpeg_CreateDecompress_ai_correct jpeg_CreateDecompress_hints.
Qed.

