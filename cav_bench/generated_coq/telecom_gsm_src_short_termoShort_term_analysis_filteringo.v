Require Import pasta.Pasta.

Notation IDShort_term_analysis_filtering_z := 1%positive.
Notation IDShort_term_analysis_filtering__tmp := 2%positive.
Notation IDShort_term_analysis_filtering_di := 3%positive.
Notation IDShort_term_analysis_filtering_i := 4%positive.
Notation IDShort_term_analysis_filtering_ltmp := 5%positive.
Notation IDShort_term_analysis_filtering_rpi := 6%positive.
Notation IDShort_term_analysis_filtering_sav := 7%positive.
Notation IDShort_term_analysis_filtering_ui := 8%positive.
Notation IDShort_term_analysis_filtering_zzz := 9%positive.
Notation IDShort_term_analysis_filtering_S := 10%positive.
Notation IDShort_term_analysis_filtering_k_n := 11%positive.
Notation IDShort_term_analysis_filtering_rp := 12%positive.
Notation IDShort_term_analysis_filtering_s := 13%positive.
Definition Short_term_analysis_filtering : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDShort_term_analysis_filtering_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDShort_term_analysis_filtering__tmp
             (Some (EVar IDShort_term_analysis_filtering_k_n))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDShort_term_analysis_filtering__tmp
             (Some (EAdd (EVar IDShort_term_analysis_filtering__tmp)
             (ENum (-1))))),5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDShort_term_analysis_filtering__tmp)
             s) <> (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDShort_term_analysis_filtering__tmp)
             s) = (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDShort_term_analysis_filtering_sav None),
             11%positive)::
             (11%positive,(AAssign IDShort_term_analysis_filtering_di None),
             12%positive)::
             (12%positive,(AAssign IDShort_term_analysis_filtering_i
             (Some (ENum (0)))),13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDShort_term_analysis_filtering_i) s) <
             (eval (ENum (8)) s))%Z)),21%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDShort_term_analysis_filtering_i) s) >=
             (eval (ENum (8)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDShort_term_analysis_filtering_z
             (Some (EAdd (ENum (1))
             (EVar IDShort_term_analysis_filtering_z)))),4%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDShort_term_analysis_filtering_ui None),
             23%positive)::
             (23%positive,(AAssign IDShort_term_analysis_filtering_rpi None),
             24%positive)::
             (24%positive,(AAssign IDShort_term_analysis_filtering_zzz None),
             25%positive)::
             (25%positive,(AAssign IDShort_term_analysis_filtering_ltmp
             (Some (EAdd (EVar IDShort_term_analysis_filtering_ui)
             (EVar IDShort_term_analysis_filtering_zzz)))),26%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,29%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,30%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDShort_term_analysis_filtering_sav None),
             31%positive)::
             (31%positive,(AAssign IDShort_term_analysis_filtering_zzz None),
             32%positive)::
             (32%positive,(AAssign IDShort_term_analysis_filtering_ltmp
             (Some (EAdd (EVar IDShort_term_analysis_filtering_di)
             (EVar IDShort_term_analysis_filtering_zzz)))),33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,36%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,ANone,37%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDShort_term_analysis_filtering_di None),
             38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDShort_term_analysis_filtering_i
             (Some (EAdd (EVar IDShort_term_analysis_filtering_i)
             (ENum (1))))),40%positive)::(40%positive,ANone,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDShort_term_analysis_filtering_z
             (Some (EAdd (ENum (1))
             (EVar IDShort_term_analysis_filtering_z)))),43%positive)::
             (43%positive,AWeaken,15%positive)::nil
|}.

Definition Short_term_analysis_filtering_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0)%Z
    | 3%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_z) <= 0)%Z
    | 4%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0)%Z
    | 5%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0)%Z
    | 6%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0)%Z
    | 7%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering__tmp) <= 0 /\ -1 * (s IDShort_term_analysis_filtering__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDShort_term_analysis_filtering__tmp) <= 0 /\ 1 * (s IDShort_term_analysis_filtering__tmp) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0)%Z
    | 9%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0)%Z
    | 10%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0)%Z
    | 11%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0)%Z
    | 12%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0)%Z
    | 13%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) <= 0)%Z
    | 14%positive => (-1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0)%Z
    | 15%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -8 <= 0)%Z
    | 16%positive => (1 * (s IDShort_term_analysis_filtering_i) + -8 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) + 8 <= 0)%Z
    | 17%positive => (-1 * (s IDShort_term_analysis_filtering_i) + 8 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -8 <= 0)%Z
    | 18%positive => (1 * (s IDShort_term_analysis_filtering_i) + -8 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) + 8 <= 0)%Z
    | 19%positive => (-1 * (s IDShort_term_analysis_filtering_i) + 8 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -8 <= 0)%Z
    | 20%positive => (1 * (s IDShort_term_analysis_filtering_i) + -8 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) + 8 <= 0)%Z
    | 21%positive => (-1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0)%Z
    | 22%positive => (1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) <= 0)%Z
    | 23%positive => (-1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0)%Z
    | 24%positive => (1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) <= 0)%Z
    | 25%positive => (-1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0)%Z
    | 26%positive => (1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) <= 0)%Z
    | 27%positive => (-1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0)%Z
    | 28%positive => (1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) <= 0)%Z
    | 29%positive => (1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) <= 0)%Z
    | 30%positive => (-1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0)%Z
    | 31%positive => (1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) <= 0)%Z
    | 32%positive => (-1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0)%Z
    | 33%positive => (1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) <= 0)%Z
    | 34%positive => (-1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0)%Z
    | 35%positive => (1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) <= 0)%Z
    | 36%positive => (1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) <= 0)%Z
    | 37%positive => (-1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0)%Z
    | 38%positive => (1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) <= 0)%Z
    | 39%positive => (-1 * (s IDShort_term_analysis_filtering_i) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -7 <= 0)%Z
    | 40%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) + 1 <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -8 <= 0)%Z
    | 41%positive => (1 * (s IDShort_term_analysis_filtering_i) + -8 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) + 1 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) <= 0)%Z
    | 42%positive => (-1 * (s IDShort_term_analysis_filtering_z) <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) + 1 <= 0 /\ 1 * (s IDShort_term_analysis_filtering_i) + -8 <= 0)%Z
    | 43%positive => (1 * (s IDShort_term_analysis_filtering_i) + -8 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_i) + 1 <= 0 /\ -1 * (s IDShort_term_analysis_filtering_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition Short_term_analysis_filtering_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((9 # 1) * (s IDShort_term_analysis_filtering_k_n))%Q
    | 2%positive => ((9 # 1) * (s IDShort_term_analysis_filtering_k_n)
                     + (s IDShort_term_analysis_filtering_z))%Q
    | 3%positive => ((9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                     + (s IDShort_term_analysis_filtering_z))%Q
    | 4%positive => ((9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                     + (s IDShort_term_analysis_filtering_z))%Q
    | 5%positive => ((9 # 1)
                     + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                     + (s IDShort_term_analysis_filtering_z))%Q
    | 6%positive => ((9 # 1)
                     + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                     + (s IDShort_term_analysis_filtering_z))%Q
    | 7%positive => ((9 # 1)
                     + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                     + (s IDShort_term_analysis_filtering_z))%Q
    | 8%positive => ((s IDShort_term_analysis_filtering_z))%Q
    | 9%positive => ((9 # 1)
                     + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                     + (s IDShort_term_analysis_filtering_z))%Q
    | 10%positive => ((9 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z))%Q
    | 11%positive => ((9 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z))%Q
    | 12%positive => ((9 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z))%Q
    | 13%positive => ((1 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(8 - (s IDShort_term_analysis_filtering_i)))%Q
    | 14%positive => ((1 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(8 - (s IDShort_term_analysis_filtering_i)))%Q
    | 15%positive => ((1 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(8 - (s IDShort_term_analysis_filtering_i)))%Q
    | 16%positive => ((1 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(8 - (s IDShort_term_analysis_filtering_i)))%Q
    | 17%positive => ((1 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z))%Q
    | 18%positive => ((1 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z))%Q
    | 19%positive => ((1 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z))%Q
    | 20%positive => ((1 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z))%Q
    | 21%positive => ((1 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(8 - (s IDShort_term_analysis_filtering_i)))%Q
    | 22%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 23%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 24%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 25%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 26%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 27%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 28%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 29%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 30%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 31%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 32%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 33%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 34%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 35%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 36%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 37%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 38%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 39%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(7 - (s IDShort_term_analysis_filtering_i)))%Q
    | 40%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(8 - (s IDShort_term_analysis_filtering_i)))%Q
    | 41%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(8 - (s IDShort_term_analysis_filtering_i)))%Q
    | 42%positive => ((2 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(8 - (s IDShort_term_analysis_filtering_i)))%Q
    | 43%positive => ((1 # 1)
                      + (9 # 1) * (s IDShort_term_analysis_filtering__tmp)
                      + (s IDShort_term_analysis_filtering_z)
                      + max0(8 - (s IDShort_term_analysis_filtering_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Short_term_analysis_filtering_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-9 0*) F_one;
                     (*-9 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDShort_term_analysis_filtering__tmp))) (F_check_ge (0) (0));
                     (*-9 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDShort_term_analysis_filtering__tmp)) (0))) (F_max0_ge_0 ((s IDShort_term_analysis_filtering__tmp)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*0 1*) F_max0_monotonic (F_check_ge (8
                                                            - (s IDShort_term_analysis_filtering_i)) (7
                                                                    - (s IDShort_term_analysis_filtering_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                 - (s IDShort_term_analysis_filtering_i))) (F_check_ge (0) (0))]
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => [(*0 1*) F_max0_pre_decrement (8
                                                    - (s IDShort_term_analysis_filtering_i)) (1)]
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
    | 43%positive => []
    | _ => []
  end.


Theorem Short_term_analysis_filtering_ai_correct:
  forall s p' s', steps (g_start Short_term_analysis_filtering) s (g_edges Short_term_analysis_filtering) p' s' -> Short_term_analysis_filtering_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Short_term_analysis_filtering_pot_correct:
  forall s p' s',
    steps (g_start Short_term_analysis_filtering) s (g_edges Short_term_analysis_filtering) p' s' ->
    (Short_term_analysis_filtering_pot (g_start Short_term_analysis_filtering) s >= Short_term_analysis_filtering_pot p' s')%Q.
Proof.
  check_lp Short_term_analysis_filtering_ai_correct Short_term_analysis_filtering_hints.
Qed.

