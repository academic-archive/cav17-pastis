Require Import pasta.Pasta.

Notation IDShort_term_synthesis_filtering_z := 1%positive.
Notation IDShort_term_synthesis_filtering__tmp := 2%positive.
Notation IDShort_term_synthesis_filtering_i := 3%positive.
Notation IDShort_term_synthesis_filtering_ltmp := 4%positive.
Notation IDShort_term_synthesis_filtering_sri := 5%positive.
Notation IDShort_term_synthesis_filtering_tmp1 := 6%positive.
Notation IDShort_term_synthesis_filtering_tmp2 := 7%positive.
Notation IDShort_term_synthesis_filtering_S := 8%positive.
Notation IDShort_term_synthesis_filtering_k := 9%positive.
Notation IDShort_term_synthesis_filtering_rrp := 10%positive.
Notation IDShort_term_synthesis_filtering_sr := 11%positive.
Notation IDShort_term_synthesis_filtering_wt := 12%positive.
Definition Short_term_synthesis_filtering : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDShort_term_synthesis_filtering_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDShort_term_synthesis_filtering__tmp
             (Some (EVar IDShort_term_synthesis_filtering_k))),3%positive)::
             (3%positive,ANone,4%positive)::
             (4%positive,(AAssign IDShort_term_synthesis_filtering__tmp
             (Some (EAdd (EVar IDShort_term_synthesis_filtering__tmp)
             (ENum (-1))))),5%positive)::(5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDShort_term_synthesis_filtering__tmp)
             s) <> (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDShort_term_synthesis_filtering__tmp)
             s) = (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AAssign IDShort_term_synthesis_filtering_sri
             None),11%positive)::
             (11%positive,(AAssign IDShort_term_synthesis_filtering_i
             (Some (ENum (8)))),12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDShort_term_synthesis_filtering_i
             (Some (EAdd (EVar IDShort_term_synthesis_filtering_i)
             (ENum (-1))))),14%positive)::(14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDShort_term_synthesis_filtering_i) s) <>
             (eval (ENum (0)) s))%Z)),20%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDShort_term_synthesis_filtering_i) s) =
             (eval (ENum (0)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDShort_term_synthesis_filtering_z
             (Some (EAdd (ENum (1))
             (EVar IDShort_term_synthesis_filtering_z)))),4%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDShort_term_synthesis_filtering_tmp1
             None),22%positive)::
             (22%positive,(AAssign IDShort_term_synthesis_filtering_tmp2
             None),23%positive)::(23%positive,AWeaken,24%positive)::
             (24%positive,ANone,25%positive)::
             (24%positive,ANone,27%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,45%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDShort_term_synthesis_filtering_tmp2
             None),29%positive)::
             (29%positive,(AAssign IDShort_term_synthesis_filtering_ltmp
             (Some (ESub (EVar IDShort_term_synthesis_filtering_sri)
             (EVar IDShort_term_synthesis_filtering_tmp2)))),30%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard (fun s => True)),44%positive)::
             (31%positive,(AGuard (fun s => True)),32%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,(AGuard (fun s => True)),37%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AGuard (fun s => True)),36%positive)::
             (36%positive,AWeaken,39%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDShort_term_synthesis_filtering_sri
             None),40%positive)::(40%positive,AWeaken,41%positive)::
             (41%positive,(AGuard (fun s => True)),43%positive)::
             (41%positive,(AGuard (fun s => True)),42%positive)::
             (42%positive,AWeaken,49%positive)::
             (43%positive,AWeaken,48%positive)::
             (44%positive,AWeaken,46%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,57%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,(AAssign IDShort_term_synthesis_filtering_tmp1
             None),51%positive)::
             (51%positive,(AAssign IDShort_term_synthesis_filtering_ltmp
             None),52%positive)::(52%positive,AWeaken,53%positive)::
             (53%positive,(AGuard (fun s => True)),56%positive)::
             (53%positive,(AGuard (fun s => True)),54%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,ANone,59%positive)::
             (56%positive,AWeaken,58%positive)::
             (57%positive,ANone,58%positive)::
             (58%positive,ANone,59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,(AAssign IDShort_term_synthesis_filtering_z
             (Some (EAdd (ENum (1))
             (EVar IDShort_term_synthesis_filtering_z)))),13%positive)::nil
|}.

Definition Short_term_synthesis_filtering_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 3%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 4%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 5%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 6%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 7%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering__tmp) <= 0 /\ -1 * (s IDShort_term_synthesis_filtering__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDShort_term_synthesis_filtering__tmp) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering__tmp) <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 9%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 10%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 11%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 12%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -8 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_i) + 8 <= 0)%Z
    | 13%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -8 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 14%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 15%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 16%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_i) <= 0)%Z
    | 17%positive => (-1 * (s IDShort_term_synthesis_filtering_i) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 18%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_i) <= 0)%Z
    | 19%positive => (-1 * (s IDShort_term_synthesis_filtering_i) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 20%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 21%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 22%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 23%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 24%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 25%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 26%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 27%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 28%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 29%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 30%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 31%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 32%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 33%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 34%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 35%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 36%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 37%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 38%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 39%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 40%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 41%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 42%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 43%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 44%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 45%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 46%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 47%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 48%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 49%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 50%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 51%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 52%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 53%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 54%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 55%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 56%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 57%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 58%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 59%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | 60%positive => (-1 * (s IDShort_term_synthesis_filtering_z) <= 0 /\ 1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0)%Z
    | 61%positive => (1 * (s IDShort_term_synthesis_filtering_i) + -7 <= 0 /\ -1 * (s IDShort_term_synthesis_filtering_z) <= 0)%Z
    | _ => False
  end.

Definition Short_term_synthesis_filtering_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering_k))%Q
    | 2%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering_k)
                     + (s IDShort_term_synthesis_filtering_z))%Q
    | 3%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                     + (s IDShort_term_synthesis_filtering_z))%Q
    | 4%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                     + (s IDShort_term_synthesis_filtering_z))%Q
    | 5%positive => ((8 # 1)
                     + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                     + (s IDShort_term_synthesis_filtering_z))%Q
    | 6%positive => ((8 # 1)
                     + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                     + (s IDShort_term_synthesis_filtering_z))%Q
    | 7%positive => ((8 # 1)
                     + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                     + (s IDShort_term_synthesis_filtering_z))%Q
    | 8%positive => ((s IDShort_term_synthesis_filtering_z))%Q
    | 9%positive => ((8 # 1)
                     + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                     + (s IDShort_term_synthesis_filtering_z))%Q
    | 10%positive => ((8 # 1)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (s IDShort_term_synthesis_filtering_z))%Q
    | 11%positive => ((8 # 1)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (s IDShort_term_synthesis_filtering_z))%Q
    | 12%positive => (-(8 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 13%positive => (-(8 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 14%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 15%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 16%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 17%positive => ((1 # 1)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (s IDShort_term_synthesis_filtering_z))%Q
    | 18%positive => ((1 # 1)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (s IDShort_term_synthesis_filtering_z))%Q
    | 19%positive => ((1 # 1)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (s IDShort_term_synthesis_filtering_z))%Q
    | 20%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 21%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 22%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 23%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 24%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 25%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 26%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 27%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 28%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 29%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 30%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 31%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 32%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 33%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 34%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 35%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 36%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 37%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 38%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 39%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 40%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 41%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 42%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 43%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 44%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 45%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 46%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 47%positive => ((8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(7
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 48%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 49%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 50%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 51%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 52%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 53%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 54%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 55%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 56%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 57%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 58%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 59%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 60%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | 61%positive => (-(1 # 7)
                      + (8 # 1) * (s IDShort_term_synthesis_filtering__tmp)
                      + (8 # 7) * (s IDShort_term_synthesis_filtering_i)
                      + (s IDShort_term_synthesis_filtering_z)
                      + (1 # 7) * max0(8
                                       - (s IDShort_term_synthesis_filtering_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Short_term_synthesis_filtering_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-8 0*) F_one;
                     (*-8 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDShort_term_synthesis_filtering__tmp))) (F_check_ge (0) (0));
                     (*-8 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDShort_term_synthesis_filtering__tmp)) (0))) (F_max0_ge_0 ((s IDShort_term_synthesis_filtering__tmp)))]
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDShort_term_synthesis_filtering_i))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDShort_term_synthesis_filtering_i)) (0))) (F_max0_ge_0 ((s IDShort_term_synthesis_filtering_i)));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_arg (7
                                                                    - (s IDShort_term_synthesis_filtering_i))) (F_check_ge (7
                                                                    - (s IDShort_term_synthesis_filtering_i)) (0))]
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
    | 42%positive => [(*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDShort_term_synthesis_filtering_i)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDShort_term_synthesis_filtering_i)));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_arg (7
                                                                    - (s IDShort_term_synthesis_filtering_i))) (F_check_ge (7
                                                                    - (s IDShort_term_synthesis_filtering_i)) (0))]
    | 43%positive => [(*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDShort_term_synthesis_filtering_i)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDShort_term_synthesis_filtering_i)));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_arg (7
                                                                    - (s IDShort_term_synthesis_filtering_i))) (F_check_ge (7
                                                                    - (s IDShort_term_synthesis_filtering_i)) (0))]
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => [(*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - (s IDShort_term_synthesis_filtering_i)) (0))) (F_max0_ge_0 (8
                                                                    - (s IDShort_term_synthesis_filtering_i)));
                      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_arg (7
                                                                    - (s IDShort_term_synthesis_filtering_i))) (F_check_ge (7
                                                                    - (s IDShort_term_synthesis_filtering_i)) (0))]
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | _ => []
  end.


Theorem Short_term_synthesis_filtering_ai_correct:
  forall s p' s', steps (g_start Short_term_synthesis_filtering) s (g_edges Short_term_synthesis_filtering) p' s' -> Short_term_synthesis_filtering_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Short_term_synthesis_filtering_pot_correct:
  forall s p' s',
    steps (g_start Short_term_synthesis_filtering) s (g_edges Short_term_synthesis_filtering) p' s' ->
    (Short_term_synthesis_filtering_pot (g_start Short_term_synthesis_filtering) s >= Short_term_synthesis_filtering_pot p' s')%Q.
Proof.
  check_lp Short_term_synthesis_filtering_ai_correct Short_term_synthesis_filtering_hints.
Qed.

