Require Import pasta.Pasta.

Notation IDGsm_Long_Term_Synthesis_Filtering_z := 1%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering_Nr := 2%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering_S_dref_off630 := 3%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering__tmp := 4%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering__tmp1 := 5%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering_brp := 6%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering_drpp := 7%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering_k := 8%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering_ltmp := 9%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering_Ncr := 10%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering_S := 11%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering_bcr := 12%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering_drp := 13%positive.
Notation IDGsm_Long_Term_Synthesis_Filtering_erp := 14%positive.
Definition Gsm_Long_Term_Synthesis_Filtering : graph := {|
  g_start := 1%positive;
  g_end := 41%positive;
  g_edges := (1%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering__tmp
             (Some (EVar IDGsm_Long_Term_Synthesis_Filtering_Ncr))),
             3%positive)::
             (3%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering__tmp1
             (Some (EVar IDGsm_Long_Term_Synthesis_Filtering_bcr))),
             4%positive)::(4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDGsm_Long_Term_Synthesis_Filtering__tmp)
             s) < (eval (ENum (40)) s))%Z)),11%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDGsm_Long_Term_Synthesis_Filtering__tmp)
             s) >= (eval (ENum (40)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDGsm_Long_Term_Synthesis_Filtering__tmp)
             s) > (eval (ENum (120)) s))%Z)),10%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDGsm_Long_Term_Synthesis_Filtering__tmp)
             s) <= (eval (ENum (120)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,13%positive)::
             (10%positive,AWeaken,12%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering_Nr
             None),14%positive)::
             (14%positive,(AAssign
             IDGsm_Long_Term_Synthesis_Filtering_S_dref_off630
             (Some (EVar IDGsm_Long_Term_Synthesis_Filtering_Nr))),
             15%positive)::(15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDGsm_Long_Term_Synthesis_Filtering_Nr)
             s) >= (eval (ENum (40)) s))%Z)),18%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDGsm_Long_Term_Synthesis_Filtering_Nr)
             s) < (eval (ENum (40)) s))%Z)),17%positive)::
             (17%positive,AWeaken,21%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDGsm_Long_Term_Synthesis_Filtering_Nr)
             s) <= (eval (ENum (120)) s))%Z)),23%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDGsm_Long_Term_Synthesis_Filtering_Nr)
             s) > (eval (ENum (120)) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,41%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering_brp
             None),26%positive)::(26%positive,AWeaken,27%positive)::
             (27%positive,ANone,30%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,AWeaken,41%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering_k
             (Some (ENum (0)))),32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDGsm_Long_Term_Synthesis_Filtering_k)
             s) <= (eval (ENum (39)) s))%Z)),49%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDGsm_Long_Term_Synthesis_Filtering_k)
             s) > (eval (ENum (39)) s))%Z)),35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering_k
             (Some (ENum (0)))),37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,(AGuard
             (fun s => ((eval (EVar IDGsm_Long_Term_Synthesis_Filtering_k)
             s) <= (eval (ENum (119)) s))%Z)),42%positive)::
             (39%positive,(AGuard
             (fun s => ((eval (EVar IDGsm_Long_Term_Synthesis_Filtering_k)
             s) > (eval (ENum (119)) s))%Z)),40%positive)::
             (40%positive,AWeaken,41%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering_k
             (Some (EAdd (EVar IDGsm_Long_Term_Synthesis_Filtering_k)
             (ENum (1))))),45%positive)::(45%positive,ANone,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering_z
             (Some (EAdd (ENum (1))
             (EVar IDGsm_Long_Term_Synthesis_Filtering_z)))),48%positive)::
             (48%positive,AWeaken,39%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering_drpp
             None),51%positive)::
             (51%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering_ltmp
             None),52%positive)::(52%positive,AWeaken,53%positive)::
             (53%positive,ANone,55%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,ANone,56%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering_k
             (Some (EAdd (EVar IDGsm_Long_Term_Synthesis_Filtering_k)
             (ENum (1))))),58%positive)::(58%positive,ANone,59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,(AAssign IDGsm_Long_Term_Synthesis_Filtering_z
             (Some (EAdd (ENum (1))
             (EVar IDGsm_Long_Term_Synthesis_Filtering_z)))),61%positive)::
             (61%positive,AWeaken,34%positive)::nil
|}.

Definition Gsm_Long_Term_Synthesis_Filtering_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 3%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 4%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 5%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 6%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering__tmp) + 40 <= 0)%Z
    | 7%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering__tmp) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 8%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering__tmp) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering__tmp) + -120 <= 0)%Z
    | 9%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering__tmp) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering__tmp) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 10%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering__tmp) + 121 <= 0)%Z
    | 11%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering__tmp) + -39 <= 0)%Z
    | 12%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 13%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 14%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 15%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 16%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 17%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -39 <= 0)%Z
    | 18%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0)%Z
    | 19%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 20%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 121 <= 0)%Z
    | 21%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 22%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 23%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 24%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 25%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 26%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 27%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 28%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 29%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 30%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 31%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 32%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0)%Z
    | 33%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 34%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -40 <= 0)%Z
    | 35%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + 40 <= 0)%Z
    | 36%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -40 <= 0)%Z
    | 37%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0)%Z
    | 38%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 39%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -120 <= 0)%Z
    | 40%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + 120 <= 0)%Z
    | 41%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 42%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -119 <= 0)%Z
    | 43%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -119 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0)%Z
    | 44%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -119 <= 0)%Z
    | 45%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + 1 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -120 <= 0)%Z
    | 46%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + 1 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0)%Z
    | 47%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + 1 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -120 <= 0)%Z
    | 48%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + 1 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) + 1 <= 0)%Z
    | 49%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -39 <= 0)%Z
    | 50%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -39 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 51%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -39 <= 0)%Z
    | 52%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -39 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 53%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -39 <= 0)%Z
    | 54%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -39 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 55%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -39 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 56%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -39 <= 0)%Z
    | 57%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -39 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 58%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + 1 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0)%Z
    | 60%positive => (1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + 1 <= 0)%Z
    | 61%positive => (-1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + 1 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_k) + -40 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + 40 <= 0 /\ 1 * (s IDGsm_Long_Term_Synthesis_Filtering_Nr) + -120 <= 0 /\ -1 * (s IDGsm_Long_Term_Synthesis_Filtering_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition Gsm_Long_Term_Synthesis_Filtering_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((2 # 1) * max0(-40
                                    + (s IDGsm_Long_Term_Synthesis_Filtering_Ncr))
                     + (2 # 1) * max0(120
                                      - (s IDGsm_Long_Term_Synthesis_Filtering_Ncr)))%Q
    | 2%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                     + (2 # 1) * max0(-40
                                      + (s IDGsm_Long_Term_Synthesis_Filtering_Ncr))
                     + (2 # 1) * max0(120
                                      - (s IDGsm_Long_Term_Synthesis_Filtering_Ncr)))%Q
    | 3%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                     + (2 # 1) * max0(-40
                                      + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))
                     + (2 # 1) * max0(120
                                      - (s IDGsm_Long_Term_Synthesis_Filtering__tmp)))%Q
    | 4%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                     + (2 # 1) * max0(-40
                                      + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))
                     + (2 # 1) * max0(120
                                      - (s IDGsm_Long_Term_Synthesis_Filtering__tmp)))%Q
    | 5%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                     + (2 # 1) * max0(-40
                                      + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))
                     + (2 # 1) * max0(120
                                      - (s IDGsm_Long_Term_Synthesis_Filtering__tmp)))%Q
    | 6%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                     + (2 # 1) * max0(-40
                                      + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))
                     + (2 # 1) * max0(120
                                      - (s IDGsm_Long_Term_Synthesis_Filtering__tmp)))%Q
    | 7%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                     + (2 # 1) * max0(-40
                                      + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))
                     + (2 # 1) * max0(120
                                      - (s IDGsm_Long_Term_Synthesis_Filtering__tmp)))%Q
    | 8%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                     + (2 # 1) * max0(-40
                                      + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))
                     + (2 # 1) * max0(120
                                      - (s IDGsm_Long_Term_Synthesis_Filtering__tmp)))%Q
    | 9%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 10%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + (2 # 1) * max0(-40
                                       + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))
                      + (2 # 1) * max0(120
                                       - (s IDGsm_Long_Term_Synthesis_Filtering__tmp)))%Q
    | 11%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + (2 # 1) * max0(-40
                                       + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))
                      + (2 # 1) * max0(120
                                       - (s IDGsm_Long_Term_Synthesis_Filtering__tmp)))%Q
    | 12%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 13%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 14%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 15%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 16%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 17%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 18%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 19%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 20%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 21%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 22%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 23%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 24%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 25%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 26%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 27%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 28%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 29%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 30%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 31%positive => ((160 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 32%positive => ((120 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(40 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 33%positive => ((120 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(40 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 34%positive => ((120 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(40 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 35%positive => ((120 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(40 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 36%positive => ((120 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 37%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(120 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 38%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(120 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 39%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(120 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 40%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(120 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 41%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z))%Q
    | 42%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(120 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 43%positive => ((1 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(119 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 44%positive => ((1 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(119 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 45%positive => ((1 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(120 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 46%positive => ((1 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(120 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 47%positive => ((1 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(120 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 48%positive => ((s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(120 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 49%positive => ((120 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(40 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 50%positive => ((120 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(40 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 51%positive => ((120 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(40 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 52%positive => ((120 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(40 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 53%positive => ((121 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(39 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 54%positive => ((121 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(39 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 55%positive => ((121 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(39 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 56%positive => ((121 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(39 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 57%positive => ((121 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(39 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 58%positive => ((121 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(40 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 59%positive => ((121 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(40 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 60%positive => ((121 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(40 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | 61%positive => ((120 # 1) + (s IDGsm_Long_Term_Synthesis_Filtering_z)
                      + max0(40 - (s IDGsm_Long_Term_Synthesis_Filtering_k)))%Q
    | _ => (0 # 1)%Q
  end.

Definition Gsm_Long_Term_Synthesis_Filtering_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-2 0*) F_binom_monotonic 1 (F_max0_ge_arg (120
                                                                  - (s IDGsm_Long_Term_Synthesis_Filtering__tmp))) (F_check_ge (120
                                                                    - (s IDGsm_Long_Term_Synthesis_Filtering__tmp)) (0));
                     (*0 2*) F_binom_monotonic 1 (F_max0_ge_arg (-40
                                                                 + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))) (F_check_ge (-40
                                                                    + (s IDGsm_Long_Term_Synthesis_Filtering__tmp)) (0))]
    | 9%positive => []
    | 10%positive => [(*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (120
                                                                 - (s IDGsm_Long_Term_Synthesis_Filtering__tmp))) (F_check_ge (0) (0));
                      (*-0.0246914 0*) F_binom_monotonic 1 (F_max0_ge_0 (-40
                                                                    + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))) (F_check_ge (0) (0));
                      (*-1.97531 0*) F_binom_monotonic 1 (F_max0_ge_arg (-40
                                                                    + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))) (F_check_ge (-40
                                                                    + (s IDGsm_Long_Term_Synthesis_Filtering__tmp)) (0));
                      (*-1.97531 0*) F_binom_monotonic 1 (F_max0_ge_0 (-121
                                                                    + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))) (F_check_ge (0) (0));
                      (*-1.97531 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-121
                                                                    + (s IDGsm_Long_Term_Synthesis_Filtering__tmp)) (0))) (F_max0_ge_0 (-121
                                                                    + (s IDGsm_Long_Term_Synthesis_Filtering__tmp)))]
    | 11%positive => [(*-0.0246914 0*) F_binom_monotonic 1 (F_max0_ge_0 (120
                                                                    - (s IDGsm_Long_Term_Synthesis_Filtering__tmp))) (F_check_ge (0) (0));
                      (*-1.97531 0*) F_binom_monotonic 1 (F_max0_ge_arg (120
                                                                    - (s IDGsm_Long_Term_Synthesis_Filtering__tmp))) (F_check_ge (120
                                                                    - (s IDGsm_Long_Term_Synthesis_Filtering__tmp)) (0));
                      (*-1.97531 0*) F_binom_monotonic 1 (F_max0_ge_0 (39
                                                                    - (s IDGsm_Long_Term_Synthesis_Filtering__tmp))) (F_check_ge (0) (0));
                      (*-1.97531 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (39
                                                                    - (s IDGsm_Long_Term_Synthesis_Filtering__tmp)) (0))) (F_max0_ge_0 (39
                                                                    - (s IDGsm_Long_Term_Synthesis_Filtering__tmp)));
                      (*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (-40
                                                                 + (s IDGsm_Long_Term_Synthesis_Filtering__tmp))) (F_check_ge (0) (0))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*0 160*) F_one]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-160 0*) F_one]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-160 0*) F_one]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (40
                                                             - (s IDGsm_Long_Term_Synthesis_Filtering_k)) (39
                                                                    - (s IDGsm_Long_Term_Synthesis_Filtering_k)));
                      (*-1 0*) F_max0_ge_0 (39
                                            - (s IDGsm_Long_Term_Synthesis_Filtering_k))]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (120
                                                             - (s IDGsm_Long_Term_Synthesis_Filtering_k)) (119
                                                                    - (s IDGsm_Long_Term_Synthesis_Filtering_k)));
                      (*-1 0*) F_max0_ge_0 (119
                                            - (s IDGsm_Long_Term_Synthesis_Filtering_k))]
    | 41%positive => []
    | 42%positive => [(*-1 0*) F_max0_pre_decrement (120
                                                     - (s IDGsm_Long_Term_Synthesis_Filtering_k)) (1)]
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | 52%positive => [(*-1 0*) F_max0_pre_decrement (40
                                                     - (s IDGsm_Long_Term_Synthesis_Filtering_k)) (1)]
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


Theorem Gsm_Long_Term_Synthesis_Filtering_ai_correct:
  forall s p' s', steps (g_start Gsm_Long_Term_Synthesis_Filtering) s (g_edges Gsm_Long_Term_Synthesis_Filtering) p' s' -> Gsm_Long_Term_Synthesis_Filtering_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem Gsm_Long_Term_Synthesis_Filtering_pot_correct:
  forall s p' s',
    steps (g_start Gsm_Long_Term_Synthesis_Filtering) s (g_edges Gsm_Long_Term_Synthesis_Filtering) p' s' ->
    (Gsm_Long_Term_Synthesis_Filtering_pot (g_start Gsm_Long_Term_Synthesis_Filtering) s >= Gsm_Long_Term_Synthesis_Filtering_pot p' s')%Q.
Proof.
  check_lp Gsm_Long_Term_Synthesis_Filtering_ai_correct Gsm_Long_Term_Synthesis_Filtering_hints.
Qed.

