Require Import pasta.Pasta.

Notation IDsynth_1to1_z := 1%positive.
Notation IDsynth_1to1__tmp := 2%positive.
Notation IDsynth_1to1_bo := 3%positive.
Notation IDsynth_1to1_bo1 := 4%positive.
Notation IDsynth_1to1_clip := 5%positive.
Notation IDsynth_1to1_gmp_dref_off31872 := 6%positive.
Notation IDsynth_1to1_j := 7%positive.
Notation IDsynth_1to1_pnt_dref := 8%positive.
Notation IDsynth_1to1_bandPtr := 9%positive.
Notation IDsynth_1to1_channel := 10%positive.
Notation IDsynth_1to1_out := 11%positive.
Notation IDsynth_1to1_pnt := 12%positive.
Definition synth_1to1 : graph := {|
  g_start := 1%positive;
  g_end := 42%positive;
  g_edges := (1%positive,(AAssign IDsynth_1to1_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDsynth_1to1__tmp
             (Some (EVar IDsynth_1to1_channel))),3%positive)::
             (3%positive,(AAssign IDsynth_1to1_clip (Some (ENum (0)))),
             4%positive)::
             (4%positive,(AAssign IDsynth_1to1_bo
             (Some (EVar IDsynth_1to1_gmp_dref_off31872))),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDsynth_1to1__tmp)
             s) <> (eval (ENum (0)) s))%Z)),12%positive)::
             (6%positive,(AGuard (fun s => ((eval (EVar IDsynth_1to1__tmp)
             s) = (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AAssign IDsynth_1to1_bo
             (Some (EAdd (EVar IDsynth_1to1_bo) (ENum (-1))))),9%positive)::
             (9%positive,(AAssign IDsynth_1to1_bo None),10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,15%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,ANone,18%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDsynth_1to1_bo1
             (Some (EAdd (EVar IDsynth_1to1_bo) (ENum (1))))),17%positive)::
             (17%positive,ANone,20%positive)::
             (18%positive,(AAssign IDsynth_1to1_bo1
             (Some (EVar IDsynth_1to1_bo))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDsynth_1to1_gmp_dref_off31872
             (Some (EVar IDsynth_1to1_bo))),21%positive)::
             (21%positive,(AAssign IDsynth_1to1_j (Some (ENum (16)))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDsynth_1to1_j)
             s) <> (eval (ENum (0)) s))%Z)),59%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDsynth_1to1_j) s) =
             (eval (ENum (0)) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,33%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,30%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,32%positive)::
             (30%positive,(AAssign IDsynth_1to1_clip
             (Some (EAdd (EVar IDsynth_1to1_clip) (ENum (1))))),31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,ANone,35%positive)::
             (33%positive,(AAssign IDsynth_1to1_clip
             (Some (EAdd (EVar IDsynth_1to1_clip) (ENum (1))))),34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,(AAssign IDsynth_1to1_j (Some (ENum (15)))),
             36%positive)::(36%positive,ANone,37%positive)::
             (37%positive,AWeaken,38%positive)::
             (38%positive,(AGuard (fun s => ((eval (EVar IDsynth_1to1_j)
             s) <> (eval (ENum (0)) s))%Z)),43%positive)::
             (38%positive,(AGuard (fun s => ((eval (EVar IDsynth_1to1_j) s) =
             (eval (ENum (0)) s))%Z)),39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AAssign IDsynth_1to1_pnt_dref
             (Some (EAdd (EVar IDsynth_1to1_pnt_dref) (ENum (128))))),
             41%positive)::(41%positive,AWeaken,42%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,ANone,51%positive)::
             (44%positive,ANone,45%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,ANone,48%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,ANone,50%positive)::
             (48%positive,(AAssign IDsynth_1to1_clip
             (Some (EAdd (EVar IDsynth_1to1_clip) (ENum (1))))),49%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,53%positive)::
             (51%positive,(AAssign IDsynth_1to1_clip
             (Some (EAdd (EVar IDsynth_1to1_clip) (ENum (1))))),52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDsynth_1to1_j
             (Some (EAdd (EVar IDsynth_1to1_j) (ENum (-1))))),55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,(AAssign IDsynth_1to1_z (Some (EAdd (ENum (1))
             (EVar IDsynth_1to1_z)))),58%positive)::
             (58%positive,AWeaken,38%positive)::
             (59%positive,AWeaken,60%positive)::
             (60%positive,ANone,67%positive)::
             (60%positive,ANone,61%positive)::
             (61%positive,AWeaken,62%positive)::
             (62%positive,ANone,64%positive)::
             (62%positive,ANone,63%positive)::
             (63%positive,ANone,66%positive)::
             (64%positive,(AAssign IDsynth_1to1_clip
             (Some (EAdd (EVar IDsynth_1to1_clip) (ENum (1))))),65%positive)::
             (65%positive,ANone,66%positive)::
             (66%positive,ANone,69%positive)::
             (67%positive,(AAssign IDsynth_1to1_clip
             (Some (EAdd (EVar IDsynth_1to1_clip) (ENum (1))))),68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,(AAssign IDsynth_1to1_j
             (Some (EAdd (EVar IDsynth_1to1_j) (ENum (-1))))),71%positive)::
             (71%positive,ANone,72%positive)::
             (72%positive,ANone,73%positive)::
             (73%positive,(AAssign IDsynth_1to1_z (Some (EAdd (ENum (1))
             (EVar IDsynth_1to1_z)))),74%positive)::
             (74%positive,AWeaken,24%positive)::nil
|}.

Definition synth_1to1_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0)%Z
    | 4%positive => (1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 5%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0)%Z
    | 6%positive => (1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 7%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1__tmp) <= 0 /\ -1 * (s IDsynth_1to1__tmp) <= 0)%Z
    | 8%positive => (-1 * (s IDsynth_1to1__tmp) <= 0 /\ 1 * (s IDsynth_1to1__tmp) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 9%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1__tmp) <= 0 /\ -1 * (s IDsynth_1to1__tmp) <= 0)%Z
    | 10%positive => (-1 * (s IDsynth_1to1__tmp) <= 0 /\ 1 * (s IDsynth_1to1__tmp) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 11%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1__tmp) <= 0 /\ -1 * (s IDsynth_1to1__tmp) <= 0)%Z
    | 12%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0)%Z
    | 13%positive => (1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 14%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0)%Z
    | 15%positive => (1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 16%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0)%Z
    | 17%positive => (1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 18%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0)%Z
    | 19%positive => (1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 20%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0)%Z
    | 21%positive => (1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 22%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -16 <= 0 /\ -1 * (s IDsynth_1to1_j) + 16 <= 0)%Z
    | 23%positive => (-1 * (s IDsynth_1to1_j) + 16 <= 0 /\ 1 * (s IDsynth_1to1_j) + -16 <= 0 /\ 1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 24%positive => (-1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -16 <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 25%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_j) <= 0)%Z
    | 26%positive => (-1 * (s IDsynth_1to1_j) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 27%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_j) <= 0)%Z
    | 28%positive => (-1 * (s IDsynth_1to1_j) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 29%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_j) <= 0)%Z
    | 30%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_j) <= 0)%Z
    | 31%positive => (-1 * (s IDsynth_1to1_j) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_j) <= 0)%Z
    | 33%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_j) <= 0)%Z
    | 34%positive => (-1 * (s IDsynth_1to1_j) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_j) <= 0)%Z
    | 36%positive => (-1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0 /\ 1 * (s IDsynth_1to1_j) + -15 <= 0 /\ -1 * (s IDsynth_1to1_j) + 15 <= 0)%Z
    | 37%positive => (-1 * (s IDsynth_1to1_j) + 15 <= 0 /\ 1 * (s IDsynth_1to1_j) + -15 <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0)%Z
    | 38%positive => (-1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -15 <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 39%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_j) <= 0)%Z
    | 40%positive => (-1 * (s IDsynth_1to1_j) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 41%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_j) <= 0)%Z
    | 42%positive => (-1 * (s IDsynth_1to1_j) <= 0 /\ 1 * (s IDsynth_1to1_j) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 43%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -15 <= 0)%Z
    | 44%positive => (1 * (s IDsynth_1to1_j) + -15 <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 45%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -15 <= 0)%Z
    | 46%positive => (1 * (s IDsynth_1to1_j) + -15 <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 47%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -15 <= 0)%Z
    | 48%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -15 <= 0)%Z
    | 49%positive => (1 * (s IDsynth_1to1_j) + -15 <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -15 <= 0)%Z
    | 51%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -15 <= 0)%Z
    | 52%positive => (1 * (s IDsynth_1to1_j) + -15 <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) + 1 <= 0)%Z
    | 53%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -15 <= 0)%Z
    | 54%positive => (1 * (s IDsynth_1to1_j) + -15 <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 55%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -14 <= 0)%Z
    | 56%positive => (1 * (s IDsynth_1to1_j) + -14 <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 57%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -14 <= 0)%Z
    | 58%positive => (1 * (s IDsynth_1to1_j) + -14 <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) + 1 <= 0)%Z
    | 59%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -16 <= 0)%Z
    | 60%positive => (1 * (s IDsynth_1to1_j) + -16 <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 61%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -16 <= 0)%Z
    | 62%positive => (1 * (s IDsynth_1to1_j) + -16 <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 63%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -16 <= 0)%Z
    | 64%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -16 <= 0)%Z
    | 65%positive => (1 * (s IDsynth_1to1_j) + -16 <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) + 1 <= 0)%Z
    | 66%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -16 <= 0)%Z
    | 67%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -16 <= 0)%Z
    | 68%positive => (1 * (s IDsynth_1to1_j) + -16 <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) + 1 <= 0)%Z
    | 69%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -16 <= 0)%Z
    | 70%positive => (1 * (s IDsynth_1to1_j) + -16 <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 71%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -15 <= 0)%Z
    | 72%positive => (1 * (s IDsynth_1to1_j) + -15 <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0)%Z
    | 73%positive => (-1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) <= 0 /\ 1 * (s IDsynth_1to1_j) + -15 <= 0)%Z
    | 74%positive => (1 * (s IDsynth_1to1_j) + -15 <= 0 /\ -1 * (s IDsynth_1to1_clip) <= 0 /\ -1 * (s IDsynth_1to1_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition synth_1to1_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((31 # 1))%Q
    | 2%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 3%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 4%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 5%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 6%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 7%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 8%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 9%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 10%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 11%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 12%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 13%positive => ((31 # 1))%Q
    | 14%positive => ((31 # 1))%Q
    | 15%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 16%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 17%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 18%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 19%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 20%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 21%positive => ((31 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 22%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 23%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 24%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 25%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 26%positive => ((15 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 27%positive => ((15 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 28%positive => ((15 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 29%positive => ((15 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 30%positive => ((15 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 31%positive => ((15 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 32%positive => ((15 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 33%positive => ((15 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 34%positive => ((15 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 35%positive => ((15 # 1) + max0((s IDsynth_1to1_z)))%Q
    | 36%positive => ((s IDsynth_1to1_j) + max0((s IDsynth_1to1_z)))%Q
    | 37%positive => ((s IDsynth_1to1_j) + max0((s IDsynth_1to1_z)))%Q
    | 38%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 39%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 40%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 41%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 42%positive => ((s IDsynth_1to1_z))%Q
    | 43%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 44%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 45%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 46%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 47%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 48%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 49%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 50%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 51%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 52%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 53%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 54%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 55%positive => ((1 # 1) + (s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 56%positive => ((1 # 1) + (s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 57%positive => ((1 # 1) + (s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 58%positive => ((s IDsynth_1to1_j) + (s IDsynth_1to1_z))%Q
    | 59%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 60%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 61%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 62%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 63%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 64%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 65%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 66%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 67%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 68%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 69%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 70%positive => ((15 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 71%positive => ((16 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 72%positive => ((16 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 73%positive => ((16 # 1) + (s IDsynth_1to1_j)
                      + max0((s IDsynth_1to1_z)))%Q
    | 74%positive => ((16 # 1) + (s IDsynth_1to1_j)
                      + max0(-1 + (s IDsynth_1to1_z)))%Q
    | _ => (0 # 1)%Q
  end.

Definition synth_1to1_hints (p : node) (s : state) := 
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
    | 12%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDsynth_1to1_z))) (F_check_ge (0) (0))]
    | 13%positive => []
    | 14%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsynth_1to1_z)) (0))) (F_max0_ge_0 ((s IDsynth_1to1_z)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDsynth_1to1_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDsynth_1to1_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDsynth_1to1_z)))]
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
    | 25%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDsynth_1to1_j))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsynth_1to1_j)) (0))) (F_max0_ge_0 ((s IDsynth_1to1_j)))]
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
    | 37%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDsynth_1to1_z))) (F_check_ge ((s IDsynth_1to1_z)) (0))]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDsynth_1to1_j))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsynth_1to1_j)) (0))) (F_max0_ge_0 ((s IDsynth_1to1_j)))]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
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
    | 62%positive => []
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => []
    | 67%positive => []
    | 68%positive => []
    | 69%positive => []
    | 70%positive => []
    | 71%positive => []
    | 72%positive => []
    | 73%positive => []
    | 74%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDsynth_1to1_z)) (0))) (F_max0_ge_0 ((s IDsynth_1to1_z)));
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                  + (s IDsynth_1to1_z))) (F_check_ge (-1
                                                                    + (s IDsynth_1to1_z)) (0))]
    | _ => []
  end.


Theorem synth_1to1_ai_correct:
  forall s p' s', steps (g_start synth_1to1) s (g_edges synth_1to1) p' s' -> synth_1to1_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem synth_1to1_pot_correct:
  forall s p' s',
    steps (g_start synth_1to1) s (g_edges synth_1to1) p' s' ->
    (synth_1to1_pot (g_start synth_1to1) s >= synth_1to1_pot p' s')%Q.
Proof.
  check_lp synth_1to1_ai_correct synth_1to1_hints.
Qed.

