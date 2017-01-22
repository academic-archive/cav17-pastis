Require Import pasta.Pasta.

Notation IDsuspect_word_z := 1%positive.
Notation IDsuspect_word__tmp := 2%positive.
Notation IDsuspect_word_ch := 3%positive.
Notation IDsuspect_word_i := 4%positive.
Notation IDsuspect_word_last := 5%positive.
Notation IDsuspect_word_seen_lower := 6%positive.
Notation IDsuspect_word_seen_upper := 7%positive.
Notation IDsuspect_word_seen_vowel := 8%positive.
Notation IDsuspect_word_n := 9%positive.
Notation IDsuspect_word_s := 10%positive.
Definition suspect_word : graph := {|
  g_start := 1%positive;
  g_end := 24%positive;
  g_edges := (1%positive,(AAssign IDsuspect_word_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDsuspect_word__tmp
             (Some (EVar IDsuspect_word_n))),3%positive)::
             (3%positive,(AAssign IDsuspect_word_i (Some (ENum (0)))),
             4%positive)::
             (4%positive,(AAssign IDsuspect_word_seen_lower
             (Some (ENum (0)))),5%positive)::
             (5%positive,(AAssign IDsuspect_word_seen_upper
             (Some (ENum (0)))),6%positive)::
             (6%positive,(AAssign IDsuspect_word_seen_vowel
             (Some (ENum (0)))),7%positive)::
             (7%positive,(AAssign IDsuspect_word_last (Some (ENum (0)))),
             8%positive)::
             (8%positive,(AAssign IDsuspect_word_i (Some (ENum (0)))),
             9%positive)::(9%positive,ANone,10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_i)
             s) < (eval (EVar IDsuspect_word__tmp) s))%Z)),25%positive)::
             (11%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_i)
             s) >= (eval (EVar IDsuspect_word__tmp) s))%Z)),12%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDsuspect_word_seen_vowel) s) <>
             (eval (ENum (0)) s))%Z)),15%positive)::
             (13%positive,(AGuard
             (fun s => ((eval (EVar IDsuspect_word_seen_vowel) s) =
             (eval (ENum (0)) s))%Z)),14%positive)::
             (14%positive,AWeaken,24%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDsuspect_word_seen_upper) s) <>
             (eval (ENum (0)) s))%Z)),18%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDsuspect_word_seen_upper) s) =
             (eval (ENum (0)) s))%Z)),17%positive)::
             (17%positive,AWeaken,21%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDsuspect_word_seen_lower) s) <>
             (eval (ENum (0)) s))%Z)),23%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDsuspect_word_seen_lower) s) =
             (eval (ENum (0)) s))%Z)),20%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,AWeaken,24%positive)::
             (23%positive,AWeaken,24%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDsuspect_word_ch None),27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_i)
             s) <> (eval (ENum (0)) s))%Z)),30%positive)::
             (28%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_i)
             s) = (eval (ENum (0)) s))%Z)),29%positive)::
             (29%positive,AWeaken,39%positive)::
             (30%positive,AWeaken,31%positive)::
             (31%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_last)
             s) <> (eval (ENum (45)) s))%Z)),33%positive)::
             (31%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_last)
             s) = (eval (ENum (45)) s))%Z)),32%positive)::
             (32%positive,AWeaken,39%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,36%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,39%positive)::
             (36%positive,(AAssign IDsuspect_word_seen_upper
             (Some (ENum (1)))),37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,ANone,41%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,AWeaken,45%positive)::
             (41%positive,(AAssign IDsuspect_word_seen_lower
             (Some (ENum (1)))),42%positive)::
             (42%positive,(AAssign IDsuspect_word_ch None),43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_ch)
             s) = (eval (ENum (65)) s))%Z)),62%positive)::
             (45%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_ch)
             s) <> (eval (ENum (65)) s))%Z)),46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_ch)
             s) = (eval (ENum (69)) s))%Z)),61%positive)::
             (47%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_ch)
             s) <> (eval (ENum (69)) s))%Z)),48%positive)::
             (48%positive,AWeaken,49%positive)::
             (49%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_ch)
             s) = (eval (ENum (73)) s))%Z)),60%positive)::
             (49%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_ch)
             s) <> (eval (ENum (73)) s))%Z)),50%positive)::
             (50%positive,AWeaken,51%positive)::
             (51%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_ch)
             s) = (eval (ENum (79)) s))%Z)),59%positive)::
             (51%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_ch)
             s) <> (eval (ENum (79)) s))%Z)),52%positive)::
             (52%positive,AWeaken,53%positive)::
             (53%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_ch)
             s) = (eval (ENum (85)) s))%Z)),58%positive)::
             (53%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_ch)
             s) <> (eval (ENum (85)) s))%Z)),54%positive)::
             (54%positive,AWeaken,55%positive)::
             (55%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_ch)
             s) = (eval (ENum (89)) s))%Z)),57%positive)::
             (55%positive,(AGuard (fun s => ((eval (EVar IDsuspect_word_ch)
             s) <> (eval (ENum (89)) s))%Z)),56%positive)::
             (56%positive,AWeaken,65%positive)::
             (57%positive,AWeaken,63%positive)::
             (58%positive,AWeaken,63%positive)::
             (59%positive,AWeaken,63%positive)::
             (60%positive,AWeaken,63%positive)::
             (61%positive,AWeaken,63%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,(AAssign IDsuspect_word_seen_vowel
             (Some (ENum (1)))),64%positive)::
             (64%positive,ANone,65%positive)::
             (65%positive,(AAssign IDsuspect_word_last
             (Some (EVar IDsuspect_word_ch))),66%positive)::
             (66%positive,ANone,67%positive)::
             (67%positive,(AAssign IDsuspect_word_i
             (Some (EAdd (EVar IDsuspect_word_i) (ENum (1))))),68%positive)::
             (68%positive,ANone,69%positive)::
             (69%positive,ANone,70%positive)::
             (70%positive,(AAssign IDsuspect_word_z (Some (EAdd (ENum (1))
             (EVar IDsuspect_word_z)))),71%positive)::
             (71%positive,AWeaken,11%positive)::nil
|}.

Definition suspect_word_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word_z) <= 0)%Z
    | 4%positive => (1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 5%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ 1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0)%Z
    | 6%positive => (-1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ 1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0)%Z
    | 7%positive => (-1 * (s IDsuspect_word_seen_upper) <= 0 /\ 1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ 1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0)%Z
    | 8%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ 1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ 1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ 1 * (s IDsuspect_word_last) <= 0 /\ -1 * (s IDsuspect_word_last) <= 0)%Z
    | 9%positive => (-1 * (s IDsuspect_word_last) <= 0 /\ 1 * (s IDsuspect_word_last) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ 1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ 1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 10%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ 1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ 1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ 1 * (s IDsuspect_word_last) <= 0 /\ -1 * (s IDsuspect_word_last) <= 0)%Z
    | 11%positive => (-1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0)%Z
    | 12%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 13%positive => (1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0)%Z
    | 14%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0 /\ 1 * (s IDsuspect_word_seen_vowel) <= 0)%Z
    | 15%positive => (-1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDsuspect_word_seen_vowel) + 1 <= 0 /\ 1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0)%Z
    | 17%positive => (-1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) + 1 <= 0 /\ 1 * (s IDsuspect_word_seen_upper) <= 0)%Z
    | 18%positive => (-1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDsuspect_word_seen_upper) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) + 1 <= 0 /\ 1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0)%Z
    | 20%positive => (-1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) + 1 <= 0 /\ 1 * (s IDsuspect_word_seen_lower) <= 0)%Z
    | 21%positive => (-1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) + 1 <= 0 /\ 1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0)%Z
    | 22%positive => (-1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0)%Z
    | 23%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ 1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_lower) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word__tmp)+ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 25%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0)%Z
    | 27%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0)%Z
    | 29%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ 1 * (s IDsuspect_word_i) <= 0)%Z
    | 30%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_i) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0)%Z
    | 32%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_i) + 1 <= 0 /\ 1 * (s IDsuspect_word_last) + -45 <= 0 /\ -1 * (s IDsuspect_word_last) + 45 <= 0)%Z
    | 33%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_i) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0)%Z
    | 35%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_i) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_i) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ 1 * (s IDsuspect_word_seen_upper) + -1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDsuspect_word_seen_upper) + 1 <= 0 /\ 1 * (s IDsuspect_word_seen_upper) + -1 <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_i) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0)%Z
    | 40%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 41%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 42%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ 1 * (s IDsuspect_word_seen_lower) + -1 <= 0 /\ -1 * (s IDsuspect_word_seen_lower) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDsuspect_word_seen_lower) + 1 <= 0 /\ 1 * (s IDsuspect_word_seen_lower) + -1 <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 44%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ 1 * (s IDsuspect_word_seen_lower) + -1 <= 0 /\ -1 * (s IDsuspect_word_seen_lower) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 46%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0)%Z
    | 47%positive => (-1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 48%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0)%Z
    | 49%positive => (-1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 50%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0)%Z
    | 51%positive => (-1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 52%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0)%Z
    | 53%positive => (-1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 54%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0)%Z
    | 55%positive => (-1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 56%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0)%Z
    | 57%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_ch) + -89 <= 0 /\ -1 * (s IDsuspect_word_ch) + 89 <= 0)%Z
    | 58%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_ch) + -85 <= 0 /\ -1 * (s IDsuspect_word_ch) + 85 <= 0)%Z
    | 59%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_ch) + -79 <= 0 /\ -1 * (s IDsuspect_word_ch) + 79 <= 0)%Z
    | 60%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_ch) + -73 <= 0 /\ -1 * (s IDsuspect_word_ch) + 73 <= 0)%Z
    | 61%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_ch) + -69 <= 0 /\ -1 * (s IDsuspect_word_ch) + 69 <= 0)%Z
    | 62%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ 1 * (s IDsuspect_word_ch) + -65 <= 0 /\ -1 * (s IDsuspect_word_ch) + 65 <= 0)%Z
    | 63%positive => (1 * (s IDsuspect_word_ch) + -89 <= 0 /\ -1 * (s IDsuspect_word_ch) + 65 <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 64%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_ch) + 65 <= 0 /\ 1 * (s IDsuspect_word_ch) + -89 <= 0 /\ 1 * (s IDsuspect_word_seen_vowel) + -1 <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) + 1 <= 0)%Z
    | 65%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 66%positive => (-1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0)%Z
    | 67%positive => (-1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_i) <= 0)%Z
    | 68%positive => (-1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_i) + 1 <= 0)%Z
    | 69%positive => (-1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0)%Z
    | 70%positive => (-1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_z) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_i) + 1 <= 0)%Z
    | 71%positive => (-1 * (s IDsuspect_word_i) + 1 <= 0 /\ -1 * (s IDsuspect_word__tmp)+ 1 * (s IDsuspect_word_i) <= 0 /\ -1 * (s IDsuspect_word_seen_vowel) <= 0 /\ -1 * (s IDsuspect_word_seen_lower) <= 0 /\ -1 * (s IDsuspect_word_seen_upper) <= 0 /\ -1 * (s IDsuspect_word_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition suspect_word_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDsuspect_word_n)))%Q
    | 2%positive => ((s IDsuspect_word_z) + max0((s IDsuspect_word_n)))%Q
    | 3%positive => ((s IDsuspect_word_z) + max0((s IDsuspect_word__tmp)))%Q
    | 4%positive => ((s IDsuspect_word_z) + max0((s IDsuspect_word__tmp)))%Q
    | 5%positive => ((s IDsuspect_word_z) + max0((s IDsuspect_word__tmp)))%Q
    | 6%positive => ((s IDsuspect_word_z) + max0((s IDsuspect_word__tmp)))%Q
    | 7%positive => ((s IDsuspect_word_z) + max0((s IDsuspect_word__tmp)))%Q
    | 8%positive => ((s IDsuspect_word_z) + max0((s IDsuspect_word__tmp)))%Q
    | 9%positive => ((s IDsuspect_word_z)
                     + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 10%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 11%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 12%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 13%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 14%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 15%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 16%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 17%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 18%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 19%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 20%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 21%positive => ((s IDsuspect_word_z))%Q
    | 22%positive => ((s IDsuspect_word_z))%Q
    | 23%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 24%positive => ((s IDsuspect_word_z))%Q
    | 25%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 26%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 27%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 28%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 29%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 30%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 31%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 32%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 33%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 34%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 35%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 36%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 37%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 38%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 39%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 40%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 41%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 42%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 43%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 44%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 45%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 46%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 47%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 48%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 49%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 50%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 51%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 52%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 53%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 54%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 55%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 56%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 57%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 58%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 59%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 60%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 61%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 62%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 63%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 64%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 65%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 66%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 67%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0(-1 + (s IDsuspect_word__tmp)
                             - (s IDsuspect_word_i)))%Q
    | 68%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 69%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 70%positive => ((1 # 1) + (s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | 71%positive => ((s IDsuspect_word_z)
                      + max0((s IDsuspect_word__tmp) - (s IDsuspect_word_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition suspect_word_hints (p : node) (s : state) := 
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
    | 14%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDsuspect_word__tmp)
                                                             - (s IDsuspect_word_i)) (-1
                                                                    + (s IDsuspect_word__tmp)
                                                                    - (s IDsuspect_word_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDsuspect_word__tmp)
                                            - (s IDsuspect_word_i))]
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*0 1*) F_max0_monotonic (F_check_ge ((s IDsuspect_word__tmp)
                                                            - (s IDsuspect_word_i)) (-1
                                                                    + (s IDsuspect_word__tmp)
                                                                    - (s IDsuspect_word_i)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDsuspect_word__tmp)
                                                                 - (s IDsuspect_word_i))) (F_check_ge (0) (0))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDsuspect_word__tmp)
                                                             - (s IDsuspect_word_i)) (-1
                                                                    + (s IDsuspect_word__tmp)
                                                                    - (s IDsuspect_word_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDsuspect_word__tmp)
                                            - (s IDsuspect_word_i))]
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDsuspect_word__tmp)
                                                             - (s IDsuspect_word_i)) (-1
                                                                    + (s IDsuspect_word__tmp)
                                                                    - (s IDsuspect_word_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDsuspect_word__tmp)
                                            - (s IDsuspect_word_i))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-1 0*) F_max0_pre_decrement ((s IDsuspect_word__tmp)
                                                     - (s IDsuspect_word_i)) (1)]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_max0_pre_decrement ((s IDsuspect_word__tmp)
                                                     - (s IDsuspect_word_i)) (1)]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_max0_pre_decrement ((s IDsuspect_word__tmp)
                                                     - (s IDsuspect_word_i)) (1)]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_max0_pre_decrement ((s IDsuspect_word__tmp)
                                                     - (s IDsuspect_word_i)) (1)]
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
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
    | _ => []
  end.


Theorem suspect_word_ai_correct:
  forall s p' s', steps (g_start suspect_word) s (g_edges suspect_word) p' s' -> suspect_word_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem suspect_word_pot_correct:
  forall s p' s',
    steps (g_start suspect_word) s (g_edges suspect_word) p' s' ->
    (suspect_word_pot (g_start suspect_word) s >= suspect_word_pot p' s')%Q.
Proof.
  check_lp suspect_word_ai_correct suspect_word_hints.
Qed.

