Require Import pasta.Pasta.

Notation IDPutVbrTag_z := 1%positive.
Notation IDPutVbrTag__tmp := 2%positive.
Notation IDPutVbrTag__tmp1 := 3%positive.
Notation IDPutVbrTag__tmp2 := 4%positive.
Notation IDPutVbrTag_abyte := 5%positive.
Notation IDPutVbrTag_frameNum := 6%positive.
Notation IDPutVbrTag_i := 7%positive.
Notation IDPutVbrTag_lFileSize := 8%positive.
Notation IDPutVbrTag_nStreamIndex := 9%positive.
Notation IDPutVbrTag_nVbrNumFrames := 10%positive.
Notation IDPutVbrTag_nZeroStreamSize := 11%positive.
Notation IDPutVbrTag_lpszFileName := 12%positive.
Notation IDPutVbrTag_nVbrScale := 13%positive.
Notation IDPutVbrTag_nVersion := 14%positive.
Definition PutVbrTag : graph := {|
  g_start := 1%positive;
  g_end := 67%positive;
  g_edges := (1%positive,(AAssign IDPutVbrTag_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDPutVbrTag__tmp2
             (Some (EVar IDPutVbrTag_nVbrScale))),3%positive)::
             (3%positive,(AAssign IDPutVbrTag__tmp1
             (Some (EVar IDPutVbrTag_nVersion))),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDPutVbrTag_nVbrNumFrames) s) =
             (eval (ENum (0)) s))%Z)),63%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDPutVbrTag_nVbrNumFrames) s) <>
             (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,ANone,64%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,60%positive)::(9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDPutVbrTag_lFileSize None),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDPutVbrTag_lFileSize) s) =
             (eval (ENum (0)) s))%Z)),56%positive)::
             (12%positive,(AGuard
             (fun s => ((eval (EVar IDPutVbrTag_lFileSize) s) <>
             (eval (ENum (0)) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDPutVbrTag__tmp1)
             s) = (eval (ENum (0)) s))%Z)),18%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDPutVbrTag__tmp1)
             s) <> (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AAssign IDPutVbrTag_abyte None),17%positive)::
             (17%positive,ANone,21%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDPutVbrTag_abyte None),20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDPutVbrTag_i (Some (ENum (1)))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDPutVbrTag_i) s) <
             (eval (ENum (100)) s))%Z)),45%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDPutVbrTag_i) s) >=
             (eval (ENum (100)) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AAssign IDPutVbrTag_nStreamIndex
             (Some (EVar IDPutVbrTag_nZeroStreamSize))),27%positive)::
             (27%positive,(AAssign IDPutVbrTag_nStreamIndex
             (Some (EAdd (EVar IDPutVbrTag_nStreamIndex) (ENum (1))))),
             28%positive)::
             (28%positive,(AAssign IDPutVbrTag_nStreamIndex
             (Some (EAdd (EVar IDPutVbrTag_nStreamIndex) (ENum (1))))),
             29%positive)::
             (29%positive,(AAssign IDPutVbrTag_nStreamIndex
             (Some (EAdd (EVar IDPutVbrTag_nStreamIndex) (ENum (1))))),
             30%positive)::
             (30%positive,(AAssign IDPutVbrTag_nStreamIndex
             (Some (EAdd (EVar IDPutVbrTag_nStreamIndex) (ENum (1))))),
             31%positive)::
             (31%positive,(AAssign IDPutVbrTag_nStreamIndex
             (Some (EAdd (EVar IDPutVbrTag_nStreamIndex) (ENum (4))))),
             32%positive)::
             (32%positive,(AAssign IDPutVbrTag_nStreamIndex
             (Some (EAdd (EVar IDPutVbrTag_nStreamIndex) (ENum (4))))),
             33%positive)::
             (33%positive,(AAssign IDPutVbrTag_nStreamIndex
             (Some (EAdd (EVar IDPutVbrTag_nStreamIndex) (ENum (4))))),
             34%positive)::
             (34%positive,(AAssign IDPutVbrTag_nStreamIndex
             (Some (EAdd (EVar IDPutVbrTag_nStreamIndex) (ENum (100))))),
             35%positive)::
             (35%positive,(AAssign IDPutVbrTag_nStreamIndex
             (Some (EAdd (EVar IDPutVbrTag_nStreamIndex) (ENum (4))))),
             36%positive)::
             (36%positive,(AAssign IDPutVbrTag_nStreamIndex
             (Some (EAdd (EVar IDPutVbrTag_nStreamIndex) (ENum (20))))),
             37%positive)::(37%positive,AWeaken,38%positive)::
             (38%positive,ANone,42%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDPutVbrTag__tmp (Some (ENum (0)))),
             40%positive)::(40%positive,ANone,41%positive)::
             (41%positive,AWeaken,67%positive)::
             (42%positive,(AAssign IDPutVbrTag__tmp (Some (ENum (-1)))),
             43%positive)::(43%positive,ANone,44%positive)::
             (44%positive,AWeaken,67%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,(AAssign IDPutVbrTag_frameNum None),47%positive)::
             (47%positive,AWeaken,48%positive)::
             (48%positive,ANone,49%positive)::
             (48%positive,ANone,50%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,ANone,51%positive)::
             (51%positive,(AAssign IDPutVbrTag_i
             (Some (EAdd (EVar IDPutVbrTag_i) (ENum (1))))),52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,(AAssign IDPutVbrTag_z (Some (EAdd (ENum (1))
             (EVar IDPutVbrTag_z)))),55%positive)::
             (55%positive,AWeaken,24%positive)::
             (56%positive,AWeaken,57%positive)::
             (57%positive,(AAssign IDPutVbrTag__tmp (Some (ENum (-1)))),
             58%positive)::(58%positive,ANone,59%positive)::
             (59%positive,AWeaken,67%positive)::
             (60%positive,(AAssign IDPutVbrTag__tmp (Some (ENum (-1)))),
             61%positive)::(61%positive,ANone,62%positive)::
             (62%positive,AWeaken,67%positive)::
             (63%positive,AWeaken,64%positive)::
             (64%positive,(AAssign IDPutVbrTag__tmp (Some (ENum (-1)))),
             65%positive)::(65%positive,ANone,66%positive)::
             (66%positive,AWeaken,67%positive)::nil
|}.

Definition PutVbrTag_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 3%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0)%Z
    | 4%positive => (1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 5%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0)%Z
    | 6%positive => (1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 7%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0)%Z
    | 8%positive => (1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 9%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0)%Z
    | 10%positive => (1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 11%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0)%Z
    | 12%positive => (1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 13%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0)%Z
    | 14%positive => (1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 15%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0)%Z
    | 16%positive => (1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 17%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0)%Z
    | 18%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag__tmp1) <= 0 /\ -1 * (s IDPutVbrTag__tmp1) <= 0)%Z
    | 19%positive => (-1 * (s IDPutVbrTag__tmp1) <= 0 /\ 1 * (s IDPutVbrTag__tmp1) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 20%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag__tmp1) <= 0 /\ -1 * (s IDPutVbrTag__tmp1) <= 0)%Z
    | 21%positive => (1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 22%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -1 <= 0 /\ -1 * (s IDPutVbrTag_i) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDPutVbrTag_i) + 1 <= 0 /\ 1 * (s IDPutVbrTag_i) + -1 <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 24%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 1 <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0)%Z
    | 25%positive => (1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 100 <= 0)%Z
    | 26%positive => (-1 * (s IDPutVbrTag_i) + 100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0)%Z
    | 27%positive => (1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 100 <= 0)%Z
    | 28%positive => (-1 * (s IDPutVbrTag_i) + 100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0)%Z
    | 29%positive => (1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 100 <= 0)%Z
    | 30%positive => (-1 * (s IDPutVbrTag_i) + 100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0)%Z
    | 31%positive => (1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 100 <= 0)%Z
    | 32%positive => (-1 * (s IDPutVbrTag_i) + 100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0)%Z
    | 33%positive => (1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 100 <= 0)%Z
    | 34%positive => (-1 * (s IDPutVbrTag_i) + 100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0)%Z
    | 35%positive => (1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 100 <= 0)%Z
    | 36%positive => (-1 * (s IDPutVbrTag_i) + 100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0)%Z
    | 37%positive => (1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 100 <= 0)%Z
    | 38%positive => (-1 * (s IDPutVbrTag_i) + 100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0)%Z
    | 39%positive => (1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 100 <= 0)%Z
    | 40%positive => (-1 * (s IDPutVbrTag_i) + 100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0 /\ 1 * (s IDPutVbrTag__tmp) <= 0 /\ -1 * (s IDPutVbrTag__tmp) <= 0)%Z
    | 41%positive => (-1 * (s IDPutVbrTag__tmp) <= 0 /\ 1 * (s IDPutVbrTag__tmp) <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 100 <= 0)%Z
    | 42%positive => (1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 100 <= 0)%Z
    | 43%positive => (-1 * (s IDPutVbrTag_i) + 100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0 /\ 1 * (s IDPutVbrTag__tmp) + 1 <= 0 /\ -1 * (s IDPutVbrTag__tmp) + -1 <= 0)%Z
    | 44%positive => (-1 * (s IDPutVbrTag__tmp) + -1 <= 0 /\ 1 * (s IDPutVbrTag__tmp) + 1 <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 100 <= 0)%Z
    | 45%positive => (-1 * (s IDPutVbrTag_i) + 1 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -99 <= 0)%Z
    | 46%positive => (1 * (s IDPutVbrTag_i) + -99 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 1 <= 0)%Z
    | 47%positive => (-1 * (s IDPutVbrTag_i) + 1 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -99 <= 0)%Z
    | 48%positive => (1 * (s IDPutVbrTag_i) + -99 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 1 <= 0)%Z
    | 49%positive => (-1 * (s IDPutVbrTag_i) + 1 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -99 <= 0)%Z
    | 50%positive => (1 * (s IDPutVbrTag_i) + -99 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDPutVbrTag_i) + 1 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_i) + -99 <= 0)%Z
    | 52%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 2 <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0)%Z
    | 53%positive => (1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_i) + 2 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 54%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_i) + 2 <= 0 /\ 1 * (s IDPutVbrTag_i) + -100 <= 0)%Z
    | 55%positive => (1 * (s IDPutVbrTag_i) + -100 <= 0 /\ -1 * (s IDPutVbrTag_i) + 2 <= 0 /\ -1 * (s IDPutVbrTag_z) + 1 <= 0)%Z
    | 56%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_lFileSize) <= 0 /\ -1 * (s IDPutVbrTag_lFileSize) <= 0)%Z
    | 57%positive => (-1 * (s IDPutVbrTag_lFileSize) <= 0 /\ 1 * (s IDPutVbrTag_lFileSize) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 58%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_lFileSize) <= 0 /\ -1 * (s IDPutVbrTag_lFileSize) <= 0 /\ 1 * (s IDPutVbrTag__tmp) + 1 <= 0 /\ -1 * (s IDPutVbrTag__tmp) + -1 <= 0)%Z
    | 59%positive => (-1 * (s IDPutVbrTag__tmp) + -1 <= 0 /\ 1 * (s IDPutVbrTag__tmp) + 1 <= 0 /\ -1 * (s IDPutVbrTag_lFileSize) <= 0 /\ 1 * (s IDPutVbrTag_lFileSize) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 60%positive => (1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 61%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag__tmp) + 1 <= 0 /\ -1 * (s IDPutVbrTag__tmp) + -1 <= 0)%Z
    | 62%positive => (-1 * (s IDPutVbrTag__tmp) + -1 <= 0 /\ 1 * (s IDPutVbrTag__tmp) + 1 <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0)%Z
    | 63%positive => (1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_nVbrNumFrames) <= 0 /\ -1 * (s IDPutVbrTag_nVbrNumFrames) <= 0)%Z
    | 64%positive => (-1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0)%Z
    | 65%positive => (1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag__tmp) + 1 <= 0 /\ -1 * (s IDPutVbrTag__tmp) + -1 <= 0)%Z
    | 66%positive => (-1 * (s IDPutVbrTag__tmp) + -1 <= 0 /\ 1 * (s IDPutVbrTag__tmp) + 1 <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ 1 * (s IDPutVbrTag_z) <= 0)%Z
    | 67%positive => (1 * (s IDPutVbrTag__tmp) <= 0 /\ -1 * (s IDPutVbrTag_z) <= 0 /\ -1 * (s IDPutVbrTag__tmp) + -1 <= 0)%Z
    | _ => False
  end.

Definition PutVbrTag_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((99 # 1))%Q
    | 2%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 3%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 4%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 5%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 6%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 7%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 8%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 9%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 10%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 11%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 12%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 13%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 14%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 15%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 16%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 17%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 18%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 19%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 20%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 21%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 22%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 23%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 24%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 25%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 26%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 27%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 28%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 29%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 30%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 31%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 32%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 33%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 34%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 35%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 36%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 37%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 38%positive => ((s IDPutVbrTag_z) + max0(100 - (s IDPutVbrTag_i)))%Q
    | 39%positive => ((s IDPutVbrTag_z) + max0(100 - (s IDPutVbrTag_i)))%Q
    | 40%positive => ((s IDPutVbrTag_z) + max0(100 - (s IDPutVbrTag_i)))%Q
    | 41%positive => ((s IDPutVbrTag_z) + max0(100 - (s IDPutVbrTag_i)))%Q
    | 42%positive => ((s IDPutVbrTag_z) + max0(100 - (s IDPutVbrTag_i)))%Q
    | 43%positive => ((s IDPutVbrTag_z) + max0(100 - (s IDPutVbrTag_i)))%Q
    | 44%positive => ((s IDPutVbrTag_z) + max0(100 - (s IDPutVbrTag_i)))%Q
    | 45%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 46%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 47%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 48%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 49%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 50%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 51%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 52%positive => ((101 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 53%positive => ((101 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 54%positive => ((101 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 55%positive => ((100 # 1) - (s IDPutVbrTag_i) + (s IDPutVbrTag_z))%Q
    | 56%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 57%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 58%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 59%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 60%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 61%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 62%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 63%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 64%positive => ((99 # 1) + (s IDPutVbrTag_z))%Q
    | 65%positive => ((s IDPutVbrTag_z)
                      + (99 # 1) * max0(-(s IDPutVbrTag__tmp)))%Q
    | 66%positive => ((s IDPutVbrTag_z)
                      + (99 # 1) * max0(-(s IDPutVbrTag__tmp)))%Q
    | 67%positive => ((s IDPutVbrTag_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition PutVbrTag_hints (p : node) (s : state) := 
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
    | 37%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (100
                                                                    - (s IDPutVbrTag_i)) (0))) (F_max0_ge_0 (100
                                                                    - (s IDPutVbrTag_i)))]
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (100
                                                             - (s IDPutVbrTag_i)) (99
                                                                    - (s IDPutVbrTag_i)));
                      (*-1 0*) F_max0_ge_0 (99 - (s IDPutVbrTag_i))]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (100
                                                             - (s IDPutVbrTag_i)) (99
                                                                    - (s IDPutVbrTag_i)));
                      (*-1 0*) F_max0_ge_0 (99 - (s IDPutVbrTag_i))]
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
    | 59%positive => [(*-99 0*) F_one]
    | 60%positive => []
    | 61%positive => []
    | 62%positive => [(*-99 0*) F_one]
    | 63%positive => []
    | 64%positive => []
    | 65%positive => []
    | 66%positive => [(*-99 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDPutVbrTag__tmp))) (F_check_ge (0) (0))]
    | 67%positive => []
    | _ => []
  end.


Theorem PutVbrTag_ai_correct:
  forall s p' s', steps (g_start PutVbrTag) s (g_edges PutVbrTag) p' s' -> PutVbrTag_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem PutVbrTag_pot_correct:
  forall s p' s',
    steps (g_start PutVbrTag) s (g_edges PutVbrTag) p' s' ->
    (PutVbrTag_pot (g_start PutVbrTag) s >= PutVbrTag_pot p' s')%Q.
Proof.
  check_lp PutVbrTag_ai_correct PutVbrTag_hints.
Qed.

