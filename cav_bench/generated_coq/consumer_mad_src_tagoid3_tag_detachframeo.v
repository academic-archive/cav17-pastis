Require Import pasta.Pasta.

Notation IDid3_tag_detachframe_z := 1%positive.
Notation IDid3_tag_detachframe__tmp := 2%positive.
Notation IDid3_tag_detachframe_i := 3%positive.
Notation IDid3_tag_detachframe_tag_dref_off24 := 4%positive.
Notation IDid3_tag_detachframe_frame := 5%positive.
Notation IDid3_tag_detachframe_tag := 6%positive.
Definition id3_tag_detachframe : graph := {|
  g_start := 1%positive;
  g_end := 39%positive;
  g_edges := (1%positive,(AAssign IDid3_tag_detachframe_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_detachframe_tag_dref_off24)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_detachframe_i) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDid3_tag_detachframe_i (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_detachframe_i) s) <
             (eval (EVar IDid3_tag_detachframe_tag_dref_off24) s))%Z)),
             10%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_detachframe_i) s) >=
             (eval (EVar IDid3_tag_detachframe_tag_dref_off24) s))%Z)),
             9%positive)::(9%positive,AWeaken,20%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,ANone,18%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDid3_tag_detachframe_i
             (Some (EAdd (EVar IDid3_tag_detachframe_i) (ENum (1))))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDid3_tag_detachframe_z
             (Some (EAdd (ENum (1)) (EVar IDid3_tag_detachframe_z)))),
             17%positive)::(17%positive,AWeaken,8%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_detachframe_i) s) =
             (eval (EVar IDid3_tag_detachframe_tag_dref_off24) s))%Z)),
             35%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_detachframe_i) s) <>
             (eval (EVar IDid3_tag_detachframe_tag_dref_off24) s))%Z)),
             21%positive)::(21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDid3_tag_detachframe_tag_dref_off24
             (Some (EAdd (EVar IDid3_tag_detachframe_tag_dref_off24)
             (ENum (-1))))),23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDid3_tag_detachframe_i
             (Some (EAdd (EVar IDid3_tag_detachframe_i) (ENum (1))))),
             25%positive)::(25%positive,AWeaken,26%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_detachframe_i) s) <
             (eval (EVar IDid3_tag_detachframe_tag_dref_off24) s))%Z)),
             31%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDid3_tag_detachframe_i) s) >=
             (eval (EVar IDid3_tag_detachframe_tag_dref_off24) s))%Z)),
             27%positive)::(27%positive,AWeaken,28%positive)::
             (28%positive,(AAssign IDid3_tag_detachframe__tmp
             (Some (ENum (0)))),29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,AWeaken,39%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDid3_tag_detachframe_z
             (Some (EAdd (ENum (1)) (EVar IDid3_tag_detachframe_z)))),
             24%positive)::(35%positive,AWeaken,36%positive)::
             (36%positive,(AAssign IDid3_tag_detachframe__tmp
             (Some (ENum (-1)))),37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,AWeaken,39%positive)::nil
|}.

Definition id3_tag_detachframe_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0)%Z
    | 3%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ 1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0)%Z
    | 4%positive => (-1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0)%Z
    | 5%positive => (-1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0 /\ 1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0)%Z
    | 6%positive => (-1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0 /\ 1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0)%Z
    | 7%positive => (-1 * (s IDid3_tag_detachframe_i) <= 0 /\ 1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0 /\ 1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0)%Z
    | 8%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0)%Z
    | 9%positive => (1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i)+ 1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0)%Z
    | 10%positive => (-1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + 1 <= 0)%Z
    | 11%positive => (1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0)%Z
    | 12%positive => (-1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + 1 <= 0)%Z
    | 13%positive => (1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0)%Z
    | 14%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDid3_tag_detachframe_i) + 1 <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0)%Z
    | 16%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDid3_tag_detachframe_i) + 1 <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + 1 <= 0)%Z
    | 19%positive => (1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0)%Z
    | 20%positive => (1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0)%Z
    | 21%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + 1 <= 0)%Z
    | 22%positive => (1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0)%Z
    | 23%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0)%Z
    | 24%positive => (1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0)%Z
    | 25%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + -1 <= 0 /\ -1 * (s IDid3_tag_detachframe_i) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDid3_tag_detachframe_i) + 1 <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + -1 <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0)%Z
    | 27%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + -1 <= 0 /\ -1 * (s IDid3_tag_detachframe_i) + 1 <= 0 /\ -1 * (s IDid3_tag_detachframe_i)+ 1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0)%Z
    | 28%positive => (-1 * (s IDid3_tag_detachframe_i)+ 1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) + 1 <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + -1 <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0)%Z
    | 29%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + -1 <= 0 /\ -1 * (s IDid3_tag_detachframe_i) + 1 <= 0 /\ -1 * (s IDid3_tag_detachframe_i)+ 1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_detachframe__tmp) <= 0 /\ -1 * (s IDid3_tag_detachframe__tmp) <= 0)%Z
    | 30%positive => (-1 * (s IDid3_tag_detachframe__tmp) <= 0 /\ 1 * (s IDid3_tag_detachframe__tmp) <= 0 /\ -1 * (s IDid3_tag_detachframe_i)+ 1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) + 1 <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + -1 <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0)%Z
    | 31%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) + 1 <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + 1 <= 0)%Z
    | 32%positive => (1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_detachframe_i) + 1 <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0)%Z
    | 33%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) + 1 <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + 1 <= 0 /\ -1 * (s IDid3_tag_detachframe_i) + 1 <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0)%Z
    | 35%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_i)+ 1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0)%Z
    | 36%positive => (-1 * (s IDid3_tag_detachframe_i)+ 1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0)%Z
    | 37%positive => (-1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_i)+ 1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_detachframe__tmp) + 1 <= 0 /\ -1 * (s IDid3_tag_detachframe__tmp) + -1 <= 0)%Z
    | 38%positive => (-1 * (s IDid3_tag_detachframe__tmp) + -1 <= 0 /\ 1 * (s IDid3_tag_detachframe__tmp) + 1 <= 0 /\ -1 * (s IDid3_tag_detachframe_i)+ 1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ 1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0)%Z
    | 39%positive => (1 * (s IDid3_tag_detachframe_i)+ -1 * (s IDid3_tag_detachframe_tag_dref_off24) + -1 <= 0 /\ 1 * (s IDid3_tag_detachframe__tmp) <= 0 /\ -1 * (s IDid3_tag_detachframe_z) <= 0 /\ -1 * (s IDid3_tag_detachframe_i) <= 0 /\ -1 * (s IDid3_tag_detachframe_i)+ 1 * (s IDid3_tag_detachframe_tag_dref_off24) <= 0 /\ -1 * (s IDid3_tag_detachframe__tmp) + -1 <= 0)%Z
    | _ => False
  end.

Definition id3_tag_detachframe_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 2%positive => ((s IDid3_tag_detachframe_z)
                     + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 3%positive => ((s IDid3_tag_detachframe_z)
                     + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 4%positive => ((s IDid3_tag_detachframe_z)
                     + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 5%positive => ((s IDid3_tag_detachframe_z)
                     + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 6%positive => (-(s IDid3_tag_detachframe_i)
                     + (s IDid3_tag_detachframe_z)
                     + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 7%positive => (-(s IDid3_tag_detachframe_i)
                     + (s IDid3_tag_detachframe_z)
                     + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 8%positive => (-(s IDid3_tag_detachframe_i)
                     + (s IDid3_tag_detachframe_z)
                     + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 9%positive => (-(s IDid3_tag_detachframe_i)
                     + (s IDid3_tag_detachframe_z)
                     + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 10%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 11%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 12%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 13%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 14%positive => ((1 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 15%positive => ((1 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 16%positive => ((1 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 17%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 18%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 19%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 20%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 21%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 22%positive => ((1 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z))%Q
    | 23%positive => ((2 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z))%Q
    | 24%positive => ((2 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z))%Q
    | 25%positive => ((3 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z))%Q
    | 26%positive => ((3 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z))%Q
    | 27%positive => ((3 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z))%Q
    | 28%positive => ((3 # 1) - (3 # 1) * (s IDid3_tag_detachframe_i)
                      + (3 # 1) * (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z)
                      + (2 # 1) * max0((s IDid3_tag_detachframe_i)
                                       - (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 29%positive => ((3 # 1) - (3 # 1) * (s IDid3_tag_detachframe_i)
                      + (3 # 1) * (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z)
                      + (2 # 1) * max0((s IDid3_tag_detachframe_i)
                                       - (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 30%positive => ((3 # 1) - (3 # 1) * (s IDid3_tag_detachframe_i)
                      + (3 # 1) * (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z)
                      + (2 # 1) * max0((s IDid3_tag_detachframe_i)
                                       - (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 31%positive => ((3 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z))%Q
    | 32%positive => ((3 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z))%Q
    | 33%positive => ((3 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z))%Q
    | 34%positive => ((3 # 1) - (s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_tag_dref_off24)
                      + (s IDid3_tag_detachframe_z))%Q
    | 35%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 36%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 37%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 38%positive => (-(s IDid3_tag_detachframe_i)
                      + (s IDid3_tag_detachframe_z)
                      + max0(1 + (s IDid3_tag_detachframe_tag_dref_off24)))%Q
    | 39%positive => ((s IDid3_tag_detachframe_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition id3_tag_detachframe_hints (p : node) (s : state) := 
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
    | 21%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                  + (s IDid3_tag_detachframe_tag_dref_off24))) (F_check_ge (1
                                                                    + (s IDid3_tag_detachframe_tag_dref_off24)) (0))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-2 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDid3_tag_detachframe_i)
                                                                    - (s IDid3_tag_detachframe_tag_dref_off24)) (0))) (F_max0_ge_0 ((s IDid3_tag_detachframe_i)
                                                                    - (s IDid3_tag_detachframe_tag_dref_off24)))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDid3_tag_detachframe_i)
                                                                 - (s IDid3_tag_detachframe_tag_dref_off24))) (F_check_ge (0) (0));
                      (*-3 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDid3_tag_detachframe_i)
                                                                 + (s IDid3_tag_detachframe_tag_dref_off24))) (F_check_ge (0) (0));
                      (*-3 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDid3_tag_detachframe_i)
                                                                    + (s IDid3_tag_detachframe_tag_dref_off24)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDid3_tag_detachframe_i)
                                                                    + (s IDid3_tag_detachframe_tag_dref_off24)))]
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                   + 
                                                                   (s IDid3_tag_detachframe_tag_dref_off24))) (F_check_ge (1
                                                                    + (s IDid3_tag_detachframe_tag_dref_off24)) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                 - (s IDid3_tag_detachframe_i)
                                                                 + (s IDid3_tag_detachframe_tag_dref_off24))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - (s IDid3_tag_detachframe_i)
                                                                    + (s IDid3_tag_detachframe_tag_dref_off24)) (0))) (F_max0_ge_0 (1
                                                                    - (s IDid3_tag_detachframe_i)
                                                                    + (s IDid3_tag_detachframe_tag_dref_off24)))]
    | 39%positive => []
    | _ => []
  end.


Theorem id3_tag_detachframe_ai_correct:
  forall s p' s', steps (g_start id3_tag_detachframe) s (g_edges id3_tag_detachframe) p' s' -> id3_tag_detachframe_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem id3_tag_detachframe_pot_correct:
  forall s p' s',
    steps (g_start id3_tag_detachframe) s (g_edges id3_tag_detachframe) p' s' ->
    (id3_tag_detachframe_pot (g_start id3_tag_detachframe) s >= id3_tag_detachframe_pot p' s')%Q.
Proof.
  check_lp id3_tag_detachframe_ai_correct id3_tag_detachframe_hints.
Qed.

