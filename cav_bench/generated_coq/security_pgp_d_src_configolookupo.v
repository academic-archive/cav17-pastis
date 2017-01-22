Require Import pasta.Pasta.

Notation IDlookup_z := 1%positive.
Notation IDlookup__tmp := 2%positive.
Notation IDlookup__tmp1 := 3%positive.
Notation IDlookup__tmp2 := 4%positive.
Notation IDlookup_index := 5%positive.
Notation IDlookup_noMatches := 6%positive.
Notation IDlookup_position := 7%positive.
Notation IDlookup_key := 8%positive.
Notation IDlookup_keyLength := 9%positive.
Notation IDlookup_keyWords := 10%positive.
Notation IDlookup_range := 11%positive.
Definition lookup : graph := {|
  g_start := 1%positive;
  g_end := 35%positive;
  g_edges := (1%positive,(AAssign IDlookup_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDlookup__tmp
             (Some (EVar IDlookup_keyLength))),3%positive)::
             (3%positive,(AAssign IDlookup__tmp1
             (Some (EVar IDlookup_range))),4%positive)::
             (4%positive,(AAssign IDlookup_position (Some (ENum (0)))),
             5%positive)::
             (5%positive,(AAssign IDlookup_noMatches (Some (ENum (0)))),
             6%positive)::
             (6%positive,(AAssign IDlookup_index (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDlookup_index) s) <
             (eval (EVar IDlookup__tmp) s))%Z)),42%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDlookup_index) s) >=
             (eval (EVar IDlookup__tmp) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AAssign IDlookup_index (Some (ENum (0)))),
             12%positive)::(12%positive,ANone,13%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDlookup_index) s) <
             (eval (EVar IDlookup__tmp1) s))%Z)),25%positive)::
             (14%positive,(AGuard (fun s => ((eval (EVar IDlookup_index)
             s) >= (eval (EVar IDlookup__tmp1) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,21%positive)::
             (16%positive,ANone,20%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDlookup__tmp2
             (Some (EVar IDlookup_position))),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,AWeaken,35%positive)::
             (20%positive,ANone,22%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDlookup__tmp2 (Some (ENum (-1)))),
             23%positive)::(23%positive,ANone,24%positive)::
             (24%positive,AWeaken,35%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,36%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,ANone,32%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,(AAssign IDlookup_position
             (Some (EVar IDlookup_index))),30%positive)::
             (30%positive,(AAssign IDlookup_noMatches
             (Some (EAdd (EVar IDlookup_noMatches) (ENum (1))))),31%positive)::
             (31%positive,ANone,36%positive)::
             (32%positive,(AAssign IDlookup__tmp2
             (Some (EVar IDlookup_index))),33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,AWeaken,35%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDlookup_index
             (Some (EAdd (EVar IDlookup_index) (ENum (1))))),38%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,(AAssign IDlookup_z (Some (EAdd (ENum (1))
             (EVar IDlookup_z)))),41%positive)::
             (41%positive,AWeaken,14%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,ANone,44%positive)::
             (44%positive,(AAssign IDlookup_index
             (Some (EAdd (EVar IDlookup_index) (ENum (1))))),45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,ANone,47%positive)::
             (47%positive,(AAssign IDlookup_z (Some (EAdd (ENum (1))
             (EVar IDlookup_z)))),48%positive)::
             (48%positive,AWeaken,9%positive)::nil
|}.

Definition lookup_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_z) <= 0)%Z
    | 3%positive => (-1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup_z) <= 0)%Z
    | 4%positive => (1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_z) <= 0)%Z
    | 5%positive => (-1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 6%positive => (-1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0)%Z
    | 7%positive => (-1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_index) <= 0)%Z
    | 8%positive => (-1 * (s IDlookup_index) <= 0 /\ 1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0)%Z
    | 9%positive => (-1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 10%positive => (-1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup__tmp)+ -1 * (s IDlookup_index) <= 0)%Z
    | 11%positive => (1 * (s IDlookup__tmp)+ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 12%positive => (-1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_index) <= 0)%Z
    | 13%positive => (-1 * (s IDlookup_index) <= 0 /\ 1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 14%positive => (-1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 15%positive => (-1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup__tmp1)+ -1 * (s IDlookup_index) <= 0)%Z
    | 16%positive => (1 * (s IDlookup__tmp1)+ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 17%positive => (-1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup__tmp1)+ -1 * (s IDlookup_index) <= 0)%Z
    | 18%positive => (1 * (s IDlookup__tmp1)+ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup__tmp2) <= 0)%Z
    | 19%positive => (-1 * (s IDlookup__tmp2) <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup__tmp1)+ -1 * (s IDlookup_index) <= 0)%Z
    | 20%positive => (-1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup__tmp1)+ -1 * (s IDlookup_index) <= 0)%Z
    | 21%positive => (-1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup__tmp1)+ -1 * (s IDlookup_index) <= 0)%Z
    | 22%positive => (1 * (s IDlookup__tmp1)+ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 23%positive => (-1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ 1 * (s IDlookup__tmp1)+ -1 * (s IDlookup_index) <= 0 /\ 1 * (s IDlookup__tmp2) + 1 <= 0 /\ -1 * (s IDlookup__tmp2) + -1 <= 0)%Z
    | 24%positive => (-1 * (s IDlookup__tmp2) + -1 <= 0 /\ 1 * (s IDlookup__tmp2) + 1 <= 0 /\ 1 * (s IDlookup__tmp1)+ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 25%positive => (-1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 27%positive => (-1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 29%positive => (-1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_position) + 1 <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 31%positive => (-1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_position) + 1 <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup_noMatches) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup__tmp2) + 1 <= 0 /\ -1 * (s IDlookup__tmp2) <= 0)%Z
    | 34%positive => (-1 * (s IDlookup__tmp2) <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup__tmp2) + 1 <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDlookup__tmp2) + -1 <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 36%positive => (-1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 37%positive => (-1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup_noMatches) <= 0)%Z
    | 38%positive => (-1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) <= 0)%Z
    | 39%positive => (-1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0)%Z
    | 40%positive => (-1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) <= 0)%Z
    | 41%positive => (-1 * (s IDlookup__tmp1)+ 1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_z) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup__tmp)+ 1 * (s IDlookup_index) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDlookup__tmp)+ 1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_position) <= 0)%Z
    | 44%positive => (-1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup__tmp)+ 1 * (s IDlookup_index) + 1 <= 0)%Z
    | 45%positive => (-1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup__tmp)+ 1 * (s IDlookup_index) <= 0)%Z
    | 46%positive => (-1 * (s IDlookup__tmp)+ 1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_z) <= 0)%Z
    | 47%positive => (-1 * (s IDlookup_z) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ -1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup__tmp)+ 1 * (s IDlookup_index) <= 0)%Z
    | 48%positive => (-1 * (s IDlookup__tmp)+ 1 * (s IDlookup_index) <= 0 /\ -1 * (s IDlookup_index) + 1 <= 0 /\ -1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_position) <= 0 /\ 1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_noMatches) <= 0 /\ -1 * (s IDlookup_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition lookup_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDlookup_keyLength)) + max0((s IDlookup_range)))%Q
    | 2%positive => ((s IDlookup_z) + max0((s IDlookup_keyLength))
                     + max0((s IDlookup_range)))%Q
    | 3%positive => ((s IDlookup_z) + max0((s IDlookup__tmp))
                     + max0((s IDlookup_range)))%Q
    | 4%positive => ((s IDlookup_z) + max0((s IDlookup__tmp))
                     + max0((s IDlookup__tmp1)))%Q
    | 5%positive => ((s IDlookup_z) + max0((s IDlookup__tmp))
                     + max0((s IDlookup__tmp1)))%Q
    | 6%positive => ((s IDlookup_z) + max0((s IDlookup__tmp))
                     + max0((s IDlookup__tmp1)))%Q
    | 7%positive => ((s IDlookup_z)
                     + max0((s IDlookup__tmp) - (s IDlookup_index))
                     + max0((s IDlookup__tmp1)))%Q
    | 8%positive => ((s IDlookup_z)
                     + max0((s IDlookup__tmp) - (s IDlookup_index))
                     + max0((s IDlookup__tmp1)))%Q
    | 9%positive => ((s IDlookup_z)
                     + max0((s IDlookup__tmp) - (s IDlookup_index))
                     + max0((s IDlookup__tmp1)))%Q
    | 10%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp) - (s IDlookup_index))
                      + max0((s IDlookup__tmp1)))%Q
    | 11%positive => ((s IDlookup_z) + max0((s IDlookup__tmp1)))%Q
    | 12%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 13%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 14%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 15%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 16%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 17%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 18%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 19%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 20%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 21%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 22%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 23%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 24%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 25%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 26%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 27%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 28%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 29%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 30%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 31%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 32%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 33%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 34%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 35%positive => ((s IDlookup_z))%Q
    | 36%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 37%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 38%positive => ((1 # 1) + (s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 39%positive => ((1 # 1) + (s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 40%positive => ((1 # 1) + (s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 41%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp1) - (s IDlookup_index)))%Q
    | 42%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp) - (s IDlookup_index))
                      + max0((s IDlookup__tmp1)))%Q
    | 43%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp) - (s IDlookup_index))
                      + max0((s IDlookup__tmp1)))%Q
    | 44%positive => ((1 # 1) + (s IDlookup_z)
                      + max0(-1 + (s IDlookup__tmp) - (s IDlookup_index))
                      + max0((s IDlookup__tmp1)))%Q
    | 45%positive => ((1 # 1) + (s IDlookup_z)
                      + max0((s IDlookup__tmp) - (s IDlookup_index))
                      + max0((s IDlookup__tmp1)))%Q
    | 46%positive => ((1 # 1) + (s IDlookup_z)
                      + max0((s IDlookup__tmp) - (s IDlookup_index))
                      + max0((s IDlookup__tmp1)))%Q
    | 47%positive => ((1 # 1) + (s IDlookup_z)
                      + max0((s IDlookup__tmp) - (s IDlookup_index))
                      + max0((s IDlookup__tmp1)))%Q
    | 48%positive => ((s IDlookup_z)
                      + max0((s IDlookup__tmp) - (s IDlookup_index))
                      + max0((s IDlookup__tmp1)))%Q
    | _ => (0 # 1)%Q
  end.

Definition lookup_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDlookup__tmp)
                                                             - (s IDlookup_index)) (-1
                                                                    + (s IDlookup__tmp)
                                                                    - (s IDlookup_index)));
                      (*0 1*) F_max0_ge_0 (-1 + (s IDlookup__tmp)
                                           - (s IDlookup_index))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDlookup__tmp1)
                                                             - (s IDlookup_index)) (-1
                                                                    + (s IDlookup__tmp1)
                                                                    - (s IDlookup_index)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDlookup__tmp1)
                                            - (s IDlookup_index))]
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDlookup__tmp1)
                                                             - (s IDlookup_index)) (-1
                                                                    + (s IDlookup__tmp1)
                                                                    - (s IDlookup_index)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDlookup__tmp1)
                                            - (s IDlookup_index))]
    | 25%positive => [(*-1 0*) F_max0_pre_decrement ((s IDlookup__tmp1)
                                                     - (s IDlookup_index)) (1)]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDlookup__tmp1)
                                            - (s IDlookup_index))]
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => [(*-1 0*) F_max0_pre_decrement ((s IDlookup__tmp)
                                                     - (s IDlookup_index)) (1)]
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | _ => []
  end.


Theorem lookup_ai_correct:
  forall s p' s', steps (g_start lookup) s (g_edges lookup) p' s' -> lookup_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem lookup_pot_correct:
  forall s p' s',
    steps (g_start lookup) s (g_edges lookup) p' s' ->
    (lookup_pot (g_start lookup) s >= lookup_pot p' s')%Q.
Proof.
  check_lp lookup_ai_correct lookup_hints.
Qed.

