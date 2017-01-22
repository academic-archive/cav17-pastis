Require Import pasta.Pasta.

Notation IDBZ2_hbAssignCodes_z := 1%positive.
Notation IDBZ2_hbAssignCodes__tmp := 2%positive.
Notation IDBZ2_hbAssignCodes__tmp1 := 3%positive.
Notation IDBZ2_hbAssignCodes__tmp2 := 4%positive.
Notation IDBZ2_hbAssignCodes_i := 5%positive.
Notation IDBZ2_hbAssignCodes_n := 6%positive.
Notation IDBZ2_hbAssignCodes_vec := 7%positive.
Notation IDBZ2_hbAssignCodes_alphaSize := 8%positive.
Notation IDBZ2_hbAssignCodes_code := 9%positive.
Notation IDBZ2_hbAssignCodes_length := 10%positive.
Notation IDBZ2_hbAssignCodes_maxLen := 11%positive.
Notation IDBZ2_hbAssignCodes_minLen := 12%positive.
Definition BZ2_hbAssignCodes : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDBZ2_hbAssignCodes_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDBZ2_hbAssignCodes__tmp2
             (Some (EVar IDBZ2_hbAssignCodes_minLen))),3%positive)::
             (3%positive,(AAssign IDBZ2_hbAssignCodes__tmp
             (Some (EVar IDBZ2_hbAssignCodes_maxLen))),4%positive)::
             (4%positive,(AAssign IDBZ2_hbAssignCodes__tmp1
             (Some (EVar IDBZ2_hbAssignCodes_alphaSize))),5%positive)::
             (5%positive,(AAssign IDBZ2_hbAssignCodes_vec (Some (ENum (0)))),
             6%positive)::
             (6%positive,(AAssign IDBZ2_hbAssignCodes_n
             (Some (EVar IDBZ2_hbAssignCodes__tmp2))),7%positive)::
             (7%positive,ANone,8%positive)::(8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_hbAssignCodes_n) s) <=
             (eval (EVar IDBZ2_hbAssignCodes__tmp) s))%Z)),12%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_hbAssignCodes_n) s) >
             (eval (EVar IDBZ2_hbAssignCodes__tmp) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDBZ2_hbAssignCodes_i (Some (ENum (0)))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_hbAssignCodes_i) s) <
             (eval (EVar IDBZ2_hbAssignCodes__tmp1) s))%Z)),25%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDBZ2_hbAssignCodes_i) s) >=
             (eval (EVar IDBZ2_hbAssignCodes__tmp1) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDBZ2_hbAssignCodes_vec None),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDBZ2_hbAssignCodes_n
             (Some (EAdd (EVar IDBZ2_hbAssignCodes_n) (ENum (1))))),
             21%positive)::(21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,(AAssign IDBZ2_hbAssignCodes_z
             (Some (EAdd (ENum (1)) (EVar IDBZ2_hbAssignCodes_z)))),
             24%positive)::(24%positive,AWeaken,9%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (26%positive,ANone,29%positive)::
             (27%positive,(AAssign IDBZ2_hbAssignCodes_vec
             (Some (EAdd (EVar IDBZ2_hbAssignCodes_vec) (ENum (1))))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDBZ2_hbAssignCodes_i
             (Some (EAdd (EVar IDBZ2_hbAssignCodes_i) (ENum (1))))),
             31%positive)::(31%positive,ANone,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDBZ2_hbAssignCodes_z
             (Some (EAdd (ENum (1)) (EVar IDBZ2_hbAssignCodes_z)))),
             34%positive)::(34%positive,AWeaken,16%positive)::nil
|}.

Definition BZ2_hbAssignCodes_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0)%Z
    | 3%positive => (-1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes_z) <= 0)%Z
    | 4%positive => (1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0)%Z
    | 5%positive => (-1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes_z) <= 0)%Z
    | 6%positive => (1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes_vec) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_vec) <= 0)%Z
    | 7%positive => (-1 * (s IDBZ2_hbAssignCodes_vec) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes_vec) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes_z) <= 0)%Z
    | 8%positive => (1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes_vec) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_vec) <= 0)%Z
    | 9%positive => (-1 * (s IDBZ2_hbAssignCodes_z) <= 0)%Z
    | 10%positive => (-1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes__tmp)+ -1 * (s IDBZ2_hbAssignCodes_n) + 1 <= 0)%Z
    | 11%positive => (1 * (s IDBZ2_hbAssignCodes__tmp)+ -1 * (s IDBZ2_hbAssignCodes_n) + 1 <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0)%Z
    | 12%positive => (-1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0)%Z
    | 13%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0)%Z
    | 14%positive => (-1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0)%Z
    | 15%positive => (-1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0)%Z
    | 16%positive => (-1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0)%Z
    | 17%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes__tmp1)+ -1 * (s IDBZ2_hbAssignCodes_i) <= 0)%Z
    | 18%positive => (1 * (s IDBZ2_hbAssignCodes__tmp1)+ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0)%Z
    | 19%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes__tmp1)+ -1 * (s IDBZ2_hbAssignCodes_i) <= 0)%Z
    | 20%positive => (1 * (s IDBZ2_hbAssignCodes__tmp1)+ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0)%Z
    | 21%positive => (-1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes__tmp1)+ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) + -1 <= 0)%Z
    | 22%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) + -1 <= 0 /\ 1 * (s IDBZ2_hbAssignCodes__tmp1)+ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0)%Z
    | 23%positive => (-1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ 1 * (s IDBZ2_hbAssignCodes__tmp1)+ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) + -1 <= 0)%Z
    | 24%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) + -1 <= 0 /\ 1 * (s IDBZ2_hbAssignCodes__tmp1)+ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) + 1 <= 0)%Z
    | 25%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp1)+ 1 * (s IDBZ2_hbAssignCodes_i) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp1)+ 1 * (s IDBZ2_hbAssignCodes_i) + 1 <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0)%Z
    | 27%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp1)+ 1 * (s IDBZ2_hbAssignCodes_i) + 1 <= 0)%Z
    | 28%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp1)+ 1 * (s IDBZ2_hbAssignCodes_i) + 1 <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0)%Z
    | 29%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp1)+ 1 * (s IDBZ2_hbAssignCodes_i) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp1)+ 1 * (s IDBZ2_hbAssignCodes_i) + 1 <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0)%Z
    | 31%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp1)+ 1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDBZ2_hbAssignCodes_i) + 1 <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp1)+ 1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0)%Z
    | 33%positive => (-1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp1)+ 1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_i) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDBZ2_hbAssignCodes_i) + 1 <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp1)+ 1 * (s IDBZ2_hbAssignCodes_i) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes__tmp)+ 1 * (s IDBZ2_hbAssignCodes_n) <= 0 /\ -1 * (s IDBZ2_hbAssignCodes_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition BZ2_hbAssignCodes_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(1 + (s IDBZ2_hbAssignCodes_maxLen)
                          - (s IDBZ2_hbAssignCodes_minLen))
                     + max0(1 + (s IDBZ2_hbAssignCodes_maxLen)
                            - (s IDBZ2_hbAssignCodes_minLen)) * max0((s IDBZ2_hbAssignCodes_alphaSize)))%Q
    | 2%positive => ((s IDBZ2_hbAssignCodes_z)
                     + max0(1 + (s IDBZ2_hbAssignCodes_maxLen)
                            - (s IDBZ2_hbAssignCodes_minLen))
                     + max0(1 + (s IDBZ2_hbAssignCodes_maxLen)
                            - (s IDBZ2_hbAssignCodes_minLen)) * max0((s IDBZ2_hbAssignCodes_alphaSize)))%Q
    | 3%positive => ((s IDBZ2_hbAssignCodes_z)
                     + max0(1 - (s IDBZ2_hbAssignCodes__tmp2)
                            + (s IDBZ2_hbAssignCodes_maxLen))
                     + max0(1 - (s IDBZ2_hbAssignCodes__tmp2)
                            + (s IDBZ2_hbAssignCodes_maxLen)) * max0((s IDBZ2_hbAssignCodes_alphaSize)))%Q
    | 4%positive => ((s IDBZ2_hbAssignCodes_z)
                     + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                            - (s IDBZ2_hbAssignCodes__tmp2))
                     + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                            - (s IDBZ2_hbAssignCodes__tmp2)) * max0((s IDBZ2_hbAssignCodes_alphaSize)))%Q
    | 5%positive => ((s IDBZ2_hbAssignCodes_z)
                     + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                            - (s IDBZ2_hbAssignCodes__tmp2))
                     + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                            - (s IDBZ2_hbAssignCodes__tmp2)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 6%positive => ((s IDBZ2_hbAssignCodes_z)
                     + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                            - (s IDBZ2_hbAssignCodes__tmp2))
                     + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                            - (s IDBZ2_hbAssignCodes__tmp2)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 7%positive => ((s IDBZ2_hbAssignCodes_z)
                     + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                            - (s IDBZ2_hbAssignCodes_n))
                     + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                            - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 8%positive => ((s IDBZ2_hbAssignCodes_z)
                     + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                            - (s IDBZ2_hbAssignCodes_n))
                     + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                            - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 9%positive => ((s IDBZ2_hbAssignCodes_z)
                     + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                            - (s IDBZ2_hbAssignCodes_n))
                     + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                            - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 10%positive => ((s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 11%positive => ((s IDBZ2_hbAssignCodes_z))%Q
    | 12%positive => ((s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 13%positive => ((s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 14%positive => ((s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      - max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i)))%Q
    | 15%positive => ((s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      - max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i)))%Q
    | 16%positive => ((s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      - max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i)))%Q
    | 17%positive => ((s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      - max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i)))%Q
    | 18%positive => ((1 # 1)
                      + (s IDBZ2_hbAssignCodes__tmp) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                            - (s IDBZ2_hbAssignCodes_i))
                      - (s IDBZ2_hbAssignCodes_n) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                         - (s IDBZ2_hbAssignCodes_i))
                      + (s IDBZ2_hbAssignCodes_z)
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      - max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i)))%Q
    | 19%positive => ((1 # 1)
                      + (s IDBZ2_hbAssignCodes__tmp) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                            - (s IDBZ2_hbAssignCodes_i))
                      - (s IDBZ2_hbAssignCodes_n) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                         - (s IDBZ2_hbAssignCodes_i))
                      + (s IDBZ2_hbAssignCodes_z)
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      - max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i)))%Q
    | 20%positive => ((1 # 1)
                      + (s IDBZ2_hbAssignCodes__tmp) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                            - (s IDBZ2_hbAssignCodes_i))
                      - (s IDBZ2_hbAssignCodes_n) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                         - (s IDBZ2_hbAssignCodes_i))
                      + (s IDBZ2_hbAssignCodes_z)
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      - max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i)))%Q
    | 21%positive => ((1 # 1)
                      + (s IDBZ2_hbAssignCodes__tmp) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                            - (s IDBZ2_hbAssignCodes_i))
                      - (s IDBZ2_hbAssignCodes_n) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                         - (s IDBZ2_hbAssignCodes_i))
                      + (s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      - max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i))
                      + max0((s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i)))%Q
    | 22%positive => ((1 # 1)
                      + (s IDBZ2_hbAssignCodes__tmp) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                            - (s IDBZ2_hbAssignCodes_i))
                      - (s IDBZ2_hbAssignCodes_n) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                         - (s IDBZ2_hbAssignCodes_i))
                      + (s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      - max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i))
                      + max0((s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i)))%Q
    | 23%positive => ((1 # 1)
                      + (s IDBZ2_hbAssignCodes__tmp) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                            - (s IDBZ2_hbAssignCodes_i))
                      - (s IDBZ2_hbAssignCodes_n) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                         - (s IDBZ2_hbAssignCodes_i))
                      + (s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      - max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i))
                      + max0((s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i)))%Q
    | 24%positive => ((s IDBZ2_hbAssignCodes__tmp) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                          - (s IDBZ2_hbAssignCodes_i))
                      - (s IDBZ2_hbAssignCodes_n) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                         - (s IDBZ2_hbAssignCodes_i))
                      + (s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      - max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i))
                      + max0((s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i)))%Q
    | 25%positive => ((s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      - max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)
                                                                 - (s IDBZ2_hbAssignCodes_i)))%Q
    | 26%positive => ((1 # 1) + (s IDBZ2_hbAssignCodes_z)
                      + max0(-1 + (s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 27%positive => ((1 # 1) + (s IDBZ2_hbAssignCodes_z)
                      + max0(-1 + (s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 28%positive => ((1 # 1) + (s IDBZ2_hbAssignCodes_z)
                      + max0(-1 + (s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 29%positive => ((1 # 1) + (s IDBZ2_hbAssignCodes_z)
                      + max0(-1 + (s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 30%positive => ((1 # 1) + (s IDBZ2_hbAssignCodes_z)
                      + max0(-1 + (s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i))
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1)))%Q
    | 31%positive => ((1 # 1) + (s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      + max0((s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i)))%Q
    | 32%positive => ((1 # 1) + (s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      + max0((s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i)))%Q
    | 33%positive => ((1 # 1) + (s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      + max0((s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i)))%Q
    | 34%positive => ((s IDBZ2_hbAssignCodes_z)
                      + max0(1 + (s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n))
                      + max0((s IDBZ2_hbAssignCodes__tmp)
                             - (s IDBZ2_hbAssignCodes_n)) * max0((s IDBZ2_hbAssignCodes__tmp1))
                      + max0((s IDBZ2_hbAssignCodes__tmp1)
                             - (s IDBZ2_hbAssignCodes_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition BZ2_hbAssignCodes_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                             + (s IDBZ2_hbAssignCodes__tmp)
                                                             - (s IDBZ2_hbAssignCodes_n)) ((s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n)));
                      (*-1 0*) F_max0_ge_0 ((s IDBZ2_hbAssignCodes__tmp)
                                            - (s IDBZ2_hbAssignCodes_n));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + (s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDBZ2_hbAssignCodes__tmp1))) (F_check_ge (0) (0)))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                     + (s IDBZ2_hbAssignCodes__tmp)
                                                     - (s IDBZ2_hbAssignCodes_n)) (1);
                      (*-1 0*) F_max0_ge_0 ((s IDBZ2_hbAssignCodes__tmp1)
                                            - (s IDBZ2_hbAssignCodes_i));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n))) (F_check_ge (1
                                                                    + (s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDBZ2_hbAssignCodes__tmp1)
                                                                    - (s IDBZ2_hbAssignCodes_i))) (F_check_ge (0) (0)))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDBZ2_hbAssignCodes__tmp1)
                                                                    - (s IDBZ2_hbAssignCodes_i))) (F_check_ge (0) (0)))]
    | 25%positive => [(*-1 0*) F_max0_pre_decrement ((s IDBZ2_hbAssignCodes__tmp1)
                                                     - (s IDBZ2_hbAssignCodes_i)) (1);
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + (s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n))) (F_check_ge (1
                                                                    + (s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDBZ2_hbAssignCodes__tmp1)
                                                                    - (s IDBZ2_hbAssignCodes_i))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n)) (0))) (F_max0_ge_0 ((s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDBZ2_hbAssignCodes__tmp1)
                                                                    - (s IDBZ2_hbAssignCodes_i))) (F_check_ge (0) (0)))]
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + (s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n)) (0))) (F_max0_ge_0 (1
                                                                    + (s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDBZ2_hbAssignCodes__tmp1)
                                                                    - (s IDBZ2_hbAssignCodes_i))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n))) (F_check_ge ((s IDBZ2_hbAssignCodes__tmp)
                                                                    - (s IDBZ2_hbAssignCodes_n)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDBZ2_hbAssignCodes__tmp1)
                                                                    - (s IDBZ2_hbAssignCodes_i))) (F_check_ge (0) (0)))]
    | _ => []
  end.


Theorem BZ2_hbAssignCodes_ai_correct:
  forall s p' s', steps (g_start BZ2_hbAssignCodes) s (g_edges BZ2_hbAssignCodes) p' s' -> BZ2_hbAssignCodes_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem BZ2_hbAssignCodes_pot_correct:
  forall s p' s',
    steps (g_start BZ2_hbAssignCodes) s (g_edges BZ2_hbAssignCodes) p' s' ->
    (BZ2_hbAssignCodes_pot (g_start BZ2_hbAssignCodes) s >= BZ2_hbAssignCodes_pot p' s')%Q.
Proof.
  check_lp BZ2_hbAssignCodes_ai_correct BZ2_hbAssignCodes_hints.
Qed.

