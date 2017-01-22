Require Import pasta.Pasta.

Notation IDref_param_read_commit_z := 1%positive.
Notation IDref_param_read_commit__tmp := 2%positive.
Notation IDref_param_read_commit_ecode := 3%positive.
Notation IDref_param_read_commit_i := 4%positive.
Notation IDref_param_read_commit_plist_dref_off48 := 5%positive.
Notation IDref_param_read_commit_plist_dref_off8_off24 := 6%positive.
Notation IDref_param_read_commit_plist := 7%positive.
Definition ref_param_read_commit : graph := {|
  g_start := 1%positive;
  g_end := 21%positive;
  g_edges := (1%positive,(AAssign IDref_param_read_commit_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDref_param_read_commit_plist_dref_off48)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDref_param_read_commit_i) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDref_param_read_commit_ecode
             (Some (ENum (0)))),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDref_param_read_commit_plist_dref_off8_off24)
             s) <> (eval (ENum (0)) s))%Z)),12%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDref_param_read_commit_plist_dref_off8_off24)
             s) = (eval (ENum (0)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AAssign IDref_param_read_commit__tmp
             (Some (ENum (0)))),10%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,21%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDref_param_read_commit_i
             (Some (ENum (0)))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDref_param_read_commit_i) s) <
             (eval (EVar IDref_param_read_commit_plist_dref_off48) s))%Z)),
             22%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDref_param_read_commit_i) s) >=
             (eval (EVar IDref_param_read_commit_plist_dref_off48) s))%Z)),
             17%positive)::(17%positive,AWeaken,18%positive)::
             (18%positive,(AAssign IDref_param_read_commit__tmp
             (Some (EVar IDref_param_read_commit_ecode))),19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,AWeaken,21%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (23%positive,ANone,26%positive)::
             (24%positive,(AAssign IDref_param_read_commit_ecode
             (Some (ENum (-21)))),25%positive)::
             (25%positive,ANone,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDref_param_read_commit_i
             (Some (EAdd (EVar IDref_param_read_commit_i) (ENum (1))))),
             28%positive)::(28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDref_param_read_commit_z
             (Some (EAdd (ENum (1)) (EVar IDref_param_read_commit_z)))),
             31%positive)::(31%positive,AWeaken,16%positive)::nil
|}.

Definition ref_param_read_commit_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0)%Z
    | 3%positive => (-1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0)%Z
    | 4%positive => (-1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0)%Z
    | 5%positive => (-1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0)%Z
    | 6%positive => (-1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_ecode) <= 0)%Z
    | 7%positive => (-1 * (s IDref_param_read_commit_ecode) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0)%Z
    | 8%positive => (-1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_ecode) <= 0 /\ 1 * (s IDref_param_read_commit_plist_dref_off8_off24) <= 0 /\ -1 * (s IDref_param_read_commit_plist_dref_off8_off24) <= 0)%Z
    | 9%positive => (-1 * (s IDref_param_read_commit_plist_dref_off8_off24) <= 0 /\ 1 * (s IDref_param_read_commit_plist_dref_off8_off24) <= 0 /\ -1 * (s IDref_param_read_commit_ecode) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0)%Z
    | 10%positive => (-1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_ecode) <= 0 /\ 1 * (s IDref_param_read_commit_plist_dref_off8_off24) <= 0 /\ -1 * (s IDref_param_read_commit_plist_dref_off8_off24) <= 0 /\ 1 * (s IDref_param_read_commit__tmp) <= 0 /\ -1 * (s IDref_param_read_commit__tmp) <= 0)%Z
    | 11%positive => (-1 * (s IDref_param_read_commit__tmp) <= 0 /\ 1 * (s IDref_param_read_commit__tmp) <= 0 /\ -1 * (s IDref_param_read_commit_plist_dref_off8_off24) <= 0 /\ 1 * (s IDref_param_read_commit_plist_dref_off8_off24) <= 0 /\ -1 * (s IDref_param_read_commit_ecode) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0)%Z
    | 12%positive => (-1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_ecode) <= 0)%Z
    | 13%positive => (-1 * (s IDref_param_read_commit_ecode) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0)%Z
    | 14%positive => (-1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_ecode) <= 0 /\ 1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0)%Z
    | 15%positive => (-1 * (s IDref_param_read_commit_i) <= 0 /\ 1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_ecode) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0)%Z
    | 16%positive => (-1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ 1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0)%Z
    | 17%positive => (1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i)+ 1 * (s IDref_param_read_commit_plist_dref_off48) <= 0)%Z
    | 18%positive => (-1 * (s IDref_param_read_commit_i)+ 1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ 1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0)%Z
    | 19%positive => (1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i)+ 1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ 1 * (s IDref_param_read_commit__tmp) <= 0)%Z
    | 20%positive => (1 * (s IDref_param_read_commit__tmp) <= 0 /\ -1 * (s IDref_param_read_commit_i)+ 1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ 1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0)%Z
    | 21%positive => (-1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit__tmp) <= 0)%Z
    | 22%positive => (1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) + 1 <= 0)%Z
    | 23%positive => (1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) + 1 <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0)%Z
    | 24%positive => (1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) + 1 <= 0)%Z
    | 25%positive => (1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) + 1 <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) + 21 <= 0 /\ -1 * (s IDref_param_read_commit_ecode) + -21 <= 0)%Z
    | 26%positive => (1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) + 1 <= 0)%Z
    | 27%positive => (1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) + 1 <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ -1 * (s IDref_param_read_commit_i) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0)%Z
    | 28%positive => (1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ -1 * (s IDref_param_read_commit_i) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDref_param_read_commit_i) + 1 <= 0 /\ 1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0)%Z
    | 30%positive => (1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_z) <= 0 /\ 1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ -1 * (s IDref_param_read_commit_i) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDref_param_read_commit_i) + 1 <= 0 /\ 1 * (s IDref_param_read_commit_i)+ -1 * (s IDref_param_read_commit_plist_dref_off48) <= 0 /\ 1 * (s IDref_param_read_commit_ecode) <= 0 /\ -1 * (s IDref_param_read_commit_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition ref_param_read_commit_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 2%positive => ((s IDref_param_read_commit_z)
                     + max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 3%positive => ((s IDref_param_read_commit_z)
                     + max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 4%positive => ((s IDref_param_read_commit_z)
                     + max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 5%positive => ((s IDref_param_read_commit_z)
                     + max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 6%positive => ((s IDref_param_read_commit_z)
                     + max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 7%positive => ((s IDref_param_read_commit_z)
                     + max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 8%positive => ((s IDref_param_read_commit_z)
                     + max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 9%positive => ((s IDref_param_read_commit_z)
                     + max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 10%positive => ((s IDref_param_read_commit_z)
                      + max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 11%positive => ((s IDref_param_read_commit_z)
                      + max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 12%positive => ((s IDref_param_read_commit_z)
                      + max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 13%positive => ((s IDref_param_read_commit_z)
                      + max0((s IDref_param_read_commit_plist_dref_off48)))%Q
    | 14%positive => ((s IDref_param_read_commit_z)
                      + max0(-(s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 15%positive => ((s IDref_param_read_commit_z)
                      + max0(-(s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 16%positive => ((s IDref_param_read_commit_z)
                      + max0(-(s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 17%positive => ((s IDref_param_read_commit_z)
                      + max0(-(s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 18%positive => ((s IDref_param_read_commit_z)
                      + max0(-(s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 19%positive => ((s IDref_param_read_commit_z)
                      + max0(-(s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 20%positive => ((s IDref_param_read_commit_z)
                      + max0(-(s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 21%positive => ((s IDref_param_read_commit_z))%Q
    | 22%positive => ((s IDref_param_read_commit_z)
                      + max0(-(s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 23%positive => ((1 # 1) + (s IDref_param_read_commit_z)
                      + max0(-1 - (s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 24%positive => ((1 # 1) + (s IDref_param_read_commit_z)
                      + max0(-1 - (s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 25%positive => ((1 # 1) + (s IDref_param_read_commit_z)
                      + max0(-1 - (s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 26%positive => ((1 # 1) + (s IDref_param_read_commit_z)
                      + max0(-1 - (s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 27%positive => ((1 # 1) + (s IDref_param_read_commit_z)
                      + max0(-1 - (s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 28%positive => ((1 # 1) + (s IDref_param_read_commit_z)
                      + max0(-(s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 29%positive => ((1 # 1) + (s IDref_param_read_commit_z)
                      + max0(-(s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 30%positive => ((1 # 1) + (s IDref_param_read_commit_z)
                      + max0(-(s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | 31%positive => ((s IDref_param_read_commit_z)
                      + max0(-(s IDref_param_read_commit_i)
                             + (s IDref_param_read_commit_plist_dref_off48)))%Q
    | _ => (0 # 1)%Q
  end.

Definition ref_param_read_commit_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_ge_0 ((s IDref_param_read_commit_plist_dref_off48))]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (-(s IDref_param_read_commit_i)
                                                             + (s IDref_param_read_commit_plist_dref_off48)) (-1
                                                                    - (s IDref_param_read_commit_i)
                                                                    + (s IDref_param_read_commit_plist_dref_off48)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            - (s IDref_param_read_commit_i)
                                            + (s IDref_param_read_commit_plist_dref_off48))]
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_max0_pre_decrement (-(s IDref_param_read_commit_i)
                                                     + (s IDref_param_read_commit_plist_dref_off48)) (1)]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | _ => []
  end.


Theorem ref_param_read_commit_ai_correct:
  forall s p' s', steps (g_start ref_param_read_commit) s (g_edges ref_param_read_commit) p' s' -> ref_param_read_commit_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem ref_param_read_commit_pot_correct:
  forall s p' s',
    steps (g_start ref_param_read_commit) s (g_edges ref_param_read_commit) p' s' ->
    (ref_param_read_commit_pot (g_start ref_param_read_commit) s >= ref_param_read_commit_pot p' s')%Q.
Proof.
  check_lp ref_param_read_commit_ai_correct ref_param_read_commit_hints.
Qed.

