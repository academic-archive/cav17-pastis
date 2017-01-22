Require Import pasta.Pasta.

Notation IDBF_ofb64_encrypt_z := 1%positive.
Notation IDBF_ofb64_encrypt__tmp := 2%positive.
Notation IDBF_ofb64_encrypt_l := 3%positive.
Notation IDBF_ofb64_encrypt_n := 4%positive.
Notation IDBF_ofb64_encrypt_num_dref := 5%positive.
Notation IDBF_ofb64_encrypt_save := 6%positive.
Notation IDBF_ofb64_encrypt_t := 7%positive.
Notation IDBF_ofb64_encrypt_v0 := 8%positive.
Notation IDBF_ofb64_encrypt_v1 := 9%positive.
Notation IDBF_ofb64_encrypt_in := 10%positive.
Notation IDBF_ofb64_encrypt_ivec := 11%positive.
Notation IDBF_ofb64_encrypt_length := 12%positive.
Notation IDBF_ofb64_encrypt_num := 13%positive.
Notation IDBF_ofb64_encrypt_out := 14%positive.
Notation IDBF_ofb64_encrypt_schedule := 15%positive.
Definition BF_ofb64_encrypt : graph := {|
  g_start := 1%positive;
  g_end := 30%positive;
  g_edges := (1%positive,(AAssign IDBF_ofb64_encrypt_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDBF_ofb64_encrypt__tmp
             (Some (EVar IDBF_ofb64_encrypt_length))),3%positive)::
             (3%positive,(AAssign IDBF_ofb64_encrypt_n
             (Some (EVar IDBF_ofb64_encrypt_num_dref))),4%positive)::
             (4%positive,(AAssign IDBF_ofb64_encrypt_l
             (Some (EVar IDBF_ofb64_encrypt__tmp))),5%positive)::
             (5%positive,(AAssign IDBF_ofb64_encrypt_save (Some (ENum (0)))),
             6%positive)::
             (6%positive,(AAssign IDBF_ofb64_encrypt_v0 None),7%positive)::
             (7%positive,(AAssign IDBF_ofb64_encrypt_v0 None),8%positive)::
             (8%positive,(AAssign IDBF_ofb64_encrypt_v0 None),9%positive)::
             (9%positive,(AAssign IDBF_ofb64_encrypt_v0 None),10%positive)::
             (10%positive,(AAssign IDBF_ofb64_encrypt_v1 None),11%positive)::
             (11%positive,(AAssign IDBF_ofb64_encrypt_v1 None),12%positive)::
             (12%positive,(AAssign IDBF_ofb64_encrypt_v1 None),13%positive)::
             (13%positive,(AAssign IDBF_ofb64_encrypt_v1 None),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDBF_ofb64_encrypt_l
             (Some (EAdd (EVar IDBF_ofb64_encrypt_l) (ENum (-1))))),
             16%positive)::(16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDBF_ofb64_encrypt_l) s) <>
             (eval (ENum (0)) s))%Z)),31%positive)::
             (17%positive,(AGuard
             (fun s => ((eval (EVar IDBF_ofb64_encrypt_l) s) =
             (eval (ENum (0)) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDBF_ofb64_encrypt_save) s) <>
             (eval (ENum (0)) s))%Z)),21%positive)::
             (19%positive,(AGuard
             (fun s => ((eval (EVar IDBF_ofb64_encrypt_save) s) =
             (eval (ENum (0)) s))%Z)),20%positive)::
             (20%positive,AWeaken,25%positive)::
             (21%positive,AWeaken,22%positive)::
             (22%positive,(AAssign IDBF_ofb64_encrypt_v0 None),23%positive)::
             (23%positive,(AAssign IDBF_ofb64_encrypt_v1 None),24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDBF_ofb64_encrypt_v1 (Some (ENum (0)))),
             26%positive)::
             (26%positive,(AAssign IDBF_ofb64_encrypt_v0 (Some (ENum (0)))),
             27%positive)::
             (27%positive,(AAssign IDBF_ofb64_encrypt_t (Some (ENum (0)))),
             28%positive)::
             (28%positive,(AAssign IDBF_ofb64_encrypt_num_dref
             (Some (EVar IDBF_ofb64_encrypt_n))),29%positive)::
             (29%positive,AWeaken,30%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDBF_ofb64_encrypt_n) s) =
             (eval (ENum (0)) s))%Z)),34%positive)::
             (32%positive,(AGuard
             (fun s => ((eval (EVar IDBF_ofb64_encrypt_n) s) <>
             (eval (ENum (0)) s))%Z)),33%positive)::
             (33%positive,AWeaken,39%positive)::
             (34%positive,AWeaken,35%positive)::
             (35%positive,(AAssign IDBF_ofb64_encrypt_t None),36%positive)::
             (36%positive,(AAssign IDBF_ofb64_encrypt_t None),37%positive)::
             (37%positive,(AAssign IDBF_ofb64_encrypt_save
             (Some (EAdd (EVar IDBF_ofb64_encrypt_save) (ENum (1))))),
             38%positive)::(38%positive,ANone,39%positive)::
             (39%positive,(AAssign IDBF_ofb64_encrypt_n None),40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,(AAssign IDBF_ofb64_encrypt_z
             (Some (EAdd (ENum (1)) (EVar IDBF_ofb64_encrypt_z)))),
             15%positive)::nil
|}.

Definition BF_ofb64_encrypt_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 3%positive => (-1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 4%positive => (1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 5%positive => (-1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 6%positive => (1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 7%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 8%positive => (1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 9%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 10%positive => (1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 11%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 12%positive => (1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 13%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 14%positive => (1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 15%positive => (-1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 16%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 17%positive => (-1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 18%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_l) <= 0)%Z
    | 19%positive => (-1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 20%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 21%positive => (-1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDBF_ofb64_encrypt_save) + 1 <= 0 /\ -1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 23%positive => (-1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDBF_ofb64_encrypt_save) + 1 <= 0 /\ -1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 25%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_l) <= 0)%Z
    | 26%positive => (-1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_v1) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_v1) <= 0)%Z
    | 27%positive => (-1 * (s IDBF_ofb64_encrypt_v1) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_v1) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_v0) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_v0) <= 0)%Z
    | 28%positive => (-1 * (s IDBF_ofb64_encrypt_v0) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_v0) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_v1) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_v1) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_t) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_t) <= 0)%Z
    | 29%positive => (-1 * (s IDBF_ofb64_encrypt_t) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_t) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_v1) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_v1) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_v0) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_v0) <= 0)%Z
    | 30%positive => (-1 * (s IDBF_ofb64_encrypt_v0) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_v0) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_l) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_v1) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_v1) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_t) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_t) <= 0)%Z
    | 31%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 32%positive => (-1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 33%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 34%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_n) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_n) <= 0)%Z
    | 35%positive => (-1 * (s IDBF_ofb64_encrypt_n) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_n) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 36%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_n) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_n) <= 0)%Z
    | 37%positive => (-1 * (s IDBF_ofb64_encrypt_n) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_n) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 38%positive => (-1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ 1 * (s IDBF_ofb64_encrypt_n) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_n) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 40%positive => (-1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | 41%positive => (-1 * (s IDBF_ofb64_encrypt_save) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_z) <= 0)%Z
    | 42%positive => (-1 * (s IDBF_ofb64_encrypt_z) <= 0 /\ -1 * (s IDBF_ofb64_encrypt_save) <= 0)%Z
    | _ => False
  end.

Definition BF_ofb64_encrypt_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((s IDBF_ofb64_encrypt_length))%Q
    | 2%positive => ((s IDBF_ofb64_encrypt_length) + (s IDBF_ofb64_encrypt_z))%Q
    | 3%positive => ((s IDBF_ofb64_encrypt__tmp) + (s IDBF_ofb64_encrypt_z))%Q
    | 4%positive => ((s IDBF_ofb64_encrypt__tmp) + (s IDBF_ofb64_encrypt_z))%Q
    | 5%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z))%Q
    | 6%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z))%Q
    | 7%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z))%Q
    | 8%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z))%Q
    | 9%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z))%Q
    | 10%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z))%Q
    | 11%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z))%Q
    | 12%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z))%Q
    | 13%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z))%Q
    | 14%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z))%Q
    | 15%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z))%Q
    | 16%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 17%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 18%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 19%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 20%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 21%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 22%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z)
                      - max0((s IDBF_ofb64_encrypt_l)))%Q
    | 23%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z)
                      - max0((s IDBF_ofb64_encrypt_l)))%Q
    | 24%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z)
                      - max0((s IDBF_ofb64_encrypt_l)))%Q
    | 25%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z)
                      - max0((s IDBF_ofb64_encrypt_l)))%Q
    | 26%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z)
                      - max0((s IDBF_ofb64_encrypt_l)))%Q
    | 27%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z)
                      - max0((s IDBF_ofb64_encrypt_l)))%Q
    | 28%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z)
                      - max0((s IDBF_ofb64_encrypt_l)))%Q
    | 29%positive => ((s IDBF_ofb64_encrypt_l) + (s IDBF_ofb64_encrypt_z)
                      - max0((s IDBF_ofb64_encrypt_l)))%Q
    | 30%positive => ((s IDBF_ofb64_encrypt_z))%Q
    | 31%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 32%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 33%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 34%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 35%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 36%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 37%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 38%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 39%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 40%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 41%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | 42%positive => ((1 # 1) + (s IDBF_ofb64_encrypt_l)
                      + (s IDBF_ofb64_encrypt_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition BF_ofb64_encrypt_hints (p : node) (s : state) := 
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
    | 20%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDBF_ofb64_encrypt_l))) (F_check_ge (0) (0))]
    | 21%positive => [(*0 1*) F_one;
                      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDBF_ofb64_encrypt_l))) (F_check_ge (0) (0))]
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDBF_ofb64_encrypt_l)) (0))) (F_max0_ge_0 ((s IDBF_ofb64_encrypt_l)))]
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
    | _ => []
  end.


Theorem BF_ofb64_encrypt_ai_correct:
  forall s p' s', steps (g_start BF_ofb64_encrypt) s (g_edges BF_ofb64_encrypt) p' s' -> BF_ofb64_encrypt_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem BF_ofb64_encrypt_pot_correct:
  forall s p' s',
    steps (g_start BF_ofb64_encrypt) s (g_edges BF_ofb64_encrypt) p' s' ->
    (BF_ofb64_encrypt_pot (g_start BF_ofb64_encrypt) s >= BF_ofb64_encrypt_pot p' s')%Q.
Proof.
  check_lp BF_ofb64_encrypt_ai_correct BF_ofb64_encrypt_hints.
Qed.

