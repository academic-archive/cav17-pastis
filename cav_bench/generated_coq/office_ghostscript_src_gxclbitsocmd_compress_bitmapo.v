Require Import pasta.Pasta.

Notation IDcmd_compress_bitmap_z := 1%positive.
Notation IDcmd_compress_bitmap__tmp := 2%positive.
Notation IDcmd_compress_bitmap__tmp1 := 3%positive.
Notation IDcmd_compress_bitmap__tmp2 := 4%positive.
Notation IDcmd_compress_bitmap_status := 5%positive.
Notation IDcmd_compress_bitmap_width_bytes := 6%positive.
Notation IDcmd_compress_bitmap_y := 7%positive.
Notation IDcmd_compress_bitmap_data := 8%positive.
Notation IDcmd_compress_bitmap_height := 9%positive.
Notation IDcmd_compress_bitmap_pw := 10%positive.
Notation IDcmd_compress_bitmap_raster := 11%positive.
Notation IDcmd_compress_bitmap_st := 12%positive.
Notation IDcmd_compress_bitmap_width_bits := 13%positive.
Definition cmd_compress_bitmap : graph := {|
  g_start := 1%positive;
  g_end := 51%positive;
  g_edges := (1%positive,(AAssign IDcmd_compress_bitmap_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_compress_bitmap_y) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_compress_bitmap__tmp1) s) >=
             (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDcmd_compress_bitmap__tmp2
             (Some (EVar IDcmd_compress_bitmap_width_bits))),6%positive)::
             (6%positive,(AAssign IDcmd_compress_bitmap__tmp
             (Some (EVar IDcmd_compress_bitmap_raster))),7%positive)::
             (7%positive,(AAssign IDcmd_compress_bitmap__tmp1
             (Some (EVar IDcmd_compress_bitmap_height))),8%positive)::
             (8%positive,(AAssign IDcmd_compress_bitmap_width_bytes None),
             9%positive)::
             (9%positive,(AAssign IDcmd_compress_bitmap_status
             (Some (ENum (0)))),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_compress_bitmap__tmp) s) =
             (eval (EVar IDcmd_compress_bitmap_width_bytes) s))%Z)),
             43%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_compress_bitmap__tmp) s) <>
             (eval (EVar IDcmd_compress_bitmap_width_bytes) s))%Z)),
             12%positive)::(12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDcmd_compress_bitmap_y
             (Some (ENum (1)))),14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_compress_bitmap_y) s) <
             (eval (EVar IDcmd_compress_bitmap__tmp1) s))%Z)),18%positive)::
             (16%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_compress_bitmap_y) s) >=
             (eval (EVar IDcmd_compress_bitmap__tmp1) s))%Z)),17%positive)::
             (17%positive,AWeaken,36%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,(AAssign IDcmd_compress_bitmap_status None),
             20%positive)::(20%positive,AWeaken,21%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_compress_bitmap_status) s) <>
             (eval (ENum (0)) s))%Z)),33%positive)::
             (21%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_compress_bitmap_status) s) =
             (eval (ENum (0)) s))%Z)),22%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,30%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,ANone,25%positive)::
             (25%positive,(AAssign IDcmd_compress_bitmap_y
             (Some (EAdd (EVar IDcmd_compress_bitmap_y) (ENum (1))))),
             26%positive)::(26%positive,ANone,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDcmd_compress_bitmap_z
             (Some (EAdd (ENum (1)) (EVar IDcmd_compress_bitmap_z)))),
             29%positive)::(29%positive,AWeaken,16%positive)::
             (30%positive,(AAssign IDcmd_compress_bitmap_status
             (Some (ENum (-1)))),31%positive)::
             (31%positive,ANone,32%positive)::
             (32%positive,AWeaken,36%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,ANone,35%positive)::
             (35%positive,AWeaken,36%positive)::
             (36%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_compress_bitmap_status) s) =
             (eval (ENum (0)) s))%Z)),38%positive)::
             (36%positive,(AGuard
             (fun s => ((eval (EVar IDcmd_compress_bitmap_status) s) <>
             (eval (ENum (0)) s))%Z)),37%positive)::
             (37%positive,AWeaken,41%positive)::
             (38%positive,AWeaken,39%positive)::
             (39%positive,(AAssign IDcmd_compress_bitmap_status None),
             40%positive)::(40%positive,ANone,41%positive)::
             (41%positive,ANone,42%positive)::
             (42%positive,AWeaken,47%positive)::
             (43%positive,AWeaken,44%positive)::
             (44%positive,(AAssign IDcmd_compress_bitmap_status None),
             45%positive)::(45%positive,ANone,46%positive)::
             (46%positive,AWeaken,47%positive)::
             (47%positive,ANone,49%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,AWeaken,51%positive)::
             (49%positive,ANone,50%positive)::
             (50%positive,AWeaken,51%positive)::nil
|}.

Definition cmd_compress_bitmap_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0)%Z
    | 3%positive => (-1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) <= 0)%Z
    | 4%positive => (-1 * (s IDcmd_compress_bitmap_y) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1) <= 0)%Z
    | 5%positive => (-1 * (s IDcmd_compress_bitmap__tmp1) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) <= 0)%Z
    | 6%positive => (-1 * (s IDcmd_compress_bitmap_y) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1) <= 0)%Z
    | 7%positive => (-1 * (s IDcmd_compress_bitmap__tmp1) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) <= 0)%Z
    | 8%positive => (-1 * (s IDcmd_compress_bitmap_y) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0)%Z
    | 9%positive => (-1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) <= 0)%Z
    | 10%positive => (-1 * (s IDcmd_compress_bitmap_y) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0)%Z
    | 11%positive => (-1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) <= 0)%Z
    | 12%positive => (-1 * (s IDcmd_compress_bitmap_y) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0)%Z
    | 13%positive => (-1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) <= 0)%Z
    | 14%positive => (1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap_y) + -1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ 1 * (s IDcmd_compress_bitmap_y) + -1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0)%Z
    | 16%positive => (-1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0)%Z
    | 17%positive => (-1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap__tmp1)+ -1 * (s IDcmd_compress_bitmap_y) <= 0)%Z
    | 18%positive => (-1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0)%Z
    | 20%positive => (-1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0)%Z
    | 23%positive => (-1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0)%Z
    | 25%positive => (-1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 2 <= 0)%Z
    | 27%positive => (-1 * (s IDcmd_compress_bitmap_y) + 2 <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0)%Z
    | 28%positive => (-1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 2 <= 0)%Z
    | 29%positive => (-1 * (s IDcmd_compress_bitmap_y) + 2 <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) + 1 <= 0)%Z
    | 30%positive => (-1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0)%Z
    | 31%positive => (-1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) + -1 <= 0)%Z
    | 32%positive => (-1 * (s IDcmd_compress_bitmap_status) + -1 <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp1)+ 1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0)%Z
    | 38%positive => (-1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0)%Z
    | 39%positive => (-1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 40%positive => (-1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0)%Z
    | 41%positive => (-1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) + 1 <= 0)%Z
    | 42%positive => (-1 * (s IDcmd_compress_bitmap_y) + 1 <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0)%Z
    | 43%positive => (-1 * (s IDcmd_compress_bitmap_y) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap__tmp)+ -1 * (s IDcmd_compress_bitmap_width_bytes) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp)+ 1 * (s IDcmd_compress_bitmap_width_bytes) <= 0)%Z
    | 44%positive => (-1 * (s IDcmd_compress_bitmap__tmp)+ 1 * (s IDcmd_compress_bitmap_width_bytes) <= 0 /\ 1 * (s IDcmd_compress_bitmap__tmp)+ -1 * (s IDcmd_compress_bitmap_width_bytes) <= 0 /\ -1 * (s IDcmd_compress_bitmap_status) <= 0 /\ 1 * (s IDcmd_compress_bitmap_status) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) <= 0)%Z
    | 45%positive => (-1 * (s IDcmd_compress_bitmap_y) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap__tmp)+ -1 * (s IDcmd_compress_bitmap_width_bytes) <= 0 /\ -1 * (s IDcmd_compress_bitmap__tmp)+ 1 * (s IDcmd_compress_bitmap_width_bytes) <= 0)%Z
    | 46%positive => (-1 * (s IDcmd_compress_bitmap__tmp)+ 1 * (s IDcmd_compress_bitmap_width_bytes) <= 0 /\ 1 * (s IDcmd_compress_bitmap__tmp)+ -1 * (s IDcmd_compress_bitmap_width_bytes) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0 /\ 1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) <= 0)%Z
    | 47%positive => (-1 * (s IDcmd_compress_bitmap_y) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0)%Z
    | 48%positive => (-1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) <= 0)%Z
    | 49%positive => (-1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) <= 0)%Z
    | 50%positive => (-1 * (s IDcmd_compress_bitmap_y) <= 0 /\ -1 * (s IDcmd_compress_bitmap_z) <= 0)%Z
    | 51%positive => (-1 * (s IDcmd_compress_bitmap_z) <= 0 /\ -1 * (s IDcmd_compress_bitmap_y) <= 0)%Z
    | _ => False
  end.

Definition cmd_compress_bitmap_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDcmd_compress_bitmap_height)))%Q
    | 2%positive => ((s IDcmd_compress_bitmap_z)
                     + max0(-1 + (s IDcmd_compress_bitmap_height)))%Q
    | 3%positive => ((s IDcmd_compress_bitmap_z)
                     + max0(-1 + (s IDcmd_compress_bitmap_height)))%Q
    | 4%positive => ((s IDcmd_compress_bitmap_z)
                     + max0(-1 + (s IDcmd_compress_bitmap_height)))%Q
    | 5%positive => ((s IDcmd_compress_bitmap_z)
                     + max0(-1 + (s IDcmd_compress_bitmap_height)))%Q
    | 6%positive => ((s IDcmd_compress_bitmap_z)
                     + max0(-1 + (s IDcmd_compress_bitmap_height)))%Q
    | 7%positive => ((s IDcmd_compress_bitmap_z)
                     + max0(-1 + (s IDcmd_compress_bitmap_height)))%Q
    | 8%positive => ((s IDcmd_compress_bitmap_z)
                     + max0(-1 + (s IDcmd_compress_bitmap__tmp1)))%Q
    | 9%positive => ((s IDcmd_compress_bitmap_z)
                     + max0(-1 + (s IDcmd_compress_bitmap__tmp1)))%Q
    | 10%positive => ((s IDcmd_compress_bitmap_z)
                      + max0(-1 + (s IDcmd_compress_bitmap__tmp1)))%Q
    | 11%positive => ((s IDcmd_compress_bitmap_z)
                      + max0(-1 + (s IDcmd_compress_bitmap__tmp1)))%Q
    | 12%positive => ((s IDcmd_compress_bitmap_z)
                      + max0(-1 + (s IDcmd_compress_bitmap__tmp1)))%Q
    | 13%positive => ((s IDcmd_compress_bitmap_z)
                      + max0(-1 + (s IDcmd_compress_bitmap__tmp1)))%Q
    | 14%positive => ((s IDcmd_compress_bitmap_z)
                      + max0((s IDcmd_compress_bitmap__tmp1)
                             - (s IDcmd_compress_bitmap_y)))%Q
    | 15%positive => ((s IDcmd_compress_bitmap_z)
                      + max0((s IDcmd_compress_bitmap__tmp1)
                             - (s IDcmd_compress_bitmap_y)))%Q
    | 16%positive => ((s IDcmd_compress_bitmap_z)
                      + max0((s IDcmd_compress_bitmap__tmp1)
                             - (s IDcmd_compress_bitmap_y)))%Q
    | 17%positive => ((s IDcmd_compress_bitmap_z)
                      + max0((s IDcmd_compress_bitmap__tmp1)
                             - (s IDcmd_compress_bitmap_y)))%Q
    | 18%positive => ((s IDcmd_compress_bitmap_z)
                      + max0((s IDcmd_compress_bitmap__tmp1)
                             - (s IDcmd_compress_bitmap_y)))%Q
    | 19%positive => ((s IDcmd_compress_bitmap_z)
                      + max0((s IDcmd_compress_bitmap__tmp1)
                             - (s IDcmd_compress_bitmap_y)))%Q
    | 20%positive => ((s IDcmd_compress_bitmap_z)
                      + max0((s IDcmd_compress_bitmap__tmp1)
                             - (s IDcmd_compress_bitmap_y)))%Q
    | 21%positive => ((s IDcmd_compress_bitmap_z)
                      + max0((s IDcmd_compress_bitmap__tmp1)
                             - (s IDcmd_compress_bitmap_y)))%Q
    | 22%positive => ((s IDcmd_compress_bitmap_z)
                      + max0((s IDcmd_compress_bitmap__tmp1)
                             - (s IDcmd_compress_bitmap_y)))%Q
    | 23%positive => ((s IDcmd_compress_bitmap__tmp1)
                      - (s IDcmd_compress_bitmap_y)
                      + (s IDcmd_compress_bitmap_z))%Q
    | 24%positive => ((s IDcmd_compress_bitmap__tmp1)
                      - (s IDcmd_compress_bitmap_y)
                      + (s IDcmd_compress_bitmap_z))%Q
    | 25%positive => ((s IDcmd_compress_bitmap__tmp1)
                      - (s IDcmd_compress_bitmap_y)
                      + (s IDcmd_compress_bitmap_z))%Q
    | 26%positive => ((1 # 1) + (s IDcmd_compress_bitmap__tmp1)
                      - (s IDcmd_compress_bitmap_y)
                      + (s IDcmd_compress_bitmap_z))%Q
    | 27%positive => ((1 # 1) + (s IDcmd_compress_bitmap__tmp1)
                      - (s IDcmd_compress_bitmap_y)
                      + (s IDcmd_compress_bitmap_z))%Q
    | 28%positive => ((1 # 1) + (s IDcmd_compress_bitmap__tmp1)
                      - (s IDcmd_compress_bitmap_y)
                      + (s IDcmd_compress_bitmap_z))%Q
    | 29%positive => ((s IDcmd_compress_bitmap__tmp1)
                      - (s IDcmd_compress_bitmap_y)
                      + (s IDcmd_compress_bitmap_z))%Q
    | 30%positive => ((s IDcmd_compress_bitmap__tmp1)
                      - (s IDcmd_compress_bitmap_y)
                      + (s IDcmd_compress_bitmap_z))%Q
    | 31%positive => ((s IDcmd_compress_bitmap__tmp1)
                      - (s IDcmd_compress_bitmap_y)
                      + (s IDcmd_compress_bitmap_z))%Q
    | 32%positive => ((s IDcmd_compress_bitmap__tmp1)
                      - (s IDcmd_compress_bitmap_y)
                      + (s IDcmd_compress_bitmap_z))%Q
    | 33%positive => ((s IDcmd_compress_bitmap_z)
                      + max0((s IDcmd_compress_bitmap__tmp1)
                             - (s IDcmd_compress_bitmap_y)))%Q
    | 34%positive => ((s IDcmd_compress_bitmap_z)
                      + max0((s IDcmd_compress_bitmap__tmp1)
                             - (s IDcmd_compress_bitmap_y)))%Q
    | 35%positive => ((s IDcmd_compress_bitmap_z)
                      + max0((s IDcmd_compress_bitmap__tmp1)
                             - (s IDcmd_compress_bitmap_y)))%Q
    | 36%positive => ((s IDcmd_compress_bitmap_z))%Q
    | 37%positive => ((s IDcmd_compress_bitmap_z))%Q
    | 38%positive => ((s IDcmd_compress_bitmap_z))%Q
    | 39%positive => ((s IDcmd_compress_bitmap_z))%Q
    | 40%positive => ((s IDcmd_compress_bitmap_z))%Q
    | 41%positive => ((s IDcmd_compress_bitmap_z))%Q
    | 42%positive => ((s IDcmd_compress_bitmap_z))%Q
    | 43%positive => ((s IDcmd_compress_bitmap_z)
                      + max0(-1 + (s IDcmd_compress_bitmap__tmp1)))%Q
    | 44%positive => ((s IDcmd_compress_bitmap_z)
                      + max0(-1 + (s IDcmd_compress_bitmap__tmp1)))%Q
    | 45%positive => ((s IDcmd_compress_bitmap_z)
                      + max0(-1 + (s IDcmd_compress_bitmap__tmp1)))%Q
    | 46%positive => ((s IDcmd_compress_bitmap_z)
                      + max0(-1 + (s IDcmd_compress_bitmap__tmp1)))%Q
    | 47%positive => ((s IDcmd_compress_bitmap_z))%Q
    | 48%positive => ((s IDcmd_compress_bitmap_z))%Q
    | 49%positive => ((s IDcmd_compress_bitmap_z))%Q
    | 50%positive => ((s IDcmd_compress_bitmap_z))%Q
    | 51%positive => ((s IDcmd_compress_bitmap_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition cmd_compress_bitmap_hints (p : node) (s : state) := 
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
    | 17%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDcmd_compress_bitmap__tmp1)
                                                             - (s IDcmd_compress_bitmap_y)) (-1
                                                                    + (s IDcmd_compress_bitmap__tmp1)
                                                                    - (s IDcmd_compress_bitmap_y)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcmd_compress_bitmap__tmp1)
                                            - (s IDcmd_compress_bitmap_y))]
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDcmd_compress_bitmap__tmp1)
                                                                   - 
                                                                   (s IDcmd_compress_bitmap_y))) (F_check_ge ((s IDcmd_compress_bitmap__tmp1)
                                                                    - (s IDcmd_compress_bitmap_y)) (0))]
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcmd_compress_bitmap__tmp1)
                                                                    - (s IDcmd_compress_bitmap_y)) (0))) (F_max0_ge_0 ((s IDcmd_compress_bitmap__tmp1)
                                                                    - (s IDcmd_compress_bitmap_y)))]
    | 30%positive => []
    | 31%positive => []
    | 32%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDcmd_compress_bitmap__tmp1)
                                                     - (s IDcmd_compress_bitmap_y)) (1);
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcmd_compress_bitmap__tmp1)
                                            - (s IDcmd_compress_bitmap_y));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDcmd_compress_bitmap__tmp1)
                                                                    - (s IDcmd_compress_bitmap_y)) (0))) (F_max0_ge_0 ((s IDcmd_compress_bitmap__tmp1)
                                                                    - (s IDcmd_compress_bitmap_y)))]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_pre_decrement ((s IDcmd_compress_bitmap__tmp1)
                                                     - (s IDcmd_compress_bitmap_y)) (1);
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcmd_compress_bitmap__tmp1)
                                            - (s IDcmd_compress_bitmap_y))]
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => [(*-1 0*) F_max0_ge_0 (-1
                                            + (s IDcmd_compress_bitmap__tmp1))]
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => []
    | 51%positive => []
    | _ => []
  end.


Theorem cmd_compress_bitmap_ai_correct:
  forall s p' s', steps (g_start cmd_compress_bitmap) s (g_edges cmd_compress_bitmap) p' s' -> cmd_compress_bitmap_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem cmd_compress_bitmap_pot_correct:
  forall s p' s',
    steps (g_start cmd_compress_bitmap) s (g_edges cmd_compress_bitmap) p' s' ->
    (cmd_compress_bitmap_pot (g_start cmd_compress_bitmap) s >= cmd_compress_bitmap_pot p' s')%Q.
Proof.
  check_lp cmd_compress_bitmap_ai_correct cmd_compress_bitmap_hints.
Qed.

