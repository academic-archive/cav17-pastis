Require Import pasta.Pasta.

Notation IDread_samples_pcm_z := 1%positive.
Notation IDread_samples_pcm_NativeByteOrder := 2%positive.
Notation IDread_samples_pcm__tmp := 3%positive.
Notation IDread_samples_pcm__tmp1 := 4%positive.
Notation IDread_samples_pcm_gfp_dref_off124 := 5%positive.
Notation IDread_samples_pcm_iswav := 6%positive.
Notation IDread_samples_pcm_rcode := 7%positive.
Notation IDread_samples_pcm_samples_read := 8%positive.
Notation IDread_samples_pcm_frame_size := 9%positive.
Notation IDread_samples_pcm_gfp := 10%positive.
Notation IDread_samples_pcm_sample_buffer := 11%positive.
Notation IDread_samples_pcm_samples_to_read := 12%positive.
Definition read_samples_pcm : graph := {|
  g_start := 1%positive;
  g_end := 66%positive;
  g_edges := (1%positive,(AAssign IDread_samples_pcm_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDread_samples_pcm__tmp
             (Some (EVar IDread_samples_pcm_frame_size))),3%positive)::
             (3%positive,(AAssign IDread_samples_pcm__tmp1
             (Some (EVar IDread_samples_pcm_samples_to_read))),4%positive)::
             (4%positive,(AAssign IDread_samples_pcm_iswav None),5%positive)::
             (5%positive,(AAssign IDread_samples_pcm_samples_read None),
             6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,ANone,65%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_NativeByteOrder) s) =
             (eval (ENum (0)) s))%Z)),11%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_NativeByteOrder) s) <>
             (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,18%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AAssign IDread_samples_pcm_NativeByteOrder None),
             13%positive)::(13%positive,AWeaken,14%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_NativeByteOrder) s) =
             (eval (ENum (0)) s))%Z)),62%positive)::
             (14%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_NativeByteOrder) s) <>
             (eval (ENum (0)) s))%Z)),15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_iswav) s) <>
             (eval (ENum (0)) s))%Z)),25%positive)::
             (18%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_iswav) s) =
             (eval (ENum (0)) s))%Z)),19%positive)::
             (19%positive,AWeaken,20%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_NativeByteOrder) s) =
             (eval (ENum (2)) s))%Z)),22%positive)::
             (20%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_NativeByteOrder) s) <>
             (eval (ENum (2)) s))%Z)),21%positive)::
             (21%positive,AWeaken,26%positive)::
             (22%positive,AWeaken,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,AWeaken,26%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_iswav) s) <>
             (eval (ENum (0)) s))%Z)),28%positive)::
             (26%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_iswav) s) =
             (eval (ENum (0)) s))%Z)),27%positive)::
             (27%positive,AWeaken,34%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_NativeByteOrder) s) =
             (eval (ENum (1)) s))%Z)),31%positive)::
             (29%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_NativeByteOrder) s) <>
             (eval (ENum (1)) s))%Z)),30%positive)::
             (30%positive,AWeaken,34%positive)::
             (31%positive,AWeaken,32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,AWeaken,34%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_gfp_dref_off124) s) =
             (eval (ENum (1)) s))%Z)),36%positive)::
             (34%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_gfp_dref_off124) s) <>
             (eval (ENum (1)) s))%Z)),35%positive)::
             (35%positive,AWeaken,38%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDread_samples_pcm_rcode
             (Some (EVar IDread_samples_pcm_samples_read))),39%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_samples_read) s) <
             (eval (EVar IDread_samples_pcm__tmp) s))%Z)),42%positive)::
             (40%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_samples_read) s) >=
             (eval (EVar IDread_samples_pcm__tmp) s))%Z)),41%positive)::
             (41%positive,AWeaken,53%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_samples_read) s) <
             (eval (ENum (0)) s))%Z)),45%positive)::
             (43%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_samples_read) s) >=
             (eval (ENum (0)) s))%Z)),44%positive)::
             (44%positive,AWeaken,48%positive)::
             (45%positive,AWeaken,46%positive)::
             (46%positive,(AAssign IDread_samples_pcm_samples_read
             (Some (ENum (0)))),47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,AWeaken,50%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_samples_read) s) <
             (eval (EVar IDread_samples_pcm__tmp) s))%Z)),55%positive)::
             (50%positive,(AGuard
             (fun s => ((eval (EVar IDread_samples_pcm_samples_read) s) >=
             (eval (EVar IDread_samples_pcm__tmp) s))%Z)),51%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,ANone,54%positive)::
             (54%positive,AWeaken,66%positive)::
             (55%positive,AWeaken,56%positive)::
             (56%positive,ANone,57%positive)::
             (57%positive,(AAssign IDread_samples_pcm_samples_read
             (Some (EAdd (EVar IDread_samples_pcm_samples_read)
             (ENum (1))))),58%positive)::(58%positive,ANone,59%positive)::
             (59%positive,ANone,60%positive)::
             (60%positive,(AAssign IDread_samples_pcm_z
             (Some (EAdd (ENum (1)) (EVar IDread_samples_pcm_z)))),
             61%positive)::(61%positive,AWeaken,50%positive)::
             (62%positive,AWeaken,63%positive)::
             (63%positive,ANone,64%positive)::
             (64%positive,AWeaken,66%positive)::
             (65%positive,AWeaken,66%positive)::nil
|}.

Definition read_samples_pcm_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 3%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 4%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 5%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 6%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 7%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 8%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 9%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 10%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 11%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_NativeByteOrder) <= 0 /\ -1 * (s IDread_samples_pcm_NativeByteOrder) <= 0)%Z
    | 12%positive => (-1 * (s IDread_samples_pcm_NativeByteOrder) <= 0 /\ 1 * (s IDread_samples_pcm_NativeByteOrder) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 13%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 14%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 15%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 16%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 17%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 18%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 19%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_iswav) <= 0 /\ -1 * (s IDread_samples_pcm_iswav) <= 0)%Z
    | 20%positive => (-1 * (s IDread_samples_pcm_iswav) <= 0 /\ 1 * (s IDread_samples_pcm_iswav) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 21%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_iswav) <= 0 /\ -1 * (s IDread_samples_pcm_iswav) <= 0)%Z
    | 22%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_iswav) <= 0 /\ -1 * (s IDread_samples_pcm_iswav) <= 0 /\ 1 * (s IDread_samples_pcm_NativeByteOrder) + -2 <= 0 /\ -1 * (s IDread_samples_pcm_NativeByteOrder) + 2 <= 0)%Z
    | 23%positive => (-1 * (s IDread_samples_pcm_NativeByteOrder) + 2 <= 0 /\ 1 * (s IDread_samples_pcm_NativeByteOrder) + -2 <= 0 /\ -1 * (s IDread_samples_pcm_iswav) <= 0 /\ 1 * (s IDread_samples_pcm_iswav) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 24%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_iswav) <= 0 /\ -1 * (s IDread_samples_pcm_iswav) <= 0 /\ 1 * (s IDread_samples_pcm_NativeByteOrder) + -2 <= 0 /\ -1 * (s IDread_samples_pcm_NativeByteOrder) + 2 <= 0)%Z
    | 25%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 26%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 27%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_iswav) <= 0 /\ -1 * (s IDread_samples_pcm_iswav) <= 0)%Z
    | 28%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 29%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 30%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 31%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_NativeByteOrder) + -1 <= 0 /\ -1 * (s IDread_samples_pcm_NativeByteOrder) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDread_samples_pcm_NativeByteOrder) + 1 <= 0 /\ 1 * (s IDread_samples_pcm_NativeByteOrder) + -1 <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 33%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_NativeByteOrder) + -1 <= 0 /\ -1 * (s IDread_samples_pcm_NativeByteOrder) + 1 <= 0)%Z
    | 34%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 35%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 36%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_gfp_dref_off124) + -1 <= 0 /\ -1 * (s IDread_samples_pcm_gfp_dref_off124) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDread_samples_pcm_gfp_dref_off124) + 1 <= 0 /\ 1 * (s IDread_samples_pcm_gfp_dref_off124) + -1 <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 38%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 39%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 40%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 41%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm__tmp)+ -1 * (s IDread_samples_pcm_samples_read) <= 0)%Z
    | 42%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm__tmp)+ 1 * (s IDread_samples_pcm_samples_read) + 1 <= 0)%Z
    | 43%positive => (-1 * (s IDread_samples_pcm__tmp)+ 1 * (s IDread_samples_pcm_samples_read) + 1 <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 44%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm__tmp)+ 1 * (s IDread_samples_pcm_samples_read) + 1 <= 0 /\ -1 * (s IDread_samples_pcm_samples_read) <= 0)%Z
    | 45%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm__tmp)+ 1 * (s IDread_samples_pcm_samples_read) + 1 <= 0 /\ 1 * (s IDread_samples_pcm_samples_read) + 1 <= 0)%Z
    | 46%positive => (1 * (s IDread_samples_pcm_samples_read) + 1 <= 0 /\ -1 * (s IDread_samples_pcm__tmp)+ 1 * (s IDread_samples_pcm_samples_read) + 1 <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 47%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_samples_read) <= 0 /\ -1 * (s IDread_samples_pcm_samples_read) <= 0)%Z
    | 48%positive => (-1 * (s IDread_samples_pcm_samples_read) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 49%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_samples_read) <= 0)%Z
    | 50%positive => (-1 * (s IDread_samples_pcm_samples_read) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 51%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_samples_read) <= 0 /\ 1 * (s IDread_samples_pcm__tmp)+ -1 * (s IDread_samples_pcm_samples_read) <= 0)%Z
    | 52%positive => (1 * (s IDread_samples_pcm__tmp)+ -1 * (s IDread_samples_pcm_samples_read) <= 0 /\ -1 * (s IDread_samples_pcm_samples_read) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 53%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm__tmp)+ -1 * (s IDread_samples_pcm_samples_read) <= 0)%Z
    | 54%positive => (1 * (s IDread_samples_pcm__tmp)+ -1 * (s IDread_samples_pcm_samples_read) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 55%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_samples_read) <= 0 /\ -1 * (s IDread_samples_pcm__tmp)+ 1 * (s IDread_samples_pcm_samples_read) + 1 <= 0)%Z
    | 56%positive => (-1 * (s IDread_samples_pcm__tmp)+ 1 * (s IDread_samples_pcm_samples_read) + 1 <= 0 /\ -1 * (s IDread_samples_pcm_samples_read) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 57%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_samples_read) <= 0 /\ -1 * (s IDread_samples_pcm__tmp)+ 1 * (s IDread_samples_pcm_samples_read) + 1 <= 0)%Z
    | 58%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_samples_read) + 1 <= 0 /\ -1 * (s IDread_samples_pcm__tmp)+ 1 * (s IDread_samples_pcm_samples_read) <= 0)%Z
    | 59%positive => (-1 * (s IDread_samples_pcm__tmp)+ 1 * (s IDread_samples_pcm_samples_read) <= 0 /\ -1 * (s IDread_samples_pcm_samples_read) + 1 <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 60%positive => (-1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_samples_read) + 1 <= 0 /\ -1 * (s IDread_samples_pcm__tmp)+ 1 * (s IDread_samples_pcm_samples_read) <= 0)%Z
    | 61%positive => (-1 * (s IDread_samples_pcm__tmp)+ 1 * (s IDread_samples_pcm_samples_read) <= 0 /\ -1 * (s IDread_samples_pcm_samples_read) + 1 <= 0 /\ -1 * (s IDread_samples_pcm_z) + 1 <= 0)%Z
    | 62%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_NativeByteOrder) <= 0 /\ -1 * (s IDread_samples_pcm_NativeByteOrder) <= 0)%Z
    | 63%positive => (-1 * (s IDread_samples_pcm_NativeByteOrder) <= 0 /\ 1 * (s IDread_samples_pcm_NativeByteOrder) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 64%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0 /\ 1 * (s IDread_samples_pcm_NativeByteOrder) <= 0 /\ -1 * (s IDread_samples_pcm_NativeByteOrder) <= 0)%Z
    | 65%positive => (1 * (s IDread_samples_pcm_z) <= 0 /\ -1 * (s IDread_samples_pcm_z) <= 0)%Z
    | 66%positive => (-1 * (s IDread_samples_pcm_z) <= 0)%Z
    | _ => False
  end.

Definition read_samples_pcm_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDread_samples_pcm_frame_size)))%Q
    | 2%positive => (max0((s IDread_samples_pcm_frame_size))
                     + max0((s IDread_samples_pcm_z)))%Q
    | 3%positive => (max0((s IDread_samples_pcm__tmp))
                     + max0((s IDread_samples_pcm_z)))%Q
    | 4%positive => (max0((s IDread_samples_pcm__tmp))
                     + max0((s IDread_samples_pcm_z)))%Q
    | 5%positive => (max0((s IDread_samples_pcm__tmp))
                     + max0((s IDread_samples_pcm_z)))%Q
    | 6%positive => (max0((s IDread_samples_pcm__tmp))
                     + max0((s IDread_samples_pcm_z)))%Q
    | 7%positive => (max0((s IDread_samples_pcm__tmp))
                     + max0((s IDread_samples_pcm_z)))%Q
    | 8%positive => (max0((s IDread_samples_pcm__tmp))
                     + max0((s IDread_samples_pcm_z)))%Q
    | 9%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 10%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 11%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 12%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 13%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 14%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 15%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 16%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 17%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 18%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 19%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 20%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 21%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 22%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 23%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 24%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 25%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 26%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 27%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 28%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 29%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 30%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 31%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 32%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp))
                      + max0(-(s IDread_samples_pcm_z)))%Q
    | 33%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp))
                      + max0(-(s IDread_samples_pcm_z)))%Q
    | 34%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 35%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 36%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 37%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 38%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 39%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 40%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 41%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 42%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 43%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 44%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 45%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 46%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)))%Q
    | 47%positive => ((s IDread_samples_pcm_samples_read)
                      + (s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)
                             - (s IDread_samples_pcm_samples_read)))%Q
    | 48%positive => ((s IDread_samples_pcm_samples_read)
                      + (s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)
                             - (s IDread_samples_pcm_samples_read)))%Q
    | 49%positive => ((s IDread_samples_pcm_samples_read)
                      + (s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)
                             - (s IDread_samples_pcm_samples_read)))%Q
    | 50%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)
                             - (s IDread_samples_pcm_samples_read)))%Q
    | 51%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)
                             - (s IDread_samples_pcm_samples_read)))%Q
    | 52%positive => ((s IDread_samples_pcm_z))%Q
    | 53%positive => ((s IDread_samples_pcm_z))%Q
    | 54%positive => ((s IDread_samples_pcm_z))%Q
    | 55%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)
                             - (s IDread_samples_pcm_samples_read)))%Q
    | 56%positive => ((1 # 1) + (s IDread_samples_pcm_z)
                      + max0(-1 + (s IDread_samples_pcm__tmp)
                             - (s IDread_samples_pcm_samples_read)))%Q
    | 57%positive => ((1 # 1) + (s IDread_samples_pcm_z)
                      + max0(-1 + (s IDread_samples_pcm__tmp)
                             - (s IDread_samples_pcm_samples_read)))%Q
    | 58%positive => ((1 # 1) + (s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)
                             - (s IDread_samples_pcm_samples_read)))%Q
    | 59%positive => ((1 # 1) + (s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)
                             - (s IDread_samples_pcm_samples_read)))%Q
    | 60%positive => ((1 # 1) + (s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)
                             - (s IDread_samples_pcm_samples_read)))%Q
    | 61%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp)
                             - (s IDread_samples_pcm_samples_read)))%Q
    | 62%positive => (max0((s IDread_samples_pcm__tmp)))%Q
    | 63%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp))
                      + max0(-(s IDread_samples_pcm_z)))%Q
    | 64%positive => ((s IDread_samples_pcm_z)
                      + max0((s IDread_samples_pcm__tmp))
                      + max0(-(s IDread_samples_pcm_z)))%Q
    | 65%positive => (max0((s IDread_samples_pcm__tmp))
                      + max0((s IDread_samples_pcm_z)))%Q
    | 66%positive => ((s IDread_samples_pcm_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition read_samples_pcm_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDread_samples_pcm_z))) (F_check_ge (0) (0))]
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
    | 27%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDread_samples_pcm_z))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDread_samples_pcm_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDread_samples_pcm_z)))]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDread_samples_pcm_z))) (F_check_ge (0) (0));
                      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDread_samples_pcm_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDread_samples_pcm_z)))]
    | 31%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDread_samples_pcm_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDread_samples_pcm_z)))]
    | 32%positive => []
    | 33%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDread_samples_pcm_z))) (F_check_ge (0) (0))]
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => [(*-1 0*) F_max0_ge_0 ((s IDread_samples_pcm__tmp))]
    | 42%positive => []
    | 43%positive => []
    | 44%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDread_samples_pcm__tmp)
                                                                    - (s IDread_samples_pcm_samples_read)) (0))) (F_max0_ge_0 ((s IDread_samples_pcm__tmp)
                                                                    - (s IDread_samples_pcm_samples_read)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDread_samples_pcm__tmp))) (F_check_ge ((s IDread_samples_pcm__tmp)) (0))]
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 ((s IDread_samples_pcm_samples_read))) (F_check_ge (0) (0));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDread_samples_pcm_samples_read)) (0))) (F_max0_ge_0 ((s IDread_samples_pcm_samples_read)))]
    | 50%positive => []
    | 51%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDread_samples_pcm__tmp)
                                                             - (s IDread_samples_pcm_samples_read)) (-1
                                                                    + (s IDread_samples_pcm__tmp)
                                                                    - (s IDread_samples_pcm_samples_read)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDread_samples_pcm__tmp)
                                            - (s IDread_samples_pcm_samples_read))]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => [(*-1 0*) F_max0_pre_decrement ((s IDread_samples_pcm__tmp)
                                                     - (s IDread_samples_pcm_samples_read)) (1)]
    | 56%positive => []
    | 57%positive => []
    | 58%positive => []
    | 59%positive => []
    | 60%positive => []
    | 61%positive => []
    | 62%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDread_samples_pcm_z)) (0))) (F_max0_ge_0 (-
                                                                    (s IDread_samples_pcm_z)))]
    | 63%positive => []
    | 64%positive => [(*-1 0*) F_max0_ge_0 ((s IDread_samples_pcm__tmp));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-(s IDread_samples_pcm_z))) (F_check_ge (0) (0))]
    | 65%positive => [(*-1 0*) F_max0_ge_0 ((s IDread_samples_pcm__tmp));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDread_samples_pcm_z))) (F_check_ge ((s IDread_samples_pcm_z)) (0))]
    | 66%positive => []
    | _ => []
  end.


Theorem read_samples_pcm_ai_correct:
  forall s p' s', steps (g_start read_samples_pcm) s (g_edges read_samples_pcm) p' s' -> read_samples_pcm_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem read_samples_pcm_pot_correct:
  forall s p' s',
    steps (g_start read_samples_pcm) s (g_edges read_samples_pcm) p' s' ->
    (read_samples_pcm_pot (g_start read_samples_pcm) s >= read_samples_pcm_pot p' s')%Q.
Proof.
  check_lp read_samples_pcm_ai_correct read_samples_pcm_hints.
Qed.

