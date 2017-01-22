Require Import pasta.Pasta.

Notation IDmad_synth_mute_z := 1%positive.
Notation IDmad_synth_mute_ch := 2%positive.
Notation IDmad_synth_mute_s := 3%positive.
Notation IDmad_synth_mute_v := 4%positive.
Notation IDmad_synth_mute_synth := 5%positive.
Definition mad_synth_mute : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDmad_synth_mute_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDmad_synth_mute_v)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDmad_synth_mute_s)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDmad_synth_mute_ch)
             s) >= (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AAssign IDmad_synth_mute_ch (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDmad_synth_mute_ch)
             s) < (eval (ENum (2)) s))%Z)),12%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDmad_synth_mute_ch)
             s) >= (eval (ENum (2)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,(AAssign IDmad_synth_mute_s (Some (ENum (0)))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDmad_synth_mute_s)
             s) < (eval (ENum (16)) s))%Z)),24%positive)::
             (16%positive,(AGuard (fun s => ((eval (EVar IDmad_synth_mute_s)
             s) >= (eval (ENum (16)) s))%Z)),17%positive)::
             (17%positive,AWeaken,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDmad_synth_mute_ch
             (Some (EAdd (EVar IDmad_synth_mute_ch) (ENum (1))))),
             20%positive)::(20%positive,ANone,21%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,(AAssign IDmad_synth_mute_z (Some (EAdd (ENum (1))
             (EVar IDmad_synth_mute_z)))),23%positive)::
             (23%positive,AWeaken,9%positive)::
             (24%positive,AWeaken,25%positive)::
             (25%positive,(AAssign IDmad_synth_mute_v (Some (ENum (0)))),
             26%positive)::(26%positive,ANone,27%positive)::
             (27%positive,AWeaken,28%positive)::
             (28%positive,(AGuard (fun s => ((eval (EVar IDmad_synth_mute_v)
             s) < (eval (ENum (8)) s))%Z)),36%positive)::
             (28%positive,(AGuard (fun s => ((eval (EVar IDmad_synth_mute_v)
             s) >= (eval (ENum (8)) s))%Z)),29%positive)::
             (29%positive,AWeaken,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDmad_synth_mute_s
             (Some (EAdd (EVar IDmad_synth_mute_s) (ENum (1))))),32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDmad_synth_mute_z (Some (EAdd (ENum (1))
             (EVar IDmad_synth_mute_z)))),35%positive)::
             (35%positive,AWeaken,16%positive)::
             (36%positive,AWeaken,37%positive)::
             (37%positive,ANone,38%positive)::
             (38%positive,(AAssign IDmad_synth_mute_v
             (Some (EAdd (EVar IDmad_synth_mute_v) (ENum (1))))),39%positive)::
             (39%positive,ANone,40%positive)::
             (40%positive,ANone,41%positive)::
             (41%positive,(AAssign IDmad_synth_mute_z (Some (EAdd (ENum (1))
             (EVar IDmad_synth_mute_z)))),42%positive)::
             (42%positive,AWeaken,28%positive)::nil
|}.

Definition mad_synth_mute_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmad_synth_mute_z) <= 0 /\ 1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0)%Z
    | 4%positive => (-1 * (s IDmad_synth_mute_v) <= 0 /\ 1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0)%Z
    | 5%positive => (-1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ 1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0)%Z
    | 6%positive => (-1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ 1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0)%Z
    | 7%positive => (-1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ 1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ 1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0)%Z
    | 8%positive => (-1 * (s IDmad_synth_mute_ch) <= 0 /\ 1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ 1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0)%Z
    | 9%positive => (-1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0)%Z
    | 10%positive => (-1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_ch) + 2 <= 0)%Z
    | 11%positive => (-1 * (s IDmad_synth_mute_ch) + 2 <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0)%Z
    | 12%positive => (-1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ 1 * (s IDmad_synth_mute_ch) + -1 <= 0)%Z
    | 13%positive => (1 * (s IDmad_synth_mute_ch) + -1 <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0)%Z
    | 14%positive => (-1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ 1 * (s IDmad_synth_mute_ch) + -1 <= 0 /\ 1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0)%Z
    | 15%positive => (-1 * (s IDmad_synth_mute_s) <= 0 /\ 1 * (s IDmad_synth_mute_s) <= 0 /\ 1 * (s IDmad_synth_mute_ch) + -1 <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0)%Z
    | 16%positive => (-1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0)%Z
    | 17%positive => (-1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_s) + 16 <= 0)%Z
    | 18%positive => (-1 * (s IDmad_synth_mute_s) + 16 <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0)%Z
    | 19%positive => (-1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_s) + 16 <= 0)%Z
    | 20%positive => (-1 * (s IDmad_synth_mute_s) + 16 <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_ch) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDmad_synth_mute_ch) + 1 <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_s) + 16 <= 0)%Z
    | 22%positive => (-1 * (s IDmad_synth_mute_s) + 16 <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_ch) + 1 <= 0)%Z
    | 23%positive => (-1 * (s IDmad_synth_mute_ch) + 1 <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_s) + 16 <= 0 /\ -1 * (s IDmad_synth_mute_z) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ 1 * (s IDmad_synth_mute_s) + -15 <= 0)%Z
    | 25%positive => (1 * (s IDmad_synth_mute_s) + -15 <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0)%Z
    | 26%positive => (-1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ 1 * (s IDmad_synth_mute_s) + -15 <= 0 /\ 1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0)%Z
    | 27%positive => (-1 * (s IDmad_synth_mute_v) <= 0 /\ 1 * (s IDmad_synth_mute_v) <= 0 /\ 1 * (s IDmad_synth_mute_s) + -15 <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0)%Z
    | 28%positive => (-1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ 1 * (s IDmad_synth_mute_v) + -8 <= 0)%Z
    | 29%positive => (1 * (s IDmad_synth_mute_v) + -8 <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) + 8 <= 0)%Z
    | 30%positive => (-1 * (s IDmad_synth_mute_v) + 8 <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ 1 * (s IDmad_synth_mute_v) + -8 <= 0)%Z
    | 31%positive => (1 * (s IDmad_synth_mute_v) + -8 <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) + 8 <= 0)%Z
    | 32%positive => (-1 * (s IDmad_synth_mute_v) + 8 <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ 1 * (s IDmad_synth_mute_v) + -8 <= 0 /\ -1 * (s IDmad_synth_mute_s) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDmad_synth_mute_s) + 1 <= 0 /\ 1 * (s IDmad_synth_mute_v) + -8 <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) + 8 <= 0)%Z
    | 34%positive => (-1 * (s IDmad_synth_mute_v) + 8 <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ 1 * (s IDmad_synth_mute_v) + -8 <= 0 /\ -1 * (s IDmad_synth_mute_s) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDmad_synth_mute_s) + 1 <= 0 /\ 1 * (s IDmad_synth_mute_v) + -8 <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_v) + 8 <= 0 /\ -1 * (s IDmad_synth_mute_z) + 1 <= 0)%Z
    | 36%positive => (-1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ 1 * (s IDmad_synth_mute_v) + -7 <= 0)%Z
    | 37%positive => (1 * (s IDmad_synth_mute_v) + -7 <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0)%Z
    | 38%positive => (-1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_v) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0 /\ 1 * (s IDmad_synth_mute_v) + -7 <= 0)%Z
    | 39%positive => (-1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_v) + 1 <= 0 /\ 1 * (s IDmad_synth_mute_v) + -8 <= 0)%Z
    | 40%positive => (1 * (s IDmad_synth_mute_v) + -8 <= 0 /\ -1 * (s IDmad_synth_mute_v) + 1 <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_z) <= 0)%Z
    | 41%positive => (-1 * (s IDmad_synth_mute_z) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_v) + 1 <= 0 /\ 1 * (s IDmad_synth_mute_v) + -8 <= 0)%Z
    | 42%positive => (1 * (s IDmad_synth_mute_v) + -8 <= 0 /\ -1 * (s IDmad_synth_mute_v) + 1 <= 0 /\ -1 * (s IDmad_synth_mute_ch) <= 0 /\ -1 * (s IDmad_synth_mute_s) <= 0 /\ -1 * (s IDmad_synth_mute_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition mad_synth_mute_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((290 # 1))%Q
    | 2%positive => ((290 # 1) + (s IDmad_synth_mute_z))%Q
    | 3%positive => ((290 # 1) + (s IDmad_synth_mute_z))%Q
    | 4%positive => ((290 # 1) + (s IDmad_synth_mute_z))%Q
    | 5%positive => ((290 # 1) + (s IDmad_synth_mute_z))%Q
    | 6%positive => ((290 # 1) + (s IDmad_synth_mute_z))%Q
    | 7%positive => ((s IDmad_synth_mute_z)
                     + (145 # 1) * max0(2 - (s IDmad_synth_mute_ch)))%Q
    | 8%positive => ((s IDmad_synth_mute_z)
                     + (145 # 1) * max0(2 - (s IDmad_synth_mute_ch)))%Q
    | 9%positive => ((s IDmad_synth_mute_z)
                     + (145 # 1) * max0(2 - (s IDmad_synth_mute_ch)))%Q
    | 10%positive => ((s IDmad_synth_mute_z)
                      + (145 # 1) * max0(2 - (s IDmad_synth_mute_ch)))%Q
    | 11%positive => ((s IDmad_synth_mute_z))%Q
    | 12%positive => ((s IDmad_synth_mute_z)
                      + (145 # 1) * max0(2 - (s IDmad_synth_mute_ch)))%Q
    | 13%positive => ((s IDmad_synth_mute_z)
                      + (145 # 1) * max0(2 - (s IDmad_synth_mute_ch)))%Q
    | 14%positive => (-(144 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(2 - (s IDmad_synth_mute_ch))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 15%positive => (-(144 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(2 - (s IDmad_synth_mute_ch))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 16%positive => ((1 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 17%positive => ((1 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 18%positive => ((1 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 19%positive => ((1 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 20%positive => ((1 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(2 - (s IDmad_synth_mute_ch))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 21%positive => ((1 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(2 - (s IDmad_synth_mute_ch))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 22%positive => ((1 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(2 - (s IDmad_synth_mute_ch))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 23%positive => ((s IDmad_synth_mute_z)
                      + (145 # 1) * max0(2 - (s IDmad_synth_mute_ch))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 24%positive => ((1 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 25%positive => ((1 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 26%positive => (-(7 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 27%positive => (-(7 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 28%positive => ((2 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(15 - (s IDmad_synth_mute_s)))%Q
    | 29%positive => ((2 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(15 - (s IDmad_synth_mute_s)))%Q
    | 30%positive => ((2 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(15 - (s IDmad_synth_mute_s)))%Q
    | 31%positive => ((2 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(15 - (s IDmad_synth_mute_s)))%Q
    | 32%positive => ((2 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 33%positive => ((2 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 34%positive => ((2 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 35%positive => ((1 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(16 - (s IDmad_synth_mute_s)))%Q
    | 36%positive => ((2 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(15 - (s IDmad_synth_mute_s)))%Q
    | 37%positive => ((3 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(7 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(15 - (s IDmad_synth_mute_s)))%Q
    | 38%positive => ((3 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(7 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(15 - (s IDmad_synth_mute_s)))%Q
    | 39%positive => ((3 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(15 - (s IDmad_synth_mute_s)))%Q
    | 40%positive => ((3 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(15 - (s IDmad_synth_mute_s)))%Q
    | 41%positive => ((3 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(15 - (s IDmad_synth_mute_s)))%Q
    | 42%positive => ((2 # 1) + (s IDmad_synth_mute_z)
                      + (145 # 1) * max0(1 - (s IDmad_synth_mute_ch))
                      + max0(8 - (s IDmad_synth_mute_v))
                      + (9 # 1) * max0(15 - (s IDmad_synth_mute_s)))%Q
    | _ => (0 # 1)%Q
  end.

Definition mad_synth_mute_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-145 0*) F_max0_monotonic (F_check_ge (2
                                                               - (s IDmad_synth_mute_ch)) (1
                                                                    - (s IDmad_synth_mute_ch)));
                      (*-145 0*) F_max0_ge_0 (1 - (s IDmad_synth_mute_ch))]
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => [(*-145 0*) F_max0_pre_decrement (2
                                                       - (s IDmad_synth_mute_ch)) (1)]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-9 0*) F_max0_ge_0 (16 - (s IDmad_synth_mute_s))]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => [(*-9 0*) F_max0_pre_decrement (16
                                                     - (s IDmad_synth_mute_s)) (1)]
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                                 - (s IDmad_synth_mute_v))) (F_check_ge (0) (0))]
    | 36%positive => [(*-1 0*) F_max0_pre_decrement (8
                                                     - (s IDmad_synth_mute_v)) (1)]
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => []
    | _ => []
  end.


Theorem mad_synth_mute_ai_correct:
  forall s p' s', steps (g_start mad_synth_mute) s (g_edges mad_synth_mute) p' s' -> mad_synth_mute_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mad_synth_mute_pot_correct:
  forall s p' s',
    steps (g_start mad_synth_mute) s (g_edges mad_synth_mute) p' s' ->
    (mad_synth_mute_pot (g_start mad_synth_mute) s >= mad_synth_mute_pot p' s')%Q.
Proof.
  check_lp mad_synth_mute_ai_correct mad_synth_mute_hints.
Qed.

