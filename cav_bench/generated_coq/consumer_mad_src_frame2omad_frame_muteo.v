Require Import pasta.Pasta.

Notation IDmad_frame_mute_z := 1%positive.
Notation IDmad_frame_mute_s := 2%positive.
Notation IDmad_frame_mute_sb := 3%positive.
Notation IDmad_frame_mute_frame := 4%positive.
Definition mad_frame_mute : graph := {|
  g_start := 1%positive;
  g_end := 19%positive;
  g_edges := (1%positive,(AAssign IDmad_frame_mute_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDmad_frame_mute_sb)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDmad_frame_mute_s)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDmad_frame_mute_s (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDmad_frame_mute_s)
             s) < (eval (ENum (36)) s))%Z)),39%positive)::
             (8%positive,(AGuard (fun s => ((eval (EVar IDmad_frame_mute_s)
             s) >= (eval (ENum (36)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,12%positive)::
             (10%positive,ANone,11%positive)::
             (11%positive,AWeaken,19%positive)::
             (12%positive,(AAssign IDmad_frame_mute_s (Some (ENum (0)))),
             13%positive)::(13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDmad_frame_mute_s)
             s) < (eval (ENum (18)) s))%Z)),20%positive)::
             (15%positive,(AGuard (fun s => ((eval (EVar IDmad_frame_mute_s)
             s) >= (eval (ENum (18)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,AWeaken,19%positive)::
             (20%positive,AWeaken,21%positive)::
             (21%positive,(AAssign IDmad_frame_mute_sb (Some (ENum (0)))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,AWeaken,24%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDmad_frame_mute_sb)
             s) < (eval (ENum (32)) s))%Z)),32%positive)::
             (24%positive,(AGuard (fun s => ((eval (EVar IDmad_frame_mute_sb)
             s) >= (eval (ENum (32)) s))%Z)),25%positive)::
             (25%positive,AWeaken,26%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,(AAssign IDmad_frame_mute_s
             (Some (EAdd (EVar IDmad_frame_mute_s) (ENum (1))))),28%positive)::
             (28%positive,ANone,29%positive)::
             (29%positive,ANone,30%positive)::
             (30%positive,(AAssign IDmad_frame_mute_z (Some (EAdd (ENum (1))
             (EVar IDmad_frame_mute_z)))),31%positive)::
             (31%positive,AWeaken,15%positive)::
             (32%positive,AWeaken,33%positive)::
             (33%positive,ANone,34%positive)::
             (34%positive,(AAssign IDmad_frame_mute_sb
             (Some (EAdd (EVar IDmad_frame_mute_sb) (ENum (1))))),
             35%positive)::(35%positive,ANone,36%positive)::
             (36%positive,ANone,37%positive)::
             (37%positive,(AAssign IDmad_frame_mute_z (Some (EAdd (ENum (1))
             (EVar IDmad_frame_mute_z)))),38%positive)::
             (38%positive,AWeaken,24%positive)::
             (39%positive,AWeaken,40%positive)::
             (40%positive,(AAssign IDmad_frame_mute_sb (Some (ENum (0)))),
             41%positive)::(41%positive,ANone,42%positive)::
             (42%positive,AWeaken,43%positive)::
             (43%positive,(AGuard (fun s => ((eval (EVar IDmad_frame_mute_sb)
             s) < (eval (ENum (32)) s))%Z)),51%positive)::
             (43%positive,(AGuard (fun s => ((eval (EVar IDmad_frame_mute_sb)
             s) >= (eval (ENum (32)) s))%Z)),44%positive)::
             (44%positive,AWeaken,45%positive)::
             (45%positive,ANone,46%positive)::
             (46%positive,(AAssign IDmad_frame_mute_s
             (Some (EAdd (EVar IDmad_frame_mute_s) (ENum (1))))),47%positive)::
             (47%positive,ANone,48%positive)::
             (48%positive,ANone,49%positive)::
             (49%positive,(AAssign IDmad_frame_mute_z (Some (EAdd (ENum (1))
             (EVar IDmad_frame_mute_z)))),50%positive)::
             (50%positive,AWeaken,8%positive)::
             (51%positive,AWeaken,52%positive)::
             (52%positive,ANone,53%positive)::
             (53%positive,(AAssign IDmad_frame_mute_sb
             (Some (EAdd (EVar IDmad_frame_mute_sb) (ENum (1))))),
             54%positive)::(54%positive,ANone,55%positive)::
             (55%positive,ANone,56%positive)::
             (56%positive,(AAssign IDmad_frame_mute_z (Some (EAdd (ENum (1))
             (EVar IDmad_frame_mute_z)))),57%positive)::
             (57%positive,AWeaken,43%positive)::nil
|}.

Definition mad_frame_mute_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0)%Z
    | 3%positive => (-1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0)%Z
    | 4%positive => (-1 * (s IDmad_frame_mute_sb) <= 0 /\ 1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0)%Z
    | 5%positive => (-1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0)%Z
    | 6%positive => (-1 * (s IDmad_frame_mute_sb) <= 0 /\ 1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0)%Z
    | 7%positive => (-1 * (s IDmad_frame_mute_s) <= 0 /\ 1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0)%Z
    | 8%positive => (-1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0)%Z
    | 9%positive => (-1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_s) + 36 <= 0)%Z
    | 10%positive => (-1 * (s IDmad_frame_mute_s) + 36 <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0)%Z
    | 11%positive => (-1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_s) + 36 <= 0)%Z
    | 12%positive => (-1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_s) + 36 <= 0)%Z
    | 13%positive => (-1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0)%Z
    | 14%positive => (-1 * (s IDmad_frame_mute_s) <= 0 /\ 1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0)%Z
    | 15%positive => (-1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0)%Z
    | 16%positive => (-1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_s) + 18 <= 0)%Z
    | 17%positive => (-1 * (s IDmad_frame_mute_s) + 18 <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0)%Z
    | 18%positive => (-1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_s) + 18 <= 0)%Z
    | 19%positive => (-1 * (s IDmad_frame_mute_s) + 18 <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0)%Z
    | 20%positive => (-1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ 1 * (s IDmad_frame_mute_s) + -17 <= 0)%Z
    | 21%positive => (1 * (s IDmad_frame_mute_s) + -17 <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0)%Z
    | 22%positive => (-1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_s) + -17 <= 0 /\ 1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0)%Z
    | 23%positive => (-1 * (s IDmad_frame_mute_sb) <= 0 /\ 1 * (s IDmad_frame_mute_sb) <= 0 /\ 1 * (s IDmad_frame_mute_s) + -17 <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0)%Z
    | 24%positive => (-1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0)%Z
    | 25%positive => (1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 32 <= 0)%Z
    | 26%positive => (-1 * (s IDmad_frame_mute_sb) + 32 <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0)%Z
    | 27%positive => (1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 32 <= 0)%Z
    | 28%positive => (-1 * (s IDmad_frame_mute_sb) + 32 <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_s) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDmad_frame_mute_s) + 1 <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 32 <= 0)%Z
    | 30%positive => (-1 * (s IDmad_frame_mute_sb) + 32 <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_s) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDmad_frame_mute_s) + 1 <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 32 <= 0 /\ -1 * (s IDmad_frame_mute_z) + 1 <= 0)%Z
    | 32%positive => (-1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -31 <= 0)%Z
    | 33%positive => (1 * (s IDmad_frame_mute_sb) + -31 <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0)%Z
    | 34%positive => (-1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -31 <= 0)%Z
    | 35%positive => (-1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 1 <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0)%Z
    | 36%positive => (1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 1 <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0)%Z
    | 37%positive => (-1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 1 <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0)%Z
    | 38%positive => (1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 1 <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) + 1 <= 0)%Z
    | 39%positive => (-1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ 1 * (s IDmad_frame_mute_s) + -35 <= 0)%Z
    | 40%positive => (1 * (s IDmad_frame_mute_s) + -35 <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0)%Z
    | 41%positive => (-1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_s) + -35 <= 0 /\ 1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0)%Z
    | 42%positive => (-1 * (s IDmad_frame_mute_sb) <= 0 /\ 1 * (s IDmad_frame_mute_sb) <= 0 /\ 1 * (s IDmad_frame_mute_s) + -35 <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0)%Z
    | 43%positive => (-1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0)%Z
    | 44%positive => (1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 32 <= 0)%Z
    | 45%positive => (-1 * (s IDmad_frame_mute_sb) + 32 <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0)%Z
    | 46%positive => (1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 32 <= 0)%Z
    | 47%positive => (-1 * (s IDmad_frame_mute_sb) + 32 <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_s) + 1 <= 0)%Z
    | 48%positive => (-1 * (s IDmad_frame_mute_s) + 1 <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 32 <= 0)%Z
    | 49%positive => (-1 * (s IDmad_frame_mute_sb) + 32 <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_s) + 1 <= 0)%Z
    | 50%positive => (-1 * (s IDmad_frame_mute_s) + 1 <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 32 <= 0 /\ -1 * (s IDmad_frame_mute_z) + 1 <= 0)%Z
    | 51%positive => (-1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -31 <= 0)%Z
    | 52%positive => (1 * (s IDmad_frame_mute_sb) + -31 <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0)%Z
    | 53%positive => (-1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_sb) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -31 <= 0)%Z
    | 54%positive => (-1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 1 <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0)%Z
    | 55%positive => (1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 1 <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) <= 0)%Z
    | 56%positive => (-1 * (s IDmad_frame_mute_z) <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 1 <= 0 /\ 1 * (s IDmad_frame_mute_sb) + -32 <= 0)%Z
    | 57%positive => (1 * (s IDmad_frame_mute_sb) + -32 <= 0 /\ -1 * (s IDmad_frame_mute_sb) + 1 <= 0 /\ -1 * (s IDmad_frame_mute_s) <= 0 /\ -1 * (s IDmad_frame_mute_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition mad_frame_mute_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((1782 # 1))%Q
    | 2%positive => ((1782 # 1) + (s IDmad_frame_mute_z))%Q
    | 3%positive => ((1782 # 1) + (s IDmad_frame_mute_z))%Q
    | 4%positive => ((1782 # 1) + (s IDmad_frame_mute_z))%Q
    | 5%positive => ((1782 # 1) + (s IDmad_frame_mute_z))%Q
    | 6%positive => ((594 # 1) + (s IDmad_frame_mute_z)
                     + (33 # 1) * max0(36 - (s IDmad_frame_mute_s)))%Q
    | 7%positive => ((594 # 1) + (s IDmad_frame_mute_z)
                     + (33 # 1) * max0(36 - (s IDmad_frame_mute_s)))%Q
    | 8%positive => ((594 # 1) + (s IDmad_frame_mute_z)
                     + (33 # 1) * max0(36 - (s IDmad_frame_mute_s)))%Q
    | 9%positive => ((594 # 1) + (s IDmad_frame_mute_z)
                     + (33 # 1) * max0(36 - (s IDmad_frame_mute_s)))%Q
    | 10%positive => ((594 # 1) + (s IDmad_frame_mute_z))%Q
    | 11%positive => ((594 # 1) + (s IDmad_frame_mute_z))%Q
    | 12%positive => ((594 # 1) + (s IDmad_frame_mute_z))%Q
    | 13%positive => ((s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s)))%Q
    | 14%positive => ((s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s)))%Q
    | 15%positive => ((s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s)))%Q
    | 16%positive => ((s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s)))%Q
    | 17%positive => ((s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s)))%Q
    | 18%positive => ((s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s)))%Q
    | 19%positive => ((s IDmad_frame_mute_z))%Q
    | 20%positive => ((s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s)))%Q
    | 21%positive => ((s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s)))%Q
    | 22%positive => (-(32 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 23%positive => (-(32 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 24%positive => ((1 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(17 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 25%positive => ((1 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(17 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 26%positive => ((1 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(17 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 27%positive => ((1 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(17 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 28%positive => ((1 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 29%positive => ((1 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 30%positive => ((1 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 31%positive => ((s IDmad_frame_mute_z)
                      + (33 # 1) * max0(18 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 32%positive => ((1 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(17 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 33%positive => ((2 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(17 - (s IDmad_frame_mute_s))
                      + max0(31 - (s IDmad_frame_mute_sb)))%Q
    | 34%positive => ((2 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(17 - (s IDmad_frame_mute_s))
                      + max0(31 - (s IDmad_frame_mute_sb)))%Q
    | 35%positive => ((2 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(17 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 36%positive => ((2 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(17 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 37%positive => ((2 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(17 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 38%positive => ((1 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(17 - (s IDmad_frame_mute_s))
                      + max0(32 - (s IDmad_frame_mute_sb)))%Q
    | 39%positive => ((594 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(36 - (s IDmad_frame_mute_s)))%Q
    | 40%positive => ((594 # 1) + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(36 - (s IDmad_frame_mute_s)))%Q
    | 41%positive => ((562 # 1) + (s IDmad_frame_mute_z)
                      + max0(32 - (s IDmad_frame_mute_sb))
                      + (33 # 1) * max0(36 - (s IDmad_frame_mute_s)))%Q
    | 42%positive => ((562 # 1) + (s IDmad_frame_mute_z)
                      + max0(32 - (s IDmad_frame_mute_sb))
                      + (33 # 1) * max0(36 - (s IDmad_frame_mute_s)))%Q
    | 43%positive => ((595 # 1) + (s IDmad_frame_mute_z)
                      + max0(32 - (s IDmad_frame_mute_sb))
                      + (33 # 1) * max0(35 - (s IDmad_frame_mute_s)))%Q
    | 44%positive => ((595 # 1) + (s IDmad_frame_mute_z)
                      + max0(32 - (s IDmad_frame_mute_sb))
                      + (33 # 1) * max0(35 - (s IDmad_frame_mute_s)))%Q
    | 45%positive => ((595 # 1) + (s IDmad_frame_mute_z)
                      + max0(32 - (s IDmad_frame_mute_sb))
                      + (33 # 1) * max0(35 - (s IDmad_frame_mute_s)))%Q
    | 46%positive => ((595 # 1) + (s IDmad_frame_mute_z)
                      + max0(32 - (s IDmad_frame_mute_sb))
                      + (33 # 1) * max0(35 - (s IDmad_frame_mute_s)))%Q
    | 47%positive => ((595 # 1) + (s IDmad_frame_mute_z)
                      + max0(32 - (s IDmad_frame_mute_sb))
                      + (33 # 1) * max0(36 - (s IDmad_frame_mute_s)))%Q
    | 48%positive => ((595 # 1) + (s IDmad_frame_mute_z)
                      + max0(32 - (s IDmad_frame_mute_sb))
                      + (33 # 1) * max0(36 - (s IDmad_frame_mute_s)))%Q
    | 49%positive => ((595 # 1) + (s IDmad_frame_mute_z)
                      + max0(32 - (s IDmad_frame_mute_sb))
                      + (33 # 1) * max0(36 - (s IDmad_frame_mute_s)))%Q
    | 50%positive => ((594 # 1) + (s IDmad_frame_mute_z)
                      + max0(32 - (s IDmad_frame_mute_sb))
                      + (33 # 1) * max0(36 - (s IDmad_frame_mute_s)))%Q
    | 51%positive => ((595 # 1) + (s IDmad_frame_mute_z)
                      + max0(32 - (s IDmad_frame_mute_sb))
                      + (33 # 1) * max0(35 - (s IDmad_frame_mute_s)))%Q
    | 52%positive => ((627 # 1) - (s IDmad_frame_mute_sb)
                      + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(35 - (s IDmad_frame_mute_s)))%Q
    | 53%positive => ((627 # 1) - (s IDmad_frame_mute_sb)
                      + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(35 - (s IDmad_frame_mute_s)))%Q
    | 54%positive => ((628 # 1) - (s IDmad_frame_mute_sb)
                      + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(35 - (s IDmad_frame_mute_s)))%Q
    | 55%positive => ((628 # 1) - (s IDmad_frame_mute_sb)
                      + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(35 - (s IDmad_frame_mute_s)))%Q
    | 56%positive => ((628 # 1) - (s IDmad_frame_mute_sb)
                      + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(35 - (s IDmad_frame_mute_s)))%Q
    | 57%positive => ((627 # 1) - (s IDmad_frame_mute_sb)
                      + (s IDmad_frame_mute_z)
                      + (33 # 1) * max0(35 - (s IDmad_frame_mute_s)))%Q
    | _ => (0 # 1)%Q
  end.

Definition mad_frame_mute_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-33 0*) F_max0_monotonic (F_check_ge (36
                                                             - (s IDmad_frame_mute_s)) (35
                                                                    - (s IDmad_frame_mute_s)));
                     (*-33 0*) F_max0_ge_0 (35 - (s IDmad_frame_mute_s))]
    | 10%positive => []
    | 11%positive => [(*-594 0*) F_one]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => [(*-33 0*) F_max0_monotonic (F_check_ge (18
                                                              - (s IDmad_frame_mute_s)) (17
                                                                    - (s IDmad_frame_mute_s)));
                      (*-33 0*) F_max0_ge_0 (17 - (s IDmad_frame_mute_s))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => [(*-33 0*) F_max0_pre_decrement (18
                                                      - (s IDmad_frame_mute_s)) (1)]
    | 24%positive => []
    | 25%positive => []
    | 26%positive => []
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => [(*0 1*) F_max0_monotonic (F_check_ge (32
                                                            - (s IDmad_frame_mute_sb)) (31
                                                                    - (s IDmad_frame_mute_sb)));
                      (*-1 0*) F_max0_ge_0 (31 - (s IDmad_frame_mute_sb))]
    | 32%positive => [(*-1 0*) F_max0_pre_decrement (32
                                                     - (s IDmad_frame_mute_sb)) (1)]
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => []
    | 38%positive => []
    | 39%positive => []
    | 40%positive => []
    | 41%positive => []
    | 42%positive => [(*-33 0*) F_max0_pre_decrement (36
                                                      - (s IDmad_frame_mute_s)) (1)]
    | 43%positive => []
    | 44%positive => []
    | 45%positive => []
    | 46%positive => []
    | 47%positive => []
    | 48%positive => []
    | 49%positive => []
    | 50%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (32
                                                                 - (s IDmad_frame_mute_sb))) (F_check_ge (0) (0))]
    | 51%positive => [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (32
                                                                  - (s IDmad_frame_mute_sb))) (F_check_ge (32
                                                                    - (s IDmad_frame_mute_sb)) (0))]
    | 52%positive => []
    | 53%positive => []
    | 54%positive => []
    | 55%positive => []
    | 56%positive => []
    | 57%positive => [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (32
                                                                    - (s IDmad_frame_mute_sb)) (0))) (F_max0_ge_0 (32
                                                                    - (s IDmad_frame_mute_sb)))]
    | _ => []
  end.


Theorem mad_frame_mute_ai_correct:
  forall s p' s', steps (g_start mad_frame_mute) s (g_edges mad_frame_mute) p' s' -> mad_frame_mute_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem mad_frame_mute_pot_correct:
  forall s p' s',
    steps (g_start mad_frame_mute) s (g_edges mad_frame_mute) p' s' ->
    (mad_frame_mute_pot (g_start mad_frame_mute) s >= mad_frame_mute_pot p' s')%Q.
Proof.
  check_lp mad_frame_mute_ai_correct mad_frame_mute_hints.
Qed.

