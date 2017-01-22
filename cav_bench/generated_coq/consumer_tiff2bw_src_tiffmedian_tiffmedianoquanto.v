Require Import pasta.Pasta.

Notation IDquant_z := 1%positive.
Notation IDquant_blue := 2%positive.
Notation IDquant_green := 3%positive.
Notation IDquant_i := 4%positive.
Notation IDquant_imagelength := 5%positive.
Notation IDquant_imagewidth := 6%positive.
Notation IDquant_j := 7%positive.
Notation IDquant_red := 8%positive.
Notation IDquant_in := 9%positive.
Notation IDquant_out := 10%positive.
Definition quant : graph := {|
  g_start := 1%positive;
  g_end := 40%positive;
  g_edges := (1%positive,(AAssign IDquant_z (Some (ENum (0)))),2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDquant_j) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDquant_imagewidth)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,(AGuard (fun s => ((eval (EVar IDquant_imagelength)
             s) >= (eval (ENum (0)) s))%Z)),5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDquant_i) s) >=
             (eval (ENum (0)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDquant_i (Some (ENum (0)))),8%positive)::
             (8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDquant_i) s) <
             (eval (EVar IDquant_imagelength) s))%Z)),12%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDquant_i) s) >=
             (eval (EVar IDquant_imagelength) s))%Z)),11%positive)::
             (11%positive,AWeaken,40%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,38%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDquant_j (Some (ENum (0)))),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,AWeaken,17%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDquant_j) s) <
             (eval (EVar IDquant_imagewidth) s))%Z)),28%positive)::
             (17%positive,(AGuard (fun s => ((eval (EVar IDquant_j) s) >=
             (eval (EVar IDquant_imagewidth) s))%Z)),18%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,26%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,21%positive)::
             (21%positive,(AAssign IDquant_i (Some (EAdd (EVar IDquant_i)
             (ENum (1))))),22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDquant_z (Some (EAdd (ENum (1))
             (EVar IDquant_z)))),25%positive)::
             (25%positive,AWeaken,10%positive)::
             (26%positive,ANone,27%positive)::
             (27%positive,AWeaken,40%positive)::
             (28%positive,AWeaken,29%positive)::
             (29%positive,(AAssign IDquant_red None),30%positive)::
             (30%positive,(AAssign IDquant_green None),31%positive)::
             (31%positive,(AAssign IDquant_blue None),32%positive)::
             (32%positive,ANone,33%positive)::
             (33%positive,(AAssign IDquant_j (Some (EAdd (EVar IDquant_j)
             (ENum (1))))),34%positive)::(34%positive,ANone,35%positive)::
             (35%positive,ANone,36%positive)::
             (36%positive,(AAssign IDquant_z (Some (EAdd (ENum (1))
             (EVar IDquant_z)))),37%positive)::
             (37%positive,AWeaken,17%positive)::
             (38%positive,ANone,39%positive)::
             (39%positive,AWeaken,40%positive)::nil
|}.

Definition quant_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_z) <= 0)%Z
    | 3%positive => (-1 * (s IDquant_z) <= 0 /\ 1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0)%Z
    | 4%positive => (-1 * (s IDquant_j) <= 0 /\ 1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_imagewidth) <= 0)%Z
    | 5%positive => (-1 * (s IDquant_imagewidth) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ 1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_imagelength) <= 0)%Z
    | 6%positive => (-1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ 1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_imagewidth) <= 0 /\ -1 * (s IDquant_i) <= 0)%Z
    | 7%positive => (-1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagewidth) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ 1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_imagelength) <= 0)%Z
    | 8%positive => (-1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ 1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_imagewidth) <= 0 /\ 1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_i) <= 0)%Z
    | 9%positive => (-1 * (s IDquant_i) <= 0 /\ 1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagewidth) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ 1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_imagelength) <= 0)%Z
    | 10%positive => (-1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagewidth) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) <= 0)%Z
    | 11%positive => (1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_imagewidth) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_i)+ 1 * (s IDquant_imagelength) <= 0)%Z
    | 12%positive => (-1 * (s IDquant_imagewidth) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0)%Z
    | 13%positive => (1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagewidth) <= 0)%Z
    | 14%positive => (-1 * (s IDquant_imagewidth) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0)%Z
    | 15%positive => (1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagewidth) <= 0 /\ 1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_j) <= 0)%Z
    | 16%positive => (-1 * (s IDquant_j) <= 0 /\ 1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_imagewidth) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0)%Z
    | 18%positive => (-1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ 1 * (s IDquant_imagewidth)+ -1 * (s IDquant_j) <= 0)%Z
    | 19%positive => (1 * (s IDquant_imagewidth)+ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0)%Z
    | 20%positive => (-1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ 1 * (s IDquant_imagewidth)+ -1 * (s IDquant_j) <= 0)%Z
    | 21%positive => (1 * (s IDquant_imagewidth)+ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0)%Z
    | 22%positive => (-1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ 1 * (s IDquant_imagewidth)+ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_i) + 1 <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) <= 0)%Z
    | 23%positive => (1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_i) + 1 <= 0 /\ 1 * (s IDquant_imagewidth)+ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0)%Z
    | 24%positive => (-1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ 1 * (s IDquant_imagewidth)+ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_i) + 1 <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) <= 0)%Z
    | 25%positive => (1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_i) + 1 <= 0 /\ 1 * (s IDquant_imagewidth)+ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ 1 * (s IDquant_imagewidth)+ -1 * (s IDquant_j) <= 0)%Z
    | 27%positive => (1 * (s IDquant_imagewidth)+ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0)%Z
    | 28%positive => (1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) + 1 <= 0)%Z
    | 29%positive => (-1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) + 1 <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0)%Z
    | 30%positive => (1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) + 1 <= 0)%Z
    | 31%positive => (-1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) + 1 <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0)%Z
    | 32%positive => (1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) + 1 <= 0)%Z
    | 33%positive => (-1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) + 1 <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0)%Z
    | 34%positive => (1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_j) + 1 <= 0)%Z
    | 35%positive => (-1 * (s IDquant_j) + 1 <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0)%Z
    | 36%positive => (1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_j) + 1 <= 0)%Z
    | 37%positive => (-1 * (s IDquant_j) + 1 <= 0 /\ -1 * (s IDquant_imagewidth)+ 1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagelength) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_z) + 1 <= 0)%Z
    | 38%positive => (-1 * (s IDquant_imagewidth) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0 /\ 1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0)%Z
    | 39%positive => (1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) + 1 <= 0 /\ -1 * (s IDquant_j) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_imagewidth) <= 0)%Z
    | 40%positive => (1 * (s IDquant_i)+ -1 * (s IDquant_imagelength) <= 0 /\ -1 * (s IDquant_imagewidth) <= 0 /\ -1 * (s IDquant_i) <= 0 /\ -1 * (s IDquant_z) <= 0 /\ -1 * (s IDquant_j) <= 0)%Z
    | _ => False
  end.

Definition quant_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDquant_imagelength))
                     + max0((s IDquant_imagelength)) * max0((s IDquant_imagewidth)))%Q
    | 2%positive => ((s IDquant_z) + max0((s IDquant_imagelength))
                     + max0((s IDquant_imagelength)) * max0((s IDquant_imagewidth)))%Q
    | 3%positive => ((s IDquant_z) + max0((s IDquant_imagelength))
                     + max0((s IDquant_imagelength)) * max0((s IDquant_imagewidth)))%Q
    | 4%positive => ((s IDquant_z) + max0((s IDquant_imagelength))
                     + max0((s IDquant_imagelength)) * max0((s IDquant_imagewidth)))%Q
    | 5%positive => ((s IDquant_z) + max0((s IDquant_imagelength))
                     + max0((s IDquant_imagelength)) * max0((s IDquant_imagewidth)))%Q
    | 6%positive => ((s IDquant_z) + max0((s IDquant_imagelength))
                     + max0((s IDquant_imagelength)) * max0((s IDquant_imagewidth)))%Q
    | 7%positive => ((69 # 100) * (s IDquant_imagelength)
                     - (69 # 100) * (s IDquant_imagelength)^2 + (s IDquant_z)
                     + (31 # 100) * max0((s IDquant_imagelength))
                     + max0((s IDquant_imagelength)) * max0((s IDquant_imagewidth))
                     + (69 # 100) * max0((s IDquant_imagelength))^2)%Q
    | 8%positive => (-(128 # 79) * (s IDquant_i)
                     + (109 # 79) * (s IDquant_i) * (s IDquant_imagelength)
                     + (7 # 43) * (s IDquant_i) * max0(-1 + (s IDquant_i))
                     + (3 # 43) * (s IDquant_i) * max0(-1
                                                       + (s IDquant_imagelength))
                     + (5 # 86) * (s IDquant_i) * max0(-(s IDquant_i)
                                                       + (s IDquant_imagelength))
                     + (7 # 43) * (s IDquant_i) * max0((s IDquant_i))
                     - (11 # 86) * (s IDquant_i) * max0((s IDquant_imagelength))
                     - (69 # 100) * (s IDquant_i)^2
                     + (69 # 100) * (s IDquant_imagelength)
                     + (14 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  - (s IDquant_i)
                                                                  + (s IDquant_imagelength))
                     - (14 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                     - (3 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                 + (s IDquant_imagelength))
                     + (11 # 86) * (s IDquant_imagelength) * max0((s IDquant_i))
                     + (3 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                     - (69 # 100) * (s IDquant_imagelength)^2 + (s IDquant_z)
                     - (14 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                     - (7 # 43) * max0(-1 + (s IDquant_i)) * max0((s IDquant_i))
                     - (8 # 43) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                     - (11 # 43) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_i))
                     + (22 # 43) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                     - (49 # 79) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength))
                     - (11 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength)) * max0((s IDquant_i))
                     + (10 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                     + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth))
                     + (32 # 65) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength))^2
                     - (7 # 43) * max0((s IDquant_i))^2
                     + (40 # 43) * max0((s IDquant_imagelength))
                     - (3 # 86) * max0((s IDquant_imagelength))^2)%Q
    | 9%positive => (-(128 # 79) * (s IDquant_i)
                     + (109 # 79) * (s IDquant_i) * (s IDquant_imagelength)
                     + (7 # 43) * (s IDquant_i) * max0(-1 + (s IDquant_i))
                     + (3 # 43) * (s IDquant_i) * max0(-1
                                                       + (s IDquant_imagelength))
                     + (5 # 86) * (s IDquant_i) * max0(-(s IDquant_i)
                                                       + (s IDquant_imagelength))
                     + (7 # 43) * (s IDquant_i) * max0((s IDquant_i))
                     - (11 # 86) * (s IDquant_i) * max0((s IDquant_imagelength))
                     - (69 # 100) * (s IDquant_i)^2
                     + (69 # 100) * (s IDquant_imagelength)
                     + (14 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  - (s IDquant_i)
                                                                  + (s IDquant_imagelength))
                     - (14 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                     - (3 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                 + (s IDquant_imagelength))
                     + (11 # 86) * (s IDquant_imagelength) * max0((s IDquant_i))
                     + (3 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                     - (69 # 100) * (s IDquant_imagelength)^2 + (s IDquant_z)
                     - (14 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                     - (7 # 43) * max0(-1 + (s IDquant_i)) * max0((s IDquant_i))
                     - (8 # 43) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                     - (11 # 43) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_i))
                     + (22 # 43) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                     - (49 # 79) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength))
                     - (11 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength)) * max0((s IDquant_i))
                     + (10 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                     + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth))
                     + (32 # 65) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength))^2
                     - (7 # 43) * max0((s IDquant_i))^2
                     + (40 # 43) * max0((s IDquant_imagelength))
                     - (3 # 86) * max0((s IDquant_imagelength))^2)%Q
    | 10%positive => (-(33 # 43) * (s IDquant_i)
                      - (14 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (7 # 43) * (s IDquant_i) * max0(-1 + (s IDquant_i))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (31 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (7 # 43) * (s IDquant_i) * max0((s IDquant_i))
                      - (11 # 86) * (s IDquant_i) * max0((s IDquant_imagelength))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      + (7 # 43) * (s IDquant_i)^2
                      + (36 # 43) * (s IDquant_imagelength)
                      + (14 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (14 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (30 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      + (11 # 86) * (s IDquant_imagelength) * max0((s IDquant_i))
                      + (3 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      + (7 # 43) * (s IDquant_imagelength)^2 + (s IDquant_z)
                      - (14 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (7 # 43) * max0(-1 + (s IDquant_i)) * max0((s IDquant_i))
                      - (8 # 43) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (11 # 43) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (22 # 43) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (10 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (10 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (13 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))^2
                      - (7 # 43) * max0((s IDquant_i))^2
                      - (3 # 43) * max0((s IDquant_imagelength))
                      - (3 # 86) * max0((s IDquant_imagelength))^2)%Q
    | 11%positive => (-(33 # 43) * (s IDquant_i)
                      - (14 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (7 # 43) * (s IDquant_i) * max0(-1 + (s IDquant_i))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (31 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (7 # 43) * (s IDquant_i) * max0((s IDquant_i))
                      - (11 # 86) * (s IDquant_i) * max0((s IDquant_imagelength))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      + (7 # 43) * (s IDquant_i)^2
                      + (36 # 43) * (s IDquant_imagelength)
                      + (14 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (14 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (30 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      + (11 # 86) * (s IDquant_imagelength) * max0((s IDquant_i))
                      + (3 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      + (7 # 43) * (s IDquant_imagelength)^2 + (s IDquant_z)
                      - (14 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (7 # 43) * max0(-1 + (s IDquant_i)) * max0((s IDquant_i))
                      - (8 # 43) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (11 # 43) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (22 # 43) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (10 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (10 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (13 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))^2
                      - (7 # 43) * max0((s IDquant_i))^2
                      - (3 # 43) * max0((s IDquant_imagelength))
                      - (3 # 86) * max0((s IDquant_imagelength))^2)%Q
    | 12%positive => (-(33 # 43) * (s IDquant_i)
                      - (14 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (7 # 43) * (s IDquant_i) * max0(-1 + (s IDquant_i))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (31 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (7 # 43) * (s IDquant_i) * max0((s IDquant_i))
                      - (11 # 86) * (s IDquant_i) * max0((s IDquant_imagelength))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      + (7 # 43) * (s IDquant_i)^2
                      + (36 # 43) * (s IDquant_imagelength)
                      + (14 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (14 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (30 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      + (11 # 86) * (s IDquant_imagelength) * max0((s IDquant_i))
                      + (3 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      + (7 # 43) * (s IDquant_imagelength)^2 + (s IDquant_z)
                      - (14 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (7 # 43) * max0(-1 + (s IDquant_i)) * max0((s IDquant_i))
                      - (8 # 43) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (11 # 43) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (22 # 43) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (10 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (10 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (13 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))^2
                      - (7 # 43) * max0((s IDquant_i))^2
                      - (3 # 43) * max0((s IDquant_imagelength))
                      - (3 # 86) * max0((s IDquant_imagelength))^2)%Q
    | 13%positive => (-(3 # 43) - (52 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (25 # 86) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      - (19 # 121) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (17 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                      + (s IDquant_imagelength))
                      + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (23 # 86) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (19 # 121) * max0(-1 - (s IDquant_i)
                                          + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (10 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (7 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (11 # 43) * max0((s IDquant_i)))%Q
    | 14%positive => (-(3 # 43) - (52 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (25 # 86) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      - (19 # 121) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (17 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                      + (s IDquant_imagelength))
                      + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (23 # 86) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (19 # 121) * max0(-1 - (s IDquant_i)
                                          + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (10 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (7 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (11 # 43) * max0((s IDquant_i)))%Q
    | 15%positive => (-(3 # 43) - (52 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (25 # 86) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      - (19 # 121) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      + (1 # 2) * (s IDquant_i) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (17 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                      + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_imagewidth) * max0((s IDquant_imagewidth)
                                                                - (s IDquant_j))
                      - (3 # 4) * (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j) * max0(-(s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      + (1 # 4) * (s IDquant_j) * max0((s IDquant_j))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (23 # 86) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (19 # 121) * max0(-1 - (s IDquant_i)
                                          + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_imagewidth))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      - (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (10 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (7 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 4) * max0(-(s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (11 # 43) * max0((s IDquant_i))
                      - (1 # 2) * max0((s IDquant_i)) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2
                      + (1 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 16%positive => (-(3 # 43) - (52 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (25 # 86) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      - (19 # 121) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      + (1 # 2) * (s IDquant_i) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (17 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                      + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_imagewidth) * max0((s IDquant_imagewidth)
                                                                - (s IDquant_j))
                      - (3 # 4) * (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j) * max0(-(s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      + (1 # 4) * (s IDquant_j) * max0((s IDquant_j))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (23 # 86) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (19 # 121) * max0(-1 - (s IDquant_i)
                                          + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_imagewidth))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      - (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (10 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (7 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 4) * max0(-(s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (11 # 43) * max0((s IDquant_i))
                      - (1 # 2) * max0((s IDquant_i)) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2
                      + (1 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 17%positive => (-(3 # 43) - (55 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (11 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      + (1 # 2) * (s IDquant_i) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (31 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (7 # 43) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (s IDquant_imagewidth)
                      - (1 # 4) * (s IDquant_imagewidth) * max0(-1
                                                                - (s IDquant_i)
                                                                + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_imagewidth) * max0((s IDquant_imagewidth)
                                                                - (s IDquant_j))
                      - (3 # 4) * (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j) * max0(-(s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      + (1 # 4) * (s IDquant_j) * max0((s IDquant_j))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (10 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      - (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (17 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (31 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 4) * max0(-(s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (14 # 43) * max0((s IDquant_i))
                      - (1 # 2) * max0((s IDquant_i)) * max0((s IDquant_j))
                      - (3 # 86) * max0((s IDquant_imagelength))
                      - max0((s IDquant_imagewidth))
                      - (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2
                      + (1 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 18%positive => (-(3 # 43) - (55 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (11 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      + (1 # 2) * (s IDquant_i) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (31 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (7 # 43) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (s IDquant_imagewidth)
                      - (1 # 4) * (s IDquant_imagewidth) * max0(-1
                                                                - (s IDquant_i)
                                                                + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_imagewidth) * max0((s IDquant_imagewidth)
                                                                - (s IDquant_j))
                      - (3 # 4) * (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j) * max0(-(s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      + (1 # 4) * (s IDquant_j) * max0((s IDquant_j))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (10 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      - (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (17 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (31 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 4) * max0(-(s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (14 # 43) * max0((s IDquant_i))
                      - (1 # 2) * max0((s IDquant_i)) * max0((s IDquant_j))
                      - (3 # 86) * max0((s IDquant_imagelength))
                      - max0((s IDquant_imagewidth))
                      - (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2
                      + (1 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 19%positive => (-(3 # 43) - (113 # 86) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      - (9 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (11 # 86) * (s IDquant_i) * max0(-1
                                                         + (s IDquant_imagelength))
                      + (7 # 43) * (s IDquant_i) * max0((s IDquant_i))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      - (16 # 43) * (s IDquant_i)^2
                      + (127 # 86) * (s IDquant_imagelength)
                      + (23 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (11 # 86) * (s IDquant_imagelength) * max0(-1
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (17 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_imagelength)^2
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j)
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      - (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-1 - (s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (7 # 43) * max0((s IDquant_i))
                      - max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_j))
                      + max0((s IDquant_imagewidth) - (s IDquant_j))
                      + (3 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 20%positive => (-(3 # 43) - (113 # 86) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      - (9 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (11 # 86) * (s IDquant_i) * max0(-1
                                                         + (s IDquant_imagelength))
                      + (7 # 43) * (s IDquant_i) * max0((s IDquant_i))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      - (16 # 43) * (s IDquant_i)^2
                      + (127 # 86) * (s IDquant_imagelength)
                      + (23 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (11 # 86) * (s IDquant_imagelength) * max0(-1
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (17 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_imagelength)^2
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j)
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      - (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-1 - (s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (7 # 43) * max0((s IDquant_i))
                      - max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_j))
                      + max0((s IDquant_imagewidth) - (s IDquant_j))
                      + (3 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 21%positive => (-(3 # 43) - (113 # 86) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      - (9 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (11 # 86) * (s IDquant_i) * max0(-1
                                                         + (s IDquant_imagelength))
                      + (7 # 43) * (s IDquant_i) * max0((s IDquant_i))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      - (16 # 43) * (s IDquant_i)^2
                      + (127 # 86) * (s IDquant_imagelength)
                      + (23 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (11 # 86) * (s IDquant_imagelength) * max0(-1
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (17 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_imagelength)^2
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j)
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      - (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-1 - (s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (7 # 43) * max0((s IDquant_i))
                      - max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_j))
                      + max0((s IDquant_imagewidth) - (s IDquant_j))
                      + (3 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 22%positive => ((75 # 86) - (49 # 86) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (7 # 43) * (s IDquant_i) * max0(-1 + (s IDquant_i))
                      + (11 # 86) * (s IDquant_i) * max0(-1
                                                         + (s IDquant_imagelength))
                      - (9 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      - (16 # 43) * (s IDquant_i)^2
                      + (91 # 86) * (s IDquant_imagelength)
                      - (11 # 86) * (s IDquant_imagelength) * max0(-1
                                                                   + 
                                                                   (s IDquant_imagelength))
                      + (23 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (17 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_imagelength)^2
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j)
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      - (11 # 86) * max0(-1 + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (6 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength))
                      - (14 # 73) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      - (1 # 170) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_j))
                      + (3 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 23%positive => ((75 # 86) - (49 # 86) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (7 # 43) * (s IDquant_i) * max0(-1 + (s IDquant_i))
                      + (11 # 86) * (s IDquant_i) * max0(-1
                                                         + (s IDquant_imagelength))
                      - (9 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      - (16 # 43) * (s IDquant_i)^2
                      + (91 # 86) * (s IDquant_imagelength)
                      - (11 # 86) * (s IDquant_imagelength) * max0(-1
                                                                   + 
                                                                   (s IDquant_imagelength))
                      + (23 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (17 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_imagelength)^2
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j)
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      - (11 # 86) * max0(-1 + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (6 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength))
                      - (14 # 73) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      - (1 # 170) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_j))
                      + (3 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 24%positive => ((75 # 86) - (49 # 86) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (7 # 43) * (s IDquant_i) * max0(-1 + (s IDquant_i))
                      + (11 # 86) * (s IDquant_i) * max0(-1
                                                         + (s IDquant_imagelength))
                      - (9 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      - (16 # 43) * (s IDquant_i)^2
                      + (91 # 86) * (s IDquant_imagelength)
                      - (11 # 86) * (s IDquant_imagelength) * max0(-1
                                                                   + 
                                                                   (s IDquant_imagelength))
                      + (23 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (17 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_imagelength)^2
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j)
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      - (11 # 86) * max0(-1 + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (6 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength))
                      - (14 # 73) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      - (1 # 170) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_j))
                      + (3 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 25%positive => (-(11 # 86) - (49 # 86) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (7 # 43) * (s IDquant_i) * max0(-1 + (s IDquant_i))
                      + (11 # 86) * (s IDquant_i) * max0(-1
                                                         + (s IDquant_imagelength))
                      - (9 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      - (16 # 43) * (s IDquant_i)^2
                      + (91 # 86) * (s IDquant_imagelength)
                      - (11 # 86) * (s IDquant_imagelength) * max0(-1
                                                                   + 
                                                                   (s IDquant_imagelength))
                      + (23 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (17 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_imagelength)^2
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j)
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      - (11 # 86) * max0(-1 + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (6 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength))
                      - (14 # 73) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      - (1 # 170) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_j))
                      + (3 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 26%positive => (-(3 # 43) - (113 # 86) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      - (9 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (11 # 86) * (s IDquant_i) * max0(-1
                                                         + (s IDquant_imagelength))
                      + (7 # 43) * (s IDquant_i) * max0((s IDquant_i))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      - (16 # 43) * (s IDquant_i)^2
                      + (127 # 86) * (s IDquant_imagelength)
                      + (23 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (11 # 86) * (s IDquant_imagelength) * max0(-1
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (17 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_imagelength)^2
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j)
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      - (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-1 - (s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (7 # 43) * max0((s IDquant_i))
                      - max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_j))
                      + max0((s IDquant_imagewidth) - (s IDquant_j))
                      + (3 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 27%positive => (-(3 # 43) - (113 # 86) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      - (9 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (11 # 86) * (s IDquant_i) * max0(-1
                                                         + (s IDquant_imagelength))
                      + (7 # 43) * (s IDquant_i) * max0((s IDquant_i))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      - (16 # 43) * (s IDquant_i)^2
                      + (127 # 86) * (s IDquant_imagelength)
                      + (23 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (11 # 86) * (s IDquant_imagelength) * max0(-1
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (17 # 86) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_imagelength)^2
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j)
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      - (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-1 - (s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (7 # 43) * max0((s IDquant_i))
                      - max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_j))
                      + max0((s IDquant_imagewidth) - (s IDquant_j))
                      + (3 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 28%positive => (-(3 # 43) - (55 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (11 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (s IDquant_i) * max0((s IDquant_imagewidth)
                                             - (s IDquant_j))
                      + (1 # 2) * (s IDquant_i) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (31 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (7 # 43) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (s IDquant_imagelength) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      - (1 # 2) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (s IDquant_imagewidth)
                      - (1 # 4) * (s IDquant_imagewidth) * max0(-1
                                                                - (s IDquant_i)
                                                                + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_imagewidth) * max0((s IDquant_imagewidth)
                                                                - (s IDquant_j))
                      - (3 # 4) * (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j) * max0(-(s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_j) * max0((s IDquant_imagewidth)
                                                       - (s IDquant_j))
                      + (1 # 4) * (s IDquant_j) * max0((s IDquant_j))
                      + (1 # 4) * (s IDquant_j)^2 + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (10 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      - (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (17 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (31 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + max0(-(s IDquant_i) + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 4) * max0(-(s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (14 # 43) * max0((s IDquant_i))
                      - (1 # 2) * max0((s IDquant_i)) * max0((s IDquant_j))
                      - (3 # 86) * max0((s IDquant_imagelength))
                      - max0((s IDquant_imagewidth))
                      - (1 # 4) * max0((s IDquant_imagewidth)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2
                      + (1 # 4) * max0((s IDquant_j))
                      - (1 # 4) * max0((s IDquant_j))^2)%Q
    | 29%positive => (-(3 # 43) - (55 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (11 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_i) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (31 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (7 # 43) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (1 # 4) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (7 # 4) * (s IDquant_imagewidth)
                      + (1 # 2) * (s IDquant_imagewidth) * (s IDquant_j)
                      - (3 # 4) * (s IDquant_imagewidth) * max0(-1
                                                                - (s IDquant_i)
                                                                + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                                + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_imagewidth) * max0((s IDquant_i))
                      - (1 # 4) * (s IDquant_imagewidth)^2 - (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_j) * max0((s IDquant_i))
                      - (1 # 2) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (s IDquant_z)
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength))
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (10 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (17 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (31 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (14 # 43) * max0((s IDquant_i))
                      - (1 # 2) * max0((s IDquant_i)) * max0((s IDquant_j))
                      - (3 # 86) * max0((s IDquant_imagelength))
                      - (3 # 2) * max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2
                      + (1 # 4) * max0((s IDquant_j)))%Q
    | 30%positive => (-(3 # 43) - (55 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (11 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_i) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (31 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (7 # 43) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (1 # 4) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (7 # 4) * (s IDquant_imagewidth)
                      + (1 # 2) * (s IDquant_imagewidth) * (s IDquant_j)
                      - (3 # 4) * (s IDquant_imagewidth) * max0(-1
                                                                - (s IDquant_i)
                                                                + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                                + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_imagewidth) * max0((s IDquant_i))
                      - (1 # 4) * (s IDquant_imagewidth)^2 - (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_j) * max0((s IDquant_i))
                      - (1 # 2) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (s IDquant_z)
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength))
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (10 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (17 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (31 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (14 # 43) * max0((s IDquant_i))
                      - (1 # 2) * max0((s IDquant_i)) * max0((s IDquant_j))
                      - (3 # 86) * max0((s IDquant_imagelength))
                      - (3 # 2) * max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2
                      + (1 # 4) * max0((s IDquant_j)))%Q
    | 31%positive => (-(3 # 43) - (55 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (11 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_i) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (31 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (7 # 43) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (1 # 4) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (7 # 4) * (s IDquant_imagewidth)
                      + (1 # 2) * (s IDquant_imagewidth) * (s IDquant_j)
                      - (3 # 4) * (s IDquant_imagewidth) * max0(-1
                                                                - (s IDquant_i)
                                                                + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                                + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_imagewidth) * max0((s IDquant_i))
                      - (1 # 4) * (s IDquant_imagewidth)^2 - (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_j) * max0((s IDquant_i))
                      - (1 # 2) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (s IDquant_z)
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength))
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (10 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (17 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (31 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (14 # 43) * max0((s IDquant_i))
                      - (1 # 2) * max0((s IDquant_i)) * max0((s IDquant_j))
                      - (3 # 86) * max0((s IDquant_imagelength))
                      - (3 # 2) * max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2
                      + (1 # 4) * max0((s IDquant_j)))%Q
    | 32%positive => (-(3 # 43) - (55 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (11 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_i) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (31 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (7 # 43) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (1 # 4) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (7 # 4) * (s IDquant_imagewidth)
                      + (1 # 2) * (s IDquant_imagewidth) * (s IDquant_j)
                      - (3 # 4) * (s IDquant_imagewidth) * max0(-1
                                                                - (s IDquant_i)
                                                                + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                                + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_imagewidth) * max0((s IDquant_i))
                      - (1 # 4) * (s IDquant_imagewidth)^2 - (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_j) * max0((s IDquant_i))
                      - (1 # 2) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (s IDquant_z)
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength))
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (10 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (17 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (31 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (14 # 43) * max0((s IDquant_i))
                      - (1 # 2) * max0((s IDquant_i)) * max0((s IDquant_j))
                      - (3 # 86) * max0((s IDquant_imagelength))
                      - (3 # 2) * max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2
                      + (1 # 4) * max0((s IDquant_j)))%Q
    | 33%positive => (-(3 # 43) - (55 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (11 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      + (1 # 4) * (s IDquant_i) * max0((s IDquant_j))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (31 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (7 # 43) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (1 # 4) * (s IDquant_imagelength) * max0((s IDquant_j))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (7 # 4) * (s IDquant_imagewidth)
                      + (1 # 2) * (s IDquant_imagewidth) * (s IDquant_j)
                      - (3 # 4) * (s IDquant_imagewidth) * max0(-1
                                                                - (s IDquant_i)
                                                                + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                                + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_imagewidth) * max0((s IDquant_i))
                      - (1 # 4) * (s IDquant_imagewidth)^2 - (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_j) * max0((s IDquant_i))
                      - (1 # 2) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (s IDquant_z)
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength))
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (10 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_j))
                      + (17 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (31 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (14 # 43) * max0((s IDquant_i))
                      - (1 # 2) * max0((s IDquant_i)) * max0((s IDquant_j))
                      - (3 # 86) * max0((s IDquant_imagelength))
                      - (3 # 2) * max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2
                      + (1 # 4) * max0((s IDquant_j)))%Q
    | 34%positive => ((40 # 43) - (55 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (1 # 4) * (s IDquant_i) * max0(-1 + (s IDquant_j))
                      + (11 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagelength) * max0(-1
                                                                 + (s IDquant_j))
                      - (31 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (7 # 43) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (5 # 4) * (s IDquant_imagewidth)
                      + (1 # 2) * (s IDquant_imagewidth) * (s IDquant_j)
                      - (3 # 4) * (s IDquant_imagewidth) * max0(-1
                                                                - (s IDquant_i)
                                                                + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                                + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_imagewidth) * max0((s IDquant_i))
                      - (1 # 4) * (s IDquant_imagewidth)^2 - (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_j) * max0((s IDquant_i))
                      - (1 # 2) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_j))
                      + (10 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0(-1 + (s IDquant_j))
                      - (1 # 2) * max0(-1 + (s IDquant_j)) * max0((s IDquant_i))
                      + (17 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (31 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (15 # 86) * max0((s IDquant_i))
                      - (3 # 86) * max0((s IDquant_imagelength))
                      - max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2)%Q
    | 35%positive => ((40 # 43) - (55 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (1 # 4) * (s IDquant_i) * max0(-1 + (s IDquant_j))
                      + (11 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagelength) * max0(-1
                                                                 + (s IDquant_j))
                      - (31 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (7 # 43) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (5 # 4) * (s IDquant_imagewidth)
                      + (1 # 2) * (s IDquant_imagewidth) * (s IDquant_j)
                      - (3 # 4) * (s IDquant_imagewidth) * max0(-1
                                                                - (s IDquant_i)
                                                                + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                                + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_imagewidth) * max0((s IDquant_i))
                      - (1 # 4) * (s IDquant_imagewidth)^2 - (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_j) * max0((s IDquant_i))
                      - (1 # 2) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_j))
                      + (10 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0(-1 + (s IDquant_j))
                      - (1 # 2) * max0(-1 + (s IDquant_j)) * max0((s IDquant_i))
                      + (17 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (31 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (15 # 86) * max0((s IDquant_i))
                      - (3 # 86) * max0((s IDquant_imagelength))
                      - max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2)%Q
    | 36%positive => ((40 # 43) - (55 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (1 # 4) * (s IDquant_i) * max0(-1 + (s IDquant_j))
                      + (11 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagelength) * max0(-1
                                                                 + (s IDquant_j))
                      - (31 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (7 # 43) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (5 # 4) * (s IDquant_imagewidth)
                      + (1 # 2) * (s IDquant_imagewidth) * (s IDquant_j)
                      - (3 # 4) * (s IDquant_imagewidth) * max0(-1
                                                                - (s IDquant_i)
                                                                + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                                + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_imagewidth) * max0((s IDquant_i))
                      - (1 # 4) * (s IDquant_imagewidth)^2 - (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_j) * max0((s IDquant_i))
                      - (1 # 2) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_j))
                      + (10 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0(-1 + (s IDquant_j))
                      - (1 # 2) * max0(-1 + (s IDquant_j)) * max0((s IDquant_i))
                      + (17 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (31 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (15 # 86) * max0((s IDquant_i))
                      - (3 # 86) * max0((s IDquant_imagelength))
                      - max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2)%Q
    | 37%positive => (-(3 # 43) - (55 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (1 # 4) * (s IDquant_i) * max0(-1 + (s IDquant_j))
                      + (11 # 43) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      + (1 # 170) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (s IDquant_i) * max0((s IDquant_imagewidth))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagelength) * max0(-1
                                                                 + (s IDquant_j))
                      - (31 # 86) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (7 # 43) * (s IDquant_imagelength) * max0((s IDquant_imagelength))
                      + (s IDquant_imagelength) * max0((s IDquant_imagewidth))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (5 # 4) * (s IDquant_imagewidth)
                      + (1 # 2) * (s IDquant_imagewidth) * (s IDquant_j)
                      - (3 # 4) * (s IDquant_imagewidth) * max0(-1
                                                                - (s IDquant_i)
                                                                + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_imagewidth) * max0(-1
                                                                + (s IDquant_imagelength))
                      - (1 # 4) * (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                                + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_imagewidth) * max0((s IDquant_i))
                      - (1 # 4) * (s IDquant_imagewidth)^2 - (s IDquant_j)
                      + (1 # 2) * (s IDquant_j) * max0(-1 - (s IDquant_i)
                                                       + (s IDquant_imagelength))
                      - (1 # 2) * (s IDquant_j) * max0(-1
                                                       + (s IDquant_imagelength))
                      + (1 # 2) * (s IDquant_j) * max0((s IDquant_i))
                      - (1 # 2) * (s IDquant_j) * max0((s IDquant_imagewidth))
                      + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (1 # 4) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_j))
                      + (10 # 43) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (14 # 73) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 2) * max0(-1 - (s IDquant_i)
                                       + (s IDquant_imagelength)) * max0((s IDquant_imagewidth)
                                                                    - (s IDquant_j))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength))
                      + (1 # 2) * max0(-1 + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_j))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (1 # 4) * max0(-1 + (s IDquant_j))
                      - (1 # 2) * max0(-1 + (s IDquant_j)) * max0((s IDquant_i))
                      + (17 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (31 # 86) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (15 # 86) * max0((s IDquant_i))
                      - (3 # 86) * max0((s IDquant_imagelength))
                      - max0((s IDquant_imagewidth))
                      + (1 # 4) * max0((s IDquant_imagewidth))^2)%Q
    | 38%positive => (-(3 # 43) - (52 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (25 # 86) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      - (19 # 121) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (17 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                      + (s IDquant_imagelength))
                      + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (23 # 86) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (19 # 121) * max0(-1 - (s IDquant_i)
                                          + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (10 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (7 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (11 # 43) * max0((s IDquant_i)))%Q
    | 39%positive => (-(3 # 43) - (52 # 43) * (s IDquant_i)
                      + (18 # 43) * (s IDquant_i) * (s IDquant_imagelength)
                      + (1 # 43) * (s IDquant_i) * max0(-1 - (s IDquant_i)
                                                        + (s IDquant_imagelength))
                      + (3 # 43) * (s IDquant_i) * max0(-1
                                                        + (s IDquant_imagelength))
                      + (25 # 86) * (s IDquant_i) * max0(-(s IDquant_i)
                                                         + (s IDquant_imagelength))
                      - (19 # 121) * (s IDquant_i) * max0((s IDquant_imagelength))
                      - (9 # 43) * (s IDquant_i)^2
                      + (113 # 86) * (s IDquant_imagelength)
                      + (10 # 43) * (s IDquant_imagelength) * max0(-1
                                                                   - 
                                                                   (s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (3 # 43) * (s IDquant_imagelength) * max0(-1
                                                                  + (s IDquant_imagelength))
                      - (17 # 43) * (s IDquant_imagelength) * max0(-(s IDquant_i)
                                                                   + 
                                                                   (s IDquant_imagelength))
                      - (21 # 86) * (s IDquant_imagelength)^2
                      + (s IDquant_imagewidth) * max0(-(s IDquant_i)
                                                      + (s IDquant_imagelength))
                      + (s IDquant_z)
                      + (3 # 43) * max0(-1 - (s IDquant_i)
                                        + (s IDquant_imagelength)) * max0(-1
                                                                    + (s IDquant_imagelength))
                      + (23 # 86) * max0(-1 - (s IDquant_i)
                                         + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      - (19 # 121) * max0(-1 - (s IDquant_i)
                                          + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      - (5 # 86) * max0(-1 + (s IDquant_imagelength)) * max0(-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))
                      + (14 # 73) * max0(-1 + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (10 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength))
                      - (11 # 43) * max0(-(s IDquant_i)
                                         + (s IDquant_imagelength)) * max0((s IDquant_i))
                      + (7 # 43) * max0(-(s IDquant_i)
                                        + (s IDquant_imagelength)) * max0((s IDquant_imagelength))
                      + (11 # 43) * max0((s IDquant_i)))%Q
    | 40%positive => ((s IDquant_z))%Q
    | _ => (0 # 1)%Q
  end.

Definition quant_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*0 0.689922*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*0 0.852713*) F_binom_monotonic 2 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0));
                     (*-0.662791 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                     (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0))]
    | 10%positive => []
    | 11%positive => [(*-0.534884 0*) F_max0_monotonic (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + 
                                                                    (s IDquant_imagelength)) (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)));
                      (*-0.534884 0*) F_max0_ge_0 (-1 - (s IDquant_i)
                                                   + (s IDquant_imagelength));
                      (*-0.302326 0*) F_binom_monotonic 2 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0));
                      (*-0.465116 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDquant_i)
                                                                    - (s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_i)
                                                                    - (s IDquant_imagelength)));
                      (*-0.465116 0*) F_binom_monotonic 2 (F_max0_ge_0 ((s IDquant_i)
                                                                    - (s IDquant_imagelength))) (F_check_ge (0) (0));
                      (*-0.186047 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.465116 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i)
                                                                    - (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*-0.162791 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_i))) (F_check_ge (0) (0)));
                      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.162791 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*-0.127907 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_i))) (F_check_ge ((s IDquant_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.465116 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)
                                                                    - (s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_i)
                                                                    - (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.127907 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*-0.0348837 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.511628 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.232558 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.0697674 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)))]
    | 12%positive => [(*0 0.0232558*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0));
                      (*-0.209302 0.186047*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)));
                      (*0 0.0348837*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)));
                      (*-0.0232558 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.267442 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.168605*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.0697674*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.127907 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*0 0.0639535*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.697674*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.0697674*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*0 0.162791*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_i))) (F_check_ge (0) (0)));
                      (*0 0.162791*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*0 0.127907*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.0542636 0.20155*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.127907*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagewidth))) (F_check_ge ((s IDquant_imagewidth)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.116279*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-0.0348837 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.0348837 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.197674 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagewidth))) (F_check_ge ((s IDquant_imagewidth)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagewidth))) (F_check_ge ((s IDquant_imagewidth)) (0));
                      (*0 0.0697674*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))]
    | 17%positive => []
    | 18%positive => [(*0 0.0348837*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_imagewidth)
                                                                    + (s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.0697674 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.0581395 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.232558 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*0 0.255814*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_j))) (F_check_ge (0) (0)));
                      (*0 0.162791*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_i))) (F_check_ge ((s IDquant_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*0 0.0348837*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.360465*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_imagewidth)
                                                                    + (s IDquant_j)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_imagewidth)
                                                                    + (s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagewidth))) (F_check_ge ((s IDquant_imagewidth)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*-0.75 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_j))) (F_check_ge (0) (0)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_j))) (F_check_ge ((s IDquant_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_j))) (F_check_ge ((s IDquant_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_j)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)));
                      (*-0.197674 0*) F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0));
                      (*-0.162791 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => [(*-0.0639535 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0));
                      (*0 0.372093*) F_binom_monotonic 2 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0));
                      (*-0.162791 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDquant_i))) (F_check_ge ((s IDquant_i)) (0));
                      (*-0.0639535 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)));
                      (*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_j)));
                      (*0 0.162791*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_i))) (F_check_ge (-1
                                                                    + (s IDquant_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*-0.0639535 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.127907 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.674419 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*-0.186047 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.133721*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.127907 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_i))) (F_check_ge ((s IDquant_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_i))) (F_check_ge ((s IDquant_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.319767 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.55814 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.0988372 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_j))) (F_check_ge ((s IDquant_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*0 0.127907*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)));
                      (*0 0.27907*) F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))]
    | 26%positive => []
    | 27%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDquant_i)
                                            + (s IDquant_imagelength));
                      (*-0.534884 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0));
                      (*-0.325581 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)));
                      (*-0.162791 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDquant_i))) (F_check_ge ((s IDquant_i)) (0));
                      (*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_j)));
                      (*-0.534884 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.19186 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*0 0.127907*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.19186*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*0 0.127907*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.197674 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.162791 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*-0.325581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_i))) (F_check_ge ((s IDquant_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.197674 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_j))) (F_check_ge ((s IDquant_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*-0.197674 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))]
    | 28%positive => [(*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagewidth)
                                                                    - (s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-1.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_i))) (F_check_ge ((s IDquant_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.5 -9.48484e-12*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagewidth))) (F_check_ge ((s IDquant_imagewidth)) (0))]
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | 33%positive => []
    | 34%positive => []
    | 35%positive => []
    | 36%positive => []
    | 37%positive => [(*-0.25 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0));
                      (*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)));
                      (*-0.25 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0));
                      (*-0.25 0*) F_binom_monotonic 2 (F_max0_ge_arg ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagewidth)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagewidth)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagewidth))) (F_check_ge (-1
                                                                    + (s IDquant_imagewidth)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagewidth))) (F_check_ge (-1
                                                                    + (s IDquant_imagewidth)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_j)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_j))) (F_check_ge (-1
                                                                    + (s IDquant_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*-1.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_i))) (F_check_ge ((s IDquant_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_i))) (F_check_ge ((s IDquant_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_i))) (F_check_ge ((s IDquant_i)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagelength)) (0))) (F_max0_ge_0 ((s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagewidth))) (F_check_ge ((s IDquant_imagewidth)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagewidth)
                                                                    - (s IDquant_j))) (F_check_ge ((s IDquant_imagewidth)
                                                                    - (s IDquant_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_j))) (F_check_ge ((s IDquant_j)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_j))) (F_check_ge (0) (0)));
                      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)));
                      (*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength)))]
    | 38%positive => []
    | 39%positive => [(*-1 0*) F_one;
                      (*-1 0*) F_max0_ge_0 (-1 - (s IDquant_i)
                                            + (s IDquant_imagelength));
                      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    - 
                                                                    (s IDquant_i)
                                                                    + 
                                                                    (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0));
                      (*0 0.0348837*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0));
                      (*-0.290698 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)));
                      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.255814 -0.0988372*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.0697674 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*0 0.0348837*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.0581395 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.156977 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (-1
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_i))) (F_check_ge (0) (0)));
                      (*-0.267442 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.290698 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 ((s IDquant_imagewidth))) (F_check_ge (0) (0)));
                      (*-0.255814 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_i)) (0))) (F_max0_ge_0 ((s IDquant_i)))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.0348837 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-0.162791 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg ((s IDquant_imagelength))) (F_check_ge ((s IDquant_imagelength)) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)));
                      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge ((s IDquant_imagewidth)) (0))) (F_max0_ge_0 ((s IDquant_imagewidth)))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    (s IDquant_i)
                                                                    + (s IDquant_imagelength))) (F_check_ge (0) (0)))]
    | 40%positive => []
    | _ => []
  end.


Theorem quant_ai_correct:
  forall s p' s', steps (g_start quant) s (g_edges quant) p' s' -> quant_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem quant_pot_correct:
  forall s p' s',
    steps (g_start quant) s (g_edges quant) p' s' ->
    (quant_pot (g_start quant) s >= quant_pot p' s')%Q.
Proof.
  check_lp quant_ai_correct quant_hints.
Qed.

