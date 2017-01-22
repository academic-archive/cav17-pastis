Require Import pasta.Pasta.

Notation IDTIFFYCbCrToRGBInit_z := 1%positive.
Notation IDTIFFYCbCrToRGBInit_D1 := 2%positive.
Notation IDTIFFYCbCrToRGBInit_D2 := 3%positive.
Notation IDTIFFYCbCrToRGBInit_D3 := 4%positive.
Notation IDTIFFYCbCrToRGBInit_D4 := 5%positive.
Notation IDTIFFYCbCrToRGBInit_i := 6%positive.
Notation IDTIFFYCbCrToRGBInit_x := 7%positive.
Notation IDTIFFYCbCrToRGBInit_tif := 8%positive.
Notation IDTIFFYCbCrToRGBInit_ycbcr := 9%positive.
Definition TIFFYCbCrToRGBInit : graph := {|
  g_start := 1%positive;
  g_end := 17%positive;
  g_edges := (1%positive,(AAssign IDTIFFYCbCrToRGBInit_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDTIFFYCbCrToRGBInit_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFYCbCrToRGBInit_i) s) <
             (eval (ENum (256)) s))%Z)),26%positive)::
             (5%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFYCbCrToRGBInit_i) s) >=
             (eval (ENum (256)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (7%positive,(AAssign IDTIFFYCbCrToRGBInit_D1 None),8%positive)::
             (8%positive,(AAssign IDTIFFYCbCrToRGBInit_D2 None),9%positive)::
             (9%positive,(AAssign IDTIFFYCbCrToRGBInit_D3 None),10%positive)::
             (10%positive,(AAssign IDTIFFYCbCrToRGBInit_D4 None),11%positive)::
             (11%positive,(AAssign IDTIFFYCbCrToRGBInit_i (Some (ENum (0)))),
             12%positive)::
             (12%positive,(AAssign IDTIFFYCbCrToRGBInit_x
             (Some (ENum (-128)))),13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,AWeaken,15%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFYCbCrToRGBInit_i) s) <
             (eval (ENum (256)) s))%Z)),18%positive)::
             (15%positive,(AGuard
             (fun s => ((eval (EVar IDTIFFYCbCrToRGBInit_i) s) >=
             (eval (ENum (256)) s))%Z)),16%positive)::
             (16%positive,AWeaken,17%positive)::
             (18%positive,AWeaken,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDTIFFYCbCrToRGBInit_i
             (Some (EAdd (EVar IDTIFFYCbCrToRGBInit_i) (ENum (1))))),
             21%positive)::
             (21%positive,(AAssign IDTIFFYCbCrToRGBInit_x
             (Some (EAdd (EVar IDTIFFYCbCrToRGBInit_x) (ENum (1))))),
             22%positive)::(22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDTIFFYCbCrToRGBInit_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFYCbCrToRGBInit_z)))),
             25%positive)::(25%positive,AWeaken,15%positive)::
             (26%positive,AWeaken,27%positive)::
             (27%positive,ANone,28%positive)::
             (28%positive,(AAssign IDTIFFYCbCrToRGBInit_i
             (Some (EAdd (EVar IDTIFFYCbCrToRGBInit_i) (ENum (1))))),
             29%positive)::(29%positive,ANone,30%positive)::
             (30%positive,ANone,31%positive)::
             (31%positive,(AAssign IDTIFFYCbCrToRGBInit_z
             (Some (EAdd (ENum (1)) (EVar IDTIFFYCbCrToRGBInit_z)))),
             32%positive)::(32%positive,AWeaken,5%positive)::nil
|}.

Definition TIFFYCbCrToRGBInit_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0)%Z
    | 3%positive => (-1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) <= 0)%Z
    | 4%positive => (-1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0)%Z
    | 5%positive => (-1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0)%Z
    | 6%positive => (1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 256 <= 0)%Z
    | 7%positive => (-1 * (s IDTIFFYCbCrToRGBInit_i) + 256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0)%Z
    | 8%positive => (1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 256 <= 0)%Z
    | 9%positive => (-1 * (s IDTIFFYCbCrToRGBInit_i) + 256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0)%Z
    | 10%positive => (1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 256 <= 0)%Z
    | 11%positive => (-1 * (s IDTIFFYCbCrToRGBInit_i) + 256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0)%Z
    | 12%positive => (-1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) <= 0)%Z
    | 13%positive => (-1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_x) + 128 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_x) + -128 <= 0)%Z
    | 14%positive => (-1 * (s IDTIFFYCbCrToRGBInit_x) + -128 <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_x) + 128 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) <= 0)%Z
    | 15%positive => (-1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_x) + -128 <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0)%Z
    | 16%positive => (1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_x) + -128 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 256 <= 0)%Z
    | 17%positive => (-1 * (s IDTIFFYCbCrToRGBInit_i) + 256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_x) + -128 <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0)%Z
    | 18%positive => (-1 * (s IDTIFFYCbCrToRGBInit_x) + -128 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -255 <= 0)%Z
    | 19%positive => (1 * (s IDTIFFYCbCrToRGBInit_i) + -255 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_x) + -128 <= 0)%Z
    | 20%positive => (-1 * (s IDTIFFYCbCrToRGBInit_x) + -128 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -255 <= 0)%Z
    | 21%positive => (-1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_x) + -128 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 1 <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0)%Z
    | 22%positive => (1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 1 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_x) + -127 <= 0)%Z
    | 23%positive => (-1 * (s IDTIFFYCbCrToRGBInit_x) + -127 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 1 <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0)%Z
    | 24%positive => (1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 1 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_x) + -127 <= 0)%Z
    | 25%positive => (-1 * (s IDTIFFYCbCrToRGBInit_x) + -127 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 1 <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) + 1 <= 0)%Z
    | 26%positive => (-1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -255 <= 0)%Z
    | 27%positive => (1 * (s IDTIFFYCbCrToRGBInit_i) + -255 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) <= 0)%Z
    | 28%positive => (-1 * (s IDTIFFYCbCrToRGBInit_i) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -255 <= 0)%Z
    | 29%positive => (-1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 1 <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0)%Z
    | 30%positive => (1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 1 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) <= 0)%Z
    | 31%positive => (-1 * (s IDTIFFYCbCrToRGBInit_z) <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 1 <= 0 /\ 1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0)%Z
    | 32%positive => (1 * (s IDTIFFYCbCrToRGBInit_i) + -256 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_i) + 1 <= 0 /\ -1 * (s IDTIFFYCbCrToRGBInit_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition TIFFYCbCrToRGBInit_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((512 # 1))%Q
    | 2%positive => ((512 # 1) + (s IDTIFFYCbCrToRGBInit_z))%Q
    | 3%positive => ((256 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                     + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 4%positive => ((256 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                     + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 5%positive => ((256 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                     + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 6%positive => ((256 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                     + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 7%positive => ((256 # 1) + (s IDTIFFYCbCrToRGBInit_z))%Q
    | 8%positive => ((256 # 1) + (s IDTIFFYCbCrToRGBInit_z))%Q
    | 9%positive => ((256 # 1) + (s IDTIFFYCbCrToRGBInit_z))%Q
    | 10%positive => ((256 # 1) + (s IDTIFFYCbCrToRGBInit_z))%Q
    | 11%positive => ((256 # 1) + (s IDTIFFYCbCrToRGBInit_z))%Q
    | 12%positive => ((s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 13%positive => ((s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 14%positive => ((s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 15%positive => ((s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 16%positive => ((s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 17%positive => ((s IDTIFFYCbCrToRGBInit_z))%Q
    | 18%positive => ((s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 19%positive => ((1 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(255 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 20%positive => ((1 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(255 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 21%positive => ((1 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 22%positive => ((1 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 23%positive => ((1 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 24%positive => ((1 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 25%positive => ((s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 26%positive => ((256 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 27%positive => ((257 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(255 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 28%positive => ((257 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(255 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 29%positive => ((257 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 30%positive => ((257 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 31%positive => ((257 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | 32%positive => ((256 # 1) + (s IDTIFFYCbCrToRGBInit_z)
                      + max0(256 - (s IDTIFFYCbCrToRGBInit_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition TIFFYCbCrToRGBInit_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (256
                                                            - (s IDTIFFYCbCrToRGBInit_i)) (255
                                                                    - (s IDTIFFYCbCrToRGBInit_i)));
                     (*-1 0*) F_max0_ge_0 (255 - (s IDTIFFYCbCrToRGBInit_i))]
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (256
                                                             - (s IDTIFFYCbCrToRGBInit_i)) (255
                                                                    - (s IDTIFFYCbCrToRGBInit_i)));
                      (*-1 0*) F_max0_ge_0 (255 - (s IDTIFFYCbCrToRGBInit_i))]
    | 17%positive => []
    | 18%positive => [(*-1 0*) F_max0_pre_decrement (256
                                                     - (s IDTIFFYCbCrToRGBInit_i)) (1)]
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | 25%positive => []
    | 26%positive => [(*-1 0*) F_max0_pre_decrement (256
                                                     - (s IDTIFFYCbCrToRGBInit_i)) (1)]
    | 27%positive => []
    | 28%positive => []
    | 29%positive => []
    | 30%positive => []
    | 31%positive => []
    | 32%positive => []
    | _ => []
  end.


Theorem TIFFYCbCrToRGBInit_ai_correct:
  forall s p' s', steps (g_start TIFFYCbCrToRGBInit) s (g_edges TIFFYCbCrToRGBInit) p' s' -> TIFFYCbCrToRGBInit_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem TIFFYCbCrToRGBInit_pot_correct:
  forall s p' s',
    steps (g_start TIFFYCbCrToRGBInit) s (g_edges TIFFYCbCrToRGBInit) p' s' ->
    (TIFFYCbCrToRGBInit_pot (g_start TIFFYCbCrToRGBInit) s >= TIFFYCbCrToRGBInit_pot p' s')%Q.
Proof.
  check_lp TIFFYCbCrToRGBInit_ai_correct TIFFYCbCrToRGBInit_hints.
Qed.

