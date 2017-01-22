Require Import pasta.Pasta.

Notation IDsetup_brightness_lut_z := 1%positive.
Notation IDsetup_brightness_lut__tmp := 2%positive.
Notation IDsetup_brightness_lut__tmp1 := 3%positive.
Notation IDsetup_brightness_lut_k := 4%positive.
Notation IDsetup_brightness_lut_bp := 5%positive.
Notation IDsetup_brightness_lut_form := 6%positive.
Notation IDsetup_brightness_lut_thresh := 7%positive.
Definition setup_brightness_lut : graph := {|
  g_start := 1%positive;
  g_end := 9%positive;
  g_edges := (1%positive,(AAssign IDsetup_brightness_lut_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDsetup_brightness_lut__tmp1
             (Some (EVar IDsetup_brightness_lut_thresh))),3%positive)::
             (3%positive,(AAssign IDsetup_brightness_lut__tmp
             (Some (EVar IDsetup_brightness_lut_form))),4%positive)::
             (4%positive,(AAssign IDsetup_brightness_lut_k
             (Some (ENum (-256)))),5%positive)::
             (5%positive,ANone,6%positive)::(6%positive,AWeaken,7%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDsetup_brightness_lut_k) s) <
             (eval (ENum (257)) s))%Z)),10%positive)::
             (7%positive,(AGuard
             (fun s => ((eval (EVar IDsetup_brightness_lut_k) s) >=
             (eval (ENum (257)) s))%Z)),8%positive)::
             (8%positive,AWeaken,9%positive)::
             (10%positive,AWeaken,11%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDsetup_brightness_lut__tmp) s) =
             (eval (ENum (6)) s))%Z)),13%positive)::
             (11%positive,(AGuard
             (fun s => ((eval (EVar IDsetup_brightness_lut__tmp) s) <>
             (eval (ENum (6)) s))%Z)),12%positive)::
             (12%positive,AWeaken,15%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDsetup_brightness_lut_k
             (Some (EAdd (EVar IDsetup_brightness_lut_k) (ENum (1))))),
             17%positive)::(17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDsetup_brightness_lut_z
             (Some (EAdd (ENum (1)) (EVar IDsetup_brightness_lut_z)))),
             20%positive)::(20%positive,AWeaken,7%positive)::nil
|}.

Definition setup_brightness_lut_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDsetup_brightness_lut_z) <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0)%Z
    | 3%positive => (-1 * (s IDsetup_brightness_lut_z) <= 0 /\ 1 * (s IDsetup_brightness_lut_z) <= 0)%Z
    | 4%positive => (1 * (s IDsetup_brightness_lut_z) <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0)%Z
    | 5%positive => (-1 * (s IDsetup_brightness_lut_z) <= 0 /\ 1 * (s IDsetup_brightness_lut_z) <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + 256 <= 0 /\ -1 * (s IDsetup_brightness_lut_k) + -256 <= 0)%Z
    | 6%positive => (-1 * (s IDsetup_brightness_lut_k) + -256 <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + 256 <= 0 /\ 1 * (s IDsetup_brightness_lut_z) <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0)%Z
    | 7%positive => (-1 * (s IDsetup_brightness_lut_z) <= 0 /\ -1 * (s IDsetup_brightness_lut_k) + -256 <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + -257 <= 0)%Z
    | 8%positive => (1 * (s IDsetup_brightness_lut_k) + -257 <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0 /\ -1 * (s IDsetup_brightness_lut_k) + 257 <= 0)%Z
    | 9%positive => (-1 * (s IDsetup_brightness_lut_k) + 257 <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + -257 <= 0)%Z
    | 10%positive => (-1 * (s IDsetup_brightness_lut_k) + -256 <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + -256 <= 0)%Z
    | 11%positive => (1 * (s IDsetup_brightness_lut_k) + -256 <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0 /\ -1 * (s IDsetup_brightness_lut_k) + -256 <= 0)%Z
    | 12%positive => (-1 * (s IDsetup_brightness_lut_k) + -256 <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + -256 <= 0)%Z
    | 13%positive => (-1 * (s IDsetup_brightness_lut_k) + -256 <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + -256 <= 0 /\ 1 * (s IDsetup_brightness_lut__tmp) + -6 <= 0 /\ -1 * (s IDsetup_brightness_lut__tmp) + 6 <= 0)%Z
    | 14%positive => (-1 * (s IDsetup_brightness_lut__tmp) + 6 <= 0 /\ 1 * (s IDsetup_brightness_lut__tmp) + -6 <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + -256 <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0 /\ -1 * (s IDsetup_brightness_lut_k) + -256 <= 0)%Z
    | 15%positive => (-1 * (s IDsetup_brightness_lut_k) + -256 <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + -256 <= 0)%Z
    | 16%positive => (1 * (s IDsetup_brightness_lut_k) + -256 <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0 /\ -1 * (s IDsetup_brightness_lut_k) + -256 <= 0)%Z
    | 17%positive => (-1 * (s IDsetup_brightness_lut_z) <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + -257 <= 0 /\ -1 * (s IDsetup_brightness_lut_k) + -255 <= 0)%Z
    | 18%positive => (-1 * (s IDsetup_brightness_lut_k) + -255 <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + -257 <= 0 /\ -1 * (s IDsetup_brightness_lut_z) <= 0)%Z
    | 19%positive => (-1 * (s IDsetup_brightness_lut_z) <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + -257 <= 0 /\ -1 * (s IDsetup_brightness_lut_k) + -255 <= 0)%Z
    | 20%positive => (-1 * (s IDsetup_brightness_lut_k) + -255 <= 0 /\ 1 * (s IDsetup_brightness_lut_k) + -257 <= 0 /\ -1 * (s IDsetup_brightness_lut_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition setup_brightness_lut_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((513 # 1))%Q
    | 2%positive => ((513 # 1) + (s IDsetup_brightness_lut_z))%Q
    | 3%positive => ((513 # 1) + (s IDsetup_brightness_lut_z))%Q
    | 4%positive => ((513 # 1) + (s IDsetup_brightness_lut_z))%Q
    | 5%positive => ((s IDsetup_brightness_lut_z)
                     + max0(257 - (s IDsetup_brightness_lut_k)))%Q
    | 6%positive => ((s IDsetup_brightness_lut_z)
                     + max0(257 - (s IDsetup_brightness_lut_k)))%Q
    | 7%positive => ((s IDsetup_brightness_lut_z)
                     + max0(257 - (s IDsetup_brightness_lut_k)))%Q
    | 8%positive => ((s IDsetup_brightness_lut_z)
                     + max0(257 - (s IDsetup_brightness_lut_k)))%Q
    | 9%positive => ((s IDsetup_brightness_lut_z))%Q
    | 10%positive => ((s IDsetup_brightness_lut_z)
                      + max0(257 - (s IDsetup_brightness_lut_k)))%Q
    | 11%positive => ((s IDsetup_brightness_lut_z)
                      + max0(257 - (s IDsetup_brightness_lut_k)))%Q
    | 12%positive => ((s IDsetup_brightness_lut_z)
                      + max0(257 - (s IDsetup_brightness_lut_k)))%Q
    | 13%positive => ((s IDsetup_brightness_lut_z)
                      + max0(257 - (s IDsetup_brightness_lut_k)))%Q
    | 14%positive => ((1 # 1) + (s IDsetup_brightness_lut_z)
                      + max0(256 - (s IDsetup_brightness_lut_k)))%Q
    | 15%positive => ((1 # 1) + (s IDsetup_brightness_lut_z)
                      + max0(256 - (s IDsetup_brightness_lut_k)))%Q
    | 16%positive => ((1 # 1) + (s IDsetup_brightness_lut_z)
                      + max0(256 - (s IDsetup_brightness_lut_k)))%Q
    | 17%positive => ((1 # 1) + (s IDsetup_brightness_lut_z)
                      + max0(257 - (s IDsetup_brightness_lut_k)))%Q
    | 18%positive => ((1 # 1) + (s IDsetup_brightness_lut_z)
                      + max0(257 - (s IDsetup_brightness_lut_k)))%Q
    | 19%positive => ((1 # 1) + (s IDsetup_brightness_lut_z)
                      + max0(257 - (s IDsetup_brightness_lut_k)))%Q
    | 20%positive => ((s IDsetup_brightness_lut_z)
                      + max0(257 - (s IDsetup_brightness_lut_k)))%Q
    | _ => (0 # 1)%Q
  end.

Definition setup_brightness_lut_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (257
                                                            - (s IDsetup_brightness_lut_k)) (256
                                                                    - (s IDsetup_brightness_lut_k)));
                     (*-1 0*) F_max0_ge_0 (256 - (s IDsetup_brightness_lut_k))]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => [(*0 1*) F_max0_pre_decrement (257
                                                    - (s IDsetup_brightness_lut_k)) (1)]
    | 13%positive => [(*-1 0*) F_max0_pre_decrement (257
                                                     - (s IDsetup_brightness_lut_k)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | _ => []
  end.


Theorem setup_brightness_lut_ai_correct:
  forall s p' s', steps (g_start setup_brightness_lut) s (g_edges setup_brightness_lut) p' s' -> setup_brightness_lut_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem setup_brightness_lut_pot_correct:
  forall s p' s',
    steps (g_start setup_brightness_lut) s (g_edges setup_brightness_lut) p' s' ->
    (setup_brightness_lut_pot (g_start setup_brightness_lut) s >= setup_brightness_lut_pot p' s')%Q.
Proof.
  check_lp setup_brightness_lut_ai_correct setup_brightness_lut_hints.
Qed.

