Require Import pasta.Pasta.

Notation IDpsw_put_bits_z := 1%positive.
Notation IDpsw_put_bits__tmp := 2%positive.
Notation IDpsw_put_bits__tmp1 := 3%positive.
Notation IDpsw_put_bits__tmp2 := 4%positive.
Notation IDpsw_put_bits__tmp3 := 5%positive.
Notation IDpsw_put_bits_y := 6%positive.
Notation IDpsw_put_bits_data := 7%positive.
Notation IDpsw_put_bits_data_x_bit := 8%positive.
Notation IDpsw_put_bits_height := 9%positive.
Notation IDpsw_put_bits_raster := 10%positive.
Notation IDpsw_put_bits_s := 11%positive.
Notation IDpsw_put_bits_width_bits := 12%positive.
Definition psw_put_bits : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDpsw_put_bits_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDpsw_put_bits__tmp3
             (Some (EVar IDpsw_put_bits_data_x_bit))),3%positive)::
             (3%positive,(AAssign IDpsw_put_bits__tmp2
             (Some (EVar IDpsw_put_bits_raster))),4%positive)::
             (4%positive,(AAssign IDpsw_put_bits__tmp1
             (Some (EVar IDpsw_put_bits_width_bits))),5%positive)::
             (5%positive,(AAssign IDpsw_put_bits__tmp
             (Some (EVar IDpsw_put_bits_height))),6%positive)::
             (6%positive,(AAssign IDpsw_put_bits_y (Some (ENum (0)))),
             7%positive)::(7%positive,ANone,8%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDpsw_put_bits_y)
             s) < (eval (EVar IDpsw_put_bits__tmp) s))%Z)),12%positive)::
             (9%positive,(AGuard (fun s => ((eval (EVar IDpsw_put_bits_y)
             s) >= (eval (EVar IDpsw_put_bits__tmp) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,(AAssign IDpsw_put_bits_y
             (Some (EAdd (EVar IDpsw_put_bits_y) (ENum (1))))),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDpsw_put_bits_z (Some (EAdd (ENum (1))
             (EVar IDpsw_put_bits_z)))),18%positive)::
             (18%positive,AWeaken,9%positive)::nil
|}.

Definition psw_put_bits_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpsw_put_bits_z) <= 0 /\ -1 * (s IDpsw_put_bits_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpsw_put_bits_z) <= 0 /\ 1 * (s IDpsw_put_bits_z) <= 0)%Z
    | 4%positive => (1 * (s IDpsw_put_bits_z) <= 0 /\ -1 * (s IDpsw_put_bits_z) <= 0)%Z
    | 5%positive => (-1 * (s IDpsw_put_bits_z) <= 0 /\ 1 * (s IDpsw_put_bits_z) <= 0)%Z
    | 6%positive => (1 * (s IDpsw_put_bits_z) <= 0 /\ -1 * (s IDpsw_put_bits_z) <= 0)%Z
    | 7%positive => (-1 * (s IDpsw_put_bits_z) <= 0 /\ 1 * (s IDpsw_put_bits_z) <= 0 /\ 1 * (s IDpsw_put_bits_y) <= 0 /\ -1 * (s IDpsw_put_bits_y) <= 0)%Z
    | 8%positive => (-1 * (s IDpsw_put_bits_y) <= 0 /\ 1 * (s IDpsw_put_bits_y) <= 0 /\ 1 * (s IDpsw_put_bits_z) <= 0 /\ -1 * (s IDpsw_put_bits_z) <= 0)%Z
    | 9%positive => (-1 * (s IDpsw_put_bits_z) <= 0 /\ -1 * (s IDpsw_put_bits_y) <= 0)%Z
    | 10%positive => (-1 * (s IDpsw_put_bits_y) <= 0 /\ -1 * (s IDpsw_put_bits_z) <= 0 /\ 1 * (s IDpsw_put_bits__tmp)+ -1 * (s IDpsw_put_bits_y) <= 0)%Z
    | 11%positive => (1 * (s IDpsw_put_bits__tmp)+ -1 * (s IDpsw_put_bits_y) <= 0 /\ -1 * (s IDpsw_put_bits_z) <= 0 /\ -1 * (s IDpsw_put_bits_y) <= 0)%Z
    | 12%positive => (-1 * (s IDpsw_put_bits_y) <= 0 /\ -1 * (s IDpsw_put_bits_z) <= 0 /\ -1 * (s IDpsw_put_bits__tmp)+ 1 * (s IDpsw_put_bits_y) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDpsw_put_bits__tmp)+ 1 * (s IDpsw_put_bits_y) + 1 <= 0 /\ -1 * (s IDpsw_put_bits_z) <= 0 /\ -1 * (s IDpsw_put_bits_y) <= 0)%Z
    | 14%positive => (-1 * (s IDpsw_put_bits_y) <= 0 /\ -1 * (s IDpsw_put_bits_z) <= 0 /\ -1 * (s IDpsw_put_bits__tmp)+ 1 * (s IDpsw_put_bits_y) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDpsw_put_bits_z) <= 0 /\ -1 * (s IDpsw_put_bits_y) + 1 <= 0 /\ -1 * (s IDpsw_put_bits__tmp)+ 1 * (s IDpsw_put_bits_y) <= 0)%Z
    | 16%positive => (-1 * (s IDpsw_put_bits__tmp)+ 1 * (s IDpsw_put_bits_y) <= 0 /\ -1 * (s IDpsw_put_bits_y) + 1 <= 0 /\ -1 * (s IDpsw_put_bits_z) <= 0)%Z
    | 17%positive => (-1 * (s IDpsw_put_bits_z) <= 0 /\ -1 * (s IDpsw_put_bits_y) + 1 <= 0 /\ -1 * (s IDpsw_put_bits__tmp)+ 1 * (s IDpsw_put_bits_y) <= 0)%Z
    | 18%positive => (-1 * (s IDpsw_put_bits__tmp)+ 1 * (s IDpsw_put_bits_y) <= 0 /\ -1 * (s IDpsw_put_bits_y) + 1 <= 0 /\ -1 * (s IDpsw_put_bits_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition psw_put_bits_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDpsw_put_bits_height)))%Q
    | 2%positive => ((s IDpsw_put_bits_z) + max0((s IDpsw_put_bits_height)))%Q
    | 3%positive => ((s IDpsw_put_bits_z) + max0((s IDpsw_put_bits_height)))%Q
    | 4%positive => ((s IDpsw_put_bits_z) + max0((s IDpsw_put_bits_height)))%Q
    | 5%positive => ((s IDpsw_put_bits_z) + max0((s IDpsw_put_bits_height)))%Q
    | 6%positive => ((s IDpsw_put_bits_z) + max0((s IDpsw_put_bits__tmp)))%Q
    | 7%positive => ((s IDpsw_put_bits_z)
                     + max0((s IDpsw_put_bits__tmp) - (s IDpsw_put_bits_y)))%Q
    | 8%positive => ((s IDpsw_put_bits_z)
                     + max0((s IDpsw_put_bits__tmp) - (s IDpsw_put_bits_y)))%Q
    | 9%positive => ((s IDpsw_put_bits_z)
                     + max0((s IDpsw_put_bits__tmp) - (s IDpsw_put_bits_y)))%Q
    | 10%positive => ((s IDpsw_put_bits_z)
                      + max0((s IDpsw_put_bits__tmp) - (s IDpsw_put_bits_y)))%Q
    | 11%positive => ((s IDpsw_put_bits_z))%Q
    | 12%positive => ((s IDpsw_put_bits_z)
                      + max0((s IDpsw_put_bits__tmp) - (s IDpsw_put_bits_y)))%Q
    | 13%positive => ((1 # 1) + (s IDpsw_put_bits_z)
                      + max0(-1 + (s IDpsw_put_bits__tmp)
                             - (s IDpsw_put_bits_y)))%Q
    | 14%positive => ((1 # 1) + (s IDpsw_put_bits_z)
                      + max0(-1 + (s IDpsw_put_bits__tmp)
                             - (s IDpsw_put_bits_y)))%Q
    | 15%positive => ((1 # 1) + (s IDpsw_put_bits_z)
                      + max0((s IDpsw_put_bits__tmp) - (s IDpsw_put_bits_y)))%Q
    | 16%positive => ((1 # 1) + (s IDpsw_put_bits_z)
                      + max0((s IDpsw_put_bits__tmp) - (s IDpsw_put_bits_y)))%Q
    | 17%positive => ((1 # 1) + (s IDpsw_put_bits_z)
                      + max0((s IDpsw_put_bits__tmp) - (s IDpsw_put_bits_y)))%Q
    | 18%positive => ((s IDpsw_put_bits_z)
                      + max0((s IDpsw_put_bits__tmp) - (s IDpsw_put_bits_y)))%Q
    | _ => (0 # 1)%Q
  end.

Definition psw_put_bits_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDpsw_put_bits__tmp)
                                                             - (s IDpsw_put_bits_y)) (-1
                                                                    + (s IDpsw_put_bits__tmp)
                                                                    - (s IDpsw_put_bits_y)));
                      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                 + (s IDpsw_put_bits__tmp)
                                                                 - (s IDpsw_put_bits_y))) (F_check_ge (0) (0))]
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_pre_decrement ((s IDpsw_put_bits__tmp)
                                                     - (s IDpsw_put_bits_y)) (1)]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | _ => []
  end.


Theorem psw_put_bits_ai_correct:
  forall s p' s', steps (g_start psw_put_bits) s (g_edges psw_put_bits) p' s' -> psw_put_bits_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem psw_put_bits_pot_correct:
  forall s p' s',
    steps (g_start psw_put_bits) s (g_edges psw_put_bits) p' s' ->
    (psw_put_bits_pot (g_start psw_put_bits) s >= psw_put_bits_pot p' s')%Q.
Proof.
  check_lp psw_put_bits_ai_correct psw_put_bits_hints.
Qed.

