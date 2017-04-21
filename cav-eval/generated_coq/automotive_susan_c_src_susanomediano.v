Require Import pasta.Pasta.

Inductive proc: Type :=
  P_median.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_median_z := 1%positive.
Notation V_median__tmp := 2%positive.
Notation V_median__tmp1 := 3%positive.
Notation V_median__tmp2 := 4%positive.
Notation V_median_k := 5%positive.
Notation V_median_l := 6%positive.
Notation V_median_p_off0 := 7%positive.
Notation V_median_p_off12 := 8%positive.
Notation V_median_p_off16 := 9%positive.
Notation V_median_p_off20 := 10%positive.
Notation V_median_p_off24 := 11%positive.
Notation V_median_p_off28 := 12%positive.
Notation V_median_p_off4 := 13%positive.
Notation V_median_p_off8 := 14%positive.
Notation V_median_tmp := 15%positive.
Notation V_median_i := 16%positive.
Notation V_median_in := 17%positive.
Notation V_median_j := 18%positive.
Notation V_median_x_size := 19%positive.
Definition Pedges_median: list (edge proc) :=
  (EA 1 (AAssign V_median_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_median__tmp2 (Some (EVar V_median_i))) 3)::(EA 3 (AAssign V_median__tmp1
  (Some (EVar V_median_j))) 4)::(EA 4 (AAssign V_median__tmp
  (Some (EVar V_median_x_size))) 5)::(EA 5 (AAssign V_median_p_off0
  None) 6)::(EA 6 (AAssign V_median_p_off4 None) 7)::(EA 7 (AAssign
  V_median_p_off8 None) 8)::(EA 8 (AAssign V_median_p_off12 None) 9)::
  (EA 9 (AAssign V_median_p_off16 None) 10)::(EA 10 (AAssign V_median_p_off20
  None) 11)::(EA 11 (AAssign V_median_p_off24 None) 12)::(EA 12 (AAssign
  V_median_p_off28 None) 13)::(EA 13 (AAssign V_median_k
  (Some (ENum (0)))) 14)::(EA 14 ANone 15)::(EA 15 AWeaken 16)::
  (EA 16 (AGuard (fun s => ((eval (EVar V_median_k) s) < (eval (ENum (7))
  s))%Z)) 19)::(EA 16 (AGuard (fun s => ((eval (EVar V_median_k) s) >=
  (eval (ENum (7)) s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 19 AWeaken 20)::
  (EA 20 (AAssign V_median_l (Some (ENum (0)))) 21)::(EA 21 ANone 22)::
  (EA 22 AWeaken 23)::(EA 23 (AGuard (fun s => ((eval (EVar V_median_l) s) <
  (eval (ESub (ENum (7)) (EVar V_median_k)) s))%Z)) 31)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_median_l) s) >= (eval (ESub (ENum (7))
  (EVar V_median_k)) s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 ANone 26)::
  (EA 26 (AAssign V_median_k (Some (EAdd (EVar V_median_k)
  (ENum (1))))) 27)::(EA 27 ANone 28)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_median_z (Some (EAdd (ENum (1)) (EVar V_median_z)))) 30)::
  (EA 30 AWeaken 16)::(EA 31 AWeaken 32)::(EA 32 ANone 33)::
  (EA 32 ANone 35)::(EA 33 (AAssign V_median_tmp None) 34)::
  (EA 34 ANone 35)::(EA 35 ANone 36)::(EA 36 (AAssign V_median_l
  (Some (EAdd (EVar V_median_l) (ENum (1))))) 37)::(EA 37 ANone 38)::
  (EA 38 ANone 39)::(EA 39 (AAssign V_median_z (Some (EAdd (ENum (1))
  (EVar V_median_z)))) 40)::(EA 40 AWeaken 23)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_median => Pedges_median
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_median => 18
     end)%positive;
  var_global := var_global
}.

Definition ai_median (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_median_z <= 0 /\ -1 * s V_median_z <= 0)%Z
   | 3 => (-1 * s V_median_z <= 0 /\ 1 * s V_median_z <= 0)%Z
   | 4 => (1 * s V_median_z <= 0 /\ -1 * s V_median_z <= 0)%Z
   | 5 => (-1 * s V_median_z <= 0 /\ 1 * s V_median_z <= 0)%Z
   | 6 => (1 * s V_median_z <= 0 /\ -1 * s V_median_z <= 0)%Z
   | 7 => (-1 * s V_median_z <= 0 /\ 1 * s V_median_z <= 0)%Z
   | 8 => (1 * s V_median_z <= 0 /\ -1 * s V_median_z <= 0)%Z
   | 9 => (-1 * s V_median_z <= 0 /\ 1 * s V_median_z <= 0)%Z
   | 10 => (1 * s V_median_z <= 0 /\ -1 * s V_median_z <= 0)%Z
   | 11 => (-1 * s V_median_z <= 0 /\ 1 * s V_median_z <= 0)%Z
   | 12 => (1 * s V_median_z <= 0 /\ -1 * s V_median_z <= 0)%Z
   | 13 => (-1 * s V_median_z <= 0 /\ 1 * s V_median_z <= 0)%Z
   | 14 => (1 * s V_median_z <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_k <= 0 /\ -1 * s V_median_k <= 0)%Z
   | 15 => (-1 * s V_median_k <= 0 /\ 1 * s V_median_k <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_z <= 0)%Z
   | 16 => (-1 * s V_median_z <= 0 /\ -1 * s V_median_k <= 0)%Z
   | 17 => (-1 * s V_median_z <= 0 /\ -1 * s V_median_k + 7 <= 0)%Z
   | 18 => (-1 * s V_median_k + 7 <= 0 /\ -1 * s V_median_z <= 0)%Z
   | 19 => (-1 * s V_median_k <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_k + -6 <= 0)%Z
   | 20 => (1 * s V_median_k + -6 <= 0 /\ -1 * s V_median_z <= 0 /\ -1 * s V_median_k <= 0)%Z
   | 21 => (-1 * s V_median_k <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_k + -6 <= 0 /\ 1 * s V_median_l <= 0 /\ -1 * s V_median_l <= 0)%Z
   | 22 => (-1 * s V_median_l <= 0 /\ 1 * s V_median_l <= 0 /\ 1 * s V_median_k + -6 <= 0 /\ -1 * s V_median_z <= 0 /\ -1 * s V_median_k <= 0)%Z
   | 23 => (-1 * s V_median_z <= 0 /\ 1 * s V_median_k + -6 <= 0 /\ -1 * s V_median_l <= 0 /\ -1 * s V_median_k <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -7 <= 0)%Z
   | 24 => (1 * s V_median_k+ 1 * s V_median_l + -7 <= 0 /\ -1 * s V_median_k <= 0 /\ 1 * s V_median_k + -6 <= 0 /\ -1 * s V_median_z <= 0 /\ -1 * s V_median_k+ -1 * s V_median_l + 7 <= 0)%Z
   | 25 => (-1 * s V_median_k+ -1 * s V_median_l + 7 <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_k + -6 <= 0 /\ -1 * s V_median_k <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -7 <= 0)%Z
   | 26 => (1 * s V_median_k+ 1 * s V_median_l + -7 <= 0 /\ -1 * s V_median_k <= 0 /\ 1 * s V_median_k + -6 <= 0 /\ -1 * s V_median_z <= 0 /\ -1 * s V_median_k+ -1 * s V_median_l + 7 <= 0)%Z
   | 27 => (-1 * s V_median_z <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -8 <= 0 /\ -1 * s V_median_k + 1 <= 0 /\ 1 * s V_median_k + -7 <= 0 /\ -1 * s V_median_k+ -1 * s V_median_l + 8 <= 0)%Z
   | 28 => (-1 * s V_median_k+ -1 * s V_median_l + 8 <= 0 /\ 1 * s V_median_k + -7 <= 0 /\ -1 * s V_median_k + 1 <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -8 <= 0 /\ -1 * s V_median_z <= 0)%Z
   | 29 => (-1 * s V_median_z <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -8 <= 0 /\ -1 * s V_median_k + 1 <= 0 /\ 1 * s V_median_k + -7 <= 0 /\ -1 * s V_median_k+ -1 * s V_median_l + 8 <= 0)%Z
   | 30 => (-1 * s V_median_k+ -1 * s V_median_l + 8 <= 0 /\ 1 * s V_median_k + -7 <= 0 /\ -1 * s V_median_k + 1 <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -8 <= 0 /\ -1 * s V_median_z + 1 <= 0)%Z
   | 31 => (-1 * s V_median_k <= 0 /\ -1 * s V_median_l <= 0 /\ 1 * s V_median_k + -6 <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -6 <= 0)%Z
   | 32 => (1 * s V_median_k+ 1 * s V_median_l + -6 <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_k + -6 <= 0 /\ -1 * s V_median_l <= 0 /\ -1 * s V_median_k <= 0)%Z
   | 33 => (-1 * s V_median_k <= 0 /\ -1 * s V_median_l <= 0 /\ 1 * s V_median_k + -6 <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -6 <= 0)%Z
   | 34 => (1 * s V_median_k+ 1 * s V_median_l + -6 <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_k + -6 <= 0 /\ -1 * s V_median_l <= 0 /\ -1 * s V_median_k <= 0)%Z
   | 35 => (-1 * s V_median_k <= 0 /\ -1 * s V_median_l <= 0 /\ 1 * s V_median_k + -6 <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -6 <= 0)%Z
   | 36 => (1 * s V_median_k+ 1 * s V_median_l + -6 <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_k + -6 <= 0 /\ -1 * s V_median_l <= 0 /\ -1 * s V_median_k <= 0)%Z
   | 37 => (-1 * s V_median_k <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -7 <= 0 /\ -1 * s V_median_l + 1 <= 0)%Z
   | 38 => (-1 * s V_median_l + 1 <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -7 <= 0 /\ -1 * s V_median_z <= 0 /\ -1 * s V_median_k <= 0)%Z
   | 39 => (-1 * s V_median_k <= 0 /\ -1 * s V_median_z <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -7 <= 0 /\ -1 * s V_median_l + 1 <= 0)%Z
   | 40 => (-1 * s V_median_l + 1 <= 0 /\ 1 * s V_median_k+ 1 * s V_median_l + -7 <= 0 /\ -1 * s V_median_k <= 0 /\ -1 * s V_median_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_median (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((50 # 1) <= z)%Q
   | 2 => ((50 # 1) + s V_median_z <= z)%Q
   | 3 => ((50 # 1) + s V_median_z <= z)%Q
   | 4 => ((50 # 1) + s V_median_z <= z)%Q
   | 5 => ((50 # 1) + s V_median_z <= z)%Q
   | 6 => ((50 # 1) + s V_median_z <= z)%Q
   | 7 => ((50 # 1) + s V_median_z <= z)%Q
   | 8 => ((50 # 1) + s V_median_z <= z)%Q
   | 9 => ((50 # 1) + s V_median_z <= z)%Q
   | 10 => ((50 # 1) + s V_median_z <= z)%Q
   | 11 => ((50 # 1) + s V_median_z <= z)%Q
   | 12 => ((50 # 1) + s V_median_z <= z)%Q
   | 13 => ((50 # 1) + s V_median_z <= z)%Q
   | 14 => ((1 # 1) - s V_median_k + s V_median_z + max0(-1 + s V_median_k)
            + (7 # 1) * max0(7 - s V_median_k) <= z)%Q
   | 15 => ((1 # 1) - s V_median_k + s V_median_z + max0(-1 + s V_median_k)
            + (7 # 1) * max0(7 - s V_median_k) <= z)%Q
   | 16 => ((1 # 1) - s V_median_k + s V_median_z + max0(-1 + s V_median_k)
            + (7 # 1) * max0(7 - s V_median_k) <= z)%Q
   | 17 => hints
     [(*-7 0*) F_max0_monotonic (F_check_ge (7 - s V_median_k) (6
                                                                - s V_median_k));
      (*-7 0*) F_max0_ge_0 (6 - s V_median_k);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_median_k)) (F_check_ge (-1
                                                                    + s V_median_k) (0))]
     ((1 # 1) - s V_median_k + s V_median_z + max0(-1 + s V_median_k)
      + (7 # 1) * max0(7 - s V_median_k) <= z)%Q
   | 18 => (s V_median_z <= z)%Q
   | 19 => hints
     [(*1.82671e-12 7*) F_max0_pre_decrement 1 (7 - s V_median_k) (1)]
     ((1 # 1) - s V_median_k + s V_median_z + max0(-1 + s V_median_k)
      + (7 # 1) * max0(7 - s V_median_k) <= z)%Q
   | 20 => ((8 # 1) - s V_median_k + s V_median_z + max0(-1 + s V_median_k)
            + (7 # 1) * max0(6 - s V_median_k) <= z)%Q
   | 21 => ((14 # 1) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
            + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
            - (2 # 5) * max0(7 - s V_median_l)
            - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 22 => ((14 # 1) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
            + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
            - (2 # 5) * max0(7 - s V_median_l)
            - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 23 => ((14 # 1) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
            + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
            - (2 # 5) * max0(7 - s V_median_l)
            - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 24 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (7 - s V_median_k - s V_median_l) (6
                                                                    - s V_median_k
                                                                    - s V_median_l));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                              - s V_median_k
                                                              - s V_median_l) (0))) (F_max0_ge_0 (7
                                                                    - s V_median_k
                                                                    - s V_median_l));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_median_k)) (F_check_ge (0) (0))]
     ((14 # 1) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
      + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
      - (2 # 5) * max0(7 - s V_median_l) - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 25 => ((7 # 1) - (4 # 5) * s V_median_l + s V_median_z
            + (7 # 1) * max0(6 - s V_median_k)
            + max0(6 - s V_median_k - s V_median_l)
            - (2 # 5) * max0(7 - s V_median_l)
            - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 26 => ((7 # 1) - (4 # 5) * s V_median_l + s V_median_z
            + (7 # 1) * max0(6 - s V_median_k)
            + max0(6 - s V_median_k - s V_median_l)
            - (2 # 5) * max0(7 - s V_median_l)
            - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 27 => ((7 # 1) - (4 # 5) * s V_median_l + s V_median_z
            + (7 # 1) * max0(7 - s V_median_k)
            + max0(7 - s V_median_k - s V_median_l)
            - (2 # 5) * max0(7 - s V_median_l)
            - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 28 => ((7 # 1) - (4 # 5) * s V_median_l + s V_median_z
            + (7 # 1) * max0(7 - s V_median_k)
            + max0(7 - s V_median_k - s V_median_l)
            - (2 # 5) * max0(7 - s V_median_l)
            - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 29 => ((7 # 1) - (4 # 5) * s V_median_l + s V_median_z
            + (7 # 1) * max0(7 - s V_median_k)
            + max0(7 - s V_median_k - s V_median_l)
            - (2 # 5) * max0(7 - s V_median_l)
            - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 30 => hints
     [(*-1 0*) F_max0_ge_0 (7 - s V_median_k - s V_median_l);
      (*-0.4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                 - s V_median_l) (0))) (F_max0_ge_0 (8
                                                                    - s V_median_l));
      (*-0.4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                 - s V_median_l) (0))) (F_max0_ge_0 (7
                                                                    - s V_median_l));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              + s V_median_k) (0))) (F_max0_ge_0 (-1
                                                                    + s V_median_k))]
     ((6 # 1) - (4 # 5) * s V_median_l + s V_median_z
      + (7 # 1) * max0(7 - s V_median_k)
      + max0(7 - s V_median_k - s V_median_l)
      - (2 # 5) * max0(7 - s V_median_l) - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 31 => hints
     [(*-0.4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                 - s V_median_l) (0))) (F_max0_ge_0 (8
                                                                    - s V_median_l));
      (*0 0.4*) F_binom_monotonic 1 (F_max0_ge_arg (6 - s V_median_l)) (F_check_ge (6
                                                                    - s V_median_l) (0))]
     ((14 # 1) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
      + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
      - (2 # 5) * max0(7 - s V_median_l) - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 32 => ((66 # 5) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
            + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
            - (2 # 5) * max0(6 - s V_median_l)
            - (2 # 5) * max0(7 - s V_median_l) <= z)%Q
   | 33 => ((66 # 5) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
            + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
            - (2 # 5) * max0(6 - s V_median_l)
            - (2 # 5) * max0(7 - s V_median_l) <= z)%Q
   | 34 => ((66 # 5) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
            + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
            - (2 # 5) * max0(6 - s V_median_l)
            - (2 # 5) * max0(7 - s V_median_l) <= z)%Q
   | 35 => ((66 # 5) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
            + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
            - (2 # 5) * max0(6 - s V_median_l)
            - (2 # 5) * max0(7 - s V_median_l) <= z)%Q
   | 36 => ((66 # 5) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
            + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
            - (2 # 5) * max0(6 - s V_median_l)
            - (2 # 5) * max0(7 - s V_median_l) <= z)%Q
   | 37 => ((15 # 1) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
            + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
            - (2 # 5) * max0(7 - s V_median_l)
            - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 38 => ((15 # 1) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
            + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
            - (2 # 5) * max0(7 - s V_median_l)
            - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 39 => ((15 # 1) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
            + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
            - (2 # 5) * max0(7 - s V_median_l)
            - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | 40 => ((14 # 1) - s V_median_k - (9 # 5) * s V_median_l + s V_median_z
            + max0(-1 + s V_median_k) + (7 # 1) * max0(6 - s V_median_k)
            - (2 # 5) * max0(7 - s V_median_l)
            - (2 # 5) * max0(8 - s V_median_l) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_median =>
    [mkPA Q (fun n z s => ai_median n s /\ annot0_median n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_median (proc_start P_median) s1 (proc_end P_median) s2 ->
    (s2 V_median_z <= (50 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_median.
Qed.
