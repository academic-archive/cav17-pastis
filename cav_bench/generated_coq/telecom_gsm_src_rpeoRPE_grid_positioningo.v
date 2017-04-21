Require Import pasta.Pasta.

Inductive proc: Type :=
  P_RPE_grid_positioning.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_RPE_grid_positioning_z := 1%positive.
Notation V_RPE_grid_positioning__tmp := 2%positive.
Notation V_RPE_grid_positioning_i := 3%positive.
Notation V_RPE_grid_positioning_Mc := 4%positive.
Notation V_RPE_grid_positioning_ep := 5%positive.
Notation V_RPE_grid_positioning_xMp := 6%positive.
Definition Pedges_RPE_grid_positioning: list (edge proc) :=
  (EA 1 (AAssign V_RPE_grid_positioning_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_RPE_grid_positioning__tmp
  (Some (EVar V_RPE_grid_positioning_Mc))) 3)::(EA 3 (AAssign
  V_RPE_grid_positioning_i (Some (ENum (13)))) 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (ENum (0)) s) <=
  (eval (EVar V_RPE_grid_positioning__tmp) s))%Z)) 7)::(EA 5 (AGuard
  (fun s => ((eval (ENum (0)) s) > (eval (EVar V_RPE_grid_positioning__tmp)
  s))%Z)) 6)::(EA 6 AWeaken 10)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_RPE_grid_positioning__tmp) s) <= (eval (ENum (3))
  s))%Z)) 12)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_RPE_grid_positioning__tmp) s) > (eval (ENum (3))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 ANone 11)::(EA 11 AWeaken 34)::
  (EA 12 AWeaken 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 ANone 29)::(EA 15 ANone 16)::(EA 15 ANone 17)::(EA 15 ANone 19)::
  (EA 15 ANone 20)::(EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 ANone 19)::
  (EA 19 ANone 20)::(EA 20 ANone 21)::(EA 21 (AAssign
  V_RPE_grid_positioning_i (Some (EAdd (EVar V_RPE_grid_positioning_i)
  (ENum (-1))))) 22)::(EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EAdd (EVar V_RPE_grid_positioning_i) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 26)::(EA 23 (AGuard
  (fun s => ((eval (EAdd (EVar V_RPE_grid_positioning_i) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 ANone 29)::
  (EA 26 AWeaken 27)::(EA 27 ANone 28)::(EA 28 (AAssign
  V_RPE_grid_positioning_z (Some (EAdd (ENum (1))
  (EVar V_RPE_grid_positioning_z)))) 18)::(EA 29 ANone 30)::(EA 30 (AAssign
  V_RPE_grid_positioning__tmp (Some (EAdd (EVar V_RPE_grid_positioning__tmp)
  (ENum (1))))) 31)::(EA 31 AWeaken 32)::(EA 32 (AGuard
  (fun s => ((eval (EAdd (EVar V_RPE_grid_positioning__tmp) (ENum (1))) s) <
  (eval (ENum (4)) s))%Z)) 35)::(EA 32 (AGuard
  (fun s => ((eval (EAdd (EVar V_RPE_grid_positioning__tmp) (ENum (1))) s) >=
  (eval (ENum (4)) s))%Z)) 33)::(EA 33 AWeaken 34)::(EA 35 AWeaken 36)::
  (EA 36 ANone 37)::(EA 37 ANone 38)::(EA 38 (AAssign
  V_RPE_grid_positioning_z (Some (EAdd (ENum (1))
  (EVar V_RPE_grid_positioning_z)))) 30)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_RPE_grid_positioning => Pedges_RPE_grid_positioning
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_RPE_grid_positioning => 34
     end)%positive;
  var_global := var_global
}.

Definition ai_RPE_grid_positioning (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_RPE_grid_positioning_z <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 3 => (-1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 4 => (1 * s V_RPE_grid_positioning_z <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 13 <= 0)%Z
   | 5 => (-1 * s V_RPE_grid_positioning_i + 13 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 6 => (1 * s V_RPE_grid_positioning_z <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 13 <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + 1 <= 0)%Z
   | 7 => (1 * s V_RPE_grid_positioning_z <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 13 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0)%Z
   | 8 => (-1 * s V_RPE_grid_positioning__tmp <= 0 /\ -1 * s V_RPE_grid_positioning_i + 13 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 9 => (1 * s V_RPE_grid_positioning_z <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 13 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp + 4 <= 0)%Z
   | 10 => (-1 * s V_RPE_grid_positioning_i + 13 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 11 => (1 * s V_RPE_grid_positioning_z <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 13 <= 0)%Z
   | 12 => (1 * s V_RPE_grid_positioning_z <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 13 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0)%Z
   | 13 => (1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ -1 * s V_RPE_grid_positioning_i + 13 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 14 => (1 * s V_RPE_grid_positioning_z <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 13 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0)%Z
   | 15 => (1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ -1 * s V_RPE_grid_positioning_i + 13 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 16 => (1 * s V_RPE_grid_positioning_z <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 13 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0)%Z
   | 17 => (1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ -1 * s V_RPE_grid_positioning_i + 13 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 18 => (-1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0)%Z
   | 19 => (-1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 20 => (-1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0)%Z
   | 21 => (-1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 22 => (-1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning_i + -12 <= 0)%Z
   | 23 => (1 * s V_RPE_grid_positioning_i + -12 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 24 => (-1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning_i + -1 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 1 <= 0)%Z
   | 25 => (-1 * s V_RPE_grid_positioning_i + 1 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -1 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 26 => (-1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning_i + -12 <= 0)%Z
   | 27 => (1 * s V_RPE_grid_positioning_i + -12 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0)%Z
   | 28 => (-1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning_i + -12 <= 0)%Z
   | 29 => (1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp <= 0 /\ -1 * s V_RPE_grid_positioning_i + 1 <= 0)%Z
   | 30 => (-1 * s V_RPE_grid_positioning__tmp <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -3 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 1 <= 0)%Z
   | 31 => (-1 * s V_RPE_grid_positioning_i + 1 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ -1 * s V_RPE_grid_positioning__tmp + 1 <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -4 <= 0)%Z
   | 32 => (1 * s V_RPE_grid_positioning__tmp + -4 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp + 1 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 1 <= 0)%Z
   | 33 => (-1 * s V_RPE_grid_positioning_i + 1 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -4 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp + 3 <= 0)%Z
   | 34 => (-1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 1 <= 0)%Z
   | 35 => (-1 * s V_RPE_grid_positioning_i + 1 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ -1 * s V_RPE_grid_positioning__tmp + 1 <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -2 <= 0)%Z
   | 36 => (1 * s V_RPE_grid_positioning__tmp + -2 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp + 1 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 1 <= 0)%Z
   | 37 => (-1 * s V_RPE_grid_positioning_i + 1 <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ -1 * s V_RPE_grid_positioning__tmp + 1 <= 0 /\ 1 * s V_RPE_grid_positioning__tmp + -2 <= 0)%Z
   | 38 => (1 * s V_RPE_grid_positioning__tmp + -2 <= 0 /\ -1 * s V_RPE_grid_positioning__tmp + 1 <= 0 /\ -1 * s V_RPE_grid_positioning_z <= 0 /\ 1 * s V_RPE_grid_positioning_i + -13 <= 0 /\ -1 * s V_RPE_grid_positioning_i + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_RPE_grid_positioning (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((15 # 4) * max0(4 - s V_RPE_grid_positioning_Mc)
           + (11 # 4) * max0(s V_RPE_grid_positioning_Mc) <= z)%Q
   | 2 => (s V_RPE_grid_positioning_z
           + (15 # 4) * max0(4 - s V_RPE_grid_positioning_Mc)
           + (11 # 4) * max0(s V_RPE_grid_positioning_Mc) <= z)%Q
   | 3 => (s V_RPE_grid_positioning_z
           + (15 # 4) * max0(4 - s V_RPE_grid_positioning__tmp)
           + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 4 => (-(11 # 4) + s V_RPE_grid_positioning_z
           + (37 # 48) * max0(-13 + s V_RPE_grid_positioning_i)
           + (11 # 48) * max0(-1 + s V_RPE_grid_positioning_i)
           + (15 # 4) * max0(4 - s V_RPE_grid_positioning__tmp)
           + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 5 => (-(11 # 4) + s V_RPE_grid_positioning_z
           + (37 # 48) * max0(-13 + s V_RPE_grid_positioning_i)
           + (11 # 48) * max0(-1 + s V_RPE_grid_positioning_i)
           + (15 # 4) * max0(4 - s V_RPE_grid_positioning__tmp)
           + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 6 => hints
     [(*0 1.375*) F_max0_ge_0 (2 - s V_RPE_grid_positioning__tmp);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_RPE_grid_positioning_z) (0))) (F_max0_ge_0 (s V_RPE_grid_positioning_z));
      (*0 2.75*) F_binom_monotonic 1 (F_max0_ge_0 (s V_RPE_grid_positioning__tmp)) (F_check_ge (0) (0));
      (*-2.375 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                     - s V_RPE_grid_positioning__tmp)) (F_check_ge (0) (0));
      (*-1.375 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                       - s V_RPE_grid_positioning__tmp)) (F_check_ge (4
                                                                    - s V_RPE_grid_positioning__tmp) (0));
      (*-1.375 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                   - 
                                                                   s V_RPE_grid_positioning__tmp) (0))) (F_max0_ge_0 (2
                                                                    - s V_RPE_grid_positioning__tmp));
      (*0 0.229167*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                       + s V_RPE_grid_positioning_i)) (F_check_ge (0) (0));
      (*0 0.770833*) F_binom_monotonic 1 (F_max0_ge_0 (-13
                                                       + s V_RPE_grid_positioning_i)) (F_check_ge (0) (0))]
     (-(11 # 4) + s V_RPE_grid_positioning_z
      + (37 # 48) * max0(-13 + s V_RPE_grid_positioning_i)
      + (11 # 48) * max0(-1 + s V_RPE_grid_positioning_i)
      + (15 # 4) * max0(4 - s V_RPE_grid_positioning__tmp)
      + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 7 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_RPE_grid_positioning_z) (0))) (F_max0_ge_0 (s V_RPE_grid_positioning_z));
      (*0 0.0833333*) F_binom_monotonic 1 (F_max0_ge_arg (13
                                                          - s V_RPE_grid_positioning_i)) (F_check_ge (13
                                                                    - s V_RPE_grid_positioning_i) (0))]
     (-(11 # 4) + s V_RPE_grid_positioning_z
      + (37 # 48) * max0(-13 + s V_RPE_grid_positioning_i)
      + (11 # 48) * max0(-1 + s V_RPE_grid_positioning_i)
      + (15 # 4) * max0(4 - s V_RPE_grid_positioning__tmp)
      + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 8 => (-(5 # 3) - (1 # 12) * s V_RPE_grid_positioning_i
           + (37 # 48) * max0(-13 + s V_RPE_grid_positioning_i)
           + (11 # 48) * max0(-1 + s V_RPE_grid_positioning_i)
           + (15 # 4) * max0(4 - s V_RPE_grid_positioning__tmp)
           - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
           + (11 # 4) * max0(s V_RPE_grid_positioning__tmp)
           + max0(s V_RPE_grid_positioning_z) <= z)%Q
   | 9 => hints
     [(*-2.75 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_RPE_grid_positioning__tmp)) (F_check_ge (s V_RPE_grid_positioning__tmp) (0));
      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                                    - s V_RPE_grid_positioning_i) (0))) (F_max0_ge_0 (13
                                                                    - s V_RPE_grid_positioning_i));
      (*-3.75 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                    - s V_RPE_grid_positioning__tmp)) (F_check_ge (0) (0));
      (*-0.229167 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                        + s V_RPE_grid_positioning_i)) (F_check_ge (0) (0));
      (*-2.75 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                    + s V_RPE_grid_positioning__tmp)) (F_check_ge (0) (0));
      (*-2.75 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                  + s V_RPE_grid_positioning__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_RPE_grid_positioning__tmp));
      (*-0.770833 0*) F_binom_monotonic 1 (F_max0_ge_0 (-13
                                                        + s V_RPE_grid_positioning_i)) (F_check_ge (0) (0))]
     (-(5 # 3) - (1 # 12) * s V_RPE_grid_positioning_i
      + (37 # 48) * max0(-13 + s V_RPE_grid_positioning_i)
      + (11 # 48) * max0(-1 + s V_RPE_grid_positioning_i)
      + (15 # 4) * max0(4 - s V_RPE_grid_positioning__tmp)
      - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
      + (11 # 4) * max0(s V_RPE_grid_positioning__tmp)
      + max0(s V_RPE_grid_positioning_z) <= z)%Q
   | 10 => (max0(s V_RPE_grid_positioning_z) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_RPE_grid_positioning_z)) (F_check_ge (s V_RPE_grid_positioning_z) (0))]
     (max0(s V_RPE_grid_positioning_z) <= z)%Q
   | 12 => (-(5 # 3) - (1 # 12) * s V_RPE_grid_positioning_i
            + (37 # 48) * max0(-13 + s V_RPE_grid_positioning_i)
            + (11 # 48) * max0(-1 + s V_RPE_grid_positioning_i)
            + (15 # 4) * max0(4 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp)
            + max0(s V_RPE_grid_positioning_z) <= z)%Q
   | 13 => (-(5 # 3) - (1 # 12) * s V_RPE_grid_positioning_i
            + (37 # 48) * max0(-13 + s V_RPE_grid_positioning_i)
            + (11 # 48) * max0(-1 + s V_RPE_grid_positioning_i)
            + (15 # 4) * max0(4 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp)
            + max0(s V_RPE_grid_positioning_z) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                             - s V_RPE_grid_positioning__tmp) (2
                                                                    - s V_RPE_grid_positioning__tmp));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_RPE_grid_positioning_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_RPE_grid_positioning_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_RPE_grid_positioning_z) (0))) (F_max0_ge_0 (-
                                                                    s V_RPE_grid_positioning_z));
      (*-3.75 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                      - s V_RPE_grid_positioning__tmp)) (F_check_ge (4
                                                                    - s V_RPE_grid_positioning__tmp) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                               - s V_RPE_grid_positioning__tmp) (0))) (F_max0_ge_0 (3
                                                                    - s V_RPE_grid_positioning__tmp));
      (*0 0.229167*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                         + s V_RPE_grid_positioning_i)) (F_check_ge (-1
                                                                    + s V_RPE_grid_positioning_i) (0));
      (*-0.770833 0*) F_binom_monotonic 1 (F_max0_ge_arg (-13
                                                          + s V_RPE_grid_positioning_i)) (F_check_ge (-13
                                                                    + s V_RPE_grid_positioning_i) (0))]
     (-(5 # 3) - (1 # 12) * s V_RPE_grid_positioning_i
      + (37 # 48) * max0(-13 + s V_RPE_grid_positioning_i)
      + (11 # 48) * max0(-1 + s V_RPE_grid_positioning_i)
      + (15 # 4) * max0(4 - s V_RPE_grid_positioning__tmp)
      - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
      + (11 # 4) * max0(s V_RPE_grid_positioning__tmp)
      + max0(s V_RPE_grid_positioning_z) <= z)%Q
   | 15 => ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 16 => ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 17 => ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 18 => ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 19 => ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 20 => ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 21 => ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 22 => hints
     [(*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (12
                                                                    - s V_RPE_grid_positioning_i) (0))) (F_max0_ge_0 (12
                                                                    - s V_RPE_grid_positioning_i))]
     ((1 # 1) - (11 # 4) * s V_RPE_grid_positioning__tmp
      + (11 # 12) * s V_RPE_grid_positioning_i + s V_RPE_grid_positioning_z
      + max0(2 - s V_RPE_grid_positioning__tmp)
      - (1 # 12) * max0(12 - s V_RPE_grid_positioning_i)
      + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 23 => (-(11 # 4) * s V_RPE_grid_positioning__tmp
            + s V_RPE_grid_positioning_i + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 24 => hints
     [(*-0.0833333 0*) F_binom_monotonic 1 (F_max0_ge_0 (13
                                                         - s V_RPE_grid_positioning_i)) (F_check_ge (0) (0));
      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                         + s V_RPE_grid_positioning_i)) (F_check_ge (0) (0));
      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_RPE_grid_positioning_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_RPE_grid_positioning_i))]
     (-(11 # 4) * s V_RPE_grid_positioning__tmp + s V_RPE_grid_positioning_i
      + s V_RPE_grid_positioning_z + max0(2 - s V_RPE_grid_positioning__tmp)
      + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 25 => ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 26 => hints
     [(*-0.0833333 0*) F_binom_monotonic 1 (F_max0_ge_arg (13
                                                           - s V_RPE_grid_positioning_i)) (F_check_ge (13
                                                                    - s V_RPE_grid_positioning_i) (0))]
     (-(11 # 4) * s V_RPE_grid_positioning__tmp + s V_RPE_grid_positioning_i
      + s V_RPE_grid_positioning_z + max0(2 - s V_RPE_grid_positioning__tmp)
      + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 27 => ((13 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 28 => ((13 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 29 => ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 30 => ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 31 => hints
     [(*0 2.75*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_RPE_grid_positioning__tmp) (0))) (F_max0_ge_0 (s V_RPE_grid_positioning__tmp));
      (*-2.75 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                      + s V_RPE_grid_positioning__tmp)) (F_check_ge (-1
                                                                    + s V_RPE_grid_positioning__tmp) (0))]
     ((17 # 6) - (11 # 4) * s V_RPE_grid_positioning__tmp
      + (11 # 12) * s V_RPE_grid_positioning_i + s V_RPE_grid_positioning_z
      + (11 # 4) * max0(-1 + s V_RPE_grid_positioning__tmp)
      + max0(3 - s V_RPE_grid_positioning__tmp)
      - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i) <= z)%Q
   | 32 => ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(3 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                             - s V_RPE_grid_positioning__tmp) (2
                                                                    - s V_RPE_grid_positioning__tmp));
      (*-1 0*) F_max0_ge_0 (2 - s V_RPE_grid_positioning__tmp);
      (*-2.75 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_RPE_grid_positioning__tmp)) (F_check_ge (s V_RPE_grid_positioning__tmp) (0));
      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                                    - s V_RPE_grid_positioning_i) (0))) (F_max0_ge_0 (13
                                                                    - s V_RPE_grid_positioning_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_RPE_grid_positioning_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_RPE_grid_positioning_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_RPE_grid_positioning_i))]
     ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
      + (11 # 12) * s V_RPE_grid_positioning_i + s V_RPE_grid_positioning_z
      + max0(3 - s V_RPE_grid_positioning__tmp)
      - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
      + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 34 => (s V_RPE_grid_positioning_z <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (3 - s V_RPE_grid_positioning__tmp) (1)]
     ((1 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
      + (11 # 12) * s V_RPE_grid_positioning_i + s V_RPE_grid_positioning_z
      + max0(3 - s V_RPE_grid_positioning__tmp)
      - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
      + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 36 => ((13 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 37 => ((13 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | 38 => ((13 # 12) - (11 # 4) * s V_RPE_grid_positioning__tmp
            + (11 # 12) * s V_RPE_grid_positioning_i
            + s V_RPE_grid_positioning_z
            + max0(2 - s V_RPE_grid_positioning__tmp)
            - (1 # 12) * max0(13 - s V_RPE_grid_positioning_i)
            + (11 # 4) * max0(s V_RPE_grid_positioning__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_RPE_grid_positioning =>
    [mkPA Q (fun n z s => ai_RPE_grid_positioning n s /\ annot0_RPE_grid_positioning n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_RPE_grid_positioning (proc_start P_RPE_grid_positioning) s1 (proc_end P_RPE_grid_positioning) s2 ->
    (s2 V_RPE_grid_positioning_z <= (15 # 4) * max0(4
                                                    - s1 V_RPE_grid_positioning_Mc)
                                    + (11 # 4) * max0(s1 V_RPE_grid_positioning_Mc))%Q.
Proof.
  prove_bound ipa admissible_ipa P_RPE_grid_positioning.
Qed.
