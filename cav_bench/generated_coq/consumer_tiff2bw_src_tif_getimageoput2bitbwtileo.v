Require Import pasta.Pasta.

Inductive proc: Type :=
  P_put2bitbwtile.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_put2bitbwtile_z := 1%positive.
Notation V_put2bitbwtile__tmp := 2%positive.
Notation V_put2bitbwtile__tmp1 := 3%positive.
Notation V_put2bitbwtile__tmp2 := 4%positive.
Notation V_put2bitbwtile__tmp3 := 5%positive.
Notation V_put2bitbwtile__tmp4 := 6%positive.
Notation V_put2bitbwtile__tmp5 := 7%positive.
Notation V_put2bitbwtile__x := 8%positive.
Notation V_put2bitbwtile_cp := 9%positive.
Notation V_put2bitbwtile_fromskew := 10%positive.
Notation V_put2bitbwtile_h := 11%positive.
Notation V_put2bitbwtile_img := 12%positive.
Notation V_put2bitbwtile_pp := 13%positive.
Notation V_put2bitbwtile_toskew := 14%positive.
Notation V_put2bitbwtile_w := 15%positive.
Notation V_put2bitbwtile_x := 16%positive.
Notation V_put2bitbwtile_y := 17%positive.
Definition Pedges_put2bitbwtile: list (edge proc) :=
  (EA 1 (AAssign V_put2bitbwtile_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_put2bitbwtile__x) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_put2bitbwtile__tmp)
  s) >= (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_put2bitbwtile__tmp5 (Some (EVar V_put2bitbwtile_x))) 6)::(EA 6 (AAssign
  V_put2bitbwtile__tmp4 (Some (EVar V_put2bitbwtile_y))) 7)::(EA 7 (AAssign
  V_put2bitbwtile__tmp1 (Some (EVar V_put2bitbwtile_w))) 8)::(EA 8 (AAssign
  V_put2bitbwtile__tmp (Some (EVar V_put2bitbwtile_h))) 9)::(EA 9 (AAssign
  V_put2bitbwtile__tmp2 (Some (EVar V_put2bitbwtile_fromskew))) 10)::
  (EA 10 (AAssign V_put2bitbwtile__tmp3
  (Some (EVar V_put2bitbwtile_toskew))) 11)::(EA 11 (AAssign
  V_put2bitbwtile__tmp2 None) 12)::(EA 12 ANone 13)::(EA 13 (AAssign
  V_put2bitbwtile__tmp (Some (EAdd (EVar V_put2bitbwtile__tmp)
  (ENum (-1))))) 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_put2bitbwtile__tmp) s) > (eval (ENum (0))
  s))%Z)) 18)::(EA 15 (AGuard (fun s => ((eval (EVar V_put2bitbwtile__tmp)
  s) <= (eval (ENum (0)) s))%Z)) 16)::(EA 16 AWeaken 17)::
  (EA 18 AWeaken 19)::(EA 19 (AAssign V_put2bitbwtile__x
  (Some (EVar V_put2bitbwtile__tmp1))) 20)::(EA 20 ANone 21)::
  (EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_put2bitbwtile__x) s) >= (eval (ENum (4))
  s))%Z)) 35)::(EA 22 (AGuard (fun s => ((eval (EVar V_put2bitbwtile__x) s) <
  (eval (ENum (4)) s))%Z)) 23)::(EA 23 AWeaken 24)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_put2bitbwtile__x) s) > (eval (ENum (0))
  s))%Z)) 26)::(EA 24 (AGuard (fun s => ((eval (EVar V_put2bitbwtile__x)
  s) <= (eval (ENum (0)) s))%Z)) 25)::(EA 25 AWeaken 32)::
  (EA 26 AWeaken 27)::(EA 27 ANone 31)::(EA 27 ANone 28)::(EA 27 ANone 29)::
  (EA 27 ANone 30)::(EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 ANone 31)::
  (EA 31 ANone 32)::(EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_put2bitbwtile_z (Some (EAdd (ENum (1)) (EVar V_put2bitbwtile_z)))) 13)::
  (EA 35 AWeaken 36)::(EA 36 ANone 37)::(EA 37 (AAssign V_put2bitbwtile__x
  (Some (ESub (EVar V_put2bitbwtile__x) (ENum (4))))) 38)::(EA 38 ANone 39)::
  (EA 39 ANone 40)::(EA 40 (AAssign V_put2bitbwtile_z (Some (EAdd (ENum (1))
  (EVar V_put2bitbwtile_z)))) 41)::(EA 41 AWeaken 22)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_put2bitbwtile => Pedges_put2bitbwtile
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_put2bitbwtile => 17
     end)%positive;
  var_global := var_global
}.

Definition ai_put2bitbwtile (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile_z <= 0)%Z
   | 3 => (-1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__x <= 0)%Z
   | 4 => (-1 * s V_put2bitbwtile__x <= 0 /\ 1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp <= 0)%Z
   | 5 => (-1 * s V_put2bitbwtile__tmp <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__x <= 0)%Z
   | 6 => (-1 * s V_put2bitbwtile__x <= 0 /\ 1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp <= 0)%Z
   | 7 => (-1 * s V_put2bitbwtile__tmp <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__x <= 0)%Z
   | 8 => (-1 * s V_put2bitbwtile__x <= 0 /\ 1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp <= 0)%Z
   | 9 => (-1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__x <= 0)%Z
   | 10 => (-1 * s V_put2bitbwtile__x <= 0 /\ 1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile_z <= 0)%Z
   | 11 => (-1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__x <= 0)%Z
   | 12 => (-1 * s V_put2bitbwtile__x <= 0 /\ 1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile_z <= 0)%Z
   | 13 => (-1 * s V_put2bitbwtile_z <= 0)%Z
   | 14 => (-1 * s V_put2bitbwtile_z <= 0)%Z
   | 15 => (-1 * s V_put2bitbwtile_z <= 0)%Z
   | 16 => (-1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile__tmp <= 0)%Z
   | 17 => (1 * s V_put2bitbwtile__tmp <= 0 /\ -1 * s V_put2bitbwtile_z <= 0)%Z
   | 18 => (-1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0)%Z
   | 19 => (-1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0)%Z
   | 20 => (-1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0)%Z
   | 21 => (-1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0)%Z
   | 22 => (-1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0)%Z
   | 23 => (-1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile__x + -3 <= 0)%Z
   | 24 => (1 * s V_put2bitbwtile__x + -3 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0)%Z
   | 25 => (-1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile__x <= 0)%Z
   | 26 => (-1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile__x + -3 <= 0 /\ -1 * s V_put2bitbwtile__x + 1 <= 0)%Z
   | 27 => (-1 * s V_put2bitbwtile__x + 1 <= 0 /\ 1 * s V_put2bitbwtile__x + -3 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0)%Z
   | 28 => (-1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile__x + -3 <= 0 /\ -1 * s V_put2bitbwtile__x + 1 <= 0)%Z
   | 29 => (-1 * s V_put2bitbwtile__x + 1 <= 0 /\ 1 * s V_put2bitbwtile__x + -3 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0)%Z
   | 30 => (-1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile__x + -3 <= 0 /\ -1 * s V_put2bitbwtile__x + 1 <= 0)%Z
   | 31 => (-1 * s V_put2bitbwtile__x + 1 <= 0 /\ 1 * s V_put2bitbwtile__x + -3 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0)%Z
   | 32 => (-1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile__x + -3 <= 0)%Z
   | 33 => (1 * s V_put2bitbwtile__x + -3 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0)%Z
   | 34 => (-1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ 1 * s V_put2bitbwtile__x + -3 <= 0)%Z
   | 35 => (-1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__x + 4 <= 0)%Z
   | 36 => (-1 * s V_put2bitbwtile__x + 4 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0)%Z
   | 37 => (-1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__x + 4 <= 0)%Z
   | 38 => (-1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile__x <= 0)%Z
   | 39 => (-1 * s V_put2bitbwtile__x <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z <= 0)%Z
   | 40 => (-1 * s V_put2bitbwtile_z <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile__x <= 0)%Z
   | 41 => (-1 * s V_put2bitbwtile__x <= 0 /\ -1 * s V_put2bitbwtile__tmp + 1 <= 0 /\ -1 * s V_put2bitbwtile_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_put2bitbwtile (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_put2bitbwtile_h)
           + (1 # 4) * max0(-1 + s V_put2bitbwtile_h) * max0(s V_put2bitbwtile_w) <= z)%Q
   | 2 => (s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile_h)
           + (1 # 4) * max0(-1 + s V_put2bitbwtile_h) * max0(s V_put2bitbwtile_w) <= z)%Q
   | 3 => (s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile_h)
           + (1 # 4) * max0(-1 + s V_put2bitbwtile_h) * max0(s V_put2bitbwtile_w) <= z)%Q
   | 4 => (s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile_h)
           + (1 # 4) * max0(-1 + s V_put2bitbwtile_h) * max0(s V_put2bitbwtile_w) <= z)%Q
   | 5 => (s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile_h)
           + (1 # 4) * max0(-1 + s V_put2bitbwtile_h) * max0(s V_put2bitbwtile_w) <= z)%Q
   | 6 => (s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile_h)
           + (1 # 4) * max0(-1 + s V_put2bitbwtile_h) * max0(s V_put2bitbwtile_w) <= z)%Q
   | 7 => (s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile_h)
           + (1 # 4) * max0(-1 + s V_put2bitbwtile_h) * max0(s V_put2bitbwtile_w) <= z)%Q
   | 8 => (s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile_h)
           + (1 # 4) * max0(-1 + s V_put2bitbwtile_h) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 9 => (s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
           + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 10 => (s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 11 => (s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 12 => (s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 13 => (s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 14 => (s V_put2bitbwtile_z + max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 15 => (s V_put2bitbwtile_z + max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_put2bitbwtile__tmp) (-1
                                                                    + s V_put2bitbwtile__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_put2bitbwtile__tmp);
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_put2bitbwtile__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_put2bitbwtile__tmp1)) (F_check_ge (0) (0)))]
     (s V_put2bitbwtile_z + max0(s V_put2bitbwtile__tmp)
      + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 17 => (s V_put2bitbwtile_z <= z)%Q
   | 18 => hints
     [(*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                             + s V_put2bitbwtile__tmp)) (F_check_ge (-1
                                                                    + s V_put2bitbwtile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_put2bitbwtile__tmp)) (F_check_ge (0) (0)))]
     (s V_put2bitbwtile_z + max0(s V_put2bitbwtile__tmp)
      + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 19 => (s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
            + s V_put2bitbwtile_z
            - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 20 => (s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
            + s V_put2bitbwtile_z
            - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1)
            - (1 # 4) * max0(s V_put2bitbwtile__tmp1)
            + (1 # 4) * max0(s V_put2bitbwtile__x) <= z)%Q
   | 21 => (s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
            + s V_put2bitbwtile_z
            - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1)
            - (1 # 4) * max0(s V_put2bitbwtile__tmp1)
            + (1 # 4) * max0(s V_put2bitbwtile__x) <= z)%Q
   | 22 => (s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
            + s V_put2bitbwtile_z
            - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1)
            - (1 # 4) * max0(s V_put2bitbwtile__tmp1)
            + (1 # 4) * max0(s V_put2bitbwtile__x) <= z)%Q
   | 23 => hints
     [(*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                 + s V_put2bitbwtile__tmp)) (F_check_ge (-1
                                                                    + s V_put2bitbwtile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_put2bitbwtile__x)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_put2bitbwtile__tmp)) (F_check_ge (s V_put2bitbwtile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_put2bitbwtile__tmp1)) (F_check_ge (0) (0)))]
     (s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
      + s V_put2bitbwtile_z
      - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
      + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1)
      - (1 # 4) * max0(s V_put2bitbwtile__tmp1)
      + (1 # 4) * max0(s V_put2bitbwtile__x) <= z)%Q
   | 24 => (s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp1)
            + (1 # 4) * s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__x)
            + s V_put2bitbwtile_z
            - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
            - (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__x)
            - (1 # 4) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_put2bitbwtile__tmp) (1);
      (*-0.25 0*) F_max0_monotonic (F_check_ge (s V_put2bitbwtile__x) (-4
                                                                    + s V_put2bitbwtile__x));
      (*-0.25 0*) F_max0_ge_0 (-4 + s V_put2bitbwtile__x);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_put2bitbwtile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_put2bitbwtile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_put2bitbwtile__tmp)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_put2bitbwtile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_put2bitbwtile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_put2bitbwtile__tmp1)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_put2bitbwtile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_put2bitbwtile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_put2bitbwtile__x)) (F_check_ge (0) (0)))]
     (s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
      + (1 # 4) * s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp1)
      + (1 # 4) * s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__x)
      + s V_put2bitbwtile_z
      - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
      - (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__x)
      - (1 # 4) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_put2bitbwtile__tmp) (1);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_put2bitbwtile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_put2bitbwtile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_put2bitbwtile__tmp)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_put2bitbwtile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_put2bitbwtile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_put2bitbwtile__tmp1)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_put2bitbwtile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_put2bitbwtile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_put2bitbwtile__x)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_put2bitbwtile__x)) (F_check_ge (s V_put2bitbwtile__x) (0));
      (*0 0.125*) F_binom_monotonic 1 (F_max0_ge_0 (3 - s V_put2bitbwtile__x)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                   - 
                                                                   s V_put2bitbwtile__x) (0))) (F_max0_ge_0 (3
                                                                    - s V_put2bitbwtile__x));
      (*0 0.375*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_put2bitbwtile__x)) (F_check_ge (0) (0));
      (*-0.375 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                   + 
                                                                   s V_put2bitbwtile__x) (0))) (F_max0_ge_0 (-1
                                                                    + s V_put2bitbwtile__x))]
     (s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
      + (1 # 4) * s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp1)
      + (1 # 4) * s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__x)
      + s V_put2bitbwtile_z
      - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
      - (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__x)
      - (1 # 4) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 27 => ((1 # 1) + s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 28 => ((1 # 1) + s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 29 => ((1 # 1) + s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 30 => ((1 # 1) + s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 31 => ((1 # 1) + s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 32 => ((1 # 1) + s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 33 => ((1 # 1) + s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 34 => ((1 # 1) + s V_put2bitbwtile_z + max0(-1 + s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 35 => hints
     [(*-0.25 0*) F_max0_pre_decrement 1 (s V_put2bitbwtile__x) (4)]
     (s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
      + s V_put2bitbwtile_z
      - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
      + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1)
      - (1 # 4) * max0(s V_put2bitbwtile__tmp1)
      + (1 # 4) * max0(s V_put2bitbwtile__x) <= z)%Q
   | 36 => ((1 # 1) + s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
            + s V_put2bitbwtile_z + (1 # 4) * max0(-4 + s V_put2bitbwtile__x)
            - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1)
            - (1 # 4) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 37 => ((1 # 1) + s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
            + s V_put2bitbwtile_z + (1 # 4) * max0(-4 + s V_put2bitbwtile__x)
            - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1)
            - (1 # 4) * max0(s V_put2bitbwtile__tmp1) <= z)%Q
   | 38 => ((1 # 1) + s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
            + s V_put2bitbwtile_z
            - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1)
            - (1 # 4) * max0(s V_put2bitbwtile__tmp1)
            + (1 # 4) * max0(s V_put2bitbwtile__x) <= z)%Q
   | 39 => ((1 # 1) + s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
            + s V_put2bitbwtile_z
            - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1)
            - (1 # 4) * max0(s V_put2bitbwtile__tmp1)
            + (1 # 4) * max0(s V_put2bitbwtile__x) <= z)%Q
   | 40 => ((1 # 1) + s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
            + s V_put2bitbwtile_z
            - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1)
            - (1 # 4) * max0(s V_put2bitbwtile__tmp1)
            + (1 # 4) * max0(s V_put2bitbwtile__x) <= z)%Q
   | 41 => (s V_put2bitbwtile__tmp * max0(s V_put2bitbwtile__tmp)
            + s V_put2bitbwtile_z
            - max0(-1 + s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp)
            + (1 # 4) * max0(s V_put2bitbwtile__tmp) * max0(s V_put2bitbwtile__tmp1)
            - (1 # 4) * max0(s V_put2bitbwtile__tmp1)
            + (1 # 4) * max0(s V_put2bitbwtile__x) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_put2bitbwtile =>
    [mkPA Q (fun n z s => ai_put2bitbwtile n s /\ annot0_put2bitbwtile n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_put2bitbwtile (proc_start P_put2bitbwtile) s1 (proc_end P_put2bitbwtile) s2 ->
    (s2 V_put2bitbwtile_z <= max0(-1 + s1 V_put2bitbwtile_h)
                             + (1 # 4) * max0(-1 + s1 V_put2bitbwtile_h) * max0(s1 V_put2bitbwtile_w))%Q.
Proof.
  prove_bound ipa admissible_ipa P_put2bitbwtile.
Qed.
