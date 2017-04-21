Require Import pasta.Pasta.

Inductive proc: Type :=
  P_putRGBseparate8bittile.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_putRGBseparate8bittile_z := 1%positive.
Notation V_putRGBseparate8bittile__tmp := 2%positive.
Notation V_putRGBseparate8bittile__tmp1 := 3%positive.
Notation V_putRGBseparate8bittile__tmp2 := 4%positive.
Notation V_putRGBseparate8bittile__tmp3 := 5%positive.
Notation V_putRGBseparate8bittile__tmp4 := 6%positive.
Notation V_putRGBseparate8bittile__tmp5 := 7%positive.
Notation V_putRGBseparate8bittile__x := 8%positive.
Notation V_putRGBseparate8bittile_a := 9%positive.
Notation V_putRGBseparate8bittile_b := 10%positive.
Notation V_putRGBseparate8bittile_cp := 11%positive.
Notation V_putRGBseparate8bittile_fromskew := 12%positive.
Notation V_putRGBseparate8bittile_g := 13%positive.
Notation V_putRGBseparate8bittile_h := 14%positive.
Notation V_putRGBseparate8bittile_img := 15%positive.
Notation V_putRGBseparate8bittile_r := 16%positive.
Notation V_putRGBseparate8bittile_toskew := 17%positive.
Notation V_putRGBseparate8bittile_w := 18%positive.
Notation V_putRGBseparate8bittile_x := 19%positive.
Notation V_putRGBseparate8bittile_y := 20%positive.
Definition Pedges_putRGBseparate8bittile: list (edge proc) :=
  (EA 1 (AAssign V_putRGBseparate8bittile_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_putRGBseparate8bittile__x) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate8bittile__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_putRGBseparate8bittile__tmp5
  (Some (EVar V_putRGBseparate8bittile_x))) 6)::(EA 6 (AAssign
  V_putRGBseparate8bittile__tmp4
  (Some (EVar V_putRGBseparate8bittile_y))) 7)::(EA 7 (AAssign
  V_putRGBseparate8bittile__tmp1
  (Some (EVar V_putRGBseparate8bittile_w))) 8)::(EA 8 (AAssign
  V_putRGBseparate8bittile__tmp
  (Some (EVar V_putRGBseparate8bittile_h))) 9)::(EA 9 (AAssign
  V_putRGBseparate8bittile__tmp3
  (Some (EVar V_putRGBseparate8bittile_fromskew))) 10)::(EA 10 (AAssign
  V_putRGBseparate8bittile__tmp2
  (Some (EVar V_putRGBseparate8bittile_toskew))) 11)::(EA 11 ANone 12)::
  (EA 12 (AAssign V_putRGBseparate8bittile__tmp
  (Some (EAdd (EVar V_putRGBseparate8bittile__tmp) (ENum (-1))))) 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate8bittile__tmp) s) > (eval (ENum (0))
  s))%Z)) 17)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate8bittile__tmp) s) <=
  (eval (ENum (0)) s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 17 AWeaken 18)::
  (EA 18 (AAssign V_putRGBseparate8bittile__x
  (Some (EVar V_putRGBseparate8bittile__tmp1))) 19)::(EA 19 ANone 20)::
  (EA 20 AWeaken 21)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate8bittile__x) s) >= (eval (ENum (8))
  s))%Z)) 38)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate8bittile__x) s) < (eval (ENum (8))
  s))%Z)) 22)::(EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate8bittile__x) s) > (eval (ENum (0))
  s))%Z)) 25)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate8bittile__x) s) <= (eval (ENum (0))
  s))%Z)) 24)::(EA 24 AWeaken 35)::(EA 25 AWeaken 26)::(EA 26 ANone 34)::
  (EA 26 ANone 27)::(EA 26 ANone 28)::(EA 26 ANone 29)::(EA 26 ANone 30)::
  (EA 26 ANone 31)::(EA 26 ANone 32)::(EA 26 ANone 33)::(EA 27 ANone 28)::
  (EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 ANone 32)::
  (EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 ANone 35)::(EA 35 ANone 36)::
  (EA 36 ANone 37)::(EA 37 (AAssign V_putRGBseparate8bittile_z
  (Some (EAdd (ENum (1)) (EVar V_putRGBseparate8bittile_z)))) 12)::
  (EA 38 AWeaken 39)::(EA 39 ANone 40)::(EA 40 (AAssign
  V_putRGBseparate8bittile__x (Some (ESub (EVar V_putRGBseparate8bittile__x)
  (ENum (8))))) 41)::(EA 41 ANone 42)::(EA 42 ANone 43)::(EA 43 (AAssign
  V_putRGBseparate8bittile_z (Some (EAdd (ENum (1))
  (EVar V_putRGBseparate8bittile_z)))) 44)::(EA 44 AWeaken 21)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_putRGBseparate8bittile => Pedges_putRGBseparate8bittile
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_putRGBseparate8bittile => 16
     end)%positive;
  var_global := var_global
}.

Definition ai_putRGBseparate8bittile (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0)%Z
   | 3 => (-1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__x <= 0)%Z
   | 4 => (-1 * s V_putRGBseparate8bittile__x <= 0 /\ 1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp <= 0)%Z
   | 5 => (-1 * s V_putRGBseparate8bittile__tmp <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__x <= 0)%Z
   | 6 => (-1 * s V_putRGBseparate8bittile__x <= 0 /\ 1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp <= 0)%Z
   | 7 => (-1 * s V_putRGBseparate8bittile__tmp <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__x <= 0)%Z
   | 8 => (-1 * s V_putRGBseparate8bittile__x <= 0 /\ 1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp <= 0)%Z
   | 9 => (-1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__x <= 0)%Z
   | 10 => (-1 * s V_putRGBseparate8bittile__x <= 0 /\ 1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0)%Z
   | 11 => (-1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__x <= 0)%Z
   | 12 => (-1 * s V_putRGBseparate8bittile_z <= 0)%Z
   | 13 => (-1 * s V_putRGBseparate8bittile_z <= 0)%Z
   | 14 => (-1 * s V_putRGBseparate8bittile_z <= 0)%Z
   | 15 => (-1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile__tmp <= 0)%Z
   | 16 => (1 * s V_putRGBseparate8bittile__tmp <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0)%Z
   | 17 => (-1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0)%Z
   | 18 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0)%Z
   | 19 => (-1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0)%Z
   | 20 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0)%Z
   | 21 => (-1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0)%Z
   | 22 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0)%Z
   | 23 => (1 * s V_putRGBseparate8bittile__x + -7 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0)%Z
   | 24 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile__x <= 0)%Z
   | 25 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0 /\ -1 * s V_putRGBseparate8bittile__x + 1 <= 0)%Z
   | 26 => (-1 * s V_putRGBseparate8bittile__x + 1 <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0)%Z
   | 27 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0 /\ -1 * s V_putRGBseparate8bittile__x + 1 <= 0)%Z
   | 28 => (-1 * s V_putRGBseparate8bittile__x + 1 <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0)%Z
   | 29 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0 /\ -1 * s V_putRGBseparate8bittile__x + 1 <= 0)%Z
   | 30 => (-1 * s V_putRGBseparate8bittile__x + 1 <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0)%Z
   | 31 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0 /\ -1 * s V_putRGBseparate8bittile__x + 1 <= 0)%Z
   | 32 => (-1 * s V_putRGBseparate8bittile__x + 1 <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0)%Z
   | 33 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0 /\ -1 * s V_putRGBseparate8bittile__x + 1 <= 0)%Z
   | 34 => (-1 * s V_putRGBseparate8bittile__x + 1 <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0)%Z
   | 35 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0)%Z
   | 36 => (1 * s V_putRGBseparate8bittile__x + -7 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0)%Z
   | 37 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ 1 * s V_putRGBseparate8bittile__x + -7 <= 0)%Z
   | 38 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__x + 8 <= 0)%Z
   | 39 => (-1 * s V_putRGBseparate8bittile__x + 8 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0)%Z
   | 40 => (-1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__x + 8 <= 0)%Z
   | 41 => (-1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile__x <= 0)%Z
   | 42 => (-1 * s V_putRGBseparate8bittile__x <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z <= 0)%Z
   | 43 => (-1 * s V_putRGBseparate8bittile_z <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile__x <= 0)%Z
   | 44 => (-1 * s V_putRGBseparate8bittile__x <= 0 /\ -1 * s V_putRGBseparate8bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bittile_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_putRGBseparate8bittile (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_putRGBseparate8bittile_h)
           + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile_h) * max0(s V_putRGBseparate8bittile_w) <= z)%Q
   | 2 => (s V_putRGBseparate8bittile_z
           + max0(-1 + s V_putRGBseparate8bittile_h)
           + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile_h) * max0(s V_putRGBseparate8bittile_w) <= z)%Q
   | 3 => (s V_putRGBseparate8bittile_z
           + max0(-1 + s V_putRGBseparate8bittile_h)
           + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile_h) * max0(s V_putRGBseparate8bittile_w) <= z)%Q
   | 4 => (s V_putRGBseparate8bittile_z
           + max0(-1 + s V_putRGBseparate8bittile_h)
           + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile_h) * max0(s V_putRGBseparate8bittile_w) <= z)%Q
   | 5 => (s V_putRGBseparate8bittile_z
           + max0(-1 + s V_putRGBseparate8bittile_h)
           + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile_h) * max0(s V_putRGBseparate8bittile_w) <= z)%Q
   | 6 => (s V_putRGBseparate8bittile_z
           + max0(-1 + s V_putRGBseparate8bittile_h)
           + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile_h) * max0(s V_putRGBseparate8bittile_w) <= z)%Q
   | 7 => (s V_putRGBseparate8bittile_z
           + max0(-1 + s V_putRGBseparate8bittile_h)
           + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile_h) * max0(s V_putRGBseparate8bittile_w) <= z)%Q
   | 8 => (s V_putRGBseparate8bittile_z
           + max0(-1 + s V_putRGBseparate8bittile_h)
           + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile_h) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 9 => (s V_putRGBseparate8bittile_z
           + max0(-1 + s V_putRGBseparate8bittile__tmp)
           + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 10 => (s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 11 => (s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 12 => (s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 13 => (s V_putRGBseparate8bittile_z
            + max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 14 => (s V_putRGBseparate8bittile_z
            + max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_putRGBseparate8bittile__tmp) (-1
                                                                    + s V_putRGBseparate8bittile__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_putRGBseparate8bittile__tmp);
      (*-0.125 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bittile__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bittile__tmp1)) (F_check_ge (0) (0)))]
     (s V_putRGBseparate8bittile_z + max0(s V_putRGBseparate8bittile__tmp)
      + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 16 => (s V_putRGBseparate8bittile_z <= z)%Q
   | 17 => (s V_putRGBseparate8bittile_z
            + max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 18 => (s V_putRGBseparate8bittile_z
            + max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 19 => (s V_putRGBseparate8bittile_z
            + max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1)
            - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__x) <= z)%Q
   | 20 => hints
     [(*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bittile__tmp)) (F_check_ge (s V_putRGBseparate8bittile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bittile__tmp)) (F_check_ge (0) (0)))]
     (s V_putRGBseparate8bittile_z + max0(s V_putRGBseparate8bittile__tmp)
      + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1)
      - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1)
      + (1 # 8) * max0(s V_putRGBseparate8bittile__x) <= z)%Q
   | 21 => ((1 # 3) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp)
            + s V_putRGBseparate8bittile_z
            + max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1)
            - (1 # 3) * max0(s V_putRGBseparate8bittile__tmp)^2
            - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__x) <= z)%Q
   | 22 => hints
     [(*-0.00416667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - s V_putRGBseparate8bittile__x) (0))) (F_max0_ge_0 (7
                                                                    - s V_putRGBseparate8bittile__x))) (F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                    - s V_putRGBseparate8bittile__x)) (F_check_ge (0) (0)));
      (*0 0.125*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bittile__tmp)) (F_check_ge (s V_putRGBseparate8bittile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bittile__tmp1)) (F_check_ge (0) (0)));
      (*0 0.666667*) F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bittile__tmp)) (F_check_ge (s V_putRGBseparate8bittile__tmp) (0));
      (*0 0.333333*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_putRGBseparate8bittile__tmp) (0))) (F_max0_ge_0 (s V_putRGBseparate8bittile__tmp))]
     ((1 # 3) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp)
      + s V_putRGBseparate8bittile_z + max0(s V_putRGBseparate8bittile__tmp)
      + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1)
      - (1 # 3) * max0(s V_putRGBseparate8bittile__tmp)^2
      - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1)
      + (1 # 8) * max0(s V_putRGBseparate8bittile__x) <= z)%Q
   | 23 => (s V_putRGBseparate8bittile__tmp
            + (1 # 3) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp1)
            - (1 # 3) * s V_putRGBseparate8bittile__tmp^2
            + (0 # 1) * s V_putRGBseparate8bittile__x * max0(7
                                                             - s V_putRGBseparate8bittile__x)
            + s V_putRGBseparate8bittile_z
            - (3 # 103) * max0(7 - s V_putRGBseparate8bittile__x)
            + (0 # 1) * max0(7 - s V_putRGBseparate8bittile__x)^2
            - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__x) <= z)%Q
   | 24 => hints
     [(*-0.333333 0*) F_max0_pre_decrement 1 (s V_putRGBseparate8bittile__tmp) (1);
      (*-0.125 0*) F_max0_monotonic (F_check_ge (s V_putRGBseparate8bittile__x) (-8
                                                                    + s V_putRGBseparate8bittile__x));
      (*-0.125 0*) F_max0_ge_0 (-8 + s V_putRGBseparate8bittile__x);
      (*-0.333333 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                          + s V_putRGBseparate8bittile__tmp)) (F_check_ge (-1
                                                                    + s V_putRGBseparate8bittile__tmp) (0));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate8bittile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bittile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bittile__tmp)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate8bittile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bittile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bittile__tmp)) (F_check_ge (0) (0)));
      (*-0.125 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate8bittile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bittile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bittile__tmp1)) (F_check_ge (0) (0)));
      (*-0.00416667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (7
                                                                    - s V_putRGBseparate8bittile__x)) (F_check_ge (7
                                                                    - s V_putRGBseparate8bittile__x) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                    - s V_putRGBseparate8bittile__x)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bittile__tmp)) (F_check_ge (s V_putRGBseparate8bittile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bittile__tmp)) (F_check_ge (0) (0)))]
     (s V_putRGBseparate8bittile__tmp
      + (1 # 3) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp)
      + (1 # 8) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp1)
      - (1 # 3) * s V_putRGBseparate8bittile__tmp^2
      + (0 # 1) * s V_putRGBseparate8bittile__x * max0(7
                                                       - s V_putRGBseparate8bittile__x)
      + s V_putRGBseparate8bittile_z
      - (3 # 103) * max0(7 - s V_putRGBseparate8bittile__x)
      + (0 # 1) * max0(7 - s V_putRGBseparate8bittile__x)^2
      - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1)
      + (1 # 8) * max0(s V_putRGBseparate8bittile__x) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_putRGBseparate8bittile__tmp) (1);
      (*-0.00416667 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                            + s V_putRGBseparate8bittile__x)) (F_check_ge (-1
                                                                    + s V_putRGBseparate8bittile__x) (0));
      (*-0.0291667 0*) F_binom_monotonic 2 (F_max0_ge_0 (-1
                                                         + s V_putRGBseparate8bittile__x)) (F_check_ge (0) (0));
      (*-0.00416667 0*) F_binom_monotonic 2 (F_max0_ge_0 (7
                                                          - s V_putRGBseparate8bittile__x)) (F_check_ge (0) (0));
      (*-0.333333 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_putRGBseparate8bittile__tmp)) (F_check_ge (s V_putRGBseparate8bittile__tmp) (0));
      (*-0.125 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate8bittile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bittile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bittile__tmp1)) (F_check_ge (0) (0)));
      (*-0.0333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate8bittile__x) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bittile__x))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bittile__x)) (F_check_ge (0) (0)));
      (*-0.0333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - s V_putRGBseparate8bittile__x) (0))) (F_max0_ge_0 (7
                                                                    - s V_putRGBseparate8bittile__x))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bittile__x)) (F_check_ge (0) (0)));
      (*-0.00416667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (7
                                                                    - s V_putRGBseparate8bittile__x)) (F_check_ge (7
                                                                    - s V_putRGBseparate8bittile__x) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bittile__x)) (F_check_ge (0) (0)));
      (*-0.0333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                    - 
                                                                    s V_putRGBseparate8bittile__x)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bittile__x)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_putRGBseparate8bittile__tmp) (0))) (F_max0_ge_0 (s V_putRGBseparate8bittile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bittile__tmp)) (F_check_ge (0) (0)));
      (*-0.00416667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_putRGBseparate8bittile__x) (0))) (F_max0_ge_0 (s V_putRGBseparate8bittile__x))) (F_binom_monotonic 1 (F_max0_ge_0 (7
                                                                    - s V_putRGBseparate8bittile__x)) (F_check_ge (0) (0)));
      (*-0.00416667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bittile__x)) (F_check_ge (s V_putRGBseparate8bittile__x) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bittile__x)) (F_check_ge (0) (0)));
      (*-0.15 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bittile__x)) (F_check_ge (s V_putRGBseparate8bittile__x) (0));
      (*-0.666667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_putRGBseparate8bittile__tmp) (0))) (F_max0_ge_0 (s V_putRGBseparate8bittile__tmp));
      (*-0.025 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                   - 
                                                                   s V_putRGBseparate8bittile__x) (0))) (F_max0_ge_0 (7
                                                                    - s V_putRGBseparate8bittile__x));
      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate8bittile__x) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bittile__x));
      (*-0.00416667 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_putRGBseparate8bittile__x) (0))) (F_max0_ge_0 (s V_putRGBseparate8bittile__x))]
     (s V_putRGBseparate8bittile__tmp
      + (1 # 3) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp)
      + (1 # 8) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp1)
      - (1 # 3) * s V_putRGBseparate8bittile__tmp^2
      + (0 # 1) * s V_putRGBseparate8bittile__x * max0(7
                                                       - s V_putRGBseparate8bittile__x)
      + s V_putRGBseparate8bittile_z
      - (3 # 103) * max0(7 - s V_putRGBseparate8bittile__x)
      + (0 # 1) * max0(7 - s V_putRGBseparate8bittile__x)^2
      - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1)
      + (1 # 8) * max0(s V_putRGBseparate8bittile__x) <= z)%Q
   | 26 => ((1 # 1) + s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 27 => ((1 # 1) + s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 28 => ((1 # 1) + s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 29 => ((1 # 1) + s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 30 => ((1 # 1) + s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 31 => ((1 # 1) + s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 32 => ((1 # 1) + s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 33 => ((1 # 1) + s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 34 => ((1 # 1) + s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 35 => ((1 # 1) + s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 36 => ((1 # 1) + s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 37 => ((1 # 1) + s V_putRGBseparate8bittile_z
            + max0(-1 + s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(-1 + s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 38 => hints
     [(*-0.125 0*) F_max0_pre_decrement 1 (s V_putRGBseparate8bittile__x) (8)]
     ((1 # 3) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp)
      + s V_putRGBseparate8bittile_z + max0(s V_putRGBseparate8bittile__tmp)
      + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1)
      - (1 # 3) * max0(s V_putRGBseparate8bittile__tmp)^2
      - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1)
      + (1 # 8) * max0(s V_putRGBseparate8bittile__x) <= z)%Q
   | 39 => ((1 # 1)
            + (1 # 3) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp)
            + s V_putRGBseparate8bittile_z
            + (1 # 8) * max0(-8 + s V_putRGBseparate8bittile__x)
            + max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1)
            - (1 # 3) * max0(s V_putRGBseparate8bittile__tmp)^2
            - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 40 => ((1 # 1)
            + (1 # 3) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp)
            + s V_putRGBseparate8bittile_z
            + (1 # 8) * max0(-8 + s V_putRGBseparate8bittile__x)
            + max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1)
            - (1 # 3) * max0(s V_putRGBseparate8bittile__tmp)^2
            - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1) <= z)%Q
   | 41 => ((1 # 1)
            + (1 # 3) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp)
            + s V_putRGBseparate8bittile_z
            + max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1)
            - (1 # 3) * max0(s V_putRGBseparate8bittile__tmp)^2
            - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__x) <= z)%Q
   | 42 => ((1 # 1)
            + (1 # 3) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp)
            + s V_putRGBseparate8bittile_z
            + max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1)
            - (1 # 3) * max0(s V_putRGBseparate8bittile__tmp)^2
            - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__x) <= z)%Q
   | 43 => ((1 # 1)
            + (1 # 3) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp)
            + s V_putRGBseparate8bittile_z
            + max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1)
            - (1 # 3) * max0(s V_putRGBseparate8bittile__tmp)^2
            - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__x) <= z)%Q
   | 44 => ((1 # 3) * s V_putRGBseparate8bittile__tmp * max0(s V_putRGBseparate8bittile__tmp)
            + s V_putRGBseparate8bittile_z
            + max0(s V_putRGBseparate8bittile__tmp)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__tmp) * max0(s V_putRGBseparate8bittile__tmp1)
            - (1 # 3) * max0(s V_putRGBseparate8bittile__tmp)^2
            - (1 # 8) * max0(s V_putRGBseparate8bittile__tmp1)
            + (1 # 8) * max0(s V_putRGBseparate8bittile__x) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_putRGBseparate8bittile =>
    [mkPA Q (fun n z s => ai_putRGBseparate8bittile n s /\ annot0_putRGBseparate8bittile n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_putRGBseparate8bittile (proc_start P_putRGBseparate8bittile) s1 (proc_end P_putRGBseparate8bittile) s2 ->
    (s2 V_putRGBseparate8bittile_z <= max0(-1 + s1 V_putRGBseparate8bittile_h)
                                      + (1 # 8) * max0(-1
                                                       + s1 V_putRGBseparate8bittile_h) * max0(s1 V_putRGBseparate8bittile_w))%Q.
Proof.
  prove_bound ipa admissible_ipa P_putRGBseparate8bittile.
Qed.
