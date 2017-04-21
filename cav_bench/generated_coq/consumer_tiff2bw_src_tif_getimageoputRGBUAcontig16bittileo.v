Require Import pasta.Pasta.

Inductive proc: Type :=
  P_putRGBUAcontig16bittile.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_putRGBUAcontig16bittile_z := 1%positive.
Notation V_putRGBUAcontig16bittile__tmp := 2%positive.
Notation V_putRGBUAcontig16bittile__tmp1 := 3%positive.
Notation V_putRGBUAcontig16bittile__tmp2 := 4%positive.
Notation V_putRGBUAcontig16bittile__tmp3 := 5%positive.
Notation V_putRGBUAcontig16bittile__tmp4 := 6%positive.
Notation V_putRGBUAcontig16bittile__tmp5 := 7%positive.
Notation V_putRGBUAcontig16bittile_a := 8%positive.
Notation V_putRGBUAcontig16bittile_b := 9%positive.
Notation V_putRGBUAcontig16bittile_g := 10%positive.
Notation V_putRGBUAcontig16bittile_img_dref_off30 := 11%positive.
Notation V_putRGBUAcontig16bittile_r := 12%positive.
Notation V_putRGBUAcontig16bittile_samplesperpixel := 13%positive.
Notation V_putRGBUAcontig16bittile_cp := 14%positive.
Notation V_putRGBUAcontig16bittile_fromskew := 15%positive.
Notation V_putRGBUAcontig16bittile_h := 16%positive.
Notation V_putRGBUAcontig16bittile_img := 17%positive.
Notation V_putRGBUAcontig16bittile_pp := 18%positive.
Notation V_putRGBUAcontig16bittile_toskew := 19%positive.
Notation V_putRGBUAcontig16bittile_w := 20%positive.
Notation V_putRGBUAcontig16bittile_x := 21%positive.
Notation V_putRGBUAcontig16bittile_y := 22%positive.
Definition Pedges_putRGBUAcontig16bittile: list (edge proc) :=
  (EA 1 (AAssign V_putRGBUAcontig16bittile_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_putRGBUAcontig16bittile__tmp1) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_putRGBUAcontig16bittile__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_putRGBUAcontig16bittile__tmp1
  (Some (EVar V_putRGBUAcontig16bittile_x))) 6)::(EA 6 (AAssign
  V_putRGBUAcontig16bittile__tmp5
  (Some (EVar V_putRGBUAcontig16bittile_y))) 7)::(EA 7 (AAssign
  V_putRGBUAcontig16bittile__tmp2
  (Some (EVar V_putRGBUAcontig16bittile_w))) 8)::(EA 8 (AAssign
  V_putRGBUAcontig16bittile__tmp
  (Some (EVar V_putRGBUAcontig16bittile_h))) 9)::(EA 9 (AAssign
  V_putRGBUAcontig16bittile__tmp3
  (Some (EVar V_putRGBUAcontig16bittile_fromskew))) 10)::(EA 10 (AAssign
  V_putRGBUAcontig16bittile__tmp4
  (Some (EVar V_putRGBUAcontig16bittile_toskew))) 11)::(EA 11 (AAssign
  V_putRGBUAcontig16bittile_samplesperpixel
  (Some (EVar V_putRGBUAcontig16bittile_img_dref_off30))) 12)::
  (EA 12 (AAssign V_putRGBUAcontig16bittile__tmp3
  (Some (EMul (EVar V_putRGBUAcontig16bittile__tmp3)
  (EVar V_putRGBUAcontig16bittile_samplesperpixel)))) 13)::(EA 13 ANone 14)::
  (EA 14 (AAssign V_putRGBUAcontig16bittile__tmp
  (Some (EAdd (EVar V_putRGBUAcontig16bittile__tmp) (ENum (-1))))) 15)::
  (EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_putRGBUAcontig16bittile__tmp) s) >
  (eval (ENum (0)) s))%Z)) 19)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_putRGBUAcontig16bittile__tmp) s) <=
  (eval (ENum (0)) s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 19 AWeaken 20)::
  (EA 20 (AAssign V_putRGBUAcontig16bittile__tmp1
  (Some (EVar V_putRGBUAcontig16bittile__tmp2))) 21)::(EA 21 ANone 22)::
  (EA 22 (AAssign V_putRGBUAcontig16bittile__tmp1
  (Some (EAdd (EVar V_putRGBUAcontig16bittile__tmp1) (ENum (-1))))) 23)::
  (EA 23 AWeaken 24)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_putRGBUAcontig16bittile__tmp1) s) >
  (eval (ENum (0)) s))%Z)) 29)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_putRGBUAcontig16bittile__tmp1) s) <=
  (eval (ENum (0)) s))%Z)) 25)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::
  (EA 27 ANone 28)::(EA 28 (AAssign V_putRGBUAcontig16bittile_z
  (Some (EAdd (ENum (1)) (EVar V_putRGBUAcontig16bittile_z)))) 14)::
  (EA 29 AWeaken 30)::(EA 30 (AAssign V_putRGBUAcontig16bittile_a None) 31)::
  (EA 31 (AAssign V_putRGBUAcontig16bittile_r None) 32)::(EA 32 (AAssign
  V_putRGBUAcontig16bittile_g None) 33)::(EA 33 (AAssign
  V_putRGBUAcontig16bittile_b None) 34)::(EA 34 ANone 35)::(EA 35 ANone 36)::
  (EA 36 (AAssign V_putRGBUAcontig16bittile_z (Some (EAdd (ENum (1))
  (EVar V_putRGBUAcontig16bittile_z)))) 22)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_putRGBUAcontig16bittile => Pedges_putRGBUAcontig16bittile
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_putRGBUAcontig16bittile => 18
     end)%positive;
  var_global := var_global
}.

Definition ai_putRGBUAcontig16bittile (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 3 => (-1 * s V_putRGBUAcontig16bittile_z <= 0 /\ 1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp1 <= 0)%Z
   | 4 => (-1 * s V_putRGBUAcontig16bittile__tmp1 <= 0 /\ 1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp <= 0)%Z
   | 5 => (-1 * s V_putRGBUAcontig16bittile__tmp <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ 1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp1 <= 0)%Z
   | 6 => (1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp <= 0)%Z
   | 7 => (-1 * s V_putRGBUAcontig16bittile__tmp <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ 1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 8 => (1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp <= 0)%Z
   | 9 => (-1 * s V_putRGBUAcontig16bittile_z <= 0 /\ 1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 10 => (1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 11 => (-1 * s V_putRGBUAcontig16bittile_z <= 0 /\ 1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 12 => (1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 13 => (-1 * s V_putRGBUAcontig16bittile_z <= 0 /\ 1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 14 => (-1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 15 => (-1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 16 => (-1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 17 => (-1 * s V_putRGBUAcontig16bittile_z <= 0 /\ 1 * s V_putRGBUAcontig16bittile__tmp <= 0)%Z
   | 18 => (1 * s V_putRGBUAcontig16bittile__tmp <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 19 => (-1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0)%Z
   | 20 => (-1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 21 => (-1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0)%Z
   | 22 => (-1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0)%Z
   | 23 => (-1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0)%Z
   | 24 => (-1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0)%Z
   | 25 => (-1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ 1 * s V_putRGBUAcontig16bittile__tmp1 <= 0)%Z
   | 26 => (1 * s V_putRGBUAcontig16bittile__tmp1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0)%Z
   | 27 => (-1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ 1 * s V_putRGBUAcontig16bittile__tmp1 <= 0)%Z
   | 28 => (1 * s V_putRGBUAcontig16bittile__tmp1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0)%Z
   | 29 => (-1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp1 + 1 <= 0)%Z
   | 30 => (-1 * s V_putRGBUAcontig16bittile__tmp1 + 1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0)%Z
   | 31 => (-1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp1 + 1 <= 0)%Z
   | 32 => (-1 * s V_putRGBUAcontig16bittile__tmp1 + 1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0)%Z
   | 33 => (-1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp1 + 1 <= 0)%Z
   | 34 => (-1 * s V_putRGBUAcontig16bittile__tmp1 + 1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0)%Z
   | 35 => (-1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp1 + 1 <= 0)%Z
   | 36 => (-1 * s V_putRGBUAcontig16bittile__tmp1 + 1 <= 0 /\ -1 * s V_putRGBUAcontig16bittile_z <= 0 /\ -1 * s V_putRGBUAcontig16bittile__tmp + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_putRGBUAcontig16bittile (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_putRGBUAcontig16bittile_h)
           + max0(-1 + s V_putRGBUAcontig16bittile_h) * max0(-1
                                                             + s V_putRGBUAcontig16bittile_w) <= z)%Q
   | 2 => (s V_putRGBUAcontig16bittile_z
           + max0(-1 + s V_putRGBUAcontig16bittile_h)
           + max0(-1 + s V_putRGBUAcontig16bittile_h) * max0(-1
                                                             + s V_putRGBUAcontig16bittile_w) <= z)%Q
   | 3 => (s V_putRGBUAcontig16bittile_z
           + max0(-1 + s V_putRGBUAcontig16bittile_h)
           + max0(-1 + s V_putRGBUAcontig16bittile_h) * max0(-1
                                                             + s V_putRGBUAcontig16bittile_w) <= z)%Q
   | 4 => (s V_putRGBUAcontig16bittile_z
           + max0(-1 + s V_putRGBUAcontig16bittile_h)
           + max0(-1 + s V_putRGBUAcontig16bittile_h) * max0(-1
                                                             + s V_putRGBUAcontig16bittile_w) <= z)%Q
   | 5 => (s V_putRGBUAcontig16bittile_z
           + max0(-1 + s V_putRGBUAcontig16bittile_h)
           + max0(-1 + s V_putRGBUAcontig16bittile_h) * max0(-1
                                                             + s V_putRGBUAcontig16bittile_w) <= z)%Q
   | 6 => (s V_putRGBUAcontig16bittile_z
           + max0(-1 + s V_putRGBUAcontig16bittile_h)
           + max0(-1 + s V_putRGBUAcontig16bittile_h) * max0(-1
                                                             + s V_putRGBUAcontig16bittile_w) <= z)%Q
   | 7 => (s V_putRGBUAcontig16bittile_z
           + max0(-1 + s V_putRGBUAcontig16bittile_h)
           + max0(-1 + s V_putRGBUAcontig16bittile_h) * max0(-1
                                                             + s V_putRGBUAcontig16bittile_w) <= z)%Q
   | 8 => (s V_putRGBUAcontig16bittile_z
           + max0(-1 + s V_putRGBUAcontig16bittile__tmp2) * max0(-1
                                                                 + s V_putRGBUAcontig16bittile_h)
           + max0(-1 + s V_putRGBUAcontig16bittile_h) <= z)%Q
   | 9 => (s V_putRGBUAcontig16bittile_z
           + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
           + max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 10 => (s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 11 => (s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 12 => (s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 13 => (s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 14 => (s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 15 => (s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp2) * max0(s V_putRGBUAcontig16bittile__tmp)
            + max0(s V_putRGBUAcontig16bittile__tmp) <= z)%Q
   | 16 => (s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp2) * max0(s V_putRGBUAcontig16bittile__tmp)
            + max0(s V_putRGBUAcontig16bittile__tmp) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_putRGBUAcontig16bittile__tmp) (-1
                                                                    + s V_putRGBUAcontig16bittile__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_putRGBUAcontig16bittile__tmp);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                            + s V_putRGBUAcontig16bittile__tmp2)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBUAcontig16bittile__tmp)) (F_check_ge (0) (0)))]
     (s V_putRGBUAcontig16bittile_z
      + max0(-1 + s V_putRGBUAcontig16bittile__tmp2) * max0(s V_putRGBUAcontig16bittile__tmp)
      + max0(s V_putRGBUAcontig16bittile__tmp) <= z)%Q
   | 18 => (s V_putRGBUAcontig16bittile_z <= z)%Q
   | 19 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_putRGBUAcontig16bittile__tmp) (1);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBUAcontig16bittile__tmp)) (F_check_ge (s V_putRGBUAcontig16bittile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBUAcontig16bittile__tmp2)) (F_check_ge (0) (0)))]
     (s V_putRGBUAcontig16bittile_z
      + max0(-1 + s V_putRGBUAcontig16bittile__tmp2) * max0(s V_putRGBUAcontig16bittile__tmp)
      + max0(s V_putRGBUAcontig16bittile__tmp) <= z)%Q
   | 20 => ((1 # 1)
            + s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                      + s V_putRGBUAcontig16bittile__tmp2)
            + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp) <= z)%Q
   | 21 => ((1 # 1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp2)
            + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp2)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp1)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 22 => ((1 # 1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp2)
            + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp2)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp1)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 23 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBUAcontig16bittile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBUAcontig16bittile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBUAcontig16bittile__tmp1)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                               + s V_putRGBUAcontig16bittile__tmp)) (F_check_ge (-1
                                                                    + s V_putRGBUAcontig16bittile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBUAcontig16bittile__tmp1)) (F_check_ge (0) (0)))]
     ((1 # 1)
      + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                          + s V_putRGBUAcontig16bittile__tmp2)
      + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(s V_putRGBUAcontig16bittile__tmp1)
      + s V_putRGBUAcontig16bittile_z
      + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
      + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + 
                                                                    s V_putRGBUAcontig16bittile__tmp2)
      - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(s V_putRGBUAcontig16bittile__tmp1)
      - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2)
      + (1 # 2) * max0(s V_putRGBUAcontig16bittile__tmp1) <= z)%Q
   | 24 => ((1 # 1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp2)
            + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp2)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp1)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2)
            + max0(s V_putRGBUAcontig16bittile__tmp1) <= z)%Q
   | 25 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBUAcontig16bittile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBUAcontig16bittile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBUAcontig16bittile__tmp1)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBUAcontig16bittile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBUAcontig16bittile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBUAcontig16bittile__tmp2)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBUAcontig16bittile__tmp1)) (F_check_ge (0) (0))]
     ((1 # 1)
      + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                          + s V_putRGBUAcontig16bittile__tmp1)
      + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                          + s V_putRGBUAcontig16bittile__tmp2)
      + s V_putRGBUAcontig16bittile_z
      + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
      - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + 
                                                                    s V_putRGBUAcontig16bittile__tmp1)
      + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + 
                                                                    s V_putRGBUAcontig16bittile__tmp2)
      - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp1)
      - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2)
      + max0(s V_putRGBUAcontig16bittile__tmp1) <= z)%Q
   | 26 => ((1 # 1) + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 27 => ((1 # 1) + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 28 => ((1 # 1) + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 29 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_putRGBUAcontig16bittile__tmp1) (1)]
     ((1 # 1)
      + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                          + s V_putRGBUAcontig16bittile__tmp1)
      + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                          + s V_putRGBUAcontig16bittile__tmp2)
      + s V_putRGBUAcontig16bittile_z
      + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
      - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + 
                                                                    s V_putRGBUAcontig16bittile__tmp1)
      + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + 
                                                                    s V_putRGBUAcontig16bittile__tmp2)
      - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp1)
      - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2)
      + max0(s V_putRGBUAcontig16bittile__tmp1) <= z)%Q
   | 30 => ((2 # 1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp2)
            + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp2)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp1)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 31 => ((2 # 1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp2)
            + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp2)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp1)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 32 => ((2 # 1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp2)
            + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp2)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp1)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 33 => ((2 # 1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp2)
            + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp2)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp1)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 34 => ((2 # 1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp2)
            + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp2)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp1)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 35 => ((2 # 1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp2)
            + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp2)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp1)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | 36 => ((2 # 1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * s V_putRGBUAcontig16bittile__tmp * max0(-1
                                                                + s V_putRGBUAcontig16bittile__tmp2)
            + s V_putRGBUAcontig16bittile_z
            + max0(-1 + s V_putRGBUAcontig16bittile__tmp)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp1)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp) * max0(-1
                                                                    + s V_putRGBUAcontig16bittile__tmp2)
            + (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp1)
            - (1 # 2) * max0(-1 + s V_putRGBUAcontig16bittile__tmp2) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_putRGBUAcontig16bittile =>
    [mkPA Q (fun n z s => ai_putRGBUAcontig16bittile n s /\ annot0_putRGBUAcontig16bittile n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_putRGBUAcontig16bittile (proc_start P_putRGBUAcontig16bittile) s1 (proc_end P_putRGBUAcontig16bittile) s2 ->
    (s2 V_putRGBUAcontig16bittile_z <= max0(-1
                                            + s1 V_putRGBUAcontig16bittile_h)
                                       + max0(-1
                                              + s1 V_putRGBUAcontig16bittile_h) * max0(-1
                                                                    + s1 V_putRGBUAcontig16bittile_w))%Q.
Proof.
  prove_bound ipa admissible_ipa P_putRGBUAcontig16bittile.
Qed.
