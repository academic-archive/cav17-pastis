Require Import pasta.Pasta.

Inductive proc: Type :=
  P_putRGBseparate16bittile.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_putRGBseparate16bittile_z := 1%positive.
Notation V_putRGBseparate16bittile__tmp := 2%positive.
Notation V_putRGBseparate16bittile__tmp1 := 3%positive.
Notation V_putRGBseparate16bittile__tmp2 := 4%positive.
Notation V_putRGBseparate16bittile__tmp3 := 5%positive.
Notation V_putRGBseparate16bittile__tmp4 := 6%positive.
Notation V_putRGBseparate16bittile__tmp5 := 7%positive.
Notation V_putRGBseparate16bittile_a := 8%positive.
Notation V_putRGBseparate16bittile_b := 9%positive.
Notation V_putRGBseparate16bittile_cp := 10%positive.
Notation V_putRGBseparate16bittile_fromskew := 11%positive.
Notation V_putRGBseparate16bittile_g := 12%positive.
Notation V_putRGBseparate16bittile_h := 13%positive.
Notation V_putRGBseparate16bittile_img := 14%positive.
Notation V_putRGBseparate16bittile_r := 15%positive.
Notation V_putRGBseparate16bittile_toskew := 16%positive.
Notation V_putRGBseparate16bittile_w := 17%positive.
Notation V_putRGBseparate16bittile_x := 18%positive.
Notation V_putRGBseparate16bittile_y := 19%positive.
Definition Pedges_putRGBseparate16bittile: list (edge proc) :=
  (EA 1 (AAssign V_putRGBseparate16bittile_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_putRGBseparate16bittile__tmp2) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate16bittile__tmp1) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate16bittile__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 (AAssign
  V_putRGBseparate16bittile__tmp1
  (Some (EVar V_putRGBseparate16bittile_x))) 7)::(EA 7 (AAssign
  V_putRGBseparate16bittile__tmp5
  (Some (EVar V_putRGBseparate16bittile_y))) 8)::(EA 8 (AAssign
  V_putRGBseparate16bittile__tmp2
  (Some (EVar V_putRGBseparate16bittile_w))) 9)::(EA 9 (AAssign
  V_putRGBseparate16bittile__tmp
  (Some (EVar V_putRGBseparate16bittile_h))) 10)::(EA 10 (AAssign
  V_putRGBseparate16bittile__tmp4
  (Some (EVar V_putRGBseparate16bittile_fromskew))) 11)::(EA 11 (AAssign
  V_putRGBseparate16bittile__tmp3
  (Some (EVar V_putRGBseparate16bittile_toskew))) 12)::(EA 12 ANone 13)::
  (EA 13 (AAssign V_putRGBseparate16bittile__tmp
  (Some (EAdd (EVar V_putRGBseparate16bittile__tmp) (ENum (-1))))) 14)::
  (EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate16bittile__tmp) s) >
  (eval (ENum (0)) s))%Z)) 18)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate16bittile__tmp) s) <=
  (eval (ENum (0)) s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 18 AWeaken 19)::
  (EA 19 (AAssign V_putRGBseparate16bittile__tmp1 (Some (ENum (0)))) 20)::
  (EA 20 ANone 21)::(EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate16bittile__tmp1) s) <
  (eval (EVar V_putRGBseparate16bittile__tmp2) s))%Z)) 27)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate16bittile__tmp1) s) >=
  (eval (EVar V_putRGBseparate16bittile__tmp2) s))%Z)) 23)::
  (EA 23 AWeaken 24)::(EA 24 ANone 25)::(EA 25 ANone 26)::(EA 26 (AAssign
  V_putRGBseparate16bittile_z (Some (EAdd (ENum (1))
  (EVar V_putRGBseparate16bittile_z)))) 13)::(EA 27 AWeaken 28)::
  (EA 28 ANone 29)::(EA 29 (AAssign V_putRGBseparate16bittile__tmp1
  (Some (EAdd (EVar V_putRGBseparate16bittile__tmp1) (ENum (1))))) 30)::
  (EA 30 ANone 31)::(EA 31 ANone 32)::(EA 32 (AAssign
  V_putRGBseparate16bittile_z (Some (EAdd (ENum (1))
  (EVar V_putRGBseparate16bittile_z)))) 33)::(EA 33 AWeaken 22)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_putRGBseparate16bittile => Pedges_putRGBseparate16bittile
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_putRGBseparate16bittile => 17
     end)%positive;
  var_global := var_global
}.

Definition ai_putRGBseparate16bittile (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0)%Z
   | 3 => (-1 * s V_putRGBseparate16bittile_z <= 0 /\ 1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp2 <= 0)%Z
   | 4 => (-1 * s V_putRGBseparate16bittile__tmp2 <= 0 /\ 1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 <= 0)%Z
   | 5 => (-1 * s V_putRGBseparate16bittile__tmp1 <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0 /\ 1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp2 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp <= 0)%Z
   | 6 => (-1 * s V_putRGBseparate16bittile__tmp <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp2 <= 0 /\ 1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 <= 0)%Z
   | 7 => (-1 * s V_putRGBseparate16bittile_z <= 0 /\ 1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp2 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp <= 0)%Z
   | 8 => (-1 * s V_putRGBseparate16bittile__tmp <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp2 <= 0 /\ 1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0)%Z
   | 9 => (-1 * s V_putRGBseparate16bittile_z <= 0 /\ 1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp <= 0)%Z
   | 10 => (1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0)%Z
   | 11 => (-1 * s V_putRGBseparate16bittile_z <= 0 /\ 1 * s V_putRGBseparate16bittile_z <= 0)%Z
   | 12 => (1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0)%Z
   | 13 => (-1 * s V_putRGBseparate16bittile_z <= 0)%Z
   | 14 => (-1 * s V_putRGBseparate16bittile_z <= 0)%Z
   | 15 => (-1 * s V_putRGBseparate16bittile_z <= 0)%Z
   | 16 => (-1 * s V_putRGBseparate16bittile_z <= 0 /\ 1 * s V_putRGBseparate16bittile__tmp <= 0)%Z
   | 17 => (1 * s V_putRGBseparate16bittile__tmp <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0)%Z
   | 18 => (-1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp + 1 <= 0)%Z
   | 19 => (-1 * s V_putRGBseparate16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0)%Z
   | 20 => (-1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp + 1 <= 0 /\ 1 * s V_putRGBseparate16bittile__tmp1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 <= 0)%Z
   | 21 => (-1 * s V_putRGBseparate16bittile__tmp1 <= 0 /\ 1 * s V_putRGBseparate16bittile__tmp1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0)%Z
   | 22 => (-1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp + 1 <= 0)%Z
   | 23 => (-1 * s V_putRGBseparate16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1+ 1 * s V_putRGBseparate16bittile__tmp2 <= 0)%Z
   | 24 => (-1 * s V_putRGBseparate16bittile__tmp1+ 1 * s V_putRGBseparate16bittile__tmp2 <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp + 1 <= 0)%Z
   | 25 => (-1 * s V_putRGBseparate16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1+ 1 * s V_putRGBseparate16bittile__tmp2 <= 0)%Z
   | 26 => (-1 * s V_putRGBseparate16bittile__tmp1+ 1 * s V_putRGBseparate16bittile__tmp2 <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp + 1 <= 0)%Z
   | 27 => (-1 * s V_putRGBseparate16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0 /\ 1 * s V_putRGBseparate16bittile__tmp1+ -1 * s V_putRGBseparate16bittile__tmp2 + 1 <= 0)%Z
   | 28 => (1 * s V_putRGBseparate16bittile__tmp1+ -1 * s V_putRGBseparate16bittile__tmp2 + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp + 1 <= 0)%Z
   | 29 => (-1 * s V_putRGBseparate16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0 /\ 1 * s V_putRGBseparate16bittile__tmp1+ -1 * s V_putRGBseparate16bittile__tmp2 + 1 <= 0)%Z
   | 30 => (-1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 + 1 <= 0 /\ 1 * s V_putRGBseparate16bittile__tmp1+ -1 * s V_putRGBseparate16bittile__tmp2 <= 0)%Z
   | 31 => (1 * s V_putRGBseparate16bittile__tmp1+ -1 * s V_putRGBseparate16bittile__tmp2 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile_z <= 0)%Z
   | 32 => (-1 * s V_putRGBseparate16bittile_z <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 + 1 <= 0 /\ 1 * s V_putRGBseparate16bittile__tmp1+ -1 * s V_putRGBseparate16bittile__tmp2 <= 0)%Z
   | 33 => (1 * s V_putRGBseparate16bittile__tmp1+ -1 * s V_putRGBseparate16bittile__tmp2 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp1 + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate16bittile_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_putRGBseparate16bittile (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_putRGBseparate16bittile_h)
           + max0(-1 + s V_putRGBseparate16bittile_h) * max0(s V_putRGBseparate16bittile_w) <= z)%Q
   | 2 => (s V_putRGBseparate16bittile_z
           + max0(-1 + s V_putRGBseparate16bittile_h)
           + max0(-1 + s V_putRGBseparate16bittile_h) * max0(s V_putRGBseparate16bittile_w) <= z)%Q
   | 3 => (s V_putRGBseparate16bittile_z
           + max0(-1 + s V_putRGBseparate16bittile_h)
           + max0(-1 + s V_putRGBseparate16bittile_h) * max0(s V_putRGBseparate16bittile_w) <= z)%Q
   | 4 => (s V_putRGBseparate16bittile_z
           + max0(-1 + s V_putRGBseparate16bittile_h)
           + max0(-1 + s V_putRGBseparate16bittile_h) * max0(s V_putRGBseparate16bittile_w) <= z)%Q
   | 5 => (s V_putRGBseparate16bittile_z
           + max0(-1 + s V_putRGBseparate16bittile_h)
           + max0(-1 + s V_putRGBseparate16bittile_h) * max0(s V_putRGBseparate16bittile_w) <= z)%Q
   | 6 => (s V_putRGBseparate16bittile_z
           + max0(-1 + s V_putRGBseparate16bittile_h)
           + max0(-1 + s V_putRGBseparate16bittile_h) * max0(s V_putRGBseparate16bittile_w) <= z)%Q
   | 7 => (s V_putRGBseparate16bittile_z
           + max0(-1 + s V_putRGBseparate16bittile_h)
           + max0(-1 + s V_putRGBseparate16bittile_h) * max0(s V_putRGBseparate16bittile_w) <= z)%Q
   | 8 => (s V_putRGBseparate16bittile_z
           + max0(-1 + s V_putRGBseparate16bittile_h)
           + max0(-1 + s V_putRGBseparate16bittile_h) * max0(s V_putRGBseparate16bittile_w) <= z)%Q
   | 9 => (s V_putRGBseparate16bittile_z
           + max0(-1 + s V_putRGBseparate16bittile_h)
           + max0(-1 + s V_putRGBseparate16bittile_h) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 10 => (s V_putRGBseparate16bittile_z
            + max0(-1 + s V_putRGBseparate16bittile__tmp)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 11 => (s V_putRGBseparate16bittile_z
            + max0(-1 + s V_putRGBseparate16bittile__tmp)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 12 => (s V_putRGBseparate16bittile_z
            + max0(-1 + s V_putRGBseparate16bittile__tmp)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 13 => (s V_putRGBseparate16bittile_z
            + max0(-1 + s V_putRGBseparate16bittile__tmp)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 14 => (s V_putRGBseparate16bittile_z
            + max0(s V_putRGBseparate16bittile__tmp)
            + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 15 => (s V_putRGBseparate16bittile_z
            + max0(s V_putRGBseparate16bittile__tmp)
            + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate16bittile__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate16bittile__tmp2)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate16bittile__tmp)) (F_check_ge (0) (0))]
     (s V_putRGBseparate16bittile_z + max0(s V_putRGBseparate16bittile__tmp)
      + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 17 => (s V_putRGBseparate16bittile_z <= z)%Q
   | 18 => hints
     [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate16bittile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate16bittile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate16bittile__tmp)) (F_check_ge (0) (0)));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate16bittile__tmp)) (F_check_ge (s V_putRGBseparate16bittile__tmp) (0));
      (*0 0.5*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_putRGBseparate16bittile__tmp) (0))) (F_max0_ge_0 (s V_putRGBseparate16bittile__tmp))]
     (s V_putRGBseparate16bittile_z + max0(s V_putRGBseparate16bittile__tmp)
      + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 19 => ((3 # 2) * s V_putRGBseparate16bittile__tmp
            - (1 # 2) * s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp)
            - (1 # 2) * s V_putRGBseparate16bittile__tmp^2
            + s V_putRGBseparate16bittile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp)
            + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
            + (1 # 2) * max0(s V_putRGBseparate16bittile__tmp)^2 <= z)%Q
   | 20 => ((3 # 2) * s V_putRGBseparate16bittile__tmp
            - (1 # 2) * s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp)
            + s V_putRGBseparate16bittile__tmp * max0(-s V_putRGBseparate16bittile__tmp1
                                                      + s V_putRGBseparate16bittile__tmp2)
            - s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp2)
            - (1 # 2) * s V_putRGBseparate16bittile__tmp^2
            + s V_putRGBseparate16bittile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp)
            - max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(-s V_putRGBseparate16bittile__tmp1
                                                                 + s V_putRGBseparate16bittile__tmp2)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
            + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
            + (1 # 2) * max0(s V_putRGBseparate16bittile__tmp)^2 <= z)%Q
   | 21 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate16bittile__tmp)) (F_check_ge (s V_putRGBseparate16bittile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate16bittile__tmp)) (F_check_ge (0) (0)))]
     ((3 # 2) * s V_putRGBseparate16bittile__tmp
      - (1 # 2) * s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp)
      + s V_putRGBseparate16bittile__tmp * max0(-s V_putRGBseparate16bittile__tmp1
                                                + s V_putRGBseparate16bittile__tmp2)
      - s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp2)
      - (1 # 2) * s V_putRGBseparate16bittile__tmp^2
      + s V_putRGBseparate16bittile_z
      + (1 # 2) * max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp)
      - max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(-s V_putRGBseparate16bittile__tmp1
                                                           + s V_putRGBseparate16bittile__tmp2)
      + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
      + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
      + (1 # 2) * max0(s V_putRGBseparate16bittile__tmp)^2 <= z)%Q
   | 22 => ((3 # 2) * s V_putRGBseparate16bittile__tmp
            + s V_putRGBseparate16bittile__tmp * max0(-s V_putRGBseparate16bittile__tmp1
                                                      + s V_putRGBseparate16bittile__tmp2)
            - s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp2)
            - (1 # 2) * s V_putRGBseparate16bittile__tmp^2
            + s V_putRGBseparate16bittile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp)
            - max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(-s V_putRGBseparate16bittile__tmp1
                                                                 + s V_putRGBseparate16bittile__tmp2)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
            + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 23 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (-s V_putRGBseparate16bittile__tmp1
                                            + s V_putRGBseparate16bittile__tmp2) (-1
                                                                    - s V_putRGBseparate16bittile__tmp1
                                                                    + s V_putRGBseparate16bittile__tmp2));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                     + s V_putRGBseparate16bittile__tmp)) (F_check_ge (-1
                                                                    + s V_putRGBseparate16bittile__tmp) (0));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate16bittile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate16bittile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate16bittile__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate16bittile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate16bittile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_putRGBseparate16bittile__tmp1
                                                                    + s V_putRGBseparate16bittile__tmp2)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate16bittile__tmp)) (F_check_ge (s V_putRGBseparate16bittile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate16bittile__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate16bittile__tmp)) (F_check_ge (s V_putRGBseparate16bittile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate16bittile__tmp2)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 - s V_putRGBseparate16bittile__tmp1
                                                 + s V_putRGBseparate16bittile__tmp2)) (F_check_ge (0) (0))]
     ((3 # 2) * s V_putRGBseparate16bittile__tmp
      + s V_putRGBseparate16bittile__tmp * max0(-s V_putRGBseparate16bittile__tmp1
                                                + s V_putRGBseparate16bittile__tmp2)
      - s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp2)
      - (1 # 2) * s V_putRGBseparate16bittile__tmp^2
      + s V_putRGBseparate16bittile_z
      + (1 # 2) * max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp)
      - max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(-s V_putRGBseparate16bittile__tmp1
                                                           + s V_putRGBseparate16bittile__tmp2)
      + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
      + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 24 => ((1 # 1) + s V_putRGBseparate16bittile_z
            + max0(-1 + s V_putRGBseparate16bittile__tmp)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 25 => ((1 # 1) + s V_putRGBseparate16bittile_z
            + max0(-1 + s V_putRGBseparate16bittile__tmp)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 26 => ((1 # 1) + s V_putRGBseparate16bittile_z
            + max0(-1 + s V_putRGBseparate16bittile__tmp)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 27 => hints
     [(*-1 2e-12*) F_max0_pre_decrement 1 (-s V_putRGBseparate16bittile__tmp1
                                           + s V_putRGBseparate16bittile__tmp2) (1);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate16bittile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate16bittile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_putRGBseparate16bittile__tmp1
                                                                    + s V_putRGBseparate16bittile__tmp2)) (F_check_ge (0) (0)))]
     ((3 # 2) * s V_putRGBseparate16bittile__tmp
      + s V_putRGBseparate16bittile__tmp * max0(-s V_putRGBseparate16bittile__tmp1
                                                + s V_putRGBseparate16bittile__tmp2)
      - s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp2)
      - (1 # 2) * s V_putRGBseparate16bittile__tmp^2
      + s V_putRGBseparate16bittile_z
      + (1 # 2) * max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp)
      - max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(-s V_putRGBseparate16bittile__tmp1
                                                           + s V_putRGBseparate16bittile__tmp2)
      + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
      + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 28 => ((1 # 1) + (3 # 2) * s V_putRGBseparate16bittile__tmp
            - s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp2)
            - (1 # 2) * s V_putRGBseparate16bittile__tmp^2
            + s V_putRGBseparate16bittile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
            + max0(-1 - s V_putRGBseparate16bittile__tmp1
                   + s V_putRGBseparate16bittile__tmp2)
            + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 29 => ((1 # 1) + (3 # 2) * s V_putRGBseparate16bittile__tmp
            - s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp2)
            - (1 # 2) * s V_putRGBseparate16bittile__tmp^2
            + s V_putRGBseparate16bittile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
            + max0(-1 - s V_putRGBseparate16bittile__tmp1
                   + s V_putRGBseparate16bittile__tmp2)
            + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 30 => ((1 # 1) + (3 # 2) * s V_putRGBseparate16bittile__tmp
            - s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp2)
            - (1 # 2) * s V_putRGBseparate16bittile__tmp^2
            + s V_putRGBseparate16bittile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
            + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
            + max0(-s V_putRGBseparate16bittile__tmp1
                   + s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 31 => ((1 # 1) + (3 # 2) * s V_putRGBseparate16bittile__tmp
            - s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp2)
            - (1 # 2) * s V_putRGBseparate16bittile__tmp^2
            + s V_putRGBseparate16bittile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
            + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
            + max0(-s V_putRGBseparate16bittile__tmp1
                   + s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 32 => ((1 # 1) + (3 # 2) * s V_putRGBseparate16bittile__tmp
            - s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp2)
            - (1 # 2) * s V_putRGBseparate16bittile__tmp^2
            + s V_putRGBseparate16bittile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp)
            + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
            + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
            + max0(-s V_putRGBseparate16bittile__tmp1
                   + s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                              + s V_putRGBseparate16bittile__tmp)) (F_check_ge (-1
                                                                    + s V_putRGBseparate16bittile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_putRGBseparate16bittile__tmp1
                                                                    + s V_putRGBseparate16bittile__tmp2)) (F_check_ge (0) (0)))]
     ((3 # 2) * s V_putRGBseparate16bittile__tmp
      - s V_putRGBseparate16bittile__tmp * max0(s V_putRGBseparate16bittile__tmp2)
      - (1 # 2) * s V_putRGBseparate16bittile__tmp^2
      + s V_putRGBseparate16bittile_z
      + (1 # 2) * max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp)
      + max0(-1 + s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
      + max0(s V_putRGBseparate16bittile__tmp) * max0(s V_putRGBseparate16bittile__tmp2)
      + max0(-s V_putRGBseparate16bittile__tmp1
             + s V_putRGBseparate16bittile__tmp2) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_putRGBseparate16bittile =>
    [mkPA Q (fun n z s => ai_putRGBseparate16bittile n s /\ annot0_putRGBseparate16bittile n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_putRGBseparate16bittile (proc_start P_putRGBseparate16bittile) s1 (proc_end P_putRGBseparate16bittile) s2 ->
    (s2 V_putRGBseparate16bittile_z <= max0(-1
                                            + s1 V_putRGBseparate16bittile_h)
                                       + max0(-1
                                              + s1 V_putRGBseparate16bittile_h) * max0(s1 V_putRGBseparate16bittile_w))%Q.
Proof.
  prove_bound ipa admissible_ipa P_putRGBseparate16bittile.
Qed.
