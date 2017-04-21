Require Import pasta.Pasta.

Inductive proc: Type :=
  P_putRGBseparate8bitMaptile.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_putRGBseparate8bitMaptile_z := 1%positive.
Notation V_putRGBseparate8bitMaptile__tmp := 2%positive.
Notation V_putRGBseparate8bitMaptile__tmp1 := 3%positive.
Notation V_putRGBseparate8bitMaptile__tmp2 := 4%positive.
Notation V_putRGBseparate8bitMaptile__tmp3 := 5%positive.
Notation V_putRGBseparate8bitMaptile__tmp4 := 6%positive.
Notation V_putRGBseparate8bitMaptile__tmp5 := 7%positive.
Notation V_putRGBseparate8bitMaptile_a := 8%positive.
Notation V_putRGBseparate8bitMaptile_b := 9%positive.
Notation V_putRGBseparate8bitMaptile_cp := 10%positive.
Notation V_putRGBseparate8bitMaptile_fromskew := 11%positive.
Notation V_putRGBseparate8bitMaptile_g := 12%positive.
Notation V_putRGBseparate8bitMaptile_h := 13%positive.
Notation V_putRGBseparate8bitMaptile_img := 14%positive.
Notation V_putRGBseparate8bitMaptile_r := 15%positive.
Notation V_putRGBseparate8bitMaptile_toskew := 16%positive.
Notation V_putRGBseparate8bitMaptile_w := 17%positive.
Notation V_putRGBseparate8bitMaptile_x := 18%positive.
Notation V_putRGBseparate8bitMaptile_y := 19%positive.
Definition Pedges_putRGBseparate8bitMaptile: list (edge proc) :=
  (EA 1 (AAssign V_putRGBseparate8bitMaptile_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_putRGBseparate8bitMaptile__tmp1)
  s) >= (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate8bitMaptile__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_putRGBseparate8bitMaptile__tmp1
  (Some (EVar V_putRGBseparate8bitMaptile_x))) 6)::(EA 6 (AAssign
  V_putRGBseparate8bitMaptile__tmp5
  (Some (EVar V_putRGBseparate8bitMaptile_y))) 7)::(EA 7 (AAssign
  V_putRGBseparate8bitMaptile__tmp2
  (Some (EVar V_putRGBseparate8bitMaptile_w))) 8)::(EA 8 (AAssign
  V_putRGBseparate8bitMaptile__tmp
  (Some (EVar V_putRGBseparate8bitMaptile_h))) 9)::(EA 9 (AAssign
  V_putRGBseparate8bitMaptile__tmp4
  (Some (EVar V_putRGBseparate8bitMaptile_fromskew))) 10)::(EA 10 (AAssign
  V_putRGBseparate8bitMaptile__tmp3
  (Some (EVar V_putRGBseparate8bitMaptile_toskew))) 11)::(EA 11 ANone 12)::
  (EA 12 (AAssign V_putRGBseparate8bitMaptile__tmp
  (Some (EAdd (EVar V_putRGBseparate8bitMaptile__tmp) (ENum (-1))))) 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate8bitMaptile__tmp) s) >
  (eval (ENum (0)) s))%Z)) 17)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate8bitMaptile__tmp) s) <=
  (eval (ENum (0)) s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 17 AWeaken 18)::
  (EA 18 (AAssign V_putRGBseparate8bitMaptile__tmp1
  (Some (EVar V_putRGBseparate8bitMaptile__tmp2))) 19)::(EA 19 ANone 20)::
  (EA 20 AWeaken 21)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate8bitMaptile__tmp1) s) >
  (eval (ENum (0)) s))%Z)) 26)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_putRGBseparate8bitMaptile__tmp1) s) <=
  (eval (ENum (0)) s))%Z)) 22)::(EA 22 AWeaken 23)::(EA 23 ANone 24)::
  (EA 24 ANone 25)::(EA 25 (AAssign V_putRGBseparate8bitMaptile_z
  (Some (EAdd (ENum (1)) (EVar V_putRGBseparate8bitMaptile_z)))) 12)::
  (EA 26 AWeaken 27)::(EA 27 ANone 28)::(EA 28 (AAssign
  V_putRGBseparate8bitMaptile__tmp1
  (Some (EAdd (EVar V_putRGBseparate8bitMaptile__tmp1) (ENum (-1))))) 29)::
  (EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 (AAssign
  V_putRGBseparate8bitMaptile_z (Some (EAdd (ENum (1))
  (EVar V_putRGBseparate8bitMaptile_z)))) 32)::(EA 32 AWeaken 21)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_putRGBseparate8bitMaptile => Pedges_putRGBseparate8bitMaptile
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_putRGBseparate8bitMaptile => 16
     end)%positive;
  var_global := var_global
}.

Definition ai_putRGBseparate8bitMaptile (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0)%Z
   | 3 => (-1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ 1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp1 <= 0)%Z
   | 4 => (-1 * s V_putRGBseparate8bitMaptile__tmp1 <= 0 /\ 1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp <= 0)%Z
   | 5 => (-1 * s V_putRGBseparate8bitMaptile__tmp <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ 1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp1 <= 0)%Z
   | 6 => (1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp <= 0)%Z
   | 7 => (-1 * s V_putRGBseparate8bitMaptile__tmp <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ 1 * s V_putRGBseparate8bitMaptile_z <= 0)%Z
   | 8 => (1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp <= 0)%Z
   | 9 => (-1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ 1 * s V_putRGBseparate8bitMaptile_z <= 0)%Z
   | 10 => (1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0)%Z
   | 11 => (-1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ 1 * s V_putRGBseparate8bitMaptile_z <= 0)%Z
   | 12 => (-1 * s V_putRGBseparate8bitMaptile_z <= 0)%Z
   | 13 => (-1 * s V_putRGBseparate8bitMaptile_z <= 0)%Z
   | 14 => (-1 * s V_putRGBseparate8bitMaptile_z <= 0)%Z
   | 15 => (-1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ 1 * s V_putRGBseparate8bitMaptile__tmp <= 0)%Z
   | 16 => (1 * s V_putRGBseparate8bitMaptile__tmp <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0)%Z
   | 17 => (-1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0)%Z
   | 18 => (-1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0)%Z
   | 19 => (-1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0)%Z
   | 20 => (-1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0)%Z
   | 21 => (-1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0)%Z
   | 22 => (-1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ 1 * s V_putRGBseparate8bitMaptile__tmp1 <= 0)%Z
   | 23 => (1 * s V_putRGBseparate8bitMaptile__tmp1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0)%Z
   | 24 => (-1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ 1 * s V_putRGBseparate8bitMaptile__tmp1 <= 0)%Z
   | 25 => (1 * s V_putRGBseparate8bitMaptile__tmp1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0)%Z
   | 26 => (-1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp1 + 1 <= 0)%Z
   | 27 => (-1 * s V_putRGBseparate8bitMaptile__tmp1 + 1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0)%Z
   | 28 => (-1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp1 + 1 <= 0)%Z
   | 29 => (-1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp1 <= 0)%Z
   | 30 => (-1 * s V_putRGBseparate8bitMaptile__tmp1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z <= 0)%Z
   | 31 => (-1 * s V_putRGBseparate8bitMaptile_z <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp1 <= 0)%Z
   | 32 => (-1 * s V_putRGBseparate8bitMaptile__tmp1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile__tmp + 1 <= 0 /\ -1 * s V_putRGBseparate8bitMaptile_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_putRGBseparate8bitMaptile (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_putRGBseparate8bitMaptile_h)
           + max0(-1 + s V_putRGBseparate8bitMaptile_h) * max0(s V_putRGBseparate8bitMaptile_w) <= z)%Q
   | 2 => (s V_putRGBseparate8bitMaptile_z
           + max0(-1 + s V_putRGBseparate8bitMaptile_h)
           + max0(-1 + s V_putRGBseparate8bitMaptile_h) * max0(s V_putRGBseparate8bitMaptile_w) <= z)%Q
   | 3 => (s V_putRGBseparate8bitMaptile_z
           + max0(-1 + s V_putRGBseparate8bitMaptile_h)
           + max0(-1 + s V_putRGBseparate8bitMaptile_h) * max0(s V_putRGBseparate8bitMaptile_w) <= z)%Q
   | 4 => (s V_putRGBseparate8bitMaptile_z
           + max0(-1 + s V_putRGBseparate8bitMaptile_h)
           + max0(-1 + s V_putRGBseparate8bitMaptile_h) * max0(s V_putRGBseparate8bitMaptile_w) <= z)%Q
   | 5 => (s V_putRGBseparate8bitMaptile_z
           + max0(-1 + s V_putRGBseparate8bitMaptile_h)
           + max0(-1 + s V_putRGBseparate8bitMaptile_h) * max0(s V_putRGBseparate8bitMaptile_w) <= z)%Q
   | 6 => (s V_putRGBseparate8bitMaptile_z
           + max0(-1 + s V_putRGBseparate8bitMaptile_h)
           + max0(-1 + s V_putRGBseparate8bitMaptile_h) * max0(s V_putRGBseparate8bitMaptile_w) <= z)%Q
   | 7 => (s V_putRGBseparate8bitMaptile_z
           + max0(-1 + s V_putRGBseparate8bitMaptile_h)
           + max0(-1 + s V_putRGBseparate8bitMaptile_h) * max0(s V_putRGBseparate8bitMaptile_w) <= z)%Q
   | 8 => (s V_putRGBseparate8bitMaptile_z
           + max0(-1 + s V_putRGBseparate8bitMaptile_h)
           + max0(-1 + s V_putRGBseparate8bitMaptile_h) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 9 => (s V_putRGBseparate8bitMaptile_z
           + max0(-1 + s V_putRGBseparate8bitMaptile__tmp)
           + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 10 => (s V_putRGBseparate8bitMaptile_z
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 11 => (s V_putRGBseparate8bitMaptile_z
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 12 => (s V_putRGBseparate8bitMaptile_z
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 13 => (s V_putRGBseparate8bitMaptile_z
            + max0(s V_putRGBseparate8bitMaptile__tmp)
            + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 14 => (s V_putRGBseparate8bitMaptile_z
            + max0(s V_putRGBseparate8bitMaptile__tmp)
            + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_putRGBseparate8bitMaptile__tmp) (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_putRGBseparate8bitMaptile__tmp);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp2)) (F_check_ge (0) (0)))]
     (s V_putRGBseparate8bitMaptile_z
      + max0(s V_putRGBseparate8bitMaptile__tmp)
      + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 16 => (s V_putRGBseparate8bitMaptile_z <= z)%Q
   | 17 => hints
     [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (0) (0)));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (s V_putRGBseparate8bitMaptile__tmp) (0));
      (*0 0.5*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_putRGBseparate8bitMaptile__tmp) (0))) (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp))]
     (s V_putRGBseparate8bitMaptile_z
      + max0(s V_putRGBseparate8bitMaptile__tmp)
      + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 18 => ((3 # 2) * s V_putRGBseparate8bitMaptile__tmp
            - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp)
            - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp^2
            + s V_putRGBseparate8bitMaptile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp)
            + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
            + (1 # 2) * max0(s V_putRGBseparate8bitMaptile__tmp)^2 <= z)%Q
   | 19 => ((3 # 2) * s V_putRGBseparate8bitMaptile__tmp
            - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp)
            + s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp1)
            - s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp2)
            - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp^2
            + s V_putRGBseparate8bitMaptile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp)
            - max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp1)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
            + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
            + (1 # 2) * max0(s V_putRGBseparate8bitMaptile__tmp)^2 <= z)%Q
   | 20 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (s V_putRGBseparate8bitMaptile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (0) (0)))]
     ((3 # 2) * s V_putRGBseparate8bitMaptile__tmp
      - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp)
      + s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp1)
      - s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp2)
      - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp^2
      + s V_putRGBseparate8bitMaptile_z
      + (1 # 2) * max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp)
      - max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp1)
      + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
      + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
      + (1 # 2) * max0(s V_putRGBseparate8bitMaptile__tmp)^2 <= z)%Q
   | 21 => ((3 # 2) * s V_putRGBseparate8bitMaptile__tmp
            + s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp1)
            - s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp2)
            - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp^2
            + s V_putRGBseparate8bitMaptile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp)
            - max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp1)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
            + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 22 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_putRGBseparate8bitMaptile__tmp1) (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp1));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                     + s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp) (0));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp1)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (s V_putRGBseparate8bitMaptile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (s V_putRGBseparate8bitMaptile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp2)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_putRGBseparate8bitMaptile__tmp1)) (F_check_ge (0) (0))]
     ((3 # 2) * s V_putRGBseparate8bitMaptile__tmp
      + s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp1)
      - s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp2)
      - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp^2
      + s V_putRGBseparate8bitMaptile_z
      + (1 # 2) * max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp)
      - max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp1)
      + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
      + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 23 => ((1 # 1) + s V_putRGBseparate8bitMaptile_z
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 24 => ((1 # 1) + s V_putRGBseparate8bitMaptile_z
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 25 => ((1 # 1) + s V_putRGBseparate8bitMaptile_z
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_putRGBseparate8bitMaptile__tmp) (0))) (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp1)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_putRGBseparate8bitMaptile__tmp1) (0))) (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bitMaptile__tmp1)) (F_check_ge (s V_putRGBseparate8bitMaptile__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (0) (0)))]
     ((3 # 2) * s V_putRGBseparate8bitMaptile__tmp
      + s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp1)
      - s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp2)
      - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp^2
      + s V_putRGBseparate8bitMaptile_z
      + (1 # 2) * max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp)
      - max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp1)
      + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
      + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 27 => ((3 # 2) * s V_putRGBseparate8bitMaptile__tmp
            - s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp2)
            - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp^2
            - s V_putRGBseparate8bitMaptile__tmp1 * max0(-1
                                                         + s V_putRGBseparate8bitMaptile__tmp)
            + s V_putRGBseparate8bitMaptile__tmp1 * max0(s V_putRGBseparate8bitMaptile__tmp)
            + s V_putRGBseparate8bitMaptile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
            + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 28 => ((3 # 2) * s V_putRGBseparate8bitMaptile__tmp
            - s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp2)
            - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp^2
            - s V_putRGBseparate8bitMaptile__tmp1 * max0(-1
                                                         + s V_putRGBseparate8bitMaptile__tmp)
            + s V_putRGBseparate8bitMaptile__tmp1 * max0(s V_putRGBseparate8bitMaptile__tmp)
            + s V_putRGBseparate8bitMaptile_z
            + (1 # 2) * max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
            + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 29 => ((3 # 2) * s V_putRGBseparate8bitMaptile__tmp
            - s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp2)
            - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp^2
            - s V_putRGBseparate8bitMaptile__tmp1 * max0(-1
                                                         + s V_putRGBseparate8bitMaptile__tmp)
            + s V_putRGBseparate8bitMaptile__tmp1 * max0(s V_putRGBseparate8bitMaptile__tmp)
            + s V_putRGBseparate8bitMaptile_z
            - max0(-1 + s V_putRGBseparate8bitMaptile__tmp)
            + (1 # 2) * max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
            + max0(s V_putRGBseparate8bitMaptile__tmp)
            + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 30 => ((3 # 2) * s V_putRGBseparate8bitMaptile__tmp
            - s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp2)
            - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp^2
            - s V_putRGBseparate8bitMaptile__tmp1 * max0(-1
                                                         + s V_putRGBseparate8bitMaptile__tmp)
            + s V_putRGBseparate8bitMaptile__tmp1 * max0(s V_putRGBseparate8bitMaptile__tmp)
            + s V_putRGBseparate8bitMaptile_z
            - max0(-1 + s V_putRGBseparate8bitMaptile__tmp)
            + (1 # 2) * max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
            + max0(s V_putRGBseparate8bitMaptile__tmp)
            + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 31 => ((3 # 2) * s V_putRGBseparate8bitMaptile__tmp
            - s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp2)
            - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp^2
            - s V_putRGBseparate8bitMaptile__tmp1 * max0(-1
                                                         + s V_putRGBseparate8bitMaptile__tmp)
            + s V_putRGBseparate8bitMaptile__tmp1 * max0(s V_putRGBseparate8bitMaptile__tmp)
            + s V_putRGBseparate8bitMaptile_z
            - max0(-1 + s V_putRGBseparate8bitMaptile__tmp)
            + (1 # 2) * max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp)
            + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
            + max0(s V_putRGBseparate8bitMaptile__tmp)
            + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_putRGBseparate8bitMaptile__tmp) (1);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (s V_putRGBseparate8bitMaptile__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp1)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_putRGBseparate8bitMaptile__tmp1) (0))) (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp1))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_putRGBseparate8bitMaptile__tmp1)) (F_check_ge (s V_putRGBseparate8bitMaptile__tmp1) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_putRGBseparate8bitMaptile__tmp)) (F_check_ge (0) (0)))]
     (-(1 # 1) + (3 # 2) * s V_putRGBseparate8bitMaptile__tmp
      - s V_putRGBseparate8bitMaptile__tmp * max0(s V_putRGBseparate8bitMaptile__tmp2)
      - (1 # 2) * s V_putRGBseparate8bitMaptile__tmp^2
      - s V_putRGBseparate8bitMaptile__tmp1 * max0(-1
                                                   + s V_putRGBseparate8bitMaptile__tmp)
      + s V_putRGBseparate8bitMaptile__tmp1 * max0(s V_putRGBseparate8bitMaptile__tmp)
      + s V_putRGBseparate8bitMaptile_z
      - max0(-1 + s V_putRGBseparate8bitMaptile__tmp)
      + (1 # 2) * max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp)
      + max0(-1 + s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2)
      + max0(s V_putRGBseparate8bitMaptile__tmp)
      + max0(s V_putRGBseparate8bitMaptile__tmp) * max0(s V_putRGBseparate8bitMaptile__tmp2) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_putRGBseparate8bitMaptile =>
    [mkPA Q (fun n z s => ai_putRGBseparate8bitMaptile n s /\ annot0_putRGBseparate8bitMaptile n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_putRGBseparate8bitMaptile (proc_start P_putRGBseparate8bitMaptile) s1 (proc_end P_putRGBseparate8bitMaptile) s2 ->
    (s2 V_putRGBseparate8bitMaptile_z <= max0(-1
                                              + s1 V_putRGBseparate8bitMaptile_h)
                                         + max0(-1
                                                + s1 V_putRGBseparate8bitMaptile_h) * max0(s1 V_putRGBseparate8bitMaptile_w))%Q.
Proof.
  prove_bound ipa admissible_ipa P_putRGBseparate8bitMaptile.
Qed.
