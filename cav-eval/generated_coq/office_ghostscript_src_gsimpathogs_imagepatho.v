Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gs_imagepath.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gs_imagepath_z := 1%positive.
Notation V_gs_imagepath__tmp := 2%positive.
Notation V_gs_imagepath__tmp1 := 3%positive.
Notation V_gs_imagepath__tmp2 := 4%positive.
Notation V_gs_imagepath_code := 5%positive.
Notation V_gs_imagepath_x := 6%positive.
Notation V_gs_imagepath_y := 7%positive.
Notation V_gs_imagepath_data := 8%positive.
Notation V_gs_imagepath_height := 9%positive.
Notation V_gs_imagepath_pgs := 10%positive.
Notation V_gs_imagepath_width := 11%positive.
Definition Pedges_gs_imagepath: list (edge proc) :=
  (EA 1 (AAssign V_gs_imagepath_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_gs_imagepath__tmp1 (Some (EVar V_gs_imagepath_width))) 3)::(EA 3 (AAssign
  V_gs_imagepath__tmp2 (Some (EVar V_gs_imagepath_height))) 4)::
  (EA 4 (AAssign V_gs_imagepath_y (Some (ESub (EVar V_gs_imagepath__tmp2)
  (ENum (1))))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_gs_imagepath_y) s) >= (eval (ENum (0))
  s))%Z)) 12)::(EA 7 (AGuard (fun s => ((eval (EVar V_gs_imagepath_y) s) <
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 (AAssign
  V_gs_imagepath__tmp (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 54)::(EA 12 AWeaken 13)::(EA 13 (AAssign V_gs_imagepath_x
  (Some (ESub (EVar V_gs_imagepath__tmp1) (ENum (1))))) 14)::
  (EA 14 ANone 15)::(EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_gs_imagepath_x) s) >= (eval (ENum (0))
  s))%Z)) 24)::(EA 16 (AGuard (fun s => ((eval (EVar V_gs_imagepath_x) s) <
  (eval (ENum (0)) s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 18 ANone 19)::
  (EA 19 (AAssign V_gs_imagepath_y (Some (EAdd (EVar V_gs_imagepath_y)
  (ENum (-1))))) 20)::(EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_gs_imagepath_z (Some (EAdd (ENum (1)) (EVar V_gs_imagepath_z)))) 23)::
  (EA 23 AWeaken 7)::(EA 24 AWeaken 25)::(EA 25 ANone 26)::(EA 25 ANone 55)::
  (EA 26 AWeaken 27)::(EA 27 ANone 55)::(EA 27 ANone 28)::
  (EA 28 AWeaken 29)::(EA 29 ANone 31)::(EA 29 ANone 30)::
  (EA 30 AWeaken 34)::(EA 31 AWeaken 32)::(EA 32 ANone 33)::
  (EA 32 ANone 55)::(EA 33 AWeaken 34)::(EA 34 ANone 55)::(EA 34 ANone 35)::
  (EA 35 (AAssign V_gs_imagepath_code None) 36)::(EA 36 AWeaken 37)::
  (EA 37 ANone 51)::(EA 37 ANone 38)::(EA 38 (AAssign V_gs_imagepath_code
  None) 39)::(EA 39 AWeaken 40)::(EA 40 ANone 48)::(EA 40 ANone 41)::
  (EA 41 (AAssign V_gs_imagepath_code None) 42)::(EA 42 AWeaken 43)::
  (EA 43 ANone 45)::(EA 43 ANone 44)::(EA 44 ANone 55)::(EA 45 (AAssign
  V_gs_imagepath__tmp (Some (EVar V_gs_imagepath_code))) 46)::
  (EA 46 ANone 47)::(EA 47 AWeaken 54)::(EA 48 (AAssign V_gs_imagepath__tmp
  (Some (EVar V_gs_imagepath_code))) 49)::(EA 49 ANone 50)::
  (EA 50 AWeaken 54)::(EA 51 (AAssign V_gs_imagepath__tmp
  (Some (EVar V_gs_imagepath_code))) 52)::(EA 52 ANone 53)::
  (EA 53 AWeaken 54)::(EA 55 ANone 56)::(EA 56 (AAssign V_gs_imagepath_x
  (Some (EAdd (EVar V_gs_imagepath_x) (ENum (-1))))) 57)::(EA 57 ANone 58)::
  (EA 58 ANone 59)::(EA 59 (AAssign V_gs_imagepath_z (Some (EAdd (ENum (1))
  (EVar V_gs_imagepath_z)))) 60)::(EA 60 AWeaken 16)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gs_imagepath => Pedges_gs_imagepath
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gs_imagepath => 54
     end)%positive;
  var_global := var_global
}.

Definition ai_gs_imagepath (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_z <= 0)%Z
   | 3 => (-1 * s V_gs_imagepath_z <= 0 /\ 1 * s V_gs_imagepath_z <= 0)%Z
   | 4 => (1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_z <= 0)%Z
   | 5 => (-1 * s V_gs_imagepath_z <= 0 /\ 1 * s V_gs_imagepath_z <= 0)%Z
   | 6 => (1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_z <= 0)%Z
   | 7 => (-1 * s V_gs_imagepath_z <= 0)%Z
   | 8 => (-1 * s V_gs_imagepath_z <= 0 /\ 1 * s V_gs_imagepath_y + 1 <= 0)%Z
   | 9 => (1 * s V_gs_imagepath_y + 1 <= 0 /\ -1 * s V_gs_imagepath_z <= 0)%Z
   | 10 => (-1 * s V_gs_imagepath_z <= 0 /\ 1 * s V_gs_imagepath_y + 1 <= 0 /\ 1 * s V_gs_imagepath__tmp <= 0 /\ -1 * s V_gs_imagepath__tmp <= 0)%Z
   | 11 => (-1 * s V_gs_imagepath__tmp <= 0 /\ 1 * s V_gs_imagepath__tmp <= 0 /\ 1 * s V_gs_imagepath_y + 1 <= 0 /\ -1 * s V_gs_imagepath_z <= 0)%Z
   | 12 => (-1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 13 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0)%Z
   | 14 => (-1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 15 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0)%Z
   | 16 => (-1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 17 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ 1 * s V_gs_imagepath_x + 1 <= 0)%Z
   | 18 => (1 * s V_gs_imagepath_x + 1 <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 19 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ 1 * s V_gs_imagepath_x + 1 <= 0)%Z
   | 20 => (1 * s V_gs_imagepath_x + 1 <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y + -1 <= 0)%Z
   | 21 => (-1 * s V_gs_imagepath_y + -1 <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ 1 * s V_gs_imagepath_x + 1 <= 0)%Z
   | 22 => (1 * s V_gs_imagepath_x + 1 <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y + -1 <= 0)%Z
   | 23 => (-1 * s V_gs_imagepath_y + -1 <= 0 /\ 1 * s V_gs_imagepath_x + 1 <= 0 /\ -1 * s V_gs_imagepath_z + 1 <= 0)%Z
   | 24 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 25 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 26 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 27 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 28 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 29 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 30 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 31 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 32 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 33 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 34 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 35 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 36 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 37 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 38 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 39 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 40 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 41 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 42 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 43 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 44 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 45 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 46 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 47 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 48 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 49 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 50 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 51 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 52 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 53 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 54 => (-1 * s V_gs_imagepath_z <= 0)%Z
   | 55 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x <= 0)%Z
   | 56 => (-1 * s V_gs_imagepath_x <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 57 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x + -1 <= 0)%Z
   | 58 => (-1 * s V_gs_imagepath_x + -1 <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_y <= 0)%Z
   | 59 => (-1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z <= 0 /\ -1 * s V_gs_imagepath_x + -1 <= 0)%Z
   | 60 => (-1 * s V_gs_imagepath_x + -1 <= 0 /\ -1 * s V_gs_imagepath_y <= 0 /\ -1 * s V_gs_imagepath_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gs_imagepath (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_gs_imagepath_height)
           + max0(s V_gs_imagepath_height) * max0(s V_gs_imagepath_width) <= z)%Q
   | 2 => (s V_gs_imagepath_z + max0(s V_gs_imagepath_height)
           + max0(s V_gs_imagepath_height) * max0(s V_gs_imagepath_width) <= z)%Q
   | 3 => (s V_gs_imagepath_z
           + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_height)
           + max0(s V_gs_imagepath_height) <= z)%Q
   | 4 => (s V_gs_imagepath_z
           + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath__tmp2)
           + max0(s V_gs_imagepath__tmp2) <= z)%Q
   | 5 => (s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_y)
           + max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath__tmp1) <= z)%Q
   | 6 => (s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_y)
           + max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath__tmp1) <= z)%Q
   | 7 => (s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_y)
           + max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath__tmp1) <= z)%Q
   | 8 => (s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_y)
           + max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath__tmp1) <= z)%Q
   | 9 => (s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_y)
           + max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath__tmp1) <= z)%Q
   | 10 => (s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_y)
            + max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath__tmp1) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_gs_imagepath_y) (s V_gs_imagepath_y));
      (*-1 0*) F_max0_ge_0 (s V_gs_imagepath_y);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                            + s V_gs_imagepath_y)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath__tmp1)) (F_check_ge (0) (0)))]
     (s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_y)
      + max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath__tmp1) <= z)%Q
   | 12 => hints
     [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                               + s V_gs_imagepath_y)) (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_y) (0))) (F_max0_ge_0 (s V_gs_imagepath_y))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (1 + s V_gs_imagepath_y)) (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                 + s V_gs_imagepath_y) (0))) (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y))]
     (s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_y)
      + max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath__tmp1) <= z)%Q
   | 13 => ((1 # 1) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 2) * s V_gs_imagepath_y^2 + s V_gs_imagepath_z
            - (1 # 2) * max0(1 + s V_gs_imagepath_y)
            + max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath__tmp1)
            - (1 # 2) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            + (1 # 2) * max0(1 + s V_gs_imagepath_y)^2
            + (1 # 2) * max0(s V_gs_imagepath_y)
            + (1 # 2) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 14 => ((1 # 1) + (1 # 2) * s V_gs_imagepath_y
            - s V_gs_imagepath_y * max0(1 + s V_gs_imagepath_x)
            + s V_gs_imagepath_y * max0(s V_gs_imagepath__tmp1)
            - (1 # 2) * s V_gs_imagepath_y^2 + s V_gs_imagepath_z
            + max0(1 + s V_gs_imagepath_x) * max0(1 + s V_gs_imagepath_y)
            - (1 # 2) * max0(1 + s V_gs_imagepath_y)
            - (1 # 2) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            + (1 # 2) * max0(1 + s V_gs_imagepath_y)^2
            + (1 # 2) * max0(s V_gs_imagepath_y)
            + (1 # 2) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 15 => hints
     [(*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_gs_imagepath_y)) (F_check_ge (s V_gs_imagepath_y) (0));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                              + s V_gs_imagepath_y)) (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_x)) (F_check_ge (0) (0)));
      (*-0.75 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                 + s V_gs_imagepath_y)) (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*-0.75 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_y) (0))) (F_max0_ge_0 (s V_gs_imagepath_y))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y)) (F_check_ge (0) (0)))]
     ((1 # 1) + (1 # 2) * s V_gs_imagepath_y
      - s V_gs_imagepath_y * max0(1 + s V_gs_imagepath_x)
      + s V_gs_imagepath_y * max0(s V_gs_imagepath__tmp1)
      - (1 # 2) * s V_gs_imagepath_y^2 + s V_gs_imagepath_z
      + max0(1 + s V_gs_imagepath_x) * max0(1 + s V_gs_imagepath_y)
      - (1 # 2) * max0(1 + s V_gs_imagepath_y)
      - (1 # 2) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
      + (1 # 2) * max0(1 + s V_gs_imagepath_y)^2
      + (1 # 2) * max0(s V_gs_imagepath_y)
      + (1 # 2) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 16 => ((1 # 1) + s V_gs_imagepath_y * max0(s V_gs_imagepath__tmp1)
            + s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_x)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath_y) <= z)%Q
   | 17 => hints
     [(*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0))) (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_gs_imagepath_y)) (F_check_ge (s V_gs_imagepath_y) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y)) (F_check_ge (0) (0)))]
     ((1 # 1) + s V_gs_imagepath_y * max0(s V_gs_imagepath__tmp1)
      + s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_x)
      + (1 # 4) * max0(1 + s V_gs_imagepath_y)
      + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
      - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2 + max0(s V_gs_imagepath_y) <= z)%Q
   | 18 => ((1 # 1) + s V_gs_imagepath_y * max0(s V_gs_imagepath__tmp1)
            + s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_x)
            + max0(s V_gs_imagepath_y) <= z)%Q
   | 19 => ((1 # 1) + s V_gs_imagepath_y * max0(s V_gs_imagepath__tmp1)
            + s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_x)
            + max0(s V_gs_imagepath_y) <= z)%Q
   | 20 => ((1 # 1) + s V_gs_imagepath_y * max0(s V_gs_imagepath__tmp1)
            + s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_x)
            + max0(1 + s V_gs_imagepath_y) + max0(s V_gs_imagepath__tmp1) <= z)%Q
   | 21 => ((1 # 1) + s V_gs_imagepath_y * max0(s V_gs_imagepath__tmp1)
            + s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_x)
            + max0(1 + s V_gs_imagepath_y) + max0(s V_gs_imagepath__tmp1) <= z)%Q
   | 22 => ((1 # 1) + s V_gs_imagepath_y * max0(s V_gs_imagepath__tmp1)
            + s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_x)
            + max0(1 + s V_gs_imagepath_y) + max0(s V_gs_imagepath__tmp1) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0))) (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath__tmp1)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1 + s V_gs_imagepath_x)) (F_check_ge (0) (0))]
     (s V_gs_imagepath_y * max0(s V_gs_imagepath__tmp1) + s V_gs_imagepath_z
      + max0(1 + s V_gs_imagepath_x) + max0(1 + s V_gs_imagepath_y)
      + max0(s V_gs_imagepath__tmp1) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1 + s V_gs_imagepath_x) (1);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_y) (0))) (F_max0_ge_0 (s V_gs_imagepath_y))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath__tmp1)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_y) (0))) (F_max0_ge_0 (s V_gs_imagepath_y))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gs_imagepath_y)) (F_check_ge (s V_gs_imagepath_y) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                 + s V_gs_imagepath_y) (0))) (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y))]
     ((1 # 1) + s V_gs_imagepath_y * max0(s V_gs_imagepath__tmp1)
      + s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_x)
      + (1 # 4) * max0(1 + s V_gs_imagepath_y)
      + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
      - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2 + max0(s V_gs_imagepath_y) <= z)%Q
   | 25 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 26 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 27 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 28 => hints
     [(*0 0.333333*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                    + 
                                                                    s V_gs_imagepath_x) (0))) (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_x));
      (*0 0.333333*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_x) (0))) (F_max0_ge_0 (s V_gs_imagepath_x))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_x)) (F_check_ge (0) (0)));
      (*0 0.666667*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gs_imagepath_x)) (F_check_ge (s V_gs_imagepath_x) (0));
      (*0 0.75*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                 + s V_gs_imagepath_y) (0))) (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y))]
     ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
      - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
      + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
      + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
      - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
      + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
      + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 29 => ((3 # 2) + (1 # 3) * s V_gs_imagepath_x
            - (1 # 3) * s V_gs_imagepath_x * max0(1 + s V_gs_imagepath_x)
            - (1 # 3) * s V_gs_imagepath_x^2 - (1 # 4) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            - (3 # 4) * s V_gs_imagepath_y^2 + s V_gs_imagepath_z
            - (1 # 3) * max0(1 + s V_gs_imagepath_x)
            + (1 # 3) * max0(1 + s V_gs_imagepath_x) * max0(s V_gs_imagepath_x)
            + (1 # 3) * max0(1 + s V_gs_imagepath_x)^2
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            + (1 # 2) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + (1 # 3) * max0(s V_gs_imagepath_x)
            + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 30 => hints
     [(*-0.333333 0*) F_binom_monotonic 2 (F_max0_ge_arg (1
                                                          + s V_gs_imagepath_x)) (F_check_ge (1
                                                                    + s V_gs_imagepath_x) (0));
      (*-0.75 0*) F_binom_monotonic 2 (F_max0_ge_arg (1 + s V_gs_imagepath_y)) (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_gs_imagepath_x)) (F_check_ge (s V_gs_imagepath_x) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_x)) (F_check_ge (0) (0)));
      (*-0.666667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_x) (0))) (F_max0_ge_0 (s V_gs_imagepath_x))]
     ((3 # 2) + (1 # 3) * s V_gs_imagepath_x
      - (1 # 3) * s V_gs_imagepath_x * max0(1 + s V_gs_imagepath_x)
      - (1 # 3) * s V_gs_imagepath_x^2 - (1 # 4) * s V_gs_imagepath_y
      - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
      - (3 # 4) * s V_gs_imagepath_y^2 + s V_gs_imagepath_z
      - (1 # 3) * max0(1 + s V_gs_imagepath_x)
      + (1 # 3) * max0(1 + s V_gs_imagepath_x) * max0(s V_gs_imagepath_x)
      + (1 # 3) * max0(1 + s V_gs_imagepath_x)^2
      + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
      + (1 # 2) * max0(1 + s V_gs_imagepath_y)^2
      + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
      + (1 # 3) * max0(s V_gs_imagepath_x)
      + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 31 => hints
     [(*-0.75 0*) F_binom_monotonic 2 (F_max0_ge_arg (1 + s V_gs_imagepath_y)) (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0));
      (*-0.333333 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_gs_imagepath_x)) (F_check_ge (s V_gs_imagepath_x) (0));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + 
                                                                    s V_gs_imagepath_x)) (F_check_ge (1
                                                                    + s V_gs_imagepath_x) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_x)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    + 
                                                                    s V_gs_imagepath_x)) (F_check_ge (1
                                                                    + s V_gs_imagepath_x) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_x)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_x) (0))) (F_max0_ge_0 (s V_gs_imagepath_x))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_x)) (F_check_ge (0) (0)))]
     ((3 # 2) + (1 # 3) * s V_gs_imagepath_x
      - (1 # 3) * s V_gs_imagepath_x * max0(1 + s V_gs_imagepath_x)
      - (1 # 3) * s V_gs_imagepath_x^2 - (1 # 4) * s V_gs_imagepath_y
      - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
      - (3 # 4) * s V_gs_imagepath_y^2 + s V_gs_imagepath_z
      - (1 # 3) * max0(1 + s V_gs_imagepath_x)
      + (1 # 3) * max0(1 + s V_gs_imagepath_x) * max0(s V_gs_imagepath_x)
      + (1 # 3) * max0(1 + s V_gs_imagepath_x)^2
      + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
      + (1 # 2) * max0(1 + s V_gs_imagepath_y)^2
      + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
      + (1 # 3) * max0(s V_gs_imagepath_x)
      + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 32 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 33 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 34 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 35 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 36 => hints
     [(*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0))) (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_gs_imagepath_y)) (F_check_ge (s V_gs_imagepath_y) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_gs_imagepath_y)) (F_check_ge (s V_gs_imagepath_y) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_y) (0))) (F_max0_ge_0 (s V_gs_imagepath_y))]
     ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
      - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
      + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
      + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
      - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
      + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
      + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 37 => ((3 # 2) + s V_gs_imagepath_z
            + (1 # 2) * max0(1 + s V_gs_imagepath_y)
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 2) * max0(s V_gs_imagepath_y) <= z)%Q
   | 38 => ((3 # 2) + s V_gs_imagepath_z
            + (1 # 2) * max0(1 + s V_gs_imagepath_y)
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 2) * max0(s V_gs_imagepath_y) <= z)%Q
   | 39 => hints
     [(*0 0.5*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                + s V_gs_imagepath_x) (0))) (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_x));
      (*0 0.5*) F_binom_monotonic 2 (F_max0_ge_arg (s V_gs_imagepath_x)) (F_check_ge (s V_gs_imagepath_x) (0));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                + s V_gs_imagepath_x)) (F_check_ge (1
                                                                    + s V_gs_imagepath_x) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_x)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_x) (0))) (F_max0_ge_0 (s V_gs_imagepath_x))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_x)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_x) (0))) (F_max0_ge_0 (s V_gs_imagepath_x))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_x)) (F_check_ge (0) (0)));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gs_imagepath_x)) (F_check_ge (s V_gs_imagepath_x) (0))]
     ((3 # 2) + s V_gs_imagepath_z + (1 # 2) * max0(1 + s V_gs_imagepath_y)
      + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
      + max0(s V_gs_imagepath_x) + (1 # 2) * max0(s V_gs_imagepath_y) <= z)%Q
   | 40 => ((3 # 2)
            - (1 # 2) * s V_gs_imagepath_x * max0(1 + s V_gs_imagepath_x)
            + s V_gs_imagepath_z - (1 # 2) * max0(1 + s V_gs_imagepath_x)
            + (1 # 2) * max0(1 + s V_gs_imagepath_x)^2
            + (1 # 2) * max0(1 + s V_gs_imagepath_y)
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 2) * max0(s V_gs_imagepath_y) <= z)%Q
   | 41 => ((3 # 2)
            - (1 # 2) * s V_gs_imagepath_x * max0(1 + s V_gs_imagepath_x)
            + s V_gs_imagepath_z - (1 # 2) * max0(1 + s V_gs_imagepath_x)
            + (1 # 2) * max0(1 + s V_gs_imagepath_x)^2
            + (1 # 2) * max0(1 + s V_gs_imagepath_y)
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 2) * max0(s V_gs_imagepath_y) <= z)%Q
   | 42 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                + s V_gs_imagepath_x)) (F_check_ge (1
                                                                    + s V_gs_imagepath_x) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_x)) (F_check_ge (0) (0)));
      (*0 0.25*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                + s V_gs_imagepath_y)) (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_y) (0))) (F_max0_ge_0 (s V_gs_imagepath_y))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_y) (0))) (F_max0_ge_0 (s V_gs_imagepath_y))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gs_imagepath_y)) (F_check_ge (s V_gs_imagepath_y) (0))]
     ((3 # 2) - (1 # 2) * s V_gs_imagepath_x * max0(1 + s V_gs_imagepath_x)
      + s V_gs_imagepath_z - (1 # 2) * max0(1 + s V_gs_imagepath_x)
      + (1 # 2) * max0(1 + s V_gs_imagepath_x)^2
      + (1 # 2) * max0(1 + s V_gs_imagepath_y)
      + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
      + max0(s V_gs_imagepath_x) + (1 # 2) * max0(s V_gs_imagepath_y) <= z)%Q
   | 43 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 44 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 45 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 46 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 47 => hints
     [(*-2 0*) F_one;
      (*-0.5 0*) F_max0_pre_decrement 1 (1 + s V_gs_imagepath_y) (1);
      (*-1 0*) F_max0_ge_0 (s V_gs_imagepath_x);
      (*-1 0*) F_max0_ge_0 (s V_gs_imagepath_y);
      (*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                  + s V_gs_imagepath_y) (0))) (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y));
      (*-0.25 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_gs_imagepath_y)) (F_check_ge (s V_gs_imagepath_y) (0));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                 + s V_gs_imagepath_y)) (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath__tmp1)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_y)) (F_check_ge (0) (0)))]
     ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
      - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
      + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
      + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
      - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
      + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
      + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 48 => ((3 # 2)
            - (1 # 2) * s V_gs_imagepath_x * max0(1 + s V_gs_imagepath_x)
            + s V_gs_imagepath_z - (1 # 2) * max0(1 + s V_gs_imagepath_x)
            + (1 # 2) * max0(1 + s V_gs_imagepath_x)^2
            + (1 # 2) * max0(1 + s V_gs_imagepath_y)
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 2) * max0(s V_gs_imagepath_y) <= z)%Q
   | 49 => ((3 # 2)
            - (1 # 2) * s V_gs_imagepath_x * max0(1 + s V_gs_imagepath_x)
            + s V_gs_imagepath_z - (1 # 2) * max0(1 + s V_gs_imagepath_x)
            + (1 # 2) * max0(1 + s V_gs_imagepath_x)^2
            + (1 # 2) * max0(1 + s V_gs_imagepath_y)
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 2) * max0(s V_gs_imagepath_y) <= z)%Q
   | 50 => hints
     [(*-2 0*) F_one;
      (*-0.5 0*) F_max0_pre_decrement 1 (1 + s V_gs_imagepath_y) (1);
      (*-1 0*) F_max0_ge_0 (s V_gs_imagepath_x);
      (*-1 0*) F_max0_ge_0 (s V_gs_imagepath_y);
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                + s V_gs_imagepath_x)) (F_check_ge (1
                                                                    + s V_gs_imagepath_x) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_x)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath__tmp1)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_y)) (F_check_ge (0) (0)))]
     ((3 # 2) - (1 # 2) * s V_gs_imagepath_x * max0(1 + s V_gs_imagepath_x)
      + s V_gs_imagepath_z - (1 # 2) * max0(1 + s V_gs_imagepath_x)
      + (1 # 2) * max0(1 + s V_gs_imagepath_x)^2
      + (1 # 2) * max0(1 + s V_gs_imagepath_y)
      + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
      + max0(s V_gs_imagepath_x) + (1 # 2) * max0(s V_gs_imagepath_y) <= z)%Q
   | 51 => ((3 # 2) + s V_gs_imagepath_z
            + (1 # 2) * max0(1 + s V_gs_imagepath_y)
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 2) * max0(s V_gs_imagepath_y) <= z)%Q
   | 52 => ((3 # 2) + s V_gs_imagepath_z
            + (1 # 2) * max0(1 + s V_gs_imagepath_y)
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 2) * max0(s V_gs_imagepath_y) <= z)%Q
   | 53 => hints
     [(*-2 0*) F_one;
      (*-0.5 0*) F_max0_pre_decrement 1 (1 + s V_gs_imagepath_y) (1);
      (*-1 0*) F_max0_ge_0 (s V_gs_imagepath_x);
      (*-1 0*) F_max0_ge_0 (s V_gs_imagepath_y);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath__tmp1)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_y)) (F_check_ge (0) (0)))]
     ((3 # 2) + s V_gs_imagepath_z + (1 # 2) * max0(1 + s V_gs_imagepath_y)
      + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
      + max0(s V_gs_imagepath_x) + (1 # 2) * max0(s V_gs_imagepath_y) <= z)%Q
   | 54 => (s V_gs_imagepath_z <= z)%Q
   | 55 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 56 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + max0(s V_gs_imagepath_x) + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 57 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_x)
            + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 58 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_x)
            + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 59 => ((3 # 2) + (1 # 2) * s V_gs_imagepath_y
            - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
            + s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_x)
            + (3 # 4) * max0(1 + s V_gs_imagepath_y)
            + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
            - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
            + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
            + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | 60 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (1 + s V_gs_imagepath_y) (1);
      (*0.25 0.5*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                   + 
                                                                   s V_gs_imagepath_y) (0))) (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y));
      (*-0.25 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_gs_imagepath_y)) (F_check_ge (s V_gs_imagepath_y) (0));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                 + s V_gs_imagepath_y)) (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                 + s V_gs_imagepath_y)) (F_check_ge (1
                                                                    + s V_gs_imagepath_y) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*-0.5 -0.25*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_imagepath_y) (0))) (F_max0_ge_0 (s V_gs_imagepath_y))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_gs_imagepath_y)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_gs_imagepath_y)) (F_check_ge (s V_gs_imagepath_y) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_imagepath__tmp1)) (F_check_ge (0) (0)))]
     ((1 # 2) + (1 # 2) * s V_gs_imagepath_y
      - (1 # 4) * s V_gs_imagepath_y * max0(s V_gs_imagepath_y)
      + s V_gs_imagepath_z + max0(1 + s V_gs_imagepath_x)
      + (3 # 4) * max0(1 + s V_gs_imagepath_y)
      + (1 # 4) * max0(1 + s V_gs_imagepath_y) * max0(s V_gs_imagepath_y)
      - (1 # 4) * max0(1 + s V_gs_imagepath_y)^2
      + max0(s V_gs_imagepath__tmp1) * max0(s V_gs_imagepath_y)
      + (1 # 4) * max0(s V_gs_imagepath_y)^2 <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gs_imagepath =>
    [mkPA Q (fun n z s => ai_gs_imagepath n s /\ annot0_gs_imagepath n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gs_imagepath (proc_start P_gs_imagepath) s1 (proc_end P_gs_imagepath) s2 ->
    (s2 V_gs_imagepath_z <= max0(s1 V_gs_imagepath_height)
                            + max0(s1 V_gs_imagepath_height) * max0(s1 V_gs_imagepath_width))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gs_imagepath.
Qed.
