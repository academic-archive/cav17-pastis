Require Import pasta.Pasta.

Inductive proc: Type :=
  P_jpeg_idct_4x4.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_jpeg_idct_4x4_z := 1%positive.
Notation V_jpeg_idct_4x4__tmp := 2%positive.
Notation V_jpeg_idct_4x4_ctr := 3%positive.
Notation V_jpeg_idct_4x4_dcval := 4%positive.
Notation V_jpeg_idct_4x4_dcval1 := 5%positive.
Notation V_jpeg_idct_4x4_tmp0 := 6%positive.
Notation V_jpeg_idct_4x4_tmp10 := 7%positive.
Notation V_jpeg_idct_4x4_tmp12 := 8%positive.
Notation V_jpeg_idct_4x4_tmp2 := 9%positive.
Notation V_jpeg_idct_4x4_z1 := 10%positive.
Notation V_jpeg_idct_4x4_z2 := 11%positive.
Notation V_jpeg_idct_4x4_z3 := 12%positive.
Notation V_jpeg_idct_4x4_z4 := 13%positive.
Notation V_jpeg_idct_4x4_cinfo := 14%positive.
Notation V_jpeg_idct_4x4_coef_block := 15%positive.
Notation V_jpeg_idct_4x4_compptr := 16%positive.
Notation V_jpeg_idct_4x4_output_buf := 17%positive.
Notation V_jpeg_idct_4x4_output_col := 18%positive.
Definition Pedges_jpeg_idct_4x4: list (edge proc) :=
  (EA 1 (AAssign V_jpeg_idct_4x4_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_jpeg_idct_4x4__tmp (Some (EVar V_jpeg_idct_4x4_output_col))) 3)::
  (EA 3 (AAssign V_jpeg_idct_4x4_ctr (Some (ENum (8)))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard (fun s => ((eval (EVar V_jpeg_idct_4x4_ctr)
  s) > (eval (ENum (0)) s))%Z)) 34)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_jpeg_idct_4x4_ctr) s) <= (eval (ENum (0))
  s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign V_jpeg_idct_4x4_ctr
  (Some (ENum (0)))) 9)::(EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_jpeg_idct_4x4_ctr) s) < (eval (ENum (4))
  s))%Z)) 14)::(EA 11 (AGuard (fun s => ((eval (EVar V_jpeg_idct_4x4_ctr)
  s) >= (eval (ENum (4)) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 14 AWeaken 15)::(EA 15 ANone 27)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_jpeg_idct_4x4_tmp0 None) 17)::(EA 17 (AAssign V_jpeg_idct_4x4_tmp2
  None) 18)::(EA 18 (AAssign V_jpeg_idct_4x4_tmp10
  (Some (EAdd (EVar V_jpeg_idct_4x4_tmp0)
  (EVar V_jpeg_idct_4x4_tmp2)))) 19)::(EA 19 (AAssign V_jpeg_idct_4x4_tmp12
  (Some (ESub (EVar V_jpeg_idct_4x4_tmp0)
  (EVar V_jpeg_idct_4x4_tmp2)))) 20)::(EA 20 (AAssign V_jpeg_idct_4x4_z1
  None) 21)::(EA 21 (AAssign V_jpeg_idct_4x4_z2 None) 22)::(EA 22 (AAssign
  V_jpeg_idct_4x4_z3 None) 23)::(EA 23 (AAssign V_jpeg_idct_4x4_z4
  None) 24)::(EA 24 (AAssign V_jpeg_idct_4x4_tmp0 None) 25)::(EA 25 (AAssign
  V_jpeg_idct_4x4_tmp2 None) 26)::(EA 26 ANone 29)::(EA 27 (AAssign
  V_jpeg_idct_4x4_dcval1 None) 28)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_jpeg_idct_4x4_ctr (Some (EAdd (EVar V_jpeg_idct_4x4_ctr)
  (ENum (1))))) 30)::(EA 30 ANone 31)::(EA 31 ANone 32)::(EA 32 (AAssign
  V_jpeg_idct_4x4_z (Some (EAdd (ENum (1)) (EVar V_jpeg_idct_4x4_z)))) 33)::
  (EA 33 AWeaken 11)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_jpeg_idct_4x4_ctr) s) = (eval (ENum (4))
  s))%Z)) 54)::(EA 35 (AGuard (fun s => ((eval (EVar V_jpeg_idct_4x4_ctr)
  s) <> (eval (ENum (4)) s))%Z)) 36)::(EA 36 AWeaken 37)::(EA 37 ANone 52)::
  (EA 37 ANone 38)::(EA 38 (AAssign V_jpeg_idct_4x4_tmp0 None) 39)::
  (EA 39 (AAssign V_jpeg_idct_4x4_tmp0 None) 40)::(EA 40 (AAssign
  V_jpeg_idct_4x4_z2 None) 41)::(EA 41 (AAssign V_jpeg_idct_4x4_z3
  None) 42)::(EA 42 (AAssign V_jpeg_idct_4x4_tmp2 None) 43)::(EA 43 (AAssign
  V_jpeg_idct_4x4_tmp10 (Some (EAdd (EVar V_jpeg_idct_4x4_tmp0)
  (EVar V_jpeg_idct_4x4_tmp2)))) 44)::(EA 44 (AAssign V_jpeg_idct_4x4_tmp12
  (Some (ESub (EVar V_jpeg_idct_4x4_tmp0)
  (EVar V_jpeg_idct_4x4_tmp2)))) 45)::(EA 45 (AAssign V_jpeg_idct_4x4_z1
  None) 46)::(EA 46 (AAssign V_jpeg_idct_4x4_z2 None) 47)::(EA 47 (AAssign
  V_jpeg_idct_4x4_z3 None) 48)::(EA 48 (AAssign V_jpeg_idct_4x4_z4
  None) 49)::(EA 49 (AAssign V_jpeg_idct_4x4_tmp0 None) 50)::(EA 50 (AAssign
  V_jpeg_idct_4x4_tmp2 None) 51)::(EA 51 ANone 56)::(EA 52 (AAssign
  V_jpeg_idct_4x4_dcval None) 53)::(EA 53 ANone 56)::(EA 54 AWeaken 55)::
  (EA 55 ANone 56)::(EA 56 (AAssign V_jpeg_idct_4x4_ctr
  (Some (EAdd (EVar V_jpeg_idct_4x4_ctr) (ENum (-1))))) 57)::
  (EA 57 ANone 58)::(EA 58 ANone 59)::(EA 59 (AAssign V_jpeg_idct_4x4_z
  (Some (EAdd (ENum (1)) (EVar V_jpeg_idct_4x4_z)))) 60)::(EA 60 AWeaken 6)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_jpeg_idct_4x4 => Pedges_jpeg_idct_4x4
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_jpeg_idct_4x4 => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_jpeg_idct_4x4 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 3 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 4 => (1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 8 <= 0)%Z
   | 5 => (-1 * s V_jpeg_idct_4x4_ctr + 8 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 6 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0)%Z
   | 7 => (-1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr <= 0)%Z
   | 8 => (1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0)%Z
   | 9 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0)%Z
   | 10 => (-1 * s V_jpeg_idct_4x4_ctr <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 11 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -4 <= 0)%Z
   | 12 => (1 * s V_jpeg_idct_4x4_ctr + -4 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 4 <= 0)%Z
   | 13 => (-1 * s V_jpeg_idct_4x4_ctr + 4 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -4 <= 0)%Z
   | 14 => (-1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -3 <= 0)%Z
   | 15 => (1 * s V_jpeg_idct_4x4_ctr + -3 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0)%Z
   | 16 => (-1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -3 <= 0)%Z
   | 17 => (1 * s V_jpeg_idct_4x4_ctr + -3 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0)%Z
   | 18 => (-1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -3 <= 0)%Z
   | 19 => (1 * s V_jpeg_idct_4x4_ctr + -3 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0)%Z
   | 20 => (-1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -3 <= 0)%Z
   | 21 => (1 * s V_jpeg_idct_4x4_ctr + -3 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0)%Z
   | 22 => (-1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -3 <= 0)%Z
   | 23 => (1 * s V_jpeg_idct_4x4_ctr + -3 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0)%Z
   | 24 => (-1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -3 <= 0)%Z
   | 25 => (1 * s V_jpeg_idct_4x4_ctr + -3 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0)%Z
   | 26 => (-1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -3 <= 0)%Z
   | 27 => (-1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -3 <= 0)%Z
   | 28 => (1 * s V_jpeg_idct_4x4_ctr + -3 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0)%Z
   | 29 => (-1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -3 <= 0)%Z
   | 30 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -4 <= 0)%Z
   | 31 => (1 * s V_jpeg_idct_4x4_ctr + -4 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 32 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -4 <= 0)%Z
   | 33 => (1 * s V_jpeg_idct_4x4_ctr + -4 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_4x4_z + 1 <= 0)%Z
   | 34 => (1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0)%Z
   | 35 => (-1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0)%Z
   | 36 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0)%Z
   | 37 => (1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 38 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0)%Z
   | 39 => (1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 40 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0)%Z
   | 41 => (1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 42 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0)%Z
   | 43 => (1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 44 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0)%Z
   | 45 => (1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 46 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0)%Z
   | 47 => (1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 48 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0)%Z
   | 49 => (1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 50 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0)%Z
   | 51 => (1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 52 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0)%Z
   | 53 => (1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 54 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -4 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr + 4 <= 0)%Z
   | 55 => (-1 * s V_jpeg_idct_4x4_ctr + 4 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -4 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 56 => (-1 * s V_jpeg_idct_4x4_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 57 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -7 <= 0)%Z
   | 58 => (1 * s V_jpeg_idct_4x4_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z <= 0)%Z
   | 59 => (-1 * s V_jpeg_idct_4x4_z <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0 /\ 1 * s V_jpeg_idct_4x4_ctr + -7 <= 0)%Z
   | 60 => (1 * s V_jpeg_idct_4x4_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_4x4_ctr <= 0 /\ -1 * s V_jpeg_idct_4x4_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_jpeg_idct_4x4 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((12 # 1) <= z)%Q
   | 2 => ((12 # 1) + s V_jpeg_idct_4x4_z <= z)%Q
   | 3 => ((12 # 1) + s V_jpeg_idct_4x4_z <= z)%Q
   | 4 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 5 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 6 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_jpeg_idct_4x4_ctr) (-1
                                                                    + 
                                                                    s V_jpeg_idct_4x4_ctr));
      (*-1 0*) F_max0_ge_0 (-1 + s V_jpeg_idct_4x4_ctr);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_jpeg_idct_4x4_ctr) (0))) (F_max0_ge_0 (s V_jpeg_idct_4x4_ctr))]
     ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 8 => ((4 # 1) + s V_jpeg_idct_4x4_z <= z)%Q
   | 9 => (s V_jpeg_idct_4x4_z + max0(4 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 10 => (s V_jpeg_idct_4x4_z + max0(4 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 11 => (s V_jpeg_idct_4x4_z + max0(4 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_ge_0 (4 - s V_jpeg_idct_4x4_ctr)]
     (s V_jpeg_idct_4x4_z + max0(4 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 13 => (s V_jpeg_idct_4x4_z <= z)%Q
   | 14 => hints
     [(*0 1*) F_max0_pre_decrement 1 (4 - s V_jpeg_idct_4x4_ctr) (1)]
     (s V_jpeg_idct_4x4_z + max0(4 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 15 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 16 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 17 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 18 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 19 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 20 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 21 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 22 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 23 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 24 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 25 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 26 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 27 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 28 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 29 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(3 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 30 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(4 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 31 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(4 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 32 => ((1 # 1) + s V_jpeg_idct_4x4_z + max0(4 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 33 => (s V_jpeg_idct_4x4_z + max0(4 - s V_jpeg_idct_4x4_ctr) <= z)%Q
   | 34 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 35 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 36 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 37 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 38 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 39 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 40 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 41 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 42 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 43 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 44 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 45 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 46 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 47 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 48 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 49 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 50 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 51 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 52 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 53 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 54 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 55 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 56 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 57 => ((5 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 58 => ((5 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 59 => ((5 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | 60 => ((4 # 1) + s V_jpeg_idct_4x4_ctr + s V_jpeg_idct_4x4_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_jpeg_idct_4x4 =>
    [mkPA Q (fun n z s => ai_jpeg_idct_4x4 n s /\ annot0_jpeg_idct_4x4 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_jpeg_idct_4x4 (proc_start P_jpeg_idct_4x4) s1 (proc_end P_jpeg_idct_4x4) s2 ->
    (s2 V_jpeg_idct_4x4_z <= (12 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_jpeg_idct_4x4.
Qed.
