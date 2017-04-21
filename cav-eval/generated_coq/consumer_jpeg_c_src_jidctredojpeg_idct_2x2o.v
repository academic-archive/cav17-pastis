Require Import pasta.Pasta.

Inductive proc: Type :=
  P_jpeg_idct_2x2.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_jpeg_idct_2x2_z := 1%positive.
Notation V_jpeg_idct_2x2__tmp := 2%positive.
Notation V_jpeg_idct_2x2_ctr := 3%positive.
Notation V_jpeg_idct_2x2_dcval := 4%positive.
Notation V_jpeg_idct_2x2_dcval1 := 5%positive.
Notation V_jpeg_idct_2x2_tmp0 := 6%positive.
Notation V_jpeg_idct_2x2_tmp10 := 7%positive.
Notation V_jpeg_idct_2x2_z1 := 8%positive.
Notation V_jpeg_idct_2x2_cinfo := 9%positive.
Notation V_jpeg_idct_2x2_coef_block := 10%positive.
Notation V_jpeg_idct_2x2_compptr := 11%positive.
Notation V_jpeg_idct_2x2_output_buf := 12%positive.
Notation V_jpeg_idct_2x2_output_col := 13%positive.
Definition Pedges_jpeg_idct_2x2: list (edge proc) :=
  (EA 1 (AAssign V_jpeg_idct_2x2_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_jpeg_idct_2x2__tmp (Some (EVar V_jpeg_idct_2x2_output_col))) 3)::
  (EA 3 (AAssign V_jpeg_idct_2x2_ctr (Some (ENum (8)))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard (fun s => ((eval (EVar V_jpeg_idct_2x2_ctr)
  s) > (eval (ENum (0)) s))%Z)) 26)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_jpeg_idct_2x2_ctr) s) <= (eval (ENum (0))
  s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign V_jpeg_idct_2x2_ctr
  (Some (ENum (0)))) 9)::(EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_jpeg_idct_2x2_ctr) s) < (eval (ENum (2))
  s))%Z)) 14)::(EA 11 (AGuard (fun s => ((eval (EVar V_jpeg_idct_2x2_ctr)
  s) >= (eval (ENum (2)) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 14 AWeaken 15)::(EA 15 ANone 19)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_jpeg_idct_2x2_tmp10 None) 17)::(EA 17 (AAssign V_jpeg_idct_2x2_tmp0
  None) 18)::(EA 18 ANone 21)::(EA 19 (AAssign V_jpeg_idct_2x2_dcval1
  None) 20)::(EA 20 ANone 21)::(EA 21 (AAssign V_jpeg_idct_2x2_ctr
  (Some (EAdd (EVar V_jpeg_idct_2x2_ctr) (ENum (1))))) 22)::
  (EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 (AAssign V_jpeg_idct_2x2_z
  (Some (EAdd (ENum (1)) (EVar V_jpeg_idct_2x2_z)))) 25)::
  (EA 25 AWeaken 11)::(EA 26 AWeaken 27)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_jpeg_idct_2x2_ctr) s) = (eval (ENum (6))
  s))%Z)) 49)::(EA 27 (AGuard (fun s => ((eval (EVar V_jpeg_idct_2x2_ctr)
  s) <> (eval (ENum (6)) s))%Z)) 28)::(EA 28 AWeaken 29)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_jpeg_idct_2x2_ctr) s) = (eval (ENum (4))
  s))%Z)) 48)::(EA 29 (AGuard (fun s => ((eval (EVar V_jpeg_idct_2x2_ctr)
  s) <> (eval (ENum (4)) s))%Z)) 30)::(EA 30 AWeaken 31)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_jpeg_idct_2x2_ctr) s) = (eval (ENum (2))
  s))%Z)) 47)::(EA 31 (AGuard (fun s => ((eval (EVar V_jpeg_idct_2x2_ctr)
  s) <> (eval (ENum (2)) s))%Z)) 32)::(EA 32 AWeaken 33)::(EA 33 ANone 45)::
  (EA 33 ANone 34)::(EA 34 (AAssign V_jpeg_idct_2x2_z1 None) 35)::
  (EA 35 (AAssign V_jpeg_idct_2x2_tmp10 None) 36)::(EA 36 (AAssign
  V_jpeg_idct_2x2_z1 None) 37)::(EA 37 (AAssign V_jpeg_idct_2x2_tmp0
  (Some (EMul (EVar V_jpeg_idct_2x2_z1) (ENum (-5906))))) 38)::
  (EA 38 (AAssign V_jpeg_idct_2x2_z1 None) 39)::(EA 39 (AAssign
  V_jpeg_idct_2x2_tmp0 (Some (EAdd (EVar V_jpeg_idct_2x2_tmp0)
  (EMul (EVar V_jpeg_idct_2x2_z1) (ENum (6967)))))) 40)::(EA 40 (AAssign
  V_jpeg_idct_2x2_z1 None) 41)::(EA 41 (AAssign V_jpeg_idct_2x2_tmp0
  None) 42)::(EA 42 (AAssign V_jpeg_idct_2x2_z1 None) 43)::(EA 43 (AAssign
  V_jpeg_idct_2x2_tmp0 None) 44)::(EA 44 ANone 51)::(EA 45 (AAssign
  V_jpeg_idct_2x2_dcval None) 46)::(EA 46 ANone 51)::(EA 47 AWeaken 50)::
  (EA 48 AWeaken 50)::(EA 49 AWeaken 50)::(EA 50 ANone 51)::(EA 51 (AAssign
  V_jpeg_idct_2x2_ctr (Some (EAdd (EVar V_jpeg_idct_2x2_ctr)
  (ENum (-1))))) 52)::(EA 52 ANone 53)::(EA 53 ANone 54)::(EA 54 (AAssign
  V_jpeg_idct_2x2_z (Some (EAdd (ENum (1)) (EVar V_jpeg_idct_2x2_z)))) 55)::
  (EA 55 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_jpeg_idct_2x2 => Pedges_jpeg_idct_2x2
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_jpeg_idct_2x2 => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_jpeg_idct_2x2 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 3 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 4 => (1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 8 <= 0)%Z
   | 5 => (-1 * s V_jpeg_idct_2x2_ctr + 8 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 6 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr <= 0)%Z
   | 7 => (-1 * s V_jpeg_idct_2x2_ctr <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr <= 0)%Z
   | 8 => (1 * s V_jpeg_idct_2x2_ctr <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr <= 0)%Z
   | 9 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr <= 0)%Z
   | 10 => (-1 * s V_jpeg_idct_2x2_ctr <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 11 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -2 <= 0)%Z
   | 12 => (1 * s V_jpeg_idct_2x2_ctr + -2 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 2 <= 0)%Z
   | 13 => (-1 * s V_jpeg_idct_2x2_ctr + 2 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -2 <= 0)%Z
   | 14 => (-1 * s V_jpeg_idct_2x2_ctr <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -1 <= 0)%Z
   | 15 => (1 * s V_jpeg_idct_2x2_ctr + -1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr <= 0)%Z
   | 16 => (-1 * s V_jpeg_idct_2x2_ctr <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -1 <= 0)%Z
   | 17 => (1 * s V_jpeg_idct_2x2_ctr + -1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr <= 0)%Z
   | 18 => (-1 * s V_jpeg_idct_2x2_ctr <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -1 <= 0)%Z
   | 19 => (-1 * s V_jpeg_idct_2x2_ctr <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -1 <= 0)%Z
   | 20 => (1 * s V_jpeg_idct_2x2_ctr + -1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr <= 0)%Z
   | 21 => (-1 * s V_jpeg_idct_2x2_ctr <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -1 <= 0)%Z
   | 22 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -2 <= 0)%Z
   | 23 => (1 * s V_jpeg_idct_2x2_ctr + -2 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 24 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -2 <= 0)%Z
   | 25 => (1 * s V_jpeg_idct_2x2_ctr + -2 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z + 1 <= 0)%Z
   | 26 => (1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0)%Z
   | 27 => (-1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0)%Z
   | 28 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0)%Z
   | 29 => (1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 30 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0)%Z
   | 31 => (1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 32 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0)%Z
   | 33 => (1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 34 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0)%Z
   | 35 => (1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 36 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0)%Z
   | 37 => (1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 38 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0)%Z
   | 39 => (1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 40 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0)%Z
   | 41 => (1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 42 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0)%Z
   | 43 => (1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 44 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0)%Z
   | 45 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0)%Z
   | 46 => (1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 47 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -2 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 2 <= 0)%Z
   | 48 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -4 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 4 <= 0)%Z
   | 49 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -6 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr + 6 <= 0)%Z
   | 50 => (-1 * s V_jpeg_idct_2x2_ctr + 2 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -6 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 51 => (-1 * s V_jpeg_idct_2x2_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 52 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -7 <= 0)%Z
   | 53 => (1 * s V_jpeg_idct_2x2_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr <= 0 /\ -1 * s V_jpeg_idct_2x2_z <= 0)%Z
   | 54 => (-1 * s V_jpeg_idct_2x2_z <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr <= 0 /\ 1 * s V_jpeg_idct_2x2_ctr + -7 <= 0)%Z
   | 55 => (1 * s V_jpeg_idct_2x2_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_2x2_ctr <= 0 /\ -1 * s V_jpeg_idct_2x2_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_jpeg_idct_2x2 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((10 # 1) <= z)%Q
   | 2 => ((10 # 1) + s V_jpeg_idct_2x2_z <= z)%Q
   | 3 => ((10 # 1) + s V_jpeg_idct_2x2_z <= z)%Q
   | 4 => ((2 # 1) + s V_jpeg_idct_2x2_z + max0(s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 5 => ((2 # 1) + s V_jpeg_idct_2x2_z + max0(s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 6 => ((2 # 1) + s V_jpeg_idct_2x2_z + max0(s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_ge_0 (s V_jpeg_idct_2x2_ctr)]
     ((2 # 1) + s V_jpeg_idct_2x2_z + max0(s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 8 => ((2 # 1) + s V_jpeg_idct_2x2_z <= z)%Q
   | 9 => (s V_jpeg_idct_2x2_z + max0(2 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 10 => (s V_jpeg_idct_2x2_z + max0(2 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 11 => (s V_jpeg_idct_2x2_z + max0(2 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (2 - s V_jpeg_idct_2x2_ctr) (1
                                                                    - s V_jpeg_idct_2x2_ctr));
      (*-1 0*) F_max0_ge_0 (1 - s V_jpeg_idct_2x2_ctr)]
     (s V_jpeg_idct_2x2_z + max0(2 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 13 => (s V_jpeg_idct_2x2_z <= z)%Q
   | 14 => hints
     [(*0 1*) F_max0_pre_decrement 1 (2 - s V_jpeg_idct_2x2_ctr) (1)]
     (s V_jpeg_idct_2x2_z + max0(2 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 15 => ((1 # 1) + s V_jpeg_idct_2x2_z + max0(1 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 16 => ((1 # 1) + s V_jpeg_idct_2x2_z + max0(1 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 17 => ((1 # 1) + s V_jpeg_idct_2x2_z + max0(1 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 18 => ((1 # 1) + s V_jpeg_idct_2x2_z + max0(1 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 19 => ((1 # 1) + s V_jpeg_idct_2x2_z + max0(1 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 20 => ((1 # 1) + s V_jpeg_idct_2x2_z + max0(1 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 21 => ((1 # 1) + s V_jpeg_idct_2x2_z + max0(1 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 22 => ((1 # 1) + s V_jpeg_idct_2x2_z + max0(2 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 23 => ((1 # 1) + s V_jpeg_idct_2x2_z + max0(2 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 24 => ((1 # 1) + s V_jpeg_idct_2x2_z + max0(2 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 25 => (s V_jpeg_idct_2x2_z + max0(2 - s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 26 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_jpeg_idct_2x2_ctr) (1)]
     ((2 # 1) + s V_jpeg_idct_2x2_z + max0(s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 27 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 28 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 29 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 30 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 31 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 32 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 33 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 34 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 35 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 36 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 37 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 38 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 39 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 40 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 41 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 42 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 43 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 44 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 45 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 46 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 47 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 48 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 49 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 50 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 51 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(-1 + s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 52 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 53 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 54 => ((3 # 1) + s V_jpeg_idct_2x2_z + max0(s V_jpeg_idct_2x2_ctr) <= z)%Q
   | 55 => ((2 # 1) + s V_jpeg_idct_2x2_z + max0(s V_jpeg_idct_2x2_ctr) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_jpeg_idct_2x2 =>
    [mkPA Q (fun n z s => ai_jpeg_idct_2x2 n s /\ annot0_jpeg_idct_2x2 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_jpeg_idct_2x2 (proc_start P_jpeg_idct_2x2) s1 (proc_end P_jpeg_idct_2x2) s2 ->
    (s2 V_jpeg_idct_2x2_z <= (10 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_jpeg_idct_2x2.
Qed.
