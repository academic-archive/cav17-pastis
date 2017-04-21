Require Import pasta.Pasta.

Inductive proc: Type :=
  P_jpeg_idct_islow.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_jpeg_idct_islow_z := 1%positive.
Notation V_jpeg_idct_islow__tmp := 2%positive.
Notation V_jpeg_idct_islow_ctr := 3%positive.
Notation V_jpeg_idct_islow_dcval := 4%positive.
Notation V_jpeg_idct_islow_dcval1 := 5%positive.
Notation V_jpeg_idct_islow_tmp0 := 6%positive.
Notation V_jpeg_idct_islow_tmp1 := 7%positive.
Notation V_jpeg_idct_islow_tmp10 := 8%positive.
Notation V_jpeg_idct_islow_tmp11 := 9%positive.
Notation V_jpeg_idct_islow_tmp12 := 10%positive.
Notation V_jpeg_idct_islow_tmp13 := 11%positive.
Notation V_jpeg_idct_islow_tmp2 := 12%positive.
Notation V_jpeg_idct_islow_tmp3 := 13%positive.
Notation V_jpeg_idct_islow_z1 := 14%positive.
Notation V_jpeg_idct_islow_z2 := 15%positive.
Notation V_jpeg_idct_islow_z3 := 16%positive.
Notation V_jpeg_idct_islow_z4 := 17%positive.
Notation V_jpeg_idct_islow_z5 := 18%positive.
Notation V_jpeg_idct_islow_cinfo := 19%positive.
Notation V_jpeg_idct_islow_coef_block := 20%positive.
Notation V_jpeg_idct_islow_compptr := 21%positive.
Notation V_jpeg_idct_islow_output_buf := 22%positive.
Notation V_jpeg_idct_islow_output_col := 23%positive.
Definition Pedges_jpeg_idct_islow: list (edge proc) :=
  (EA 1 (AAssign V_jpeg_idct_islow_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_jpeg_idct_islow__tmp (Some (EVar V_jpeg_idct_islow_output_col))) 3)::
  (EA 3 (AAssign V_jpeg_idct_islow_ctr (Some (ENum (8)))) 4)::
  (EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_jpeg_idct_islow_ctr) s) > (eval (ENum (0))
  s))%Z)) 58)::(EA 6 (AGuard (fun s => ((eval (EVar V_jpeg_idct_islow_ctr)
  s) <= (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_jpeg_idct_islow_ctr (Some (ENum (0)))) 9)::(EA 9 ANone 10)::
  (EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_jpeg_idct_islow_ctr) s) < (eval (ENum (8))
  s))%Z)) 14)::(EA 11 (AGuard (fun s => ((eval (EVar V_jpeg_idct_islow_ctr)
  s) >= (eval (ENum (8)) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 14 AWeaken 15)::(EA 15 ANone 51)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_jpeg_idct_islow_z2 None) 17)::(EA 17 (AAssign V_jpeg_idct_islow_z3
  None) 18)::(EA 18 (AAssign V_jpeg_idct_islow_z1
  (Some (EMul (EAdd (EVar V_jpeg_idct_islow_z2) (EVar V_jpeg_idct_islow_z3))
  (ENum (4433))))) 19)::(EA 19 (AAssign V_jpeg_idct_islow_tmp2 None) 20)::
  (EA 20 (AAssign V_jpeg_idct_islow_tmp3
  (Some (EAdd (EVar V_jpeg_idct_islow_z1) (EMul (EVar V_jpeg_idct_islow_z2)
  (ENum (6270)))))) 21)::(EA 21 (AAssign V_jpeg_idct_islow_tmp0 None) 22)::
  (EA 22 (AAssign V_jpeg_idct_islow_tmp1 None) 23)::(EA 23 (AAssign
  V_jpeg_idct_islow_tmp10 (Some (EAdd (EVar V_jpeg_idct_islow_tmp0)
  (EVar V_jpeg_idct_islow_tmp3)))) 24)::(EA 24 (AAssign
  V_jpeg_idct_islow_tmp13 (Some (ESub (EVar V_jpeg_idct_islow_tmp0)
  (EVar V_jpeg_idct_islow_tmp3)))) 25)::(EA 25 (AAssign
  V_jpeg_idct_islow_tmp11 (Some (EAdd (EVar V_jpeg_idct_islow_tmp1)
  (EVar V_jpeg_idct_islow_tmp2)))) 26)::(EA 26 (AAssign
  V_jpeg_idct_islow_tmp12 (Some (ESub (EVar V_jpeg_idct_islow_tmp1)
  (EVar V_jpeg_idct_islow_tmp2)))) 27)::(EA 27 (AAssign
  V_jpeg_idct_islow_tmp0 None) 28)::(EA 28 (AAssign V_jpeg_idct_islow_tmp1
  None) 29)::(EA 29 (AAssign V_jpeg_idct_islow_tmp2 None) 30)::
  (EA 30 (AAssign V_jpeg_idct_islow_tmp3 None) 31)::(EA 31 (AAssign
  V_jpeg_idct_islow_z1 (Some (EAdd (EVar V_jpeg_idct_islow_tmp0)
  (EVar V_jpeg_idct_islow_tmp3)))) 32)::(EA 32 (AAssign V_jpeg_idct_islow_z2
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp1)
  (EVar V_jpeg_idct_islow_tmp2)))) 33)::(EA 33 (AAssign V_jpeg_idct_islow_z3
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp0)
  (EVar V_jpeg_idct_islow_tmp2)))) 34)::(EA 34 (AAssign V_jpeg_idct_islow_z4
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp1)
  (EVar V_jpeg_idct_islow_tmp3)))) 35)::(EA 35 (AAssign V_jpeg_idct_islow_z5
  (Some (EMul (EAdd (EVar V_jpeg_idct_islow_z3) (EVar V_jpeg_idct_islow_z4))
  (ENum (9633))))) 36)::(EA 36 (AAssign V_jpeg_idct_islow_tmp0
  (Some (EMul (EVar V_jpeg_idct_islow_tmp0) (ENum (2446))))) 37)::
  (EA 37 (AAssign V_jpeg_idct_islow_tmp1 None) 38)::(EA 38 (AAssign
  V_jpeg_idct_islow_tmp2 None) 39)::(EA 39 (AAssign V_jpeg_idct_islow_tmp3
  None) 40)::(EA 40 (AAssign V_jpeg_idct_islow_z1
  (Some (EMul (EVar V_jpeg_idct_islow_z1) (ENum (-7373))))) 41)::
  (EA 41 (AAssign V_jpeg_idct_islow_z2 None) 42)::(EA 42 (AAssign
  V_jpeg_idct_islow_z3 None) 43)::(EA 43 (AAssign V_jpeg_idct_islow_z4
  (Some (EMul (EVar V_jpeg_idct_islow_z4) (ENum (-3196))))) 44)::
  (EA 44 (AAssign V_jpeg_idct_islow_z3
  (Some (EAdd (EVar V_jpeg_idct_islow_z3)
  (EVar V_jpeg_idct_islow_z5)))) 45)::(EA 45 (AAssign V_jpeg_idct_islow_z4
  (Some (EAdd (EVar V_jpeg_idct_islow_z4)
  (EVar V_jpeg_idct_islow_z5)))) 46)::(EA 46 (AAssign V_jpeg_idct_islow_tmp0
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp0) (EAdd (EVar V_jpeg_idct_islow_z1)
  (EVar V_jpeg_idct_islow_z3))))) 47)::(EA 47 (AAssign V_jpeg_idct_islow_tmp1
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp1) (EAdd (EVar V_jpeg_idct_islow_z2)
  (EVar V_jpeg_idct_islow_z4))))) 48)::(EA 48 (AAssign V_jpeg_idct_islow_tmp2
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp2) (EAdd (EVar V_jpeg_idct_islow_z2)
  (EVar V_jpeg_idct_islow_z3))))) 49)::(EA 49 (AAssign V_jpeg_idct_islow_tmp3
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp3) (EAdd (EVar V_jpeg_idct_islow_z1)
  (EVar V_jpeg_idct_islow_z4))))) 50)::(EA 50 ANone 53)::(EA 51 (AAssign
  V_jpeg_idct_islow_dcval1 None) 52)::(EA 52 ANone 53)::(EA 53 (AAssign
  V_jpeg_idct_islow_ctr (Some (EAdd (EVar V_jpeg_idct_islow_ctr)
  (ENum (1))))) 54)::(EA 54 ANone 55)::(EA 55 ANone 56)::(EA 56 (AAssign
  V_jpeg_idct_islow_z (Some (EAdd (ENum (1))
  (EVar V_jpeg_idct_islow_z)))) 57)::(EA 57 AWeaken 11)::(EA 58 AWeaken 59)::
  (EA 59 ANone 97)::(EA 59 ANone 60)::(EA 60 (AAssign V_jpeg_idct_islow_z2
  None) 61)::(EA 61 (AAssign V_jpeg_idct_islow_z3 None) 62)::(EA 62 (AAssign
  V_jpeg_idct_islow_z1 (Some (EMul (EAdd (EVar V_jpeg_idct_islow_z2)
  (EVar V_jpeg_idct_islow_z3)) (ENum (4433))))) 63)::(EA 63 (AAssign
  V_jpeg_idct_islow_tmp2 None) 64)::(EA 64 (AAssign V_jpeg_idct_islow_tmp3
  (Some (EAdd (EVar V_jpeg_idct_islow_z1) (EMul (EVar V_jpeg_idct_islow_z2)
  (ENum (6270)))))) 65)::(EA 65 (AAssign V_jpeg_idct_islow_z2 None) 66)::
  (EA 66 (AAssign V_jpeg_idct_islow_z3 None) 67)::(EA 67 (AAssign
  V_jpeg_idct_islow_tmp0 None) 68)::(EA 68 (AAssign V_jpeg_idct_islow_tmp1
  None) 69)::(EA 69 (AAssign V_jpeg_idct_islow_tmp10
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp0)
  (EVar V_jpeg_idct_islow_tmp3)))) 70)::(EA 70 (AAssign
  V_jpeg_idct_islow_tmp13 (Some (ESub (EVar V_jpeg_idct_islow_tmp0)
  (EVar V_jpeg_idct_islow_tmp3)))) 71)::(EA 71 (AAssign
  V_jpeg_idct_islow_tmp11 (Some (EAdd (EVar V_jpeg_idct_islow_tmp1)
  (EVar V_jpeg_idct_islow_tmp2)))) 72)::(EA 72 (AAssign
  V_jpeg_idct_islow_tmp12 (Some (ESub (EVar V_jpeg_idct_islow_tmp1)
  (EVar V_jpeg_idct_islow_tmp2)))) 73)::(EA 73 (AAssign
  V_jpeg_idct_islow_tmp0 None) 74)::(EA 74 (AAssign V_jpeg_idct_islow_tmp1
  None) 75)::(EA 75 (AAssign V_jpeg_idct_islow_tmp2 None) 76)::
  (EA 76 (AAssign V_jpeg_idct_islow_tmp3 None) 77)::(EA 77 (AAssign
  V_jpeg_idct_islow_z1 (Some (EAdd (EVar V_jpeg_idct_islow_tmp0)
  (EVar V_jpeg_idct_islow_tmp3)))) 78)::(EA 78 (AAssign V_jpeg_idct_islow_z2
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp1)
  (EVar V_jpeg_idct_islow_tmp2)))) 79)::(EA 79 (AAssign V_jpeg_idct_islow_z3
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp0)
  (EVar V_jpeg_idct_islow_tmp2)))) 80)::(EA 80 (AAssign V_jpeg_idct_islow_z4
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp1)
  (EVar V_jpeg_idct_islow_tmp3)))) 81)::(EA 81 (AAssign V_jpeg_idct_islow_z5
  (Some (EMul (EAdd (EVar V_jpeg_idct_islow_z3) (EVar V_jpeg_idct_islow_z4))
  (ENum (9633))))) 82)::(EA 82 (AAssign V_jpeg_idct_islow_tmp0
  (Some (EMul (EVar V_jpeg_idct_islow_tmp0) (ENum (2446))))) 83)::
  (EA 83 (AAssign V_jpeg_idct_islow_tmp1 None) 84)::(EA 84 (AAssign
  V_jpeg_idct_islow_tmp2 None) 85)::(EA 85 (AAssign V_jpeg_idct_islow_tmp3
  None) 86)::(EA 86 (AAssign V_jpeg_idct_islow_z1
  (Some (EMul (EVar V_jpeg_idct_islow_z1) (ENum (-7373))))) 87)::
  (EA 87 (AAssign V_jpeg_idct_islow_z2 None) 88)::(EA 88 (AAssign
  V_jpeg_idct_islow_z3 None) 89)::(EA 89 (AAssign V_jpeg_idct_islow_z4
  (Some (EMul (EVar V_jpeg_idct_islow_z4) (ENum (-3196))))) 90)::
  (EA 90 (AAssign V_jpeg_idct_islow_z3
  (Some (EAdd (EVar V_jpeg_idct_islow_z3)
  (EVar V_jpeg_idct_islow_z5)))) 91)::(EA 91 (AAssign V_jpeg_idct_islow_z4
  (Some (EAdd (EVar V_jpeg_idct_islow_z4)
  (EVar V_jpeg_idct_islow_z5)))) 92)::(EA 92 (AAssign V_jpeg_idct_islow_tmp0
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp0) (EAdd (EVar V_jpeg_idct_islow_z1)
  (EVar V_jpeg_idct_islow_z3))))) 93)::(EA 93 (AAssign V_jpeg_idct_islow_tmp1
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp1) (EAdd (EVar V_jpeg_idct_islow_z2)
  (EVar V_jpeg_idct_islow_z4))))) 94)::(EA 94 (AAssign V_jpeg_idct_islow_tmp2
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp2) (EAdd (EVar V_jpeg_idct_islow_z2)
  (EVar V_jpeg_idct_islow_z3))))) 95)::(EA 95 (AAssign V_jpeg_idct_islow_tmp3
  (Some (EAdd (EVar V_jpeg_idct_islow_tmp3) (EAdd (EVar V_jpeg_idct_islow_z1)
  (EVar V_jpeg_idct_islow_z4))))) 96)::(EA 96 ANone 99)::(EA 97 (AAssign
  V_jpeg_idct_islow_dcval None) 98)::(EA 98 ANone 99)::(EA 99 (AAssign
  V_jpeg_idct_islow_ctr (Some (EAdd (EVar V_jpeg_idct_islow_ctr)
  (ENum (-1))))) 100)::(EA 100 ANone 101)::(EA 101 ANone 102)::
  (EA 102 (AAssign V_jpeg_idct_islow_z (Some (EAdd (ENum (1))
  (EVar V_jpeg_idct_islow_z)))) 103)::(EA 103 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_jpeg_idct_islow => Pedges_jpeg_idct_islow
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_jpeg_idct_islow => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_jpeg_idct_islow (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0)%Z
   | 3 => (-1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_z <= 0)%Z
   | 4 => (1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 8 <= 0)%Z
   | 5 => (-1 * s V_jpeg_idct_islow_ctr + 8 <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_z <= 0)%Z
   | 6 => (-1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 7 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 8 => (1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 9 => (-1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 10 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ 1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0)%Z
   | 11 => (-1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 12 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 8 <= 0)%Z
   | 13 => (-1 * s V_jpeg_idct_islow_ctr + 8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 14 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 15 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 16 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 17 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 18 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 19 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 20 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 21 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 22 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 23 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 24 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 25 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 26 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 27 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 28 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 29 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 30 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 31 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 32 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 33 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 34 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 35 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 36 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 37 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 38 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 39 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 40 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 41 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 42 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 43 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 44 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 45 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 46 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 47 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 48 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 49 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 50 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 51 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 52 => (1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 53 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0)%Z
   | 54 => (-1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 55 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0)%Z
   | 56 => (-1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 57 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z + 1 <= 0)%Z
   | 58 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 59 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 60 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 61 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 62 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 63 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 64 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 65 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 66 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 67 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 68 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 69 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 70 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 71 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 72 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 73 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 74 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 75 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 76 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 77 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 78 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 79 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 80 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 81 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 82 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 83 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 84 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 85 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 86 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 87 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 88 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 89 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 90 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 91 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 92 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 93 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 94 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 95 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 96 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 97 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 98 => (-1 * s V_jpeg_idct_islow_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -8 <= 0)%Z
   | 99 => (1 * s V_jpeg_idct_islow_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0 /\ -1 * s V_jpeg_idct_islow_ctr + 1 <= 0)%Z
   | 100 => (-1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 101 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z <= 0)%Z
   | 102 => (-1 * s V_jpeg_idct_islow_z <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_ctr <= 0)%Z
   | 103 => (-1 * s V_jpeg_idct_islow_ctr <= 0 /\ 1 * s V_jpeg_idct_islow_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_islow_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_jpeg_idct_islow (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((16 # 1) <= z)%Q
   | 2 => ((16 # 1) + s V_jpeg_idct_islow_z <= z)%Q
   | 3 => ((16 # 1) + s V_jpeg_idct_islow_z <= z)%Q
   | 4 => ((8 # 1) + s V_jpeg_idct_islow_z + max0(s V_jpeg_idct_islow_ctr) <= z)%Q
   | 5 => ((8 # 1) + s V_jpeg_idct_islow_z + max0(s V_jpeg_idct_islow_ctr) <= z)%Q
   | 6 => ((8 # 1) + s V_jpeg_idct_islow_z + max0(s V_jpeg_idct_islow_ctr) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_jpeg_idct_islow_ctr) (-1
                                                                    + s V_jpeg_idct_islow_ctr));
      (*-1 0*) F_max0_ge_0 (-1 + s V_jpeg_idct_islow_ctr)]
     ((8 # 1) + s V_jpeg_idct_islow_z + max0(s V_jpeg_idct_islow_ctr) <= z)%Q
   | 8 => ((8 # 1) + s V_jpeg_idct_islow_z <= z)%Q
   | 9 => (s V_jpeg_idct_islow_z + max0(8 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 10 => (s V_jpeg_idct_islow_z + max0(8 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 11 => (s V_jpeg_idct_islow_z + max0(8 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_jpeg_idct_islow_ctr) (7
                                                                    - s V_jpeg_idct_islow_ctr));
      (*-1 0*) F_max0_ge_0 (7 - s V_jpeg_idct_islow_ctr)]
     (s V_jpeg_idct_islow_z + max0(8 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 13 => (s V_jpeg_idct_islow_z <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (8 - s V_jpeg_idct_islow_ctr) (1)]
     (s V_jpeg_idct_islow_z + max0(8 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 15 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 16 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 17 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 18 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 19 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 20 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 21 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 22 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 23 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 24 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 25 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 26 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 27 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 28 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 29 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 30 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 31 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 32 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 33 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 34 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 35 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 36 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 37 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 38 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 39 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 40 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 41 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 42 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 43 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 44 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 45 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 46 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 47 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 48 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 49 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 50 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 51 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 52 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 53 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(7 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 54 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(8 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 55 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(8 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 56 => ((1 # 1) + s V_jpeg_idct_islow_z
            + max0(8 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 57 => (s V_jpeg_idct_islow_z + max0(8 - s V_jpeg_idct_islow_ctr) <= z)%Q
   | 58 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_jpeg_idct_islow_ctr) (1)]
     ((8 # 1) + s V_jpeg_idct_islow_z + max0(s V_jpeg_idct_islow_ctr) <= z)%Q
   | 59 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 60 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 61 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 62 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 63 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 64 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 65 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 66 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 67 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 68 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 69 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 70 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 71 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 72 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 73 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 74 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 75 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 76 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 77 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 78 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 79 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 80 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 81 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 82 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 83 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 84 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 85 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 86 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 87 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 88 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 89 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 90 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 91 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 92 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 93 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 94 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 95 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 96 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 97 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 98 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 99 => ((9 # 1) + s V_jpeg_idct_islow_z
            + max0(-1 + s V_jpeg_idct_islow_ctr) <= z)%Q
   | 100 => ((9 # 1) + s V_jpeg_idct_islow_z + max0(s V_jpeg_idct_islow_ctr) <= z)%Q
   | 101 => ((9 # 1) + s V_jpeg_idct_islow_z + max0(s V_jpeg_idct_islow_ctr) <= z)%Q
   | 102 => ((9 # 1) + s V_jpeg_idct_islow_z + max0(s V_jpeg_idct_islow_ctr) <= z)%Q
   | 103 => ((8 # 1) + s V_jpeg_idct_islow_z + max0(s V_jpeg_idct_islow_ctr) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_jpeg_idct_islow =>
    [mkPA Q (fun n z s => ai_jpeg_idct_islow n s /\ annot0_jpeg_idct_islow n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_jpeg_idct_islow (proc_start P_jpeg_idct_islow) s1 (proc_end P_jpeg_idct_islow) s2 ->
    (s2 V_jpeg_idct_islow_z <= (16 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_jpeg_idct_islow.
Qed.
