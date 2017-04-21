Require Import pasta.Pasta.

Inductive proc: Type :=
  P_jpeg_idct_ifast.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_jpeg_idct_ifast_z := 1%positive.
Notation V_jpeg_idct_ifast__tmp := 2%positive.
Notation V_jpeg_idct_ifast_ctr := 3%positive.
Notation V_jpeg_idct_ifast_dcval := 4%positive.
Notation V_jpeg_idct_ifast_dcval1 := 5%positive.
Notation V_jpeg_idct_ifast_tmp0 := 6%positive.
Notation V_jpeg_idct_ifast_tmp1 := 7%positive.
Notation V_jpeg_idct_ifast_tmp10 := 8%positive.
Notation V_jpeg_idct_ifast_tmp11 := 9%positive.
Notation V_jpeg_idct_ifast_tmp12 := 10%positive.
Notation V_jpeg_idct_ifast_tmp13 := 11%positive.
Notation V_jpeg_idct_ifast_tmp2 := 12%positive.
Notation V_jpeg_idct_ifast_tmp3 := 13%positive.
Notation V_jpeg_idct_ifast_tmp4 := 14%positive.
Notation V_jpeg_idct_ifast_tmp5 := 15%positive.
Notation V_jpeg_idct_ifast_tmp6 := 16%positive.
Notation V_jpeg_idct_ifast_tmp7 := 17%positive.
Notation V_jpeg_idct_ifast_z10 := 18%positive.
Notation V_jpeg_idct_ifast_z11 := 19%positive.
Notation V_jpeg_idct_ifast_z12 := 20%positive.
Notation V_jpeg_idct_ifast_z13 := 21%positive.
Notation V_jpeg_idct_ifast_z5 := 22%positive.
Notation V_jpeg_idct_ifast_cinfo := 23%positive.
Notation V_jpeg_idct_ifast_coef_block := 24%positive.
Notation V_jpeg_idct_ifast_compptr := 25%positive.
Notation V_jpeg_idct_ifast_output_buf := 26%positive.
Notation V_jpeg_idct_ifast_output_col := 27%positive.
Definition Pedges_jpeg_idct_ifast: list (edge proc) :=
  (EA 1 (AAssign V_jpeg_idct_ifast_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_jpeg_idct_ifast__tmp (Some (EVar V_jpeg_idct_ifast_output_col))) 3)::
  (EA 3 (AAssign V_jpeg_idct_ifast_ctr (Some (ENum (8)))) 4)::
  (EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_jpeg_idct_ifast_ctr) s) > (eval (ENum (0))
  s))%Z)) 44)::(EA 6 (AGuard (fun s => ((eval (EVar V_jpeg_idct_ifast_ctr)
  s) <= (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_jpeg_idct_ifast_ctr (Some (ENum (0)))) 9)::(EA 9 ANone 10)::
  (EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_jpeg_idct_ifast_ctr) s) < (eval (ENum (8))
  s))%Z)) 14)::(EA 11 (AGuard (fun s => ((eval (EVar V_jpeg_idct_ifast_ctr)
  s) >= (eval (ENum (8)) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 14 AWeaken 15)::(EA 15 ANone 37)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_jpeg_idct_ifast_tmp10 None) 17)::(EA 17 (AAssign V_jpeg_idct_ifast_tmp11
  None) 18)::(EA 18 (AAssign V_jpeg_idct_ifast_tmp13 None) 19)::
  (EA 19 (AAssign V_jpeg_idct_ifast_tmp12 None) 20)::(EA 20 (AAssign
  V_jpeg_idct_ifast_tmp0 (Some (EAdd (EVar V_jpeg_idct_ifast_tmp10)
  (EVar V_jpeg_idct_ifast_tmp13)))) 21)::(EA 21 (AAssign
  V_jpeg_idct_ifast_tmp3 (Some (ESub (EVar V_jpeg_idct_ifast_tmp10)
  (EVar V_jpeg_idct_ifast_tmp13)))) 22)::(EA 22 (AAssign
  V_jpeg_idct_ifast_tmp1 (Some (EAdd (EVar V_jpeg_idct_ifast_tmp11)
  (EVar V_jpeg_idct_ifast_tmp12)))) 23)::(EA 23 (AAssign
  V_jpeg_idct_ifast_tmp2 (Some (ESub (EVar V_jpeg_idct_ifast_tmp11)
  (EVar V_jpeg_idct_ifast_tmp12)))) 24)::(EA 24 (AAssign
  V_jpeg_idct_ifast_z13 None) 25)::(EA 25 (AAssign V_jpeg_idct_ifast_z10
  None) 26)::(EA 26 (AAssign V_jpeg_idct_ifast_z11 None) 27)::(EA 27 (AAssign
  V_jpeg_idct_ifast_z12 None) 28)::(EA 28 (AAssign V_jpeg_idct_ifast_tmp7
  (Some (EAdd (EVar V_jpeg_idct_ifast_z11)
  (EVar V_jpeg_idct_ifast_z13)))) 29)::(EA 29 (AAssign
  V_jpeg_idct_ifast_tmp11 None) 30)::(EA 30 (AAssign V_jpeg_idct_ifast_z5
  None) 31)::(EA 31 (AAssign V_jpeg_idct_ifast_tmp10 None) 32)::
  (EA 32 (AAssign V_jpeg_idct_ifast_tmp12 None) 33)::(EA 33 (AAssign
  V_jpeg_idct_ifast_tmp6 (Some (ESub (EVar V_jpeg_idct_ifast_tmp12)
  (EVar V_jpeg_idct_ifast_tmp7)))) 34)::(EA 34 (AAssign
  V_jpeg_idct_ifast_tmp5 (Some (ESub (EVar V_jpeg_idct_ifast_tmp11)
  (EVar V_jpeg_idct_ifast_tmp6)))) 35)::(EA 35 (AAssign
  V_jpeg_idct_ifast_tmp4 (Some (EAdd (EVar V_jpeg_idct_ifast_tmp10)
  (EVar V_jpeg_idct_ifast_tmp5)))) 36)::(EA 36 ANone 39)::(EA 37 (AAssign
  V_jpeg_idct_ifast_dcval1 None) 38)::(EA 38 ANone 39)::(EA 39 (AAssign
  V_jpeg_idct_ifast_ctr (Some (EAdd (EVar V_jpeg_idct_ifast_ctr)
  (ENum (1))))) 40)::(EA 40 ANone 41)::(EA 41 ANone 42)::(EA 42 (AAssign
  V_jpeg_idct_ifast_z (Some (EAdd (ENum (1))
  (EVar V_jpeg_idct_ifast_z)))) 43)::(EA 43 AWeaken 11)::(EA 44 AWeaken 45)::
  (EA 45 ANone 75)::(EA 45 ANone 46)::(EA 46 (AAssign V_jpeg_idct_ifast_tmp0
  None) 47)::(EA 47 (AAssign V_jpeg_idct_ifast_tmp1 None) 48)::
  (EA 48 (AAssign V_jpeg_idct_ifast_tmp2 None) 49)::(EA 49 (AAssign
  V_jpeg_idct_ifast_tmp3 None) 50)::(EA 50 (AAssign V_jpeg_idct_ifast_tmp10
  (Some (EAdd (EVar V_jpeg_idct_ifast_tmp0)
  (EVar V_jpeg_idct_ifast_tmp2)))) 51)::(EA 51 (AAssign
  V_jpeg_idct_ifast_tmp11 (Some (ESub (EVar V_jpeg_idct_ifast_tmp0)
  (EVar V_jpeg_idct_ifast_tmp2)))) 52)::(EA 52 (AAssign
  V_jpeg_idct_ifast_tmp13 (Some (EAdd (EVar V_jpeg_idct_ifast_tmp1)
  (EVar V_jpeg_idct_ifast_tmp3)))) 53)::(EA 53 (AAssign
  V_jpeg_idct_ifast_tmp12 None) 54)::(EA 54 (AAssign V_jpeg_idct_ifast_tmp0
  (Some (EAdd (EVar V_jpeg_idct_ifast_tmp10)
  (EVar V_jpeg_idct_ifast_tmp13)))) 55)::(EA 55 (AAssign
  V_jpeg_idct_ifast_tmp3 (Some (ESub (EVar V_jpeg_idct_ifast_tmp10)
  (EVar V_jpeg_idct_ifast_tmp13)))) 56)::(EA 56 (AAssign
  V_jpeg_idct_ifast_tmp1 (Some (EAdd (EVar V_jpeg_idct_ifast_tmp11)
  (EVar V_jpeg_idct_ifast_tmp12)))) 57)::(EA 57 (AAssign
  V_jpeg_idct_ifast_tmp2 (Some (ESub (EVar V_jpeg_idct_ifast_tmp11)
  (EVar V_jpeg_idct_ifast_tmp12)))) 58)::(EA 58 (AAssign
  V_jpeg_idct_ifast_tmp4 None) 59)::(EA 59 (AAssign V_jpeg_idct_ifast_tmp5
  None) 60)::(EA 60 (AAssign V_jpeg_idct_ifast_tmp6 None) 61)::
  (EA 61 (AAssign V_jpeg_idct_ifast_tmp7 None) 62)::(EA 62 (AAssign
  V_jpeg_idct_ifast_z13 (Some (EAdd (EVar V_jpeg_idct_ifast_tmp6)
  (EVar V_jpeg_idct_ifast_tmp5)))) 63)::(EA 63 (AAssign V_jpeg_idct_ifast_z10
  (Some (ESub (EVar V_jpeg_idct_ifast_tmp6)
  (EVar V_jpeg_idct_ifast_tmp5)))) 64)::(EA 64 (AAssign V_jpeg_idct_ifast_z11
  (Some (EAdd (EVar V_jpeg_idct_ifast_tmp4)
  (EVar V_jpeg_idct_ifast_tmp7)))) 65)::(EA 65 (AAssign V_jpeg_idct_ifast_z12
  (Some (ESub (EVar V_jpeg_idct_ifast_tmp4)
  (EVar V_jpeg_idct_ifast_tmp7)))) 66)::(EA 66 (AAssign
  V_jpeg_idct_ifast_tmp7 (Some (EAdd (EVar V_jpeg_idct_ifast_z11)
  (EVar V_jpeg_idct_ifast_z13)))) 67)::(EA 67 (AAssign
  V_jpeg_idct_ifast_tmp11 None) 68)::(EA 68 (AAssign V_jpeg_idct_ifast_z5
  None) 69)::(EA 69 (AAssign V_jpeg_idct_ifast_tmp10 None) 70)::
  (EA 70 (AAssign V_jpeg_idct_ifast_tmp12 None) 71)::(EA 71 (AAssign
  V_jpeg_idct_ifast_tmp6 (Some (ESub (EVar V_jpeg_idct_ifast_tmp12)
  (EVar V_jpeg_idct_ifast_tmp7)))) 72)::(EA 72 (AAssign
  V_jpeg_idct_ifast_tmp5 (Some (ESub (EVar V_jpeg_idct_ifast_tmp11)
  (EVar V_jpeg_idct_ifast_tmp6)))) 73)::(EA 73 (AAssign
  V_jpeg_idct_ifast_tmp4 (Some (EAdd (EVar V_jpeg_idct_ifast_tmp10)
  (EVar V_jpeg_idct_ifast_tmp5)))) 74)::(EA 74 ANone 77)::(EA 75 (AAssign
  V_jpeg_idct_ifast_dcval None) 76)::(EA 76 ANone 77)::(EA 77 (AAssign
  V_jpeg_idct_ifast_ctr (Some (EAdd (EVar V_jpeg_idct_ifast_ctr)
  (ENum (-1))))) 78)::(EA 78 ANone 79)::(EA 79 ANone 80)::(EA 80 (AAssign
  V_jpeg_idct_ifast_z (Some (EAdd (ENum (1))
  (EVar V_jpeg_idct_ifast_z)))) 81)::(EA 81 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_jpeg_idct_ifast => Pedges_jpeg_idct_ifast
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_jpeg_idct_ifast => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_jpeg_idct_ifast (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0)%Z
   | 3 => (-1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_z <= 0)%Z
   | 4 => (1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 8 <= 0)%Z
   | 5 => (-1 * s V_jpeg_idct_ifast_ctr + 8 <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_z <= 0)%Z
   | 6 => (-1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 7 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 8 => (1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 9 => (-1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 10 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0)%Z
   | 11 => (-1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 12 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 8 <= 0)%Z
   | 13 => (-1 * s V_jpeg_idct_ifast_ctr + 8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 14 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 15 => (1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 16 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 17 => (1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 18 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 19 => (1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 20 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 21 => (1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 22 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 23 => (1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 24 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 25 => (1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 26 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 27 => (1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 28 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 29 => (1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 30 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 31 => (1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 32 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 33 => (1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 34 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 35 => (1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 36 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 37 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 38 => (1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 39 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0)%Z
   | 40 => (-1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 41 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0)%Z
   | 42 => (-1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 43 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z + 1 <= 0)%Z
   | 44 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 45 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 46 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 47 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 48 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 49 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 50 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 51 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 52 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 53 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 54 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 55 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 56 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 57 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 58 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 59 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 60 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 61 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 62 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 63 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 64 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 65 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 66 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 67 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 68 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 69 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 70 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 71 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 72 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 73 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 74 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 75 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 76 => (-1 * s V_jpeg_idct_ifast_ctr + 1 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -8 <= 0)%Z
   | 77 => (1 * s V_jpeg_idct_ifast_ctr + -8 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr + 1 <= 0)%Z
   | 78 => (-1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 79 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z <= 0)%Z
   | 80 => (-1 * s V_jpeg_idct_ifast_z <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_ctr <= 0)%Z
   | 81 => (-1 * s V_jpeg_idct_ifast_ctr <= 0 /\ 1 * s V_jpeg_idct_ifast_ctr + -7 <= 0 /\ -1 * s V_jpeg_idct_ifast_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_jpeg_idct_ifast (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((16 # 1) <= z)%Q
   | 2 => ((16 # 1) + s V_jpeg_idct_ifast_z <= z)%Q
   | 3 => ((16 # 1) + s V_jpeg_idct_ifast_z <= z)%Q
   | 4 => ((8 # 1) + s V_jpeg_idct_ifast_z + max0(s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 5 => ((8 # 1) + s V_jpeg_idct_ifast_z + max0(s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 6 => ((8 # 1) + s V_jpeg_idct_ifast_z + max0(s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_jpeg_idct_ifast_ctr) (-1
                                                                    + s V_jpeg_idct_ifast_ctr));
      (*-1 0*) F_max0_ge_0 (-1 + s V_jpeg_idct_ifast_ctr)]
     ((8 # 1) + s V_jpeg_idct_ifast_z + max0(s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 8 => ((8 # 1) + s V_jpeg_idct_ifast_z <= z)%Q
   | 9 => (s V_jpeg_idct_ifast_z + max0(8 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 10 => (s V_jpeg_idct_ifast_z + max0(8 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 11 => (s V_jpeg_idct_ifast_z + max0(8 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_jpeg_idct_ifast_ctr) (7
                                                                    - s V_jpeg_idct_ifast_ctr));
      (*-1 0*) F_max0_ge_0 (7 - s V_jpeg_idct_ifast_ctr)]
     (s V_jpeg_idct_ifast_z + max0(8 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 13 => (s V_jpeg_idct_ifast_z <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (8 - s V_jpeg_idct_ifast_ctr) (1)]
     (s V_jpeg_idct_ifast_z + max0(8 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 15 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 16 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 17 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 18 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 19 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 20 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 21 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 22 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 23 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 24 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 25 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 26 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 27 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 28 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 29 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 30 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 31 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 32 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 33 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 34 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 35 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 36 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 37 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 38 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 39 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(7 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 40 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(8 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 41 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(8 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 42 => ((1 # 1) + s V_jpeg_idct_ifast_z
            + max0(8 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 43 => (s V_jpeg_idct_ifast_z + max0(8 - s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 44 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_jpeg_idct_ifast_ctr) (1)]
     ((8 # 1) + s V_jpeg_idct_ifast_z + max0(s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 45 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 46 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 47 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 48 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 49 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 50 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 51 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 52 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 53 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 54 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 55 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 56 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 57 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 58 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 59 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 60 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 61 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 62 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 63 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 64 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 65 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 66 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 67 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 68 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 69 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 70 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 71 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 72 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 73 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 74 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 75 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 76 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 77 => ((9 # 1) + s V_jpeg_idct_ifast_z
            + max0(-1 + s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 78 => ((9 # 1) + s V_jpeg_idct_ifast_z + max0(s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 79 => ((9 # 1) + s V_jpeg_idct_ifast_z + max0(s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 80 => ((9 # 1) + s V_jpeg_idct_ifast_z + max0(s V_jpeg_idct_ifast_ctr) <= z)%Q
   | 81 => ((8 # 1) + s V_jpeg_idct_ifast_z + max0(s V_jpeg_idct_ifast_ctr) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_jpeg_idct_ifast =>
    [mkPA Q (fun n z s => ai_jpeg_idct_ifast n s /\ annot0_jpeg_idct_ifast n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_jpeg_idct_ifast (proc_start P_jpeg_idct_ifast) s1 (proc_end P_jpeg_idct_ifast) s2 ->
    (s2 V_jpeg_idct_ifast_z <= (16 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_jpeg_idct_ifast.
Qed.
