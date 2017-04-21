Require Import pasta.Pasta.

Inductive proc: Type :=
  P_forward_DCT.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_forward_DCT_z := 1%positive.
Notation V_forward_DCT__tmp := 2%positive.
Notation V_forward_DCT__tmp1 := 3%positive.
Notation V_forward_DCT__tmp2 := 4%positive.
Notation V_forward_DCT_bi := 5%positive.
Notation V_forward_DCT_elemr := 6%positive.
Notation V_forward_DCT_i := 7%positive.
Notation V_forward_DCT_qval := 8%positive.
Notation V_forward_DCT_temp := 9%positive.
Notation V_forward_DCT_cinfo := 10%positive.
Notation V_forward_DCT_coef_blocks := 11%positive.
Notation V_forward_DCT_compptr := 12%positive.
Notation V_forward_DCT_num_blocks := 13%positive.
Notation V_forward_DCT_sample_data := 14%positive.
Notation V_forward_DCT_start_col := 15%positive.
Notation V_forward_DCT_start_row := 16%positive.
Definition Pedges_forward_DCT: list (edge proc) :=
  (EA 1 (AAssign V_forward_DCT_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_bi) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_forward_DCT__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_forward_DCT__tmp2 (Some (EVar V_forward_DCT_start_row))) 6)::
  (EA 6 (AAssign V_forward_DCT__tmp1
  (Some (EVar V_forward_DCT_start_col))) 7)::(EA 7 (AAssign
  V_forward_DCT__tmp (Some (EVar V_forward_DCT_num_blocks))) 8)::
  (EA 8 (AAssign V_forward_DCT_bi (Some (ENum (0)))) 9)::(EA 9 ANone 10)::
  (EA 10 AWeaken 11)::(EA 11 (AGuard (fun s => ((eval (EVar V_forward_DCT_bi)
  s) < (eval (EVar V_forward_DCT__tmp) s))%Z)) 14)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_bi) s) >=
  (eval (EVar V_forward_DCT__tmp) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 14 AWeaken 15)::(EA 15 (AAssign V_forward_DCT_elemr
  (Some (ENum (0)))) 16)::(EA 16 ANone 17)::(EA 17 AWeaken 18)::
  (EA 18 (AGuard (fun s => ((eval (EVar V_forward_DCT_elemr) s) <
  (eval (ENum (8)) s))%Z)) 67)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_elemr) s) >= (eval (ENum (8))
  s))%Z)) 19)::(EA 19 AWeaken 20)::(EA 20 (AAssign V_forward_DCT_i
  (Some (ENum (0)))) 21)::(EA 21 ANone 22)::(EA 22 AWeaken 23)::
  (EA 23 (AGuard (fun s => ((eval (EVar V_forward_DCT_i) s) <
  (eval (ENum (64)) s))%Z)) 32)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_i) s) >= (eval (ENum (64))
  s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 ANone 26)::(EA 26 (AAssign
  V_forward_DCT_bi (Some (EAdd (EVar V_forward_DCT_bi) (ENum (1))))) 27)::
  (EA 27 (AAssign V_forward_DCT__tmp1 (Some (EAdd (EVar V_forward_DCT__tmp1)
  (ENum (8))))) 28)::(EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign
  V_forward_DCT_z (Some (EAdd (ENum (1)) (EVar V_forward_DCT_z)))) 31)::
  (EA 31 AWeaken 11)::(EA 32 AWeaken 33)::(EA 33 (AAssign V_forward_DCT_qval
  None) 34)::(EA 34 (AAssign V_forward_DCT_temp None) 35)::
  (EA 35 AWeaken 36)::(EA 36 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_temp) s) < (eval (ENum (0))
  s))%Z)) 48)::(EA 36 (AGuard (fun s => ((eval (EVar V_forward_DCT_temp)
  s) >= (eval (ENum (0)) s))%Z)) 37)::(EA 37 AWeaken 38)::(EA 38 (AAssign
  V_forward_DCT_temp None) 39)::(EA 39 AWeaken 40)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_temp) s) >=
  (eval (EVar V_forward_DCT_qval) s))%Z)) 44)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_temp) s) <
  (eval (EVar V_forward_DCT_qval) s))%Z)) 41)::(EA 41 AWeaken 42)::
  (EA 42 (AAssign V_forward_DCT_temp (Some (ENum (0)))) 43)::
  (EA 43 ANone 47)::(EA 44 AWeaken 45)::(EA 45 (AAssign V_forward_DCT_temp
  None) 46)::(EA 46 ANone 47)::(EA 47 ANone 61)::(EA 48 AWeaken 49)::
  (EA 49 (AAssign V_forward_DCT_temp (Some (ESub (ENum (0))
  (EVar V_forward_DCT_temp)))) 50)::(EA 50 (AAssign V_forward_DCT_temp
  None) 51)::(EA 51 AWeaken 52)::(EA 52 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_temp) s) >=
  (eval (EVar V_forward_DCT_qval) s))%Z)) 56)::(EA 52 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_temp) s) <
  (eval (EVar V_forward_DCT_qval) s))%Z)) 53)::(EA 53 AWeaken 54)::
  (EA 54 (AAssign V_forward_DCT_temp (Some (ENum (0)))) 55)::
  (EA 55 ANone 59)::(EA 56 AWeaken 57)::(EA 57 (AAssign V_forward_DCT_temp
  None) 58)::(EA 58 ANone 59)::(EA 59 (AAssign V_forward_DCT_temp
  (Some (ESub (ENum (0)) (EVar V_forward_DCT_temp)))) 60)::(EA 60 ANone 61)::
  (EA 61 ANone 62)::(EA 62 (AAssign V_forward_DCT_i
  (Some (EAdd (EVar V_forward_DCT_i) (ENum (1))))) 63)::(EA 63 ANone 64)::
  (EA 64 ANone 65)::(EA 65 (AAssign V_forward_DCT_z (Some (EAdd (ENum (1))
  (EVar V_forward_DCT_z)))) 66)::(EA 66 AWeaken 23)::(EA 67 AWeaken 68)::
  (EA 68 ANone 69)::(EA 69 (AAssign V_forward_DCT_elemr
  (Some (EAdd (EVar V_forward_DCT_elemr) (ENum (1))))) 70)::
  (EA 70 ANone 71)::(EA 71 ANone 72)::(EA 72 (AAssign V_forward_DCT_z
  (Some (EAdd (ENum (1)) (EVar V_forward_DCT_z)))) 73)::(EA 73 AWeaken 18)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_forward_DCT => Pedges_forward_DCT
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_forward_DCT => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_forward_DCT (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_z <= 0)%Z
   | 3 => (-1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_bi <= 0)%Z
   | 4 => (-1 * s V_forward_DCT_bi <= 0 /\ 1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT__tmp <= 0)%Z
   | 5 => (-1 * s V_forward_DCT__tmp <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_bi <= 0)%Z
   | 6 => (-1 * s V_forward_DCT_bi <= 0 /\ 1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT__tmp <= 0)%Z
   | 7 => (-1 * s V_forward_DCT__tmp <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_bi <= 0)%Z
   | 8 => (-1 * s V_forward_DCT_bi <= 0 /\ 1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_z <= 0)%Z
   | 9 => (-1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_bi <= 0)%Z
   | 10 => (-1 * s V_forward_DCT_bi <= 0 /\ 1 * s V_forward_DCT_bi <= 0 /\ 1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_z <= 0)%Z
   | 11 => (-1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_bi <= 0)%Z
   | 12 => (-1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT__tmp+ -1 * s V_forward_DCT_bi <= 0)%Z
   | 13 => (1 * s V_forward_DCT__tmp+ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_bi <= 0)%Z
   | 14 => (-1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0)%Z
   | 15 => (-1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_bi <= 0)%Z
   | 16 => (-1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ 1 * s V_forward_DCT_elemr <= 0 /\ -1 * s V_forward_DCT_elemr <= 0)%Z
   | 17 => (-1 * s V_forward_DCT_elemr <= 0 /\ 1 * s V_forward_DCT_elemr <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_bi <= 0)%Z
   | 18 => (-1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_elemr <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ 1 * s V_forward_DCT_elemr + -8 <= 0)%Z
   | 19 => (1 * s V_forward_DCT_elemr + -8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0)%Z
   | 20 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ 1 * s V_forward_DCT_elemr + -8 <= 0)%Z
   | 21 => (1 * s V_forward_DCT_elemr + -8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_i <= 0)%Z
   | 22 => (-1 * s V_forward_DCT_i <= 0 /\ 1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ 1 * s V_forward_DCT_elemr + -8 <= 0)%Z
   | 23 => (-1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_i + -64 <= 0)%Z
   | 24 => (1 * s V_forward_DCT_i + -64 <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i + 64 <= 0)%Z
   | 25 => (-1 * s V_forward_DCT_i + 64 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_i + -64 <= 0)%Z
   | 26 => (1 * s V_forward_DCT_i + -64 <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i + 64 <= 0)%Z
   | 27 => (-1 * s V_forward_DCT_i + 64 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_i + -64 <= 0 /\ -1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi <= 0)%Z
   | 28 => (-1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_bi + 1 <= 0 /\ 1 * s V_forward_DCT_i + -64 <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i + 64 <= 0)%Z
   | 29 => (-1 * s V_forward_DCT_i + 64 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_i + -64 <= 0 /\ -1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi <= 0)%Z
   | 30 => (-1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_bi + 1 <= 0 /\ 1 * s V_forward_DCT_i + -64 <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i + 64 <= 0)%Z
   | 31 => (-1 * s V_forward_DCT_i + 64 <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_i + -64 <= 0 /\ -1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_z + 1 <= 0)%Z
   | 32 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 33 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0)%Z
   | 34 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 35 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0)%Z
   | 36 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 37 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_temp <= 0)%Z
   | 38 => (-1 * s V_forward_DCT_temp <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 39 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0)%Z
   | 40 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 41 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_qval+ 1 * s V_forward_DCT_temp + 1 <= 0)%Z
   | 42 => (-1 * s V_forward_DCT_qval+ 1 * s V_forward_DCT_temp + 1 <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 43 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_temp <= 0 /\ -1 * s V_forward_DCT_temp <= 0)%Z
   | 44 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_qval+ -1 * s V_forward_DCT_temp <= 0)%Z
   | 45 => (1 * s V_forward_DCT_qval+ -1 * s V_forward_DCT_temp <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 46 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0)%Z
   | 47 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 48 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_temp + 1 <= 0)%Z
   | 49 => (1 * s V_forward_DCT_temp + 1 <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 50 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_temp + 1 <= 0)%Z
   | 51 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 52 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0)%Z
   | 53 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_qval+ 1 * s V_forward_DCT_temp + 1 <= 0)%Z
   | 54 => (-1 * s V_forward_DCT_qval+ 1 * s V_forward_DCT_temp + 1 <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0)%Z
   | 55 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0 /\ 1 * s V_forward_DCT_temp <= 0 /\ -1 * s V_forward_DCT_temp <= 0)%Z
   | 56 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0 /\ 1 * s V_forward_DCT_qval+ -1 * s V_forward_DCT_temp <= 0)%Z
   | 57 => (1 * s V_forward_DCT_qval+ -1 * s V_forward_DCT_temp <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0)%Z
   | 58 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 59 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0)%Z
   | 60 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 61 => (1 * s V_forward_DCT_i + -63 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0)%Z
   | 62 => (-1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_i <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_i + -63 <= 0)%Z
   | 63 => (-1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_i + 1 <= 0 /\ 1 * s V_forward_DCT_i + -64 <= 0)%Z
   | 64 => (1 * s V_forward_DCT_i + -64 <= 0 /\ -1 * s V_forward_DCT_i + 1 <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_z <= 0)%Z
   | 65 => (-1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_i + 1 <= 0 /\ 1 * s V_forward_DCT_i + -64 <= 0)%Z
   | 66 => (1 * s V_forward_DCT_i + -64 <= 0 /\ -1 * s V_forward_DCT_i + 1 <= 0 /\ -1 * s V_forward_DCT_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_z + 1 <= 0)%Z
   | 67 => (-1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_elemr <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_elemr + -7 <= 0)%Z
   | 68 => (1 * s V_forward_DCT_elemr + -7 <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT_elemr <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0)%Z
   | 69 => (-1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_elemr <= 0 /\ -1 * s V_forward_DCT_z <= 0 /\ 1 * s V_forward_DCT_elemr + -7 <= 0)%Z
   | 70 => (-1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 1 <= 0 /\ 1 * s V_forward_DCT_elemr + -8 <= 0)%Z
   | 71 => (1 * s V_forward_DCT_elemr + -8 <= 0 /\ -1 * s V_forward_DCT_elemr + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_z <= 0)%Z
   | 72 => (-1 * s V_forward_DCT_z <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT_elemr + 1 <= 0 /\ 1 * s V_forward_DCT_elemr + -8 <= 0)%Z
   | 73 => (1 * s V_forward_DCT_elemr + -8 <= 0 /\ -1 * s V_forward_DCT_elemr + 1 <= 0 /\ -1 * s V_forward_DCT_bi <= 0 /\ -1 * s V_forward_DCT__tmp+ 1 * s V_forward_DCT_bi + 1 <= 0 /\ -1 * s V_forward_DCT_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_forward_DCT (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((73 # 1) * max0(s V_forward_DCT_num_blocks) <= z)%Q
   | 2 => (s V_forward_DCT_z + (73 # 1) * max0(s V_forward_DCT_num_blocks) <= z)%Q
   | 3 => (s V_forward_DCT_z + (73 # 1) * max0(s V_forward_DCT_num_blocks) <= z)%Q
   | 4 => (s V_forward_DCT_z + (73 # 1) * max0(s V_forward_DCT_num_blocks) <= z)%Q
   | 5 => (s V_forward_DCT_z + (73 # 1) * max0(s V_forward_DCT_num_blocks) <= z)%Q
   | 6 => (s V_forward_DCT_z + (73 # 1) * max0(s V_forward_DCT_num_blocks) <= z)%Q
   | 7 => (s V_forward_DCT_z + (73 # 1) * max0(s V_forward_DCT_num_blocks) <= z)%Q
   | 8 => (s V_forward_DCT_z + (73 # 1) * max0(s V_forward_DCT__tmp) <= z)%Q
   | 9 => (s V_forward_DCT_z
           + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 10 => (s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 11 => (s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 12 => hints
     [(*-73 0*) F_max0_monotonic (F_check_ge (s V_forward_DCT__tmp
                                              - s V_forward_DCT_bi) (-1
                                                                    + 
                                                                    s V_forward_DCT__tmp
                                                                    - 
                                                                    s V_forward_DCT_bi));
      (*-73 0*) F_max0_ge_0 (-1 + s V_forward_DCT__tmp - s V_forward_DCT_bi)]
     (s V_forward_DCT_z
      + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 13 => (s V_forward_DCT_z <= z)%Q
   | 14 => (s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 15 => (s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 16 => (-s V_forward_DCT_elemr + s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 17 => (-s V_forward_DCT_elemr + s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 18 => (-s V_forward_DCT_elemr + s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 19 => (-s V_forward_DCT_elemr + s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 20 => (-s V_forward_DCT_elemr + s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 21 => (-(64 # 1) - s V_forward_DCT_elemr + s V_forward_DCT_z
            + max0(64 - s V_forward_DCT_i)
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 22 => hints
     [(*-73 0*) F_max0_pre_decrement 1 (s V_forward_DCT__tmp
                                        - s V_forward_DCT_bi) (1);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                              - s V_forward_DCT_elemr) (0))) (F_max0_ge_0 (8
                                                                    - s V_forward_DCT_elemr));
      (*-73 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_forward_DCT__tmp
                                                    - s V_forward_DCT_bi)) (F_check_ge (-1
                                                                    + s V_forward_DCT__tmp
                                                                    - s V_forward_DCT_bi) (0))]
     (-(64 # 1) - s V_forward_DCT_elemr + s V_forward_DCT_z
      + max0(64 - s V_forward_DCT_i)
      + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 23 => (-(72 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 24 => (-(72 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 25 => (-(72 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 26 => (-(72 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 27 => ((1 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 28 => ((1 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 29 => ((1 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 30 => ((1 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 31 => hints
     [(*-73 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_forward_DCT__tmp
                                                                - s V_forward_DCT_bi) (0))) (F_max0_ge_0 (s V_forward_DCT__tmp
                                                                    - s V_forward_DCT_bi));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (64 - s V_forward_DCT_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 - s V_forward_DCT_elemr)) (F_check_ge (0) (0))]
     ((73 # 1) * s V_forward_DCT__tmp - (73 # 1) * s V_forward_DCT_bi
      + s V_forward_DCT_z + max0(8 - s V_forward_DCT_elemr)
      + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 32 => (-(72 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 33 => (-(72 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 34 => (-(72 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 35 => (-(72 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 36 => (-(72 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 37 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (64 - s V_forward_DCT_i) (1)]
     (-(72 # 1) + (73 # 1) * s V_forward_DCT__tmp
      - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
      + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 38 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 39 => hints
     [(*0 1.125*) F_binom_monotonic 1 (F_max0_ge_arg (64 - s V_forward_DCT_i)) (F_check_ge (64
                                                                    - s V_forward_DCT_i) (0))]
     (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
      - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
      + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 40 => ((1 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi - (9 # 8) * s V_forward_DCT_i
            + s V_forward_DCT_z + max0(8 - s V_forward_DCT_elemr)
            + max0(63 - s V_forward_DCT_i)
            - (9 # 8) * max0(64 - s V_forward_DCT_i) <= z)%Q
   | 41 => hints
     [(*-1.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                   - 
                                                                   s V_forward_DCT_i) (0))) (F_max0_ge_0 (64
                                                                    - s V_forward_DCT_i))]
     ((1 # 1) + (73 # 1) * s V_forward_DCT__tmp
      - (73 # 1) * s V_forward_DCT_bi - (9 # 8) * s V_forward_DCT_i
      + s V_forward_DCT_z + max0(8 - s V_forward_DCT_elemr)
      + max0(63 - s V_forward_DCT_i) - (9 # 8) * max0(64 - s V_forward_DCT_i) <= z)%Q
   | 42 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 43 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 44 => hints
     [(*-1.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                                   - 
                                                                   s V_forward_DCT_i) (0))) (F_max0_ge_0 (64
                                                                    - s V_forward_DCT_i))]
     ((1 # 1) + (73 # 1) * s V_forward_DCT__tmp
      - (73 # 1) * s V_forward_DCT_bi - (9 # 8) * s V_forward_DCT_i
      + s V_forward_DCT_z + max0(8 - s V_forward_DCT_elemr)
      + max0(63 - s V_forward_DCT_i) - (9 # 8) * max0(64 - s V_forward_DCT_i) <= z)%Q
   | 45 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 46 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 47 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 48 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (64 - s V_forward_DCT_i) (1)]
     (-(72 # 1) + (73 # 1) * s V_forward_DCT__tmp
      - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
      + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 49 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 50 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 51 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 52 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 53 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 54 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 55 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 56 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 57 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 58 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 59 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 60 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 61 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 62 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(63 - s V_forward_DCT_i) <= z)%Q
   | 63 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 64 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 65 => (-(71 # 1) + (73 # 1) * s V_forward_DCT__tmp
            - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
            + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 66 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_forward_DCT_z)) (F_check_ge (-1
                                                                    + s V_forward_DCT_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              + s V_forward_DCT_z) (0))) (F_max0_ge_0 (-1
                                                                    + s V_forward_DCT_z))]
     (-(72 # 1) + (73 # 1) * s V_forward_DCT__tmp
      - (73 # 1) * s V_forward_DCT_bi + s V_forward_DCT_z
      + max0(8 - s V_forward_DCT_elemr) + max0(64 - s V_forward_DCT_i) <= z)%Q
   | 67 => (-s V_forward_DCT_elemr + s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 68 => (-s V_forward_DCT_elemr + s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 69 => (-s V_forward_DCT_elemr + s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 70 => ((1 # 1) - s V_forward_DCT_elemr + s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 71 => ((1 # 1) - s V_forward_DCT_elemr + s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 72 => ((1 # 1) - s V_forward_DCT_elemr + s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | 73 => (-s V_forward_DCT_elemr + s V_forward_DCT_z
            + (73 # 1) * max0(s V_forward_DCT__tmp - s V_forward_DCT_bi) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_forward_DCT =>
    [mkPA Q (fun n z s => ai_forward_DCT n s /\ annot0_forward_DCT n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_forward_DCT (proc_start P_forward_DCT) s1 (proc_end P_forward_DCT) s2 ->
    (s2 V_forward_DCT_z <= (73 # 1) * max0(s1 V_forward_DCT_num_blocks))%Q.
Proof.
  prove_bound ipa admissible_ipa P_forward_DCT.
Qed.
