Require Import pasta.Pasta.

Inductive proc: Type :=
  P_forward_DCT_float.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_forward_DCT_float_z := 1%positive.
Notation V_forward_DCT_float__tmp := 2%positive.
Notation V_forward_DCT_float__tmp1 := 3%positive.
Notation V_forward_DCT_float__tmp2 := 4%positive.
Notation V_forward_DCT_float_bi := 5%positive.
Notation V_forward_DCT_float_elemr := 6%positive.
Notation V_forward_DCT_float_i := 7%positive.
Notation V_forward_DCT_float_cinfo := 8%positive.
Notation V_forward_DCT_float_coef_blocks := 9%positive.
Notation V_forward_DCT_float_compptr := 10%positive.
Notation V_forward_DCT_float_num_blocks := 11%positive.
Notation V_forward_DCT_float_sample_data := 12%positive.
Notation V_forward_DCT_float_start_col := 13%positive.
Notation V_forward_DCT_float_start_row := 14%positive.
Definition Pedges_forward_DCT_float: list (edge proc) :=
  (EA 1 (AAssign V_forward_DCT_float_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_float_bi) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_forward_DCT_float__tmp)
  s) >= (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_forward_DCT_float__tmp2 (Some (EVar V_forward_DCT_float_start_row))) 6)::
  (EA 6 (AAssign V_forward_DCT_float__tmp1
  (Some (EVar V_forward_DCT_float_start_col))) 7)::(EA 7 (AAssign
  V_forward_DCT_float__tmp (Some (EVar V_forward_DCT_float_num_blocks))) 8)::
  (EA 8 (AAssign V_forward_DCT_float_bi (Some (ENum (0)))) 9)::
  (EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_float_bi) s) <
  (eval (EVar V_forward_DCT_float__tmp) s))%Z)) 14)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_float_bi) s) >=
  (eval (EVar V_forward_DCT_float__tmp) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 14 AWeaken 15)::(EA 15 (AAssign V_forward_DCT_float_elemr
  (Some (ENum (0)))) 16)::(EA 16 ANone 17)::(EA 17 AWeaken 18)::
  (EA 18 (AGuard (fun s => ((eval (EVar V_forward_DCT_float_elemr) s) <
  (eval (ENum (8)) s))%Z)) 39)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_float_elemr) s) >= (eval (ENum (8))
  s))%Z)) 19)::(EA 19 AWeaken 20)::(EA 20 (AAssign V_forward_DCT_float_i
  (Some (ENum (0)))) 21)::(EA 21 ANone 22)::(EA 22 AWeaken 23)::
  (EA 23 (AGuard (fun s => ((eval (EVar V_forward_DCT_float_i) s) <
  (eval (ENum (64)) s))%Z)) 32)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_forward_DCT_float_i) s) >= (eval (ENum (64))
  s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 ANone 26)::(EA 26 (AAssign
  V_forward_DCT_float_bi (Some (EAdd (EVar V_forward_DCT_float_bi)
  (ENum (1))))) 27)::(EA 27 (AAssign V_forward_DCT_float__tmp1
  (Some (EAdd (EVar V_forward_DCT_float__tmp1) (ENum (8))))) 28)::
  (EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign V_forward_DCT_float_z
  (Some (EAdd (ENum (1)) (EVar V_forward_DCT_float_z)))) 31)::
  (EA 31 AWeaken 11)::(EA 32 AWeaken 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_forward_DCT_float_i (Some (EAdd (EVar V_forward_DCT_float_i)
  (ENum (1))))) 35)::(EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign
  V_forward_DCT_float_z (Some (EAdd (ENum (1))
  (EVar V_forward_DCT_float_z)))) 38)::(EA 38 AWeaken 23)::
  (EA 39 AWeaken 40)::(EA 40 ANone 41)::(EA 41 (AAssign
  V_forward_DCT_float_elemr (Some (EAdd (EVar V_forward_DCT_float_elemr)
  (ENum (1))))) 42)::(EA 42 ANone 43)::(EA 43 ANone 44)::(EA 44 (AAssign
  V_forward_DCT_float_z (Some (EAdd (ENum (1))
  (EVar V_forward_DCT_float_z)))) 45)::(EA 45 AWeaken 18)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_forward_DCT_float => Pedges_forward_DCT_float
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_forward_DCT_float => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_forward_DCT_float (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_z <= 0)%Z
   | 3 => (-1 * s V_forward_DCT_float_z <= 0 /\ 1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0)%Z
   | 4 => (-1 * s V_forward_DCT_float_bi <= 0 /\ 1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float__tmp <= 0)%Z
   | 5 => (-1 * s V_forward_DCT_float__tmp <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ 1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0)%Z
   | 6 => (-1 * s V_forward_DCT_float_bi <= 0 /\ 1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float__tmp <= 0)%Z
   | 7 => (-1 * s V_forward_DCT_float__tmp <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ 1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0)%Z
   | 8 => (-1 * s V_forward_DCT_float_bi <= 0 /\ 1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_z <= 0)%Z
   | 9 => (-1 * s V_forward_DCT_float_z <= 0 /\ 1 * s V_forward_DCT_float_z <= 0 /\ 1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0)%Z
   | 10 => (-1 * s V_forward_DCT_float_bi <= 0 /\ 1 * s V_forward_DCT_float_bi <= 0 /\ 1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_z <= 0)%Z
   | 11 => (-1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0)%Z
   | 12 => (-1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ 1 * s V_forward_DCT_float__tmp+ -1 * s V_forward_DCT_float_bi <= 0)%Z
   | 13 => (1 * s V_forward_DCT_float__tmp+ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0)%Z
   | 14 => (-1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0)%Z
   | 15 => (-1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0)%Z
   | 16 => (-1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ 1 * s V_forward_DCT_float_elemr <= 0 /\ -1 * s V_forward_DCT_float_elemr <= 0)%Z
   | 17 => (-1 * s V_forward_DCT_float_elemr <= 0 /\ 1 * s V_forward_DCT_float_elemr <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0)%Z
   | 18 => (-1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_elemr <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ 1 * s V_forward_DCT_float_elemr + -8 <= 0)%Z
   | 19 => (1 * s V_forward_DCT_float_elemr + -8 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0)%Z
   | 20 => (-1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ 1 * s V_forward_DCT_float_elemr + -8 <= 0)%Z
   | 21 => (1 * s V_forward_DCT_float_elemr + -8 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_float_i <= 0 /\ -1 * s V_forward_DCT_float_i <= 0)%Z
   | 22 => (-1 * s V_forward_DCT_float_i <= 0 /\ 1 * s V_forward_DCT_float_i <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ 1 * s V_forward_DCT_float_elemr + -8 <= 0)%Z
   | 23 => (-1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_i <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_float_i + -64 <= 0)%Z
   | 24 => (1 * s V_forward_DCT_float_i + -64 <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_i + 64 <= 0)%Z
   | 25 => (-1 * s V_forward_DCT_float_i + 64 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_float_i + -64 <= 0)%Z
   | 26 => (1 * s V_forward_DCT_float_i + -64 <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_i + 64 <= 0)%Z
   | 27 => (-1 * s V_forward_DCT_float_i + 64 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_float_i + -64 <= 0 /\ -1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi <= 0)%Z
   | 28 => (-1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_bi + 1 <= 0 /\ 1 * s V_forward_DCT_float_i + -64 <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_i + 64 <= 0)%Z
   | 29 => (-1 * s V_forward_DCT_float_i + 64 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_float_i + -64 <= 0 /\ -1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi <= 0)%Z
   | 30 => (-1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_bi + 1 <= 0 /\ 1 * s V_forward_DCT_float_i + -64 <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_i + 64 <= 0)%Z
   | 31 => (-1 * s V_forward_DCT_float_i + 64 <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ 1 * s V_forward_DCT_float_i + -64 <= 0 /\ -1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_z + 1 <= 0)%Z
   | 32 => (-1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_i <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ 1 * s V_forward_DCT_float_i + -63 <= 0)%Z
   | 33 => (1 * s V_forward_DCT_float_i + -63 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_i <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0)%Z
   | 34 => (-1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_i <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ 1 * s V_forward_DCT_float_i + -63 <= 0)%Z
   | 35 => (-1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_float_i + 1 <= 0 /\ 1 * s V_forward_DCT_float_i + -64 <= 0)%Z
   | 36 => (1 * s V_forward_DCT_float_i + -64 <= 0 /\ -1 * s V_forward_DCT_float_i + 1 <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0)%Z
   | 37 => (-1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_float_i + 1 <= 0 /\ 1 * s V_forward_DCT_float_i + -64 <= 0)%Z
   | 38 => (1 * s V_forward_DCT_float_i + -64 <= 0 /\ -1 * s V_forward_DCT_float_i + 1 <= 0 /\ -1 * s V_forward_DCT_float_elemr + 8 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_z + 1 <= 0)%Z
   | 39 => (-1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_elemr <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ 1 * s V_forward_DCT_float_elemr + -7 <= 0)%Z
   | 40 => (1 * s V_forward_DCT_float_elemr + -7 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float_elemr <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0)%Z
   | 41 => (-1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_elemr <= 0 /\ -1 * s V_forward_DCT_float_z <= 0 /\ 1 * s V_forward_DCT_float_elemr + -7 <= 0)%Z
   | 42 => (-1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_elemr + 1 <= 0 /\ 1 * s V_forward_DCT_float_elemr + -8 <= 0)%Z
   | 43 => (1 * s V_forward_DCT_float_elemr + -8 <= 0 /\ -1 * s V_forward_DCT_float_elemr + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_z <= 0)%Z
   | 44 => (-1 * s V_forward_DCT_float_z <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float_elemr + 1 <= 0 /\ 1 * s V_forward_DCT_float_elemr + -8 <= 0)%Z
   | 45 => (1 * s V_forward_DCT_float_elemr + -8 <= 0 /\ -1 * s V_forward_DCT_float_elemr + 1 <= 0 /\ -1 * s V_forward_DCT_float_bi <= 0 /\ -1 * s V_forward_DCT_float__tmp+ 1 * s V_forward_DCT_float_bi + 1 <= 0 /\ -1 * s V_forward_DCT_float_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_forward_DCT_float (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((73 # 1) * max0(s V_forward_DCT_float_num_blocks) <= z)%Q
   | 2 => (s V_forward_DCT_float_z
           + (73 # 1) * max0(s V_forward_DCT_float_num_blocks) <= z)%Q
   | 3 => (s V_forward_DCT_float_z
           + (73 # 1) * max0(s V_forward_DCT_float_num_blocks) <= z)%Q
   | 4 => (s V_forward_DCT_float_z
           + (73 # 1) * max0(s V_forward_DCT_float_num_blocks) <= z)%Q
   | 5 => (s V_forward_DCT_float_z
           + (73 # 1) * max0(s V_forward_DCT_float_num_blocks) <= z)%Q
   | 6 => (s V_forward_DCT_float_z
           + (73 # 1) * max0(s V_forward_DCT_float_num_blocks) <= z)%Q
   | 7 => (s V_forward_DCT_float_z
           + (73 # 1) * max0(s V_forward_DCT_float_num_blocks) <= z)%Q
   | 8 => (s V_forward_DCT_float_z
           + (73 # 1) * max0(s V_forward_DCT_float__tmp) <= z)%Q
   | 9 => (s V_forward_DCT_float_z
           + (73 # 1) * max0(s V_forward_DCT_float__tmp
                             - s V_forward_DCT_float_bi) <= z)%Q
   | 10 => (s V_forward_DCT_float_z
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 11 => (s V_forward_DCT_float_z
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 12 => hints
     [(*-73 0*) F_max0_monotonic (F_check_ge (s V_forward_DCT_float__tmp
                                              - s V_forward_DCT_float_bi) (-1
                                                                    + s V_forward_DCT_float__tmp
                                                                    - s V_forward_DCT_float_bi));
      (*-73 0*) F_max0_ge_0 (-1 + s V_forward_DCT_float__tmp
                             - s V_forward_DCT_float_bi)]
     (s V_forward_DCT_float_z
      + (73 # 1) * max0(s V_forward_DCT_float__tmp - s V_forward_DCT_float_bi) <= z)%Q
   | 13 => (s V_forward_DCT_float_z <= z)%Q
   | 14 => hints
     [(*0 73*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_forward_DCT_float__tmp
                                                   - s V_forward_DCT_float_bi)) (F_check_ge (-1
                                                                    + s V_forward_DCT_float__tmp
                                                                    - s V_forward_DCT_float_bi) (0))]
     (s V_forward_DCT_float_z
      + (73 # 1) * max0(s V_forward_DCT_float__tmp - s V_forward_DCT_float_bi) <= z)%Q
   | 15 => (-(73 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 16 => (-(73 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 17 => (-(73 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 18 => (-(73 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 19 => (-(73 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 20 => (-(73 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 21 => (-(73 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr - s V_forward_DCT_float_i
            + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 22 => hints
     [(*-73 0*) F_max0_pre_decrement 1 (s V_forward_DCT_float__tmp
                                        - s V_forward_DCT_float_bi) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_forward_DCT_float_z) (0))) (F_max0_ge_0 (s V_forward_DCT_float_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                               - s V_forward_DCT_float_elemr) (0))) (F_max0_ge_0 (8
                                                                    - s V_forward_DCT_float_elemr));
      (*-73 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                + s V_forward_DCT_float__tmp
                                                                - s V_forward_DCT_float_bi) (0))) (F_max0_ge_0 (-1
                                                                    + s V_forward_DCT_float__tmp
                                                                    - s V_forward_DCT_float_bi))]
     (-(73 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
      - (73 # 1) * s V_forward_DCT_float_bi - s V_forward_DCT_float_elemr
      - s V_forward_DCT_float_i + s V_forward_DCT_float_z
      - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                        - s V_forward_DCT_float_bi)
      + (73 # 1) * max0(s V_forward_DCT_float__tmp - s V_forward_DCT_float_bi) <= z)%Q
   | 23 => ((65 # 1) - s V_forward_DCT_float_i
            + (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(8 - s V_forward_DCT_float_elemr)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 24 => ((65 # 1) - s V_forward_DCT_float_i
            + (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(8 - s V_forward_DCT_float_elemr)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 25 => ((65 # 1) - s V_forward_DCT_float_i
            + (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(8 - s V_forward_DCT_float_elemr)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 26 => ((65 # 1) - s V_forward_DCT_float_i
            + (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(8 - s V_forward_DCT_float_elemr)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 27 => ((65 # 1) - s V_forward_DCT_float_i
            + max0(8 - s V_forward_DCT_float_elemr)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 28 => ((65 # 1) - s V_forward_DCT_float_i
            + max0(8 - s V_forward_DCT_float_elemr)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 29 => ((65 # 1) - s V_forward_DCT_float_i
            + max0(8 - s V_forward_DCT_float_elemr)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 30 => ((65 # 1) - s V_forward_DCT_float_i
            + max0(8 - s V_forward_DCT_float_elemr)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (64 - s V_forward_DCT_float_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                               - s V_forward_DCT_float_i) (0))) (F_max0_ge_0 (64
                                                                    - s V_forward_DCT_float_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                 - s V_forward_DCT_float_elemr)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                  + s V_forward_DCT_float_z)) (F_check_ge (-1
                                                                    + s V_forward_DCT_float_z) (0))]
     ((65 # 1) - s V_forward_DCT_float_i + max0(-1 + s V_forward_DCT_float_z)
      + max0(8 - s V_forward_DCT_float_elemr)
      + (73 # 1) * max0(s V_forward_DCT_float__tmp - s V_forward_DCT_float_bi) <= z)%Q
   | 32 => ((65 # 1) - s V_forward_DCT_float_i
            + (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(8 - s V_forward_DCT_float_elemr)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 33 => ((65 # 1) - s V_forward_DCT_float_i
            + (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(8 - s V_forward_DCT_float_elemr)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 34 => ((65 # 1) - s V_forward_DCT_float_i
            + (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(8 - s V_forward_DCT_float_elemr)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 35 => ((66 # 1) - s V_forward_DCT_float_i
            + (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(8 - s V_forward_DCT_float_elemr)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 36 => ((66 # 1) - s V_forward_DCT_float_i
            + (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(8 - s V_forward_DCT_float_elemr)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 37 => ((66 # 1) - s V_forward_DCT_float_i
            + (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + max0(8 - s V_forward_DCT_float_elemr)
            + max0(s V_forward_DCT_float_z) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_forward_DCT_float_z) (0))) (F_max0_ge_0 (s V_forward_DCT_float_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                  + s V_forward_DCT_float_z)) (F_check_ge (-1
                                                                    + s V_forward_DCT_float_z) (0))]
     ((66 # 1) - s V_forward_DCT_float_i
      + (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                        - s V_forward_DCT_float_bi)
      + max0(-1 + s V_forward_DCT_float_z)
      + max0(8 - s V_forward_DCT_float_elemr) <= z)%Q
   | 39 => (-(73 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 40 => (-(73 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 41 => (-(73 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 42 => (-(72 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 43 => (-(72 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 44 => (-(72 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | 45 => (-(73 # 1) + (73 # 1) * s V_forward_DCT_float__tmp
            - (73 # 1) * s V_forward_DCT_float_bi
            - s V_forward_DCT_float_elemr + s V_forward_DCT_float_z
            - (73 # 1) * max0(-1 + s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi)
            + (73 # 1) * max0(s V_forward_DCT_float__tmp
                              - s V_forward_DCT_float_bi) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_forward_DCT_float =>
    [mkPA Q (fun n z s => ai_forward_DCT_float n s /\ annot0_forward_DCT_float n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_forward_DCT_float (proc_start P_forward_DCT_float) s1 (proc_end P_forward_DCT_float) s2 ->
    (s2 V_forward_DCT_float_z <= (73 # 1) * max0(s1 V_forward_DCT_float_num_blocks))%Q.
Proof.
  prove_bound ipa admissible_ipa P_forward_DCT_float.
Qed.
