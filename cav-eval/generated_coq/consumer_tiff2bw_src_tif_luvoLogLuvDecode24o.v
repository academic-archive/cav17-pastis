Require Import pasta.Pasta.

Inductive proc: Type :=
  P_LogLuvDecode24.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_LogLuvDecode24_z := 1%positive.
Notation V_LogLuvDecode24__tmp := 2%positive.
Notation V_LogLuvDecode24__tmp1 := 3%positive.
Notation V_LogLuvDecode24__tmp2 := 4%positive.
Notation V_LogLuvDecode24_cc := 5%positive.
Notation V_LogLuvDecode24_i := 6%positive.
Notation V_LogLuvDecode24_npixels := 7%positive.
Notation V_LogLuvDecode24_tif_dref_off744 := 8%positive.
Notation V_LogLuvDecode24_occ := 9%positive.
Notation V_LogLuvDecode24_op := 10%positive.
Notation V_LogLuvDecode24_s := 11%positive.
Notation V_LogLuvDecode24_tif := 12%positive.
Definition Pedges_LogLuvDecode24: list (edge proc) :=
  (EA 1 (AAssign V_LogLuvDecode24_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_LogLuvDecode24__tmp2 (Some (EVar V_LogLuvDecode24_occ))) 3)::
  (EA 3 (AAssign V_LogLuvDecode24__tmp (Some (EVar V_LogLuvDecode24_s))) 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_LogLuvDecode24__tmp) s) = (eval (ENum (0))
  s))%Z)) 9)::(EA 5 (AGuard (fun s => ((eval (EVar V_LogLuvDecode24__tmp)
  s) <> (eval (ENum (0)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 48)::(EA 9 AWeaken 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 ANone 15)::(EA 12 ANone 13)::(EA 13 ANone 14)::
  (EA 14 AWeaken 48)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_LogLuvDecode24_npixels None) 17)::(EA 17 AWeaken 18)::(EA 18 ANone 25)::
  (EA 18 ANone 19)::(EA 19 AWeaken 20)::(EA 20 ANone 23)::(EA 20 ANone 21)::
  (EA 21 ANone 22)::(EA 22 AWeaken 48)::(EA 23 ANone 24)::(EA 24 ANone 26)::
  (EA 25 ANone 26)::(EA 26 (AAssign V_LogLuvDecode24_cc
  (Some (EVar V_LogLuvDecode24_tif_dref_off744))) 27)::(EA 27 (AAssign
  V_LogLuvDecode24_i (Some (ENum (0)))) 28)::(EA 28 ANone 29)::
  (EA 29 AWeaken 30)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_LogLuvDecode24_i) s) <
  (eval (EVar V_LogLuvDecode24_npixels) s))%Z)) 32)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_LogLuvDecode24_i) s) >=
  (eval (EVar V_LogLuvDecode24_npixels) s))%Z)) 31)::(EA 31 AWeaken 37)::
  (EA 32 AWeaken 33)::(EA 33 ANone 34)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_LogLuvDecode24_cc) s) > (eval (ENum (0))
  s))%Z)) 49)::(EA 35 (AGuard (fun s => ((eval (EVar V_LogLuvDecode24_cc)
  s) <= (eval (ENum (0)) s))%Z)) 36)::(EA 36 AWeaken 37)::(EA 37 (AAssign
  V_LogLuvDecode24_tif_dref_off744 (Some (EVar V_LogLuvDecode24_cc))) 38)::
  (EA 38 AWeaken 39)::(EA 39 (AGuard
  (fun s => ((eval (EVar V_LogLuvDecode24_i) s) <>
  (eval (EVar V_LogLuvDecode24_npixels) s))%Z)) 44)::(EA 39 (AGuard
  (fun s => ((eval (EVar V_LogLuvDecode24_i) s) =
  (eval (EVar V_LogLuvDecode24_npixels) s))%Z)) 40)::(EA 40 AWeaken 41)::
  (EA 41 (AAssign V_LogLuvDecode24__tmp1 (Some (ENum (1)))) 42)::
  (EA 42 ANone 43)::(EA 43 AWeaken 48)::(EA 44 AWeaken 45)::(EA 45 (AAssign
  V_LogLuvDecode24__tmp1 (Some (ENum (0)))) 46)::(EA 46 ANone 47)::
  (EA 47 AWeaken 48)::(EA 49 AWeaken 50)::(EA 50 (AAssign V_LogLuvDecode24_cc
  (Some (ESub (EVar V_LogLuvDecode24_cc) (ENum (3))))) 51)::
  (EA 51 ANone 52)::(EA 52 (AAssign V_LogLuvDecode24_i
  (Some (EAdd (EVar V_LogLuvDecode24_i) (ENum (1))))) 53)::(EA 53 ANone 54)::
  (EA 54 ANone 55)::(EA 55 (AAssign V_LogLuvDecode24_z (Some (EAdd (ENum (1))
  (EVar V_LogLuvDecode24_z)))) 56)::(EA 56 AWeaken 30)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_LogLuvDecode24 => Pedges_LogLuvDecode24
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_LogLuvDecode24 => 48
     end)%positive;
  var_global := var_global
}.

Definition ai_LogLuvDecode24 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0)%Z
   | 3 => (-1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0)%Z
   | 4 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0)%Z
   | 5 => (-1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0)%Z
   | 6 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0)%Z
   | 7 => (-1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0)%Z
   | 8 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0)%Z
   | 9 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 10 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0)%Z
   | 11 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 12 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0)%Z
   | 13 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 14 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0)%Z
   | 15 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 16 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0)%Z
   | 17 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 18 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0)%Z
   | 19 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 20 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0)%Z
   | 21 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 22 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0)%Z
   | 23 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 24 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0)%Z
   | 25 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 26 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0)%Z
   | 27 => (1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 28 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_i <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0)%Z
   | 29 => (-1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 30 => (-1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 31 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_i+ 1 * s V_LogLuvDecode24_npixels <= 0)%Z
   | 32 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels + 1 <= 0)%Z
   | 33 => (1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels + 1 <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 34 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels + 1 <= 0)%Z
   | 35 => (1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels + 1 <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 36 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels + 1 <= 0 /\ 1 * s V_LogLuvDecode24_cc <= 0)%Z
   | 37 => (-1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 38 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0)%Z
   | 39 => (-1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 40 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels <= 0 /\ -1 * s V_LogLuvDecode24_i+ 1 * s V_LogLuvDecode24_npixels <= 0)%Z
   | 41 => (-1 * s V_LogLuvDecode24_i+ 1 * s V_LogLuvDecode24_npixels <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 42 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels <= 0 /\ -1 * s V_LogLuvDecode24_i+ 1 * s V_LogLuvDecode24_npixels <= 0 /\ 1 * s V_LogLuvDecode24__tmp1 + -1 <= 0 /\ -1 * s V_LogLuvDecode24__tmp1 + 1 <= 0)%Z
   | 43 => (-1 * s V_LogLuvDecode24__tmp1 + 1 <= 0 /\ 1 * s V_LogLuvDecode24__tmp1 + -1 <= 0 /\ -1 * s V_LogLuvDecode24_i+ 1 * s V_LogLuvDecode24_npixels <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 44 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0)%Z
   | 45 => (-1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 46 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp1 <= 0 /\ -1 * s V_LogLuvDecode24__tmp1 <= 0)%Z
   | 47 => (-1 * s V_LogLuvDecode24__tmp1 <= 0 /\ 1 * s V_LogLuvDecode24__tmp1 <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 48 => (-1 * s V_LogLuvDecode24_z <= 0)%Z
   | 49 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels + 1 <= 0 /\ -1 * s V_LogLuvDecode24_cc + 1 <= 0)%Z
   | 50 => (-1 * s V_LogLuvDecode24_cc + 1 <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels + 1 <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 51 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels + 1 <= 0 /\ -1 * s V_LogLuvDecode24_cc + -2 <= 0)%Z
   | 52 => (-1 * s V_LogLuvDecode24_cc + -2 <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels + 1 <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_i <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 53 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_cc + -2 <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels <= 0 /\ -1 * s V_LogLuvDecode24_i + 1 <= 0)%Z
   | 54 => (-1 * s V_LogLuvDecode24_i + 1 <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels <= 0 /\ -1 * s V_LogLuvDecode24_cc + -2 <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0)%Z
   | 55 => (-1 * s V_LogLuvDecode24__tmp <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z <= 0 /\ -1 * s V_LogLuvDecode24_cc + -2 <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels <= 0 /\ -1 * s V_LogLuvDecode24_i + 1 <= 0)%Z
   | 56 => (-1 * s V_LogLuvDecode24_i + 1 <= 0 /\ 1 * s V_LogLuvDecode24_i+ -1 * s V_LogLuvDecode24_npixels <= 0 /\ -1 * s V_LogLuvDecode24_cc + -2 <= 0 /\ 1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24__tmp <= 0 /\ -1 * s V_LogLuvDecode24_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_LogLuvDecode24 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 2 => (s V_LogLuvDecode24_z
           + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 3 => (s V_LogLuvDecode24_z
           + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 4 => (s V_LogLuvDecode24_z
           + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 5 => (s V_LogLuvDecode24_z
           + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 6 => (s V_LogLuvDecode24_z
           + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 7 => (s V_LogLuvDecode24_z
           + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 8 => hints
     [(*-0.333333 0*) F_max0_ge_0 (2 + s V_LogLuvDecode24_tif_dref_off744)]
     (s V_LogLuvDecode24_z
      + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 9 => (s V_LogLuvDecode24_z
           + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 10 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 11 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 12 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 13 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 14 => hints
     [(*-0.333333 0*) F_max0_ge_0 (2 + s V_LogLuvDecode24_tif_dref_off744)]
     (s V_LogLuvDecode24_z
      + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 15 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 16 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 17 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 18 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 19 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 20 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 21 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 22 => hints
     [(*-0.333333 0*) F_max0_ge_0 (2 + s V_LogLuvDecode24_tif_dref_off744)]
     (s V_LogLuvDecode24_z
      + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 23 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 24 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 25 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 26 => (s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_tif_dref_off744) <= z)%Q
   | 27 => (s V_LogLuvDecode24_z + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 28 => (s V_LogLuvDecode24_z + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 29 => (s V_LogLuvDecode24_z + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 30 => (s V_LogLuvDecode24_z + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 31 => hints
     [(*-0.333333 0*) F_max0_monotonic (F_check_ge (2 + s V_LogLuvDecode24_cc) (-1
                                                                    + s V_LogLuvDecode24_cc));
      (*-0.333333 0*) F_max0_ge_0 (-1 + s V_LogLuvDecode24_cc)]
     (s V_LogLuvDecode24_z + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 32 => (s V_LogLuvDecode24_z + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 33 => (s V_LogLuvDecode24_z + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 34 => (s V_LogLuvDecode24_z + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 35 => (s V_LogLuvDecode24_z + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 36 => hints
     [(*0 0.333333*) F_max0_monotonic (F_check_ge (2 + s V_LogLuvDecode24_cc) (-1
                                                                    + s V_LogLuvDecode24_cc));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                        + s V_LogLuvDecode24_cc)) (F_check_ge (0) (0))]
     (s V_LogLuvDecode24_z + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 37 => (s V_LogLuvDecode24_z <= z)%Q
   | 38 => (s V_LogLuvDecode24_z <= z)%Q
   | 39 => (s V_LogLuvDecode24_z <= z)%Q
   | 40 => (s V_LogLuvDecode24_z <= z)%Q
   | 41 => (s V_LogLuvDecode24_z <= z)%Q
   | 42 => (s V_LogLuvDecode24_z <= z)%Q
   | 43 => (s V_LogLuvDecode24_z <= z)%Q
   | 44 => (s V_LogLuvDecode24_z <= z)%Q
   | 45 => (s V_LogLuvDecode24_z <= z)%Q
   | 46 => (s V_LogLuvDecode24_z <= z)%Q
   | 47 => (s V_LogLuvDecode24_z <= z)%Q
   | 48 => (s V_LogLuvDecode24_z <= z)%Q
   | 49 => hints
     [(*-0.333333 0*) F_max0_pre_decrement 1 (2 + s V_LogLuvDecode24_cc) (3)]
     (s V_LogLuvDecode24_z + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 50 => ((1 # 1) + s V_LogLuvDecode24_z
            + (1 # 3) * max0(-1 + s V_LogLuvDecode24_cc) <= z)%Q
   | 51 => ((1 # 1) + s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 52 => ((1 # 1) + s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 53 => ((1 # 1) + s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 54 => ((1 # 1) + s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 55 => ((1 # 1) + s V_LogLuvDecode24_z
            + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | 56 => (s V_LogLuvDecode24_z + (1 # 3) * max0(2 + s V_LogLuvDecode24_cc) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_LogLuvDecode24 =>
    [mkPA Q (fun n z s => ai_LogLuvDecode24 n s /\ annot0_LogLuvDecode24 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_LogLuvDecode24 (proc_start P_LogLuvDecode24) s1 (proc_end P_LogLuvDecode24) s2 ->
    (s2 V_LogLuvDecode24_z <= (1 # 3) * max0(2
                                             + s1 V_LogLuvDecode24_tif_dref_off744))%Q.
Proof.
  prove_bound ipa admissible_ipa P_LogLuvDecode24.
Qed.
