Require Import pasta.Pasta.

Inductive proc: Type :=
  P_susan_principle_small.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_susan_principle_small_z := 1%positive.
Notation V_susan_principle_small__tmp := 2%positive.
Notation V_susan_principle_small__tmp1 := 3%positive.
Notation V_susan_principle_small__tmp2 := 4%positive.
Notation V_susan_principle_small_i := 5%positive.
Notation V_susan_principle_small_j := 6%positive.
Notation V_susan_principle_small_n := 7%positive.
Notation V_susan_principle_small_bp := 8%positive.
Notation V_susan_principle_small_in := 9%positive.
Notation V_susan_principle_small_max_no := 10%positive.
Notation V_susan_principle_small_r := 11%positive.
Notation V_susan_principle_small_x_size := 12%positive.
Notation V_susan_principle_small_y_size := 13%positive.
Definition Pedges_susan_principle_small: list (edge proc) :=
  (EA 1 (AAssign V_susan_principle_small_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_susan_principle_small__tmp2
  (Some (EVar V_susan_principle_small_max_no))) 3)::(EA 3 (AAssign
  V_susan_principle_small__tmp1
  (Some (EVar V_susan_principle_small_x_size))) 4)::(EA 4 (AAssign
  V_susan_principle_small__tmp
  (Some (EVar V_susan_principle_small_y_size))) 5)::(EA 5 (AAssign
  V_susan_principle_small__tmp2 (Some (ENum (730)))) 6)::(EA 6 (AAssign
  V_susan_principle_small_i (Some (ENum (1)))) 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_susan_principle_small_i) s) <
  (eval (ESub (EVar V_susan_principle_small__tmp) (ENum (1))) s))%Z)) 12)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_susan_principle_small_i) s) >=
  (eval (ESub (EVar V_susan_principle_small__tmp) (ENum (1))) s))%Z)) 10)::
  (EA 10 AWeaken 11)::(EA 12 AWeaken 13)::(EA 13 (AAssign
  V_susan_principle_small_j (Some (ENum (1)))) 14)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_susan_principle_small_j) s) <
  (eval (ESub (EVar V_susan_principle_small__tmp1) (ENum (1))) s))%Z)) 24)::
  (EA 16 (AGuard (fun s => ((eval (EVar V_susan_principle_small_j) s) >=
  (eval (ESub (EVar V_susan_principle_small__tmp1) (ENum (1))) s))%Z)) 17)::
  (EA 17 AWeaken 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_susan_principle_small_i (Some (EAdd (EVar V_susan_principle_small_i)
  (ENum (1))))) 20)::(EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_susan_principle_small_z (Some (EAdd (ENum (1))
  (EVar V_susan_principle_small_z)))) 23)::(EA 23 AWeaken 9)::
  (EA 24 AWeaken 25)::(EA 25 (AAssign V_susan_principle_small_n
  (Some (ENum (100)))) 26)::(EA 26 (AAssign V_susan_principle_small_n
  None) 27)::(EA 27 (AAssign V_susan_principle_small_n None) 28)::
  (EA 28 (AAssign V_susan_principle_small_n None) 29)::(EA 29 (AAssign
  V_susan_principle_small_n None) 30)::(EA 30 (AAssign
  V_susan_principle_small_n None) 31)::(EA 31 (AAssign
  V_susan_principle_small_n None) 32)::(EA 32 (AAssign
  V_susan_principle_small_n None) 33)::(EA 33 (AAssign
  V_susan_principle_small_n None) 34)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_susan_principle_small_n) s) <=
  (eval (EVar V_susan_principle_small__tmp2) s))%Z)) 37)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_susan_principle_small_n) s) >
  (eval (EVar V_susan_principle_small__tmp2) s))%Z)) 36)::
  (EA 36 AWeaken 39)::(EA 37 AWeaken 38)::(EA 38 ANone 39)::
  (EA 39 ANone 40)::(EA 40 (AAssign V_susan_principle_small_j
  (Some (EAdd (EVar V_susan_principle_small_j) (ENum (1))))) 41)::
  (EA 41 ANone 42)::(EA 42 ANone 43)::(EA 43 (AAssign
  V_susan_principle_small_z (Some (EAdd (ENum (1))
  (EVar V_susan_principle_small_z)))) 44)::(EA 44 AWeaken 16)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_susan_principle_small => Pedges_susan_principle_small
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_susan_principle_small => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_susan_principle_small (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_z <= 0)%Z
   | 3 => (-1 * s V_susan_principle_small_z <= 0 /\ 1 * s V_susan_principle_small_z <= 0)%Z
   | 4 => (1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_z <= 0)%Z
   | 5 => (-1 * s V_susan_principle_small_z <= 0 /\ 1 * s V_susan_principle_small_z <= 0)%Z
   | 6 => (1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0)%Z
   | 7 => (-1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ 1 * s V_susan_principle_small_z <= 0 /\ 1 * s V_susan_principle_small_i + -1 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 8 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small_i + -1 <= 0 /\ 1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0)%Z
   | 9 => (-1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0)%Z
   | 10 => (-1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ 1 * s V_susan_principle_small__tmp+ -1 * s V_susan_principle_small_i + -1 <= 0)%Z
   | 11 => (1 * s V_susan_principle_small__tmp+ -1 * s V_susan_principle_small_i + -1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0)%Z
   | 12 => (-1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0)%Z
   | 13 => (-1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0)%Z
   | 14 => (-1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ 1 * s V_susan_principle_small_j + -1 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0)%Z
   | 15 => (-1 * s V_susan_principle_small_j + 1 <= 0 /\ 1 * s V_susan_principle_small_j + -1 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0)%Z
   | 16 => (-1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 17 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ 1 * s V_susan_principle_small__tmp1+ -1 * s V_susan_principle_small_j + -1 <= 0)%Z
   | 18 => (1 * s V_susan_principle_small__tmp1+ -1 * s V_susan_principle_small_j + -1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 19 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ 1 * s V_susan_principle_small__tmp1+ -1 * s V_susan_principle_small_j + -1 <= 0)%Z
   | 20 => (1 * s V_susan_principle_small__tmp1+ -1 * s V_susan_principle_small_j + -1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 21 => (-1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 1 <= 0 /\ -1 * s V_susan_principle_small_i + 2 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ 1 * s V_susan_principle_small__tmp1+ -1 * s V_susan_principle_small_j + -1 <= 0)%Z
   | 22 => (1 * s V_susan_principle_small__tmp1+ -1 * s V_susan_principle_small_j + -1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 23 => (-1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 1 <= 0 /\ -1 * s V_susan_principle_small_i + 2 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp1+ -1 * s V_susan_principle_small_j + -1 <= 0 /\ -1 * s V_susan_principle_small_z + 1 <= 0)%Z
   | 24 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0)%Z
   | 25 => (-1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 26 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0 /\ 1 * s V_susan_principle_small_n + -100 <= 0 /\ -1 * s V_susan_principle_small_n + 100 <= 0)%Z
   | 27 => (-1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 28 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0)%Z
   | 29 => (-1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 30 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0)%Z
   | 31 => (-1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 32 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0)%Z
   | 33 => (-1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 34 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0)%Z
   | 35 => (-1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 36 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0 /\ 1 * s V_susan_principle_small__tmp2+ -1 * s V_susan_principle_small_n + 1 <= 0)%Z
   | 37 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2+ 1 * s V_susan_principle_small_n <= 0)%Z
   | 38 => (-1 * s V_susan_principle_small__tmp2+ 1 * s V_susan_principle_small_n <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 39 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0)%Z
   | 40 => (-1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 2 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 41 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_j + 2 <= 0)%Z
   | 42 => (-1 * s V_susan_principle_small_j + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0)%Z
   | 43 => (-1 * s V_susan_principle_small_i + 1 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small_z <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small_j + 2 <= 0)%Z
   | 44 => (-1 * s V_susan_principle_small_j + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp1+ 1 * s V_susan_principle_small_j + 1 <= 0 /\ -1 * s V_susan_principle_small__tmp+ 1 * s V_susan_principle_small_i + 2 <= 0 /\ -1 * s V_susan_principle_small__tmp2 + 730 <= 0 /\ 1 * s V_susan_principle_small__tmp2 + -730 <= 0 /\ -1 * s V_susan_principle_small_i + 1 <= 0 /\ -1 * s V_susan_principle_small_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_susan_principle_small (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-2 + s V_susan_principle_small_x_size) * max0(-2
                                                              + s V_susan_principle_small_y_size)
           + max0(-2 + s V_susan_principle_small_y_size) <= z)%Q
   | 2 => (s V_susan_principle_small_z
           + max0(-2 + s V_susan_principle_small_x_size) * max0(-2
                                                                + s V_susan_principle_small_y_size)
           + max0(-2 + s V_susan_principle_small_y_size) <= z)%Q
   | 3 => (s V_susan_principle_small_z
           + max0(-2 + s V_susan_principle_small_x_size) * max0(-2
                                                                + s V_susan_principle_small_y_size)
           + max0(-2 + s V_susan_principle_small_y_size) <= z)%Q
   | 4 => (s V_susan_principle_small_z
           + max0(-2 + s V_susan_principle_small__tmp1) * max0(-2
                                                               + s V_susan_principle_small_y_size)
           + max0(-2 + s V_susan_principle_small_y_size) <= z)%Q
   | 5 => (s V_susan_principle_small_z
           + max0(-2 + s V_susan_principle_small__tmp)
           + max0(-2 + s V_susan_principle_small__tmp) * max0(-2
                                                              + s V_susan_principle_small__tmp1) <= z)%Q
   | 6 => ((0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                            + s V_susan_principle_small__tmp)
           + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                              + s V_susan_principle_small__tmp1
                                                              - s V_susan_principle_small_j)
           - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                              + s V_susan_principle_small__tmp1
                                                              - s V_susan_principle_small_j)
           + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                              - s V_susan_principle_small_n)
           - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                              - s V_susan_principle_small_n)
           + s V_susan_principle_small_z
           + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small__tmp2)
           - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
           + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
           + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
           - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
           - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
           - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
           + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
           - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
           + max0(-2 + s V_susan_principle_small__tmp) * max0(-2
                                                              + s V_susan_principle_small__tmp1)
           - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j)
           + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j)
           + (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                            - s V_susan_principle_small_j) * max0(730
                                                                  - s V_susan_principle_small__tmp2) <= z)%Q
   | 7 => ((0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                            + s V_susan_principle_small__tmp1
                                                            - s V_susan_principle_small_j)
           + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                              + s V_susan_principle_small__tmp
                                                              - s V_susan_principle_small_i)
           - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                              + s V_susan_principle_small__tmp1
                                                              - s V_susan_principle_small_j)
           + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                              - s V_susan_principle_small_n)
           - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                              - s V_susan_principle_small_n)
           + s V_susan_principle_small_z
           + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small__tmp2)
           - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
           + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
           + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
           - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
           - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
           - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
           + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
           - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
           + max0(-2 + s V_susan_principle_small__tmp1) * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
           - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j)
           + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j)
           + (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                            - s V_susan_principle_small_j) * max0(730
                                                                  - s V_susan_principle_small__tmp2) <= z)%Q
   | 8 => ((0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                            + s V_susan_principle_small__tmp1
                                                            - s V_susan_principle_small_j)
           + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                              + s V_susan_principle_small__tmp
                                                              - s V_susan_principle_small_i)
           - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                              + s V_susan_principle_small__tmp1
                                                              - s V_susan_principle_small_j)
           + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                              - s V_susan_principle_small_n)
           - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                              - s V_susan_principle_small_n)
           + s V_susan_principle_small_z
           + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small__tmp2)
           - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
           + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
           + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
           - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
           - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
           - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
           + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
           - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
           + max0(-2 + s V_susan_principle_small__tmp1) * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
           - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j)
           + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j)
           + (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                            - s V_susan_principle_small_j) * max0(730
                                                                  - s V_susan_principle_small__tmp2) <= z)%Q
   | 9 => ((0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                            + s V_susan_principle_small__tmp1
                                                            - s V_susan_principle_small_j)
           + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                              + s V_susan_principle_small__tmp
                                                              - s V_susan_principle_small_i)
           - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                              + s V_susan_principle_small__tmp1
                                                              - s V_susan_principle_small_j)
           + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                              - s V_susan_principle_small_n)
           - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                              - s V_susan_principle_small_n)
           + s V_susan_principle_small_z
           + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small__tmp2)
           - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
           + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
           + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
           - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
           - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
           - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
           + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
           - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
           + max0(-2 + s V_susan_principle_small__tmp1) * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
           - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j)
           + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j)
           + (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                            - s V_susan_principle_small_j) * max0(730
                                                                  - s V_susan_principle_small__tmp2) <= z)%Q
   | 10 => hints
     [(*-0.136986 0*) F_max0_monotonic (F_check_ge (-1
                                                    + s V_susan_principle_small__tmp
                                                    - s V_susan_principle_small_i) (-2
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i));
      (*-0.136986 0*) F_max0_ge_0 (-2 + s V_susan_principle_small__tmp
                                   - s V_susan_principle_small_i);
      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.000641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-0.00136986 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i)) (F_check_ge (0) (0)));
      (*-3.53703e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-3.53703e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-0.00136986 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + 
                                                                    s V_susan_principle_small__tmp2)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                            + s V_susan_principle_small__tmp1)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i)) (F_check_ge (0) (0)));
      (*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + 
                                                                    s V_susan_principle_small__tmp1
                                                                    - 
                                                                    s V_susan_principle_small_j)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-8.78117e-05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)))]
     ((0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                       + s V_susan_principle_small__tmp1
                                                       - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                         - s V_susan_principle_small_n)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                         - s V_susan_principle_small_n)
      + s V_susan_principle_small_z
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small__tmp2)
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                  - s V_susan_principle_small__tmp2)
      + max0(-2 + s V_susan_principle_small__tmp1) * max0(-1
                                                          + s V_susan_principle_small__tmp
                                                          - s V_susan_principle_small_i)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      + (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                       - s V_susan_principle_small_j) * max0(730
                                                             - s V_susan_principle_small__tmp2) <= z)%Q
   | 11 => (s V_susan_principle_small_z <= z)%Q
   | 12 => hints
     [(*-0.000641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + 
                                                                    s V_susan_principle_small__tmp1
                                                                    - 
                                                                    s V_susan_principle_small_j)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-8.78117e-05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)))]
     ((0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                       + s V_susan_principle_small__tmp1
                                                       - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                         - s V_susan_principle_small_n)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                         - s V_susan_principle_small_n)
      + s V_susan_principle_small_z
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small__tmp2)
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                  - s V_susan_principle_small__tmp2)
      + max0(-2 + s V_susan_principle_small__tmp1) * max0(-1
                                                          + s V_susan_principle_small__tmp
                                                          - s V_susan_principle_small_i)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      + (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                       - s V_susan_principle_small_j) * max0(730
                                                             - s V_susan_principle_small__tmp2) <= z)%Q
   | 13 => ((0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                             + s V_susan_principle_small__tmp
                                                             - s V_susan_principle_small_i)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                               - s V_susan_principle_small_n)
            + s V_susan_principle_small_z
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + max0(-2 + s V_susan_principle_small__tmp1) * max0(-1
                                                                + s V_susan_principle_small__tmp
                                                                - s V_susan_principle_small_i) <= z)%Q
   | 14 => (-(0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                              + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                               - s V_susan_principle_small_n)
            + s V_susan_principle_small_z
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j) <= z)%Q
   | 15 => hints
     [(*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)))]
     (-(0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                        + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                         - s V_susan_principle_small_n)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                         - s V_susan_principle_small_n)
      + s V_susan_principle_small_z
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small__tmp2)
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                  - s V_susan_principle_small__tmp2)
      + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
      + max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-2
                                                   + s V_susan_principle_small__tmp1)
      - max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-1
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      + max0(-1 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-1
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j) <= z)%Q
   | 16 => (-(0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                              + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                               - s V_susan_principle_small_n)
            + s V_susan_principle_small_z
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j) <= z)%Q
   | 17 => hints
     [(*0 0.136986*) F_max0_pre_decrement 1 (-1
                                             + s V_susan_principle_small__tmp
                                             - s V_susan_principle_small_i) (1);
      (*-0.00136986 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i)) (F_check_ge (0) (0)));
      (*0 0.000946276*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-0.00136986 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i)) (F_check_ge (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                              + s V_susan_principle_small__tmp
                                                              - s V_susan_principle_small_i)) (F_check_ge (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*0 0.000217439*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)))]
     (-(0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                        + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                         - s V_susan_principle_small_n)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                         - s V_susan_principle_small_n)
      + s V_susan_principle_small_z
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                  - s V_susan_principle_small__tmp2)
      + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
      + max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-2
                                                   + s V_susan_principle_small__tmp1)
      - max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-1
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      + max0(-1 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-1
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j) <= z)%Q
   | 18 => ((10 # 73)
            + (0 # 1) * s V_susan_principle_small__tmp * max0(-100
                                                              + s V_susan_principle_small__tmp2)
            + s V_susan_principle_small__tmp * max0(-1
                                                    + s V_susan_principle_small__tmp1
                                                    - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small_i * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - s V_susan_principle_small_i * max0(-1
                                                 + s V_susan_principle_small__tmp1
                                                 - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + (10 # 73) * max0(-2 + s V_susan_principle_small__tmp
                               - s V_susan_principle_small_i)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            - max0(-1 + s V_susan_principle_small__tmp1
                   - s V_susan_principle_small_j)
            + (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j) * max0(730
                                                                   - 
                                                                   s V_susan_principle_small__tmp2) <= z)%Q
   | 19 => ((10 # 73)
            + (0 # 1) * s V_susan_principle_small__tmp * max0(-100
                                                              + s V_susan_principle_small__tmp2)
            + s V_susan_principle_small__tmp * max0(-1
                                                    + s V_susan_principle_small__tmp1
                                                    - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small_i * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - s V_susan_principle_small_i * max0(-1
                                                 + s V_susan_principle_small__tmp1
                                                 - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + (10 # 73) * max0(-2 + s V_susan_principle_small__tmp
                               - s V_susan_principle_small_i)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            - max0(-1 + s V_susan_principle_small__tmp1
                   - s V_susan_principle_small_j)
            + (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j) * max0(730
                                                                   - 
                                                                   s V_susan_principle_small__tmp2) <= z)%Q
   | 20 => ((10 # 73)
            + (0 # 1) * s V_susan_principle_small__tmp * max0(-100
                                                              + s V_susan_principle_small__tmp2)
            + s V_susan_principle_small__tmp * max0(-1
                                                    + s V_susan_principle_small__tmp1
                                                    - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small_i * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - s V_susan_principle_small_i * max0(-1
                                                 + s V_susan_principle_small__tmp1
                                                 - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp1) * max0(-1
                                                                + s V_susan_principle_small__tmp
                                                                - s V_susan_principle_small_i)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + (10 # 73) * max0(-1 + s V_susan_principle_small__tmp
                               - s V_susan_principle_small_i)
            - max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            + (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j) * max0(730
                                                                   - 
                                                                   s V_susan_principle_small__tmp2) <= z)%Q
   | 21 => ((10 # 73)
            + (0 # 1) * s V_susan_principle_small__tmp * max0(-100
                                                              + s V_susan_principle_small__tmp2)
            + s V_susan_principle_small__tmp * max0(-1
                                                    + s V_susan_principle_small__tmp1
                                                    - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small_i * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - s V_susan_principle_small_i * max0(-1
                                                 + s V_susan_principle_small__tmp1
                                                 - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp1) * max0(-1
                                                                + s V_susan_principle_small__tmp
                                                                - s V_susan_principle_small_i)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + (10 # 73) * max0(-1 + s V_susan_principle_small__tmp
                               - s V_susan_principle_small_i)
            - max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            + (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j) * max0(730
                                                                   - 
                                                                   s V_susan_principle_small__tmp2) <= z)%Q
   | 22 => ((10 # 73)
            + (0 # 1) * s V_susan_principle_small__tmp * max0(-100
                                                              + s V_susan_principle_small__tmp2)
            + s V_susan_principle_small__tmp * max0(-1
                                                    + s V_susan_principle_small__tmp1
                                                    - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                               - s V_susan_principle_small_n)
            - (0 # 1) * s V_susan_principle_small_i * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - s V_susan_principle_small_i * max0(-1
                                                 + s V_susan_principle_small__tmp1
                                                 - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp1) * max0(-1
                                                                + s V_susan_principle_small__tmp
                                                                - s V_susan_principle_small_i)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + (10 # 73) * max0(-1 + s V_susan_principle_small__tmp
                               - s V_susan_principle_small_i)
            - max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            + (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j) * max0(730
                                                                   - 
                                                                   s V_susan_principle_small__tmp2) <= z)%Q
   | 23 => hints
     [(*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.000641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_susan_principle_small__tmp1)) (F_check_ge (0) (0)));
      (*-0.000641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_small__tmp1)) (F_check_ge (0) (0)));
      (*-0.00136986 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i)) (F_check_ge (0) (0)));
      (*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.00136986 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + 
                                                                    s V_susan_principle_small__tmp1
                                                                    - 
                                                                    s V_susan_principle_small_j)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-2.17439e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-0.00136986 0*) F_binom_monotonic 1 (F_max0_ge_arg (730
                                                            - s V_susan_principle_small__tmp2)) (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0));
      (*-0.00136986 0*) F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                            + s V_susan_principle_small__tmp2)) (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))]
     (-(63 # 73)
      + (0 # 1) * s V_susan_principle_small__tmp * max0(-100
                                                        + s V_susan_principle_small__tmp2)
      + s V_susan_principle_small__tmp * max0(-1
                                              + s V_susan_principle_small__tmp1
                                              - s V_susan_principle_small_j)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                         + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                         - s V_susan_principle_small_n)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                         - s V_susan_principle_small_n)
      - (0 # 1) * s V_susan_principle_small_i * max0(-100
                                                     + s V_susan_principle_small__tmp2)
      - s V_susan_principle_small_i * max0(-1
                                           + s V_susan_principle_small__tmp1
                                           - s V_susan_principle_small_j)
      + s V_susan_principle_small_z
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                  - s V_susan_principle_small__tmp2)
      + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
      + max0(-2 + s V_susan_principle_small__tmp1) * max0(-1
                                                          + s V_susan_principle_small__tmp
                                                          - s V_susan_principle_small_i)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      + (10 # 73) * max0(-1 + s V_susan_principle_small__tmp
                         - s V_susan_principle_small_i)
      - max0(-1 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-1
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      + (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                       - s V_susan_principle_small_j) * max0(730
                                                             - s V_susan_principle_small__tmp2) <= z)%Q
   | 24 => hints
     [(*-0.192308 0*) F_max0_pre_decrement 1 (-1
                                              + s V_susan_principle_small__tmp1
                                              - s V_susan_principle_small_j) (1);
      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*0 0.0015873*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + 
                                                                    s V_susan_principle_small__tmp2)) (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_small_z)) (F_check_ge (0) (0)));
      (*-0.000641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-3.53703e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*0 0.0015873*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_small_z)) (F_check_ge (0) (0)));
      (*-3.53703e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*0 0.0015873*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_susan_principle_small_z) (0))) (F_max0_ge_0 (s V_susan_principle_small_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)))]
     (-(0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                        + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                         - s V_susan_principle_small_n)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(s V_susan_principle_small__tmp2
                                                         - s V_susan_principle_small_n)
      + s V_susan_principle_small_z
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                  - s V_susan_principle_small__tmp2)
      + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
      + max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-2
                                                   + s V_susan_principle_small__tmp1)
      - max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-1
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      + max0(-1 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-1
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j) <= z)%Q
   | 25 => ((5 # 26)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small_z)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 39) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            - max0(s V_susan_principle_small_z) <= z)%Q
   | 26 => ((5 # 26)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small_z)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 39) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            - max0(s V_susan_principle_small_z) <= z)%Q
   | 27 => ((5 # 26)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small_z)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 39) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            - max0(s V_susan_principle_small_z) <= z)%Q
   | 28 => ((5 # 26)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small_z)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 39) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            - max0(s V_susan_principle_small_z) <= z)%Q
   | 29 => ((5 # 26)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small_z)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 39) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            - max0(s V_susan_principle_small_z) <= z)%Q
   | 30 => ((5 # 26)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small_z)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 39) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            - max0(s V_susan_principle_small_z) <= z)%Q
   | 31 => ((5 # 26)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small_z)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 39) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            - max0(s V_susan_principle_small_z) <= z)%Q
   | 32 => ((5 # 26)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small_z)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 39) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            - max0(s V_susan_principle_small_z) <= z)%Q
   | 33 => ((5 # 26)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + s V_susan_principle_small_z
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small_z)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 39) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            - max0(s V_susan_principle_small_z) <= z)%Q
   | 34 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i) (0))) (F_max0_ge_0 (-2
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                              + s V_susan_principle_small__tmp
                                                              - s V_susan_principle_small_i)) (F_check_ge (-2
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                             + s V_susan_principle_small__tmp
                                                             - s V_susan_principle_small_i)) (F_check_ge (-1
                                                                    + s V_susan_principle_small__tmp
                                                                    - s V_susan_principle_small_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*0 0.00128205*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*0 0.0015873*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_susan_principle_small_z)) (F_check_ge (s V_susan_principle_small_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_susan_principle_small_z) (0))) (F_max0_ge_0 (s V_susan_principle_small_z))]
     ((5 # 26)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                         + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + s V_susan_principle_small_z
      - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                     + s V_susan_principle_small__tmp2)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small_z)
      + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
      + max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-2
                                                   + s V_susan_principle_small__tmp1)
      - max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-1
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
      + (5 # 39) * max0(-2 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      + max0(-1 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-1
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      - (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      - max0(s V_susan_principle_small_z) <= z)%Q
   | 35 => ((5 # 26)
            + (0 # 1) * s V_susan_principle_small__tmp1 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp1 * max0(730
                                                               - s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small_j * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_j * max0(730
                                                           - s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_z * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-2 + s V_susan_principle_small__tmp1
                   - s V_susan_principle_small_j) * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
            - (0 # 1) * max0(-2 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j) * max0(730
                                                                   - 
                                                                   s V_susan_principle_small__tmp2)
            + (73 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                               - s V_susan_principle_small_j)
            - (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                             - s V_susan_principle_small_j) * max0(730
                                                                   - 
                                                                   s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(730 - s V_susan_principle_small__tmp2) <= z)%Q
   | 36 => hints
     [(*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-731
                                                                    + s V_susan_principle_small_n) (0))) (F_max0_ge_0 (-731
                                                                    + s V_susan_principle_small_n))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-731
                                                                    + s V_susan_principle_small_n) (0))) (F_max0_ge_0 (-731
                                                                    + s V_susan_principle_small_n))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-731
                                                                    + s V_susan_principle_small_n) (0))) (F_max0_ge_0 (-731
                                                                    + s V_susan_principle_small_n))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-731
                                                                    + s V_susan_principle_small_n)) (F_check_ge (-731
                                                                    + s V_susan_principle_small_n) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-731
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-1.5448e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-1.5448e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + 
                                                                    s V_susan_principle_small__tmp2)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-2.035e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-4.84523e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-731
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small_n) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small_n))) (F_binom_monotonic 1 (F_max0_ge_0 (-731
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-2.02933e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small_n)) (F_check_ge (-100
                                                                    + s V_susan_principle_small_n) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small_n)) (F_check_ge (-100
                                                                    + s V_susan_principle_small_n) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small_n)) (F_check_ge (-100
                                                                    + s V_susan_principle_small_n) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j) (0))) (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j) (0))) (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-2.02933e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n) (0))) (F_max0_ge_0 (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n) (0))) (F_max0_ge_0 (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-4.83756e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n)) (F_check_ge (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-731
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n)) (F_check_ge (-1
                                                                    - s V_susan_principle_small__tmp2
                                                                    + s V_susan_principle_small_n) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-0.00030525 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2));
      (*-1.5448e-06 0*) F_binom_monotonic 2 (F_max0_ge_0 (-730
                                                          + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0))]
     ((5 # 26)
      + (0 # 1) * s V_susan_principle_small__tmp1 * max0(-100
                                                         + s V_susan_principle_small__tmp2)
      + (0 # 1) * s V_susan_principle_small__tmp1 * max0(730
                                                         - s V_susan_principle_small__tmp2)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                         + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      - (0 # 1) * s V_susan_principle_small_j * max0(-100
                                                     + s V_susan_principle_small__tmp2)
      - (0 # 1) * s V_susan_principle_small_j * max0(730
                                                     - s V_susan_principle_small__tmp2)
      - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                     + s V_susan_principle_small__tmp2)
      + (0 # 1) * s V_susan_principle_small_z * max0(-100
                                                     + s V_susan_principle_small__tmp2)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
      + max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-2
                                                   + s V_susan_principle_small__tmp1)
      - max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-2
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
      + (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      + max0(-2 + s V_susan_principle_small__tmp1
             - s V_susan_principle_small_j) * max0(-1
                                                   + s V_susan_principle_small__tmp
                                                   - s V_susan_principle_small_i)
      - (0 # 1) * max0(-2 + s V_susan_principle_small__tmp1
                       - s V_susan_principle_small_j) * max0(730
                                                             - s V_susan_principle_small__tmp2)
      + (73 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                         - s V_susan_principle_small_j)
      - (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                       - s V_susan_principle_small_j) * max0(730
                                                             - s V_susan_principle_small__tmp2)
      - (0 # 1) * max0(730 - s V_susan_principle_small__tmp2) <= z)%Q
   | 37 => hints
     [(*-2.035e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-1.54557e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j) (0))) (F_max0_ge_0 (-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j) (0))) (F_max0_ge_0 (-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-1.54802e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small_n) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small_n))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (730
                                                                    - s V_susan_principle_small_n)) (F_check_ge (730
                                                                    - s V_susan_principle_small_n) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n) (0))) (F_max0_ge_0 (s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-4.84524e-07 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)) (F_check_ge (s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.00030525 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_susan_principle_small__tmp2
                                                            - s V_susan_principle_small_n)) (F_check_ge (s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n) (0));
      (*-0.00030525 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small_n) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small_n))]
     ((5 # 26)
      + (0 # 1) * s V_susan_principle_small__tmp1 * max0(-100
                                                         + s V_susan_principle_small__tmp2)
      + (0 # 1) * s V_susan_principle_small__tmp1 * max0(730
                                                         - s V_susan_principle_small__tmp2)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                         + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      - (0 # 1) * s V_susan_principle_small_j * max0(-100
                                                     + s V_susan_principle_small__tmp2)
      - (0 # 1) * s V_susan_principle_small_j * max0(730
                                                     - s V_susan_principle_small__tmp2)
      - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                     + s V_susan_principle_small__tmp2)
      + (0 # 1) * s V_susan_principle_small_z * max0(-100
                                                     + s V_susan_principle_small__tmp2)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
      + max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-2
                                                   + s V_susan_principle_small__tmp1)
      - max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-2
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
      + (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      + max0(-2 + s V_susan_principle_small__tmp1
             - s V_susan_principle_small_j) * max0(-1
                                                   + s V_susan_principle_small__tmp
                                                   - s V_susan_principle_small_i)
      - (0 # 1) * max0(-2 + s V_susan_principle_small__tmp1
                       - s V_susan_principle_small_j) * max0(730
                                                             - s V_susan_principle_small__tmp2)
      + (73 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                         - s V_susan_principle_small_j)
      - (0 # 1) * max0(-1 + s V_susan_principle_small__tmp1
                       - s V_susan_principle_small_j) * max0(730
                                                             - s V_susan_principle_small__tmp2)
      - (0 # 1) * max0(730 - s V_susan_principle_small__tmp2) <= z)%Q
   | 38 => (-(3 # 98)
            - (0 # 1) * s V_susan_principle_small__tmp1 * max0(-730
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp1 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp2
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-730
                                                               + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_j * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_j * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_z * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-2 + s V_susan_principle_small__tmp1
                   - s V_susan_principle_small_j) * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
            + (0 # 1) * max0(730 - s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(730 - s V_susan_principle_small_n)
            - (0 # 1) * max0(s V_susan_principle_small__tmp2
                             - s V_susan_principle_small_n) <= z)%Q
   | 39 => (-(3 # 98)
            - (0 # 1) * s V_susan_principle_small__tmp1 * max0(-730
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp1 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp2
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-730
                                                               + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_j * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_j * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_z * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-2 + s V_susan_principle_small__tmp1
                   - s V_susan_principle_small_j) * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
            + (0 # 1) * max0(730 - s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(730 - s V_susan_principle_small_n)
            - (0 # 1) * max0(s V_susan_principle_small__tmp2
                             - s V_susan_principle_small_n) <= z)%Q
   | 40 => (-(3 # 98)
            - (0 # 1) * s V_susan_principle_small__tmp1 * max0(-730
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp1 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp2
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-730
                                                               + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_j * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_j * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_z * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + max0(-2 + s V_susan_principle_small__tmp1
                   - s V_susan_principle_small_j) * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
            + (0 # 1) * max0(730 - s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(730 - s V_susan_principle_small_n)
            - (0 # 1) * max0(s V_susan_principle_small__tmp2
                             - s V_susan_principle_small_n) <= z)%Q
   | 41 => (-(3 # 98)
            - (0 # 1) * s V_susan_principle_small__tmp1 * max0(-730
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp1 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp2
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-730
                                                               + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_j * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_j * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_z * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + (0 # 1) * max0(730 - s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(730 - s V_susan_principle_small_n)
            - (0 # 1) * max0(s V_susan_principle_small__tmp2
                             - s V_susan_principle_small_n) <= z)%Q
   | 42 => (-(3 # 98)
            - (0 # 1) * s V_susan_principle_small__tmp1 * max0(-730
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp1 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp2
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-730
                                                               + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_j * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_j * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_z * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + (0 # 1) * max0(730 - s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(730 - s V_susan_principle_small_n)
            - (0 # 1) * max0(s V_susan_principle_small__tmp2
                             - s V_susan_principle_small_n) <= z)%Q
   | 43 => (-(3 # 98)
            - (0 # 1) * s V_susan_principle_small__tmp1 * max0(-730
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp1 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small__tmp2
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-730
                                                               + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-100
                                                               + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                               + s V_susan_principle_small__tmp1)
            + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp
                                                               - s V_susan_principle_small_i)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                               + s V_susan_principle_small__tmp1
                                                               - s V_susan_principle_small_j)
            - (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                               - s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_j * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_j * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * s V_susan_principle_small_z * max0(-100
                                                           + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
            + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
            - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
            + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
            + max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-2
                                                         + s V_susan_principle_small__tmp1)
            - max0(-2 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
            + max0(-1 + s V_susan_principle_small__tmp
                   - s V_susan_principle_small_i) * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
            + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                              - s V_susan_principle_small_j)
            + (0 # 1) * max0(730 - s V_susan_principle_small__tmp2)
            + (0 # 1) * max0(730 - s V_susan_principle_small_n)
            - (0 # 1) * max0(s V_susan_principle_small__tmp2
                             - s V_susan_principle_small_n) <= z)%Q
   | 44 => hints
     [(*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_small_z)) (F_check_ge (0) (0)));
      (*-2.51953e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-2.035e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-730
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-2.51953e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-3.0525e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-0.000641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (0) (0)));
      (*-3.0525e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small_n)) (F_check_ge (0) (0)));
      (*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_small_z)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j) (0))) (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.00128205 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)) (F_check_ge (-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-2.035e-06 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (730
                                                                    - s V_susan_principle_small__tmp2))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-2.035e-06 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (730
                                                                    - s V_susan_principle_small__tmp2)) (F_check_ge (730
                                                                    - s V_susan_principle_small__tmp2) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_susan_principle_small_z) (0))) (F_max0_ge_0 (s V_susan_principle_small_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-0.0015873 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_susan_principle_small_z)) (F_check_ge (s V_susan_principle_small_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-730
                                                                    + s V_susan_principle_small__tmp2)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_susan_principle_small_z)) (F_check_ge (s V_susan_principle_small_z) (0));
      (*-0.00030525 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-100
                                                                    + s V_susan_principle_small__tmp2) (0))) (F_max0_ge_0 (-100
                                                                    + s V_susan_principle_small__tmp2))]
     (-(3 # 98)
      - (0 # 1) * s V_susan_principle_small__tmp1 * max0(-730
                                                         + s V_susan_principle_small__tmp2)
      + (0 # 1) * s V_susan_principle_small__tmp1 * max0(-100
                                                         + s V_susan_principle_small__tmp2)
      + (0 # 1) * s V_susan_principle_small__tmp2
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-730
                                                         + s V_susan_principle_small__tmp2)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-100
                                                         + s V_susan_principle_small__tmp2)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-3
                                                         + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-2
                                                         + s V_susan_principle_small__tmp1)
      + (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp
                                                         - s V_susan_principle_small_i)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(-1
                                                         + s V_susan_principle_small__tmp1
                                                         - s V_susan_principle_small_j)
      - (0 # 1) * s V_susan_principle_small__tmp2 * max0(730
                                                         - s V_susan_principle_small__tmp2)
      + (0 # 1) * s V_susan_principle_small_j * max0(-730
                                                     + s V_susan_principle_small__tmp2)
      - (0 # 1) * s V_susan_principle_small_j * max0(-100
                                                     + s V_susan_principle_small__tmp2)
      - (0 # 1) * s V_susan_principle_small_z * max0(-730
                                                     + s V_susan_principle_small__tmp2)
      + (0 # 1) * s V_susan_principle_small_z * max0(-100
                                                     + s V_susan_principle_small__tmp2)
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2)
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-100
                                                                    + s V_susan_principle_small_n)
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      + (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-730 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-3
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-2
                                                                    + s V_susan_principle_small__tmp1)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(-1
                                                                    + s V_susan_principle_small__tmp1
                                                                    - s V_susan_principle_small_j)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small__tmp2)
      - (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(730
                                                                    - s V_susan_principle_small_n)
      + (0 # 1) * max0(-100 + s V_susan_principle_small__tmp2) * max0(s V_susan_principle_small__tmp2
                                                                    - s V_susan_principle_small_n)
      - (0 # 1) * max0(-100 + s V_susan_principle_small_n) * max0(730
                                                                  - s V_susan_principle_small__tmp2)
      + (5 # 78) * max0(-3 + s V_susan_principle_small__tmp1)
      + max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-2
                                                   + s V_susan_principle_small__tmp1)
      - max0(-2 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-1
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      - (5 # 78) * max0(-2 + s V_susan_principle_small__tmp1)
      + max0(-1 + s V_susan_principle_small__tmp
             - s V_susan_principle_small_i) * max0(-1
                                                   + s V_susan_principle_small__tmp1
                                                   - s V_susan_principle_small_j)
      + (5 # 78) * max0(-1 + s V_susan_principle_small__tmp1
                        - s V_susan_principle_small_j)
      + (0 # 1) * max0(730 - s V_susan_principle_small__tmp2)
      + (0 # 1) * max0(730 - s V_susan_principle_small_n)
      - (0 # 1) * max0(s V_susan_principle_small__tmp2
                       - s V_susan_principle_small_n) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_susan_principle_small =>
    [mkPA Q (fun n z s => ai_susan_principle_small n s /\ annot0_susan_principle_small n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_susan_principle_small (proc_start P_susan_principle_small) s1 (proc_end P_susan_principle_small) s2 ->
    (s2 V_susan_principle_small_z <= max0(-2
                                          + s1 V_susan_principle_small_x_size) * max0(-2
                                                                    + s1 V_susan_principle_small_y_size)
                                     + max0(-2
                                            + s1 V_susan_principle_small_y_size))%Q.
Proof.
  prove_bound ipa admissible_ipa P_susan_principle_small.
Qed.
