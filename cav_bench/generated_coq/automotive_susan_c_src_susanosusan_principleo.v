Require Import pasta.Pasta.

Inductive proc: Type :=
  P_susan_principle.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_susan_principle_z := 1%positive.
Notation V_susan_principle__tmp := 2%positive.
Notation V_susan_principle__tmp1 := 3%positive.
Notation V_susan_principle__tmp2 := 4%positive.
Notation V_susan_principle_i := 5%positive.
Notation V_susan_principle_j := 6%positive.
Notation V_susan_principle_n := 7%positive.
Notation V_susan_principle_bp := 8%positive.
Notation V_susan_principle_in := 9%positive.
Notation V_susan_principle_max_no := 10%positive.
Notation V_susan_principle_r := 11%positive.
Notation V_susan_principle_x_size := 12%positive.
Notation V_susan_principle_y_size := 13%positive.
Definition Pedges_susan_principle: list (edge proc) :=
  (EA 1 (AAssign V_susan_principle_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_susan_principle__tmp2 (Some (EVar V_susan_principle_max_no))) 3)::
  (EA 3 (AAssign V_susan_principle__tmp1
  (Some (EVar V_susan_principle_x_size))) 4)::(EA 4 (AAssign
  V_susan_principle__tmp (Some (EVar V_susan_principle_y_size))) 5)::
  (EA 5 (AAssign V_susan_principle_i (Some (ENum (3)))) 6)::(EA 6 ANone 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard (fun s => ((eval (EVar V_susan_principle_i)
  s) < (eval (ESub (EVar V_susan_principle__tmp) (ENum (3))) s))%Z)) 11)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_susan_principle_i) s) >=
  (eval (ESub (EVar V_susan_principle__tmp) (ENum (3))) s))%Z)) 9)::
  (EA 9 AWeaken 10)::(EA 11 AWeaken 12)::(EA 12 (AAssign V_susan_principle_j
  (Some (ENum (3)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_susan_principle_j) s) <
  (eval (ESub (EVar V_susan_principle__tmp1) (ENum (3))) s))%Z)) 23)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_susan_principle_j) s) >=
  (eval (ESub (EVar V_susan_principle__tmp1) (ENum (3))) s))%Z)) 16)::
  (EA 16 AWeaken 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_susan_principle_i
  (Some (EAdd (EVar V_susan_principle_i) (ENum (1))))) 19)::
  (EA 19 ANone 20)::(EA 20 ANone 21)::(EA 21 (AAssign V_susan_principle_z
  (Some (EAdd (ENum (1)) (EVar V_susan_principle_z)))) 22)::
  (EA 22 AWeaken 8)::(EA 23 AWeaken 24)::(EA 24 (AAssign V_susan_principle_n
  (Some (ENum (100)))) 25)::(EA 25 (AAssign V_susan_principle_n None) 26)::
  (EA 26 (AAssign V_susan_principle_n None) 27)::(EA 27 (AAssign
  V_susan_principle_n None) 28)::(EA 28 (AAssign V_susan_principle_n
  None) 29)::(EA 29 (AAssign V_susan_principle_n None) 30)::(EA 30 (AAssign
  V_susan_principle_n None) 31)::(EA 31 (AAssign V_susan_principle_n
  None) 32)::(EA 32 (AAssign V_susan_principle_n None) 33)::(EA 33 (AAssign
  V_susan_principle_n None) 34)::(EA 34 (AAssign V_susan_principle_n
  None) 35)::(EA 35 (AAssign V_susan_principle_n None) 36)::(EA 36 (AAssign
  V_susan_principle_n None) 37)::(EA 37 (AAssign V_susan_principle_n
  None) 38)::(EA 38 (AAssign V_susan_principle_n None) 39)::(EA 39 (AAssign
  V_susan_principle_n None) 40)::(EA 40 (AAssign V_susan_principle_n
  None) 41)::(EA 41 (AAssign V_susan_principle_n None) 42)::(EA 42 (AAssign
  V_susan_principle_n None) 43)::(EA 43 (AAssign V_susan_principle_n
  None) 44)::(EA 44 (AAssign V_susan_principle_n None) 45)::(EA 45 (AAssign
  V_susan_principle_n None) 46)::(EA 46 (AAssign V_susan_principle_n
  None) 47)::(EA 47 (AAssign V_susan_principle_n None) 48)::(EA 48 (AAssign
  V_susan_principle_n None) 49)::(EA 49 (AAssign V_susan_principle_n
  None) 50)::(EA 50 (AAssign V_susan_principle_n None) 51)::(EA 51 (AAssign
  V_susan_principle_n None) 52)::(EA 52 (AAssign V_susan_principle_n
  None) 53)::(EA 53 (AAssign V_susan_principle_n None) 54)::(EA 54 (AAssign
  V_susan_principle_n None) 55)::(EA 55 (AAssign V_susan_principle_n
  None) 56)::(EA 56 (AAssign V_susan_principle_n None) 57)::(EA 57 (AAssign
  V_susan_principle_n None) 58)::(EA 58 (AAssign V_susan_principle_n
  None) 59)::(EA 59 (AAssign V_susan_principle_n None) 60)::(EA 60 (AAssign
  V_susan_principle_n None) 61)::(EA 61 AWeaken 62)::(EA 62 (AGuard
  (fun s => ((eval (EVar V_susan_principle_n) s) <=
  (eval (EVar V_susan_principle__tmp2) s))%Z)) 64)::(EA 62 (AGuard
  (fun s => ((eval (EVar V_susan_principle_n) s) >
  (eval (EVar V_susan_principle__tmp2) s))%Z)) 63)::(EA 63 AWeaken 66)::
  (EA 64 AWeaken 65)::(EA 65 ANone 66)::(EA 66 ANone 67)::(EA 67 (AAssign
  V_susan_principle_j (Some (EAdd (EVar V_susan_principle_j)
  (ENum (1))))) 68)::(EA 68 ANone 69)::(EA 69 ANone 70)::(EA 70 (AAssign
  V_susan_principle_z (Some (EAdd (ENum (1))
  (EVar V_susan_principle_z)))) 71)::(EA 71 AWeaken 15)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_susan_principle => Pedges_susan_principle
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_susan_principle => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_susan_principle (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_z <= 0)%Z
   | 3 => (-1 * s V_susan_principle_z <= 0 /\ 1 * s V_susan_principle_z <= 0)%Z
   | 4 => (1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_z <= 0)%Z
   | 5 => (-1 * s V_susan_principle_z <= 0 /\ 1 * s V_susan_principle_z <= 0)%Z
   | 6 => (1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ 1 * s V_susan_principle_i + -3 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 7 => (-1 * s V_susan_principle_i + 3 <= 0 /\ 1 * s V_susan_principle_i + -3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ 1 * s V_susan_principle_z <= 0)%Z
   | 8 => (-1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 9 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ 1 * s V_susan_principle__tmp+ -1 * s V_susan_principle_i + -3 <= 0)%Z
   | 10 => (1 * s V_susan_principle__tmp+ -1 * s V_susan_principle_i + -3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 11 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0)%Z
   | 12 => (-1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 13 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ 1 * s V_susan_principle_j + -3 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0)%Z
   | 14 => (-1 * s V_susan_principle_j + 3 <= 0 /\ 1 * s V_susan_principle_j + -3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 15 => (-1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 16 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ 1 * s V_susan_principle__tmp1+ -1 * s V_susan_principle_j + -3 <= 0)%Z
   | 17 => (1 * s V_susan_principle__tmp1+ -1 * s V_susan_principle_j + -3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 18 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ 1 * s V_susan_principle__tmp1+ -1 * s V_susan_principle_j + -3 <= 0)%Z
   | 19 => (1 * s V_susan_principle__tmp1+ -1 * s V_susan_principle_j + -3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 3 <= 0)%Z
   | 20 => (-1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ 1 * s V_susan_principle__tmp1+ -1 * s V_susan_principle_j + -3 <= 0)%Z
   | 21 => (1 * s V_susan_principle__tmp1+ -1 * s V_susan_principle_j + -3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 3 <= 0)%Z
   | 22 => (-1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ 1 * s V_susan_principle__tmp1+ -1 * s V_susan_principle_j + -3 <= 0 /\ -1 * s V_susan_principle_z + 1 <= 0)%Z
   | 23 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 24 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 25 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ 1 * s V_susan_principle_n + -100 <= 0 /\ -1 * s V_susan_principle_n + 100 <= 0)%Z
   | 26 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 27 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 28 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 29 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 30 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 31 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 32 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 33 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 34 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 35 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 36 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 37 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 38 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 39 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 40 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 41 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 42 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 43 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 44 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 45 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 46 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 47 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 48 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 49 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 50 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 51 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 52 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 53 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 54 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 55 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 56 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 57 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 58 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 59 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 60 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 61 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 62 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 63 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ 1 * s V_susan_principle__tmp2+ -1 * s V_susan_principle_n + 1 <= 0)%Z
   | 64 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle__tmp2+ 1 * s V_susan_principle_n <= 0)%Z
   | 65 => (-1 * s V_susan_principle__tmp2+ 1 * s V_susan_principle_n <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 66 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0)%Z
   | 67 => (-1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 68 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_j + 4 <= 0)%Z
   | 69 => (-1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0)%Z
   | 70 => (-1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_z <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle_j + 4 <= 0)%Z
   | 71 => (-1 * s V_susan_principle_j + 4 <= 0 /\ -1 * s V_susan_principle__tmp1+ 1 * s V_susan_principle_j + 3 <= 0 /\ -1 * s V_susan_principle__tmp+ 1 * s V_susan_principle_i + 4 <= 0 /\ -1 * s V_susan_principle_i + 3 <= 0 /\ -1 * s V_susan_principle_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_susan_principle (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-6 + s V_susan_principle_x_size) * max0(-6
                                                        + s V_susan_principle_y_size)
           + max0(-6 + s V_susan_principle_y_size) <= z)%Q
   | 2 => (s V_susan_principle_z
           + max0(-6 + s V_susan_principle_x_size) * max0(-6
                                                          + s V_susan_principle_y_size)
           + max0(-6 + s V_susan_principle_y_size) <= z)%Q
   | 3 => (s V_susan_principle_z
           + max0(-6 + s V_susan_principle_x_size) * max0(-6
                                                          + s V_susan_principle_y_size)
           + max0(-6 + s V_susan_principle_y_size) <= z)%Q
   | 4 => (s V_susan_principle_z
           + max0(-6 + s V_susan_principle__tmp1) * max0(-6
                                                         + s V_susan_principle_y_size)
           + max0(-6 + s V_susan_principle_y_size) <= z)%Q
   | 5 => (s V_susan_principle_z + max0(-6 + s V_susan_principle__tmp)
           + max0(-6 + s V_susan_principle__tmp) * max0(-6
                                                        + s V_susan_principle__tmp1) <= z)%Q
   | 6 => (-(10 # 27)
           - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                         + s V_susan_principle__tmp)
           + (10 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
           + (10 # 81) * s V_susan_principle_i
           + (10 # 81) * s V_susan_principle_i * max0(-6
                                                      + s V_susan_principle__tmp)
           + s V_susan_principle_z
           + (10 # 27) * max0(-6 + s V_susan_principle__tmp)
           - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
           + max0(-6 + s V_susan_principle__tmp1) * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
           + (7 # 27) * max0(-3 + s V_susan_principle__tmp
                             - s V_susan_principle_i)
           + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(3
                                                              - s V_susan_principle__tmp
                                                              + s V_susan_principle_i)
           - (10 # 81) * max0(-3 + s V_susan_principle_i)
           + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                 - s V_susan_principle__tmp
                                                                 + s V_susan_principle_i) <= z)%Q
   | 7 => (-(10 # 27)
           - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                         + s V_susan_principle__tmp)
           + (10 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
           + (10 # 81) * s V_susan_principle_i
           + (10 # 81) * s V_susan_principle_i * max0(-6
                                                      + s V_susan_principle__tmp)
           + s V_susan_principle_z
           + (10 # 27) * max0(-6 + s V_susan_principle__tmp)
           - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
           + max0(-6 + s V_susan_principle__tmp1) * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
           + (7 # 27) * max0(-3 + s V_susan_principle__tmp
                             - s V_susan_principle_i)
           + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(3
                                                              - s V_susan_principle__tmp
                                                              + s V_susan_principle_i)
           - (10 # 81) * max0(-3 + s V_susan_principle_i)
           + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                 - s V_susan_principle__tmp
                                                                 + s V_susan_principle_i) <= z)%Q
   | 8 => (-(10 # 27)
           - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                         + s V_susan_principle__tmp)
           + (10 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
           + (10 # 81) * s V_susan_principle_i
           + (10 # 81) * s V_susan_principle_i * max0(-6
                                                      + s V_susan_principle__tmp)
           + s V_susan_principle_z
           + (10 # 27) * max0(-6 + s V_susan_principle__tmp)
           - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
           + max0(-6 + s V_susan_principle__tmp1) * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
           + (7 # 27) * max0(-3 + s V_susan_principle__tmp
                             - s V_susan_principle_i)
           + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(3
                                                              - s V_susan_principle__tmp
                                                              + s V_susan_principle_i)
           - (10 # 81) * max0(-3 + s V_susan_principle_i)
           + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                 - s V_susan_principle__tmp
                                                                 + s V_susan_principle_i) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-3 + s V_susan_principle__tmp
                                             - s V_susan_principle_i) (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i));
      (*-1 0*) F_max0_ge_0 (-4 + s V_susan_principle__tmp
                            - s V_susan_principle_i);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                            + s V_susan_principle__tmp1)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                   + 
                                                                   s V_susan_principle__tmp
                                                                   - 
                                                                   s V_susan_principle_i)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                   + 
                                                                   s V_susan_principle_i)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - s V_susan_principle__tmp
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - s V_susan_principle__tmp
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (3
                                                                    - s V_susan_principle__tmp
                                                                    + s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)) (F_check_ge (3
                                                                    - s V_susan_principle__tmp
                                                                    + s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i))]
     (-(10 # 27)
      - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                    + s V_susan_principle__tmp)
      + (10 # 81) * s V_susan_principle__tmp * max0(-3
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (10 # 81) * s V_susan_principle_i
      + (10 # 81) * s V_susan_principle_i * max0(-6
                                                 + s V_susan_principle__tmp)
      + s V_susan_principle_z
      + (10 # 27) * max0(-6 + s V_susan_principle__tmp)
      - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
      + max0(-6 + s V_susan_principle__tmp1) * max0(-3
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (7 # 27) * max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i)
      + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i) * max0(3
                                                         - s V_susan_principle__tmp
                                                         + s V_susan_principle_i)
      - (10 # 81) * max0(-3 + s V_susan_principle_i)
      + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                            - s V_susan_principle__tmp
                                                            + s V_susan_principle_i) <= z)%Q
   | 10 => (s V_susan_principle_z <= z)%Q
   | 11 => hints
     [(*0 0.037037*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + 
                                                                    s V_susan_principle__tmp)) (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*0 0.0864198*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*0 0.123457*) F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0));
      (*0 0.123457*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-6
                                                                    + 
                                                                    s V_susan_principle__tmp) (0))) (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp))]
     (-(10 # 27)
      - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                    + s V_susan_principle__tmp)
      + (10 # 81) * s V_susan_principle__tmp * max0(-3
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (10 # 81) * s V_susan_principle_i
      + (10 # 81) * s V_susan_principle_i * max0(-6
                                                 + s V_susan_principle__tmp)
      + s V_susan_principle_z
      + (10 # 27) * max0(-6 + s V_susan_principle__tmp)
      - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
      + max0(-6 + s V_susan_principle__tmp1) * max0(-3
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (7 # 27) * max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i)
      + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i) * max0(3
                                                         - s V_susan_principle__tmp
                                                         + s V_susan_principle_i)
      - (10 # 81) * max0(-3 + s V_susan_principle_i)
      + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                            - s V_susan_principle__tmp
                                                            + s V_susan_principle_i) <= z)%Q
   | 12 => (-(10 # 81) * s V_susan_principle__tmp * max0(-6
                                                         + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-3
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            + s V_susan_principle_z
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-3
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            + (11 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i) <= z)%Q
   | 13 => (-(10 # 81) * s V_susan_principle__tmp * max0(-6
                                                         + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-3
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            + s V_susan_principle_z
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            - max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            + (11 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-3 + s V_susan_principle__tmp
                                       - s V_susan_principle_i) (1);
      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + 
                                                                    s V_susan_principle__tmp)) (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.0864198 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.0864198 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)))]
     (-(10 # 81) * s V_susan_principle__tmp * max0(-6
                                                   + s V_susan_principle__tmp)
      + (10 # 81) * s V_susan_principle__tmp * max0(-3
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      - (4 # 81) * s V_susan_principle__tmp * max0(-3 + s V_susan_principle_i)
      + (10 # 81) * s V_susan_principle_i * max0(-6
                                                 + s V_susan_principle__tmp)
      + (7 # 81) * s V_susan_principle_i * max0(-3 + s V_susan_principle_i)
      + s V_susan_principle_z
      + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
      - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                              + s V_susan_principle_i)
      - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
      + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      - max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
      + (7 # 81) * max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle_i)
      + (11 # 81) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i)
      + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
      + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i) * max0(3
                                                         - s V_susan_principle__tmp
                                                         + s V_susan_principle_i)
      + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                            - s V_susan_principle__tmp
                                                            + s V_susan_principle_i) <= z)%Q
   | 15 => ((1 # 1)
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            + s V_susan_principle_z
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            - max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (14 # 27) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i) <= z)%Q
   | 16 => hints
     [(*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-7
                                                                    + s V_susan_principle__tmp) (0))) (F_max0_ge_0 (-7
                                                                    + s V_susan_principle__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0))) (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - s V_susan_principle__tmp
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                              + s V_susan_principle__tmp
                                                              - s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)) (F_check_ge (0) (0)));
      (*-0.0864198 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - s V_susan_principle__tmp
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.0864198 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + 
                                                                    s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - s V_susan_principle__tmp
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)))]
     ((1 # 1)
      - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                    + s V_susan_principle__tmp)
      + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                   + s V_susan_principle__tmp
                                                   - s V_susan_principle_i)
      - (4 # 81) * s V_susan_principle__tmp * max0(-3 + s V_susan_principle_i)
      + (10 # 81) * s V_susan_principle_i * max0(-6
                                                 + s V_susan_principle__tmp)
      - (7 # 81) * s V_susan_principle_i * max0(-4 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      + (7 # 81) * s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      + (7 # 81) * s V_susan_principle_i * max0(-3 + s V_susan_principle_i)
      + s V_susan_principle_z
      + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
      - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                              + s V_susan_principle__tmp
                                                              - s V_susan_principle_i)
      - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                              + s V_susan_principle_i)
      - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
      + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                         - s V_susan_principle_i)
      - max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
      + (7 # 81) * max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle_i)
      - (14 # 27) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i)
      + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
      + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i) * max0(3
                                                         - s V_susan_principle__tmp
                                                         + s V_susan_principle_i)
      + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                            - s V_susan_principle__tmp
                                                            + s V_susan_principle_i) <= z)%Q
   | 17 => ((1 # 1)
            + (1 # 27) * s V_susan_principle__tmp * max0(-7
                                                         + s V_susan_principle__tmp)
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            - (1 # 27) * s V_susan_principle_i * max0(-7
                                                      + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            - s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + s V_susan_principle_z
            - (1 # 9) * max0(-7 + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            - max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (3 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            - (7 # 27) * max0(-3 + s V_susan_principle_i) <= z)%Q
   | 18 => ((1 # 1)
            + (1 # 27) * s V_susan_principle__tmp * max0(-7
                                                         + s V_susan_principle__tmp)
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            - (1 # 27) * s V_susan_principle_i * max0(-7
                                                      + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            - s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + s V_susan_principle_z
            - (1 # 9) * max0(-7 + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            - max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (3 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            - (7 # 27) * max0(-3 + s V_susan_principle_i) <= z)%Q
   | 19 => ((1 # 1)
            + (1 # 27) * s V_susan_principle__tmp * max0(-7
                                                         + s V_susan_principle__tmp)
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (1 # 27) * s V_susan_principle__tmp * max0(-4
                                                         + s V_susan_principle_i)
            + (10 # 81) * s V_susan_principle__tmp * max0(-3
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (1 # 27) * s V_susan_principle_i * max0(-7
                                                      + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            - s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + s V_susan_principle_z
            - (2 # 27) * max0(-7 + s V_susan_principle__tmp)
            + (10 # 27) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-3
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            - (7 # 27) * max0(-4 + s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle_i) * max0(-3
                                                                 + s V_susan_principle__tmp
                                                                 - s V_susan_principle_i)
            + (49 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            - max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            - (2 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j) <= z)%Q
   | 20 => ((1 # 1)
            + (1 # 27) * s V_susan_principle__tmp * max0(-7
                                                         + s V_susan_principle__tmp)
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (1 # 27) * s V_susan_principle__tmp * max0(-4
                                                         + s V_susan_principle_i)
            + (10 # 81) * s V_susan_principle__tmp * max0(-3
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (1 # 27) * s V_susan_principle_i * max0(-7
                                                      + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            - s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + s V_susan_principle_z
            - (2 # 27) * max0(-7 + s V_susan_principle__tmp)
            + (10 # 27) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-3
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            - (7 # 27) * max0(-4 + s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle_i) * max0(-3
                                                                 + s V_susan_principle__tmp
                                                                 - s V_susan_principle_i)
            + (49 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            - max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            - (2 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j) <= z)%Q
   | 21 => ((1 # 1)
            + (1 # 27) * s V_susan_principle__tmp * max0(-7
                                                         + s V_susan_principle__tmp)
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (1 # 27) * s V_susan_principle__tmp * max0(-4
                                                         + s V_susan_principle_i)
            + (10 # 81) * s V_susan_principle__tmp * max0(-3
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (1 # 27) * s V_susan_principle_i * max0(-7
                                                      + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            - s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + s V_susan_principle_z
            - (2 # 27) * max0(-7 + s V_susan_principle__tmp)
            + (10 # 27) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-3
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            - (7 # 27) * max0(-4 + s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle_i) * max0(-3
                                                                 + s V_susan_principle__tmp
                                                                 - s V_susan_principle_i)
            + (49 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            - max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            - (2 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j) <= z)%Q
   | 22 => hints
     [(*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-7
                                                                    + s V_susan_principle__tmp) (0))) (F_max0_ge_0 (-7
                                                                    + s V_susan_principle__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-7
                                                                    + 
                                                                    s V_susan_principle__tmp)) (F_check_ge (-7
                                                                    + s V_susan_principle__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0))) (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + 
                                                                    s V_susan_principle__tmp)) (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - s V_susan_principle__tmp
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (-4
                                                                    + s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                                    + 
                                                                    s V_susan_principle_i)) (F_check_ge (-4
                                                                    + s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.0864198 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                                    + s V_susan_principle_i)) (F_check_ge (-4
                                                                    + s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.037037 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - s V_susan_principle__tmp
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (3
                                                                    - s V_susan_principle__tmp
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.123457 0*) F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                          + s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                 + s V_susan_principle__tmp1
                                                 - s V_susan_principle_j)) (F_check_ge (0) (0))]
     ((1 # 27) * s V_susan_principle__tmp * max0(-7
                                                 + s V_susan_principle__tmp)
      - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                    + s V_susan_principle__tmp)
      + (1 # 27) * s V_susan_principle__tmp * max0(-4 + s V_susan_principle_i)
      + (10 # 81) * s V_susan_principle__tmp * max0(-3
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                        - s V_susan_principle_j)
      - (1 # 27) * s V_susan_principle_i * max0(-7 + s V_susan_principle__tmp)
      + (10 # 81) * s V_susan_principle_i * max0(-6
                                                 + s V_susan_principle__tmp)
      - (7 # 81) * s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      - s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                     - s V_susan_principle_j)
      + s V_susan_principle_z
      - (2 # 27) * max0(-7 + s V_susan_principle__tmp)
      + (10 # 27) * max0(-6 + s V_susan_principle__tmp)
      - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                              + s V_susan_principle_i)
      - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                              + s V_susan_principle__tmp
                                                              - s V_susan_principle_i)
      + max0(-6 + s V_susan_principle__tmp1) * max0(-3
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      - (7 # 27) * max0(-4 + s V_susan_principle_i)
      + (7 # 81) * max0(-4 + s V_susan_principle_i) * max0(-3
                                                           + s V_susan_principle__tmp
                                                           - s V_susan_principle_i)
      + (49 # 81) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i)
      - max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
      - (2 # 1) * max0(-3 + s V_susan_principle__tmp1 - s V_susan_principle_j) <= z)%Q
   | 23 => hints
     [(*0 0.166667*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + 
                                                                    s V_susan_principle__tmp)) (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_z)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_z)) (F_check_ge (0) (0)));
      (*0 0.166667*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_susan_principle_z) (0))) (F_max0_ge_0 (s V_susan_principle_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))]
     ((1 # 1)
      - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                    + s V_susan_principle__tmp)
      + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                   + s V_susan_principle__tmp
                                                   - s V_susan_principle_i)
      - (4 # 81) * s V_susan_principle__tmp * max0(-3 + s V_susan_principle_i)
      + (10 # 81) * s V_susan_principle_i * max0(-6
                                                 + s V_susan_principle__tmp)
      - (7 # 81) * s V_susan_principle_i * max0(-4 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      + (7 # 81) * s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      + (7 # 81) * s V_susan_principle_i * max0(-3 + s V_susan_principle_i)
      + s V_susan_principle_z
      + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
      - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                              + s V_susan_principle__tmp
                                                              - s V_susan_principle_i)
      - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                              + s V_susan_principle_i)
      - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
      + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                         - s V_susan_principle_i)
      - max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
      + (7 # 81) * max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle_i)
      - (14 # 27) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i)
      + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
      + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i) * max0(3
                                                         - s V_susan_principle__tmp
                                                         + s V_susan_principle_i)
      + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                            - s V_susan_principle__tmp
                                                            + s V_susan_principle_i) <= z)%Q
   | 24 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 25 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 26 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 27 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 28 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 29 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 30 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 31 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 32 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 33 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 34 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 35 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 36 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 37 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 38 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 39 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 40 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 41 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 42 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 43 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 44 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 45 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 46 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 47 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 48 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 49 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 50 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 51 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 52 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 53 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 54 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 55 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 56 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 57 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 58 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 59 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 60 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                              - s V_susan_principle_j)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                           - s V_susan_principle_j)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + (4 # 1) * max0(-3 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 61 => hints
     [(*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.0128205 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*0 0.2*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                               + s V_susan_principle__tmp1
                                                               - s V_susan_principle_j)) (F_check_ge (-4
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                             + s V_susan_principle__tmp
                                                             - s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)) (F_check_ge (0) (0)));
      (*0 0.166667*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_susan_principle_z)) (F_check_ge (s V_susan_principle_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_susan_principle_z) (0))) (F_max0_ge_0 (s V_susan_principle_z))]
     ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
      - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                    + s V_susan_principle__tmp)
      + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (1 # 27) * s V_susan_principle__tmp * max0(-3
                                                   + s V_susan_principle__tmp
                                                   - s V_susan_principle_i)
      - s V_susan_principle__tmp * max0(-3 + s V_susan_principle__tmp1
                                        - s V_susan_principle_j)
      - (4 # 81) * s V_susan_principle__tmp * max0(-3 + s V_susan_principle_i)
      + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
      + (1 # 6) * s V_susan_principle_i
      + (10 # 81) * s V_susan_principle_i * max0(-6
                                                 + s V_susan_principle__tmp)
      - (7 # 81) * s V_susan_principle_i * max0(-4 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      + (7 # 81) * s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      + s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp1
                                     - s V_susan_principle_j)
      + (7 # 81) * s V_susan_principle_i * max0(-3 + s V_susan_principle_i)
      - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
      + s V_susan_principle_z
      - (1 # 6) * s V_susan_principle_z * max0(-6 + s V_susan_principle__tmp)
      + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
      - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                              + s V_susan_principle__tmp
                                                              - s V_susan_principle_i)
      - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                              + s V_susan_principle_i)
      - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
      + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (14 # 27) * max0(-4 + s V_susan_principle__tmp
                         - s V_susan_principle_i)
      + (7 # 81) * max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle_i)
      - (19 # 54) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i)
      + max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)
      + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i) * max0(3
                                                         - s V_susan_principle__tmp
                                                         + s V_susan_principle_i)
      + (4 # 1) * max0(-3 + s V_susan_principle__tmp1 - s V_susan_principle_j)
      + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                            - s V_susan_principle__tmp
                                                            + s V_susan_principle_i)
      + (1 # 6) * max0(-3 + s V_susan_principle_i) * max0(s V_susan_principle_z)
      - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 62 => ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
            - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 77) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 20) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - (4 # 81) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 5) * s V_susan_principle__tmp1 * max0(-4
                                                         + s V_susan_principle__tmp1
                                                         - s V_susan_principle_j)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (7 # 81) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (5 # 68) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            - (1 # 5) * s V_susan_principle_j * max0(-4
                                                     + s V_susan_principle__tmp1
                                                     - s V_susan_principle_j)
            + (1 # 2) * s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (1 # 6) * s V_susan_principle_z * max0(-3
                                                     + s V_susan_principle_i)
            + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (12 # 25) * max0(-4 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            - (1 # 78) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle__tmp
                                                              - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (4 # 5) * max0(-4 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)
            - (1 # 5) * max0(-4 + s V_susan_principle__tmp1
                             - s V_susan_principle_j)^2
            - (25 # 62) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp1 - s V_susan_principle_j)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i) <= z)%Q
   | 63 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-3 + s V_susan_principle__tmp1
                                       - s V_susan_principle_j) (1);
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-7
                                                                    + s V_susan_principle__tmp)) (F_check_ge (-7
                                                                    + s V_susan_principle__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0))) (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j) (0))) (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_susan_principle_z)) (F_check_ge (s V_susan_principle_z) (0));
      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                          + s V_susan_principle__tmp)) (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0))]
     ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
      - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                    + s V_susan_principle__tmp)
      + (10 # 77) * s V_susan_principle__tmp * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (1 # 20) * s V_susan_principle__tmp * max0(-3
                                                   + s V_susan_principle__tmp
                                                   - s V_susan_principle_i)
      - (4 # 81) * s V_susan_principle__tmp * max0(-3 + s V_susan_principle_i)
      + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
      + (1 # 5) * s V_susan_principle__tmp1 * max0(-4
                                                   + s V_susan_principle__tmp1
                                                   - s V_susan_principle_j)
      + (1 # 6) * s V_susan_principle_i
      + (10 # 81) * s V_susan_principle_i * max0(-6
                                                 + s V_susan_principle__tmp)
      - (7 # 81) * s V_susan_principle_i * max0(-4 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      + (5 # 68) * s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      + (7 # 81) * s V_susan_principle_i * max0(-3 + s V_susan_principle_i)
      - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
      - (1 # 5) * s V_susan_principle_j * max0(-4 + s V_susan_principle__tmp1
                                               - s V_susan_principle_j)
      + (1 # 2) * s V_susan_principle_z
      - (1 # 6) * s V_susan_principle_z * max0(-6 + s V_susan_principle__tmp)
      + (1 # 6) * s V_susan_principle_z * max0(-3 + s V_susan_principle_i)
      + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
      - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                              + s V_susan_principle__tmp
                                                              - s V_susan_principle_i)
      - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                              + s V_susan_principle_i)
      - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
      + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (12 # 25) * max0(-4 + s V_susan_principle__tmp
                         - s V_susan_principle_i)
      - (1 # 78) * max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)
      + (7 # 81) * max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle_i)
      - (4 # 5) * max0(-4 + s V_susan_principle__tmp1 - s V_susan_principle_j)
      - (1 # 5) * max0(-4 + s V_susan_principle__tmp1 - s V_susan_principle_j)^2
      - (25 # 62) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i)
      + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i) * max0(3
                                                         - s V_susan_principle__tmp
                                                         + s V_susan_principle_i)
      + max0(-3 + s V_susan_principle__tmp1 - s V_susan_principle_j)
      + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                            - s V_susan_principle__tmp
                                                            + s V_susan_principle_i) <= z)%Q
   | 64 => hints
     [(*0 1*) F_max0_pre_decrement 1 (-3 + s V_susan_principle__tmp1
                                      - s V_susan_principle_j) (1);
      (*0 0.00641026*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-7
                                                                    + s V_susan_principle__tmp)) (F_check_ge (-7
                                                                    + s V_susan_principle__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-7
                                                                    + s V_susan_principle__tmp)) (F_check_ge (-7
                                                                    + s V_susan_principle__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0))) (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j) (0))) (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)) (F_check_ge (0) (0)));
      (*-0.0128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*0 0.0128205*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + 
                                                                    s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (s V_susan_principle_z)) (F_check_ge (s V_susan_principle_z) (0));
      (*-0.0128205 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i));
      (*-0.00641026 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i));
      (*0 0.00641026*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i));
      (*-0.173077 0*) F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                          + s V_susan_principle__tmp)) (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0));
      (*-0.00641026 0*) F_binom_monotonic 1 (F_max0_ge_arg (-7
                                                            + s V_susan_principle__tmp)) (F_check_ge (-7
                                                                    + s V_susan_principle__tmp) (0))]
     ((3 # 2) - (1 # 6) * s V_susan_principle__tmp
      - (10 # 81) * s V_susan_principle__tmp * max0(-6
                                                    + s V_susan_principle__tmp)
      + (10 # 77) * s V_susan_principle__tmp * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (1 # 20) * s V_susan_principle__tmp * max0(-3
                                                   + s V_susan_principle__tmp
                                                   - s V_susan_principle_i)
      - (4 # 81) * s V_susan_principle__tmp * max0(-3 + s V_susan_principle_i)
      + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
      + (1 # 5) * s V_susan_principle__tmp1 * max0(-4
                                                   + s V_susan_principle__tmp1
                                                   - s V_susan_principle_j)
      + (1 # 6) * s V_susan_principle_i
      + (10 # 81) * s V_susan_principle_i * max0(-6
                                                 + s V_susan_principle__tmp)
      - (7 # 81) * s V_susan_principle_i * max0(-4 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      + (5 # 68) * s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      + (7 # 81) * s V_susan_principle_i * max0(-3 + s V_susan_principle_i)
      - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
      - (1 # 5) * s V_susan_principle_j * max0(-4 + s V_susan_principle__tmp1
                                               - s V_susan_principle_j)
      + (1 # 2) * s V_susan_principle_z
      - (1 # 6) * s V_susan_principle_z * max0(-6 + s V_susan_principle__tmp)
      + (1 # 6) * s V_susan_principle_z * max0(-3 + s V_susan_principle_i)
      + (40 # 81) * max0(-6 + s V_susan_principle__tmp)
      - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                              + s V_susan_principle__tmp
                                                              - s V_susan_principle_i)
      - (1 # 27) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                              + s V_susan_principle_i)
      - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
      + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (12 # 25) * max0(-4 + s V_susan_principle__tmp
                         - s V_susan_principle_i)
      - (1 # 78) * max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)
      + (7 # 81) * max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle_i)
      - (4 # 5) * max0(-4 + s V_susan_principle__tmp1 - s V_susan_principle_j)
      - (1 # 5) * max0(-4 + s V_susan_principle__tmp1 - s V_susan_principle_j)^2
      - (25 # 62) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i)
      + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i) * max0(3
                                                         - s V_susan_principle__tmp
                                                         + s V_susan_principle_i)
      + max0(-3 + s V_susan_principle__tmp1 - s V_susan_principle_j)
      + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                            - s V_susan_principle__tmp
                                                            + s V_susan_principle_i) <= z)%Q
   | 65 => ((3 # 2)
            - (1 # 154) * s V_susan_principle__tmp * max0(-7
                                                          + s V_susan_principle__tmp)
            - (11 # 94) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 23) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - (3 # 82) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (2 # 25) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (2 # 25) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (1 # 6) * s V_susan_principle_z * max0(-3
                                                     + s V_susan_principle_i)
            + (1 # 26) * max0(-7 + s V_susan_principle__tmp)
            + (24 # 85) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 2) * max0(-4 + s V_susan_principle__tmp
                             - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            + max0(-4 + s V_susan_principle__tmp1 - s V_susan_principle_j)
            - (48 # 121) * max0(-3 + s V_susan_principle__tmp
                                - s V_susan_principle_i)
            - (1 # 78) * max0(-3 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            - (3 # 52) * max0(-3 + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 66 => ((3 # 2)
            - (1 # 154) * s V_susan_principle__tmp * max0(-7
                                                          + s V_susan_principle__tmp)
            - (11 # 94) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 23) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - (3 # 82) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (2 # 25) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (2 # 25) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (1 # 6) * s V_susan_principle_z * max0(-3
                                                     + s V_susan_principle_i)
            + (1 # 26) * max0(-7 + s V_susan_principle__tmp)
            + (24 # 85) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 2) * max0(-4 + s V_susan_principle__tmp
                             - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            + max0(-4 + s V_susan_principle__tmp1 - s V_susan_principle_j)
            - (48 # 121) * max0(-3 + s V_susan_principle__tmp
                                - s V_susan_principle_i)
            - (1 # 78) * max0(-3 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            - (3 # 52) * max0(-3 + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 67 => ((3 # 2)
            - (1 # 154) * s V_susan_principle__tmp * max0(-7
                                                          + s V_susan_principle__tmp)
            - (11 # 94) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 23) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - (3 # 82) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (2 # 25) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (2 # 25) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (1 # 6) * s V_susan_principle_z * max0(-3
                                                     + s V_susan_principle_i)
            + (1 # 26) * max0(-7 + s V_susan_principle__tmp)
            + (24 # 85) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 2) * max0(-4 + s V_susan_principle__tmp
                             - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            + max0(-4 + s V_susan_principle__tmp1 - s V_susan_principle_j)
            - (48 # 121) * max0(-3 + s V_susan_principle__tmp
                                - s V_susan_principle_i)
            - (1 # 78) * max0(-3 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            - (3 # 52) * max0(-3 + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 68 => ((3 # 2)
            - (1 # 154) * s V_susan_principle__tmp * max0(-7
                                                          + s V_susan_principle__tmp)
            - (11 # 94) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 23) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - (3 # 82) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (2 # 25) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (2 # 25) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (1 # 6) * s V_susan_principle_z * max0(-3
                                                     + s V_susan_principle_i)
            + (1 # 26) * max0(-7 + s V_susan_principle__tmp)
            + (24 # 85) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 2) * max0(-4 + s V_susan_principle__tmp
                             - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (48 # 121) * max0(-3 + s V_susan_principle__tmp
                                - s V_susan_principle_i)
            - (1 # 78) * max0(-3 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp1 - s V_susan_principle_j)
            - (3 # 52) * max0(-3 + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 69 => ((3 # 2)
            - (1 # 154) * s V_susan_principle__tmp * max0(-7
                                                          + s V_susan_principle__tmp)
            - (11 # 94) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 23) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - (3 # 82) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (2 # 25) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (2 # 25) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (1 # 6) * s V_susan_principle_z * max0(-3
                                                     + s V_susan_principle_i)
            + (1 # 26) * max0(-7 + s V_susan_principle__tmp)
            + (24 # 85) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 2) * max0(-4 + s V_susan_principle__tmp
                             - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (48 # 121) * max0(-3 + s V_susan_principle__tmp
                                - s V_susan_principle_i)
            - (1 # 78) * max0(-3 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp1 - s V_susan_principle_j)
            - (3 # 52) * max0(-3 + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 70 => ((3 # 2)
            - (1 # 154) * s V_susan_principle__tmp * max0(-7
                                                          + s V_susan_principle__tmp)
            - (11 # 94) * s V_susan_principle__tmp * max0(-6
                                                          + s V_susan_principle__tmp)
            + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 23) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle__tmp
                                                         - s V_susan_principle_i)
            - (3 # 82) * s V_susan_principle__tmp * max0(-3
                                                         + s V_susan_principle_i)
            + (1 # 6) * s V_susan_principle__tmp * max0(s V_susan_principle_z)
            + (1 # 6) * s V_susan_principle_i
            + (10 # 81) * s V_susan_principle_i * max0(-6
                                                       + s V_susan_principle__tmp)
            - (2 # 25) * s V_susan_principle_i * max0(-4
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (7 # 81) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle__tmp
                                                      - s V_susan_principle_i)
            + (2 # 25) * s V_susan_principle_i * max0(-3
                                                      + s V_susan_principle_i)
            - (1 # 6) * s V_susan_principle_i * max0(s V_susan_principle_z)
            + s V_susan_principle_z
            - (1 # 6) * s V_susan_principle_z * max0(-6
                                                     + s V_susan_principle__tmp)
            + (1 # 6) * s V_susan_principle_z * max0(-3
                                                     + s V_susan_principle_i)
            + (1 # 26) * max0(-7 + s V_susan_principle__tmp)
            + (24 # 85) * max0(-6 + s V_susan_principle__tmp)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)
            - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                                    + 
                                                                    s V_susan_principle_i)
            - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                                    - 
                                                                    s V_susan_principle__tmp
                                                                    + 
                                                                    s V_susan_principle_i)
            + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                          + s V_susan_principle__tmp
                                                          - s V_susan_principle_i)
            + (1 # 2) * max0(-4 + s V_susan_principle__tmp
                             - s V_susan_principle_i)
            + (7 # 81) * max0(-4 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            - (48 # 121) * max0(-3 + s V_susan_principle__tmp
                                - s V_susan_principle_i)
            - (1 # 78) * max0(-3 + s V_susan_principle__tmp
                              - s V_susan_principle_i) * max0(-3
                                                              + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                               - s V_susan_principle_i) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
            + max0(-3 + s V_susan_principle__tmp1 - s V_susan_principle_j)
            - (3 # 52) * max0(-3 + s V_susan_principle_i)
            + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                                  - s V_susan_principle__tmp
                                                                  + s V_susan_principle_i)
            - (1 # 2) * max0(s V_susan_principle_z) <= z)%Q
   | 71 => hints
     [(*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-7
                                                                    + s V_susan_principle__tmp) (0))) (F_max0_ge_0 (-7
                                                                    + s V_susan_principle__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-7
                                                                    + s V_susan_principle__tmp) (0))) (F_max0_ge_0 (-7
                                                                    + s V_susan_principle__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-6
                                                                    + s V_susan_principle__tmp) (0))) (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_z)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                              + s V_susan_principle__tmp
                                                              - s V_susan_principle_i)) (F_check_ge (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp1
                                                                    - s V_susan_principle_j)) (F_check_ge (0) (0)));
      (*-0.0128205 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_susan_principle_z)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + 
                                                                    s V_susan_principle__tmp
                                                                    - 
                                                                    s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_z)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i))) (F_binom_monotonic 1 (F_max0_ge_0 (-4
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.00641026 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                                    + 
                                                                    s V_susan_principle_i)) (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_susan_principle_z)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_susan_principle_z)) (F_check_ge (-1
                                                                    + s V_susan_principle_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_susan_principle_z) (0))) (F_max0_ge_0 (s V_susan_principle_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle__tmp
                                                                    - s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_susan_principle_z) (0))) (F_max0_ge_0 (s V_susan_principle_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_susan_principle_z)) (F_check_ge (s V_susan_principle_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                                    + s V_susan_principle__tmp)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_susan_principle_i) (0))) (F_max0_ge_0 (-3
                                                                    + s V_susan_principle_i))]
     ((1 # 2)
      - (1 # 154) * s V_susan_principle__tmp * max0(-7
                                                    + s V_susan_principle__tmp)
      - (11 # 94) * s V_susan_principle__tmp * max0(-6
                                                    + s V_susan_principle__tmp)
      + (10 # 81) * s V_susan_principle__tmp * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (1 # 23) * s V_susan_principle__tmp * max0(-3
                                                   + s V_susan_principle__tmp
                                                   - s V_susan_principle_i)
      - (3 # 82) * s V_susan_principle__tmp * max0(-3 + s V_susan_principle_i)
      + (1 # 6) * s V_susan_principle__tmp * max0(-1 + s V_susan_principle_z)
      + (1 # 6) * s V_susan_principle_i
      + (10 # 81) * s V_susan_principle_i * max0(-6
                                                 + s V_susan_principle__tmp)
      - (2 # 25) * s V_susan_principle_i * max0(-4 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      + (7 # 81) * s V_susan_principle_i * max0(-3 + s V_susan_principle__tmp
                                                - s V_susan_principle_i)
      + (2 # 25) * s V_susan_principle_i * max0(-3 + s V_susan_principle_i)
      - (1 # 6) * s V_susan_principle_i * max0(-1 + s V_susan_principle_z)
      + s V_susan_principle_z
      - (1 # 6) * s V_susan_principle_z * max0(-6 + s V_susan_principle__tmp)
      + (1 # 6) * s V_susan_principle_z * max0(-3 + s V_susan_principle_i)
      + (1 # 26) * max0(-7 + s V_susan_principle__tmp)
      + (22 # 49) * max0(-6 + s V_susan_principle__tmp)
      - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-4
                                                              + s V_susan_principle__tmp
                                                              - s V_susan_principle_i)
      - (1 # 23) * max0(-6 + s V_susan_principle__tmp) * max0(-3
                                                              + s V_susan_principle_i)
      - (10 # 81) * max0(-6 + s V_susan_principle__tmp) * max0(3
                                                               - s V_susan_principle__tmp
                                                               + s V_susan_principle_i)
      + max0(-6 + s V_susan_principle__tmp1) * max0(-4
                                                    + s V_susan_principle__tmp
                                                    - s V_susan_principle_i)
      + (1 # 2) * max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i)
      + (7 # 81) * max0(-4 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle_i)
      - (48 # 121) * max0(-3 + s V_susan_principle__tmp
                          - s V_susan_principle_i)
      - (1 # 78) * max0(-3 + s V_susan_principle__tmp - s V_susan_principle_i) * max0(-3
                                                                    + s V_susan_principle_i)
      + (10 # 81) * max0(-3 + s V_susan_principle__tmp
                         - s V_susan_principle_i) * max0(3
                                                         - s V_susan_principle__tmp
                                                         + s V_susan_principle_i)
      + max0(-3 + s V_susan_principle__tmp1 - s V_susan_principle_j)
      - (24 # 107) * max0(-3 + s V_susan_principle_i)
      + (10 # 81) * max0(-3 + s V_susan_principle_i) * max0(3
                                                            - s V_susan_principle__tmp
                                                            + s V_susan_principle_i)
      - (1 # 2) * max0(-1 + s V_susan_principle_z) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_susan_principle =>
    [mkPA Q (fun n z s => ai_susan_principle n s /\ annot0_susan_principle n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_susan_principle (proc_start P_susan_principle) s1 (proc_end P_susan_principle) s2 ->
    (s2 V_susan_principle_z <= max0(-6 + s1 V_susan_principle_x_size) * max0(-6
                                                                    + s1 V_susan_principle_y_size)
                               + max0(-6 + s1 V_susan_principle_y_size))%Q.
Proof.
  prove_bound ipa admissible_ipa P_susan_principle.
Qed.
