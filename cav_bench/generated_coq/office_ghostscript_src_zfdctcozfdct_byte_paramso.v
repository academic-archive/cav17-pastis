Require Import pasta.Pasta.

Inductive proc: Type :=
  P_zfdct_byte_params.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_zfdct_byte_params_z := 1%positive.
Notation V_zfdct_byte_params__tmp := 2%positive.
Notation V_zfdct_byte_params__tmp1 := 3%positive.
Notation V_zfdct_byte_params__tmp2 := 4%positive.
Notation V_zfdct_byte_params_i := 5%positive.
Notation V_zfdct_byte_params_count := 6%positive.
Notation V_zfdct_byte_params_op := 7%positive.
Notation V_zfdct_byte_params_pvals := 8%positive.
Notation V_zfdct_byte_params_start := 9%positive.
Definition Pedges_zfdct_byte_params: list (edge proc) :=
  (EA 1 (AAssign V_zfdct_byte_params_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_zfdct_byte_params__tmp2 (Some (EVar V_zfdct_byte_params_start))) 3)::
  (EA 3 (AAssign V_zfdct_byte_params__tmp1
  (Some (EVar V_zfdct_byte_params_count))) 4)::(EA 4 AWeaken 5)::
  (EA 5 ANone 93)::(EA 5 ANone 73)::(EA 5 ANone 28)::(EA 5 ANone 7)::
  (EA 5 ANone 6)::(EA 6 AWeaken 8)::(EA 7 AWeaken 8)::(EA 8 ANone 12)::
  (EA 8 ANone 9)::(EA 9 (AAssign V_zfdct_byte_params__tmp
  (Some (ENum (-7)))) 10)::(EA 10 ANone 11)::(EA 11 AWeaken 96)::
  (EA 12 (AAssign V_zfdct_byte_params_i (Some (ENum (0)))) 13)::
  (EA 13 ANone 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_zfdct_byte_params_i) s) <
  (eval (EVar V_zfdct_byte_params__tmp2) s))%Z)) 18)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_zfdct_byte_params_i) s) >=
  (eval (EVar V_zfdct_byte_params__tmp2) s))%Z)) 16)::(EA 16 AWeaken 17)::
  (EA 17 ANone 34)::(EA 18 AWeaken 19)::(EA 19 ANone 21)::(EA 19 ANone 20)::
  (EA 20 ANone 22)::(EA 21 ANone 22)::(EA 22 ANone 23)::(EA 23 (AAssign
  V_zfdct_byte_params_i (Some (EAdd (EVar V_zfdct_byte_params_i)
  (ENum (1))))) 24)::(EA 24 ANone 25)::(EA 25 ANone 26)::(EA 26 (AAssign
  V_zfdct_byte_params_z (Some (EAdd (ENum (1))
  (EVar V_zfdct_byte_params_z)))) 27)::(EA 27 AWeaken 15)::
  (EA 28 AWeaken 29)::(EA 29 ANone 33)::(EA 29 ANone 30)::(EA 30 (AAssign
  V_zfdct_byte_params__tmp (Some (ENum (-7)))) 31)::(EA 31 ANone 32)::
  (EA 32 AWeaken 96)::(EA 33 ANone 34)::(EA 34 (AAssign V_zfdct_byte_params_i
  (Some (ENum (0)))) 35)::(EA 35 ANone 36)::(EA 36 AWeaken 37)::
  (EA 37 (AGuard (fun s => ((eval (EVar V_zfdct_byte_params_i) s) <
  (eval (EVar V_zfdct_byte_params__tmp1) s))%Z)) 42)::(EA 37 (AGuard
  (fun s => ((eval (EVar V_zfdct_byte_params_i) s) >=
  (eval (EVar V_zfdct_byte_params__tmp1) s))%Z)) 38)::(EA 38 AWeaken 39)::
  (EA 39 (AAssign V_zfdct_byte_params__tmp (Some (ENum (0)))) 40)::
  (EA 40 ANone 41)::(EA 41 AWeaken 96)::(EA 42 AWeaken 43)::
  (EA 43 ANone 70)::(EA 43 ANone 52)::(EA 43 ANone 44)::(EA 44 AWeaken 45)::
  (EA 45 ANone 49)::(EA 45 ANone 46)::(EA 46 AWeaken 47)::(EA 47 ANone 49)::
  (EA 47 ANone 48)::(EA 48 ANone 57)::(EA 49 (AAssign
  V_zfdct_byte_params__tmp (Some (ENum (-15)))) 50)::(EA 50 ANone 51)::
  (EA 51 AWeaken 96)::(EA 52 AWeaken 53)::(EA 53 ANone 67)::
  (EA 53 ANone 54)::(EA 54 AWeaken 55)::(EA 55 ANone 67)::(EA 55 ANone 56)::
  (EA 56 ANone 57)::(EA 57 ANone 58)::(EA 58 AWeaken 59)::(EA 59 ANone 61)::
  (EA 59 ANone 60)::(EA 60 ANone 62)::(EA 61 ANone 62)::(EA 62 (AAssign
  V_zfdct_byte_params_i (Some (EAdd (EVar V_zfdct_byte_params_i)
  (ENum (1))))) 63)::(EA 63 ANone 64)::(EA 64 ANone 65)::(EA 65 (AAssign
  V_zfdct_byte_params_z (Some (EAdd (ENum (1))
  (EVar V_zfdct_byte_params_z)))) 66)::(EA 66 AWeaken 37)::(EA 67 (AAssign
  V_zfdct_byte_params__tmp (Some (ENum (-15)))) 68)::(EA 68 ANone 69)::
  (EA 69 AWeaken 96)::(EA 70 (AAssign V_zfdct_byte_params__tmp
  (Some (ENum (-20)))) 71)::(EA 71 ANone 72)::(EA 72 AWeaken 96)::
  (EA 73 AWeaken 74)::(EA 74 ANone 78)::(EA 74 ANone 75)::(EA 75 (AAssign
  V_zfdct_byte_params__tmp (Some (ENum (-7)))) 76)::(EA 76 ANone 77)::
  (EA 77 AWeaken 96)::(EA 78 (AAssign V_zfdct_byte_params_i
  (Some (ENum (0)))) 79)::(EA 79 ANone 80)::(EA 80 AWeaken 81)::
  (EA 81 (AGuard (fun s => ((eval (EVar V_zfdct_byte_params_i) s) <
  (eval (EVar V_zfdct_byte_params__tmp1) s))%Z)) 86)::(EA 81 (AGuard
  (fun s => ((eval (EVar V_zfdct_byte_params_i) s) >=
  (eval (EVar V_zfdct_byte_params__tmp1) s))%Z)) 82)::(EA 82 AWeaken 83)::
  (EA 83 (AAssign V_zfdct_byte_params__tmp (Some (ENum (0)))) 84)::
  (EA 84 ANone 85)::(EA 85 AWeaken 96)::(EA 86 AWeaken 87)::
  (EA 87 ANone 88)::(EA 88 (AAssign V_zfdct_byte_params_i
  (Some (EAdd (EVar V_zfdct_byte_params_i) (ENum (1))))) 89)::
  (EA 89 ANone 90)::(EA 90 ANone 91)::(EA 91 (AAssign V_zfdct_byte_params_z
  (Some (EAdd (ENum (1)) (EVar V_zfdct_byte_params_z)))) 92)::
  (EA 92 AWeaken 81)::(EA 93 (AAssign V_zfdct_byte_params__tmp
  (Some (ENum (-20)))) 94)::(EA 94 ANone 95)::(EA 95 AWeaken 96)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_zfdct_byte_params => Pedges_zfdct_byte_params
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_zfdct_byte_params => 96
     end)%positive;
  var_global := var_global
}.

Definition ai_zfdct_byte_params (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 3 => (-1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0)%Z
   | 4 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 5 => (-1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0)%Z
   | 6 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 7 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 8 => (-1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0)%Z
   | 9 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 10 => (-1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 7 <= 0 /\ -1 * s V_zfdct_byte_params__tmp + -7 <= 0)%Z
   | 11 => (-1 * s V_zfdct_byte_params__tmp + -7 <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 7 <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 12 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 13 => (-1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 14 => (-1 * s V_zfdct_byte_params_i <= 0 /\ 1 * s V_zfdct_byte_params_i <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 15 => (-1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 16 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params__tmp2+ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 17 => (1 * s V_zfdct_byte_params__tmp2+ -1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 18 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp2+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 19 => (-1 * s V_zfdct_byte_params__tmp2+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 20 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp2+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 21 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp2+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 22 => (-1 * s V_zfdct_byte_params__tmp2+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 23 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp2+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 24 => (-1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params__tmp2+ 1 * s V_zfdct_byte_params_i <= 0)%Z
   | 25 => (-1 * s V_zfdct_byte_params__tmp2+ 1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 26 => (-1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params__tmp2+ 1 * s V_zfdct_byte_params_i <= 0)%Z
   | 27 => (-1 * s V_zfdct_byte_params__tmp2+ 1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z + 1 <= 0)%Z
   | 28 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 29 => (-1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0)%Z
   | 30 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 31 => (-1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 7 <= 0 /\ -1 * s V_zfdct_byte_params__tmp + -7 <= 0)%Z
   | 32 => (-1 * s V_zfdct_byte_params__tmp + -7 <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 7 <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 33 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 34 => (-1 * s V_zfdct_byte_params_z <= 0)%Z
   | 35 => (-1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 36 => (-1 * s V_zfdct_byte_params_i <= 0 /\ 1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 37 => (-1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 38 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params__tmp1+ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 39 => (1 * s V_zfdct_byte_params__tmp1+ -1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 40 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params__tmp1+ -1 * s V_zfdct_byte_params_i <= 0 /\ 1 * s V_zfdct_byte_params__tmp <= 0 /\ -1 * s V_zfdct_byte_params__tmp <= 0)%Z
   | 41 => (-1 * s V_zfdct_byte_params__tmp <= 0 /\ 1 * s V_zfdct_byte_params__tmp <= 0 /\ 1 * s V_zfdct_byte_params__tmp1+ -1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 42 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 43 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 44 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 45 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 46 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 47 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 48 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 49 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 50 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 15 <= 0 /\ -1 * s V_zfdct_byte_params__tmp + -15 <= 0)%Z
   | 51 => (-1 * s V_zfdct_byte_params__tmp + -15 <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 15 <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 52 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 53 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 54 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 55 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 56 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 57 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 58 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 59 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 60 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 61 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 62 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 63 => (-1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 64 => (-1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 65 => (-1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 66 => (-1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z + 1 <= 0)%Z
   | 67 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 68 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 15 <= 0 /\ -1 * s V_zfdct_byte_params__tmp + -15 <= 0)%Z
   | 69 => (-1 * s V_zfdct_byte_params__tmp + -15 <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 15 <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 70 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 71 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 20 <= 0 /\ -1 * s V_zfdct_byte_params__tmp + -20 <= 0)%Z
   | 72 => (-1 * s V_zfdct_byte_params__tmp + -20 <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 20 <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 73 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 74 => (-1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0)%Z
   | 75 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 76 => (-1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 7 <= 0 /\ -1 * s V_zfdct_byte_params__tmp + -7 <= 0)%Z
   | 77 => (-1 * s V_zfdct_byte_params__tmp + -7 <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 7 <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 78 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 79 => (-1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 80 => (-1 * s V_zfdct_byte_params_i <= 0 /\ 1 * s V_zfdct_byte_params_i <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 81 => (-1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 82 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params__tmp1+ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 83 => (1 * s V_zfdct_byte_params__tmp1+ -1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 84 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params__tmp1+ -1 * s V_zfdct_byte_params_i <= 0 /\ 1 * s V_zfdct_byte_params__tmp <= 0 /\ -1 * s V_zfdct_byte_params__tmp <= 0)%Z
   | 85 => (-1 * s V_zfdct_byte_params__tmp <= 0 /\ 1 * s V_zfdct_byte_params__tmp <= 0 /\ 1 * s V_zfdct_byte_params__tmp1+ -1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 86 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 87 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i <= 0)%Z
   | 88 => (-1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i + 1 <= 0)%Z
   | 89 => (-1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i <= 0)%Z
   | 90 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 91 => (-1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i <= 0)%Z
   | 92 => (-1 * s V_zfdct_byte_params__tmp1+ 1 * s V_zfdct_byte_params_i <= 0 /\ -1 * s V_zfdct_byte_params_i + 1 <= 0 /\ -1 * s V_zfdct_byte_params_z + 1 <= 0)%Z
   | 93 => (1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 94 => (-1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 20 <= 0 /\ -1 * s V_zfdct_byte_params__tmp + -20 <= 0)%Z
   | 95 => (-1 * s V_zfdct_byte_params__tmp + -20 <= 0 /\ 1 * s V_zfdct_byte_params__tmp + 20 <= 0 /\ 1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0)%Z
   | 96 => (1 * s V_zfdct_byte_params__tmp <= 0 /\ -1 * s V_zfdct_byte_params_z <= 0 /\ -1 * s V_zfdct_byte_params__tmp + -20 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_zfdct_byte_params (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_zfdct_byte_params_count)
           + max0(s V_zfdct_byte_params_start) <= z)%Q
   | 2 => (max0(s V_zfdct_byte_params_count)
           + max0(s V_zfdct_byte_params_start)
           + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 3 => (max0(s V_zfdct_byte_params__tmp2)
           + max0(s V_zfdct_byte_params_count)
           + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 4 => (max0(s V_zfdct_byte_params__tmp1)
           + max0(s V_zfdct_byte_params__tmp2)
           + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 5 => (max0(s V_zfdct_byte_params__tmp1)
           + max0(s V_zfdct_byte_params__tmp2)
           + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 6 => (max0(s V_zfdct_byte_params__tmp1)
           + max0(s V_zfdct_byte_params__tmp2)
           + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 7 => (max0(s V_zfdct_byte_params__tmp1)
           + max0(s V_zfdct_byte_params__tmp2)
           + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 8 => (max0(s V_zfdct_byte_params__tmp1)
           + max0(s V_zfdct_byte_params__tmp2)
           + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 9 => (max0(s V_zfdct_byte_params__tmp1)
           + max0(s V_zfdct_byte_params__tmp2)
           + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 10 => (max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2)
            + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_ge_0 (s V_zfdct_byte_params__tmp1);
      (*-1 0*) F_max0_ge_0 (s V_zfdct_byte_params__tmp2);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zfdct_byte_params_z)) (F_check_ge (s V_zfdct_byte_params_z) (0))]
     (max0(s V_zfdct_byte_params__tmp1) + max0(s V_zfdct_byte_params__tmp2)
      + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 12 => (max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2)
            + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 13 => (max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zfdct_byte_params_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zfdct_byte_params_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zfdct_byte_params_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zfdct_byte_params_z))]
     (max0(s V_zfdct_byte_params__tmp1)
      + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
      + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 15 => (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_zfdct_byte_params__tmp2
                                             - s V_zfdct_byte_params_i) (-1
                                                                    + s V_zfdct_byte_params__tmp2
                                                                    - s V_zfdct_byte_params_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_zfdct_byte_params__tmp2
                            - s V_zfdct_byte_params_i)]
     (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1)
      + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 17 => (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_zfdct_byte_params__tmp2
                                       - s V_zfdct_byte_params_i) (1)]
     (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1)
      + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 19 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1) <= z)%Q
   | 20 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1) <= z)%Q
   | 21 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1) <= z)%Q
   | 22 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1) <= z)%Q
   | 23 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1) <= z)%Q
   | 24 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 25 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 26 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 27 => (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 28 => hints
     [(*-1 0*) F_max0_ge_0 (s V_zfdct_byte_params__tmp2);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zfdct_byte_params_z)) (F_check_ge (s V_zfdct_byte_params_z) (0))]
     (max0(s V_zfdct_byte_params__tmp1) + max0(s V_zfdct_byte_params__tmp2)
      + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 29 => (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1) <= z)%Q
   | 30 => (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1) <= z)%Q
   | 31 => (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_max0_ge_0 (s V_zfdct_byte_params__tmp1)]
     (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1) <= z)%Q
   | 33 => (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1) <= z)%Q
   | 34 => (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1) <= z)%Q
   | 35 => (s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 36 => (s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 37 => (s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 38 => (s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 39 => (s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 40 => (s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 41 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_zfdct_byte_params__tmp1
                                             - s V_zfdct_byte_params_i) (-1
                                                                    + s V_zfdct_byte_params__tmp1
                                                                    - s V_zfdct_byte_params_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_zfdct_byte_params__tmp1
                            - s V_zfdct_byte_params_i)]
     (s V_zfdct_byte_params_z
      + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 42 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              + s V_zfdct_byte_params__tmp1
                                                              - s V_zfdct_byte_params_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zfdct_byte_params__tmp1
                                                                    - s V_zfdct_byte_params_i))]
     (s V_zfdct_byte_params_z
      + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 43 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 44 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 45 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 46 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 47 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 48 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 49 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 50 => (-s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + (1 # 8) * max0(-7 - s V_zfdct_byte_params__tmp)
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 51 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zfdct_byte_params__tmp1
                                                   - s V_zfdct_byte_params_i)) (F_check_ge (s V_zfdct_byte_params__tmp1
                                                                    - s V_zfdct_byte_params_i) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                + s V_zfdct_byte_params__tmp1
                                                - s V_zfdct_byte_params_i)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                     - s V_zfdct_byte_params__tmp)) (F_check_ge (0) (0))]
     (-s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
      + s V_zfdct_byte_params_z
      + (1 # 8) * max0(-7 - s V_zfdct_byte_params__tmp)
      + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
      + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 52 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 53 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 54 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 55 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 56 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 57 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 58 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zfdct_byte_params__tmp1
                                                   - s V_zfdct_byte_params_i)) (F_check_ge (s V_zfdct_byte_params__tmp1
                                                                    - s V_zfdct_byte_params_i) (0))]
     ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
      + s V_zfdct_byte_params_z
      + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
      + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 59 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 60 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 61 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 62 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 63 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 64 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 65 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 66 => (s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 67 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 68 => (-s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + (1 # 8) * max0(-7 - s V_zfdct_byte_params__tmp)
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 69 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_zfdct_byte_params__tmp1
                                       - s V_zfdct_byte_params_i) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_zfdct_byte_params__tmp1
                            - s V_zfdct_byte_params_i);
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                  + s V_zfdct_byte_params__tmp1
                                                  - s V_zfdct_byte_params_i)) (F_check_ge (-1
                                                                    + s V_zfdct_byte_params__tmp1
                                                                    - s V_zfdct_byte_params_i) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                     - s V_zfdct_byte_params__tmp)) (F_check_ge (0) (0))]
     (-s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
      + s V_zfdct_byte_params_z
      + (1 # 8) * max0(-7 - s V_zfdct_byte_params__tmp)
      + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
      + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 70 => ((1 # 1) - s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 71 => (-s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
            + s V_zfdct_byte_params_z
            + (1 # 5) * max0(-15 - s V_zfdct_byte_params__tmp)
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 72 => hints
     [(*0 1*) F_max0_ge_0 (-1 + s V_zfdct_byte_params__tmp1
                           - s V_zfdct_byte_params_i);
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zfdct_byte_params__tmp1
                                                  - s V_zfdct_byte_params_i)) (F_check_ge (s V_zfdct_byte_params__tmp1
                                                                    - s V_zfdct_byte_params_i) (0));
      (*-0.2 0*) F_binom_monotonic 1 (F_max0_ge_0 (-15
                                                   - s V_zfdct_byte_params__tmp)) (F_check_ge (0) (0))]
     (-s V_zfdct_byte_params__tmp1 + s V_zfdct_byte_params_i
      + s V_zfdct_byte_params_z
      + (1 # 5) * max0(-15 - s V_zfdct_byte_params__tmp)
      + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
      + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 73 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zfdct_byte_params_z)) (F_check_ge (s V_zfdct_byte_params_z) (0))]
     (max0(s V_zfdct_byte_params__tmp1) + max0(s V_zfdct_byte_params__tmp2)
      + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 74 => (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2) <= z)%Q
   | 75 => (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2) <= z)%Q
   | 76 => (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2) <= z)%Q
   | 77 => hints
     [(*-1 0*) F_max0_ge_0 (s V_zfdct_byte_params__tmp1);
      (*-1 0*) F_max0_ge_0 (s V_zfdct_byte_params__tmp2)]
     (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1)
      + max0(s V_zfdct_byte_params__tmp2) <= z)%Q
   | 78 => (s V_zfdct_byte_params_z + max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2) <= z)%Q
   | 79 => (s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 80 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_zfdct_byte_params__tmp2
                                             - s V_zfdct_byte_params_i) (-1
                                                                    + s V_zfdct_byte_params__tmp2
                                                                    - s V_zfdct_byte_params_i))]
     (s V_zfdct_byte_params_z
      + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
      + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 81 => (s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 82 => (s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 83 => (s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 84 => (s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 85 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_zfdct_byte_params__tmp1
                                             - s V_zfdct_byte_params_i) (-1
                                                                    + s V_zfdct_byte_params__tmp1
                                                                    - s V_zfdct_byte_params_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_zfdct_byte_params__tmp1
                            - s V_zfdct_byte_params_i);
      (*-1 0*) F_max0_ge_0 (-1 + s V_zfdct_byte_params__tmp2
                            - s V_zfdct_byte_params_i)]
     (s V_zfdct_byte_params_z
      + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
      + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 86 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_zfdct_byte_params__tmp1
                                       - s V_zfdct_byte_params_i) (1)]
     (s V_zfdct_byte_params_z
      + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i)
      + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i) <= z)%Q
   | 87 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 88 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(-1 + s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(-1 + s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 89 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 90 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 91 => ((1 # 1) + s V_zfdct_byte_params_z
            + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
            + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 92 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_zfdct_byte_params__tmp2
                                             - s V_zfdct_byte_params_i) (-1
                                                                    + s V_zfdct_byte_params__tmp2
                                                                    - s V_zfdct_byte_params_i))]
     (s V_zfdct_byte_params_z
      + max0(s V_zfdct_byte_params__tmp1 - s V_zfdct_byte_params_i)
      + max0(s V_zfdct_byte_params__tmp2 - s V_zfdct_byte_params_i) <= z)%Q
   | 93 => (max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2)
            + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 94 => (max0(s V_zfdct_byte_params__tmp1)
            + max0(s V_zfdct_byte_params__tmp2)
            + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 95 => hints
     [(*-1 0*) F_max0_ge_0 (s V_zfdct_byte_params__tmp1);
      (*-1 0*) F_max0_ge_0 (s V_zfdct_byte_params__tmp2);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zfdct_byte_params_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zfdct_byte_params_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zfdct_byte_params_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zfdct_byte_params_z))]
     (max0(s V_zfdct_byte_params__tmp1) + max0(s V_zfdct_byte_params__tmp2)
      + max0(s V_zfdct_byte_params_z) <= z)%Q
   | 96 => (s V_zfdct_byte_params_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_zfdct_byte_params =>
    [mkPA Q (fun n z s => ai_zfdct_byte_params n s /\ annot0_zfdct_byte_params n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_zfdct_byte_params (proc_start P_zfdct_byte_params) s1 (proc_end P_zfdct_byte_params) s2 ->
    (s2 V_zfdct_byte_params_z <= max0(s1 V_zfdct_byte_params_count)
                                 + max0(s1 V_zfdct_byte_params_start))%Q.
Proof.
  prove_bound ipa admissible_ipa P_zfdct_byte_params.
Qed.
