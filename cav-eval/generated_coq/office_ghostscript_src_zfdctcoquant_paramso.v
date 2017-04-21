Require Import pasta.Pasta.

Inductive proc: Type :=
  P_quant_params.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_quant_params_z := 1%positive.
Notation V_quant_params__tmp := 2%positive.
Notation V_quant_params__tmp1 := 3%positive.
Notation V_quant_params_i := 4%positive.
Notation V_quant_params_QFactor := 5%positive.
Notation V_quant_params_count := 6%positive.
Notation V_quant_params_op := 7%positive.
Notation V_quant_params_pvals := 8%positive.
Definition Pedges_quant_params: list (edge proc) :=
  (EA 1 (AAssign V_quant_params_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_quant_params__tmp1 (Some (EVar V_quant_params_count))) 3)::
  (EA 3 AWeaken 4)::(EA 4 ANone 77)::(EA 4 ANone 51)::(EA 4 ANone 12)::
  (EA 4 ANone 6)::(EA 4 ANone 5)::(EA 5 AWeaken 7)::(EA 6 AWeaken 7)::
  (EA 7 ANone 11)::(EA 7 ANone 8)::(EA 8 (AAssign V_quant_params__tmp
  (Some (ENum (-7)))) 9)::(EA 9 ANone 10)::(EA 10 AWeaken 80)::
  (EA 11 ANone 18)::(EA 12 AWeaken 13)::(EA 13 ANone 17)::(EA 13 ANone 14)::
  (EA 14 (AAssign V_quant_params__tmp (Some (ENum (-7)))) 15)::
  (EA 15 ANone 16)::(EA 16 AWeaken 80)::(EA 17 ANone 18)::(EA 18 (AAssign
  V_quant_params_i (Some (ENum (0)))) 19)::(EA 19 ANone 20)::
  (EA 20 AWeaken 21)::(EA 21 (AGuard (fun s => ((eval (EVar V_quant_params_i)
  s) < (eval (EVar V_quant_params__tmp1) s))%Z)) 26)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_quant_params_i) s) >=
  (eval (EVar V_quant_params__tmp1) s))%Z)) 22)::(EA 22 AWeaken 23)::
  (EA 23 (AAssign V_quant_params__tmp (Some (ENum (0)))) 24)::
  (EA 24 ANone 25)::(EA 25 AWeaken 80)::(EA 26 AWeaken 27)::
  (EA 27 ANone 48)::(EA 27 ANone 30)::(EA 27 ANone 28)::(EA 28 ANone 29)::
  (EA 29 AWeaken 32)::(EA 30 ANone 31)::(EA 31 AWeaken 32)::
  (EA 32 ANone 34)::(EA 32 ANone 33)::(EA 33 AWeaken 36)::(EA 34 ANone 35)::
  (EA 35 AWeaken 36)::(EA 36 ANone 37)::(EA 36 ANone 38)::(EA 37 ANone 38)::
  (EA 38 ANone 39)::(EA 39 AWeaken 40)::(EA 40 ANone 42)::(EA 40 ANone 41)::
  (EA 41 ANone 43)::(EA 42 ANone 43)::(EA 43 (AAssign V_quant_params_i
  (Some (EAdd (EVar V_quant_params_i) (ENum (1))))) 44)::(EA 44 ANone 45)::
  (EA 45 ANone 46)::(EA 46 (AAssign V_quant_params_z (Some (EAdd (ENum (1))
  (EVar V_quant_params_z)))) 47)::(EA 47 AWeaken 21)::(EA 48 (AAssign
  V_quant_params__tmp (Some (ENum (-20)))) 49)::(EA 49 ANone 50)::
  (EA 50 AWeaken 80)::(EA 51 AWeaken 52)::(EA 52 ANone 56)::
  (EA 52 ANone 53)::(EA 53 (AAssign V_quant_params__tmp
  (Some (ENum (-7)))) 54)::(EA 54 ANone 55)::(EA 55 AWeaken 80)::
  (EA 56 (AAssign V_quant_params_i (Some (ENum (0)))) 57)::(EA 57 ANone 58)::
  (EA 58 AWeaken 59)::(EA 59 (AGuard (fun s => ((eval (EVar V_quant_params_i)
  s) < (eval (EVar V_quant_params__tmp1) s))%Z)) 64)::(EA 59 (AGuard
  (fun s => ((eval (EVar V_quant_params_i) s) >=
  (eval (EVar V_quant_params__tmp1) s))%Z)) 60)::(EA 60 AWeaken 61)::
  (EA 61 (AAssign V_quant_params__tmp (Some (ENum (0)))) 62)::
  (EA 62 ANone 63)::(EA 63 AWeaken 80)::(EA 64 AWeaken 65)::
  (EA 65 ANone 67)::(EA 65 ANone 66)::(EA 66 AWeaken 69)::(EA 67 ANone 68)::
  (EA 68 AWeaken 69)::(EA 69 ANone 70)::(EA 69 ANone 71)::(EA 70 ANone 71)::
  (EA 71 ANone 72)::(EA 72 (AAssign V_quant_params_i
  (Some (EAdd (EVar V_quant_params_i) (ENum (1))))) 73)::(EA 73 ANone 74)::
  (EA 74 ANone 75)::(EA 75 (AAssign V_quant_params_z (Some (EAdd (ENum (1))
  (EVar V_quant_params_z)))) 76)::(EA 76 AWeaken 59)::(EA 77 (AAssign
  V_quant_params__tmp (Some (ENum (-20)))) 78)::(EA 78 ANone 79)::
  (EA 79 AWeaken 80)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_quant_params => Pedges_quant_params
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_quant_params => 80
     end)%positive;
  var_global := var_global
}.

Definition ai_quant_params (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_z <= 0)%Z
   | 3 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 4 => (1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_z <= 0)%Z
   | 5 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 6 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 7 => (1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_z <= 0)%Z
   | 8 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 9 => (1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params__tmp + 7 <= 0 /\ -1 * s V_quant_params__tmp + -7 <= 0)%Z
   | 10 => (-1 * s V_quant_params__tmp + -7 <= 0 /\ 1 * s V_quant_params__tmp + 7 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 11 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 12 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 13 => (1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_z <= 0)%Z
   | 14 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 15 => (1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params__tmp + 7 <= 0 /\ -1 * s V_quant_params__tmp + -7 <= 0)%Z
   | 16 => (-1 * s V_quant_params__tmp + -7 <= 0 /\ 1 * s V_quant_params__tmp + 7 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 17 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 18 => (1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_z <= 0)%Z
   | 19 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 20 => (-1 * s V_quant_params_i <= 0 /\ 1 * s V_quant_params_i <= 0 /\ 1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_z <= 0)%Z
   | 21 => (-1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 22 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params__tmp1+ -1 * s V_quant_params_i <= 0)%Z
   | 23 => (1 * s V_quant_params__tmp1+ -1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 24 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params__tmp1+ -1 * s V_quant_params_i <= 0 /\ 1 * s V_quant_params__tmp <= 0 /\ -1 * s V_quant_params__tmp <= 0)%Z
   | 25 => (-1 * s V_quant_params__tmp <= 0 /\ 1 * s V_quant_params__tmp <= 0 /\ 1 * s V_quant_params__tmp1+ -1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 26 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 27 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 28 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 29 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 30 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 31 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 32 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 33 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 34 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 35 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 36 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 37 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 38 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 39 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 40 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 41 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 42 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 43 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 44 => (-1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_i + 1 <= 0)%Z
   | 45 => (-1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0)%Z
   | 46 => (-1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_i + 1 <= 0)%Z
   | 47 => (-1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z + 1 <= 0)%Z
   | 48 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 49 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0 /\ 1 * s V_quant_params__tmp + 20 <= 0 /\ -1 * s V_quant_params__tmp + -20 <= 0)%Z
   | 50 => (-1 * s V_quant_params__tmp + -20 <= 0 /\ 1 * s V_quant_params__tmp + 20 <= 0 /\ -1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 51 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 52 => (1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_z <= 0)%Z
   | 53 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 54 => (1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params__tmp + 7 <= 0 /\ -1 * s V_quant_params__tmp + -7 <= 0)%Z
   | 55 => (-1 * s V_quant_params__tmp + -7 <= 0 /\ 1 * s V_quant_params__tmp + 7 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 56 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 57 => (1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 58 => (-1 * s V_quant_params_i <= 0 /\ 1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 59 => (-1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 60 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params__tmp1+ -1 * s V_quant_params_i <= 0)%Z
   | 61 => (1 * s V_quant_params__tmp1+ -1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 62 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params__tmp1+ -1 * s V_quant_params_i <= 0 /\ 1 * s V_quant_params__tmp <= 0 /\ -1 * s V_quant_params__tmp <= 0)%Z
   | 63 => (-1 * s V_quant_params__tmp <= 0 /\ 1 * s V_quant_params__tmp <= 0 /\ 1 * s V_quant_params__tmp1+ -1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 64 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 65 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 66 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 67 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 68 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 69 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 70 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 71 => (-1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0)%Z
   | 72 => (-1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_i <= 0)%Z
   | 73 => (-1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_i + 1 <= 0)%Z
   | 74 => (-1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z <= 0)%Z
   | 75 => (-1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_i + 1 <= 0)%Z
   | 76 => (-1 * s V_quant_params_i + 1 <= 0 /\ -1 * s V_quant_params__tmp1+ 1 * s V_quant_params_i <= 0 /\ -1 * s V_quant_params_z + 1 <= 0)%Z
   | 77 => (-1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 78 => (1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params__tmp + 20 <= 0 /\ -1 * s V_quant_params__tmp + -20 <= 0)%Z
   | 79 => (-1 * s V_quant_params__tmp + -20 <= 0 /\ 1 * s V_quant_params__tmp + 20 <= 0 /\ -1 * s V_quant_params_z <= 0 /\ 1 * s V_quant_params_z <= 0)%Z
   | 80 => (1 * s V_quant_params__tmp <= 0 /\ -1 * s V_quant_params_z <= 0 /\ -1 * s V_quant_params__tmp + -20 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_quant_params (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_quant_params_count) <= z)%Q
   | 2 => (max0(s V_quant_params_count) + max0(s V_quant_params_z) <= z)%Q
   | 3 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 4 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 5 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 6 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 7 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 8 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 9 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_ge_0 (s V_quant_params__tmp1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_params_z)) (F_check_ge (s V_quant_params_z) (0))]
     (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 11 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 12 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 13 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 14 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 15 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_ge_0 (s V_quant_params__tmp1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_params_z)) (F_check_ge (s V_quant_params_z) (0))]
     (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 17 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 18 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 19 => (max0(s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params_z) <= z)%Q
   | 20 => (max0(s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params_z) <= z)%Q
   | 21 => (max0(s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params_z) <= z)%Q
   | 22 => (max0(s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params_z) <= z)%Q
   | 23 => (max0(s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params_z) <= z)%Q
   | 24 => (max0(s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params_z) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_quant_params__tmp1
                                             - s V_quant_params_i) (-1
                                                                    + 
                                                                    s V_quant_params__tmp1
                                                                    - 
                                                                    s V_quant_params_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_quant_params__tmp1 - s V_quant_params_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_params_z)) (F_check_ge (s V_quant_params_z) (0))]
     (max0(s V_quant_params__tmp1 - s V_quant_params_i)
      + max0(s V_quant_params_z) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_params_z)) (F_check_ge (s V_quant_params_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              + s V_quant_params__tmp1
                                                              - s V_quant_params_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quant_params__tmp1
                                                                    - s V_quant_params_i))]
     (max0(s V_quant_params__tmp1 - s V_quant_params_i)
      + max0(s V_quant_params_z) <= z)%Q
   | 27 => ((1 # 1) - s V_quant_params__tmp1 + s V_quant_params_i
            + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 28 => ((1 # 1) - s V_quant_params__tmp1 + s V_quant_params_i
            + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 29 => ((1 # 1) - s V_quant_params__tmp1 + s V_quant_params_i
            + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 30 => ((1 # 1) - s V_quant_params__tmp1 + s V_quant_params_i
            + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 31 => ((1 # 1) - s V_quant_params__tmp1 + s V_quant_params_i
            + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 32 => ((1 # 1) - s V_quant_params__tmp1 + s V_quant_params_i
            + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_params__tmp1
                                                   - s V_quant_params_i)) (F_check_ge (s V_quant_params__tmp1
                                                                    - s V_quant_params_i) (0))]
     ((1 # 1) - s V_quant_params__tmp1 + s V_quant_params_i
      + s V_quant_params_z
      + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i)
      + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 34 => ((1 # 1) - s V_quant_params__tmp1 + s V_quant_params_i
            + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_quant_params__tmp1
                                       - s V_quant_params_i) (1);
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_quant_params__tmp1
                                                  - s V_quant_params_i)) (F_check_ge (-1
                                                                    + s V_quant_params__tmp1
                                                                    - s V_quant_params_i) (0))]
     ((1 # 1) - s V_quant_params__tmp1 + s V_quant_params_i
      + s V_quant_params_z
      + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i)
      + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 36 => ((1 # 1) + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 37 => ((1 # 1) + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 38 => ((1 # 1) + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 39 => ((1 # 1) + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 40 => ((1 # 1) + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 41 => ((1 # 1) + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 42 => ((1 # 1) + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 43 => ((1 # 1) + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 44 => ((1 # 1) + s V_quant_params_z
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 45 => ((1 # 1) + s V_quant_params_z
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 46 => ((1 # 1) + s V_quant_params_z
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_params_z) (0))) (F_max0_ge_0 (s V_quant_params_z))]
     (s V_quant_params_z + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 48 => ((1 # 1) - s V_quant_params__tmp1 + s V_quant_params_i
            + s V_quant_params_z
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 49 => (-s V_quant_params__tmp1 + s V_quant_params_i + s V_quant_params_z
            + (1 # 13) * max0(-7 - s V_quant_params__tmp)
            + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i)
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 50 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_quant_params__tmp1 - s V_quant_params_i);
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_params__tmp1
                                                  - s V_quant_params_i)) (F_check_ge (s V_quant_params__tmp1
                                                                    - s V_quant_params_i) (0));
      (*-0.0769231 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                         - s V_quant_params__tmp)) (F_check_ge (0) (0))]
     (-s V_quant_params__tmp1 + s V_quant_params_i + s V_quant_params_z
      + (1 # 13) * max0(-7 - s V_quant_params__tmp)
      + max0(-1 + s V_quant_params__tmp1 - s V_quant_params_i)
      + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 51 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_params_z)) (F_check_ge (s V_quant_params_z) (0))]
     (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 52 => (s V_quant_params_z + max0(s V_quant_params__tmp1) <= z)%Q
   | 53 => (s V_quant_params_z + max0(s V_quant_params__tmp1) <= z)%Q
   | 54 => (s V_quant_params_z + max0(s V_quant_params__tmp1) <= z)%Q
   | 55 => hints
     [(*-1 0*) F_max0_ge_0 (s V_quant_params__tmp1)]
     (s V_quant_params_z + max0(s V_quant_params__tmp1) <= z)%Q
   | 56 => (s V_quant_params_z + max0(s V_quant_params__tmp1) <= z)%Q
   | 57 => (s V_quant_params_z
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 58 => (s V_quant_params_z
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 59 => (s V_quant_params_z
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 60 => (s V_quant_params_z
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 61 => (s V_quant_params_z
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 62 => (s V_quant_params_z
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 63 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_quant_params__tmp1
                                             - s V_quant_params_i) (-1
                                                                    + 
                                                                    s V_quant_params__tmp1
                                                                    - 
                                                                    s V_quant_params_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_quant_params__tmp1 - s V_quant_params_i)]
     (s V_quant_params_z + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 64 => (s V_quant_params_z
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 65 => (s V_quant_params_z
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 66 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_params__tmp1
                                                   - s V_quant_params_i)) (F_check_ge (s V_quant_params__tmp1
                                                                    - s V_quant_params_i) (0))]
     (s V_quant_params_z + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 67 => (s V_quant_params_z
            + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 68 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quant_params__tmp1
                                                   - s V_quant_params_i)) (F_check_ge (s V_quant_params__tmp1
                                                                    - s V_quant_params_i) (0))]
     (s V_quant_params_z + max0(s V_quant_params__tmp1 - s V_quant_params_i) <= z)%Q
   | 69 => (s V_quant_params__tmp1 - s V_quant_params_i + s V_quant_params_z <= z)%Q
   | 70 => (s V_quant_params__tmp1 - s V_quant_params_i + s V_quant_params_z <= z)%Q
   | 71 => (s V_quant_params__tmp1 - s V_quant_params_i + s V_quant_params_z <= z)%Q
   | 72 => (s V_quant_params__tmp1 - s V_quant_params_i + s V_quant_params_z <= z)%Q
   | 73 => ((1 # 1) + s V_quant_params__tmp1 - s V_quant_params_i
            + s V_quant_params_z <= z)%Q
   | 74 => ((1 # 1) + s V_quant_params__tmp1 - s V_quant_params_i
            + s V_quant_params_z <= z)%Q
   | 75 => ((1 # 1) + s V_quant_params__tmp1 - s V_quant_params_i
            + s V_quant_params_z <= z)%Q
   | 76 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quant_params__tmp1
                                                               - s V_quant_params_i) (0))) (F_max0_ge_0 (s V_quant_params__tmp1
                                                                    - s V_quant_params_i))]
     (s V_quant_params__tmp1 - s V_quant_params_i + s V_quant_params_z <= z)%Q
   | 77 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 78 => (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 79 => hints
     [(*-1 0*) F_max0_ge_0 (s V_quant_params__tmp1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_quant_params_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_quant_params_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_quant_params_z) (0))) (F_max0_ge_0 (-
                                                                    s V_quant_params_z))]
     (max0(s V_quant_params__tmp1) + max0(s V_quant_params_z) <= z)%Q
   | 80 => (s V_quant_params_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_quant_params =>
    [mkPA Q (fun n z s => ai_quant_params n s /\ annot0_quant_params n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_quant_params (proc_start P_quant_params) s1 (proc_end P_quant_params) s2 ->
    (s2 V_quant_params_z <= max0(s1 V_quant_params_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_quant_params.
Qed.
