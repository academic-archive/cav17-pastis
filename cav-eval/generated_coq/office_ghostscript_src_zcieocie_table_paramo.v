Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cie_table_param.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cie_table_param_z := 1%positive.
Notation V_cie_table_param__tmp := 2%positive.
Notation V_cie_table_param_code := 3%positive.
Notation V_cie_table_param_d0 := 4%positive.
Notation V_cie_table_param_d1 := 5%positive.
Notation V_cie_table_param_i := 6%positive.
Notation V_cie_table_param_m := 7%positive.
Notation V_cie_table_param_n := 8%positive.
Notation V_cie_table_param_nbytes := 9%positive.
Notation V_cie_table_param_ntables := 10%positive.
Notation V_cie_table_param_pclt_dref_off0 := 11%positive.
Notation V_cie_table_param_pclt_dref_off20 := 12%positive.
Notation V_cie_table_param_pclt_dref_off4_off0 := 13%positive.
Notation V_cie_table_param_pclt_dref_off4_off4 := 14%positive.
Notation V_cie_table_param_mem := 15%positive.
Notation V_cie_table_param_pclt := 16%positive.
Notation V_cie_table_param_ptref := 17%positive.
Definition Pedges_cie_table_param: list (edge proc) :=
  (EA 1 (AAssign V_cie_table_param_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_cie_table_param_n (Some (EVar V_cie_table_param_pclt_dref_off0))) 3)::
  (EA 3 (AAssign V_cie_table_param_m
  (Some (EVar V_cie_table_param_pclt_dref_off20))) 4)::(EA 4 (AAssign
  V_cie_table_param_i (Some (ENum (0)))) 5)::(EA 5 ANone 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard (fun s => ((eval (EVar V_cie_table_param_i)
  s) < (eval (EVar V_cie_table_param_n) s))%Z)) 73)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_cie_table_param_i) s) >=
  (eval (EVar V_cie_table_param_n) s))%Z)) 8)::(EA 8 AWeaken 9)::
  (EA 9 (AAssign V_cie_table_param_nbytes None) 10)::(EA 10 AWeaken 11)::
  (EA 11 (AGuard (fun s => ((eval (EVar V_cie_table_param_n) s) =
  (eval (ENum (3)) s))%Z)) 56)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_cie_table_param_n) s) <> (eval (ENum (3))
  s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 (AAssign V_cie_table_param_d0
  (Some (EVar V_cie_table_param_pclt_dref_off4_off0))) 14)::(EA 14 (AAssign
  V_cie_table_param_d1
  (Some (EVar V_cie_table_param_pclt_dref_off4_off4))) 15)::(EA 15 (AAssign
  V_cie_table_param_ntables (Some (EMul (EVar V_cie_table_param_d0)
  (EVar V_cie_table_param_d1)))) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 26)::
  (EA 17 ANone 18)::(EA 18 AWeaken 19)::(EA 19 ANone 24)::(EA 19 ANone 20)::
  (EA 20 ANone 21)::(EA 21 (AAssign V_cie_table_param__tmp None) 22)::
  (EA 22 ANone 23)::(EA 23 AWeaken 91)::(EA 24 ANone 25)::
  (EA 25 AWeaken 91)::(EA 26 AWeaken 27)::(EA 27 ANone 53)::
  (EA 27 ANone 28)::(EA 28 AWeaken 29)::(EA 29 ANone 50)::(EA 29 ANone 30)::
  (EA 30 (AAssign V_cie_table_param_i (Some (ENum (0)))) 31)::
  (EA 31 ANone 32)::(EA 32 AWeaken 33)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_cie_table_param_i) s) <
  (eval (EVar V_cie_table_param_d0) s))%Z)) 35)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_cie_table_param_i) s) >=
  (eval (EVar V_cie_table_param_d0) s))%Z)) 34)::(EA 34 AWeaken 48)::
  (EA 35 AWeaken 36)::(EA 36 (AAssign V_cie_table_param_code None) 37)::
  (EA 37 AWeaken 38)::(EA 38 (AGuard
  (fun s => ((eval (EVar V_cie_table_param_code) s) < (eval (ENum (0))
  s))%Z)) 46)::(EA 38 (AGuard (fun s => ((eval (EVar V_cie_table_param_code)
  s) >= (eval (ENum (0)) s))%Z)) 39)::(EA 39 AWeaken 40)::(EA 40 ANone 41)::
  (EA 41 (AAssign V_cie_table_param_i (Some (EAdd (EVar V_cie_table_param_i)
  (ENum (1))))) 42)::(EA 42 ANone 43)::(EA 43 ANone 44)::(EA 44 (AAssign
  V_cie_table_param_z (Some (EAdd (ENum (1))
  (EVar V_cie_table_param_z)))) 45)::(EA 45 AWeaken 33)::(EA 46 AWeaken 47)::
  (EA 47 ANone 48)::(EA 48 ANone 49)::(EA 49 AWeaken 61)::(EA 50 (AAssign
  V_cie_table_param__tmp (Some (ENum (-25)))) 51)::(EA 51 ANone 52)::
  (EA 52 AWeaken 91)::(EA 53 (AAssign V_cie_table_param__tmp
  (Some (ENum (-15)))) 54)::(EA 54 ANone 55)::(EA 55 AWeaken 91)::
  (EA 56 AWeaken 57)::(EA 57 ANone 70)::(EA 57 ANone 58)::(EA 58 (AAssign
  V_cie_table_param_code None) 59)::(EA 59 ANone 60)::(EA 60 AWeaken 61)::
  (EA 61 (AGuard (fun s => ((eval (EVar V_cie_table_param_code) s) <
  (eval (ENum (0)) s))%Z)) 66)::(EA 61 (AGuard
  (fun s => ((eval (EVar V_cie_table_param_code) s) >= (eval (ENum (0))
  s))%Z)) 62)::(EA 62 AWeaken 63)::(EA 63 (AAssign V_cie_table_param__tmp
  (Some (ENum (0)))) 64)::(EA 64 ANone 65)::(EA 65 AWeaken 91)::
  (EA 66 AWeaken 67)::(EA 67 (AAssign V_cie_table_param__tmp
  (Some (EVar V_cie_table_param_code))) 68)::(EA 68 ANone 69)::
  (EA 69 AWeaken 91)::(EA 70 (AAssign V_cie_table_param__tmp
  (Some (ENum (-25)))) 71)::(EA 71 ANone 72)::(EA 72 AWeaken 91)::
  (EA 73 AWeaken 74)::(EA 74 ANone 78)::(EA 74 ANone 75)::(EA 75 (AAssign
  V_cie_table_param__tmp (Some (ENum (-20)))) 76)::(EA 76 ANone 77)::
  (EA 77 AWeaken 91)::(EA 78 AWeaken 79)::(EA 79 ANone 88)::
  (EA 79 ANone 80)::(EA 80 AWeaken 81)::(EA 81 ANone 88)::(EA 81 ANone 82)::
  (EA 82 ANone 83)::(EA 83 (AAssign V_cie_table_param_i
  (Some (EAdd (EVar V_cie_table_param_i) (ENum (1))))) 84)::
  (EA 84 ANone 85)::(EA 85 ANone 86)::(EA 86 (AAssign V_cie_table_param_z
  (Some (EAdd (ENum (1)) (EVar V_cie_table_param_z)))) 87)::
  (EA 87 AWeaken 7)::(EA 88 (AAssign V_cie_table_param__tmp
  (Some (ENum (-15)))) 89)::(EA 89 ANone 90)::(EA 90 AWeaken 91)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cie_table_param => Pedges_cie_table_param
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cie_table_param => 91
     end)%positive;
  var_global := var_global
}.

Definition ai_cie_table_param (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 3 => (-1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_z <= 0)%Z
   | 4 => (1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 5 => (-1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 6 => (-1 * s V_cie_table_param_i <= 0 /\ 1 * s V_cie_table_param_i <= 0 /\ 1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 7 => (-1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 8 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 9 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 10 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 11 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 12 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 13 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 14 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 15 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 16 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 17 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 18 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 19 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 20 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 21 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 22 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 23 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 24 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 25 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 26 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 27 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 28 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 29 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 30 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 31 => (-1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 32 => (-1 * s V_cie_table_param_i <= 0 /\ 1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 33 => (-1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 34 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_d0+ -1 * s V_cie_table_param_i <= 0)%Z
   | 35 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i + 1 <= 0)%Z
   | 36 => (-1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i + 1 <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 37 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i + 1 <= 0)%Z
   | 38 => (-1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i + 1 <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 39 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i + 1 <= 0 /\ -1 * s V_cie_table_param_code <= 0)%Z
   | 40 => (-1 * s V_cie_table_param_code <= 0 /\ -1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i + 1 <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 41 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i + 1 <= 0 /\ -1 * s V_cie_table_param_code <= 0)%Z
   | 42 => (-1 * s V_cie_table_param_code <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i + 1 <= 0 /\ -1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i <= 0)%Z
   | 43 => (-1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_i + 1 <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_code <= 0)%Z
   | 44 => (-1 * s V_cie_table_param_code <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i + 1 <= 0 /\ -1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i <= 0)%Z
   | 45 => (-1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_i + 1 <= 0 /\ -1 * s V_cie_table_param_code <= 0 /\ -1 * s V_cie_table_param_z + 1 <= 0)%Z
   | 46 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i + 1 <= 0 /\ 1 * s V_cie_table_param_code + 1 <= 0)%Z
   | 47 => (1 * s V_cie_table_param_code + 1 <= 0 /\ -1 * s V_cie_table_param_d0+ 1 * s V_cie_table_param_i + 1 <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 48 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 49 => (-1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 50 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 51 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ 1 * s V_cie_table_param__tmp + 25 <= 0 /\ -1 * s V_cie_table_param__tmp + -25 <= 0)%Z
   | 52 => (-1 * s V_cie_table_param__tmp + -25 <= 0 /\ 1 * s V_cie_table_param__tmp + 25 <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 53 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 54 => (-1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ 1 * s V_cie_table_param__tmp + 15 <= 0 /\ -1 * s V_cie_table_param__tmp + -15 <= 0)%Z
   | 55 => (-1 * s V_cie_table_param__tmp + -15 <= 0 /\ 1 * s V_cie_table_param__tmp + 15 <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0)%Z
   | 56 => (-1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ 1 * s V_cie_table_param_n + -3 <= 0 /\ -1 * s V_cie_table_param_n + 3 <= 0)%Z
   | 57 => (-1 * s V_cie_table_param_n + 3 <= 0 /\ 1 * s V_cie_table_param_n + -3 <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 58 => (-1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ 1 * s V_cie_table_param_n + -3 <= 0 /\ -1 * s V_cie_table_param_n + 3 <= 0)%Z
   | 59 => (-1 * s V_cie_table_param_n + 3 <= 0 /\ 1 * s V_cie_table_param_n + -3 <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 60 => (-1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ 1 * s V_cie_table_param_n + -3 <= 0 /\ -1 * s V_cie_table_param_n + 3 <= 0)%Z
   | 61 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 62 => (-1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_code <= 0)%Z
   | 63 => (-1 * s V_cie_table_param_code <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 64 => (-1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_code <= 0 /\ 1 * s V_cie_table_param__tmp <= 0 /\ -1 * s V_cie_table_param__tmp <= 0)%Z
   | 65 => (-1 * s V_cie_table_param__tmp <= 0 /\ 1 * s V_cie_table_param__tmp <= 0 /\ -1 * s V_cie_table_param_code <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 66 => (-1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ 1 * s V_cie_table_param_code + 1 <= 0)%Z
   | 67 => (1 * s V_cie_table_param_code + 1 <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 68 => (-1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ 1 * s V_cie_table_param_code + 1 <= 0 /\ 1 * s V_cie_table_param__tmp + 1 <= 0)%Z
   | 69 => (1 * s V_cie_table_param__tmp + 1 <= 0 /\ 1 * s V_cie_table_param_code + 1 <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 70 => (-1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ 1 * s V_cie_table_param_n + -3 <= 0 /\ -1 * s V_cie_table_param_n + 3 <= 0)%Z
   | 71 => (-1 * s V_cie_table_param_n + 3 <= 0 /\ 1 * s V_cie_table_param_n + -3 <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param__tmp + 25 <= 0 /\ -1 * s V_cie_table_param__tmp + -25 <= 0)%Z
   | 72 => (-1 * s V_cie_table_param__tmp + -25 <= 0 /\ 1 * s V_cie_table_param__tmp + 25 <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i+ 1 * s V_cie_table_param_n <= 0 /\ 1 * s V_cie_table_param_n + -3 <= 0 /\ -1 * s V_cie_table_param_n + 3 <= 0)%Z
   | 73 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0)%Z
   | 74 => (1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 75 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0)%Z
   | 76 => (1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ 1 * s V_cie_table_param__tmp + 20 <= 0 /\ -1 * s V_cie_table_param__tmp + -20 <= 0)%Z
   | 77 => (-1 * s V_cie_table_param__tmp + -20 <= 0 /\ 1 * s V_cie_table_param__tmp + 20 <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0)%Z
   | 78 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0)%Z
   | 79 => (1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 80 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0)%Z
   | 81 => (1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 82 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0)%Z
   | 83 => (1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | 84 => (-1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_i + 1 <= 0)%Z
   | 85 => (-1 * s V_cie_table_param_i + 1 <= 0 /\ 1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z <= 0)%Z
   | 86 => (-1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_i + 1 <= 0)%Z
   | 87 => (-1 * s V_cie_table_param_i + 1 <= 0 /\ 1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n <= 0 /\ -1 * s V_cie_table_param_z + 1 <= 0)%Z
   | 88 => (-1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0)%Z
   | 89 => (1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ 1 * s V_cie_table_param__tmp + 15 <= 0 /\ -1 * s V_cie_table_param__tmp + -15 <= 0)%Z
   | 90 => (-1 * s V_cie_table_param__tmp + -15 <= 0 /\ 1 * s V_cie_table_param__tmp + 15 <= 0 /\ -1 * s V_cie_table_param_i <= 0 /\ -1 * s V_cie_table_param_z <= 0 /\ 1 * s V_cie_table_param_i+ -1 * s V_cie_table_param_n + 1 <= 0)%Z
   | 91 => (-1 * s V_cie_table_param_z <= 0 /\ -1 * s V_cie_table_param_i <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cie_table_param (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_cie_table_param_pclt_dref_off0)
           + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 2 => (s V_cie_table_param_z + max0(s V_cie_table_param_pclt_dref_off0)
           + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 3 => (s V_cie_table_param_z + max0(s V_cie_table_param_n)
           + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 4 => (s V_cie_table_param_z + max0(s V_cie_table_param_n)
           + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 5 => (s V_cie_table_param_z
           + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
           + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 6 => (s V_cie_table_param_z
           + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
           + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 7 => (s V_cie_table_param_z
           + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
           + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 8 => (s V_cie_table_param_z
           + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
           + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 9 => (s V_cie_table_param_z
           + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
           + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 10 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 11 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 12 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 13 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 14 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 15 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 16 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 17 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 18 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 19 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 20 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 21 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 22 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_cie_table_param_i
                                             + s V_cie_table_param_n) (-1
                                                                    - s V_cie_table_param_i
                                                                    + s V_cie_table_param_n));
      (*-1 0*) F_max0_ge_0 (-1 - s V_cie_table_param_i
                            + s V_cie_table_param_n);
      (*-1 0*) F_max0_ge_0 (s V_cie_table_param_d0)]
     (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
      + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 24 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_cie_table_param_i
                                             + s V_cie_table_param_n) (-1
                                                                    - s V_cie_table_param_i
                                                                    + s V_cie_table_param_n));
      (*-1 0*) F_max0_ge_0 (-1 - s V_cie_table_param_i
                            + s V_cie_table_param_n);
      (*-1 0*) F_max0_ge_0 (s V_cie_table_param_d0)]
     (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
      + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 26 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 27 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 28 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_cie_table_param_i
                                             + s V_cie_table_param_n) (-1
                                                                    - s V_cie_table_param_i
                                                                    + s V_cie_table_param_n));
      (*-1 0*) F_max0_ge_0 (-1 - s V_cie_table_param_i
                            + s V_cie_table_param_n)]
     (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
      + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 29 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0) <= z)%Q
   | 30 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0) <= z)%Q
   | 31 => (s V_cie_table_param_z
            + max0(s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 32 => (s V_cie_table_param_z
            + max0(s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 33 => (s V_cie_table_param_z
            + max0(s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 34 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_cie_table_param_d0
                                            - s V_cie_table_param_i) (-1
                                                                    + s V_cie_table_param_d0
                                                                    - s V_cie_table_param_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_cie_table_param_d0
                            - s V_cie_table_param_i)]
     (s V_cie_table_param_z
      + max0(s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 35 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_cie_table_param_d0
                                      - s V_cie_table_param_i) (1)]
     (s V_cie_table_param_z
      + max0(s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 36 => ((1 # 1) + s V_cie_table_param_z
            + max0(-1 + s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 37 => ((1 # 1) + s V_cie_table_param_z
            + max0(-1 + s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 38 => ((1 # 1) + s V_cie_table_param_z
            + max0(-1 + s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 39 => ((1 # 1) + s V_cie_table_param_z
            + max0(-1 + s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 40 => ((1 # 1) + s V_cie_table_param_z
            + max0(-1 + s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 41 => ((1 # 1) + s V_cie_table_param_z
            + max0(-1 + s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 42 => ((1 # 1) + s V_cie_table_param_z
            + max0(s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 43 => ((1 # 1) + s V_cie_table_param_z
            + max0(s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 44 => ((1 # 1) + s V_cie_table_param_z
            + max0(s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 45 => (s V_cie_table_param_z
            + max0(s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 46 => hints
     [(*0 1*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_cie_table_param_d0
                                                 - s V_cie_table_param_i)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_cie_table_param_z
      + max0(-1 + s V_cie_table_param_d0 - s V_cie_table_param_i) <= z)%Q
   | 47 => (s V_cie_table_param_z <= z)%Q
   | 48 => (s V_cie_table_param_z <= z)%Q
   | 49 => (s V_cie_table_param_z <= z)%Q
   | 50 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0) <= z)%Q
   | 51 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0) <= z)%Q
   | 52 => hints
     [(*-1 0*) F_max0_ge_0 (s V_cie_table_param_d0)]
     (s V_cie_table_param_z + max0(s V_cie_table_param_d0) <= z)%Q
   | 53 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 54 => (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 55 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_cie_table_param_i
                                             + s V_cie_table_param_n) (-1
                                                                    - s V_cie_table_param_i
                                                                    + s V_cie_table_param_n));
      (*-1 0*) F_max0_ge_0 (-1 - s V_cie_table_param_i
                            + s V_cie_table_param_n);
      (*-1 0*) F_max0_ge_0 (s V_cie_table_param_d0)]
     (s V_cie_table_param_z + max0(s V_cie_table_param_d0)
      + max0(-s V_cie_table_param_i + s V_cie_table_param_n) <= z)%Q
   | 56 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 57 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 58 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 59 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 60 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_cie_table_param_i
                                             + s V_cie_table_param_n) (-1
                                                                    - s V_cie_table_param_i
                                                                    + s V_cie_table_param_n));
      (*-1 0*) F_max0_ge_0 (-1 - s V_cie_table_param_i
                            + s V_cie_table_param_n);
      (*-1 0*) F_max0_ge_0 (s V_cie_table_param_pclt_dref_off4_off0)]
     (s V_cie_table_param_z
      + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
      + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 61 => (s V_cie_table_param_z <= z)%Q
   | 62 => (s V_cie_table_param_z <= z)%Q
   | 63 => (s V_cie_table_param_z <= z)%Q
   | 64 => (s V_cie_table_param_z <= z)%Q
   | 65 => (s V_cie_table_param_z <= z)%Q
   | 66 => (s V_cie_table_param_z <= z)%Q
   | 67 => (s V_cie_table_param_z <= z)%Q
   | 68 => (s V_cie_table_param_z <= z)%Q
   | 69 => (s V_cie_table_param_z <= z)%Q
   | 70 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 71 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 72 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_cie_table_param_i
                                             + s V_cie_table_param_n) (-1
                                                                    - s V_cie_table_param_i
                                                                    + s V_cie_table_param_n));
      (*-1 0*) F_max0_ge_0 (-1 - s V_cie_table_param_i
                            + s V_cie_table_param_n);
      (*-1 0*) F_max0_ge_0 (s V_cie_table_param_pclt_dref_off4_off0)]
     (s V_cie_table_param_z
      + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
      + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 73 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 74 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 75 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 76 => (s V_cie_table_param_z
            + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 77 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_cie_table_param_i
                                             + s V_cie_table_param_n) (-1
                                                                    - s V_cie_table_param_i
                                                                    + s V_cie_table_param_n));
      (*-1 0*) F_max0_ge_0 (-1 - s V_cie_table_param_i
                            + s V_cie_table_param_n);
      (*-1 0*) F_max0_ge_0 (s V_cie_table_param_pclt_dref_off4_off0)]
     (s V_cie_table_param_z
      + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
      + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 78 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-s V_cie_table_param_i
                                                   + s V_cie_table_param_n)) (F_check_ge (-
                                                                    s V_cie_table_param_i
                                                                    + s V_cie_table_param_n) (0))]
     (s V_cie_table_param_z
      + max0(-s V_cie_table_param_i + s V_cie_table_param_n)
      + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 79 => (-s V_cie_table_param_i + s V_cie_table_param_n
            + s V_cie_table_param_z
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 80 => (-s V_cie_table_param_i + s V_cie_table_param_n
            + s V_cie_table_param_z
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 81 => (-s V_cie_table_param_i + s V_cie_table_param_n
            + s V_cie_table_param_z
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 82 => (-s V_cie_table_param_i + s V_cie_table_param_n
            + s V_cie_table_param_z
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 83 => (-s V_cie_table_param_i + s V_cie_table_param_n
            + s V_cie_table_param_z
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 84 => ((1 # 1) - s V_cie_table_param_i + s V_cie_table_param_n
            + s V_cie_table_param_z
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 85 => ((1 # 1) - s V_cie_table_param_i + s V_cie_table_param_n
            + s V_cie_table_param_z
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 86 => ((1 # 1) - s V_cie_table_param_i + s V_cie_table_param_n
            + s V_cie_table_param_z
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 87 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_cie_table_param_i
                                                               + s V_cie_table_param_n) (0))) (F_max0_ge_0 (-
                                                                    s V_cie_table_param_i
                                                                    + s V_cie_table_param_n))]
     (-s V_cie_table_param_i + s V_cie_table_param_n + s V_cie_table_param_z
      + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 88 => (-s V_cie_table_param_i + s V_cie_table_param_n
            + s V_cie_table_param_z
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 89 => (-(1 # 1) - s V_cie_table_param_i + s V_cie_table_param_n
            + s V_cie_table_param_z
            + (1 # 5) * max0(20 + s V_cie_table_param__tmp)
            + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 90 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_cie_table_param_i
                                       + s V_cie_table_param_n) (1);
      (*-1 0*) F_max0_ge_0 (-1 - s V_cie_table_param_i
                            + s V_cie_table_param_n);
      (*-1 0*) F_max0_ge_0 (s V_cie_table_param_pclt_dref_off4_off0);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_cie_table_param_i
                                                               + s V_cie_table_param_n) (0))) (F_max0_ge_0 (-
                                                                    s V_cie_table_param_i
                                                                    + s V_cie_table_param_n));
      (*-0.2 0*) F_binom_monotonic 1 (F_max0_ge_0 (20
                                                   + s V_cie_table_param__tmp)) (F_check_ge (0) (0))]
     (-(1 # 1) - s V_cie_table_param_i + s V_cie_table_param_n
      + s V_cie_table_param_z + (1 # 5) * max0(20 + s V_cie_table_param__tmp)
      + max0(s V_cie_table_param_pclt_dref_off4_off0) <= z)%Q
   | 91 => (s V_cie_table_param_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cie_table_param =>
    [mkPA Q (fun n z s => ai_cie_table_param n s /\ annot0_cie_table_param n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cie_table_param (proc_start P_cie_table_param) s1 (proc_end P_cie_table_param) s2 ->
    (s2 V_cie_table_param_z <= max0(s1 V_cie_table_param_pclt_dref_off0)
                               + max0(s1 V_cie_table_param_pclt_dref_off4_off0))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cie_table_param.
Qed.
