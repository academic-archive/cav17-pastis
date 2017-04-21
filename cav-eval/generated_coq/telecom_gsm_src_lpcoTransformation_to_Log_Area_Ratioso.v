Require Import pasta.Pasta.

Inductive proc: Type :=
  P_Transformation_to_Log_Area_Ratios.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_Transformation_to_Log_Area_Ratios_z := 1%positive.
Notation V_Transformation_to_Log_Area_Ratios_i := 2%positive.
Notation V_Transformation_to_Log_Area_Ratios_temp := 3%positive.
Notation V_Transformation_to_Log_Area_Ratios_r := 4%positive.
Definition Pedges_Transformation_to_Log_Area_Ratios: list (edge proc) :=
  (EA 1 (AAssign V_Transformation_to_Log_Area_Ratios_z
  (Some (ENum (0)))) 2)::(EA 2 (AAssign V_Transformation_to_Log_Area_Ratios_i
  (Some (ENum (1)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_Transformation_to_Log_Area_Ratios_i) s) <=
  (eval (ENum (8)) s))%Z)) 9)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_Transformation_to_Log_Area_Ratios_i) s) >
  (eval (ENum (8)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 61)::(EA 9 AWeaken 10)::(EA 10 (AAssign
  V_Transformation_to_Log_Area_Ratios_temp None) 11)::(EA 11 AWeaken 12)::
  (EA 12 (AGuard
  (fun s => ((eval (EVar V_Transformation_to_Log_Area_Ratios_temp) s) <
  (eval (ENum (0)) s))%Z)) 15)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_Transformation_to_Log_Area_Ratios_temp) s) >=
  (eval (ENum (0)) s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 ANone 22)::
  (EA 15 AWeaken 16)::(EA 16 (AGuard (fun s => True)) 20)::(EA 16 ANone 17)::
  (EA 17 ANone 18)::(EA 18 (AGuard (fun s => True)) 19)::(EA 19 AWeaken 22)::
  (EA 20 AWeaken 21)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_Transformation_to_Log_Area_Ratios_temp None) 23)::(EA 23 AWeaken 24)::
  (EA 24 (AGuard
  (fun s => ((eval (EVar V_Transformation_to_Log_Area_Ratios_temp) s) >=
  (eval (ENum (0)) s))%Z)) 28)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_Transformation_to_Log_Area_Ratios_temp) s) <
  (eval (ENum (0)) s))%Z)) 25)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::
  (EA 27 AWeaken 61)::(EA 28 AWeaken 29)::(EA 29 ANone 30)::
  (EA 30 AWeaken 31)::(EA 31 ANone 51)::(EA 31 ANone 32)::
  (EA 32 AWeaken 33)::(EA 33 ANone 42)::(EA 33 ANone 34)::
  (EA 34 AWeaken 35)::(EA 35 ANone 38)::(EA 35 ANone 36)::(EA 36 ANone 37)::
  (EA 37 AWeaken 61)::(EA 38 ANone 39)::(EA 39 (AAssign
  V_Transformation_to_Log_Area_Ratios_temp None) 40)::(EA 40 (AAssign
  V_Transformation_to_Log_Area_Ratios_temp None) 41)::(EA 41 ANone 49)::
  (EA 42 AWeaken 43)::(EA 43 ANone 46)::(EA 43 ANone 44)::(EA 44 ANone 45)::
  (EA 45 AWeaken 61)::(EA 46 ANone 47)::(EA 47 (AAssign
  V_Transformation_to_Log_Area_Ratios_temp None) 48)::(EA 48 ANone 49)::
  (EA 49 ANone 50)::(EA 50 AWeaken 54)::(EA 51 (AAssign
  V_Transformation_to_Log_Area_Ratios_temp None) 52)::(EA 52 ANone 53)::
  (EA 53 AWeaken 54)::(EA 54 ANone 57)::(EA 54 ANone 55)::(EA 55 ANone 56)::
  (EA 56 AWeaken 59)::(EA 57 ANone 58)::(EA 58 AWeaken 59)::
  (EA 59 ANone 62)::(EA 59 ANone 60)::(EA 60 AWeaken 61)::(EA 62 ANone 63)::
  (EA 63 ANone 64)::(EA 64 (AAssign V_Transformation_to_Log_Area_Ratios_i
  (Some (EAdd (EVar V_Transformation_to_Log_Area_Ratios_i)
  (ENum (1))))) 65)::(EA 65 ANone 66)::(EA 66 ANone 67)::(EA 67 (AAssign
  V_Transformation_to_Log_Area_Ratios_z (Some (EAdd (ENum (1))
  (EVar V_Transformation_to_Log_Area_Ratios_z)))) 68)::(EA 68 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_Transformation_to_Log_Area_Ratios => Pedges_Transformation_to_Log_Area_Ratios
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_Transformation_to_Log_Area_Ratios => 61
     end)%positive;
  var_global := var_global
}.

Definition ai_Transformation_to_Log_Area_Ratios (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0)%Z
   | 3 => (-1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 4 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -1 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0)%Z
   | 5 => (-1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -9 <= 0)%Z
   | 6 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -9 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 9 <= 0)%Z
   | 7 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 9 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -9 <= 0)%Z
   | 8 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -9 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 9 <= 0)%Z
   | 9 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 10 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 11 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 12 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 13 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0)%Z
   | 14 => (-1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 15 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_temp + 1 <= 0)%Z
   | 16 => (1 * s V_Transformation_to_Log_Area_Ratios_temp + 1 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 17 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_temp + 1 <= 0)%Z
   | 18 => (1 * s V_Transformation_to_Log_Area_Ratios_temp + 1 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 19 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_temp + 1 <= 0)%Z
   | 20 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_temp + 1 <= 0)%Z
   | 21 => (1 * s V_Transformation_to_Log_Area_Ratios_temp + 1 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 22 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 23 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 24 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 25 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_temp + 1 <= 0)%Z
   | 26 => (1 * s V_Transformation_to_Log_Area_Ratios_temp + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 27 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_temp + 1 <= 0)%Z
   | 28 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0)%Z
   | 29 => (-1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 30 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0)%Z
   | 31 => (-1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 32 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0)%Z
   | 33 => (-1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 34 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0)%Z
   | 35 => (-1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 36 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0)%Z
   | 37 => (-1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 38 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0)%Z
   | 39 => (-1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 40 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 41 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 42 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0)%Z
   | 43 => (-1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 44 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0)%Z
   | 45 => (-1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 46 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0)%Z
   | 47 => (-1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 48 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 49 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 50 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 51 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_temp <= 0)%Z
   | 52 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 53 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 54 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 55 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 56 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 57 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 58 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 59 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 60 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 61 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -9 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 62 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 63 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0)%Z
   | 64 => (-1 * s V_Transformation_to_Log_Area_Ratios_i + 1 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -8 <= 0)%Z
   | 65 => (-1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 2 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -9 <= 0)%Z
   | 66 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -9 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 2 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z <= 0)%Z
   | 67 => (-1 * s V_Transformation_to_Log_Area_Ratios_z <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 2 <= 0 /\ 1 * s V_Transformation_to_Log_Area_Ratios_i + -9 <= 0)%Z
   | 68 => (1 * s V_Transformation_to_Log_Area_Ratios_i + -9 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_i + 2 <= 0 /\ -1 * s V_Transformation_to_Log_Area_Ratios_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_Transformation_to_Log_Area_Ratios (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 3 => (max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
           + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 4 => (max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
           + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 5 => (max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
           + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 6 => (max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
           + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 7 => (max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
           + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (9
                                             - s V_Transformation_to_Log_Area_Ratios_i) (8
                                                                    - s V_Transformation_to_Log_Area_Ratios_i));
      (*-1 0*) F_max0_ge_0 (8 - s V_Transformation_to_Log_Area_Ratios_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_Transformation_to_Log_Area_Ratios_z)) (F_check_ge (s V_Transformation_to_Log_Area_Ratios_z) (0))]
     (max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
      + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 9 => (max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
           + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 10 => (max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
            + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 11 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                  - s V_Transformation_to_Log_Area_Ratios_i)) (F_check_ge (9
                                                                    - s V_Transformation_to_Log_Area_Ratios_i) (0))]
     (max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
      + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 12 => ((9 # 1) - s V_Transformation_to_Log_Area_Ratios_i
            + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (9
                                       - s V_Transformation_to_Log_Area_Ratios_i) (1)]
     ((9 # 1) - s V_Transformation_to_Log_Area_Ratios_i
      + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 14 => ((10 # 1) - s V_Transformation_to_Log_Area_Ratios_i
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i)
            - max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
            + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 15 => ((9 # 1) - s V_Transformation_to_Log_Area_Ratios_i
            + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 16 => ((9 # 1) - s V_Transformation_to_Log_Area_Ratios_i
            + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 17 => ((9 # 1) - s V_Transformation_to_Log_Area_Ratios_i
            + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 18 => ((9 # 1) - s V_Transformation_to_Log_Area_Ratios_i
            + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (9
                                       - s V_Transformation_to_Log_Area_Ratios_i) (1)]
     ((9 # 1) - s V_Transformation_to_Log_Area_Ratios_i
      + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (9
                                       - s V_Transformation_to_Log_Area_Ratios_i) (1)]
     ((9 # 1) - s V_Transformation_to_Log_Area_Ratios_i
      + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 21 => ((10 # 1) - s V_Transformation_to_Log_Area_Ratios_i
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i)
            - max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
            + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 22 => ((10 # 1) - s V_Transformation_to_Log_Area_Ratios_i
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i)
            - max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
            + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 23 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_Transformation_to_Log_Area_Ratios_z)) (F_check_ge (s V_Transformation_to_Log_Area_Ratios_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                               - s V_Transformation_to_Log_Area_Ratios_i) (0))) (F_max0_ge_0 (9
                                                                    - s V_Transformation_to_Log_Area_Ratios_i))]
     ((10 # 1) - s V_Transformation_to_Log_Area_Ratios_i
      + max0(8 - s V_Transformation_to_Log_Area_Ratios_i)
      - max0(9 - s V_Transformation_to_Log_Area_Ratios_i)
      + max0(s V_Transformation_to_Log_Area_Ratios_z) <= z)%Q
   | 24 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 25 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 26 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (8 - s V_Transformation_to_Log_Area_Ratios_i)]
     ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
      + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 28 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 29 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 30 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 31 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 32 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 33 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 34 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 35 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 36 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 37 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (8 - s V_Transformation_to_Log_Area_Ratios_i)]
     ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
      + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 38 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 39 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 40 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 41 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 42 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 43 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 44 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 45 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (8 - s V_Transformation_to_Log_Area_Ratios_i)]
     ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
      + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 46 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 47 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 48 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 49 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 50 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 51 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 52 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 53 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 54 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 55 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 56 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 57 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 58 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 59 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 60 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (8 - s V_Transformation_to_Log_Area_Ratios_i)]
     ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
      + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 61 => (s V_Transformation_to_Log_Area_Ratios_z <= z)%Q
   | 62 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 63 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 64 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(8 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 65 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(9 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 66 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(9 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 67 => ((1 # 1) + s V_Transformation_to_Log_Area_Ratios_z
            + max0(9 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | 68 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_Transformation_to_Log_Area_Ratios_z) (0))) (F_max0_ge_0 (s V_Transformation_to_Log_Area_Ratios_z))]
     (s V_Transformation_to_Log_Area_Ratios_z
      + max0(9 - s V_Transformation_to_Log_Area_Ratios_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_Transformation_to_Log_Area_Ratios =>
    [mkPA Q (fun n z s => ai_Transformation_to_Log_Area_Ratios n s /\ annot0_Transformation_to_Log_Area_Ratios n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_Transformation_to_Log_Area_Ratios (proc_start P_Transformation_to_Log_Area_Ratios) s1 (proc_end P_Transformation_to_Log_Area_Ratios) s2 ->
    (s2 V_Transformation_to_Log_Area_Ratios_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_Transformation_to_Log_Area_Ratios.
Qed.
