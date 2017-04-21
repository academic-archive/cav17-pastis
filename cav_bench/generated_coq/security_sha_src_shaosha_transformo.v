Require Import pasta.Pasta.

Inductive proc: Type :=
  P_sha_transform.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_sha_transform_z := 1%positive.
Notation V_sha_transform_A := 2%positive.
Notation V_sha_transform_B := 3%positive.
Notation V_sha_transform_C := 4%positive.
Notation V_sha_transform_D := 5%positive.
Notation V_sha_transform_E := 6%positive.
Notation V_sha_transform_i := 7%positive.
Notation V_sha_transform_sha_info_dref_off0_off0 := 8%positive.
Notation V_sha_transform_sha_info_dref_off0_off16 := 9%positive.
Notation V_sha_transform_sha_info_dref_off0_off24 := 10%positive.
Notation V_sha_transform_sha_info_dref_off0_off32 := 11%positive.
Notation V_sha_transform_sha_info_dref_off0_off8 := 12%positive.
Notation V_sha_transform_temp := 13%positive.
Notation V_sha_transform_sha_info := 14%positive.
Definition Pedges_sha_transform: list (edge proc) :=
  (EA 1 (AAssign V_sha_transform_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_sha_transform_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_sha_transform_i) s) <
  (eval (ENum (16)) s))%Z)) 103)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_sha_transform_i) s) >= (eval (ENum (16))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign V_sha_transform_i
  (Some (ENum (16)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_sha_transform_i) s) < (eval (ENum (80))
  s))%Z)) 96)::(EA 10 (AGuard (fun s => ((eval (EVar V_sha_transform_i) s) >=
  (eval (ENum (80)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_sha_transform_A
  (Some (EVar V_sha_transform_sha_info_dref_off0_off0))) 13)::(EA 13 (AAssign
  V_sha_transform_B
  (Some (EVar V_sha_transform_sha_info_dref_off0_off8))) 14)::(EA 14 (AAssign
  V_sha_transform_C
  (Some (EVar V_sha_transform_sha_info_dref_off0_off16))) 15)::
  (EA 15 (AAssign V_sha_transform_D
  (Some (EVar V_sha_transform_sha_info_dref_off0_off24))) 16)::
  (EA 16 (AAssign V_sha_transform_E
  (Some (EVar V_sha_transform_sha_info_dref_off0_off32))) 17)::
  (EA 17 (AAssign V_sha_transform_i (Some (ENum (0)))) 18)::
  (EA 18 ANone 19)::(EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_sha_transform_i) s) < (eval (ENum (20))
  s))%Z)) 83)::(EA 20 (AGuard (fun s => ((eval (EVar V_sha_transform_i) s) >=
  (eval (ENum (20)) s))%Z)) 21)::(EA 21 AWeaken 22)::(EA 22 (AAssign
  V_sha_transform_i (Some (ENum (20)))) 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 25)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_sha_transform_i) s) < (eval (ENum (40))
  s))%Z)) 70)::(EA 25 (AGuard (fun s => ((eval (EVar V_sha_transform_i) s) >=
  (eval (ENum (40)) s))%Z)) 26)::(EA 26 AWeaken 27)::(EA 27 (AAssign
  V_sha_transform_i (Some (ENum (40)))) 28)::(EA 28 ANone 29)::
  (EA 29 AWeaken 30)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_sha_transform_i) s) < (eval (ENum (60))
  s))%Z)) 57)::(EA 30 (AGuard (fun s => ((eval (EVar V_sha_transform_i) s) >=
  (eval (ENum (60)) s))%Z)) 31)::(EA 31 AWeaken 32)::(EA 32 (AAssign
  V_sha_transform_i (Some (ENum (60)))) 33)::(EA 33 ANone 34)::
  (EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_sha_transform_i) s) < (eval (ENum (80))
  s))%Z)) 44)::(EA 35 (AGuard (fun s => ((eval (EVar V_sha_transform_i) s) >=
  (eval (ENum (80)) s))%Z)) 36)::(EA 36 AWeaken 37)::(EA 37 (AAssign
  V_sha_transform_sha_info_dref_off0_off0
  (Some (EAdd (EVar V_sha_transform_sha_info_dref_off0_off0)
  (EVar V_sha_transform_A)))) 38)::(EA 38 (AAssign
  V_sha_transform_sha_info_dref_off0_off8
  (Some (EAdd (EVar V_sha_transform_sha_info_dref_off0_off8)
  (EVar V_sha_transform_B)))) 39)::(EA 39 (AAssign
  V_sha_transform_sha_info_dref_off0_off16
  (Some (EAdd (EVar V_sha_transform_sha_info_dref_off0_off16)
  (EVar V_sha_transform_C)))) 40)::(EA 40 (AAssign
  V_sha_transform_sha_info_dref_off0_off24
  (Some (EAdd (EVar V_sha_transform_sha_info_dref_off0_off24)
  (EVar V_sha_transform_D)))) 41)::(EA 41 (AAssign
  V_sha_transform_sha_info_dref_off0_off32
  (Some (EAdd (EVar V_sha_transform_sha_info_dref_off0_off32)
  (EVar V_sha_transform_E)))) 42)::(EA 42 AWeaken 43)::(EA 44 AWeaken 45)::
  (EA 45 (AAssign V_sha_transform_temp None) 46)::(EA 46 (AAssign
  V_sha_transform_E (Some (EVar V_sha_transform_D))) 47)::(EA 47 (AAssign
  V_sha_transform_D (Some (EVar V_sha_transform_C))) 48)::(EA 48 (AAssign
  V_sha_transform_C None) 49)::(EA 49 (AAssign V_sha_transform_B
  (Some (EVar V_sha_transform_A))) 50)::(EA 50 (AAssign V_sha_transform_A
  (Some (EVar V_sha_transform_temp))) 51)::(EA 51 ANone 52)::(EA 52 (AAssign
  V_sha_transform_i (Some (EAdd (EVar V_sha_transform_i) (ENum (1))))) 53)::
  (EA 53 ANone 54)::(EA 54 ANone 55)::(EA 55 (AAssign V_sha_transform_z
  (Some (EAdd (ENum (1)) (EVar V_sha_transform_z)))) 56)::
  (EA 56 AWeaken 35)::(EA 57 AWeaken 58)::(EA 58 (AAssign
  V_sha_transform_temp None) 59)::(EA 59 (AAssign V_sha_transform_E
  (Some (EVar V_sha_transform_D))) 60)::(EA 60 (AAssign V_sha_transform_D
  (Some (EVar V_sha_transform_C))) 61)::(EA 61 (AAssign V_sha_transform_C
  None) 62)::(EA 62 (AAssign V_sha_transform_B
  (Some (EVar V_sha_transform_A))) 63)::(EA 63 (AAssign V_sha_transform_A
  (Some (EVar V_sha_transform_temp))) 64)::(EA 64 ANone 65)::(EA 65 (AAssign
  V_sha_transform_i (Some (EAdd (EVar V_sha_transform_i) (ENum (1))))) 66)::
  (EA 66 ANone 67)::(EA 67 ANone 68)::(EA 68 (AAssign V_sha_transform_z
  (Some (EAdd (ENum (1)) (EVar V_sha_transform_z)))) 69)::
  (EA 69 AWeaken 30)::(EA 70 AWeaken 71)::(EA 71 (AAssign
  V_sha_transform_temp None) 72)::(EA 72 (AAssign V_sha_transform_E
  (Some (EVar V_sha_transform_D))) 73)::(EA 73 (AAssign V_sha_transform_D
  (Some (EVar V_sha_transform_C))) 74)::(EA 74 (AAssign V_sha_transform_C
  None) 75)::(EA 75 (AAssign V_sha_transform_B
  (Some (EVar V_sha_transform_A))) 76)::(EA 76 (AAssign V_sha_transform_A
  (Some (EVar V_sha_transform_temp))) 77)::(EA 77 ANone 78)::(EA 78 (AAssign
  V_sha_transform_i (Some (EAdd (EVar V_sha_transform_i) (ENum (1))))) 79)::
  (EA 79 ANone 80)::(EA 80 ANone 81)::(EA 81 (AAssign V_sha_transform_z
  (Some (EAdd (ENum (1)) (EVar V_sha_transform_z)))) 82)::
  (EA 82 AWeaken 25)::(EA 83 AWeaken 84)::(EA 84 (AAssign
  V_sha_transform_temp None) 85)::(EA 85 (AAssign V_sha_transform_E
  (Some (EVar V_sha_transform_D))) 86)::(EA 86 (AAssign V_sha_transform_D
  (Some (EVar V_sha_transform_C))) 87)::(EA 87 (AAssign V_sha_transform_C
  None) 88)::(EA 88 (AAssign V_sha_transform_B
  (Some (EVar V_sha_transform_A))) 89)::(EA 89 (AAssign V_sha_transform_A
  (Some (EVar V_sha_transform_temp))) 90)::(EA 90 ANone 91)::(EA 91 (AAssign
  V_sha_transform_i (Some (EAdd (EVar V_sha_transform_i) (ENum (1))))) 92)::
  (EA 92 ANone 93)::(EA 93 ANone 94)::(EA 94 (AAssign V_sha_transform_z
  (Some (EAdd (ENum (1)) (EVar V_sha_transform_z)))) 95)::
  (EA 95 AWeaken 20)::(EA 96 AWeaken 97)::(EA 97 ANone 98)::(EA 98 (AAssign
  V_sha_transform_i (Some (EAdd (EVar V_sha_transform_i) (ENum (1))))) 99)::
  (EA 99 ANone 100)::(EA 100 ANone 101)::(EA 101 (AAssign V_sha_transform_z
  (Some (EAdd (ENum (1)) (EVar V_sha_transform_z)))) 102)::
  (EA 102 AWeaken 10)::(EA 103 AWeaken 104)::(EA 104 ANone 105)::
  (EA 105 (AAssign V_sha_transform_i (Some (EAdd (EVar V_sha_transform_i)
  (ENum (1))))) 106)::(EA 106 ANone 107)::(EA 107 ANone 108)::
  (EA 108 (AAssign V_sha_transform_z (Some (EAdd (ENum (1))
  (EVar V_sha_transform_z)))) 109)::(EA 109 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_sha_transform => Pedges_sha_transform
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_sha_transform => 43
     end)%positive;
  var_global := var_global
}.

Definition ai_sha_transform (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 3 => (-1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i <= 0 /\ -1 * s V_sha_transform_i <= 0)%Z
   | 4 => (-1 * s V_sha_transform_i <= 0 /\ 1 * s V_sha_transform_i <= 0 /\ 1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 5 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i <= 0 /\ 1 * s V_sha_transform_i + -16 <= 0)%Z
   | 6 => (1 * s V_sha_transform_i + -16 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 16 <= 0)%Z
   | 7 => (-1 * s V_sha_transform_i + 16 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -16 <= 0)%Z
   | 8 => (-1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -16 <= 0 /\ -1 * s V_sha_transform_i + 16 <= 0)%Z
   | 9 => (-1 * s V_sha_transform_i + 16 <= 0 /\ 1 * s V_sha_transform_i + -16 <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 10 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 16 <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 11 => (1 * s V_sha_transform_i + -80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 80 <= 0)%Z
   | 12 => (-1 * s V_sha_transform_i + 80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 13 => (1 * s V_sha_transform_i + -80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 80 <= 0)%Z
   | 14 => (-1 * s V_sha_transform_i + 80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 15 => (1 * s V_sha_transform_i + -80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 80 <= 0)%Z
   | 16 => (-1 * s V_sha_transform_i + 80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 17 => (1 * s V_sha_transform_i + -80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 80 <= 0)%Z
   | 18 => (-1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i <= 0 /\ -1 * s V_sha_transform_i <= 0)%Z
   | 19 => (-1 * s V_sha_transform_i <= 0 /\ 1 * s V_sha_transform_i <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 20 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i <= 0 /\ 1 * s V_sha_transform_i + -20 <= 0)%Z
   | 21 => (1 * s V_sha_transform_i + -20 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 20 <= 0)%Z
   | 22 => (-1 * s V_sha_transform_i + 20 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -20 <= 0)%Z
   | 23 => (-1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -20 <= 0 /\ -1 * s V_sha_transform_i + 20 <= 0)%Z
   | 24 => (-1 * s V_sha_transform_i + 20 <= 0 /\ 1 * s V_sha_transform_i + -20 <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 25 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 20 <= 0 /\ 1 * s V_sha_transform_i + -40 <= 0)%Z
   | 26 => (1 * s V_sha_transform_i + -40 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 40 <= 0)%Z
   | 27 => (-1 * s V_sha_transform_i + 40 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -40 <= 0)%Z
   | 28 => (-1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -40 <= 0 /\ -1 * s V_sha_transform_i + 40 <= 0)%Z
   | 29 => (-1 * s V_sha_transform_i + 40 <= 0 /\ 1 * s V_sha_transform_i + -40 <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 30 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 40 <= 0 /\ 1 * s V_sha_transform_i + -60 <= 0)%Z
   | 31 => (1 * s V_sha_transform_i + -60 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 60 <= 0)%Z
   | 32 => (-1 * s V_sha_transform_i + 60 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -60 <= 0)%Z
   | 33 => (-1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -60 <= 0 /\ -1 * s V_sha_transform_i + 60 <= 0)%Z
   | 34 => (-1 * s V_sha_transform_i + 60 <= 0 /\ 1 * s V_sha_transform_i + -60 <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 35 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 60 <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 36 => (1 * s V_sha_transform_i + -80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 80 <= 0)%Z
   | 37 => (-1 * s V_sha_transform_i + 80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 38 => (1 * s V_sha_transform_i + -80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 80 <= 0)%Z
   | 39 => (-1 * s V_sha_transform_i + 80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 40 => (1 * s V_sha_transform_i + -80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 80 <= 0)%Z
   | 41 => (-1 * s V_sha_transform_i + 80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 42 => (1 * s V_sha_transform_i + -80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 80 <= 0)%Z
   | 43 => (-1 * s V_sha_transform_i + 80 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 44 => (-1 * s V_sha_transform_i + 60 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -79 <= 0)%Z
   | 45 => (1 * s V_sha_transform_i + -79 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 60 <= 0)%Z
   | 46 => (-1 * s V_sha_transform_i + 60 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -79 <= 0)%Z
   | 47 => (1 * s V_sha_transform_i + -79 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 60 <= 0)%Z
   | 48 => (-1 * s V_sha_transform_i + 60 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -79 <= 0)%Z
   | 49 => (1 * s V_sha_transform_i + -79 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 60 <= 0)%Z
   | 50 => (-1 * s V_sha_transform_i + 60 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -79 <= 0)%Z
   | 51 => (1 * s V_sha_transform_i + -79 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 60 <= 0)%Z
   | 52 => (-1 * s V_sha_transform_i + 60 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -79 <= 0)%Z
   | 53 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 61 <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 54 => (1 * s V_sha_transform_i + -80 <= 0 /\ -1 * s V_sha_transform_i + 61 <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 55 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 61 <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 56 => (1 * s V_sha_transform_i + -80 <= 0 /\ -1 * s V_sha_transform_i + 61 <= 0 /\ -1 * s V_sha_transform_z + 1 <= 0)%Z
   | 57 => (-1 * s V_sha_transform_i + 40 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -59 <= 0)%Z
   | 58 => (1 * s V_sha_transform_i + -59 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 40 <= 0)%Z
   | 59 => (-1 * s V_sha_transform_i + 40 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -59 <= 0)%Z
   | 60 => (1 * s V_sha_transform_i + -59 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 40 <= 0)%Z
   | 61 => (-1 * s V_sha_transform_i + 40 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -59 <= 0)%Z
   | 62 => (1 * s V_sha_transform_i + -59 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 40 <= 0)%Z
   | 63 => (-1 * s V_sha_transform_i + 40 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -59 <= 0)%Z
   | 64 => (1 * s V_sha_transform_i + -59 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 40 <= 0)%Z
   | 65 => (-1 * s V_sha_transform_i + 40 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -59 <= 0)%Z
   | 66 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 41 <= 0 /\ 1 * s V_sha_transform_i + -60 <= 0)%Z
   | 67 => (1 * s V_sha_transform_i + -60 <= 0 /\ -1 * s V_sha_transform_i + 41 <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 68 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 41 <= 0 /\ 1 * s V_sha_transform_i + -60 <= 0)%Z
   | 69 => (1 * s V_sha_transform_i + -60 <= 0 /\ -1 * s V_sha_transform_i + 41 <= 0 /\ -1 * s V_sha_transform_z + 1 <= 0)%Z
   | 70 => (-1 * s V_sha_transform_i + 20 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -39 <= 0)%Z
   | 71 => (1 * s V_sha_transform_i + -39 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 20 <= 0)%Z
   | 72 => (-1 * s V_sha_transform_i + 20 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -39 <= 0)%Z
   | 73 => (1 * s V_sha_transform_i + -39 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 20 <= 0)%Z
   | 74 => (-1 * s V_sha_transform_i + 20 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -39 <= 0)%Z
   | 75 => (1 * s V_sha_transform_i + -39 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 20 <= 0)%Z
   | 76 => (-1 * s V_sha_transform_i + 20 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -39 <= 0)%Z
   | 77 => (1 * s V_sha_transform_i + -39 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 20 <= 0)%Z
   | 78 => (-1 * s V_sha_transform_i + 20 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -39 <= 0)%Z
   | 79 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 21 <= 0 /\ 1 * s V_sha_transform_i + -40 <= 0)%Z
   | 80 => (1 * s V_sha_transform_i + -40 <= 0 /\ -1 * s V_sha_transform_i + 21 <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 81 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 21 <= 0 /\ 1 * s V_sha_transform_i + -40 <= 0)%Z
   | 82 => (1 * s V_sha_transform_i + -40 <= 0 /\ -1 * s V_sha_transform_i + 21 <= 0 /\ -1 * s V_sha_transform_z + 1 <= 0)%Z
   | 83 => (-1 * s V_sha_transform_i <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -19 <= 0)%Z
   | 84 => (1 * s V_sha_transform_i + -19 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i <= 0)%Z
   | 85 => (-1 * s V_sha_transform_i <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -19 <= 0)%Z
   | 86 => (1 * s V_sha_transform_i + -19 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i <= 0)%Z
   | 87 => (-1 * s V_sha_transform_i <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -19 <= 0)%Z
   | 88 => (1 * s V_sha_transform_i + -19 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i <= 0)%Z
   | 89 => (-1 * s V_sha_transform_i <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -19 <= 0)%Z
   | 90 => (1 * s V_sha_transform_i + -19 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i <= 0)%Z
   | 91 => (-1 * s V_sha_transform_i <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -19 <= 0)%Z
   | 92 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 1 <= 0 /\ 1 * s V_sha_transform_i + -20 <= 0)%Z
   | 93 => (1 * s V_sha_transform_i + -20 <= 0 /\ -1 * s V_sha_transform_i + 1 <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 94 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 1 <= 0 /\ 1 * s V_sha_transform_i + -20 <= 0)%Z
   | 95 => (1 * s V_sha_transform_i + -20 <= 0 /\ -1 * s V_sha_transform_i + 1 <= 0 /\ -1 * s V_sha_transform_z + 1 <= 0)%Z
   | 96 => (-1 * s V_sha_transform_i + 16 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -79 <= 0)%Z
   | 97 => (1 * s V_sha_transform_i + -79 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 16 <= 0)%Z
   | 98 => (-1 * s V_sha_transform_i + 16 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -79 <= 0)%Z
   | 99 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 17 <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 100 => (1 * s V_sha_transform_i + -80 <= 0 /\ -1 * s V_sha_transform_i + 17 <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 101 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 17 <= 0 /\ 1 * s V_sha_transform_i + -80 <= 0)%Z
   | 102 => (1 * s V_sha_transform_i + -80 <= 0 /\ -1 * s V_sha_transform_i + 17 <= 0 /\ -1 * s V_sha_transform_z + 1 <= 0)%Z
   | 103 => (-1 * s V_sha_transform_i <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -15 <= 0)%Z
   | 104 => (1 * s V_sha_transform_i + -15 <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i <= 0)%Z
   | 105 => (-1 * s V_sha_transform_i <= 0 /\ -1 * s V_sha_transform_z <= 0 /\ 1 * s V_sha_transform_i + -15 <= 0)%Z
   | 106 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 1 <= 0 /\ 1 * s V_sha_transform_i + -16 <= 0)%Z
   | 107 => (1 * s V_sha_transform_i + -16 <= 0 /\ -1 * s V_sha_transform_i + 1 <= 0 /\ -1 * s V_sha_transform_z <= 0)%Z
   | 108 => (-1 * s V_sha_transform_z <= 0 /\ -1 * s V_sha_transform_i + 1 <= 0 /\ 1 * s V_sha_transform_i + -16 <= 0)%Z
   | 109 => (1 * s V_sha_transform_i + -16 <= 0 /\ -1 * s V_sha_transform_i + 1 <= 0 /\ -1 * s V_sha_transform_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_sha_transform (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((160 # 1) <= z)%Q
   | 2 => ((160 # 1) + s V_sha_transform_z <= z)%Q
   | 3 => ((52 # 21) * s V_sha_transform_i + s V_sha_transform_z
           + max0(16 - s V_sha_transform_i)
           + (163 # 60) * max0(60 - s V_sha_transform_i)
           - (19 # 79) * max0(79 - s V_sha_transform_i) <= z)%Q
   | 4 => hints
     [(*-0.240506 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (79
                                                                    - s V_sha_transform_i) (0))) (F_max0_ge_0 (79
                                                                    - s V_sha_transform_i))]
     ((52 # 21) * s V_sha_transform_i + s V_sha_transform_z
      + max0(16 - s V_sha_transform_i)
      + (163 # 60) * max0(60 - s V_sha_transform_i)
      - (19 # 79) * max0(79 - s V_sha_transform_i) <= z)%Q
   | 5 => (-(19 # 1) + (163 # 60) * s V_sha_transform_i + s V_sha_transform_z
           + max0(16 - s V_sha_transform_i)
           + (163 # 60) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (16 - s V_sha_transform_i) (15
                                                                    - s V_sha_transform_i));
      (*-1 0*) F_max0_ge_0 (15 - s V_sha_transform_i);
      (*-2.71667 0*) F_binom_monotonic 1 (F_max0_ge_arg (60
                                                         - s V_sha_transform_i)) (F_check_ge (60
                                                                    - s V_sha_transform_i) (0))]
     (-(19 # 1) + (163 # 60) * s V_sha_transform_i + s V_sha_transform_z
      + max0(16 - s V_sha_transform_i)
      + (163 # 60) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 7 => ((144 # 1) + s V_sha_transform_z <= z)%Q
   | 8 => ((80 # 1) + s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 9 => ((80 # 1) + s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 10 => ((80 # 1) + s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (80 - s V_sha_transform_i) (79
                                                                    - s V_sha_transform_i));
      (*-1 0*) F_max0_ge_0 (79 - s V_sha_transform_i)]
     ((80 # 1) + s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 12 => ((80 # 1) + s V_sha_transform_z <= z)%Q
   | 13 => ((80 # 1) + s V_sha_transform_z <= z)%Q
   | 14 => ((80 # 1) + s V_sha_transform_z <= z)%Q
   | 15 => ((80 # 1) + s V_sha_transform_z <= z)%Q
   | 16 => ((80 # 1) + s V_sha_transform_z <= z)%Q
   | 17 => ((80 # 1) + s V_sha_transform_z <= z)%Q
   | 18 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 19 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 20 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 21 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (20 - s V_sha_transform_i) (19
                                                                    - s V_sha_transform_i));
      (*-1 0*) F_max0_ge_0 (19 - s V_sha_transform_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                               - s V_sha_transform_i) (0))) (F_max0_ge_0 (20
                                                                    - s V_sha_transform_i))]
     ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 22 => ((60 # 1) + s V_sha_transform_z <= z)%Q
   | 23 => ((2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(40 - s V_sha_transform_i)
            + (2 # 3) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 24 => ((2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(40 - s V_sha_transform_i)
            + (2 # 3) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 25 => ((2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(40 - s V_sha_transform_i)
            + (2 # 3) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (40 - s V_sha_transform_i) (39
                                                                    - s V_sha_transform_i));
      (*-1 0*) F_max0_ge_0 (39 - s V_sha_transform_i);
      (*-0.666667 0*) F_binom_monotonic 1 (F_max0_ge_arg (60
                                                          - s V_sha_transform_i)) (F_check_ge (60
                                                                    - s V_sha_transform_i) (0))]
     ((2 # 3) * s V_sha_transform_i + s V_sha_transform_z
      + max0(40 - s V_sha_transform_i)
      + (2 # 3) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 27 => ((40 # 1) + s V_sha_transform_z <= z)%Q
   | 28 => ((20 # 1) + s V_sha_transform_z + max0(60 - s V_sha_transform_i) <= z)%Q
   | 29 => ((20 # 1) + s V_sha_transform_z + max0(60 - s V_sha_transform_i) <= z)%Q
   | 30 => ((20 # 1) + s V_sha_transform_z + max0(60 - s V_sha_transform_i) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (60 - s V_sha_transform_i) (59
                                                                    - s V_sha_transform_i));
      (*-1 0*) F_max0_ge_0 (59 - s V_sha_transform_i)]
     ((20 # 1) + s V_sha_transform_z + max0(60 - s V_sha_transform_i) <= z)%Q
   | 32 => ((20 # 1) + s V_sha_transform_z <= z)%Q
   | 33 => (s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 34 => (s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 35 => (s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 36 => (s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 37 => (s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 38 => (s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 39 => (s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 40 => (s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 41 => (s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 42 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (80 - s V_sha_transform_i) (79
                                                                    - s V_sha_transform_i));
      (*-1 0*) F_max0_ge_0 (79 - s V_sha_transform_i)]
     (s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 43 => (s V_sha_transform_z <= z)%Q
   | 44 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (80 - s V_sha_transform_i) (1)]
     (s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 45 => ((1 # 1) + s V_sha_transform_z + max0(79 - s V_sha_transform_i) <= z)%Q
   | 46 => ((1 # 1) + s V_sha_transform_z + max0(79 - s V_sha_transform_i) <= z)%Q
   | 47 => ((1 # 1) + s V_sha_transform_z + max0(79 - s V_sha_transform_i) <= z)%Q
   | 48 => ((1 # 1) + s V_sha_transform_z + max0(79 - s V_sha_transform_i) <= z)%Q
   | 49 => ((1 # 1) + s V_sha_transform_z + max0(79 - s V_sha_transform_i) <= z)%Q
   | 50 => ((1 # 1) + s V_sha_transform_z + max0(79 - s V_sha_transform_i) <= z)%Q
   | 51 => ((1 # 1) + s V_sha_transform_z + max0(79 - s V_sha_transform_i) <= z)%Q
   | 52 => ((1 # 1) + s V_sha_transform_z + max0(79 - s V_sha_transform_i) <= z)%Q
   | 53 => ((1 # 1) + s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 54 => ((1 # 1) + s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 55 => ((1 # 1) + s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 56 => (s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 57 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (60 - s V_sha_transform_i) (1)]
     ((20 # 1) + s V_sha_transform_z + max0(60 - s V_sha_transform_i) <= z)%Q
   | 58 => ((21 # 1) + s V_sha_transform_z + max0(59 - s V_sha_transform_i) <= z)%Q
   | 59 => ((21 # 1) + s V_sha_transform_z + max0(59 - s V_sha_transform_i) <= z)%Q
   | 60 => ((21 # 1) + s V_sha_transform_z + max0(59 - s V_sha_transform_i) <= z)%Q
   | 61 => ((21 # 1) + s V_sha_transform_z + max0(59 - s V_sha_transform_i) <= z)%Q
   | 62 => ((21 # 1) + s V_sha_transform_z + max0(59 - s V_sha_transform_i) <= z)%Q
   | 63 => ((21 # 1) + s V_sha_transform_z + max0(59 - s V_sha_transform_i) <= z)%Q
   | 64 => ((21 # 1) + s V_sha_transform_z + max0(59 - s V_sha_transform_i) <= z)%Q
   | 65 => ((21 # 1) + s V_sha_transform_z + max0(59 - s V_sha_transform_i) <= z)%Q
   | 66 => ((21 # 1) + s V_sha_transform_z + max0(60 - s V_sha_transform_i) <= z)%Q
   | 67 => ((21 # 1) + s V_sha_transform_z + max0(60 - s V_sha_transform_i) <= z)%Q
   | 68 => ((21 # 1) + s V_sha_transform_z + max0(60 - s V_sha_transform_i) <= z)%Q
   | 69 => ((20 # 1) + s V_sha_transform_z + max0(60 - s V_sha_transform_i) <= z)%Q
   | 70 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (40 - s V_sha_transform_i) (1);
      (*0 0.666667*) F_max0_pre_decrement 1 (60 - s V_sha_transform_i) (1)]
     ((2 # 3) * s V_sha_transform_i + s V_sha_transform_z
      + max0(40 - s V_sha_transform_i)
      + (2 # 3) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 71 => ((5 # 3) + (2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(39 - s V_sha_transform_i)
            + (2 # 3) * max0(59 - s V_sha_transform_i) <= z)%Q
   | 72 => ((5 # 3) + (2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(39 - s V_sha_transform_i)
            + (2 # 3) * max0(59 - s V_sha_transform_i) <= z)%Q
   | 73 => ((5 # 3) + (2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(39 - s V_sha_transform_i)
            + (2 # 3) * max0(59 - s V_sha_transform_i) <= z)%Q
   | 74 => ((5 # 3) + (2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(39 - s V_sha_transform_i)
            + (2 # 3) * max0(59 - s V_sha_transform_i) <= z)%Q
   | 75 => ((5 # 3) + (2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(39 - s V_sha_transform_i)
            + (2 # 3) * max0(59 - s V_sha_transform_i) <= z)%Q
   | 76 => ((5 # 3) + (2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(39 - s V_sha_transform_i)
            + (2 # 3) * max0(59 - s V_sha_transform_i) <= z)%Q
   | 77 => ((5 # 3) + (2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(39 - s V_sha_transform_i)
            + (2 # 3) * max0(59 - s V_sha_transform_i) <= z)%Q
   | 78 => ((5 # 3) + (2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(39 - s V_sha_transform_i)
            + (2 # 3) * max0(59 - s V_sha_transform_i) <= z)%Q
   | 79 => ((1 # 1) + (2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(40 - s V_sha_transform_i)
            + (2 # 3) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 80 => ((1 # 1) + (2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(40 - s V_sha_transform_i)
            + (2 # 3) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 81 => ((1 # 1) + (2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(40 - s V_sha_transform_i)
            + (2 # 3) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 82 => ((2 # 3) * s V_sha_transform_i + s V_sha_transform_z
            + max0(40 - s V_sha_transform_i)
            + (2 # 3) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 83 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 84 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 85 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 86 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 87 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 88 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 89 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 90 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 91 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 92 => ((81 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 93 => ((81 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 94 => ((81 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 95 => ((80 # 1) - s V_sha_transform_i + s V_sha_transform_z <= z)%Q
   | 96 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (80 - s V_sha_transform_i) (1)]
     ((80 # 1) + s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 97 => ((81 # 1) + s V_sha_transform_z + max0(79 - s V_sha_transform_i) <= z)%Q
   | 98 => ((81 # 1) + s V_sha_transform_z + max0(79 - s V_sha_transform_i) <= z)%Q
   | 99 => ((81 # 1) + s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 100 => ((81 # 1) + s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 101 => ((81 # 1) + s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 102 => ((80 # 1) + s V_sha_transform_z + max0(80 - s V_sha_transform_i) <= z)%Q
   | 103 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (16 - s V_sha_transform_i) (1);
      (*-2.71667 0*) F_max0_pre_decrement 1 (60 - s V_sha_transform_i) (1)]
     (-(19 # 1) + (163 # 60) * s V_sha_transform_i + s V_sha_transform_z
      + max0(16 - s V_sha_transform_i)
      + (163 # 60) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 104 => (-(917 # 60) + (163 # 60) * s V_sha_transform_i
             + s V_sha_transform_z + max0(15 - s V_sha_transform_i)
             + (163 # 60) * max0(59 - s V_sha_transform_i) <= z)%Q
   | 105 => (-(917 # 60) + (163 # 60) * s V_sha_transform_i
             + s V_sha_transform_z + max0(15 - s V_sha_transform_i)
             + (163 # 60) * max0(59 - s V_sha_transform_i) <= z)%Q
   | 106 => (-(18 # 1) + (163 # 60) * s V_sha_transform_i
             + s V_sha_transform_z + max0(16 - s V_sha_transform_i)
             + (163 # 60) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 107 => (-(18 # 1) + (163 # 60) * s V_sha_transform_i
             + s V_sha_transform_z + max0(16 - s V_sha_transform_i)
             + (163 # 60) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 108 => (-(18 # 1) + (163 # 60) * s V_sha_transform_i
             + s V_sha_transform_z + max0(16 - s V_sha_transform_i)
             + (163 # 60) * max0(60 - s V_sha_transform_i) <= z)%Q
   | 109 => (-(19 # 1) + (163 # 60) * s V_sha_transform_i
             + s V_sha_transform_z + max0(16 - s V_sha_transform_i)
             + (163 # 60) * max0(60 - s V_sha_transform_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_sha_transform =>
    [mkPA Q (fun n z s => ai_sha_transform n s /\ annot0_sha_transform n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_sha_transform (proc_start P_sha_transform) s1 (proc_end P_sha_transform) s2 ->
    (s2 V_sha_transform_z <= (160 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_sha_transform.
Qed.
