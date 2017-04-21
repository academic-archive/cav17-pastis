Require Import pasta.Pasta.

Inductive proc: Type :=
  P_Reflection_coefficients.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_Reflection_coefficients_z := 1%positive.
Notation V_Reflection_coefficients_L_ACF_dref_off0 := 2%positive.
Notation V_Reflection_coefficients_P_off0 := 3%positive.
Notation V_Reflection_coefficients_P_off2 := 4%positive.
Notation V_Reflection_coefficients_i := 5%positive.
Notation V_Reflection_coefficients_ltmp := 6%positive.
Notation V_Reflection_coefficients_m := 7%positive.
Notation V_Reflection_coefficients_n := 8%positive.
Notation V_Reflection_coefficients_temp := 9%positive.
Notation V_Reflection_coefficients_L_ACF := 10%positive.
Notation V_Reflection_coefficients_r := 11%positive.
Definition Pedges_Reflection_coefficients: list (edge proc) :=
  (EA 1 (AAssign V_Reflection_coefficients_z (Some (ENum (0)))) 2)::
  (EA 2 AWeaken 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_L_ACF_dref_off0) s) =
  (eval (ENum (0)) s))%Z)) 152)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_L_ACF_dref_off0) s) <>
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_L_ACF_dref_off0) s) <>
  (eval (ENum (0)) s))%Z)) 9)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_L_ACF_dref_off0) s) =
  (eval (ENum (0)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 161)::(EA 9 AWeaken 10)::(EA 10 ANone 11)::(EA 11 (AAssign
  V_Reflection_coefficients_temp None) 12)::(EA 12 AWeaken 13)::
  (EA 13 (AGuard (fun s => ((eval (EVar V_Reflection_coefficients_temp) s) >=
  (eval (ENum (0)) s))%Z)) 15)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_temp) s) <
  (eval (ENum (0)) s))%Z)) 14)::(EA 14 AWeaken 18)::(EA 15 AWeaken 16)::
  (EA 16 (AGuard (fun s => ((eval (EVar V_Reflection_coefficients_temp) s) <
  (eval (ENum (32)) s))%Z)) 20)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_temp) s) >=
  (eval (ENum (32)) s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 18 ANone 19)::
  (EA 19 AWeaken 161)::(EA 20 AWeaken 21)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_Reflection_coefficients_i (Some (ENum (0)))) 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 25)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_i) s) <= (eval (ENum (8))
  s))%Z)) 145)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_i) s) > (eval (ENum (8))
  s))%Z)) 26)::(EA 26 AWeaken 27)::(EA 27 (AAssign
  V_Reflection_coefficients_i (Some (ENum (1)))) 28)::(EA 28 ANone 29)::
  (EA 29 AWeaken 30)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_i) s) <= (eval (ENum (7))
  s))%Z)) 138)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_i) s) > (eval (ENum (7))
  s))%Z)) 31)::(EA 31 AWeaken 32)::(EA 32 (AAssign
  V_Reflection_coefficients_i (Some (ENum (0)))) 33)::(EA 33 ANone 34)::
  (EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_i) s) <= (eval (ENum (8))
  s))%Z)) 131)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_i) s) > (eval (ENum (8))
  s))%Z)) 36)::(EA 36 AWeaken 37)::(EA 37 (AAssign
  V_Reflection_coefficients_n (Some (ENum (1)))) 38)::(EA 38 ANone 39)::
  (EA 39 AWeaken 40)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_n) s) <= (eval (ENum (8))
  s))%Z)) 42)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_n) s) > (eval (ENum (8))
  s))%Z)) 41)::(EA 41 AWeaken 161)::(EA 42 AWeaken 43)::(EA 43 (AAssign
  V_Reflection_coefficients_temp
  (Some (EVar V_Reflection_coefficients_P_off2))) 44)::(EA 44 AWeaken 45)::
  (EA 45 (AGuard (fun s => ((eval (EVar V_Reflection_coefficients_temp) s) <
  (eval (ENum (0)) s))%Z)) 48)::(EA 45 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_temp) s) >=
  (eval (ENum (0)) s))%Z)) 46)::(EA 46 AWeaken 47)::(EA 47 ANone 55)::
  (EA 48 AWeaken 49)::(EA 49 (AGuard (fun s => True)) 53)::(EA 49 ANone 50)::
  (EA 50 ANone 51)::(EA 51 (AGuard (fun s => True)) 52)::(EA 52 AWeaken 55)::
  (EA 53 AWeaken 54)::(EA 54 ANone 55)::(EA 55 (AAssign
  V_Reflection_coefficients_temp None) 56)::(EA 56 AWeaken 57)::
  (EA 57 (AGuard (fun s => ((eval (EVar V_Reflection_coefficients_P_off0)
  s) < (eval (EVar V_Reflection_coefficients_temp) s))%Z)) 116)::
  (EA 57 (AGuard (fun s => ((eval (EVar V_Reflection_coefficients_P_off0)
  s) >= (eval (EVar V_Reflection_coefficients_temp) s))%Z)) 58)::
  (EA 58 AWeaken 59)::(EA 59 ANone 62)::(EA 59 ANone 60)::(EA 60 ANone 61)::
  (EA 61 AWeaken 161)::(EA 62 ANone 63)::(EA 63 AWeaken 64)::(EA 64 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_P_off2) s) >
  (eval (ENum (0)) s))%Z)) 66)::(EA 64 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_P_off2) s) <=
  (eval (ENum (0)) s))%Z)) 65)::(EA 65 AWeaken 69)::(EA 66 AWeaken 67)::
  (EA 67 ANone 68)::(EA 68 AWeaken 69)::(EA 69 ANone 72)::(EA 69 ANone 70)::
  (EA 70 ANone 71)::(EA 71 AWeaken 161)::(EA 72 ANone 73)::
  (EA 73 AWeaken 74)::(EA 74 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_n) s) = (eval (ENum (8))
  s))%Z)) 113)::(EA 74 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_n) s) <> (eval (ENum (8))
  s))%Z)) 75)::(EA 75 AWeaken 76)::(EA 76 (AAssign
  V_Reflection_coefficients_temp None) 77)::(EA 77 (AAssign
  V_Reflection_coefficients_ltmp
  (Some (EAdd (EVar V_Reflection_coefficients_P_off0)
  (EVar V_Reflection_coefficients_temp)))) 78)::(EA 78 AWeaken 79)::
  (EA 79 ANone 81)::(EA 79 ANone 80)::(EA 80 ANone 82)::(EA 81 ANone 82)::
  (EA 82 (AAssign V_Reflection_coefficients_P_off0 None) 83)::(EA 83 (AAssign
  V_Reflection_coefficients_m (Some (ENum (1)))) 84)::(EA 84 ANone 85)::
  (EA 85 AWeaken 86)::(EA 86 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_m) s) <=
  (eval (ESub (ENum (8)) (EVar V_Reflection_coefficients_n)) s))%Z)) 94)::
  (EA 86 (AGuard (fun s => ((eval (EVar V_Reflection_coefficients_m) s) >
  (eval (ESub (ENum (8)) (EVar V_Reflection_coefficients_n)) s))%Z)) 87)::
  (EA 87 AWeaken 88)::(EA 88 ANone 89)::(EA 89 (AAssign
  V_Reflection_coefficients_n (Some (EAdd (EVar V_Reflection_coefficients_n)
  (ENum (1))))) 90)::(EA 90 ANone 91)::(EA 91 ANone 92)::(EA 92 (AAssign
  V_Reflection_coefficients_z (Some (EAdd (ENum (1))
  (EVar V_Reflection_coefficients_z)))) 93)::(EA 93 AWeaken 40)::
  (EA 94 AWeaken 95)::(EA 95 (AAssign V_Reflection_coefficients_temp
  None) 96)::(EA 96 (AAssign V_Reflection_coefficients_ltmp None) 97)::
  (EA 97 AWeaken 98)::(EA 98 ANone 100)::(EA 98 ANone 99)::
  (EA 99 ANone 101)::(EA 100 ANone 101)::(EA 101 (AAssign
  V_Reflection_coefficients_temp None) 102)::(EA 102 (AAssign
  V_Reflection_coefficients_ltmp None) 103)::(EA 103 AWeaken 104)::
  (EA 104 ANone 106)::(EA 104 ANone 105)::(EA 105 ANone 107)::
  (EA 106 ANone 107)::(EA 107 ANone 108)::(EA 108 (AAssign
  V_Reflection_coefficients_m (Some (EAdd (EVar V_Reflection_coefficients_m)
  (ENum (1))))) 109)::(EA 109 ANone 110)::(EA 110 ANone 111)::
  (EA 111 (AAssign V_Reflection_coefficients_z (Some (EAdd (ENum (1))
  (EVar V_Reflection_coefficients_z)))) 112)::(EA 112 AWeaken 86)::
  (EA 113 AWeaken 114)::(EA 114 ANone 115)::(EA 115 AWeaken 161)::
  (EA 116 AWeaken 117)::(EA 117 (AAssign V_Reflection_coefficients_i
  (Some (EVar V_Reflection_coefficients_n))) 118)::(EA 118 ANone 119)::
  (EA 119 AWeaken 120)::(EA 120 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_i) s) <= (eval (ENum (8))
  s))%Z)) 124)::(EA 120 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_i) s) > (eval (ENum (8))
  s))%Z)) 121)::(EA 121 AWeaken 122)::(EA 122 ANone 123)::
  (EA 123 AWeaken 161)::(EA 124 AWeaken 125)::(EA 125 ANone 126)::
  (EA 126 (AAssign V_Reflection_coefficients_i
  (Some (EAdd (EVar V_Reflection_coefficients_i) (ENum (1))))) 127)::
  (EA 127 ANone 128)::(EA 128 ANone 129)::(EA 129 (AAssign
  V_Reflection_coefficients_z (Some (EAdd (ENum (1))
  (EVar V_Reflection_coefficients_z)))) 130)::(EA 130 AWeaken 120)::
  (EA 131 AWeaken 132)::(EA 132 ANone 133)::(EA 133 (AAssign
  V_Reflection_coefficients_i (Some (EAdd (EVar V_Reflection_coefficients_i)
  (ENum (1))))) 134)::(EA 134 ANone 135)::(EA 135 ANone 136)::
  (EA 136 (AAssign V_Reflection_coefficients_z (Some (EAdd (ENum (1))
  (EVar V_Reflection_coefficients_z)))) 137)::(EA 137 AWeaken 35)::
  (EA 138 AWeaken 139)::(EA 139 ANone 140)::(EA 140 (AAssign
  V_Reflection_coefficients_i (Some (EAdd (EVar V_Reflection_coefficients_i)
  (ENum (1))))) 141)::(EA 141 ANone 142)::(EA 142 ANone 143)::
  (EA 143 (AAssign V_Reflection_coefficients_z (Some (EAdd (ENum (1))
  (EVar V_Reflection_coefficients_z)))) 144)::(EA 144 AWeaken 30)::
  (EA 145 AWeaken 146)::(EA 146 ANone 147)::(EA 147 (AAssign
  V_Reflection_coefficients_i (Some (EAdd (EVar V_Reflection_coefficients_i)
  (ENum (1))))) 148)::(EA 148 ANone 149)::(EA 149 ANone 150)::
  (EA 150 (AAssign V_Reflection_coefficients_z (Some (EAdd (ENum (1))
  (EVar V_Reflection_coefficients_z)))) 151)::(EA 151 AWeaken 25)::
  (EA 152 AWeaken 153)::(EA 153 (AAssign V_Reflection_coefficients_i
  (Some (ENum (8)))) 154)::(EA 154 ANone 155)::(EA 155 (AAssign
  V_Reflection_coefficients_i (Some (EAdd (EVar V_Reflection_coefficients_i)
  (ENum (-1))))) 156)::(EA 156 AWeaken 157)::(EA 157 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_i) s) <> (eval (ENum (0))
  s))%Z)) 162)::(EA 157 (AGuard
  (fun s => ((eval (EVar V_Reflection_coefficients_i) s) = (eval (ENum (0))
  s))%Z)) 158)::(EA 158 AWeaken 159)::(EA 159 ANone 160)::
  (EA 160 AWeaken 161)::(EA 162 AWeaken 163)::(EA 163 ANone 164)::
  (EA 164 ANone 165)::(EA 165 ANone 166)::(EA 166 (AAssign
  V_Reflection_coefficients_z (Some (EAdd (ENum (1))
  (EVar V_Reflection_coefficients_z)))) 155)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_Reflection_coefficients => Pedges_Reflection_coefficients
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_Reflection_coefficients => 161
     end)%positive;
  var_global := var_global
}.

Definition ai_Reflection_coefficients (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 3 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0)%Z
   | 4 => (1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 5 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0)%Z
   | 6 => (1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0)%Z
   | 7 => (-1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ 1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0)%Z
   | 8 => (1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0)%Z
   | 9 => (1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 10 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0)%Z
   | 11 => (1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 12 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0)%Z
   | 13 => (1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 14 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_temp + 1 <= 0)%Z
   | 15 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 16 => (-1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 17 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_temp + 32 <= 0)%Z
   | 18 => (1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 19 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0)%Z
   | 20 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0)%Z
   | 21 => (1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 22 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0)%Z
   | 23 => (1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i <= 0 /\ -1 * s V_Reflection_coefficients_i <= 0)%Z
   | 24 => (-1 * s V_Reflection_coefficients_i <= 0 /\ 1 * s V_Reflection_coefficients_i <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0)%Z
   | 25 => (-1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 26 => (1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0)%Z
   | 27 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 28 => (-1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0)%Z
   | 29 => (-1 * s V_Reflection_coefficients_i + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 30 => (-1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0)%Z
   | 31 => (1 * s V_Reflection_coefficients_i + -8 <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i + 8 <= 0)%Z
   | 32 => (-1 * s V_Reflection_coefficients_i + 8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0)%Z
   | 33 => (1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i <= 0 /\ -1 * s V_Reflection_coefficients_i <= 0)%Z
   | 34 => (-1 * s V_Reflection_coefficients_i <= 0 /\ 1 * s V_Reflection_coefficients_i <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0)%Z
   | 35 => (-1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 36 => (1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0)%Z
   | 37 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 38 => (1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_n + -1 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0)%Z
   | 39 => (-1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 40 => (-1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0)%Z
   | 41 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 9 <= 0)%Z
   | 42 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 43 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0)%Z
   | 44 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 45 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0)%Z
   | 46 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 47 => (-1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0)%Z
   | 48 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ 1 * s V_Reflection_coefficients_temp + 1 <= 0)%Z
   | 49 => (1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0)%Z
   | 50 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ 1 * s V_Reflection_coefficients_temp + 1 <= 0)%Z
   | 51 => (1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0)%Z
   | 52 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ 1 * s V_Reflection_coefficients_temp + 1 <= 0)%Z
   | 53 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ 1 * s V_Reflection_coefficients_temp + 1 <= 0)%Z
   | 54 => (1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0)%Z
   | 55 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 56 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0)%Z
   | 57 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 58 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 59 => (-1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 60 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 61 => (-1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 62 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 63 => (-1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 64 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 65 => (-1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ 1 * s V_Reflection_coefficients_P_off2 <= 0)%Z
   | 66 => (-1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_P_off2 + 1 <= 0)%Z
   | 67 => (-1 * s V_Reflection_coefficients_P_off2 + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 68 => (-1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_P_off2 + 1 <= 0)%Z
   | 69 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 70 => (-1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 71 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 72 => (-1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 73 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 74 => (-1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 75 => (-1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0)%Z
   | 76 => (1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 77 => (-1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0)%Z
   | 78 => (1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 79 => (-1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0)%Z
   | 80 => (1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 81 => (1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 82 => (-1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0)%Z
   | 83 => (1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 84 => (-1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ 1 * s V_Reflection_coefficients_m + -1 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0)%Z
   | 85 => (-1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_m + -1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 86 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -9 <= 0)%Z
   | 87 => (1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_m+ -1 * s V_Reflection_coefficients_n + 9 <= 0)%Z
   | 88 => (-1 * s V_Reflection_coefficients_m+ -1 * s V_Reflection_coefficients_n + 9 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -9 <= 0)%Z
   | 89 => (1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_m+ -1 * s V_Reflection_coefficients_n + 9 <= 0)%Z
   | 90 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -10 <= 0 /\ -1 * s V_Reflection_coefficients_n + 2 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_m+ -1 * s V_Reflection_coefficients_n + 10 <= 0)%Z
   | 91 => (-1 * s V_Reflection_coefficients_m+ -1 * s V_Reflection_coefficients_n + 10 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_n + 2 <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -10 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 92 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -10 <= 0 /\ -1 * s V_Reflection_coefficients_n + 2 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_m+ -1 * s V_Reflection_coefficients_n + 10 <= 0)%Z
   | 93 => (-1 * s V_Reflection_coefficients_m+ -1 * s V_Reflection_coefficients_n + 10 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_n + 2 <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -10 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_z + 1 <= 0)%Z
   | 94 => (-1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 95 => (1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0)%Z
   | 96 => (-1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 97 => (1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0)%Z
   | 98 => (-1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 99 => (1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0)%Z
   | 100 => (1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0)%Z
   | 101 => (-1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 102 => (1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0)%Z
   | 103 => (-1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 104 => (1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0)%Z
   | 105 => (-1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 106 => (-1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 107 => (1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0)%Z
   | 108 => (-1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_m + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 109 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_m + 2 <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -9 <= 0)%Z
   | 110 => (1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -9 <= 0 /\ -1 * s V_Reflection_coefficients_m + 2 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 111 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_m + 2 <= 0 /\ 1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -9 <= 0)%Z
   | 112 => (1 * s V_Reflection_coefficients_m+ 1 * s V_Reflection_coefficients_n + -9 <= 0 /\ -1 * s V_Reflection_coefficients_m + 2 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_z + 1 <= 0)%Z
   | 113 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_n + 8 <= 0)%Z
   | 114 => (-1 * s V_Reflection_coefficients_n + 8 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 115 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_P_off0+ 1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_n + 8 <= 0)%Z
   | 116 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0)%Z
   | 117 => (1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 118 => (1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0)%Z
   | 119 => (-1 * s V_Reflection_coefficients_i + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0 /\ 1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0)%Z
   | 120 => (-1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 121 => (1 * s V_Reflection_coefficients_i + -9 <= 0 /\ 1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0)%Z
   | 122 => (-1 * s V_Reflection_coefficients_i + 9 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 123 => (1 * s V_Reflection_coefficients_i + -9 <= 0 /\ 1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i + 9 <= 0)%Z
   | 124 => (1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0)%Z
   | 125 => (1 * s V_Reflection_coefficients_i + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0)%Z
   | 126 => (1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0)%Z
   | 127 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 2 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 128 => (1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 2 <= 0 /\ 1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 129 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ -1 * s V_Reflection_coefficients_i + 2 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 130 => (1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 2 <= 0 /\ 1 * s V_Reflection_coefficients_P_off0+ -1 * s V_Reflection_coefficients_temp + 1 <= 0 /\ -1 * s V_Reflection_coefficients_n + 1 <= 0 /\ 1 * s V_Reflection_coefficients_n + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z + 1 <= 0)%Z
   | 131 => (-1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_i <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0)%Z
   | 132 => (1 * s V_Reflection_coefficients_i + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 133 => (-1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_i <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0)%Z
   | 134 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 135 => (1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 136 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 137 => (1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_z + 1 <= 0)%Z
   | 138 => (1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -7 <= 0)%Z
   | 139 => (1 * s V_Reflection_coefficients_i + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0)%Z
   | 140 => (1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -7 <= 0)%Z
   | 141 => (-1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_i + 2 <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0)%Z
   | 142 => (1 * s V_Reflection_coefficients_i + -8 <= 0 /\ -1 * s V_Reflection_coefficients_i + 2 <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 143 => (-1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_i + 2 <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0)%Z
   | 144 => (1 * s V_Reflection_coefficients_i + -8 <= 0 /\ -1 * s V_Reflection_coefficients_i + 2 <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_z + 1 <= 0)%Z
   | 145 => (-1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_i <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0)%Z
   | 146 => (1 * s V_Reflection_coefficients_i + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_i <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0)%Z
   | 147 => (-1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_i <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0)%Z
   | 148 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 149 => (1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0)%Z
   | 150 => (-1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ 1 * s V_Reflection_coefficients_i + -9 <= 0)%Z
   | 151 => (1 * s V_Reflection_coefficients_i + -9 <= 0 /\ -1 * s V_Reflection_coefficients_i + 1 <= 0 /\ -1 * s V_Reflection_coefficients_temp <= 0 /\ 1 * s V_Reflection_coefficients_temp + -31 <= 0 /\ -1 * s V_Reflection_coefficients_z + 1 <= 0)%Z
   | 152 => (1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0)%Z
   | 153 => (-1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ 1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_z <= 0)%Z
   | 154 => (1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ 1 * s V_Reflection_coefficients_i + -8 <= 0 /\ -1 * s V_Reflection_coefficients_i + 8 <= 0)%Z
   | 155 => (1 * s V_Reflection_coefficients_i + -8 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ 1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0)%Z
   | 156 => (1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -7 <= 0)%Z
   | 157 => (1 * s V_Reflection_coefficients_i + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ 1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0)%Z
   | 158 => (1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i <= 0 /\ -1 * s V_Reflection_coefficients_i <= 0)%Z
   | 159 => (-1 * s V_Reflection_coefficients_i <= 0 /\ 1 * s V_Reflection_coefficients_i <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ 1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0)%Z
   | 160 => (1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i <= 0 /\ -1 * s V_Reflection_coefficients_i <= 0)%Z
   | 161 => (-1 * s V_Reflection_coefficients_z <= 0)%Z
   | 162 => (1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -7 <= 0)%Z
   | 163 => (1 * s V_Reflection_coefficients_i + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ 1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0)%Z
   | 164 => (1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -7 <= 0)%Z
   | 165 => (1 * s V_Reflection_coefficients_i + -7 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ 1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0)%Z
   | 166 => (1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_L_ACF_dref_off0 <= 0 /\ -1 * s V_Reflection_coefficients_z <= 0 /\ 1 * s V_Reflection_coefficients_i + -7 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_Reflection_coefficients (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((82 # 1) <= z)%Q
   | 2 => ((82 # 1) <= z)%Q
   | 3 => ((82 # 1) <= z)%Q
   | 4 => ((82 # 1) <= z)%Q
   | 5 => ((82 # 1) <= z)%Q
   | 6 => ((82 # 1) <= z)%Q
   | 7 => ((82 # 1) <= z)%Q
   | 8 => hints
     [(*-82 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_Reflection_coefficients_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_Reflection_coefficients_z) (0))) (F_max0_ge_0 (-
                                                                    s V_Reflection_coefficients_z))]
     ((82 # 1) <= z)%Q
   | 9 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_Reflection_coefficients_z) (0))) (F_max0_ge_0 (-
                                                                    s V_Reflection_coefficients_z))]
     ((82 # 1) <= z)%Q
   | 10 => ((82 # 1) + s V_Reflection_coefficients_z
            + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 11 => ((82 # 1) + s V_Reflection_coefficients_z
            + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 12 => ((82 # 1) + s V_Reflection_coefficients_z
            + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 13 => ((82 # 1) + s V_Reflection_coefficients_z
            + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 14 => hints
     [(*0 82*) F_one;
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_Reflection_coefficients_z)) (F_check_ge (0) (0))]
     ((82 # 1) + s V_Reflection_coefficients_z
      + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 15 => ((82 # 1) + s V_Reflection_coefficients_z
            + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 16 => ((82 # 1) + s V_Reflection_coefficients_z
            + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 17 => hints
     [(*-82 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_Reflection_coefficients_z)) (F_check_ge (0) (0))]
     ((82 # 1) + s V_Reflection_coefficients_z
      + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 18 => (s V_Reflection_coefficients_z <= z)%Q
   | 19 => (s V_Reflection_coefficients_z <= z)%Q
   | 20 => ((82 # 1) + s V_Reflection_coefficients_z
            + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 21 => ((82 # 1) + s V_Reflection_coefficients_z
            + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 22 => ((82 # 1) + s V_Reflection_coefficients_z
            + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 23 => ((73 # 1) + s V_Reflection_coefficients_z
            + max0(9 - s V_Reflection_coefficients_i)
            + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_Reflection_coefficients_z)) (F_check_ge (0) (0))]
     ((73 # 1) + s V_Reflection_coefficients_z
      + max0(9 - s V_Reflection_coefficients_i)
      + max0(-s V_Reflection_coefficients_z) <= z)%Q
   | 25 => ((73 # 1) + s V_Reflection_coefficients_z
            + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                             - s V_Reflection_coefficients_i) (7
                                                                    - s V_Reflection_coefficients_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_Reflection_coefficients_i);
      (*-1 0*) F_max0_monotonic (F_check_ge (9
                                             - s V_Reflection_coefficients_i) (8
                                                                    - s V_Reflection_coefficients_i))]
     ((73 # 1) + s V_Reflection_coefficients_z
      + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 27 => ((73 # 1) + s V_Reflection_coefficients_z <= z)%Q
   | 28 => ((66 # 1) + s V_Reflection_coefficients_z
            + max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 29 => ((66 # 1) + s V_Reflection_coefficients_z
            + max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 30 => ((66 # 1) + s V_Reflection_coefficients_z
            + max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                             - s V_Reflection_coefficients_i) (7
                                                                    - s V_Reflection_coefficients_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_Reflection_coefficients_i)]
     ((66 # 1) + s V_Reflection_coefficients_z
      + max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 32 => ((66 # 1) + s V_Reflection_coefficients_z <= z)%Q
   | 33 => ((57 # 1) + s V_Reflection_coefficients_z
            + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 34 => ((57 # 1) + s V_Reflection_coefficients_z
            + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 35 => ((57 # 1) + s V_Reflection_coefficients_z
            + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 36 => hints
     [(*0 1.83871*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (31
                                                                    - 
                                                                    s V_Reflection_coefficients_temp) (0))) (F_max0_ge_0 (31
                                                                    - s V_Reflection_coefficients_temp))]
     ((57 # 1) + s V_Reflection_coefficients_z
      + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 37 => ((57 # 31) * s V_Reflection_coefficients_temp
            + s V_Reflection_coefficients_z
            + max0(9 - s V_Reflection_coefficients_i)
            + (57 # 31) * max0(31 - s V_Reflection_coefficients_temp) <= z)%Q
   | 38 => (-(393 # 7) - (6 # 7) * s V_Reflection_coefficients_n
            + (57 # 31) * s V_Reflection_coefficients_temp
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + max0(9 - s V_Reflection_coefficients_i)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n)
            + (57 # 31) * max0(31 - s V_Reflection_coefficients_temp) <= z)%Q
   | 39 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8
                                             - s V_Reflection_coefficients_i) (7
                                                                    - s V_Reflection_coefficients_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_Reflection_coefficients_i);
      (*-1 0*) F_max0_monotonic (F_check_ge (9
                                             - s V_Reflection_coefficients_i) (8
                                                                    - s V_Reflection_coefficients_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_Reflection_coefficients_z) (0))) (F_max0_ge_0 (s V_Reflection_coefficients_z));
      (*-1.83871 0*) F_binom_monotonic 1 (F_max0_ge_arg (31
                                                         - s V_Reflection_coefficients_temp)) (F_check_ge (31
                                                                    - s V_Reflection_coefficients_temp) (0))]
     (-(393 # 7) - (6 # 7) * s V_Reflection_coefficients_n
      + (57 # 31) * s V_Reflection_coefficients_temp
      + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_n)
      + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
      + max0(9 - s V_Reflection_coefficients_i)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n)
      + (57 # 31) * max0(31 - s V_Reflection_coefficients_temp) <= z)%Q
   | 40 => ((6 # 7) - (6 # 7) * s V_Reflection_coefficients_n
            + max0(-2 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n)
            + max0(s V_Reflection_coefficients_z) <= z)%Q
   | 41 => hints
     [(*-7.14286 0*) F_max0_monotonic (F_check_ge (8
                                                   - s V_Reflection_coefficients_n) (7
                                                                    - s V_Reflection_coefficients_n));
      (*-7.14286 0*) F_max0_ge_0 (7 - s V_Reflection_coefficients_n);
      (*-7 0*) F_max0_monotonic (F_check_ge (9
                                             - s V_Reflection_coefficients_n) (8
                                                                    - s V_Reflection_coefficients_n));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_Reflection_coefficients_z)) (F_check_ge (s V_Reflection_coefficients_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                   + s V_Reflection_coefficients_n)) (F_check_ge (-2
                                                                    + s V_Reflection_coefficients_n) (0));
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_0 (-8
                                                        + s V_Reflection_coefficients_n)) (F_check_ge (0) (0));
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-8
                                                                    + s V_Reflection_coefficients_n) (0))) (F_max0_ge_0 (-8
                                                                    + s V_Reflection_coefficients_n))]
     ((6 # 7) - (6 # 7) * s V_Reflection_coefficients_n
      + max0(-2 + s V_Reflection_coefficients_n)
      + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n)
      + max0(s V_Reflection_coefficients_z) <= z)%Q
   | 42 => hints
     [(*0 0.857143*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                         + s V_Reflection_coefficients_n)) (F_check_ge (-1
                                                                    + s V_Reflection_coefficients_n) (0))]
     ((6 # 7) - (6 # 7) * s V_Reflection_coefficients_n
      + max0(-2 + s V_Reflection_coefficients_n)
      + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n)
      + max0(s V_Reflection_coefficients_z) <= z)%Q
   | 43 => (max0(-2 + s V_Reflection_coefficients_n)
            - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n)
            + max0(s V_Reflection_coefficients_z) <= z)%Q
   | 44 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_Reflection_coefficients_z)) (F_check_ge (s V_Reflection_coefficients_z) (0))]
     (max0(-2 + s V_Reflection_coefficients_n)
      - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
      + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n)
      + max0(s V_Reflection_coefficients_z) <= z)%Q
   | 45 => (s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 46 => (s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 47 => (s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 48 => (s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 49 => (s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 50 => (s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 51 => (s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 52 => (s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 53 => (s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 54 => (s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 55 => (s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 56 => hints
     [(*-0.857143 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_Reflection_coefficients_n) (0))) (F_max0_ge_0 (-1
                                                                    + s V_Reflection_coefficients_n))]
     (s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_n)
      - (6 # 7) * max0(-1 + s V_Reflection_coefficients_n)
      + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 57 => ((6 # 7) - (6 # 7) * s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 58 => hints
     [(*0 0.857143*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - 
                                                                    s V_Reflection_coefficients_n) (0))) (F_max0_ge_0 (8
                                                                    - s V_Reflection_coefficients_n))]
     ((6 # 7) - (6 # 7) * s V_Reflection_coefficients_n
      + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_n)
      + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 59 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 60 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 61 => hints
     [(*-6 0*) F_max0_pre_decrement 1 (9 - s V_Reflection_coefficients_n) (1);
      (*-8 0*) F_max0_monotonic (F_check_ge (8
                                             - s V_Reflection_coefficients_n) (7
                                                                    - s V_Reflection_coefficients_n));
      (*-8 0*) F_max0_ge_0 (7 - s V_Reflection_coefficients_n);
      (*-1 0*) F_max0_monotonic (F_check_ge (9
                                             - s V_Reflection_coefficients_n) (8
                                                                    - s V_Reflection_coefficients_n));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                 + s V_Reflection_coefficients_n)) (F_check_ge (0) (0))]
     (-(6 # 1) + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_n)
      + max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 62 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 63 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 64 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 65 => hints
     [(*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                   - 
                                                                   s V_Reflection_coefficients_n) (0))) (F_max0_ge_0 (8
                                                                    - s V_Reflection_coefficients_n))]
     (-(6 # 1) + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_n)
      + max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 66 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 67 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 68 => hints
     [(*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                   - 
                                                                   s V_Reflection_coefficients_n) (0))) (F_max0_ge_0 (8
                                                                    - s V_Reflection_coefficients_n))]
     (-(6 # 1) + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_n)
      + max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 69 => (-(7 # 1) + (1 # 8) * s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (9 # 8) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 70 => (-(7 # 1) + (1 # 8) * s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (9 # 8) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 71 => hints
     [(*-7 0*) F_max0_pre_decrement 1 (9 - s V_Reflection_coefficients_n) (1);
      (*-8.125 0*) F_max0_monotonic (F_check_ge (8
                                                 - s V_Reflection_coefficients_n) (7
                                                                    - s V_Reflection_coefficients_n));
      (*-8.125 0*) F_max0_ge_0 (7 - s V_Reflection_coefficients_n);
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_Reflection_coefficients_n)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_Reflection_coefficients_n) (0))) (F_max0_ge_0 (s V_Reflection_coefficients_n));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                 + s V_Reflection_coefficients_n)) (F_check_ge (0) (0))]
     (-(7 # 1) + (1 # 8) * s V_Reflection_coefficients_n
      + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_n)
      + (9 # 8) * max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 72 => (-(7 # 1) + (1 # 8) * s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (9 # 8) * max0(8 - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 73 => hints
     [(*-1.125 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                       - s V_Reflection_coefficients_n)) (F_check_ge (8
                                                                    - s V_Reflection_coefficients_n) (0))]
     (-(7 # 1) + (1 # 8) * s V_Reflection_coefficients_n
      + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_n)
      + (9 # 8) * max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 74 => ((2 # 1) - s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 75 => ((2 # 1) - s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 76 => ((2 # 1) - s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 77 => ((2 # 1) - s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 78 => ((2 # 1) - s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 79 => ((2 # 1) - s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 80 => ((2 # 1) - s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 81 => ((2 # 1) - s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 82 => ((2 # 1) - s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 83 => ((2 # 1) - s V_Reflection_coefficients_n
            + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 84 => ((3 # 1) - s V_Reflection_coefficients_m
            - s V_Reflection_coefficients_n + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 85 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                               - s V_Reflection_coefficients_m
                                                               - s V_Reflection_coefficients_n) (0))) (F_max0_ge_0 (9
                                                                    - s V_Reflection_coefficients_m
                                                                    - s V_Reflection_coefficients_n))]
     ((3 # 1) - s V_Reflection_coefficients_m - s V_Reflection_coefficients_n
      + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 86 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(9 - s V_Reflection_coefficients_m
                   - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 87 => hints
     [(*-7 0*) F_max0_pre_decrement 1 (9 - s V_Reflection_coefficients_n) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                 + s V_Reflection_coefficients_n)) (F_check_ge (0) (0))]
     (-(6 # 1) + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_n)
      + max0(9 - s V_Reflection_coefficients_m
             - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 88 => ((1 # 1) + s V_Reflection_coefficients_z
            + (7 # 1) * max0(8 - s V_Reflection_coefficients_n)
            + max0(9 - s V_Reflection_coefficients_m
                   - s V_Reflection_coefficients_n) <= z)%Q
   | 89 => ((1 # 1) + s V_Reflection_coefficients_z
            + (7 # 1) * max0(8 - s V_Reflection_coefficients_n)
            + max0(9 - s V_Reflection_coefficients_m
                   - s V_Reflection_coefficients_n) <= z)%Q
   | 90 => ((1 # 1) + s V_Reflection_coefficients_z
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n)
            + max0(10 - s V_Reflection_coefficients_m
                   - s V_Reflection_coefficients_n) <= z)%Q
   | 91 => ((1 # 1) + s V_Reflection_coefficients_z
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n)
            + max0(10 - s V_Reflection_coefficients_m
                   - s V_Reflection_coefficients_n) <= z)%Q
   | 92 => ((1 # 1) + s V_Reflection_coefficients_z
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n)
            + max0(10 - s V_Reflection_coefficients_m
                   - s V_Reflection_coefficients_n) <= z)%Q
   | 93 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_Reflection_coefficients_z) (0))) (F_max0_ge_0 (s V_Reflection_coefficients_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (10
                                                 - s V_Reflection_coefficients_m
                                                 - s V_Reflection_coefficients_n)) (F_check_ge (0) (0));
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - s V_Reflection_coefficients_n) (0))) (F_max0_ge_0 (8
                                                                    - s V_Reflection_coefficients_n));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                               + s V_Reflection_coefficients_n) (0))) (F_max0_ge_0 (-2
                                                                    + s V_Reflection_coefficients_n))]
     (s V_Reflection_coefficients_z
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n)
      + max0(10 - s V_Reflection_coefficients_m
             - s V_Reflection_coefficients_n) <= z)%Q
   | 94 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(9 - s V_Reflection_coefficients_m
                   - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 95 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(9 - s V_Reflection_coefficients_m
                   - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 96 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(9 - s V_Reflection_coefficients_m
                   - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 97 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(9 - s V_Reflection_coefficients_m
                   - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 98 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(9 - s V_Reflection_coefficients_m
                   - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 99 => (-(6 # 1) + s V_Reflection_coefficients_z
            + max0(-2 + s V_Reflection_coefficients_n)
            + max0(9 - s V_Reflection_coefficients_m
                   - s V_Reflection_coefficients_n)
            + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 100 => (-(6 # 1) + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + max0(9 - s V_Reflection_coefficients_m
                    - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 101 => (-(6 # 1) + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + max0(9 - s V_Reflection_coefficients_m
                    - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 102 => (-(6 # 1) + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + max0(9 - s V_Reflection_coefficients_m
                    - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 103 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (9 - s V_Reflection_coefficients_m
                                       - s V_Reflection_coefficients_n) (1)]
     (-(6 # 1) + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_n)
      + max0(9 - s V_Reflection_coefficients_m
             - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 104 => (-(5 # 1) + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + max0(8 - s V_Reflection_coefficients_m
                    - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 105 => (-(5 # 1) + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + max0(8 - s V_Reflection_coefficients_m
                    - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 106 => (-(5 # 1) + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + max0(8 - s V_Reflection_coefficients_m
                    - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 107 => (-(5 # 1) + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + max0(8 - s V_Reflection_coefficients_m
                    - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 108 => (-(5 # 1) + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + max0(8 - s V_Reflection_coefficients_m
                    - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 109 => (-(5 # 1) + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + max0(9 - s V_Reflection_coefficients_m
                    - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 110 => (-(5 # 1) + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + max0(9 - s V_Reflection_coefficients_m
                    - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 111 => (-(5 # 1) + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + max0(9 - s V_Reflection_coefficients_m
                    - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 112 => (-(6 # 1) + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + max0(9 - s V_Reflection_coefficients_m
                    - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 113 => ((2 # 1) - s V_Reflection_coefficients_n
             + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 114 => ((2 # 1) - s V_Reflection_coefficients_n
             + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 115 => hints
     [(*-7 0*) F_max0_monotonic (F_check_ge (8
                                             - s V_Reflection_coefficients_n) (7
                                                                    - s V_Reflection_coefficients_n));
      (*-7 0*) F_max0_ge_0 (7 - s V_Reflection_coefficients_n);
      (*-7 0*) F_max0_monotonic (F_check_ge (9
                                             - s V_Reflection_coefficients_n) (8
                                                                    - s V_Reflection_coefficients_n));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                   + s V_Reflection_coefficients_n)) (F_check_ge (-2
                                                                    + s V_Reflection_coefficients_n) (0))]
     ((2 # 1) - s V_Reflection_coefficients_n + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 116 => ((6 # 7) - (6 # 7) * s V_Reflection_coefficients_n
             + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 117 => ((6 # 7) - (6 # 7) * s V_Reflection_coefficients_n
             + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_n)
             + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_n) <= z)%Q
   | 118 => ((6 # 7) - s V_Reflection_coefficients_i
             + (1 # 7) * s V_Reflection_coefficients_n
             + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_i)
             + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 119 => ((6 # 7) - s V_Reflection_coefficients_i
             + (1 # 7) * s V_Reflection_coefficients_n
             + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_i)
             + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 120 => ((6 # 7) - s V_Reflection_coefficients_i
             + (1 # 7) * s V_Reflection_coefficients_n
             + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_i)
             + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 121 => hints
     [(*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                          - s V_Reflection_coefficients_n)) (F_check_ge (8
                                                                    - s V_Reflection_coefficients_n) (0))]
     ((6 # 7) - s V_Reflection_coefficients_i
      + (1 # 7) * s V_Reflection_coefficients_n
      + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_i)
      + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 122 => ((2 # 1) - s V_Reflection_coefficients_i
             + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_i)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 123 => hints
     [(*-7 0*) F_max0_monotonic (F_check_ge (8
                                             - s V_Reflection_coefficients_i) (7
                                                                    - s V_Reflection_coefficients_i));
      (*-7 0*) F_max0_ge_0 (7 - s V_Reflection_coefficients_i);
      (*-7 0*) F_max0_monotonic (F_check_ge (9
                                             - s V_Reflection_coefficients_i) (8
                                                                    - s V_Reflection_coefficients_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                   + s V_Reflection_coefficients_i)) (F_check_ge (-2
                                                                    + s V_Reflection_coefficients_i) (0))]
     ((2 # 1) - s V_Reflection_coefficients_i + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_i)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 124 => hints
     [(*-7 0*) F_max0_pre_decrement 1 (9 - s V_Reflection_coefficients_i) (1);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              + s V_Reflection_coefficients_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_Reflection_coefficients_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                 + s V_Reflection_coefficients_i)) (F_check_ge (0) (0))]
     ((6 # 7) - s V_Reflection_coefficients_i
      + (1 # 7) * s V_Reflection_coefficients_n
      + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_i)
      + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 125 => ((62 # 7) - (2 # 1) * s V_Reflection_coefficients_i
             + (1 # 7) * s V_Reflection_coefficients_n
             + s V_Reflection_coefficients_z
             + max0(-1 + s V_Reflection_coefficients_i)
             + (7 # 1) * max0(8 - s V_Reflection_coefficients_i)
             + (1 # 7) * max0(8 - s V_Reflection_coefficients_n) <= z)%Q
   | 126 => ((62 # 7) - (2 # 1) * s V_Reflection_coefficients_i
             + (1 # 7) * s V_Reflection_coefficients_n
             + s V_Reflection_coefficients_z
             + max0(-1 + s V_Reflection_coefficients_i)
             + (7 # 1) * max0(8 - s V_Reflection_coefficients_i)
             + (1 # 7) * max0(8 - s V_Reflection_coefficients_n) <= z)%Q
   | 127 => ((76 # 7) - (2 # 1) * s V_Reflection_coefficients_i
             + (1 # 7) * s V_Reflection_coefficients_n
             + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_i)
             + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 128 => ((76 # 7) - (2 # 1) * s V_Reflection_coefficients_i
             + (1 # 7) * s V_Reflection_coefficients_n
             + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_i)
             + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 129 => ((76 # 7) - (2 # 1) * s V_Reflection_coefficients_i
             + (1 # 7) * s V_Reflection_coefficients_n
             + s V_Reflection_coefficients_z
             + max0(-2 + s V_Reflection_coefficients_i)
             + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
             + (7 # 1) * max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 130 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (9
                                                 - s V_Reflection_coefficients_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                               - s V_Reflection_coefficients_i) (0))) (F_max0_ge_0 (9
                                                                    - s V_Reflection_coefficients_i))]
     ((69 # 7) - (2 # 1) * s V_Reflection_coefficients_i
      + (1 # 7) * s V_Reflection_coefficients_n
      + s V_Reflection_coefficients_z
      + max0(-2 + s V_Reflection_coefficients_i)
      + (1 # 7) * max0(8 - s V_Reflection_coefficients_n)
      + (7 # 1) * max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 131 => hints
     [(*0 1*) F_max0_pre_decrement 1 (9 - s V_Reflection_coefficients_i) (1)]
     ((57 # 1) + s V_Reflection_coefficients_z
      + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 132 => ((58 # 1) + s V_Reflection_coefficients_z
             + max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 133 => ((58 # 1) + s V_Reflection_coefficients_z
             + max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 134 => ((58 # 1) + s V_Reflection_coefficients_z
             + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 135 => ((58 # 1) + s V_Reflection_coefficients_z
             + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 136 => ((58 # 1) + s V_Reflection_coefficients_z
             + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 137 => ((57 # 1) + s V_Reflection_coefficients_z
             + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 138 => ((66 # 1) + s V_Reflection_coefficients_z
             + max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 139 => ((66 # 1) + s V_Reflection_coefficients_z
             + max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 140 => ((66 # 1) + s V_Reflection_coefficients_z
             + max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 141 => ((66 # 1) + s V_Reflection_coefficients_z
             + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 142 => ((66 # 1) + s V_Reflection_coefficients_z
             + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 143 => ((66 # 1) + s V_Reflection_coefficients_z
             + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 144 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (9 - s V_Reflection_coefficients_i) (1)]
     ((65 # 1) + s V_Reflection_coefficients_z
      + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 145 => hints
     [(*0 1*) F_max0_pre_decrement 1 (9 - s V_Reflection_coefficients_i) (1)]
     ((73 # 1) + s V_Reflection_coefficients_z
      + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 146 => ((74 # 1) + s V_Reflection_coefficients_z
             + max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 147 => ((74 # 1) + s V_Reflection_coefficients_z
             + max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 148 => ((74 # 1) + s V_Reflection_coefficients_z
             + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 149 => ((74 # 1) + s V_Reflection_coefficients_z
             + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 150 => ((74 # 1) + s V_Reflection_coefficients_z
             + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 151 => ((73 # 1) + s V_Reflection_coefficients_z
             + max0(9 - s V_Reflection_coefficients_i) <= z)%Q
   | 152 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_Reflection_coefficients_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_Reflection_coefficients_z) (0))) (F_max0_ge_0 (-
                                                                    s V_Reflection_coefficients_z))]
     ((82 # 1) <= z)%Q
   | 153 => ((82 # 1) + s V_Reflection_coefficients_z <= z)%Q
   | 154 => ((82 # 1) + s V_Reflection_coefficients_z
             - (82 # 7) * max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 155 => ((82 # 1) + s V_Reflection_coefficients_z
             - (82 # 7) * max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 156 => ((82 # 1) + s V_Reflection_coefficients_z
             - (82 # 7) * max0(7 - s V_Reflection_coefficients_i) <= z)%Q
   | 157 => ((82 # 1) + s V_Reflection_coefficients_z
             - (82 # 7) * max0(7 - s V_Reflection_coefficients_i) <= z)%Q
   | 158 => ((82 # 1) + s V_Reflection_coefficients_z
             - (82 # 7) * max0(7 - s V_Reflection_coefficients_i) <= z)%Q
   | 159 => ((82 # 1) + s V_Reflection_coefficients_z
             - (82 # 7) * max0(7 - s V_Reflection_coefficients_i) <= z)%Q
   | 160 => hints
     [(*-11.7143 0*) F_max0_pre_decrement 1 (8
                                             - s V_Reflection_coefficients_i) (1);
      (*-11.7143 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_Reflection_coefficients_i)) (F_check_ge (0) (0));
      (*-11.7143 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_Reflection_coefficients_i) (0))) (F_max0_ge_0 (s V_Reflection_coefficients_i));
      (*-11.7143 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - 
                                                                    s V_Reflection_coefficients_i) (0))) (F_max0_ge_0 (8
                                                                    - s V_Reflection_coefficients_i))]
     ((82 # 1) + s V_Reflection_coefficients_z
      - (82 # 7) * max0(7 - s V_Reflection_coefficients_i) <= z)%Q
   | 161 => (s V_Reflection_coefficients_z <= z)%Q
   | 162 => hints
     [(*0 10.7143*) F_one;
      (*-11.7143 0*) F_max0_pre_decrement 1 (8
                                             - s V_Reflection_coefficients_i) (1)]
     ((82 # 1) + s V_Reflection_coefficients_z
      - (82 # 7) * max0(7 - s V_Reflection_coefficients_i) <= z)%Q
   | 163 => ((83 # 1) + s V_Reflection_coefficients_z
             - (82 # 7) * max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 164 => ((83 # 1) + s V_Reflection_coefficients_z
             - (82 # 7) * max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 165 => ((83 # 1) + s V_Reflection_coefficients_z
             - (82 # 7) * max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | 166 => ((83 # 1) + s V_Reflection_coefficients_z
             - (82 # 7) * max0(8 - s V_Reflection_coefficients_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_Reflection_coefficients =>
    [mkPA Q (fun n z s => ai_Reflection_coefficients n s /\ annot0_Reflection_coefficients n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_Reflection_coefficients (proc_start P_Reflection_coefficients) s1 (proc_end P_Reflection_coefficients) s2 ->
    (s2 V_Reflection_coefficients_z <= (82 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_Reflection_coefficients.
Qed.
