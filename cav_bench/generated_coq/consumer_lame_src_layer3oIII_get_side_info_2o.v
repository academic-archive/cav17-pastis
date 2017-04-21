Require Import pasta.Pasta.

Inductive proc: Type :=
  P_III_get_side_info_2.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_III_get_side_info_2_z := 1%positive.
Notation V_III_get_side_info_2__tmp := 2%positive.
Notation V_III_get_side_info_2__tmp1 := 3%positive.
Notation V_III_get_side_info_2__tmp2 := 4%positive.
Notation V_III_get_side_info_2__tmp3 := 5%positive.
Notation V_III_get_side_info_2_ch := 6%positive.
Notation V_III_get_side_info_2_i := 7%positive.
Notation V_III_get_side_info_2_i1 := 8%positive.
Notation V_III_get_side_info_2_powdiff := 9%positive.
Notation V_III_get_side_info_2_qss := 10%positive.
Notation V_III_get_side_info_2_r0c := 11%positive.
Notation V_III_get_side_info_2_r1c := 12%positive.
Notation V_III_get_side_info_2_sbg := 13%positive.
Notation V_III_get_side_info_2_ms_stereo := 14%positive.
Notation V_III_get_side_info_2_sfreq := 15%positive.
Notation V_III_get_side_info_2_si := 16%positive.
Notation V_III_get_side_info_2_single := 17%positive.
Notation V_III_get_side_info_2_stereo := 18%positive.
Definition Pedges_III_get_side_info_2: list (edge proc) :=
  (EA 1 (AAssign V_III_get_side_info_2_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_III_get_side_info_2__tmp
  (Some (EVar V_III_get_side_info_2_stereo))) 3)::(EA 3 (AAssign
  V_III_get_side_info_2__tmp1
  (Some (EVar V_III_get_side_info_2_ms_stereo))) 4)::(EA 4 (AAssign
  V_III_get_side_info_2__tmp2 (Some (EVar V_III_get_side_info_2_sfreq))) 5)::
  (EA 5 (AAssign V_III_get_side_info_2__tmp3
  (Some (EVar V_III_get_side_info_2_single))) 6)::(EA 6 (AAssign
  V_III_get_side_info_2_powdiff None) 7)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_III_get_side_info_2__tmp) s) = (eval (ENum (1))
  s))%Z)) 11)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_III_get_side_info_2__tmp) s) <> (eval (ENum (1))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 ANone 13)::(EA 11 AWeaken 12)::
  (EA 12 ANone 13)::(EA 13 (AAssign V_III_get_side_info_2_ch
  (Some (ENum (0)))) 14)::(EA 14 ANone 15)::(EA 15 AWeaken 16)::
  (EA 16 (AGuard (fun s => ((eval (EVar V_III_get_side_info_2_ch) s) <
  (eval (EVar V_III_get_side_info_2__tmp) s))%Z)) 20)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_III_get_side_info_2_ch) s) >=
  (eval (EVar V_III_get_side_info_2__tmp) s))%Z)) 17)::(EA 17 AWeaken 18)::
  (EA 18 ANone 19)::(EA 19 AWeaken 70)::(EA 20 AWeaken 21)::
  (EA 21 ANone 22)::(EA 21 ANone 23)::(EA 22 ANone 23)::(EA 23 (AAssign
  V_III_get_side_info_2_qss None) 24)::(EA 24 AWeaken 25)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_III_get_side_info_2__tmp1) s) <> (eval (ENum (0))
  s))%Z)) 27)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_III_get_side_info_2__tmp1) s) = (eval (ENum (0))
  s))%Z)) 26)::(EA 26 AWeaken 30)::(EA 27 AWeaken 28)::(EA 28 ANone 29)::
  (EA 29 AWeaken 30)::(EA 30 ANone 46)::(EA 30 ANone 31)::(EA 31 (AAssign
  V_III_get_side_info_2_i1 (Some (ENum (0)))) 32)::(EA 32 ANone 33)::
  (EA 33 AWeaken 34)::(EA 34 (AGuard
  (fun s => ((eval (EVar V_III_get_side_info_2_i1) s) < (eval (ENum (3))
  s))%Z)) 39)::(EA 34 (AGuard
  (fun s => ((eval (EVar V_III_get_side_info_2_i1) s) >= (eval (ENum (3))
  s))%Z)) 35)::(EA 35 AWeaken 36)::(EA 36 (AAssign V_III_get_side_info_2_r0c
  None) 37)::(EA 37 (AAssign V_III_get_side_info_2_r1c None) 38)::
  (EA 38 ANone 63)::(EA 39 AWeaken 40)::(EA 40 ANone 41)::(EA 41 (AAssign
  V_III_get_side_info_2_i1 (Some (EAdd (EVar V_III_get_side_info_2_i1)
  (ENum (1))))) 42)::(EA 42 ANone 43)::(EA 43 ANone 44)::(EA 44 (AAssign
  V_III_get_side_info_2_z (Some (EAdd (ENum (1))
  (EVar V_III_get_side_info_2_z)))) 45)::(EA 45 AWeaken 34)::(EA 46 (AAssign
  V_III_get_side_info_2_i (Some (ENum (0)))) 47)::(EA 47 ANone 48)::
  (EA 48 AWeaken 49)::(EA 49 (AGuard
  (fun s => ((eval (EVar V_III_get_side_info_2_i) s) < (eval (ENum (3))
  s))%Z)) 71)::(EA 49 (AGuard (fun s => ((eval (EVar V_III_get_side_info_2_i)
  s) >= (eval (ENum (3)) s))%Z)) 50)::(EA 50 AWeaken 51)::(EA 51 ANone 69)::
  (EA 51 ANone 52)::(EA 52 AWeaken 53)::(EA 53 ANone 61)::(EA 53 ANone 54)::
  (EA 54 AWeaken 55)::(EA 55 (AGuard
  (fun s => ((eval (EVar V_III_get_side_info_2__tmp2) s) = (eval (ENum (8))
  s))%Z)) 58)::(EA 55 (AGuard
  (fun s => ((eval (EVar V_III_get_side_info_2__tmp2) s) <> (eval (ENum (8))
  s))%Z)) 56)::(EA 56 AWeaken 57)::(EA 57 ANone 60)::(EA 58 AWeaken 59)::
  (EA 59 ANone 60)::(EA 60 ANone 62)::(EA 61 ANone 62)::(EA 62 ANone 63)::
  (EA 63 ANone 64)::(EA 64 (AAssign V_III_get_side_info_2_ch
  (Some (EAdd (EVar V_III_get_side_info_2_ch) (ENum (1))))) 65)::
  (EA 65 ANone 66)::(EA 66 ANone 67)::(EA 67 (AAssign V_III_get_side_info_2_z
  (Some (EAdd (ENum (1)) (EVar V_III_get_side_info_2_z)))) 68)::
  (EA 68 AWeaken 16)::(EA 69 AWeaken 70)::(EA 71 AWeaken 72)::(EA 72 (AAssign
  V_III_get_side_info_2_sbg None) 73)::(EA 73 ANone 74)::(EA 74 (AAssign
  V_III_get_side_info_2_i (Some (EAdd (EVar V_III_get_side_info_2_i)
  (ENum (1))))) 75)::(EA 75 ANone 76)::(EA 76 ANone 77)::(EA 77 (AAssign
  V_III_get_side_info_2_z (Some (EAdd (ENum (1))
  (EVar V_III_get_side_info_2_z)))) 78)::(EA 78 AWeaken 49)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_III_get_side_info_2 => Pedges_III_get_side_info_2
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_III_get_side_info_2 => 70
     end)%positive;
  var_global := var_global
}.

Definition ai_III_get_side_info_2 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0)%Z
   | 3 => (-1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_z <= 0)%Z
   | 4 => (1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0)%Z
   | 5 => (-1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_z <= 0)%Z
   | 6 => (1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0)%Z
   | 7 => (-1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_z <= 0)%Z
   | 8 => (1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0)%Z
   | 9 => (-1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_z <= 0)%Z
   | 10 => (1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0)%Z
   | 11 => (-1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2__tmp + -1 <= 0 /\ -1 * s V_III_get_side_info_2__tmp + 1 <= 0)%Z
   | 12 => (-1 * s V_III_get_side_info_2__tmp + 1 <= 0 /\ 1 * s V_III_get_side_info_2__tmp + -1 <= 0 /\ 1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0)%Z
   | 13 => (-1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_z <= 0)%Z
   | 14 => (1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 15 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ 1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_z <= 0)%Z
   | 16 => (-1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 17 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2__tmp+ -1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 18 => (1 * s V_III_get_side_info_2__tmp+ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 19 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2__tmp+ -1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 20 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 21 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 22 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 23 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 24 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 25 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 26 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ 1 * s V_III_get_side_info_2__tmp1 <= 0 /\ -1 * s V_III_get_side_info_2__tmp1 <= 0)%Z
   | 27 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 28 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 29 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 30 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 31 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 32 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ 1 * s V_III_get_side_info_2_i1 <= 0 /\ -1 * s V_III_get_side_info_2_i1 <= 0)%Z
   | 33 => (-1 * s V_III_get_side_info_2_i1 <= 0 /\ 1 * s V_III_get_side_info_2_i1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 34 => (-1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i1 <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ 1 * s V_III_get_side_info_2_i1 + -3 <= 0)%Z
   | 35 => (1 * s V_III_get_side_info_2_i1 + -3 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i1 + 3 <= 0)%Z
   | 36 => (-1 * s V_III_get_side_info_2_i1 + 3 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ 1 * s V_III_get_side_info_2_i1 + -3 <= 0)%Z
   | 37 => (1 * s V_III_get_side_info_2_i1 + -3 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i1 + 3 <= 0)%Z
   | 38 => (-1 * s V_III_get_side_info_2_i1 + 3 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ 1 * s V_III_get_side_info_2_i1 + -3 <= 0)%Z
   | 39 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_i1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_i1 + -2 <= 0)%Z
   | 40 => (1 * s V_III_get_side_info_2_i1 + -2 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i1 <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 41 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_i1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_i1 + -2 <= 0)%Z
   | 42 => (-1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_i1 + 1 <= 0 /\ 1 * s V_III_get_side_info_2_i1 + -3 <= 0)%Z
   | 43 => (1 * s V_III_get_side_info_2_i1 + -3 <= 0 /\ -1 * s V_III_get_side_info_2_i1 + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0)%Z
   | 44 => (-1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_i1 + 1 <= 0 /\ 1 * s V_III_get_side_info_2_i1 + -3 <= 0)%Z
   | 45 => (1 * s V_III_get_side_info_2_i1 + -3 <= 0 /\ -1 * s V_III_get_side_info_2_i1 + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z + 1 <= 0)%Z
   | 46 => (-1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 47 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ 1 * s V_III_get_side_info_2_i <= 0 /\ -1 * s V_III_get_side_info_2_i <= 0)%Z
   | 48 => (-1 * s V_III_get_side_info_2_i <= 0 /\ 1 * s V_III_get_side_info_2_i <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 49 => (-1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ 1 * s V_III_get_side_info_2_i + -3 <= 0)%Z
   | 50 => (1 * s V_III_get_side_info_2_i + -3 <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i + 3 <= 0)%Z
   | 51 => (-1 * s V_III_get_side_info_2_i + 3 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ 1 * s V_III_get_side_info_2_i + -3 <= 0)%Z
   | 52 => (1 * s V_III_get_side_info_2_i + -3 <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i + 3 <= 0)%Z
   | 53 => (-1 * s V_III_get_side_info_2_i + 3 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ 1 * s V_III_get_side_info_2_i + -3 <= 0)%Z
   | 54 => (1 * s V_III_get_side_info_2_i + -3 <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i + 3 <= 0)%Z
   | 55 => (-1 * s V_III_get_side_info_2_i + 3 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ 1 * s V_III_get_side_info_2_i + -3 <= 0)%Z
   | 56 => (1 * s V_III_get_side_info_2_i + -3 <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i + 3 <= 0)%Z
   | 57 => (-1 * s V_III_get_side_info_2_i + 3 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ 1 * s V_III_get_side_info_2_i + -3 <= 0)%Z
   | 58 => (1 * s V_III_get_side_info_2_i + -3 <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i + 3 <= 0 /\ 1 * s V_III_get_side_info_2__tmp2 + -8 <= 0 /\ -1 * s V_III_get_side_info_2__tmp2 + 8 <= 0)%Z
   | 59 => (-1 * s V_III_get_side_info_2__tmp2 + 8 <= 0 /\ 1 * s V_III_get_side_info_2__tmp2 + -8 <= 0 /\ -1 * s V_III_get_side_info_2_i + 3 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ 1 * s V_III_get_side_info_2_i + -3 <= 0)%Z
   | 60 => (1 * s V_III_get_side_info_2_i + -3 <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i + 3 <= 0)%Z
   | 61 => (1 * s V_III_get_side_info_2_i + -3 <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i + 3 <= 0)%Z
   | 62 => (-1 * s V_III_get_side_info_2_i + 3 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ 1 * s V_III_get_side_info_2_i + -3 <= 0)%Z
   | 63 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0)%Z
   | 64 => (-1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 65 => (-1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 66 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0)%Z
   | 67 => (-1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 68 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z + 1 <= 0)%Z
   | 69 => (1 * s V_III_get_side_info_2_i + -3 <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i + 3 <= 0)%Z
   | 70 => (-1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0)%Z
   | 71 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_i <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_i + -2 <= 0)%Z
   | 72 => (1 * s V_III_get_side_info_2_i + -2 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 73 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_i <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_i + -2 <= 0)%Z
   | 74 => (1 * s V_III_get_side_info_2_i + -2 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_i <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 75 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_i + -3 <= 0 /\ -1 * s V_III_get_side_info_2_i + 1 <= 0)%Z
   | 76 => (-1 * s V_III_get_side_info_2_i + 1 <= 0 /\ 1 * s V_III_get_side_info_2_i + -3 <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0)%Z
   | 77 => (-1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2_z <= 0 /\ 1 * s V_III_get_side_info_2_i + -3 <= 0 /\ -1 * s V_III_get_side_info_2_i + 1 <= 0)%Z
   | 78 => (-1 * s V_III_get_side_info_2_i + 1 <= 0 /\ 1 * s V_III_get_side_info_2_i + -3 <= 0 /\ -1 * s V_III_get_side_info_2_ch <= 0 /\ -1 * s V_III_get_side_info_2__tmp+ 1 * s V_III_get_side_info_2_ch + 1 <= 0 /\ -1 * s V_III_get_side_info_2_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_III_get_side_info_2 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) * max0(s V_III_get_side_info_2_stereo) <= z)%Q
   | 2 => (s V_III_get_side_info_2_z
           + (4 # 1) * max0(s V_III_get_side_info_2_stereo) <= z)%Q
   | 3 => (s V_III_get_side_info_2_z
           + (4 # 1) * max0(s V_III_get_side_info_2__tmp) <= z)%Q
   | 4 => (s V_III_get_side_info_2_z
           + (4 # 1) * max0(s V_III_get_side_info_2__tmp) <= z)%Q
   | 5 => (s V_III_get_side_info_2_z
           + (4 # 1) * max0(s V_III_get_side_info_2__tmp) <= z)%Q
   | 6 => (s V_III_get_side_info_2_z
           + (4 # 1) * max0(s V_III_get_side_info_2__tmp) <= z)%Q
   | 7 => (s V_III_get_side_info_2_z
           + (4 # 1) * max0(s V_III_get_side_info_2__tmp) <= z)%Q
   | 8 => (s V_III_get_side_info_2_z
           + (4 # 1) * max0(s V_III_get_side_info_2__tmp) <= z)%Q
   | 9 => (s V_III_get_side_info_2_z
           + (4 # 1) * max0(s V_III_get_side_info_2__tmp) <= z)%Q
   | 10 => (s V_III_get_side_info_2_z
            + (4 # 1) * max0(s V_III_get_side_info_2__tmp) <= z)%Q
   | 11 => (s V_III_get_side_info_2_z
            + (4 # 1) * max0(s V_III_get_side_info_2__tmp) <= z)%Q
   | 12 => (s V_III_get_side_info_2_z
            + (4 # 1) * max0(s V_III_get_side_info_2__tmp) <= z)%Q
   | 13 => (s V_III_get_side_info_2_z
            + (4 # 1) * max0(s V_III_get_side_info_2__tmp) <= z)%Q
   | 14 => (s V_III_get_side_info_2_z
            + (4 # 1) * max0(s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_get_side_info_2_z) (0))) (F_max0_ge_0 (s V_III_get_side_info_2_z))]
     (s V_III_get_side_info_2_z
      + (4 # 1) * max0(s V_III_get_side_info_2__tmp
                       - s V_III_get_side_info_2_ch) <= z)%Q
   | 16 => ((4 # 1) * max0(s V_III_get_side_info_2__tmp
                           - s V_III_get_side_info_2_ch)
            + max0(s V_III_get_side_info_2_z) <= z)%Q
   | 17 => ((4 # 1) * max0(s V_III_get_side_info_2__tmp
                           - s V_III_get_side_info_2_ch)
            + max0(s V_III_get_side_info_2_z) <= z)%Q
   | 18 => ((4 # 1) * max0(s V_III_get_side_info_2__tmp
                           - s V_III_get_side_info_2_ch)
            + max0(s V_III_get_side_info_2_z) <= z)%Q
   | 19 => hints
     [(*-4 0*) F_max0_monotonic (F_check_ge (s V_III_get_side_info_2__tmp
                                             - s V_III_get_side_info_2_ch) (-1
                                                                    + s V_III_get_side_info_2__tmp
                                                                    - s V_III_get_side_info_2_ch));
      (*-4 0*) F_max0_ge_0 (-1 + s V_III_get_side_info_2__tmp
                            - s V_III_get_side_info_2_ch);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_III_get_side_info_2_z)) (F_check_ge (s V_III_get_side_info_2_z) (0))]
     ((4 # 1) * max0(s V_III_get_side_info_2__tmp
                     - s V_III_get_side_info_2_ch)
      + max0(s V_III_get_side_info_2_z) <= z)%Q
   | 20 => ((4 # 1) * max0(s V_III_get_side_info_2__tmp
                           - s V_III_get_side_info_2_ch)
            + max0(s V_III_get_side_info_2_z) <= z)%Q
   | 21 => ((4 # 1) * max0(s V_III_get_side_info_2__tmp
                           - s V_III_get_side_info_2_ch)
            + max0(s V_III_get_side_info_2_z) <= z)%Q
   | 22 => ((4 # 1) * max0(s V_III_get_side_info_2__tmp
                           - s V_III_get_side_info_2_ch)
            + max0(s V_III_get_side_info_2_z) <= z)%Q
   | 23 => ((4 # 1) * max0(s V_III_get_side_info_2__tmp
                           - s V_III_get_side_info_2_ch)
            + max0(s V_III_get_side_info_2_z) <= z)%Q
   | 24 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_III_get_side_info_2_z)) (F_check_ge (s V_III_get_side_info_2_z) (0))]
     ((4 # 1) * max0(s V_III_get_side_info_2__tmp
                     - s V_III_get_side_info_2_ch)
      + max0(s V_III_get_side_info_2_z) <= z)%Q
   | 25 => (s V_III_get_side_info_2_z
            + (4 # 1) * max0(s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 26 => hints
     [(*-4 0*) F_max0_pre_decrement 1 (s V_III_get_side_info_2__tmp
                                       - s V_III_get_side_info_2_ch) (1)]
     (s V_III_get_side_info_2_z
      + (4 # 1) * max0(s V_III_get_side_info_2__tmp
                       - s V_III_get_side_info_2_ch) <= z)%Q
   | 27 => (s V_III_get_side_info_2_z
            + (4 # 1) * max0(s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 28 => (s V_III_get_side_info_2_z
            + (4 # 1) * max0(s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 29 => hints
     [(*-4 0*) F_max0_pre_decrement 1 (s V_III_get_side_info_2__tmp
                                       - s V_III_get_side_info_2_ch) (1)]
     (s V_III_get_side_info_2_z
      + (4 # 1) * max0(s V_III_get_side_info_2__tmp
                       - s V_III_get_side_info_2_ch) <= z)%Q
   | 30 => ((4 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 31 => ((4 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 32 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i1) <= z)%Q
   | 33 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i1) <= z)%Q
   | 34 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i1) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3 - s V_III_get_side_info_2_i1) (2
                                                                    - s V_III_get_side_info_2_i1));
      (*-1 0*) F_max0_ge_0 (2 - s V_III_get_side_info_2_i1)]
     ((1 # 1) + s V_III_get_side_info_2_z
      + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                       - s V_III_get_side_info_2_ch)
      + max0(3 - s V_III_get_side_info_2_i1) <= z)%Q
   | 36 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 37 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 38 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 39 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (3 - s V_III_get_side_info_2_i1) (1)]
     ((1 # 1) + s V_III_get_side_info_2_z
      + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                       - s V_III_get_side_info_2_ch)
      + max0(3 - s V_III_get_side_info_2_i1) <= z)%Q
   | 40 => ((2 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(2 - s V_III_get_side_info_2_i1) <= z)%Q
   | 41 => ((2 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(2 - s V_III_get_side_info_2_i1) <= z)%Q
   | 42 => ((2 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i1) <= z)%Q
   | 43 => ((2 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i1) <= z)%Q
   | 44 => ((2 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i1) <= z)%Q
   | 45 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i1) <= z)%Q
   | 46 => ((4 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 47 => (s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + (4 # 3) * max0(3 - s V_III_get_side_info_2_i) <= z)%Q
   | 48 => hints
     [(*-0.333333 0*) F_max0_monotonic (F_check_ge (3
                                                    - s V_III_get_side_info_2_i) (3))]
     (s V_III_get_side_info_2_z
      + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                       - s V_III_get_side_info_2_ch)
      + (4 # 3) * max0(3 - s V_III_get_side_info_2_i) <= z)%Q
   | 49 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i) <= z)%Q
   | 50 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i) <= z)%Q
   | 51 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i) <= z)%Q
   | 52 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3 - s V_III_get_side_info_2_i) (2
                                                                    - s V_III_get_side_info_2_i));
      (*-1 0*) F_max0_ge_0 (2 - s V_III_get_side_info_2_i)]
     ((1 # 1) + s V_III_get_side_info_2_z
      + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                       - s V_III_get_side_info_2_ch)
      + max0(3 - s V_III_get_side_info_2_i) <= z)%Q
   | 53 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 54 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 55 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 56 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 57 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 58 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 59 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 60 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 61 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 62 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 63 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 64 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 65 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 66 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 67 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch) <= z)%Q
   | 68 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_III_get_side_info_2_z) (0))) (F_max0_ge_0 (s V_III_get_side_info_2_z))]
     (s V_III_get_side_info_2_z
      + (4 # 1) * max0(s V_III_get_side_info_2__tmp
                       - s V_III_get_side_info_2_ch) <= z)%Q
   | 69 => hints
     [(*-1 0*) F_one;
      (*-4 0*) F_max0_ge_0 (-1 + s V_III_get_side_info_2__tmp
                            - s V_III_get_side_info_2_ch);
      (*-1 0*) F_max0_monotonic (F_check_ge (3 - s V_III_get_side_info_2_i) (2
                                                                    - s V_III_get_side_info_2_i));
      (*-1 0*) F_max0_ge_0 (2 - s V_III_get_side_info_2_i)]
     ((1 # 1) + s V_III_get_side_info_2_z
      + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                       - s V_III_get_side_info_2_ch)
      + max0(3 - s V_III_get_side_info_2_i) <= z)%Q
   | 70 => (s V_III_get_side_info_2_z <= z)%Q
   | 71 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (3 - s V_III_get_side_info_2_i) (1)]
     ((1 # 1) + s V_III_get_side_info_2_z
      + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                       - s V_III_get_side_info_2_ch)
      + max0(3 - s V_III_get_side_info_2_i) <= z)%Q
   | 72 => ((2 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(2 - s V_III_get_side_info_2_i) <= z)%Q
   | 73 => ((2 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(2 - s V_III_get_side_info_2_i) <= z)%Q
   | 74 => ((2 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(2 - s V_III_get_side_info_2_i) <= z)%Q
   | 75 => ((2 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i) <= z)%Q
   | 76 => ((2 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i) <= z)%Q
   | 77 => ((2 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i) <= z)%Q
   | 78 => ((1 # 1) + s V_III_get_side_info_2_z
            + (4 # 1) * max0(-1 + s V_III_get_side_info_2__tmp
                             - s V_III_get_side_info_2_ch)
            + max0(3 - s V_III_get_side_info_2_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_III_get_side_info_2 =>
    [mkPA Q (fun n z s => ai_III_get_side_info_2 n s /\ annot0_III_get_side_info_2 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_III_get_side_info_2 (proc_start P_III_get_side_info_2) s1 (proc_end P_III_get_side_info_2) s2 ->
    (s2 V_III_get_side_info_2_z <= (4 # 1) * max0(s1 V_III_get_side_info_2_stereo))%Q.
Proof.
  prove_bound ipa admissible_ipa P_III_get_side_info_2.
Qed.
