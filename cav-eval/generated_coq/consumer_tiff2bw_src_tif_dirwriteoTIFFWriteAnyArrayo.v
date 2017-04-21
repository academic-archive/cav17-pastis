Require Import pasta.Pasta.

Inductive proc: Type :=
  P_TIFFWriteAnyArray.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_TIFFWriteAnyArray_z := 1%positive.
Notation V_TIFFWriteAnyArray__tmp := 2%positive.
Notation V_TIFFWriteAnyArray__tmp1 := 3%positive.
Notation V_TIFFWriteAnyArray__tmp2 := 4%positive.
Notation V_TIFFWriteAnyArray__tmp3 := 5%positive.
Notation V_TIFFWriteAnyArray_i := 6%positive.
Notation V_TIFFWriteAnyArray_status := 7%positive.
Notation V_TIFFWriteAnyArray_dir := 8%positive.
Notation V_TIFFWriteAnyArray_n := 9%positive.
Notation V_TIFFWriteAnyArray_tag := 10%positive.
Notation V_TIFFWriteAnyArray_tif := 11%positive.
Notation V_TIFFWriteAnyArray_type := 12%positive.
Notation V_TIFFWriteAnyArray_v := 13%positive.
Definition Pedges_TIFFWriteAnyArray: list (edge proc) :=
  (EA 1 (AAssign V_TIFFWriteAnyArray_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_TIFFWriteAnyArray__tmp3 (Some (EVar V_TIFFWriteAnyArray_type))) 3)::
  (EA 3 (AAssign V_TIFFWriteAnyArray__tmp2
  (Some (EVar V_TIFFWriteAnyArray_tag))) 4)::(EA 4 (AAssign
  V_TIFFWriteAnyArray__tmp (Some (EVar V_TIFFWriteAnyArray_n))) 5)::
  (EA 5 (AAssign V_TIFFWriteAnyArray_status (Some (ENum (0)))) 6)::
  (EA 6 AWeaken 7)::(EA 7 ANone 9)::(EA 7 ANone 8)::(EA 8 AWeaken 11)::
  (EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 ANone 130)::(EA 11 ANone 111)::
  (EA 11 ANone 95)::(EA 11 ANone 79)::(EA 11 ANone 63)::(EA 11 ANone 47)::
  (EA 11 ANone 31)::(EA 11 ANone 15)::(EA 11 ANone 12)::(EA 12 (AAssign
  V_TIFFWriteAnyArray__tmp1 None) 13)::(EA 13 ANone 14)::
  (EA 14 AWeaken 137)::(EA 15 (AAssign V_TIFFWriteAnyArray_i
  (Some (ENum (0)))) 16)::(EA 16 ANone 17)::(EA 17 AWeaken 18)::
  (EA 18 (AGuard (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) <
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 24)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) >=
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 19)::(EA 19 AWeaken 20)::
  (EA 20 ANone 23)::(EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 AWeaken 132)::
  (EA 23 ANone 120)::(EA 24 AWeaken 25)::(EA 25 ANone 26)::(EA 26 (AAssign
  V_TIFFWriteAnyArray_i (Some (EAdd (EVar V_TIFFWriteAnyArray_i)
  (ENum (1))))) 27)::(EA 27 ANone 28)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_TIFFWriteAnyArray_z (Some (EAdd (ENum (1))
  (EVar V_TIFFWriteAnyArray_z)))) 30)::(EA 30 AWeaken 18)::(EA 31 (AAssign
  V_TIFFWriteAnyArray_i (Some (ENum (0)))) 32)::(EA 32 ANone 33)::
  (EA 33 AWeaken 34)::(EA 34 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) <
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 40)::(EA 34 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) >=
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 35)::(EA 35 AWeaken 36)::
  (EA 36 ANone 39)::(EA 36 ANone 37)::(EA 37 ANone 38)::(EA 38 AWeaken 132)::
  (EA 39 ANone 120)::(EA 40 AWeaken 41)::(EA 41 ANone 42)::(EA 42 (AAssign
  V_TIFFWriteAnyArray_i (Some (EAdd (EVar V_TIFFWriteAnyArray_i)
  (ENum (1))))) 43)::(EA 43 ANone 44)::(EA 44 ANone 45)::(EA 45 (AAssign
  V_TIFFWriteAnyArray_z (Some (EAdd (ENum (1))
  (EVar V_TIFFWriteAnyArray_z)))) 46)::(EA 46 AWeaken 34)::(EA 47 (AAssign
  V_TIFFWriteAnyArray_i (Some (ENum (0)))) 48)::(EA 48 ANone 49)::
  (EA 49 AWeaken 50)::(EA 50 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) <
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 56)::(EA 50 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) >=
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 51)::(EA 51 AWeaken 52)::
  (EA 52 ANone 55)::(EA 52 ANone 53)::(EA 53 ANone 54)::(EA 54 AWeaken 132)::
  (EA 55 ANone 120)::(EA 56 AWeaken 57)::(EA 57 ANone 58)::(EA 58 (AAssign
  V_TIFFWriteAnyArray_i (Some (EAdd (EVar V_TIFFWriteAnyArray_i)
  (ENum (1))))) 59)::(EA 59 ANone 60)::(EA 60 ANone 61)::(EA 61 (AAssign
  V_TIFFWriteAnyArray_z (Some (EAdd (ENum (1))
  (EVar V_TIFFWriteAnyArray_z)))) 62)::(EA 62 AWeaken 50)::(EA 63 (AAssign
  V_TIFFWriteAnyArray_i (Some (ENum (0)))) 64)::(EA 64 ANone 65)::
  (EA 65 AWeaken 66)::(EA 66 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) <
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 72)::(EA 66 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) >=
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 67)::(EA 67 AWeaken 68)::
  (EA 68 ANone 71)::(EA 68 ANone 69)::(EA 69 ANone 70)::(EA 70 AWeaken 132)::
  (EA 71 ANone 120)::(EA 72 AWeaken 73)::(EA 73 ANone 74)::(EA 74 (AAssign
  V_TIFFWriteAnyArray_i (Some (EAdd (EVar V_TIFFWriteAnyArray_i)
  (ENum (1))))) 75)::(EA 75 ANone 76)::(EA 76 ANone 77)::(EA 77 (AAssign
  V_TIFFWriteAnyArray_z (Some (EAdd (ENum (1))
  (EVar V_TIFFWriteAnyArray_z)))) 78)::(EA 78 AWeaken 66)::(EA 79 (AAssign
  V_TIFFWriteAnyArray_i (Some (ENum (0)))) 80)::(EA 80 ANone 81)::
  (EA 81 AWeaken 82)::(EA 82 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) <
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 88)::(EA 82 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) >=
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 83)::(EA 83 AWeaken 84)::
  (EA 84 ANone 87)::(EA 84 ANone 85)::(EA 85 ANone 86)::(EA 86 AWeaken 132)::
  (EA 87 ANone 120)::(EA 88 AWeaken 89)::(EA 89 ANone 90)::(EA 90 (AAssign
  V_TIFFWriteAnyArray_i (Some (EAdd (EVar V_TIFFWriteAnyArray_i)
  (ENum (1))))) 91)::(EA 91 ANone 92)::(EA 92 ANone 93)::(EA 93 (AAssign
  V_TIFFWriteAnyArray_z (Some (EAdd (ENum (1))
  (EVar V_TIFFWriteAnyArray_z)))) 94)::(EA 94 AWeaken 82)::(EA 95 (AAssign
  V_TIFFWriteAnyArray_i (Some (ENum (0)))) 96)::(EA 96 ANone 97)::
  (EA 97 AWeaken 98)::(EA 98 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) <
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 104)::(EA 98 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) >=
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 99)::(EA 99 AWeaken 100)::
  (EA 100 ANone 103)::(EA 100 ANone 101)::(EA 101 ANone 102)::
  (EA 102 AWeaken 132)::(EA 103 ANone 120)::(EA 104 AWeaken 105)::
  (EA 105 ANone 106)::(EA 106 (AAssign V_TIFFWriteAnyArray_i
  (Some (EAdd (EVar V_TIFFWriteAnyArray_i) (ENum (1))))) 107)::
  (EA 107 ANone 108)::(EA 108 ANone 109)::(EA 109 (AAssign
  V_TIFFWriteAnyArray_z (Some (EAdd (ENum (1))
  (EVar V_TIFFWriteAnyArray_z)))) 110)::(EA 110 AWeaken 98)::(EA 111 (AAssign
  V_TIFFWriteAnyArray_i (Some (ENum (0)))) 112)::(EA 112 ANone 113)::
  (EA 113 AWeaken 114)::(EA 114 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) <
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 123)::(EA 114 (AGuard
  (fun s => ((eval (EVar V_TIFFWriteAnyArray_i) s) >=
  (eval (EVar V_TIFFWriteAnyArray__tmp) s))%Z)) 115)::(EA 115 AWeaken 116)::
  (EA 116 ANone 119)::(EA 116 ANone 117)::(EA 117 ANone 118)::
  (EA 118 AWeaken 132)::(EA 119 ANone 120)::(EA 120 (AAssign
  V_TIFFWriteAnyArray_status (Some (ENum (1)))) 121)::(EA 121 ANone 122)::
  (EA 122 AWeaken 132)::(EA 123 AWeaken 124)::(EA 124 ANone 125)::
  (EA 125 (AAssign V_TIFFWriteAnyArray_i
  (Some (EAdd (EVar V_TIFFWriteAnyArray_i) (ENum (1))))) 126)::
  (EA 126 ANone 127)::(EA 127 ANone 128)::(EA 128 (AAssign
  V_TIFFWriteAnyArray_z (Some (EAdd (ENum (1))
  (EVar V_TIFFWriteAnyArray_z)))) 129)::(EA 129 AWeaken 114)::
  (EA 130 ANone 131)::(EA 131 AWeaken 132)::(EA 132 ANone 133)::
  (EA 132 ANone 134)::(EA 133 ANone 134)::(EA 134 (AAssign
  V_TIFFWriteAnyArray__tmp1 (Some (EVar V_TIFFWriteAnyArray_status))) 135)::
  (EA 135 ANone 136)::(EA 136 AWeaken 137)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_TIFFWriteAnyArray => Pedges_TIFFWriteAnyArray
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_TIFFWriteAnyArray => 137
     end)%positive;
  var_global := var_global
}.

Definition ai_TIFFWriteAnyArray (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 3 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 4 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 5 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 6 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 7 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 8 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 9 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 10 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 11 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 12 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 13 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 14 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 15 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 16 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 17 => (-1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 18 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 19 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 20 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 21 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 22 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 23 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 24 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 25 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 26 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 27 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 28 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 29 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 30 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z + 1 <= 0)%Z
   | 31 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 32 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 33 => (-1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 34 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 35 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 36 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 37 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 38 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 39 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 40 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 41 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 42 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 43 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 44 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 45 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 46 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z + 1 <= 0)%Z
   | 47 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 48 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 49 => (-1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 50 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 51 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 52 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 53 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 54 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 55 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 56 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 57 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 58 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 59 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 60 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 61 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 62 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z + 1 <= 0)%Z
   | 63 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 64 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 65 => (-1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 66 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 67 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 68 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 69 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 70 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 71 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 72 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 73 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 74 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 75 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 76 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 77 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 78 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z + 1 <= 0)%Z
   | 79 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 80 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 81 => (-1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 82 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 83 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 84 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 85 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 86 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 87 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 88 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 89 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 90 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 91 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 92 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 93 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 94 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z + 1 <= 0)%Z
   | 95 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 96 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 97 => (-1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 98 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 99 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 100 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 101 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 102 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 103 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 104 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 105 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 106 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 107 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 108 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 109 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 110 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z + 1 <= 0)%Z
   | 111 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 112 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 113 => (-1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 114 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 115 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 116 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 117 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 118 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 119 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 120 => (1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 121 => (-1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status + -1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status + 1 <= 0)%Z
   | 122 => (-1 * s V_TIFFWriteAnyArray_status + 1 <= 0 /\ 1 * s V_TIFFWriteAnyArray_status + -1 <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp+ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 123 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 124 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 125 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i + 1 <= 0)%Z
   | 126 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 127 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 128 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0)%Z
   | 129 => (-1 * s V_TIFFWriteAnyArray__tmp+ 1 * s V_TIFFWriteAnyArray_i <= 0 /\ -1 * s V_TIFFWriteAnyArray_i + 1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z + 1 <= 0)%Z
   | 130 => (-1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 131 => (1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0 /\ 1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0)%Z
   | 132 => (1 * s V_TIFFWriteAnyArray_status + -1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 133 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status + -1 <= 0)%Z
   | 134 => (1 * s V_TIFFWriteAnyArray_status + -1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 135 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status + -1 <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp1 + -1 <= 0 /\ -1 * s V_TIFFWriteAnyArray__tmp1 <= 0)%Z
   | 136 => (-1 * s V_TIFFWriteAnyArray__tmp1 <= 0 /\ 1 * s V_TIFFWriteAnyArray__tmp1 + -1 <= 0 /\ 1 * s V_TIFFWriteAnyArray_status + -1 <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ -1 * s V_TIFFWriteAnyArray_z <= 0)%Z
   | 137 => (-1 * s V_TIFFWriteAnyArray_z <= 0 /\ -1 * s V_TIFFWriteAnyArray_status <= 0 /\ 1 * s V_TIFFWriteAnyArray_status + -1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_TIFFWriteAnyArray (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_TIFFWriteAnyArray_n) <= z)%Q
   | 2 => (max0(s V_TIFFWriteAnyArray_n) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 3 => (max0(s V_TIFFWriteAnyArray_n) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 4 => (max0(s V_TIFFWriteAnyArray_n) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 5 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 6 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 7 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 8 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 9 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 10 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 11 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 12 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 13 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_ge_0 (s V_TIFFWriteAnyArray__tmp);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_TIFFWriteAnyArray_z)) (F_check_ge (s V_TIFFWriteAnyArray_z) (0))]
     (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 15 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 16 => (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
            + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_TIFFWriteAnyArray_z)) (F_check_ge (s V_TIFFWriteAnyArray_z) (0))]
     (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
      + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 18 => (s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFWriteAnyArray__tmp
                                             - s V_TIFFWriteAnyArray_i) (-1
                                                                    + s V_TIFFWriteAnyArray__tmp
                                                                    - s V_TIFFWriteAnyArray_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFWriteAnyArray__tmp
                            - s V_TIFFWriteAnyArray_i)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 20 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 21 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 22 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 23 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 24 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_TIFFWriteAnyArray__tmp
                                      - s V_TIFFWriteAnyArray_i) (1)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 25 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 26 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 27 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 28 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 29 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 30 => (s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 31 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 32 => (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
            + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_TIFFWriteAnyArray_z)) (F_check_ge (s V_TIFFWriteAnyArray_z) (0))]
     (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
      + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 34 => (s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFWriteAnyArray__tmp
                                             - s V_TIFFWriteAnyArray_i) (-1
                                                                    + s V_TIFFWriteAnyArray__tmp
                                                                    - s V_TIFFWriteAnyArray_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFWriteAnyArray__tmp
                            - s V_TIFFWriteAnyArray_i)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 36 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 37 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 38 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 39 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 40 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_TIFFWriteAnyArray__tmp
                                      - s V_TIFFWriteAnyArray_i) (1)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 41 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 42 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 43 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 44 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 45 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 46 => (s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 47 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 48 => (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
            + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 49 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_TIFFWriteAnyArray_z)) (F_check_ge (s V_TIFFWriteAnyArray_z) (0))]
     (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
      + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 50 => (s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 51 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFWriteAnyArray__tmp
                                             - s V_TIFFWriteAnyArray_i) (-1
                                                                    + s V_TIFFWriteAnyArray__tmp
                                                                    - s V_TIFFWriteAnyArray_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFWriteAnyArray__tmp
                            - s V_TIFFWriteAnyArray_i)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 52 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 53 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 54 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 55 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 56 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_TIFFWriteAnyArray__tmp
                                      - s V_TIFFWriteAnyArray_i) (1)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 57 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 58 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 59 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 60 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 61 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 62 => (s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 63 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 64 => (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
            + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 65 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_TIFFWriteAnyArray_z)) (F_check_ge (s V_TIFFWriteAnyArray_z) (0))]
     (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
      + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 66 => (s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 67 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFWriteAnyArray__tmp
                                             - s V_TIFFWriteAnyArray_i) (-1
                                                                    + s V_TIFFWriteAnyArray__tmp
                                                                    - s V_TIFFWriteAnyArray_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFWriteAnyArray__tmp
                            - s V_TIFFWriteAnyArray_i)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 68 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 69 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 70 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 71 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 72 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_TIFFWriteAnyArray__tmp
                                      - s V_TIFFWriteAnyArray_i) (1)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 73 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 74 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 75 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 76 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 77 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 78 => (s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 79 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 80 => (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
            + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 81 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_TIFFWriteAnyArray_z)) (F_check_ge (s V_TIFFWriteAnyArray_z) (0))]
     (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
      + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 82 => (s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 83 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFWriteAnyArray__tmp
                                             - s V_TIFFWriteAnyArray_i) (-1
                                                                    + s V_TIFFWriteAnyArray__tmp
                                                                    - s V_TIFFWriteAnyArray_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFWriteAnyArray__tmp
                            - s V_TIFFWriteAnyArray_i)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 84 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 85 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 86 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 87 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 88 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_TIFFWriteAnyArray__tmp
                                      - s V_TIFFWriteAnyArray_i) (1)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 89 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 90 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 91 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 92 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 93 => ((1 # 1) + s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 94 => (s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 95 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 96 => (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
            + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 97 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_TIFFWriteAnyArray_z)) (F_check_ge (s V_TIFFWriteAnyArray_z) (0))]
     (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
      + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 98 => (s V_TIFFWriteAnyArray_z
            + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 99 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFWriteAnyArray__tmp
                                             - s V_TIFFWriteAnyArray_i) (-1
                                                                    + s V_TIFFWriteAnyArray__tmp
                                                                    - s V_TIFFWriteAnyArray_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFWriteAnyArray__tmp
                            - s V_TIFFWriteAnyArray_i)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 100 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 101 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 102 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 103 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 104 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_TIFFWriteAnyArray__tmp
                                      - s V_TIFFWriteAnyArray_i) (1)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 105 => ((1 # 1) + s V_TIFFWriteAnyArray_z
             + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 106 => ((1 # 1) + s V_TIFFWriteAnyArray_z
             + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 107 => ((1 # 1) + s V_TIFFWriteAnyArray_z
             + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 108 => ((1 # 1) + s V_TIFFWriteAnyArray_z
             + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 109 => ((1 # 1) + s V_TIFFWriteAnyArray_z
             + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 110 => (s V_TIFFWriteAnyArray_z
             + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 111 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 112 => (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
             + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 113 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_TIFFWriteAnyArray_z)) (F_check_ge (s V_TIFFWriteAnyArray_z) (0))]
     (max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i)
      + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 114 => (s V_TIFFWriteAnyArray_z
             + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 115 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFWriteAnyArray__tmp
                                             - s V_TIFFWriteAnyArray_i) (-1
                                                                    + s V_TIFFWriteAnyArray__tmp
                                                                    - s V_TIFFWriteAnyArray_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFWriteAnyArray__tmp
                            - s V_TIFFWriteAnyArray_i)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 116 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 117 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 118 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 119 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 120 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 121 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 122 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 123 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_TIFFWriteAnyArray__tmp
                                      - s V_TIFFWriteAnyArray_i) (1)]
     (s V_TIFFWriteAnyArray_z
      + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 124 => ((1 # 1) + s V_TIFFWriteAnyArray_z
             + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 125 => ((1 # 1) + s V_TIFFWriteAnyArray_z
             + max0(-1 + s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 126 => ((1 # 1) + s V_TIFFWriteAnyArray_z
             + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 127 => ((1 # 1) + s V_TIFFWriteAnyArray_z
             + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 128 => ((1 # 1) + s V_TIFFWriteAnyArray_z
             + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 129 => (s V_TIFFWriteAnyArray_z
             + max0(s V_TIFFWriteAnyArray__tmp - s V_TIFFWriteAnyArray_i) <= z)%Q
   | 130 => (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 131 => hints
     [(*-1 0*) F_max0_ge_0 (s V_TIFFWriteAnyArray__tmp);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_TIFFWriteAnyArray_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_TIFFWriteAnyArray_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_TIFFWriteAnyArray_z) (0))) (F_max0_ge_0 (-
                                                                    s V_TIFFWriteAnyArray_z))]
     (max0(s V_TIFFWriteAnyArray__tmp) + max0(s V_TIFFWriteAnyArray_z) <= z)%Q
   | 132 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 133 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 134 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 135 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 136 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | 137 => (s V_TIFFWriteAnyArray_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_TIFFWriteAnyArray =>
    [mkPA Q (fun n z s => ai_TIFFWriteAnyArray n s /\ annot0_TIFFWriteAnyArray n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_TIFFWriteAnyArray (proc_start P_TIFFWriteAnyArray) s1 (proc_end P_TIFFWriteAnyArray) s2 ->
    (s2 V_TIFFWriteAnyArray_z <= max0(s1 V_TIFFWriteAnyArray_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_TIFFWriteAnyArray.
Qed.
