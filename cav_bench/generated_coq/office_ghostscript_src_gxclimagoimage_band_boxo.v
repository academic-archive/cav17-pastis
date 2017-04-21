Require Import pasta.Pasta.

Inductive proc: Type :=
  P_image_band_box.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_image_band_box_z := 1%positive.
Notation V_image_band_box__tmp := 2%positive.
Notation V_image_band_box__tmp1 := 3%positive.
Notation V_image_band_box_by0 := 4%positive.
Notation V_image_band_box_by1 := 5%positive.
Notation V_image_band_box_i := 6%positive.
Notation V_image_band_box_px := 7%positive.
Notation V_image_band_box_py := 8%positive.
Notation V_image_band_box_qx := 9%positive.
Notation V_image_band_box_qy := 10%positive.
Notation V_image_band_box_dev := 11%positive.
Notation V_image_band_box_h := 12%positive.
Notation V_image_band_box_pbox := 13%positive.
Notation V_image_band_box_pie := 14%positive.
Notation V_image_band_box_y := 15%positive.
Definition Pedges_image_band_box: list (edge proc) :=
  (EA 1 (AAssign V_image_band_box_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_image_band_box__tmp1 (Some (EVar V_image_band_box_y))) 3)::(EA 3 (AAssign
  V_image_band_box__tmp (Some (EVar V_image_band_box_h))) 4)::(EA 4 (AAssign
  V_image_band_box_by0 None) 5)::(EA 5 (AAssign V_image_band_box_by1
  None) 6)::(EA 6 (AAssign V_image_band_box_px None) 7)::(EA 7 (AAssign
  V_image_band_box_py None) 8)::(EA 8 (AAssign V_image_band_box_qx None) 9)::
  (EA 9 (AAssign V_image_band_box_qy None) 10)::(EA 10 AWeaken 11)::
  (EA 11 ANone 14)::(EA 11 ANone 12)::(EA 12 ANone 13)::(EA 13 AWeaken 16)::
  (EA 14 ANone 15)::(EA 15 AWeaken 16)::(EA 16 ANone 19)::(EA 16 ANone 17)::
  (EA 17 ANone 18)::(EA 18 AWeaken 21)::(EA 19 ANone 20)::
  (EA 20 AWeaken 21)::(EA 21 ANone 23)::(EA 21 ANone 22)::
  (EA 22 AWeaken 26)::(EA 23 AWeaken 24)::(EA 24 ANone 119)::
  (EA 24 ANone 25)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::(EA 26 ANone 29)::
  (EA 27 AWeaken 28)::(EA 28 ANone 118)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_image_band_box_i (Some (ENum (0)))) 30)::(EA 30 ANone 31)::
  (EA 31 AWeaken 32)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_image_band_box_i) s) < (eval (ENum (4))
  s))%Z)) 35)::(EA 32 (AGuard (fun s => ((eval (EVar V_image_band_box_i)
  s) >= (eval (ENum (4)) s))%Z)) 33)::(EA 33 AWeaken 34)::(EA 34 ANone 139)::
  (EA 35 AWeaken 36)::(EA 36 ANone 38)::(EA 36 ANone 37)::
  (EA 37 AWeaken 49)::(EA 38 AWeaken 39)::(EA 39 ANone 41)::
  (EA 39 ANone 40)::(EA 40 AWeaken 49)::(EA 41 AWeaken 42)::
  (EA 42 ANone 44)::(EA 42 ANone 43)::(EA 43 AWeaken 49)::
  (EA 44 AWeaken 45)::(EA 45 ANone 47)::(EA 45 ANone 46)::
  (EA 46 AWeaken 49)::(EA 47 ANone 48)::(EA 48 AWeaken 49)::
  (EA 49 ANone 51)::(EA 49 ANone 50)::(EA 50 AWeaken 62)::
  (EA 51 AWeaken 52)::(EA 52 ANone 54)::(EA 52 ANone 53)::
  (EA 53 AWeaken 62)::(EA 54 AWeaken 55)::(EA 55 ANone 57)::
  (EA 55 ANone 56)::(EA 56 AWeaken 62)::(EA 57 AWeaken 58)::
  (EA 58 ANone 60)::(EA 58 ANone 59)::(EA 59 AWeaken 62)::(EA 60 ANone 61)::
  (EA 61 AWeaken 62)::(EA 62 ANone 64)::(EA 62 ANone 63)::
  (EA 63 AWeaken 88)::(EA 64 ANone 65)::(EA 65 ANone 66)::
  (EA 66 AWeaken 67)::(EA 67 ANone 68)::(EA 67 ANone 75)::
  (EA 68 AWeaken 69)::(EA 69 ANone 70)::(EA 69 ANone 75)::
  (EA 70 AWeaken 71)::(EA 71 ANone 72)::(EA 71 ANone 75)::
  (EA 72 AWeaken 73)::(EA 73 ANone 74)::(EA 73 ANone 75)::(EA 74 ANone 75)::
  (EA 75 ANone 76)::(EA 76 ANone 77)::(EA 77 AWeaken 78)::(EA 78 ANone 79)::
  (EA 78 ANone 86)::(EA 79 AWeaken 80)::(EA 80 ANone 81)::(EA 80 ANone 86)::
  (EA 81 AWeaken 82)::(EA 82 ANone 83)::(EA 82 ANone 86)::
  (EA 83 AWeaken 84)::(EA 84 ANone 85)::(EA 84 ANone 86)::(EA 85 ANone 86)::
  (EA 86 ANone 87)::(EA 87 AWeaken 88)::(EA 88 ANone 89)::(EA 88 ANone 112)::
  (EA 89 ANone 90)::(EA 90 ANone 91)::(EA 91 AWeaken 92)::(EA 92 ANone 93)::
  (EA 92 ANone 100)::(EA 93 AWeaken 94)::(EA 94 ANone 95)::
  (EA 94 ANone 100)::(EA 95 AWeaken 96)::(EA 96 ANone 97)::
  (EA 96 ANone 100)::(EA 97 AWeaken 98)::(EA 98 ANone 99)::
  (EA 98 ANone 100)::(EA 99 ANone 100)::(EA 100 ANone 101)::
  (EA 101 ANone 102)::(EA 102 AWeaken 103)::(EA 103 ANone 104)::
  (EA 103 ANone 111)::(EA 104 AWeaken 105)::(EA 105 ANone 106)::
  (EA 105 ANone 111)::(EA 106 AWeaken 107)::(EA 107 ANone 108)::
  (EA 107 ANone 111)::(EA 108 AWeaken 109)::(EA 109 ANone 110)::
  (EA 109 ANone 111)::(EA 110 ANone 111)::(EA 111 ANone 112)::
  (EA 112 ANone 113)::(EA 113 (AAssign V_image_band_box_i
  (Some (EAdd (EVar V_image_band_box_i) (ENum (1))))) 114)::
  (EA 114 ANone 115)::(EA 115 ANone 116)::(EA 116 (AAssign V_image_band_box_z
  (Some (EAdd (ENum (1)) (EVar V_image_band_box_z)))) 117)::
  (EA 117 AWeaken 32)::(EA 118 AWeaken 120)::(EA 119 AWeaken 120)::
  (EA 120 ANone 123)::(EA 120 ANone 121)::(EA 121 ANone 122)::
  (EA 122 AWeaken 125)::(EA 123 ANone 124)::(EA 124 AWeaken 125)::
  (EA 125 ANone 128)::(EA 125 ANone 126)::(EA 126 ANone 127)::
  (EA 127 AWeaken 130)::(EA 128 ANone 129)::(EA 129 AWeaken 130)::
  (EA 130 ANone 133)::(EA 130 ANone 131)::(EA 131 ANone 132)::
  (EA 132 AWeaken 135)::(EA 133 ANone 134)::(EA 134 AWeaken 135)::
  (EA 135 ANone 137)::(EA 135 ANone 136)::(EA 136 ANone 138)::
  (EA 137 ANone 138)::(EA 138 ANone 139)::(EA 139 ANone 140)::
  (EA 140 ANone 141)::(EA 141 AWeaken 142)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_image_band_box => Pedges_image_band_box
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_image_band_box => 142
     end)%positive;
  var_global := var_global
}.

Definition ai_image_band_box (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 3 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 4 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 5 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 6 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 7 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 8 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 9 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 10 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 11 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 12 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 13 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 14 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 15 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 16 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 17 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 18 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 19 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 20 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 21 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 22 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 23 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 24 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 25 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 26 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 27 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 28 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 29 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 30 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 31 => (-1 * s V_image_band_box_i <= 0 /\ 1 * s V_image_band_box_i <= 0 /\ 1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 32 => (-1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0 /\ 1 * s V_image_band_box_i + -4 <= 0)%Z
   | 33 => (1 * s V_image_band_box_i + -4 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i + 4 <= 0)%Z
   | 34 => (-1 * s V_image_band_box_i + 4 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -4 <= 0)%Z
   | 35 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 36 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 37 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 38 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 39 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 40 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 41 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 42 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 43 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 44 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 45 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 46 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 47 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 48 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 49 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 50 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 51 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 52 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 53 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 54 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 55 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 56 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 57 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 58 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 59 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 60 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 61 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 62 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 63 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 64 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 65 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 66 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 67 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 68 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 69 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 70 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 71 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 72 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 73 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 74 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 75 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 76 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 77 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 78 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 79 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 80 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 81 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 82 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 83 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 84 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 85 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 86 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 87 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 88 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 89 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 90 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 91 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 92 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 93 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 94 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 95 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 96 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 97 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 98 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 99 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 100 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 101 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 102 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 103 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 104 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 105 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 106 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 107 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 108 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 109 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 110 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 111 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 112 => (-1 * s V_image_band_box_i <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -3 <= 0)%Z
   | 113 => (1 * s V_image_band_box_i + -3 <= 0 /\ -1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_i <= 0)%Z
   | 114 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -4 <= 0 /\ -1 * s V_image_band_box_i + 1 <= 0)%Z
   | 115 => (-1 * s V_image_band_box_i + 1 <= 0 /\ 1 * s V_image_band_box_i + -4 <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 116 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_i + -4 <= 0 /\ -1 * s V_image_band_box_i + 1 <= 0)%Z
   | 117 => (-1 * s V_image_band_box_i + 1 <= 0 /\ 1 * s V_image_band_box_i + -4 <= 0 /\ -1 * s V_image_band_box_z + 1 <= 0)%Z
   | 118 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 119 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 120 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 121 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 122 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 123 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 124 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 125 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 126 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 127 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 128 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 129 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 130 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 131 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 132 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 133 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 134 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 135 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 136 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 137 => (-1 * s V_image_band_box_z <= 0 /\ 1 * s V_image_band_box_z <= 0)%Z
   | 138 => (1 * s V_image_band_box_z <= 0 /\ -1 * s V_image_band_box_z <= 0)%Z
   | 139 => (-1 * s V_image_band_box_z <= 0)%Z
   | 140 => (-1 * s V_image_band_box_z <= 0)%Z
   | 141 => (-1 * s V_image_band_box_z <= 0)%Z
   | 142 => (-1 * s V_image_band_box_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_image_band_box (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 3 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 4 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 5 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 6 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 7 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 8 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 9 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 10 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 11 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 12 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 13 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 14 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 15 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 16 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 17 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 18 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 19 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 20 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 21 => ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 22 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_image_band_box_z) (0))) (F_max0_ge_0 (s V_image_band_box_z))]
     ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 23 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_image_band_box_z) (0))) (F_max0_ge_0 (s V_image_band_box_z))]
     ((4 # 1) + s V_image_band_box_z <= z)%Q
   | 24 => ((4 # 1) + max0(s V_image_band_box_z) <= z)%Q
   | 25 => ((4 # 1) + max0(s V_image_band_box_z) <= z)%Q
   | 26 => ((4 # 1) + max0(s V_image_band_box_z) <= z)%Q
   | 27 => ((4 # 1) + max0(s V_image_band_box_z) <= z)%Q
   | 28 => ((4 # 1) + max0(s V_image_band_box_z) <= z)%Q
   | 29 => ((4 # 1) + max0(s V_image_band_box_z) <= z)%Q
   | 30 => (max0(4 - s V_image_band_box_i) + max0(s V_image_band_box_z) <= z)%Q
   | 31 => (max0(4 - s V_image_band_box_i) + max0(s V_image_band_box_z) <= z)%Q
   | 32 => (max0(4 - s V_image_band_box_i) + max0(s V_image_band_box_z) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_image_band_box_i) (3
                                                                    - s V_image_band_box_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_image_band_box_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_image_band_box_z)) (F_check_ge (s V_image_band_box_z) (0))]
     (max0(4 - s V_image_band_box_i) + max0(s V_image_band_box_z) <= z)%Q
   | 34 => (s V_image_band_box_z <= z)%Q
   | 35 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_image_band_box_z)) (F_check_ge (s V_image_band_box_z) (0))]
     (max0(4 - s V_image_band_box_i) + max0(s V_image_band_box_z) <= z)%Q
   | 36 => (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 37 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_image_band_box_i) (1)]
     (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 38 => (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 39 => (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 40 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_image_band_box_i) (1)]
     (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 41 => (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 42 => (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 43 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_image_band_box_i) (1)]
     (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 44 => (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 45 => (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 46 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_image_band_box_i) (1)]
     (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 47 => (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 48 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_image_band_box_i) (1)]
     (s V_image_band_box_z + max0(4 - s V_image_band_box_i) <= z)%Q
   | 49 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 50 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 51 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 52 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 53 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 54 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 55 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 56 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 57 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 58 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 59 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 60 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 61 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 62 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 63 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_image_band_box_z) (0))) (F_max0_ge_0 (s V_image_band_box_z))]
     ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 64 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 65 => ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 66 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_image_band_box_z) (0))) (F_max0_ge_0 (s V_image_band_box_z))]
     ((1 # 1) + s V_image_band_box_z + max0(3 - s V_image_band_box_i) <= z)%Q
   | 67 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 68 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 69 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 70 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 71 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 72 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 73 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 74 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 75 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 76 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 77 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 78 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 79 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 80 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 81 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 82 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 83 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 84 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 85 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 86 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 87 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 88 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 89 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 90 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 91 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 92 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 93 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 94 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 95 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 96 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 97 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 98 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 99 => ((1 # 1) + max0(3 - s V_image_band_box_i)
            + max0(s V_image_band_box_z) <= z)%Q
   | 100 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 101 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 102 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 103 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 104 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 105 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 106 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 107 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 108 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 109 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 110 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 111 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 112 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 113 => ((1 # 1) + max0(3 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 114 => ((1 # 1) + max0(4 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 115 => ((1 # 1) + max0(4 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 116 => ((1 # 1) + max0(4 - s V_image_band_box_i)
             + max0(s V_image_band_box_z) <= z)%Q
   | 117 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_image_band_box_z) (0))) (F_max0_ge_0 (s V_image_band_box_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_image_band_box_z)) (F_check_ge (-1
                                                                    + s V_image_band_box_z) (0))]
     ((1 # 1) + max0(-1 + s V_image_band_box_z)
      + max0(4 - s V_image_band_box_i) <= z)%Q
   | 118 => hints
     [(*-4 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_image_band_box_z)) (F_check_ge (0) (0))]
     ((4 # 1) + max0(s V_image_band_box_z) <= z)%Q
   | 119 => hints
     [(*-4 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_image_band_box_z)) (F_check_ge (0) (0))]
     ((4 # 1) + max0(s V_image_band_box_z) <= z)%Q
   | 120 => (0 <= z)%Q
   | 121 => (0 <= z)%Q
   | 122 => (0 <= z)%Q
   | 123 => (0 <= z)%Q
   | 124 => (0 <= z)%Q
   | 125 => (0 <= z)%Q
   | 126 => (0 <= z)%Q
   | 127 => (0 <= z)%Q
   | 128 => (0 <= z)%Q
   | 129 => (0 <= z)%Q
   | 130 => (0 <= z)%Q
   | 131 => (0 <= z)%Q
   | 132 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_image_band_box_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_image_band_box_z) (0))) (F_max0_ge_0 (-
                                                                    s V_image_band_box_z))]
     (0 <= z)%Q
   | 133 => (0 <= z)%Q
   | 134 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_image_band_box_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_image_band_box_z) (0))) (F_max0_ge_0 (-
                                                                    s V_image_band_box_z))]
     (0 <= z)%Q
   | 135 => (s V_image_band_box_z <= z)%Q
   | 136 => (s V_image_band_box_z <= z)%Q
   | 137 => (s V_image_band_box_z <= z)%Q
   | 138 => (s V_image_band_box_z <= z)%Q
   | 139 => (s V_image_band_box_z <= z)%Q
   | 140 => (s V_image_band_box_z <= z)%Q
   | 141 => (s V_image_band_box_z <= z)%Q
   | 142 => (s V_image_band_box_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_image_band_box =>
    [mkPA Q (fun n z s => ai_image_band_box n s /\ annot0_image_band_box n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_image_band_box (proc_start P_image_band_box) s1 (proc_end P_image_band_box) s2 ->
    (s2 V_image_band_box_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_image_band_box.
Qed.
