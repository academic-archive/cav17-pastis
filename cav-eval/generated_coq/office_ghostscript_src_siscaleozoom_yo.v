Require Import pasta.Pasta.

Inductive proc: Type :=
  P_zoom_y.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_zoom_y_z := 1%positive.
Notation V_zoom_y__tmp := 2%positive.
Notation V_zoom_y__tmp1 := 3%positive.
Notation V_zoom_y__tmp2 := 4%positive.
Notation V_zoom_y__tmp3 := 5%positive.
Notation V_zoom_y__tmp4 := 6%positive.
Notation V_zoom_y_cn := 7%positive.
Notation V_zoom_y_contrib_dref_off4 := 8%positive.
Notation V_zoom_y_contrib_dref_off8 := 9%positive.
Notation V_zoom_y_first_pixel := 10%positive.
Notation V_zoom_y_j := 11%positive.
Notation V_zoom_y_j3 := 12%positive.
Notation V_zoom_y_kc := 13%positive.
Notation V_zoom_y_kn := 14%positive.
Notation V_zoom_y_max_weight := 15%positive.
Notation V_zoom_y_pixel := 16%positive.
Notation V_zoom_y_pixel5 := 17%positive.
Notation V_zoom_y_Colors := 18%positive.
Notation V_zoom_y_MaxValueOut := 19%positive.
Notation V_zoom_y_WidthOut := 20%positive.
Notation V_zoom_y_contrib := 21%positive.
Notation V_zoom_y_dst := 22%positive.
Notation V_zoom_y_items := 23%positive.
Notation V_zoom_y_sizeofPixelOut := 24%positive.
Notation V_zoom_y_tmp := 25%positive.
Notation V_zoom_y_tmp_width := 26%positive.
Definition Pedges_zoom_y: list (edge proc) :=
  (EA 1 (AAssign V_zoom_y_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_zoom_y__tmp (Some (EVar V_zoom_y_sizeofPixelOut))) 3)::(EA 3 (AAssign
  V_zoom_y__tmp1 (Some (EVar V_zoom_y_MaxValueOut))) 4)::(EA 4 (AAssign
  V_zoom_y__tmp3 (Some (EVar V_zoom_y_WidthOut))) 5)::(EA 5 (AAssign
  V_zoom_y__tmp4 (Some (EVar V_zoom_y_tmp_width))) 6)::(EA 6 (AAssign
  V_zoom_y__tmp2 (Some (EVar V_zoom_y_Colors))) 7)::(EA 7 (AAssign
  V_zoom_y_kn (Some (EMul (EVar V_zoom_y__tmp3) (EVar V_zoom_y__tmp2)))) 8)::
  (EA 8 (AAssign V_zoom_y_cn (Some (EVar V_zoom_y_contrib_dref_off4))) 9)::
  (EA 9 (AAssign V_zoom_y_first_pixel
  (Some (EVar V_zoom_y_contrib_dref_off8))) 10)::(EA 10 (AAssign
  V_zoom_y_max_weight (Some (EVar V_zoom_y__tmp1))) 11)::(EA 11 AWeaken 12)::
  (EA 12 (AGuard (fun s => ((eval (EVar V_zoom_y__tmp) s) = (eval (ENum (1))
  s))%Z)) 52)::(EA 12 (AGuard (fun s => ((eval (EVar V_zoom_y__tmp) s) <>
  (eval (ENum (1)) s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 (AAssign
  V_zoom_y_kc (Some (ENum (0)))) 15)::(EA 15 ANone 16)::(EA 16 AWeaken 17)::
  (EA 17 (AGuard (fun s => ((eval (EVar V_zoom_y_kc) s) <
  (eval (EVar V_zoom_y_kn) s))%Z)) 21)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_zoom_y_kc) s) >= (eval (EVar V_zoom_y_kn)
  s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 ANone 20)::(EA 20 AWeaken 60)::
  (EA 21 AWeaken 22)::(EA 22 (AAssign V_zoom_y_j3
  (Some (EVar V_zoom_y_cn))) 23)::(EA 23 ANone 24)::(EA 24 AWeaken 25)::
  (EA 25 (AGuard (fun s => ((eval (EVar V_zoom_y_j3) s) > (eval (ENum (0))
  s))%Z)) 45)::(EA 25 (AGuard (fun s => ((eval (EVar V_zoom_y_j3) s) <=
  (eval (ENum (0)) s))%Z)) 26)::(EA 26 AWeaken 27)::(EA 27 (AAssign
  V_zoom_y_pixel5 None) 28)::(EA 28 AWeaken 29)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_zoom_y_pixel5) s) < (eval (ENum (0)) s))%Z)) 37)::
  (EA 29 (AGuard (fun s => ((eval (EVar V_zoom_y_pixel5) s) >=
  (eval (ENum (0)) s))%Z)) 30)::(EA 30 AWeaken 31)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_zoom_y_pixel5) s) >
  (eval (EVar V_zoom_y_max_weight) s))%Z)) 34)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_zoom_y_pixel5) s) <=
  (eval (EVar V_zoom_y_max_weight) s))%Z)) 32)::(EA 32 AWeaken 33)::
  (EA 33 ANone 36)::(EA 34 AWeaken 35)::(EA 35 ANone 36)::(EA 36 ANone 39)::
  (EA 37 AWeaken 38)::(EA 38 ANone 39)::(EA 39 ANone 40)::(EA 40 (AAssign
  V_zoom_y_kc (Some (EAdd (EVar V_zoom_y_kc) (ENum (1))))) 41)::
  (EA 41 ANone 42)::(EA 42 ANone 43)::(EA 43 (AAssign V_zoom_y_z
  (Some (EAdd (ENum (1)) (EVar V_zoom_y_z)))) 44)::(EA 44 AWeaken 17)::
  (EA 45 AWeaken 46)::(EA 46 ANone 47)::(EA 47 (AAssign V_zoom_y_j3
  (Some (EAdd (EVar V_zoom_y_j3) (ENum (-1))))) 48)::(EA 48 ANone 49)::
  (EA 49 ANone 50)::(EA 50 (AAssign V_zoom_y_z (Some (EAdd (ENum (1))
  (EVar V_zoom_y_z)))) 51)::(EA 51 AWeaken 25)::(EA 52 AWeaken 53)::
  (EA 53 (AAssign V_zoom_y_kc (Some (ENum (0)))) 54)::(EA 54 ANone 55)::
  (EA 55 AWeaken 56)::(EA 56 (AGuard (fun s => ((eval (EVar V_zoom_y_kc) s) <
  (eval (EVar V_zoom_y_kn) s))%Z)) 61)::(EA 56 (AGuard
  (fun s => ((eval (EVar V_zoom_y_kc) s) >= (eval (EVar V_zoom_y_kn)
  s))%Z)) 57)::(EA 57 AWeaken 58)::(EA 58 ANone 59)::(EA 59 AWeaken 60)::
  (EA 61 AWeaken 62)::(EA 62 (AAssign V_zoom_y_j
  (Some (EVar V_zoom_y_cn))) 63)::(EA 63 ANone 64)::(EA 64 AWeaken 65)::
  (EA 65 (AGuard (fun s => ((eval (EVar V_zoom_y_j) s) > (eval (ENum (0))
  s))%Z)) 85)::(EA 65 (AGuard (fun s => ((eval (EVar V_zoom_y_j) s) <=
  (eval (ENum (0)) s))%Z)) 66)::(EA 66 AWeaken 67)::(EA 67 (AAssign
  V_zoom_y_pixel None) 68)::(EA 68 AWeaken 69)::(EA 69 (AGuard
  (fun s => ((eval (EVar V_zoom_y_pixel) s) < (eval (ENum (0)) s))%Z)) 77)::
  (EA 69 (AGuard (fun s => ((eval (EVar V_zoom_y_pixel) s) >=
  (eval (ENum (0)) s))%Z)) 70)::(EA 70 AWeaken 71)::(EA 71 (AGuard
  (fun s => ((eval (EVar V_zoom_y_pixel) s) >
  (eval (EVar V_zoom_y_max_weight) s))%Z)) 74)::(EA 71 (AGuard
  (fun s => ((eval (EVar V_zoom_y_pixel) s) <=
  (eval (EVar V_zoom_y_max_weight) s))%Z)) 72)::(EA 72 AWeaken 73)::
  (EA 73 ANone 76)::(EA 74 AWeaken 75)::(EA 75 ANone 76)::(EA 76 ANone 79)::
  (EA 77 AWeaken 78)::(EA 78 ANone 79)::(EA 79 ANone 80)::(EA 80 (AAssign
  V_zoom_y_kc (Some (EAdd (EVar V_zoom_y_kc) (ENum (1))))) 81)::
  (EA 81 ANone 82)::(EA 82 ANone 83)::(EA 83 (AAssign V_zoom_y_z
  (Some (EAdd (ENum (1)) (EVar V_zoom_y_z)))) 84)::(EA 84 AWeaken 56)::
  (EA 85 AWeaken 86)::(EA 86 ANone 87)::(EA 87 (AAssign V_zoom_y_j
  (Some (EAdd (EVar V_zoom_y_j) (ENum (-1))))) 88)::(EA 88 ANone 89)::
  (EA 89 ANone 90)::(EA 90 (AAssign V_zoom_y_z (Some (EAdd (ENum (1))
  (EVar V_zoom_y_z)))) 91)::(EA 91 AWeaken 65)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_zoom_y => Pedges_zoom_y
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_zoom_y => 60
     end)%positive;
  var_global := var_global
}.

Definition ai_zoom_y (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 3 => (-1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_z <= 0)%Z
   | 4 => (1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 5 => (-1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_z <= 0)%Z
   | 6 => (1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 7 => (-1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_z <= 0)%Z
   | 8 => (1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 9 => (-1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_z <= 0)%Z
   | 10 => (1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 11 => (-1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_z <= 0)%Z
   | 12 => (1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 13 => (-1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_z <= 0)%Z
   | 14 => (1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 15 => (-1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_kc <= 0)%Z
   | 16 => (-1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 17 => (-1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0)%Z
   | 18 => (-1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc+ 1 * s V_zoom_y_kn <= 0)%Z
   | 19 => (-1 * s V_zoom_y_kc+ 1 * s V_zoom_y_kn <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0)%Z
   | 20 => (-1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc+ 1 * s V_zoom_y_kn <= 0)%Z
   | 21 => (-1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 22 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0)%Z
   | 23 => (-1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 24 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0)%Z
   | 25 => (-1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 26 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j3 <= 0)%Z
   | 27 => (1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 28 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j3 <= 0)%Z
   | 29 => (1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 30 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_pixel5 <= 0)%Z
   | 31 => (-1 * s V_zoom_y_pixel5 <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 32 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_pixel5 <= 0 /\ -1 * s V_zoom_y_max_weight+ 1 * s V_zoom_y_pixel5 <= 0)%Z
   | 33 => (-1 * s V_zoom_y_max_weight+ 1 * s V_zoom_y_pixel5 <= 0 /\ -1 * s V_zoom_y_pixel5 <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 34 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_pixel5 <= 0 /\ 1 * s V_zoom_y_max_weight+ -1 * s V_zoom_y_pixel5 + 1 <= 0)%Z
   | 35 => (1 * s V_zoom_y_max_weight+ -1 * s V_zoom_y_pixel5 + 1 <= 0 /\ -1 * s V_zoom_y_pixel5 <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 36 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_pixel5 <= 0)%Z
   | 37 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ 1 * s V_zoom_y_pixel5 + 1 <= 0)%Z
   | 38 => (1 * s V_zoom_y_pixel5 + 1 <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 39 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j3 <= 0)%Z
   | 40 => (1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 41 => (-1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_kc + 1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn <= 0)%Z
   | 42 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn <= 0 /\ -1 * s V_zoom_y_kc + 1 <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 43 => (-1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_kc + 1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn <= 0)%Z
   | 44 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn <= 0 /\ -1 * s V_zoom_y_kc + 1 <= 0 /\ 1 * s V_zoom_y_j3 <= 0 /\ -1 * s V_zoom_y_z + 1 <= 0)%Z
   | 45 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_j3 + 1 <= 0)%Z
   | 46 => (-1 * s V_zoom_y_j3 + 1 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 47 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_j3 + 1 <= 0)%Z
   | 48 => (-1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_j3 <= 0)%Z
   | 49 => (-1 * s V_zoom_y_j3 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 50 => (-1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_j3 <= 0)%Z
   | 51 => (-1 * s V_zoom_y_j3 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z + 1 <= 0)%Z
   | 52 => (-1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0)%Z
   | 53 => (-1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 54 => (-1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_kc <= 0)%Z
   | 55 => (-1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 56 => (-1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0)%Z
   | 57 => (-1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc+ 1 * s V_zoom_y_kn <= 0)%Z
   | 58 => (-1 * s V_zoom_y_kc+ 1 * s V_zoom_y_kn <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0)%Z
   | 59 => (-1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc+ 1 * s V_zoom_y_kn <= 0)%Z
   | 60 => (-1 * s V_zoom_y_kc+ 1 * s V_zoom_y_kn <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0)%Z
   | 61 => (-1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 62 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0)%Z
   | 63 => (-1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 64 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0)%Z
   | 65 => (-1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 66 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j <= 0)%Z
   | 67 => (1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 68 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j <= 0)%Z
   | 69 => (1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 70 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_pixel <= 0)%Z
   | 71 => (-1 * s V_zoom_y_pixel <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 72 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_pixel <= 0 /\ -1 * s V_zoom_y_max_weight+ 1 * s V_zoom_y_pixel <= 0)%Z
   | 73 => (-1 * s V_zoom_y_max_weight+ 1 * s V_zoom_y_pixel <= 0 /\ -1 * s V_zoom_y_pixel <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 74 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_pixel <= 0 /\ 1 * s V_zoom_y_max_weight+ -1 * s V_zoom_y_pixel + 1 <= 0)%Z
   | 75 => (1 * s V_zoom_y_max_weight+ -1 * s V_zoom_y_pixel + 1 <= 0 /\ -1 * s V_zoom_y_pixel <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 76 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_pixel <= 0)%Z
   | 77 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ 1 * s V_zoom_y_pixel + 1 <= 0)%Z
   | 78 => (1 * s V_zoom_y_pixel + 1 <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 79 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j <= 0)%Z
   | 80 => (1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 81 => (1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_kc + 1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn <= 0)%Z
   | 82 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn <= 0 /\ -1 * s V_zoom_y_kc + 1 <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0)%Z
   | 83 => (1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y_kc + 1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn <= 0)%Z
   | 84 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn <= 0 /\ -1 * s V_zoom_y_kc + 1 <= 0 /\ 1 * s V_zoom_y_j <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y_z + 1 <= 0)%Z
   | 85 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_j + 1 <= 0)%Z
   | 86 => (-1 * s V_zoom_y_j + 1 <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0)%Z
   | 87 => (1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_j + 1 <= 0)%Z
   | 88 => (-1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_j <= 0)%Z
   | 89 => (-1 * s V_zoom_y_j <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z <= 0)%Z
   | 90 => (-1 * s V_zoom_y_z <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ -1 * s V_zoom_y_j <= 0)%Z
   | 91 => (-1 * s V_zoom_y_j <= 0 /\ 1 * s V_zoom_y_kc+ -1 * s V_zoom_y_kn + 1 <= 0 /\ 1 * s V_zoom_y__tmp + -1 <= 0 /\ -1 * s V_zoom_y__tmp + 1 <= 0 /\ -1 * s V_zoom_y_kc <= 0 /\ -1 * s V_zoom_y_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_zoom_y (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_zoom_y_Colors * s V_zoom_y_WidthOut)
           + max0(s V_zoom_y_Colors * s V_zoom_y_WidthOut) * max0(s V_zoom_y_contrib_dref_off4) <= z)%Q
   | 2 => (max0(s V_zoom_y_Colors * s V_zoom_y_WidthOut)
           + max0(s V_zoom_y_Colors * s V_zoom_y_WidthOut) * max0(s V_zoom_y_contrib_dref_off4) <= z)%Q
   | 3 => (max0(s V_zoom_y_Colors * s V_zoom_y_WidthOut)
           + max0(s V_zoom_y_Colors * s V_zoom_y_WidthOut) * max0(s V_zoom_y_contrib_dref_off4) <= z)%Q
   | 4 => (max0(s V_zoom_y_Colors * s V_zoom_y_WidthOut)
           + max0(s V_zoom_y_Colors * s V_zoom_y_WidthOut) * max0(s V_zoom_y_contrib_dref_off4) <= z)%Q
   | 5 => (max0(s V_zoom_y_Colors * s V_zoom_y__tmp3)
           + max0(s V_zoom_y_Colors * s V_zoom_y__tmp3) * max0(s V_zoom_y_contrib_dref_off4) <= z)%Q
   | 6 => (max0(s V_zoom_y_Colors * s V_zoom_y__tmp3)
           + max0(s V_zoom_y_Colors * s V_zoom_y__tmp3) * max0(s V_zoom_y_contrib_dref_off4) <= z)%Q
   | 7 => (max0(s V_zoom_y__tmp2 * s V_zoom_y__tmp3)
           + max0(s V_zoom_y__tmp2 * s V_zoom_y__tmp3) * max0(s V_zoom_y_contrib_dref_off4) <= z)%Q
   | 8 => (max0(s V_zoom_y_contrib_dref_off4) * max0(s V_zoom_y_kn)
           + max0(s V_zoom_y_kn) <= z)%Q
   | 9 => (max0(s V_zoom_y_cn) * max0(s V_zoom_y_kn) + max0(s V_zoom_y_kn) <= z)%Q
   | 10 => (max0(s V_zoom_y_cn) * max0(s V_zoom_y_kn) + max0(s V_zoom_y_kn) <= z)%Q
   | 11 => (max0(s V_zoom_y_cn) * max0(s V_zoom_y_kn) + max0(s V_zoom_y_kn) <= z)%Q
   | 12 => (max0(s V_zoom_y_cn) * max0(s V_zoom_y_kn) + max0(s V_zoom_y_kn) <= z)%Q
   | 13 => (max0(s V_zoom_y_cn) * max0(s V_zoom_y_kn) + max0(s V_zoom_y_kn) <= z)%Q
   | 14 => (max0(s V_zoom_y_cn) * max0(s V_zoom_y_kn) + max0(s V_zoom_y_kn) <= z)%Q
   | 15 => (-(3 # 31) * s V_zoom_y_kc
            + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (4 # 31) * s V_zoom_y_kc^2
            - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            + (3 # 31) * max0(s V_zoom_y_kc)
            - (1 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc
                                                    - s V_zoom_y_kn)
            - (3 # 31) * max0(s V_zoom_y_kc)^2
            + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_binom_monotonic 2 (F_max0_ge_0 (s V_zoom_y_z)) (F_check_ge (0) (0));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_z)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_zoom_y_z)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_z)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_z) (0))) (F_max0_ge_0 (s V_zoom_y_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_z)) (F_check_ge (0) (0)))]
     (-(3 # 31) * s V_zoom_y_kc
      + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      + (4 # 31) * s V_zoom_y_kc^2
      - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      + (3 # 31) * max0(s V_zoom_y_kc)
      - (1 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (3 # 31) * max0(s V_zoom_y_kc)^2
      + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 17 => (-(3 # 31) * s V_zoom_y_kc
            + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (4 # 31) * s V_zoom_y_kc^2
            - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            + (3 # 31) * max0(s V_zoom_y_kc)
            - (1 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc
                                                    - s V_zoom_y_kn)
            - (3 # 31) * max0(s V_zoom_y_kc)^2
            + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + max0(s V_zoom_y_z) <= z)%Q
   | 18 => hints
     [(*0 0.0967742*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))]
     (-(3 # 31) * s V_zoom_y_kc
      + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      + (4 # 31) * s V_zoom_y_kc^2
      - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      + (3 # 31) * max0(s V_zoom_y_kc)
      - (1 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (3 # 31) * max0(s V_zoom_y_kc)^2
      + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
      + max0(s V_zoom_y_z) <= z)%Q
   | 19 => ((1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc^2
            - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (1 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc
                                                    - s V_zoom_y_kn)
            + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + max0(s V_zoom_y_z) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_zoom_y_kc + s V_zoom_y_kn) (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn));
      (*-1 0*) F_max0_ge_0 (-1 - s V_zoom_y_kc + s V_zoom_y_kn);
      (*-0.0322581 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_z)) (F_check_ge (s V_zoom_y_z) (0));
      (*-0.0322581 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))]
     ((1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      + (1 # 31) * s V_zoom_y_kc^2
      - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (1 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
      + max0(s V_zoom_y_z) <= z)%Q
   | 21 => hints
     [(*0 0.322581*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn));
      (*0 0.0322581*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn));
      (*0 0.0322581*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*0 0.0967742*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - 
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*0 0.290323*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*0 0.677419*) F_binom_monotonic 1 (F_max0_ge_arg (-s V_zoom_y_kc
                                                         + s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))]
     (-(3 # 31) * s V_zoom_y_kc
      + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      + (4 # 31) * s V_zoom_y_kc^2
      - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      + (3 # 31) * max0(s V_zoom_y_kc)
      - (1 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (3 # 31) * max0(s V_zoom_y_kc)^2
      + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
      + max0(s V_zoom_y_z) <= z)%Q
   | 22 => (-(34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            - (11 # 31) * s V_zoom_y_kn^2
            - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)
            - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            - (1 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc
                                                    - s V_zoom_y_kn)
            - (3 # 31) * max0(s V_zoom_y_kc)^2
            + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + (1 # 31) * max0(s V_zoom_y_kn)^2 + max0(s V_zoom_y_z) <= z)%Q
   | 23 => (-(34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            - (11 # 31) * s V_zoom_y_kn^2
            - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)
            - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            - (1 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc
                                                    - s V_zoom_y_kn)
            - (3 # 31) * max0(s V_zoom_y_kc)^2
            + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + (1 # 31) * max0(s V_zoom_y_kn)^2 + max0(s V_zoom_y_z) <= z)%Q
   | 24 => hints
     [(*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_z)) (F_check_ge (s V_zoom_y_z) (0))]
     (-(34 # 31) * s V_zoom_y_kc + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
      - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
      + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + s V_zoom_y_kn * max0(s V_zoom_y_j3)
      - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      - (11 # 31) * s V_zoom_y_kn^2
      - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-s V_zoom_y_kc
                                                                   + 
                                                                   s V_zoom_y_kn)
      - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + (1 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kn)
      + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
      - (1 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (3 # 31) * max0(s V_zoom_y_kc)^2
      + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
      + (1 # 31) * max0(s V_zoom_y_kn)^2 + max0(s V_zoom_y_z) <= z)%Q
   | 25 => (-(34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)
            - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            - (3 # 31) * max0(s V_zoom_y_kc)^2
            + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 26 => hints
     [(*0 0.0322581*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - 
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)))]
     (-(34 # 31) * s V_zoom_y_kc + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
      - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
      + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + s V_zoom_y_kn * max0(s V_zoom_y_j3)
      - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
      - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-s V_zoom_y_kc
                                                                   + 
                                                                   s V_zoom_y_kn)
      - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + (1 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kn)
      + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
      - (3 # 31) * max0(s V_zoom_y_kc)^2
      + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 27 => (-(34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)
            - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            - (3 # 31) * max0(s V_zoom_y_kc)^2
            + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            - (1 # 31) * max0(s V_zoom_y_kn) <= z)%Q
   | 28 => hints
     [(*-0.322581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*0 0.0967742*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*0 0.0967742*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*0 0.0645161*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*0 0.0322581*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0322581 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn))]
     (-(34 # 31) * s V_zoom_y_kc + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
      - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
      + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + s V_zoom_y_kn * max0(s V_zoom_y_j3)
      - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
      - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-s V_zoom_y_kc
                                                                   + 
                                                                   s V_zoom_y_kn)
      - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
      - (3 # 31) * max0(s V_zoom_y_kc)^2
      + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
      - (1 # 31) * max0(s V_zoom_y_kn) <= z)%Q
   | 29 => (-(34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (12 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            - (9 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            + (9 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (2 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kn) <= z)%Q
   | 30 => hints
     [(*-0.290323 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)))]
     (-(34 # 31) * s V_zoom_y_kc + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
      - (12 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (9 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc^2 + s V_zoom_y_kn
      + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + s V_zoom_y_kn * max0(s V_zoom_y_j3)
      + (9 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
      - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-s V_zoom_y_kc
                                                                   + 
                                                                   s V_zoom_y_kn)
      + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (2 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kn) <= z)%Q
   | 31 => (-(34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (12 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (9 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (2 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kn) <= z)%Q
   | 32 => hints
     [(*-0.290323 0*) F_max0_pre_decrement 1 (-s V_zoom_y_kc + s V_zoom_y_kn) (1);
      (*-1 0*) F_max0_monotonic (F_check_ge (s V_zoom_y_j3) (-1
                                                             + s V_zoom_y_j3));
      (*-1 0*) F_max0_ge_0 (-1 + s V_zoom_y_j3);
      (*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_zoom_y_kc
                                                              + s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)))]
     (-(34 # 31) * s V_zoom_y_kc + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
      - (12 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc^2 + s V_zoom_y_kn
      + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + s V_zoom_y_kn * max0(s V_zoom_y_j3)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
      + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (9 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (2 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kn) <= z)%Q
   | 33 => ((9 # 31) - (34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (12 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            - s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (6 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            - max0(s V_zoom_y_j3)
            - (2 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kn) <= z)%Q
   | 34 => hints
     [(*-0.290323 0*) F_max0_pre_decrement 1 (-s V_zoom_y_kc + s V_zoom_y_kn) (1);
      (*-1 0*) F_max0_monotonic (F_check_ge (s V_zoom_y_j3) (-1
                                                             + s V_zoom_y_j3));
      (*-1 0*) F_max0_ge_0 (-1 + s V_zoom_y_j3);
      (*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_zoom_y_kc
                                                              + s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)))]
     (-(34 # 31) * s V_zoom_y_kc + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
      - (12 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc^2 + s V_zoom_y_kn
      + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + s V_zoom_y_kn * max0(s V_zoom_y_j3)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
      + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (9 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (2 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kn) <= z)%Q
   | 35 => ((9 # 31) - (34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (12 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            - s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (6 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            - max0(s V_zoom_y_j3)
            - (2 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kn) <= z)%Q
   | 36 => ((9 # 31) - (34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (12 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            - s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (6 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            - max0(s V_zoom_y_j3)
            - (2 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kn) <= z)%Q
   | 37 => hints
     [(*-0.290323 0*) F_max0_pre_decrement 1 (-s V_zoom_y_kc + s V_zoom_y_kn) (1);
      (*-1 0*) F_max0_monotonic (F_check_ge (s V_zoom_y_j3) (-1
                                                             + s V_zoom_y_j3));
      (*-1 0*) F_max0_ge_0 (-1 + s V_zoom_y_j3);
      (*-0.290323 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_zoom_y_kc
                                                              + s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)))]
     (-(34 # 31) * s V_zoom_y_kc + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
      - (12 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (9 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc^2 + s V_zoom_y_kn
      + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + s V_zoom_y_kn * max0(s V_zoom_y_j3)
      + (9 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
      - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-s V_zoom_y_kc
                                                                   + 
                                                                   s V_zoom_y_kn)
      + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (2 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kn) <= z)%Q
   | 38 => ((9 # 31) - (34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (12 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            - s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (6 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            - max0(s V_zoom_y_j3)
            - (2 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kn) <= z)%Q
   | 39 => ((9 # 31) - (34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (12 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            - s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (6 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            - max0(s V_zoom_y_j3)
            - (2 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kn) <= z)%Q
   | 40 => ((9 # 31) - (34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (12 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            - s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (6 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            - max0(s V_zoom_y_j3)
            - (2 # 31) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kn) <= z)%Q
   | 41 => ((37 # 31) - (22 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (6 # 31) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kc)
            - s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (12 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + (11 # 31) * s V_zoom_y_kn
            + (2 # 31) * s V_zoom_y_kn * max0(-1 + s V_zoom_y_kc)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            + (9 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (6 # 31) * max0(-1 + s V_zoom_y_kc)
            - (2 # 31) * max0(-1 + s V_zoom_y_kc) * max0(s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            + max0(s V_zoom_y_cn) - max0(s V_zoom_y_j3)
            + (21 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(s V_zoom_y_kn) <= z)%Q
   | 42 => ((37 # 31) - (22 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (6 # 31) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kc)
            - s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (12 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + (11 # 31) * s V_zoom_y_kn
            + (2 # 31) * s V_zoom_y_kn * max0(-1 + s V_zoom_y_kc)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            + (9 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (6 # 31) * max0(-1 + s V_zoom_y_kc)
            - (2 # 31) * max0(-1 + s V_zoom_y_kc) * max0(s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            + max0(s V_zoom_y_cn) - max0(s V_zoom_y_j3)
            + (21 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(s V_zoom_y_kn) <= z)%Q
   | 43 => ((37 # 31) - (22 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (6 # 31) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kc)
            - s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (12 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + (11 # 31) * s V_zoom_y_kn
            + (2 # 31) * s V_zoom_y_kn * max0(-1 + s V_zoom_y_kc)
            + s V_zoom_y_kn * max0(s V_zoom_y_j3)
            + (9 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (6 # 31) * max0(-1 + s V_zoom_y_kc)
            - (2 # 31) * max0(-1 + s V_zoom_y_kc) * max0(s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
            + max0(s V_zoom_y_cn) - max0(s V_zoom_y_j3)
            + (21 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(s V_zoom_y_kn) <= z)%Q
   | 44 => hints
     [(*-0.193548 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kc) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kc));
      (*-0.322581 0*) F_binom_monotonic 2 (F_max0_ge_arg (-s V_zoom_y_kc
                                                          + s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0));
      (*0 0.193548*) F_binom_monotonic 2 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0));
      (*-0.0322581 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0));
      (*0 0.193548*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_zoom_y_kc)) (F_check_ge (-1
                                                                    + s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j3)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                              + s V_zoom_y_kn)) (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)));
      (*-0.322581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0967742 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0967742 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0645161 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0967742 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0645161 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0322581 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0645161 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_z) (0))) (F_max0_ge_0 (s V_zoom_y_z));
      (*-0.0967742 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0));
      (*0 0.193548*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + 
                                                                    s V_zoom_y_kc) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kc))]
     ((6 # 31) - (22 # 31) * s V_zoom_y_kc
      + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
      - (6 # 31) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kc)
      - s V_zoom_y_kc * max0(s V_zoom_y_cn)
      - (12 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc^2 + (11 # 31) * s V_zoom_y_kn
      + (2 # 31) * s V_zoom_y_kn * max0(-1 + s V_zoom_y_kc)
      + s V_zoom_y_kn * max0(s V_zoom_y_j3)
      + (9 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
      + (6 # 31) * max0(-1 + s V_zoom_y_kc)
      - (2 # 31) * max0(-1 + s V_zoom_y_kc) * max0(s V_zoom_y_kn)
      + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3) + max0(s V_zoom_y_cn)
      - max0(s V_zoom_y_j3)
      + (21 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(s V_zoom_y_kn) <= z)%Q
   | 45 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_j3) (0))) (F_max0_ge_0 (s V_zoom_y_j3))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_j3)) (F_check_ge (s V_zoom_y_j3) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_j3)) (F_check_ge (s V_zoom_y_j3) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j3)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j3)) (F_check_ge (0) (0)))]
     (-(34 # 31) * s V_zoom_y_kc + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
      - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
      + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + s V_zoom_y_kn * max0(s V_zoom_y_j3)
      - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
      - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-s V_zoom_y_kc
                                                                   + 
                                                                   s V_zoom_y_kn)
      - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + (1 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kn)
      + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j3)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
      - (3 # 31) * max0(s V_zoom_y_kc)^2
      + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 46 => (-s V_zoom_y_j3 * max0(-1 + s V_zoom_y_kn)
            + s V_zoom_y_j3 * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + s V_zoom_y_j3 * max0(s V_zoom_y_kc) - (34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - s V_zoom_y_kn * max0(s V_zoom_y_cn)
            - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)
            - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            - (3 # 31) * max0(s V_zoom_y_kc)^2
            + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 47 => (-s V_zoom_y_j3 * max0(-1 + s V_zoom_y_kn)
            + s V_zoom_y_j3 * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + s V_zoom_y_j3 * max0(s V_zoom_y_kc) - (34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - s V_zoom_y_kn * max0(s V_zoom_y_cn)
            - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)
            - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            - (3 # 31) * max0(s V_zoom_y_kc)^2
            + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 48 => (-s V_zoom_y_j3 * max0(-1 + s V_zoom_y_kn)
            + s V_zoom_y_j3 * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + s V_zoom_y_j3 * max0(s V_zoom_y_kc) - (34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - s V_zoom_y_kn * max0(s V_zoom_y_cn)
            - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)
            - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            - max0(-1 + s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            + max0(s V_zoom_y_kc) - (3 # 31) * max0(s V_zoom_y_kc)^2
            + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 49 => (-s V_zoom_y_j3 * max0(-1 + s V_zoom_y_kn)
            + s V_zoom_y_j3 * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + s V_zoom_y_j3 * max0(s V_zoom_y_kc) - (34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - s V_zoom_y_kn * max0(s V_zoom_y_cn)
            - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)
            - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            - max0(-1 + s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            + max0(s V_zoom_y_kc) - (3 # 31) * max0(s V_zoom_y_kc)^2
            + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 50 => (-s V_zoom_y_j3 * max0(-1 + s V_zoom_y_kn)
            + s V_zoom_y_j3 * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + s V_zoom_y_j3 * max0(s V_zoom_y_kc) - (34 # 31) * s V_zoom_y_kc
            + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
            - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
            + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - s V_zoom_y_kn * max0(s V_zoom_y_cn)
            - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
            - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)
            - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            - max0(-1 + s V_zoom_y_kn)
            + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            + max0(s V_zoom_y_kc) - (3 # 31) * max0(s V_zoom_y_kc)^2
            + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 51 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_zoom_y_kc + s V_zoom_y_kn) (1);
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1 + s V_zoom_y_kn)) (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-s V_zoom_y_kc
                                                     + s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                - s V_zoom_y_kc
                                                                + s V_zoom_y_kn)) (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                - s V_zoom_y_kc
                                                                + s V_zoom_y_kn)) (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_j3) (0))) (F_max0_ge_0 (s V_zoom_y_j3))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_j3) (0))) (F_max0_ge_0 (s V_zoom_y_j3))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_j3)) (F_check_ge (s V_zoom_y_j3) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_zoom_y_kc
                                                              + s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j3)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j3)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                 - s V_zoom_y_kc
                                                                 + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))]
     (-(1 # 1) - s V_zoom_y_j3 * max0(-1 + s V_zoom_y_kn)
      + s V_zoom_y_j3 * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + s V_zoom_y_j3 * max0(s V_zoom_y_kc) - (34 # 31) * s V_zoom_y_kc
      + (20 # 31) * s V_zoom_y_kc * s V_zoom_y_kn
      - (9 # 31) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 31) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (1 # 31) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (6 # 31) * s V_zoom_y_kc^2 + (32 # 31) * s V_zoom_y_kn
      + (9 # 31) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - s V_zoom_y_kn * max0(s V_zoom_y_cn)
      - (1 # 31) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (2 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (1 # 31) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      - (11 # 31) * s V_zoom_y_kn^2 + s V_zoom_y_z
      - (9 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-s V_zoom_y_kc
                                                                   + 
                                                                   s V_zoom_y_kn)
      - (3 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + (1 # 31) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kn)
      - max0(-1 + s V_zoom_y_kn)
      + max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      + (10 # 31) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
      + max0(s V_zoom_y_kc) - (3 # 31) * max0(s V_zoom_y_kc)^2
      + (1 # 31) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 52 => (max0(s V_zoom_y_cn) * max0(s V_zoom_y_kn) + max0(s V_zoom_y_kn) <= z)%Q
   | 53 => (max0(s V_zoom_y_cn) * max0(s V_zoom_y_kn) + max0(s V_zoom_y_kn) <= z)%Q
   | 54 => ((1 # 44) * s V_zoom_y_kc
            + (3 # 44) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (1 # 22) * s V_zoom_y_kc^2
            - (3 # 44) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (1 # 44) * max0(s V_zoom_y_kc)
            - (3 # 44) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc
                                                    - s V_zoom_y_kn)
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 55 => hints
     [(*-1 0*) F_binom_monotonic 2 (F_max0_ge_0 (s V_zoom_y_z)) (F_check_ge (0) (0));
      (*0 0.666667*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - s V_zoom_y__tmp) (0))) (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - s V_zoom_y__tmp) (0))) (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.666667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                   - 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                   - 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_z)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_zoom_y_z)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_z)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_z) (0))) (F_max0_ge_0 (s V_zoom_y_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_z)) (F_check_ge (0) (0)))]
     ((1 # 44) * s V_zoom_y_kc
      + (3 # 44) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      + (1 # 22) * s V_zoom_y_kc^2
      - (3 # 44) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (1 # 44) * max0(s V_zoom_y_kc)
      - (3 # 44) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 44) * max0(s V_zoom_y_kc)^2
      + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 56 => ((2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_cn)
            + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                 + s V_zoom_y_kn)
            + (1 # 44) * s V_zoom_y_kc
            + (3 # 44) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (1 # 22) * s V_zoom_y_kc^2
            - (3 # 44) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (2 # 3) * max0(s V_zoom_y_cn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (71 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (1 # 44) * max0(s V_zoom_y_kc)
            - (3 # 44) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc
                                                    - s V_zoom_y_kn)
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + max0(s V_zoom_y_z) <= z)%Q
   | 57 => ((2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_cn)
            + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                 + s V_zoom_y_kn)
            + (1 # 44) * s V_zoom_y_kc
            + (3 # 44) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (1 # 22) * s V_zoom_y_kc^2
            - (3 # 44) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (2 # 3) * max0(s V_zoom_y_cn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (71 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (1 # 44) * max0(s V_zoom_y_kc)
            - (3 # 44) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc
                                                    - s V_zoom_y_kn)
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + max0(s V_zoom_y_z) <= z)%Q
   | 58 => ((2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_cn)
            + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                 + s V_zoom_y_kn)
            + (1 # 44) * s V_zoom_y_kc
            + (3 # 44) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (1 # 22) * s V_zoom_y_kc^2
            - (3 # 44) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            - (2 # 3) * max0(s V_zoom_y_cn)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (71 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (1 # 44) * max0(s V_zoom_y_kc)
            - (3 # 44) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc
                                                    - s V_zoom_y_kn)
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + max0(s V_zoom_y_z) <= z)%Q
   | 59 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_zoom_y_kc + s V_zoom_y_kn) (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn));
      (*-1 0*) F_max0_ge_0 (-1 - s V_zoom_y_kc + s V_zoom_y_kn);
      (*-0.666667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.666667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_z)) (F_check_ge (s V_zoom_y_z) (0));
      (*-0.0681818 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc));
      (*-0.0454545 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))]
     ((2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_cn)
      + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 44) * s V_zoom_y_kc
      + (3 # 44) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      + (1 # 22) * s V_zoom_y_kc^2
      - (3 # 44) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      - (2 # 3) * max0(s V_zoom_y_cn)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (71 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (1 # 44) * max0(s V_zoom_y_kc)
      - (3 # 44) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 44) * max0(s V_zoom_y_kc)^2
      + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
      + max0(s V_zoom_y_z) <= z)%Q
   | 60 => (s V_zoom_y_z <= z)%Q
   | 61 => hints
     [(*0 0.193182*) F_binom_monotonic 2 (F_max0_ge_arg (-s V_zoom_y_kc
                                                         + s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0));
      (*0 0.666667*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)));
      (*0 0.0227273*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*0 0.261364*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - 
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*0 0.261364*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*0 0.261364*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)))]
     ((2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_cn)
      + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 44) * s V_zoom_y_kc
      + (3 # 44) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      + (1 # 22) * s V_zoom_y_kc^2
      - (3 # 44) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      - (2 # 3) * max0(s V_zoom_y_cn)
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (71 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (1 # 44) * max0(s V_zoom_y_kc)
      - (3 # 44) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 44) * max0(s V_zoom_y_kc)^2
      + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
      + max0(s V_zoom_y_z) <= z)%Q
   | 62 => ((2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_cn)
            + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                 + s V_zoom_y_kn)
            + (19 # 88) * s V_zoom_y_kc
            - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (2 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (17 # 88) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
            - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + (17 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + (17 # 88) * s V_zoom_y_kn^2
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            + (2 # 3) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (65 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            - (3 # 44) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc
                                                    - s V_zoom_y_kn)
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + max0(s V_zoom_y_z) <= z)%Q
   | 63 => ((2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                 + s V_zoom_y_kn)
            + (19 # 88) * s V_zoom_y_kc
            - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
            + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_j)
            - (17 # 88) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
            - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + (17 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + (17 # 88) * s V_zoom_y_kn^2
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - (1 # 3) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (1 # 3) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc
                                                   + s V_zoom_y_kn)
            - (1 # 3) * max0(s V_zoom_y_j)
            + (2 # 3) * max0(s V_zoom_y_j) * max0(-s V_zoom_y_kc
                                                  + s V_zoom_y_kn)
            + (65 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            - (3 # 44) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc
                                                    - s V_zoom_y_kn)
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
            + max0(s V_zoom_y_z) <= z)%Q
   | 64 => hints
     [(*-0.738636 0*) F_max0_pre_decrement 1 (-s V_zoom_y_kc + s V_zoom_y_kn) (1);
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_z)) (F_check_ge (s V_zoom_y_z) (0))]
     ((2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
      + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (19 # 88) * s V_zoom_y_kc - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
      + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
      + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_j)
      - (17 # 88) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
      - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + (17 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      + (17 # 88) * s V_zoom_y_kn^2
      + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
      - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
      - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + 
                                                                    s V_zoom_y_kn)
      + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - (1 # 3) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_j)
      + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
      + (1 # 3) * max0(s V_zoom_y_cn)
      + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 3) * max0(s V_zoom_y_j)
      + (2 # 3) * max0(s V_zoom_y_j) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (65 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
      - (3 # 44) * max0(s V_zoom_y_kc) * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 44) * max0(s V_zoom_y_kc)^2
      + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn)
      + max0(s V_zoom_y_z) <= z)%Q
   | 65 => ((65 # 88) + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                 + s V_zoom_y_kn)
            + (19 # 88) * s V_zoom_y_kc
            - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
            + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_j)
            - (17 # 88) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
            - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + (17 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + (17 # 88) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (21 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - (1 # 3) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (1 # 3) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc
                                                   + s V_zoom_y_kn)
            - (1 # 3) * max0(s V_zoom_y_j)
            + (2 # 3) * max0(s V_zoom_y_j) * max0(-s V_zoom_y_kc
                                                  + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 66 => hints
     [(*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j)) (F_check_ge (0) (0)))]
     ((65 # 88) + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
      + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (19 # 88) * s V_zoom_y_kc - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
      + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
      + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_j)
      - (17 # 88) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
      - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + (17 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      + (17 # 88) * s V_zoom_y_kn^2 + s V_zoom_y_z
      + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
      - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
      + (21 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + 
                                                                    s V_zoom_y_kn)
      + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - (1 # 3) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_j)
      + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
      + (1 # 3) * max0(s V_zoom_y_cn)
      + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 3) * max0(s V_zoom_y_j)
      + (2 # 3) * max0(s V_zoom_y_j) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
      + (1 # 44) * max0(s V_zoom_y_kc)^2
      + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 67 => ((65 # 88) + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                 + s V_zoom_y_kn)
            + (19 # 88) * s V_zoom_y_kc
            - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
            + (2 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_j)
            - (17 # 88) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
            - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
            - (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
            + (17 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + (17 # 88) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (21 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (1 # 3) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc
                                                   + s V_zoom_y_kn)
            + (2 # 3) * max0(s V_zoom_y_j) * max0(-s V_zoom_y_kc
                                                  + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 68 => hints
     [(*-0.193182 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn));
      (*0 0.176136*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - 
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)));
      (*-0.176136 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - s V_zoom_y__tmp) (0))) (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.666667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j)) (F_check_ge (0) (0)));
      (*0 0.102273*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*0 0.0681818*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*0 0.295455*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*0 0.0227273*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*0 0.0681818*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*0 0.102273*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*0 0.0681818*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.352273 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - s V_zoom_y__tmp) (0))) (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp))]
     ((65 # 88) + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
      + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (19 # 88) * s V_zoom_y_kc - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
      + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
      + (2 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_j)
      - (17 # 88) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
      - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
      - (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
      + (17 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      + (17 # 88) * s V_zoom_y_kn^2 + s V_zoom_y_z
      + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
      - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
      + (21 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + 
                                                                    s V_zoom_y_kn)
      + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
      + (1 # 3) * max0(s V_zoom_y_cn)
      + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (2 # 3) * max0(s V_zoom_y_j) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
      + (1 # 44) * max0(s V_zoom_y_kc)^2
      + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 69 => ((31 # 88) * s V_zoom_y__tmp
            + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            - (4 # 11) * s V_zoom_y_kc
            - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
            + (3 # 44) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
            - (13 # 44) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
            + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (17 # 88) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 22) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
            - (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
            + (17 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            + s V_zoom_y_z
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp) * max0(1
                                                             - s V_zoom_y__tmp)
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-s V_zoom_y_kc
                                                            + s V_zoom_y_kn)
            + (9 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_kc)
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
            + (1 # 11) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (3 # 44) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (31 # 88) * max0(1 - s V_zoom_y__tmp)
            + (13 # 44) * max0(1 - s V_zoom_y__tmp) * max0(s V_zoom_y_kc)
            - (9 # 88) * max0(1 - s V_zoom_y__tmp) * max0(s V_zoom_y_kn)
            + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc
                                                   + s V_zoom_y_kn)
            + (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            + (3 # 44) * max0(s V_zoom_y_kn) <= z)%Q
   | 70 => hints
     [(*0 0.102273*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_zoom_y__tmp)) (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*0 0.0909091*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*0 0.102273*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*0 0.102273*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*0 0.102273*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*0 0.0340909*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)))]
     ((31 # 88) * s V_zoom_y__tmp
      + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
      - (4 # 11) * s V_zoom_y_kc
      - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
      + (3 # 44) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
      - (13 # 44) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
      + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
      - (17 # 88) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 22) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
      + (23 # 88) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
      - (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
      + (17 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + s V_zoom_y_z
      + (25 # 142) * max0(-1 + s V_zoom_y__tmp) * max0(1 - s V_zoom_y__tmp)
      + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
      - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
      + (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-s V_zoom_y_kc
                                                      + s V_zoom_y_kn)
      + (9 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_kc)
      + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
      + (1 # 11) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + 
                                                                    s V_zoom_y_kn)
      + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
      + (3 # 44) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + (31 # 88) * max0(1 - s V_zoom_y__tmp)
      + (13 # 44) * max0(1 - s V_zoom_y__tmp) * max0(s V_zoom_y_kc)
      - (9 # 88) * max0(1 - s V_zoom_y__tmp) * max0(s V_zoom_y_kn)
      + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
      + (3 # 44) * max0(s V_zoom_y_kn) <= z)%Q
   | 71 => ((31 # 88) * s V_zoom_y__tmp
            + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
            - (4 # 11) * s V_zoom_y_kc
            - (17 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
            + (3 # 44) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
            - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
            + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
            + (1 # 22) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
            - (9 # 88) * s V_zoom_y_kn * max0(-1 + s V_zoom_y__tmp)
            + (23 # 88) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            - (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp) * max0(1
                                                             - s V_zoom_y__tmp)
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-s V_zoom_y_kc
                                                            + s V_zoom_y_kn)
            + (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_kc)
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
            + (1 # 11) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (3 # 44) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (31 # 88) * max0(1 - s V_zoom_y__tmp)
            + (17 # 88) * max0(1 - s V_zoom_y__tmp) * max0(s V_zoom_y_kc)
            + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc
                                                   + s V_zoom_y_kn)
            + (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (3 # 88) * max0(s V_zoom_y_kn)
            + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 72 => hints
     [(*-0.193182 0*) F_max0_pre_decrement 1 (-s V_zoom_y_kc + s V_zoom_y_kn) (1);
      (*-1 0*) F_max0_monotonic (F_check_ge (s V_zoom_y_j) (-1 + s V_zoom_y_j));
      (*-1 0*) F_max0_ge_0 (-1 + s V_zoom_y_j);
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.176136 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_zoom_y__tmp)) (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.284091 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   - 
                                                                   s V_zoom_y_kc
                                                                   + 
                                                                   s V_zoom_y_kn)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y_kn)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*0 0.0909091*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - s V_zoom_y__tmp) (0))) (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - s V_zoom_y__tmp) (0))) (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.176136 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                    - 
                                                                    s V_zoom_y__tmp)) (F_check_ge (1
                                                                    - s V_zoom_y__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                   - 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)));
      (*-0.0909091 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*0 0.278409*) F_binom_monotonic 1 (F_max0_ge_0 (1 - s V_zoom_y__tmp)) (F_check_ge (0) (0));
      (*0 0.278409*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zoom_y__tmp)) (F_check_ge (0) (0))]
     ((31 # 88) * s V_zoom_y__tmp
      + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
      + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
      - (4 # 11) * s V_zoom_y_kc
      - (17 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
      + (3 # 44) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
      - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
      + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
      + (1 # 22) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
      - (9 # 88) * s V_zoom_y_kn * max0(-1 + s V_zoom_y__tmp)
      + (23 # 88) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
      - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
      + (25 # 142) * max0(-1 + s V_zoom_y__tmp) * max0(1 - s V_zoom_y__tmp)
      + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
      - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
      + (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-s V_zoom_y_kc
                                                      + s V_zoom_y_kn)
      + (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_kc)
      + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
      + (1 # 11) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + 
                                                                    s V_zoom_y_kn)
      + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
      + (3 # 44) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + (31 # 88) * max0(1 - s V_zoom_y__tmp)
      + (17 # 88) * max0(1 - s V_zoom_y__tmp) * max0(s V_zoom_y_kc)
      + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (3 # 88) * max0(s V_zoom_y_kn) + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 73 => ((17 # 88) + (31 # 88) * s V_zoom_y__tmp
            + (1 # 11) * s V_zoom_y__tmp * max0(-1 - s V_zoom_y_kc
                                                + s V_zoom_y_kn)
            + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
            - (4 # 11) * s V_zoom_y_kc
            - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
            + (3 # 44) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
            - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
            + (1 # 22) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
            + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
            - (22 # 79) * max0(-1 + s V_zoom_y__tmp)
            - (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-1
                                                            + s V_zoom_y_kn)
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
            + (17 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            - (17 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(1
                                                                    - s V_zoom_y__tmp)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            - (9 # 88) * max0(-1 + s V_zoom_y_kn) * max0(1 - s V_zoom_y__tmp)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (3 # 44) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (9 # 122) * max0(1 - s V_zoom_y__tmp)
            - (25 # 142) * max0(1 - s V_zoom_y__tmp)^2 - max0(s V_zoom_y_j)
            - (3 # 88) * max0(s V_zoom_y_kn)
            + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 74 => hints
     [(*-0.193182 0*) F_max0_pre_decrement 1 (-s V_zoom_y_kc + s V_zoom_y_kn) (1);
      (*-1 0*) F_max0_monotonic (F_check_ge (s V_zoom_y_j) (-1 + s V_zoom_y_j));
      (*-1 0*) F_max0_ge_0 (-1 + s V_zoom_y_j);
      (*-0.176136 0*) F_binom_monotonic 2 (F_max0_ge_0 (1 - s V_zoom_y__tmp)) (F_check_ge (0) (0));
      (*-0.0909091 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_zoom_y__tmp)) (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.176136 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0909091 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   - 
                                                                   s V_zoom_y_kc
                                                                   + 
                                                                   s V_zoom_y_kn)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                   - 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)));
      (*-0.352273 0*) F_binom_monotonic 1 (F_max0_ge_0 (1 - s V_zoom_y__tmp)) (F_check_ge (0) (0));
      (*-0.176136 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zoom_y__tmp)) (F_check_ge (0) (0))]
     ((31 # 88) * s V_zoom_y__tmp
      + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
      + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
      - (4 # 11) * s V_zoom_y_kc
      - (17 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
      + (3 # 44) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
      - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
      + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
      + (1 # 22) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
      - (9 # 88) * s V_zoom_y_kn * max0(-1 + s V_zoom_y__tmp)
      + (23 # 88) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
      - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
      + (25 # 142) * max0(-1 + s V_zoom_y__tmp) * max0(1 - s V_zoom_y__tmp)
      + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
      - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
      + (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-s V_zoom_y_kc
                                                      + s V_zoom_y_kn)
      + (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_kc)
      + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
      + (1 # 11) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + 
                                                                    s V_zoom_y_kn)
      + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
      + (3 # 44) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + (31 # 88) * max0(1 - s V_zoom_y__tmp)
      + (17 # 88) * max0(1 - s V_zoom_y__tmp) * max0(s V_zoom_y_kc)
      + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (3 # 88) * max0(s V_zoom_y_kn) + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 75 => ((17 # 88) + (31 # 88) * s V_zoom_y__tmp
            + (1 # 11) * s V_zoom_y__tmp * max0(-1 - s V_zoom_y_kc
                                                + s V_zoom_y_kn)
            + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
            - (4 # 11) * s V_zoom_y_kc
            - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
            + (3 # 44) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
            - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
            + (1 # 22) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
            + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
            - (22 # 79) * max0(-1 + s V_zoom_y__tmp)
            - (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-1
                                                            + s V_zoom_y_kn)
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
            + (17 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            - (17 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(1
                                                                    - s V_zoom_y__tmp)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            - (9 # 88) * max0(-1 + s V_zoom_y_kn) * max0(1 - s V_zoom_y__tmp)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (3 # 44) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (9 # 122) * max0(1 - s V_zoom_y__tmp)
            - (25 # 142) * max0(1 - s V_zoom_y__tmp)^2 - max0(s V_zoom_y_j)
            - (3 # 88) * max0(s V_zoom_y_kn)
            + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 76 => ((17 # 88) + (31 # 88) * s V_zoom_y__tmp
            + (1 # 11) * s V_zoom_y__tmp * max0(-1 - s V_zoom_y_kc
                                                + s V_zoom_y_kn)
            + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
            - (4 # 11) * s V_zoom_y_kc
            - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
            + (3 # 44) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
            - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
            + (1 # 22) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
            + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
            - (22 # 79) * max0(-1 + s V_zoom_y__tmp)
            - (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-1
                                                            + s V_zoom_y_kn)
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
            + (17 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            - (17 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(1
                                                                    - s V_zoom_y__tmp)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            - (9 # 88) * max0(-1 + s V_zoom_y_kn) * max0(1 - s V_zoom_y__tmp)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (3 # 44) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (9 # 122) * max0(1 - s V_zoom_y__tmp)
            - (25 # 142) * max0(1 - s V_zoom_y__tmp)^2 - max0(s V_zoom_y_j)
            - (3 # 88) * max0(s V_zoom_y_kn)
            + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 77 => hints
     [(*-0.193182 0*) F_max0_pre_decrement 1 (-s V_zoom_y_kc + s V_zoom_y_kn) (1);
      (*-1 0*) F_max0_monotonic (F_check_ge (s V_zoom_y_j) (-1 + s V_zoom_y_j));
      (*-1 0*) F_max0_ge_0 (-1 + s V_zoom_y_j);
      (*-0.176136 0*) F_binom_monotonic 2 (F_max0_ge_0 (1 - s V_zoom_y__tmp)) (F_check_ge (0) (0));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.176136 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   + 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.386364 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                   - 
                                                                   s V_zoom_y_kc
                                                                   + 
                                                                   s V_zoom_y_kn)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.0909091 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - s V_zoom_y__tmp) (0))) (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - s V_zoom_y__tmp) (0))) (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.295455 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                   - 
                                                                   s V_zoom_y__tmp)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)));
      (*-0.0340909 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.454545 0*) F_binom_monotonic 1 (F_max0_ge_0 (1 - s V_zoom_y__tmp)) (F_check_ge (0) (0));
      (*-0.278409 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zoom_y__tmp)) (F_check_ge (0) (0))]
     ((31 # 88) * s V_zoom_y__tmp
      + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
      - (4 # 11) * s V_zoom_y_kc
      - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
      + (3 # 44) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
      - (13 # 44) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
      + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
      - (17 # 88) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 22) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
      + (23 # 88) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
      - (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
      + (17 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      + s V_zoom_y_z
      + (25 # 142) * max0(-1 + s V_zoom_y__tmp) * max0(1 - s V_zoom_y__tmp)
      + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
      - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
      + (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-s V_zoom_y_kc
                                                      + s V_zoom_y_kn)
      + (9 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_kc)
      + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
      + (1 # 11) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + 
                                                                    s V_zoom_y_kn)
      + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
      + (3 # 44) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + (31 # 88) * max0(1 - s V_zoom_y__tmp)
      + (13 # 44) * max0(1 - s V_zoom_y__tmp) * max0(s V_zoom_y_kc)
      - (9 # 88) * max0(1 - s V_zoom_y__tmp) * max0(s V_zoom_y_kn)
      + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
      + (3 # 44) * max0(s V_zoom_y_kn) <= z)%Q
   | 78 => ((17 # 88) + (31 # 88) * s V_zoom_y__tmp
            + (1 # 11) * s V_zoom_y__tmp * max0(-1 - s V_zoom_y_kc
                                                + s V_zoom_y_kn)
            + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
            - (4 # 11) * s V_zoom_y_kc
            - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
            + (3 # 44) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
            - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
            + (1 # 22) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
            + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
            - (22 # 79) * max0(-1 + s V_zoom_y__tmp)
            - (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-1
                                                            + s V_zoom_y_kn)
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
            + (17 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            - (17 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(1
                                                                    - s V_zoom_y__tmp)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            - (9 # 88) * max0(-1 + s V_zoom_y_kn) * max0(1 - s V_zoom_y__tmp)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (3 # 44) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (9 # 122) * max0(1 - s V_zoom_y__tmp)
            - (25 # 142) * max0(1 - s V_zoom_y__tmp)^2 - max0(s V_zoom_y_j)
            - (3 # 88) * max0(s V_zoom_y_kn)
            + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 79 => ((17 # 88) + (31 # 88) * s V_zoom_y__tmp
            + (1 # 11) * s V_zoom_y__tmp * max0(-1 - s V_zoom_y_kc
                                                + s V_zoom_y_kn)
            + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
            - (4 # 11) * s V_zoom_y_kc
            - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
            + (3 # 44) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
            - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
            + (1 # 22) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
            + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
            - (22 # 79) * max0(-1 + s V_zoom_y__tmp)
            - (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-1
                                                            + s V_zoom_y_kn)
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
            + (17 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            - (17 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(1
                                                                    - s V_zoom_y__tmp)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            - (9 # 88) * max0(-1 + s V_zoom_y_kn) * max0(1 - s V_zoom_y__tmp)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (3 # 44) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (9 # 122) * max0(1 - s V_zoom_y__tmp)
            - (25 # 142) * max0(1 - s V_zoom_y__tmp)^2 - max0(s V_zoom_y_j)
            - (3 # 88) * max0(s V_zoom_y_kn)
            + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 80 => ((17 # 88) + (31 # 88) * s V_zoom_y__tmp
            + (1 # 11) * s V_zoom_y__tmp * max0(-1 - s V_zoom_y_kc
                                                + s V_zoom_y_kn)
            + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
            - (4 # 11) * s V_zoom_y_kc
            - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
            + (3 # 44) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                              + s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
            - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
            + (1 # 22) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kn * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
            + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
            - (22 # 79) * max0(-1 + s V_zoom_y__tmp)
            - (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-1
                                                            + s V_zoom_y_kn)
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
            + (17 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            - (17 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(1
                                                                    - s V_zoom_y__tmp)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            - (9 # 88) * max0(-1 + s V_zoom_y_kn) * max0(1 - s V_zoom_y__tmp)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            + (3 # 44) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (9 # 122) * max0(1 - s V_zoom_y__tmp)
            - (25 # 142) * max0(1 - s V_zoom_y__tmp)^2 - max0(s V_zoom_y_j)
            - (3 # 88) * max0(s V_zoom_y_kn)
            + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 81 => ((53 # 88) + (31 # 88) * s V_zoom_y__tmp
            + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (1 # 11) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                + s V_zoom_y_kn)
            + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
            - (5 # 11) * s V_zoom_y_kc
            - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
            + (1 # 22) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
            - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
            + (3 # 44) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
            - (1 # 11) * s V_zoom_y_kn * max0(-1 + s V_zoom_y_kc)
            + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
            + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
            + (23 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
            - (25 # 142) * max0(-1 + s V_zoom_y__tmp)
            - (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-1
                                                            + s V_zoom_y_kn)
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
            - (1 # 22) * max0(-1 + s V_zoom_y_kc)
            + (3 # 44) * max0(-1 + s V_zoom_y_kc) * max0(-1 + s V_zoom_y_kn)
            + (1 # 44) * max0(-1 + s V_zoom_y_kc) * max0(-s V_zoom_y_kc
                                                         + s V_zoom_y_kn)
            + (3 # 44) * max0(-1 + s V_zoom_y_kn)
            - (9 # 88) * max0(-1 + s V_zoom_y_kn) * max0(1 - s V_zoom_y__tmp)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            - (23 # 88) * max0(-1 + s V_zoom_y_kn) * max0(-s V_zoom_y_kc
                                                          + s V_zoom_y_kn)
            + (39 # 146) * max0(1 - s V_zoom_y__tmp)
            - (17 # 88) * max0(1 - s V_zoom_y__tmp) * max0(-s V_zoom_y_kc
                                                           + s V_zoom_y_kn)
            - (25 # 142) * max0(1 - s V_zoom_y__tmp)^2
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - max0(s V_zoom_y_j)
            + (1 # 8) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 88) * max0(s V_zoom_y_kn)
            + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 82 => ((53 # 88) + (31 # 88) * s V_zoom_y__tmp
            + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (1 # 11) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                + s V_zoom_y_kn)
            + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
            - (5 # 11) * s V_zoom_y_kc
            - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
            + (1 # 22) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
            - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
            + (3 # 44) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
            - (1 # 11) * s V_zoom_y_kn * max0(-1 + s V_zoom_y_kc)
            + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
            + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
            + (23 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
            - (25 # 142) * max0(-1 + s V_zoom_y__tmp)
            - (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-1
                                                            + s V_zoom_y_kn)
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
            - (1 # 22) * max0(-1 + s V_zoom_y_kc)
            + (3 # 44) * max0(-1 + s V_zoom_y_kc) * max0(-1 + s V_zoom_y_kn)
            + (1 # 44) * max0(-1 + s V_zoom_y_kc) * max0(-s V_zoom_y_kc
                                                         + s V_zoom_y_kn)
            + (3 # 44) * max0(-1 + s V_zoom_y_kn)
            - (9 # 88) * max0(-1 + s V_zoom_y_kn) * max0(1 - s V_zoom_y__tmp)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            - (23 # 88) * max0(-1 + s V_zoom_y_kn) * max0(-s V_zoom_y_kc
                                                          + s V_zoom_y_kn)
            + (39 # 146) * max0(1 - s V_zoom_y__tmp)
            - (17 # 88) * max0(1 - s V_zoom_y__tmp) * max0(-s V_zoom_y_kc
                                                           + s V_zoom_y_kn)
            - (25 # 142) * max0(1 - s V_zoom_y__tmp)^2
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - max0(s V_zoom_y_j)
            + (1 # 8) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 88) * max0(s V_zoom_y_kn)
            + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 83 => ((53 # 88) + (31 # 88) * s V_zoom_y__tmp
            + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
            + (1 # 11) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                + s V_zoom_y_kn)
            + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
            - (5 # 11) * s V_zoom_y_kc
            - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
            + (1 # 22) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
            - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
            + (3 # 44) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
            - (1 # 11) * s V_zoom_y_kn * max0(-1 + s V_zoom_y_kc)
            + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
            + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
            + (23 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
            - (25 # 142) * max0(-1 + s V_zoom_y__tmp)
            - (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-1
                                                            + s V_zoom_y_kn)
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
            + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
            - (1 # 22) * max0(-1 + s V_zoom_y_kc)
            + (3 # 44) * max0(-1 + s V_zoom_y_kc) * max0(-1 + s V_zoom_y_kn)
            + (1 # 44) * max0(-1 + s V_zoom_y_kc) * max0(-s V_zoom_y_kc
                                                         + s V_zoom_y_kn)
            + (3 # 44) * max0(-1 + s V_zoom_y_kn)
            - (9 # 88) * max0(-1 + s V_zoom_y_kn) * max0(1 - s V_zoom_y__tmp)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
            - (23 # 88) * max0(-1 + s V_zoom_y_kn) * max0(-s V_zoom_y_kc
                                                          + s V_zoom_y_kn)
            + (39 # 146) * max0(1 - s V_zoom_y__tmp)
            - (17 # 88) * max0(1 - s V_zoom_y__tmp) * max0(-s V_zoom_y_kc
                                                           + s V_zoom_y_kn)
            - (25 # 142) * max0(1 - s V_zoom_y__tmp)^2
            + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - max0(s V_zoom_y_j)
            + (1 # 8) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 88) * max0(s V_zoom_y_kn)
            + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 84 => hints
     [(*-0.0454545 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1 + s V_zoom_y_kc)) (F_check_ge (-1
                                                                    + s V_zoom_y_kc) (0));
      (*-0.176136 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (1
                                                                    - s V_zoom_y__tmp) (0))) (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp));
      (*0 0.0454545*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc));
      (*0 0.0340909*) F_binom_monotonic 2 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.666667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.666667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_zoom_y__tmp)) (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_cn)) (F_check_ge (0) (0)));
      (*0 0.102273*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_zoom_y__tmp)) (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*0 0.0454545*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kc) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0227273 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kc) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j)) (F_check_ge (0) (0)));
      (*-0.784091 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.590909 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0340909 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0909091 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.556818 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    - s V_zoom_y__tmp) (0))) (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.193182 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.454545 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0227273 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.522727 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.522727 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0227273 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.590909 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0227273 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.556818 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.0681818 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc
                                                                    - s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.102273 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp)) (F_check_ge (0) (0)));
      (*-0.454545 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.522727 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_z) (0))) (F_max0_ge_0 (s V_zoom_y_z));
      (*-0.454545 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn));
      (*-0.590909 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0));
      (*0 0.0227273*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kc) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kc));
      (*-0.0340909 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn));
      (*-0.176136 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                          + s V_zoom_y__tmp)) (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))]
     (-(35 # 88) + (31 # 88) * s V_zoom_y__tmp
      + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
      + (1 # 11) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (9 # 88) * s V_zoom_y__tmp * max0(s V_zoom_y_kn)
      - (5 # 11) * s V_zoom_y_kc
      - (9 # 88) * s V_zoom_y_kc * max0(-1 + s V_zoom_y__tmp)
      + (1 # 22) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kc)
      - (3 # 44) * s V_zoom_y_kc * max0(-1 + s V_zoom_y_kn)
      - (17 # 88) * s V_zoom_y_kc * max0(1 - s V_zoom_y__tmp)
      + (3 # 44) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 22) * s V_zoom_y_kc^2 + (17 # 44) * s V_zoom_y_kn
      - (1 # 11) * s V_zoom_y_kn * max0(-1 + s V_zoom_y_kc)
      + (9 # 88) * s V_zoom_y_kn * max0(1 - s V_zoom_y__tmp)
      + (1 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_j)
      + (23 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 88) * s V_zoom_y_kn * max0(s V_zoom_y_kn) + s V_zoom_y_z
      - (25 # 142) * max0(-1 + s V_zoom_y__tmp)
      - (17 # 88) * max0(-1 + s V_zoom_y__tmp) * max0(-1 + s V_zoom_y_kn)
      + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
      - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
      + (25 # 142) * max0(-1 + s V_zoom_y__tmp)^2
      - (1 # 22) * max0(-1 + s V_zoom_y_kc)
      + (3 # 44) * max0(-1 + s V_zoom_y_kc) * max0(-1 + s V_zoom_y_kn)
      + (1 # 44) * max0(-1 + s V_zoom_y_kc) * max0(-s V_zoom_y_kc
                                                   + s V_zoom_y_kn)
      + (3 # 44) * max0(-1 + s V_zoom_y_kn)
      - (9 # 88) * max0(-1 + s V_zoom_y_kn) * max0(1 - s V_zoom_y__tmp)
      - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
      - (23 # 88) * max0(-1 + s V_zoom_y_kn) * max0(-s V_zoom_y_kc
                                                    + s V_zoom_y_kn)
      + (39 # 146) * max0(1 - s V_zoom_y__tmp)
      - (17 # 88) * max0(1 - s V_zoom_y__tmp) * max0(-s V_zoom_y_kc
                                                     + s V_zoom_y_kn)
      - (25 # 142) * max0(1 - s V_zoom_y__tmp)^2
      + max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - max0(s V_zoom_y_j) + (1 # 8) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 88) * max0(s V_zoom_y_kn) + (3 # 88) * max0(s V_zoom_y_kn)^2 <= z)%Q
   | 85 => hints
     [(*-0.333333 0*) F_max0_pre_decrement 1 (s V_zoom_y_j) (1);
      (*0 0.666667*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j)) (F_check_ge (0) (0)));
      (*0.333333 0.666667*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_j) (0))) (F_max0_ge_0 (s V_zoom_y_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_j) (0))) (F_max0_ge_0 (s V_zoom_y_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.666667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_j)) (F_check_ge (s V_zoom_y_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_j)) (F_check_ge (s V_zoom_y_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_kc) (0))) (F_max0_ge_0 (s V_zoom_y_kc))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j)) (F_check_ge (0) (0)))]
     ((65 # 88) + (2 # 3) * s V_zoom_y__tmp * max0(s V_zoom_y_j)
      + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (19 # 88) * s V_zoom_y_kc - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
      + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
      + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_j)
      - (17 # 88) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
      - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + (17 # 88) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      + (17 # 88) * s V_zoom_y_kn^2 + s V_zoom_y_z
      + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
      - (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_j)
      + (21 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + 
                                                                    s V_zoom_y_kn)
      + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - (1 # 3) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_j)
      + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      - (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_j)
      + (1 # 3) * max0(s V_zoom_y_cn)
      + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 3) * max0(s V_zoom_y_j)
      + (2 # 3) * max0(s V_zoom_y_j) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (17 # 88) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
      + (1 # 44) * max0(s V_zoom_y_kc)^2
      + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 86 => ((134 # 125)
            + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                 + s V_zoom_y_kn)
            - (1 # 3) * s V_zoom_y_j * max0(-1 - s V_zoom_y_kc
                                            + s V_zoom_y_kn)
            - (1 # 3) * s V_zoom_y_j * max0(-1 + s V_zoom_y_kn)
            + (2 # 3) * s V_zoom_y_j * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_j * max0(s V_zoom_y_kc)
            + (19 # 88) * s V_zoom_y_kc
            - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (3 # 113) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
            - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + (3 # 113) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + (17 # 88) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(-1 + s V_zoom_y_j)
            + (21 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc
                                                   + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (3 # 113) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 87 => ((134 # 125)
            + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                 + s V_zoom_y_kn)
            - (1 # 3) * s V_zoom_y_j * max0(-1 - s V_zoom_y_kc
                                            + s V_zoom_y_kn)
            - (1 # 3) * s V_zoom_y_j * max0(-1 + s V_zoom_y_kn)
            + (2 # 3) * s V_zoom_y_j * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_j * max0(s V_zoom_y_kc)
            + (19 # 88) * s V_zoom_y_kc
            - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (3 # 113) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
            - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + (3 # 113) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + (17 # 88) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(-1 + s V_zoom_y_j)
            + (21 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc
                                                   + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (3 # 113) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 88 => ((134 # 125)
            + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                 + s V_zoom_y_kn)
            - (1 # 3) * s V_zoom_y_j * max0(-1 - s V_zoom_y_kc
                                            + s V_zoom_y_kn)
            - (1 # 3) * s V_zoom_y_j * max0(-1 + s V_zoom_y_kn)
            + (2 # 3) * s V_zoom_y_j * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_j * max0(s V_zoom_y_kc)
            + (19 # 88) * s V_zoom_y_kc
            - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (3 # 113) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
            - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + (3 # 113) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + (17 # 88) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            + (18 # 125) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn)
            + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc
                                                   + s V_zoom_y_kn)
            + (1 # 3) * max0(s V_zoom_y_j)
            + (2 # 3) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (3 # 113) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            + (1 # 3) * max0(s V_zoom_y_kc)
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 89 => ((134 # 125)
            + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                 + s V_zoom_y_kn)
            - (1 # 3) * s V_zoom_y_j * max0(-1 - s V_zoom_y_kc
                                            + s V_zoom_y_kn)
            - (1 # 3) * s V_zoom_y_j * max0(-1 + s V_zoom_y_kn)
            + (2 # 3) * s V_zoom_y_j * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_j * max0(s V_zoom_y_kc)
            + (19 # 88) * s V_zoom_y_kc
            - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (3 # 113) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
            - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + (3 # 113) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + (17 # 88) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            + (18 # 125) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn)
            + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc
                                                   + s V_zoom_y_kn)
            + (1 # 3) * max0(s V_zoom_y_j)
            + (2 # 3) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (3 # 113) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            + (1 # 3) * max0(s V_zoom_y_kc)
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 90 => ((134 # 125)
            + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc
                                                 + s V_zoom_y_kn)
            - (1 # 3) * s V_zoom_y_j * max0(-1 - s V_zoom_y_kc
                                            + s V_zoom_y_kn)
            - (1 # 3) * s V_zoom_y_j * max0(-1 + s V_zoom_y_kn)
            + (2 # 3) * s V_zoom_y_j * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_j * max0(s V_zoom_y_kc)
            + (19 # 88) * s V_zoom_y_kc
            - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
            + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc
                                               + s V_zoom_y_kn)
            + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
            - (3 # 113) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
            - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
            + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
            - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
            + (3 # 113) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
            + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
            + (17 # 88) * s V_zoom_y_kn^2 + s V_zoom_y_z
            + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
            + (18 # 125) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
            - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + s V_zoom_y_kn)
            + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
            - (1 # 3) * max0(-1 + s V_zoom_y_kn)
            + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn)
            + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc
                                                   + s V_zoom_y_kn)
            + (1 # 3) * max0(s V_zoom_y_j)
            + (2 # 3) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
            - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                                    - 
                                                                    s V_zoom_y_kn)
            - (3 # 113) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
            + (1 # 3) * max0(s V_zoom_y_kc)
            + (1 # 44) * max0(s V_zoom_y_kc)^2
            + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | 91 => hints
     [(*-0.666667 0*) F_max0_pre_decrement 1 (-s V_zoom_y_kc + s V_zoom_y_kn) (1);
      (*-0.166667 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn));
      (*-0.166667 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1 + s V_zoom_y_kn)) (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0));
      (*-0.166667 0*) F_binom_monotonic 2 (F_max0_ge_arg (-s V_zoom_y_kc
                                                          + s V_zoom_y_kn)) (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0));
      (*-0.166667 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_zoom_y_kn) (0))) (F_max0_ge_0 (s V_zoom_y_kn));
      (*-0.666667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_zoom_y__tmp)) (F_check_ge (-1
                                                                    + s V_zoom_y__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - 
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - 
                                                                    s V_zoom_y_kc
                                                                    + 
                                                                    s V_zoom_y_kn)) (F_check_ge (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.666667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_j) (0))) (F_max0_ge_0 (s V_zoom_y_j))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zoom_y_j) (0))) (F_max0_ge_0 (s V_zoom_y_j))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kc)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_j)) (F_check_ge (s V_zoom_y_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_j)) (F_check_ge (s V_zoom_y_j) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn) (0))) (F_max0_ge_0 (-
                                                                    s V_zoom_y_kc
                                                                    + s V_zoom_y_kn))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_zoom_y_kc
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_j)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.166667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kn)) (F_check_ge (s V_zoom_y_kn) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_zoom_y_kn)) (F_check_ge (0) (0)));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zoom_y_kc)) (F_check_ge (s V_zoom_y_kc) (0))]
     ((9 # 125)
      + (17 # 88) * s V_zoom_y__tmp * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 3) * s V_zoom_y_j * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 3) * s V_zoom_y_j * max0(-1 + s V_zoom_y_kn)
      + (2 # 3) * s V_zoom_y_j * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 3) * s V_zoom_y_j * max0(s V_zoom_y_kc)
      + (19 # 88) * s V_zoom_y_kc - (17 # 44) * s V_zoom_y_kc * s V_zoom_y_kn
      + (23 # 88) * s V_zoom_y_kc * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 3) * s V_zoom_y_kc * max0(s V_zoom_y_cn)
      - (3 # 113) * s V_zoom_y_kc * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kc - s V_zoom_y_kn)
      - (3 # 44) * s V_zoom_y_kc * max0(s V_zoom_y_kn)
      + (21 # 88) * s V_zoom_y_kc^2 - (17 # 88) * s V_zoom_y_kn
      - (2 # 3) * s V_zoom_y_kn * max0(s V_zoom_y_cn)
      + (3 # 113) * s V_zoom_y_kn * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (1 # 11) * s V_zoom_y_kn * max0(s V_zoom_y_kc)
      + (3 # 44) * s V_zoom_y_kn * max0(s V_zoom_y_kn)
      + (17 # 88) * s V_zoom_y_kn^2 + s V_zoom_y_z
      + (2 # 3) * max0(-1 + s V_zoom_y__tmp) * max0(s V_zoom_y_cn)
      + (18 # 125) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn)
      - (23 # 88) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(-1
                                                                    + 
                                                                    s V_zoom_y_kn)
      + max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      + (1 # 44) * max0(-1 - s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc)
      - (1 # 3) * max0(-1 + s V_zoom_y_kn)
      + (1 # 3) * max0(-1 + s V_zoom_y_kn) * max0(s V_zoom_y_cn)
      + (1 # 3) * max0(s V_zoom_y_cn)
      + (1 # 3) * max0(s V_zoom_y_cn) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      + (1 # 3) * max0(s V_zoom_y_j)
      + (2 # 3) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)
      - (3 # 44) * max0(-s V_zoom_y_kc + s V_zoom_y_kn) * max0(s V_zoom_y_kc
                                                               - s V_zoom_y_kn)
      - (3 # 113) * max0(-s V_zoom_y_kc + s V_zoom_y_kn)^2
      + (1 # 3) * max0(s V_zoom_y_kc) + (1 # 44) * max0(s V_zoom_y_kc)^2
      + (3 # 44) * max0(s V_zoom_y_kc - s V_zoom_y_kn) * max0(s V_zoom_y_kn) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_zoom_y =>
    [mkPA Q (fun n z s => ai_zoom_y n s /\ annot0_zoom_y n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_zoom_y (proc_start P_zoom_y) s1 (proc_end P_zoom_y) s2 ->
    (s2 V_zoom_y_z <= max0(s1 V_zoom_y_Colors * s1 V_zoom_y_WidthOut)
                      + max0(s1 V_zoom_y_Colors * s1 V_zoom_y_WidthOut) * max0(s1 V_zoom_y_contrib_dref_off4))%Q.
Proof.
  prove_bound ipa admissible_ipa P_zoom_y.
Qed.
