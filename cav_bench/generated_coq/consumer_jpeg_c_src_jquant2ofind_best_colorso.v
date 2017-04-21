Require Import pasta.Pasta.

Inductive proc: Type :=
  P_find_best_colors.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_find_best_colors_z := 1%positive.
Notation V_find_best_colors__tmp := 2%positive.
Notation V_find_best_colors__tmp1 := 3%positive.
Notation V_find_best_colors__tmp2 := 4%positive.
Notation V_find_best_colors__tmp3 := 5%positive.
Notation V_find_best_colors_dist0 := 6%positive.
Notation V_find_best_colors_dist1 := 7%positive.
Notation V_find_best_colors_dist2 := 8%positive.
Notation V_find_best_colors_i := 9%positive.
Notation V_find_best_colors_ic0 := 10%positive.
Notation V_find_best_colors_ic1 := 11%positive.
Notation V_find_best_colors_ic2 := 12%positive.
Notation V_find_best_colors_icolor := 13%positive.
Notation V_find_best_colors_inc0 := 14%positive.
Notation V_find_best_colors_inc1 := 15%positive.
Notation V_find_best_colors_inc2 := 16%positive.
Notation V_find_best_colors_xx0 := 17%positive.
Notation V_find_best_colors_xx1 := 18%positive.
Notation V_find_best_colors_xx2 := 19%positive.
Notation V_find_best_colors_bestcolor := 20%positive.
Notation V_find_best_colors_cinfo := 21%positive.
Notation V_find_best_colors_colorlist := 22%positive.
Notation V_find_best_colors_minc0 := 23%positive.
Notation V_find_best_colors_minc1 := 24%positive.
Notation V_find_best_colors_minc2 := 25%positive.
Notation V_find_best_colors_numcolors := 26%positive.
Definition Pedges_find_best_colors: list (edge proc) :=
  (EA 1 (AAssign V_find_best_colors_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_find_best_colors__tmp3 (Some (EVar V_find_best_colors_minc0))) 3)::
  (EA 3 (AAssign V_find_best_colors__tmp2
  (Some (EVar V_find_best_colors_minc1))) 4)::(EA 4 (AAssign
  V_find_best_colors__tmp1 (Some (EVar V_find_best_colors_minc2))) 5)::
  (EA 5 (AAssign V_find_best_colors__tmp
  (Some (EVar V_find_best_colors_numcolors))) 6)::(EA 6 (AAssign
  V_find_best_colors_i (Some (ENum (127)))) 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_find_best_colors_i) s) >= (eval (ENum (0))
  s))%Z)) 83)::(EA 9 (AGuard (fun s => ((eval (EVar V_find_best_colors_i)
  s) < (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 (AAssign
  V_find_best_colors_i (Some (ENum (0)))) 12)::(EA 12 ANone 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_find_best_colors_i) s) <
  (eval (EVar V_find_best_colors__tmp) s))%Z)) 17)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_find_best_colors_i) s) >=
  (eval (EVar V_find_best_colors__tmp) s))%Z)) 15)::(EA 15 AWeaken 16)::
  (EA 17 AWeaken 18)::(EA 18 (AAssign V_find_best_colors_icolor None) 19)::
  (EA 19 (AAssign V_find_best_colors_inc0 None) 20)::(EA 20 (AAssign
  V_find_best_colors_dist0 (Some (EMul (EVar V_find_best_colors_inc0)
  (EVar V_find_best_colors_inc0)))) 21)::(EA 21 (AAssign
  V_find_best_colors_inc1 None) 22)::(EA 22 (AAssign V_find_best_colors_dist0
  (Some (EAdd (EVar V_find_best_colors_dist0)
  (EMul (EVar V_find_best_colors_inc1)
  (EVar V_find_best_colors_inc1))))) 23)::(EA 23 (AAssign
  V_find_best_colors_inc2 None) 24)::(EA 24 (AAssign V_find_best_colors_dist0
  (Some (EAdd (EVar V_find_best_colors_dist0)
  (EMul (EVar V_find_best_colors_inc2)
  (EVar V_find_best_colors_inc2))))) 25)::(EA 25 (AAssign
  V_find_best_colors_inc0 (Some (EAdd (EMul (EVar V_find_best_colors_inc0)
  (ENum (32))) (ENum (256))))) 26)::(EA 26 (AAssign V_find_best_colors_inc1
  (Some (EAdd (EMul (EVar V_find_best_colors_inc1) (ENum (24)))
  (ENum (144))))) 27)::(EA 27 (AAssign V_find_best_colors_inc2
  (Some (EAdd (EMul (EVar V_find_best_colors_inc2) (ENum (16)))
  (ENum (64))))) 28)::(EA 28 (AAssign V_find_best_colors_xx0
  (Some (EVar V_find_best_colors_inc0))) 29)::(EA 29 (AAssign
  V_find_best_colors_ic0 (Some (ENum (3)))) 30)::(EA 30 ANone 31)::
  (EA 31 AWeaken 32)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_find_best_colors_ic0) s) >= (eval (ENum (0))
  s))%Z)) 40)::(EA 32 (AGuard (fun s => ((eval (EVar V_find_best_colors_ic0)
  s) < (eval (ENum (0)) s))%Z)) 33)::(EA 33 AWeaken 34)::(EA 34 ANone 35)::
  (EA 35 (AAssign V_find_best_colors_i
  (Some (EAdd (EVar V_find_best_colors_i) (ENum (1))))) 36)::
  (EA 36 ANone 37)::(EA 37 ANone 38)::(EA 38 (AAssign V_find_best_colors_z
  (Some (EAdd (ENum (1)) (EVar V_find_best_colors_z)))) 39)::
  (EA 39 AWeaken 14)::(EA 40 AWeaken 41)::(EA 41 (AAssign
  V_find_best_colors_dist1 (Some (EVar V_find_best_colors_dist0))) 42)::
  (EA 42 (AAssign V_find_best_colors_xx1
  (Some (EVar V_find_best_colors_inc1))) 43)::(EA 43 (AAssign
  V_find_best_colors_ic1 (Some (ENum (7)))) 44)::(EA 44 ANone 45)::
  (EA 45 AWeaken 46)::(EA 46 (AGuard
  (fun s => ((eval (EVar V_find_best_colors_ic1) s) >= (eval (ENum (0))
  s))%Z)) 56)::(EA 46 (AGuard (fun s => ((eval (EVar V_find_best_colors_ic1)
  s) < (eval (ENum (0)) s))%Z)) 47)::(EA 47 AWeaken 48)::(EA 48 (AAssign
  V_find_best_colors_dist0 (Some (EAdd (EVar V_find_best_colors_dist0)
  (EVar V_find_best_colors_xx0)))) 49)::(EA 49 (AAssign
  V_find_best_colors_xx0 (Some (EAdd (EVar V_find_best_colors_xx0)
  (ENum (512))))) 50)::(EA 50 ANone 51)::(EA 51 (AAssign
  V_find_best_colors_ic0 (Some (EAdd (EVar V_find_best_colors_ic0)
  (ENum (-1))))) 52)::(EA 52 ANone 53)::(EA 53 ANone 54)::(EA 54 (AAssign
  V_find_best_colors_z (Some (EAdd (ENum (1))
  (EVar V_find_best_colors_z)))) 55)::(EA 55 AWeaken 32)::
  (EA 56 AWeaken 57)::(EA 57 (AAssign V_find_best_colors_dist2
  (Some (EVar V_find_best_colors_dist1))) 58)::(EA 58 (AAssign
  V_find_best_colors_xx2 (Some (EVar V_find_best_colors_inc2))) 59)::
  (EA 59 (AAssign V_find_best_colors_ic2 (Some (ENum (3)))) 60)::
  (EA 60 ANone 61)::(EA 61 AWeaken 62)::(EA 62 (AGuard
  (fun s => ((eval (EVar V_find_best_colors_ic2) s) >= (eval (ENum (0))
  s))%Z)) 72)::(EA 62 (AGuard (fun s => ((eval (EVar V_find_best_colors_ic2)
  s) < (eval (ENum (0)) s))%Z)) 63)::(EA 63 AWeaken 64)::(EA 64 (AAssign
  V_find_best_colors_dist1 (Some (EAdd (EVar V_find_best_colors_dist1)
  (EVar V_find_best_colors_xx1)))) 65)::(EA 65 (AAssign
  V_find_best_colors_xx1 (Some (EAdd (EVar V_find_best_colors_xx1)
  (ENum (288))))) 66)::(EA 66 ANone 67)::(EA 67 (AAssign
  V_find_best_colors_ic1 (Some (EAdd (EVar V_find_best_colors_ic1)
  (ENum (-1))))) 68)::(EA 68 ANone 69)::(EA 69 ANone 70)::(EA 70 (AAssign
  V_find_best_colors_z (Some (EAdd (ENum (1))
  (EVar V_find_best_colors_z)))) 71)::(EA 71 AWeaken 46)::
  (EA 72 AWeaken 73)::(EA 73 ANone 74)::(EA 73 ANone 75)::(EA 74 ANone 75)::
  (EA 75 (AAssign V_find_best_colors_dist2
  (Some (EAdd (EVar V_find_best_colors_dist2)
  (EVar V_find_best_colors_xx2)))) 76)::(EA 76 (AAssign
  V_find_best_colors_xx2 (Some (EAdd (EVar V_find_best_colors_xx2)
  (ENum (128))))) 77)::(EA 77 ANone 78)::(EA 78 (AAssign
  V_find_best_colors_ic2 (Some (EAdd (EVar V_find_best_colors_ic2)
  (ENum (-1))))) 79)::(EA 79 ANone 80)::(EA 80 ANone 81)::(EA 81 (AAssign
  V_find_best_colors_z (Some (EAdd (ENum (1))
  (EVar V_find_best_colors_z)))) 82)::(EA 82 AWeaken 62)::
  (EA 83 AWeaken 84)::(EA 84 ANone 85)::(EA 85 (AAssign V_find_best_colors_i
  (Some (EAdd (EVar V_find_best_colors_i) (ENum (-1))))) 86)::
  (EA 86 ANone 87)::(EA 87 ANone 88)::(EA 88 (AAssign V_find_best_colors_z
  (Some (EAdd (ENum (1)) (EVar V_find_best_colors_z)))) 89)::
  (EA 89 AWeaken 9)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_find_best_colors => Pedges_find_best_colors
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_find_best_colors => 16
     end)%positive;
  var_global := var_global
}.

Definition ai_find_best_colors (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_z <= 0)%Z
   | 3 => (-1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_z <= 0)%Z
   | 4 => (1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_z <= 0)%Z
   | 5 => (-1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_z <= 0)%Z
   | 6 => (1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_z <= 0)%Z
   | 7 => (-1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_i + -127 <= 0 /\ -1 * s V_find_best_colors_i + 127 <= 0)%Z
   | 8 => (-1 * s V_find_best_colors_i + 127 <= 0 /\ 1 * s V_find_best_colors_i + -127 <= 0 /\ 1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_z <= 0)%Z
   | 9 => (-1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_i + -127 <= 0 /\ -1 * s V_find_best_colors_i + -1 <= 0)%Z
   | 10 => (-1 * s V_find_best_colors_i + -1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 11 => (1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i + -1 <= 0)%Z
   | 12 => (-1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_i <= 0)%Z
   | 13 => (-1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0)%Z
   | 14 => (-1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i <= 0)%Z
   | 15 => (-1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors__tmp+ -1 * s V_find_best_colors_i <= 0)%Z
   | 16 => (1 * s V_find_best_colors__tmp+ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i <= 0)%Z
   | 17 => (-1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 18 => (-1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i <= 0)%Z
   | 19 => (-1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 20 => (-1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i <= 0)%Z
   | 21 => (-1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 22 => (-1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i <= 0)%Z
   | 23 => (-1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 24 => (-1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i <= 0)%Z
   | 25 => (-1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 26 => (-1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i <= 0)%Z
   | 27 => (-1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 28 => (-1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i <= 0)%Z
   | 29 => (-1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 30 => (-1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_ic0 + 3 <= 0)%Z
   | 31 => (-1 * s V_find_best_colors_ic0 + 3 <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 32 => (-1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 33 => (-1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic0 + 1 <= 0)%Z
   | 34 => (1 * s V_find_best_colors_ic0 + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 35 => (-1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic0 + 1 <= 0)%Z
   | 36 => (1 * s V_find_best_colors_ic0 + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 37 => (-1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic0 + 1 <= 0)%Z
   | 38 => (1 * s V_find_best_colors_ic0 + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 39 => (-1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + 1 <= 0 /\ -1 * s V_find_best_colors_z + 1 <= 0)%Z
   | 40 => (-1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_ic0 <= 0)%Z
   | 41 => (-1 * s V_find_best_colors_ic0 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 42 => (-1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_ic0 <= 0)%Z
   | 43 => (-1 * s V_find_best_colors_ic0 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 44 => (-1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_ic0 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors_ic1 + 7 <= 0)%Z
   | 45 => (-1 * s V_find_best_colors_ic1 + 7 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors_ic0 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0)%Z
   | 46 => (-1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0)%Z
   | 47 => (1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic1 + 1 <= 0)%Z
   | 48 => (1 * s V_find_best_colors_ic1 + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0)%Z
   | 49 => (1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic1 + 1 <= 0)%Z
   | 50 => (1 * s V_find_best_colors_ic1 + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0)%Z
   | 51 => (1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic1 + 1 <= 0)%Z
   | 52 => (1 * s V_find_best_colors_ic1 + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -2 <= 0)%Z
   | 53 => (1 * s V_find_best_colors_ic0 + -2 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic1 + 1 <= 0)%Z
   | 54 => (1 * s V_find_best_colors_ic1 + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -2 <= 0)%Z
   | 55 => (1 * s V_find_best_colors_ic0 + -2 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + 1 <= 0 /\ -1 * s V_find_best_colors_z + 1 <= 0)%Z
   | 56 => (1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_ic1 <= 0)%Z
   | 57 => (-1 * s V_find_best_colors_ic1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0)%Z
   | 58 => (1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_ic1 <= 0)%Z
   | 59 => (-1 * s V_find_best_colors_ic1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0)%Z
   | 60 => (1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_ic1 <= 0 /\ 1 * s V_find_best_colors_ic2 + -3 <= 0 /\ -1 * s V_find_best_colors_ic2 + 3 <= 0)%Z
   | 61 => (-1 * s V_find_best_colors_ic2 + 3 <= 0 /\ 1 * s V_find_best_colors_ic2 + -3 <= 0 /\ -1 * s V_find_best_colors_ic1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0)%Z
   | 62 => (-1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic2 + -3 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_ic2 + -1 <= 0)%Z
   | 63 => (-1 * s V_find_best_colors_ic2 + -1 <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic2 + 1 <= 0)%Z
   | 64 => (1 * s V_find_best_colors_ic2 + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_ic2 + -1 <= 0)%Z
   | 65 => (-1 * s V_find_best_colors_ic2 + -1 <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic2 + 1 <= 0)%Z
   | 66 => (1 * s V_find_best_colors_ic2 + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_ic2 + -1 <= 0)%Z
   | 67 => (-1 * s V_find_best_colors_ic2 + -1 <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic2 + 1 <= 0)%Z
   | 68 => (1 * s V_find_best_colors_ic2 + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_ic2 + -1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -6 <= 0)%Z
   | 69 => (1 * s V_find_best_colors_ic1 + -6 <= 0 /\ -1 * s V_find_best_colors_ic2 + -1 <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic2 + 1 <= 0)%Z
   | 70 => (1 * s V_find_best_colors_ic2 + 1 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_ic2 + -1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -6 <= 0)%Z
   | 71 => (1 * s V_find_best_colors_ic1 + -6 <= 0 /\ -1 * s V_find_best_colors_ic2 + -1 <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic2 + 1 <= 0 /\ -1 * s V_find_best_colors_z + 1 <= 0)%Z
   | 72 => (1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ 1 * s V_find_best_colors_ic2 + -3 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_ic2 <= 0)%Z
   | 73 => (-1 * s V_find_best_colors_ic2 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic2 + -3 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0)%Z
   | 74 => (1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ 1 * s V_find_best_colors_ic2 + -3 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_ic2 <= 0)%Z
   | 75 => (-1 * s V_find_best_colors_ic2 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic2 + -3 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0)%Z
   | 76 => (1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ 1 * s V_find_best_colors_ic2 + -3 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_ic2 <= 0)%Z
   | 77 => (-1 * s V_find_best_colors_ic2 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic2 + -3 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0)%Z
   | 78 => (1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ 1 * s V_find_best_colors_ic2 + -3 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_ic2 <= 0)%Z
   | 79 => (-1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ 1 * s V_find_best_colors_ic2 + -2 <= 0 /\ -1 * s V_find_best_colors_ic2 + -1 <= 0)%Z
   | 80 => (-1 * s V_find_best_colors_ic2 + -1 <= 0 /\ 1 * s V_find_best_colors_ic2 + -2 <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors_z <= 0)%Z
   | 81 => (-1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ 1 * s V_find_best_colors_ic2 + -2 <= 0 /\ -1 * s V_find_best_colors_ic2 + -1 <= 0)%Z
   | 82 => (-1 * s V_find_best_colors_ic2 + -1 <= 0 /\ 1 * s V_find_best_colors_ic2 + -2 <= 0 /\ 1 * s V_find_best_colors_ic0 + -3 <= 0 /\ -1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors__tmp+ 1 * s V_find_best_colors_i + 1 <= 0 /\ 1 * s V_find_best_colors_ic1 + -7 <= 0 /\ -1 * s V_find_best_colors_z + 1 <= 0)%Z
   | 83 => (1 * s V_find_best_colors_i + -127 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i <= 0)%Z
   | 84 => (-1 * s V_find_best_colors_i <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_i + -127 <= 0)%Z
   | 85 => (1 * s V_find_best_colors_i + -127 <= 0 /\ -1 * s V_find_best_colors_z <= 0 /\ -1 * s V_find_best_colors_i <= 0)%Z
   | 86 => (-1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_i + -126 <= 0 /\ -1 * s V_find_best_colors_i + -1 <= 0)%Z
   | 87 => (-1 * s V_find_best_colors_i + -1 <= 0 /\ 1 * s V_find_best_colors_i + -126 <= 0 /\ -1 * s V_find_best_colors_z <= 0)%Z
   | 88 => (-1 * s V_find_best_colors_z <= 0 /\ 1 * s V_find_best_colors_i + -126 <= 0 /\ -1 * s V_find_best_colors_i + -1 <= 0)%Z
   | 89 => (-1 * s V_find_best_colors_i + -1 <= 0 /\ 1 * s V_find_best_colors_i + -126 <= 0 /\ -1 * s V_find_best_colors_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_find_best_colors (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((128 # 1) + (165 # 1) * max0(s V_find_best_colors_numcolors) <= z)%Q
   | 2 => ((128 # 1) + (165 # 1) * max0(s V_find_best_colors_numcolors)
           + max0(s V_find_best_colors_z) <= z)%Q
   | 3 => ((128 # 1) + (165 # 1) * max0(s V_find_best_colors_numcolors)
           + max0(s V_find_best_colors_z) <= z)%Q
   | 4 => ((128 # 1) + (165 # 1) * max0(s V_find_best_colors_numcolors)
           + max0(s V_find_best_colors_z) <= z)%Q
   | 5 => ((128 # 1) + (165 # 1) * max0(s V_find_best_colors_numcolors)
           + max0(s V_find_best_colors_z) <= z)%Q
   | 6 => ((128 # 1) + (165 # 1) * max0(s V_find_best_colors__tmp)
           + max0(s V_find_best_colors_z) <= z)%Q
   | 7 => (max0(1 + s V_find_best_colors_i)
           + (165 # 1) * max0(s V_find_best_colors__tmp)
           + max0(s V_find_best_colors_z) <= z)%Q
   | 8 => (max0(1 + s V_find_best_colors_i)
           + (165 # 1) * max0(s V_find_best_colors__tmp)
           + max0(s V_find_best_colors_z) <= z)%Q
   | 9 => (max0(1 + s V_find_best_colors_i)
           + (165 # 1) * max0(s V_find_best_colors__tmp)
           + max0(s V_find_best_colors_z) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_find_best_colors_i) (s V_find_best_colors_i));
      (*-1 0*) F_max0_ge_0 (s V_find_best_colors_i)]
     (max0(1 + s V_find_best_colors_i)
      + (165 # 1) * max0(s V_find_best_colors__tmp)
      + max0(s V_find_best_colors_z) <= z)%Q
   | 11 => ((165 # 1) * max0(s V_find_best_colors__tmp)
            + max0(s V_find_best_colors_z) <= z)%Q
   | 12 => ((165 # 1) * max0(s V_find_best_colors__tmp
                             - s V_find_best_colors_i)
            + max0(s V_find_best_colors_z) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_find_best_colors_z)) (F_check_ge (s V_find_best_colors_z) (0))]
     ((165 # 1) * max0(s V_find_best_colors__tmp - s V_find_best_colors_i)
      + max0(s V_find_best_colors_z) <= z)%Q
   | 14 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 15 => hints
     [(*-165 0*) F_max0_monotonic (F_check_ge (s V_find_best_colors__tmp
                                               - s V_find_best_colors_i) (-1
                                                                    + s V_find_best_colors__tmp
                                                                    - s V_find_best_colors_i));
      (*-165 0*) F_max0_ge_0 (-1 + s V_find_best_colors__tmp
                              - s V_find_best_colors_i)]
     (s V_find_best_colors_z
      + (165 # 1) * max0(s V_find_best_colors__tmp - s V_find_best_colors_i) <= z)%Q
   | 16 => (s V_find_best_colors_z <= z)%Q
   | 17 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 18 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 19 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 20 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 21 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 22 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 23 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 24 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 25 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 26 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 27 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 28 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 29 => (s V_find_best_colors_z
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 30 => (-(164 # 1) + s V_find_best_colors_z
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0)
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 31 => hints
     [(*-165 0*) F_max0_pre_decrement 1 (s V_find_best_colors__tmp
                                         - s V_find_best_colors_i) (1)]
     (-(164 # 1) + s V_find_best_colors_z
      + (41 # 1) * max0(1 + s V_find_best_colors_ic0)
      + (165 # 1) * max0(s V_find_best_colors__tmp - s V_find_best_colors_i) <= z)%Q
   | 32 => ((1 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0) <= z)%Q
   | 33 => ((1 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0) <= z)%Q
   | 34 => ((1 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0) <= z)%Q
   | 35 => ((1 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0) <= z)%Q
   | 36 => ((1 # 1) + s V_find_best_colors_z
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0)
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 37 => ((1 # 1) + s V_find_best_colors_z
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0)
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 38 => ((1 # 1) + s V_find_best_colors_z
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0)
            + (165 # 1) * max0(s V_find_best_colors__tmp
                               - s V_find_best_colors_i) <= z)%Q
   | 39 => hints
     [(*-41 0*) F_max0_monotonic (F_check_ge (1 + s V_find_best_colors_ic0) (s V_find_best_colors_ic0));
      (*-41 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_find_best_colors_ic0)) (F_check_ge (0) (0))]
     (s V_find_best_colors_z + (41 # 1) * max0(1 + s V_find_best_colors_ic0)
      + (165 # 1) * max0(s V_find_best_colors__tmp - s V_find_best_colors_i) <= z)%Q
   | 40 => ((1 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0) <= z)%Q
   | 41 => ((1 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0) <= z)%Q
   | 42 => ((1 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0) <= z)%Q
   | 43 => ((1 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0) <= z)%Q
   | 44 => (-(39 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1) <= z)%Q
   | 45 => hints
     [(*-41 0*) F_max0_pre_decrement 1 (1 + s V_find_best_colors_ic0) (1)]
     (-(39 # 1) + s V_find_best_colors_z
      + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                         - s V_find_best_colors_i)
      + (41 # 1) * max0(1 + s V_find_best_colors_ic0)
      + (5 # 1) * max0(1 + s V_find_best_colors_ic1) <= z)%Q
   | 46 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 47 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 48 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 49 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 50 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 51 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 52 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1) <= z)%Q
   | 53 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1) <= z)%Q
   | 54 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(1 + s V_find_best_colors_ic0)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1) <= z)%Q
   | 55 => hints
     [(*-5 0*) F_max0_ge_0 (1 + s V_find_best_colors_ic1)]
     ((1 # 1) + s V_find_best_colors_z
      + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                         - s V_find_best_colors_i)
      + (41 # 1) * max0(1 + s V_find_best_colors_ic0)
      + (5 # 1) * max0(1 + s V_find_best_colors_ic1) <= z)%Q
   | 56 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 57 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 58 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 59 => ((2 # 1) + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 60 => (-(3 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 61 => hints
     [(*-5 0*) F_max0_pre_decrement 1 (1 + s V_find_best_colors_ic1) (1)]
     (-(3 # 2) + (1 # 2) * s V_find_best_colors_ic2 + s V_find_best_colors_z
      + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                         - s V_find_best_colors_i)
      + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
      + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
      + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 62 => ((7 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1) <= z)%Q
   | 63 => ((7 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1) <= z)%Q
   | 64 => ((7 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1) <= z)%Q
   | 65 => ((7 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1) <= z)%Q
   | 66 => ((7 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1) <= z)%Q
   | 67 => ((7 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1) <= z)%Q
   | 68 => ((7 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 69 => ((7 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 70 => ((7 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 71 => hints
     [(*0 1*) F_max0_ge_0 (1 + s V_find_best_colors_ic2);
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                 + s V_find_best_colors_ic2) (0))) (F_max0_ge_0 (1
                                                                    + s V_find_best_colors_ic2))]
     ((5 # 2) + (1 # 2) * s V_find_best_colors_ic2 + s V_find_best_colors_z
      + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                         - s V_find_best_colors_i)
      + (5 # 1) * max0(1 + s V_find_best_colors_ic1)
      + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
      + (41 # 1) * max0(s V_find_best_colors_ic0) <= z)%Q
   | 72 => hints
     [(*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_find_best_colors_ic2) (0))) (F_max0_ge_0 (s V_find_best_colors_ic2));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                     + s V_find_best_colors_ic2)) (F_check_ge (1
                                                                    + s V_find_best_colors_ic2) (0))]
     ((7 # 2) + (1 # 2) * s V_find_best_colors_ic2 + s V_find_best_colors_z
      + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                         - s V_find_best_colors_i)
      + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
      + (41 # 1) * max0(s V_find_best_colors_ic0)
      + (5 # 1) * max0(s V_find_best_colors_ic1) <= z)%Q
   | 73 => ((4 # 1) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1)
            + (1 # 2) * max0(s V_find_best_colors_ic2) <= z)%Q
   | 74 => ((4 # 1) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1)
            + (1 # 2) * max0(s V_find_best_colors_ic2) <= z)%Q
   | 75 => ((4 # 1) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1)
            + (1 # 2) * max0(s V_find_best_colors_ic2) <= z)%Q
   | 76 => ((4 # 1) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1)
            + (1 # 2) * max0(s V_find_best_colors_ic2) <= z)%Q
   | 77 => ((4 # 1) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1)
            + (1 # 2) * max0(s V_find_best_colors_ic2) <= z)%Q
   | 78 => ((4 # 1) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1)
            + (1 # 2) * max0(s V_find_best_colors_ic2) <= z)%Q
   | 79 => ((9 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1) <= z)%Q
   | 80 => ((9 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1) <= z)%Q
   | 81 => ((9 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1) <= z)%Q
   | 82 => ((7 # 2) + (1 # 2) * s V_find_best_colors_ic2
            + s V_find_best_colors_z
            + (165 # 1) * max0(-1 + s V_find_best_colors__tmp
                               - s V_find_best_colors_i)
            + (1 # 2) * max0(1 + s V_find_best_colors_ic2)
            + (41 # 1) * max0(s V_find_best_colors_ic0)
            + (5 # 1) * max0(s V_find_best_colors_ic1) <= z)%Q
   | 83 => hints
     [(*0 1*) F_max0_pre_decrement 1 (1 + s V_find_best_colors_i) (1)]
     (max0(1 + s V_find_best_colors_i)
      + (165 # 1) * max0(s V_find_best_colors__tmp)
      + max0(s V_find_best_colors_z) <= z)%Q
   | 84 => ((1 # 1) + (165 # 1) * max0(s V_find_best_colors__tmp)
            + max0(s V_find_best_colors_i) + max0(s V_find_best_colors_z) <= z)%Q
   | 85 => ((1 # 1) + (165 # 1) * max0(s V_find_best_colors__tmp)
            + max0(s V_find_best_colors_i) + max0(s V_find_best_colors_z) <= z)%Q
   | 86 => ((1 # 1) + max0(1 + s V_find_best_colors_i)
            + (165 # 1) * max0(s V_find_best_colors__tmp)
            + max0(s V_find_best_colors_z) <= z)%Q
   | 87 => ((1 # 1) + max0(1 + s V_find_best_colors_i)
            + (165 # 1) * max0(s V_find_best_colors__tmp)
            + max0(s V_find_best_colors_z) <= z)%Q
   | 88 => ((1 # 1) + max0(1 + s V_find_best_colors_i)
            + (165 # 1) * max0(s V_find_best_colors__tmp)
            + max0(s V_find_best_colors_z) <= z)%Q
   | 89 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_find_best_colors_z) (0))) (F_max0_ge_0 (s V_find_best_colors_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_find_best_colors_z)) (F_check_ge (-1
                                                                    + s V_find_best_colors_z) (0))]
     ((1 # 1) + max0(-1 + s V_find_best_colors_z)
      + max0(1 + s V_find_best_colors_i)
      + (165 # 1) * max0(s V_find_best_colors__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_find_best_colors =>
    [mkPA Q (fun n z s => ai_find_best_colors n s /\ annot0_find_best_colors n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_find_best_colors (proc_start P_find_best_colors) s1 (proc_end P_find_best_colors) s2 ->
    (s2 V_find_best_colors_z <= (128 # 1)
                                + (165 # 1) * max0(s1 V_find_best_colors_numcolors))%Q.
Proof.
  prove_bound ipa admissible_ipa P_find_best_colors.
Qed.
