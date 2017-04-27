Require Import pasta.Pasta.

Inductive proc: Type :=
  P_find_nearby_colors.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_find_nearby_colors_z := 1%positive.
Notation V_find_nearby_colors__tmp := 2%positive.
Notation V_find_nearby_colors__tmp1 := 3%positive.
Notation V_find_nearby_colors__tmp2 := 4%positive.
Notation V_find_nearby_colors_centerc0 := 5%positive.
Notation V_find_nearby_colors_centerc1 := 6%positive.
Notation V_find_nearby_colors_centerc2 := 7%positive.
Notation V_find_nearby_colors_cinfo_dref_off148 := 8%positive.
Notation V_find_nearby_colors_i := 9%positive.
Notation V_find_nearby_colors_max_dist := 10%positive.
Notation V_find_nearby_colors_maxc0 := 11%positive.
Notation V_find_nearby_colors_maxc1 := 12%positive.
Notation V_find_nearby_colors_maxc2 := 13%positive.
Notation V_find_nearby_colors_min_dist := 14%positive.
Notation V_find_nearby_colors_minmaxdist := 15%positive.
Notation V_find_nearby_colors_ncolors := 16%positive.
Notation V_find_nearby_colors_numcolors := 17%positive.
Notation V_find_nearby_colors_tdist := 18%positive.
Notation V_find_nearby_colors_x := 19%positive.
Notation V_find_nearby_colors_cinfo := 20%positive.
Notation V_find_nearby_colors_colorlist := 21%positive.
Notation V_find_nearby_colors_minc0 := 22%positive.
Notation V_find_nearby_colors_minc1 := 23%positive.
Notation V_find_nearby_colors_minc2 := 24%positive.
Definition Pedges_find_nearby_colors: list (edge proc) :=
  (EA 1 (AAssign V_find_nearby_colors_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_find_nearby_colors__tmp (Some (EVar V_find_nearby_colors_minc0))) 3)::
  (EA 3 (AAssign V_find_nearby_colors__tmp1
  (Some (EVar V_find_nearby_colors_minc1))) 4)::(EA 4 (AAssign
  V_find_nearby_colors__tmp2 (Some (EVar V_find_nearby_colors_minc2))) 5)::
  (EA 5 (AAssign V_find_nearby_colors_numcolors
  (Some (EVar V_find_nearby_colors_cinfo_dref_off148))) 6)::(EA 6 (AAssign
  V_find_nearby_colors_maxc0 (Some (EAdd (EVar V_find_nearby_colors__tmp)
  (ENum (24))))) 7)::(EA 7 (AAssign V_find_nearby_colors_centerc0 None) 8)::
  (EA 8 (AAssign V_find_nearby_colors_maxc1
  (Some (EAdd (EVar V_find_nearby_colors__tmp1) (ENum (28))))) 9)::
  (EA 9 (AAssign V_find_nearby_colors_centerc1 None) 10)::(EA 10 (AAssign
  V_find_nearby_colors_maxc2 (Some (EAdd (EVar V_find_nearby_colors__tmp2)
  (ENum (24))))) 11)::(EA 11 (AAssign V_find_nearby_colors_centerc2
  None) 12)::(EA 12 (AAssign V_find_nearby_colors_minmaxdist None) 13)::
  (EA 13 (AAssign V_find_nearby_colors_i (Some (ENum (0)))) 14)::
  (EA 14 ANone 15)::(EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_i) s) <
  (eval (EVar V_find_nearby_colors_numcolors) s))%Z)) 35)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_i) s) >=
  (eval (EVar V_find_nearby_colors_numcolors) s))%Z)) 17)::
  (EA 17 AWeaken 18)::(EA 18 (AAssign V_find_nearby_colors_ncolors
  (Some (ENum (0)))) 19)::(EA 19 (AAssign V_find_nearby_colors_i
  (Some (ENum (0)))) 20)::(EA 20 ANone 21)::(EA 21 AWeaken 22)::
  (EA 22 (AGuard (fun s => ((eval (EVar V_find_nearby_colors_i) s) <
  (eval (EVar V_find_nearby_colors_numcolors) s))%Z)) 25)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_i) s) >=
  (eval (EVar V_find_nearby_colors_numcolors) s))%Z)) 23)::
  (EA 23 AWeaken 24)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::
  (EA 26 ANone 29)::(EA 27 (AAssign V_find_nearby_colors_ncolors
  (Some (EAdd (EVar V_find_nearby_colors_ncolors) (ENum (1))))) 28)::
  (EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign V_find_nearby_colors_i
  (Some (EAdd (EVar V_find_nearby_colors_i) (ENum (1))))) 31)::
  (EA 31 ANone 32)::(EA 32 ANone 33)::(EA 33 (AAssign V_find_nearby_colors_z
  (Some (EAdd (ENum (1)) (EVar V_find_nearby_colors_z)))) 34)::
  (EA 34 AWeaken 22)::(EA 35 AWeaken 36)::(EA 36 (AAssign
  V_find_nearby_colors_x None) 37)::(EA 37 AWeaken 38)::(EA 38 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) <
  (eval (EVar V_find_nearby_colors__tmp) s))%Z)) 61)::(EA 38 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) >=
  (eval (EVar V_find_nearby_colors__tmp) s))%Z)) 39)::(EA 39 AWeaken 40)::
  (EA 40 (AGuard (fun s => ((eval (EVar V_find_nearby_colors_x) s) >
  (eval (EVar V_find_nearby_colors_maxc0) s))%Z)) 54)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) <=
  (eval (EVar V_find_nearby_colors_maxc0) s))%Z)) 41)::(EA 41 AWeaken 42)::
  (EA 42 (AAssign V_find_nearby_colors_min_dist (Some (ENum (0)))) 43)::
  (EA 43 AWeaken 44)::(EA 44 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) <=
  (eval (EVar V_find_nearby_colors_centerc0) s))%Z)) 49)::(EA 44 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) >
  (eval (EVar V_find_nearby_colors_centerc0) s))%Z)) 45)::
  (EA 45 AWeaken 46)::(EA 46 (AAssign V_find_nearby_colors_tdist
  (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors__tmp)) (ENum (2))))) 47)::(EA 47 (AAssign
  V_find_nearby_colors_max_dist (Some (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist)))) 48)::(EA 48 ANone 53)::
  (EA 49 AWeaken 50)::(EA 50 (AAssign V_find_nearby_colors_tdist
  (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors_maxc0)) (ENum (2))))) 51)::(EA 51 (AAssign
  V_find_nearby_colors_max_dist (Some (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist)))) 52)::(EA 52 ANone 53)::
  (EA 53 ANone 60)::(EA 54 AWeaken 55)::(EA 55 (AAssign
  V_find_nearby_colors_tdist (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors_maxc0)) (ENum (2))))) 56)::(EA 56 (AAssign
  V_find_nearby_colors_min_dist (Some (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist)))) 57)::(EA 57 (AAssign
  V_find_nearby_colors_tdist (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors__tmp)) (ENum (2))))) 58)::(EA 58 (AAssign
  V_find_nearby_colors_max_dist (Some (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist)))) 59)::(EA 59 ANone 60)::
  (EA 60 ANone 67)::(EA 61 AWeaken 62)::(EA 62 (AAssign
  V_find_nearby_colors_tdist (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors__tmp)) (ENum (2))))) 63)::(EA 63 (AAssign
  V_find_nearby_colors_min_dist (Some (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist)))) 64)::(EA 64 (AAssign
  V_find_nearby_colors_tdist (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors_maxc0)) (ENum (2))))) 65)::(EA 65 (AAssign
  V_find_nearby_colors_max_dist (Some (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist)))) 66)::(EA 66 ANone 67)::(EA 67 (AAssign
  V_find_nearby_colors_x None) 68)::(EA 68 AWeaken 69)::(EA 69 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) <
  (eval (EVar V_find_nearby_colors__tmp1) s))%Z)) 90)::(EA 69 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) >=
  (eval (EVar V_find_nearby_colors__tmp1) s))%Z)) 70)::(EA 70 AWeaken 71)::
  (EA 71 (AGuard (fun s => ((eval (EVar V_find_nearby_colors_x) s) >
  (eval (EVar V_find_nearby_colors_maxc1) s))%Z)) 83)::(EA 71 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) <=
  (eval (EVar V_find_nearby_colors_maxc1) s))%Z)) 72)::(EA 72 AWeaken 73)::
  (EA 73 (AGuard (fun s => ((eval (EVar V_find_nearby_colors_x) s) <=
  (eval (EVar V_find_nearby_colors_centerc1) s))%Z)) 78)::(EA 73 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) >
  (eval (EVar V_find_nearby_colors_centerc1) s))%Z)) 74)::
  (EA 74 AWeaken 75)::(EA 75 (AAssign V_find_nearby_colors_tdist
  (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors__tmp1)) (ENum (3))))) 76)::(EA 76 (AAssign
  V_find_nearby_colors_max_dist
  (Some (EAdd (EVar V_find_nearby_colors_max_dist)
  (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist))))) 77)::(EA 77 ANone 82)::
  (EA 78 AWeaken 79)::(EA 79 (AAssign V_find_nearby_colors_tdist
  (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors_maxc1)) (ENum (3))))) 80)::(EA 80 (AAssign
  V_find_nearby_colors_max_dist
  (Some (EAdd (EVar V_find_nearby_colors_max_dist)
  (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist))))) 81)::(EA 81 ANone 82)::
  (EA 82 ANone 89)::(EA 83 AWeaken 84)::(EA 84 (AAssign
  V_find_nearby_colors_tdist (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors_maxc1)) (ENum (3))))) 85)::(EA 85 (AAssign
  V_find_nearby_colors_min_dist
  (Some (EAdd (EVar V_find_nearby_colors_min_dist)
  (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist))))) 86)::(EA 86 (AAssign
  V_find_nearby_colors_tdist (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors__tmp1)) (ENum (3))))) 87)::(EA 87 (AAssign
  V_find_nearby_colors_max_dist
  (Some (EAdd (EVar V_find_nearby_colors_max_dist)
  (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist))))) 88)::(EA 88 ANone 89)::
  (EA 89 ANone 96)::(EA 90 AWeaken 91)::(EA 91 (AAssign
  V_find_nearby_colors_tdist (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors__tmp1)) (ENum (3))))) 92)::(EA 92 (AAssign
  V_find_nearby_colors_min_dist
  (Some (EAdd (EVar V_find_nearby_colors_min_dist)
  (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist))))) 93)::(EA 93 (AAssign
  V_find_nearby_colors_tdist (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors_maxc1)) (ENum (3))))) 94)::(EA 94 (AAssign
  V_find_nearby_colors_max_dist
  (Some (EAdd (EVar V_find_nearby_colors_max_dist)
  (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist))))) 95)::(EA 95 ANone 96)::
  (EA 96 (AAssign V_find_nearby_colors_x None) 97)::(EA 97 AWeaken 98)::
  (EA 98 (AGuard (fun s => ((eval (EVar V_find_nearby_colors_x) s) <
  (eval (EVar V_find_nearby_colors__tmp2) s))%Z)) 120)::(EA 98 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) >=
  (eval (EVar V_find_nearby_colors__tmp2) s))%Z)) 99)::(EA 99 AWeaken 100)::
  (EA 100 (AGuard (fun s => ((eval (EVar V_find_nearby_colors_x) s) >
  (eval (EVar V_find_nearby_colors_maxc2) s))%Z)) 112)::(EA 100 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) <=
  (eval (EVar V_find_nearby_colors_maxc2) s))%Z)) 101)::
  (EA 101 AWeaken 102)::(EA 102 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) <=
  (eval (EVar V_find_nearby_colors_centerc2) s))%Z)) 107)::(EA 102 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_x) s) >
  (eval (EVar V_find_nearby_colors_centerc2) s))%Z)) 103)::
  (EA 103 AWeaken 104)::(EA 104 (AAssign V_find_nearby_colors_tdist
  (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors__tmp2)) (ENum (1))))) 105)::(EA 105 (AAssign
  V_find_nearby_colors_max_dist
  (Some (EAdd (EVar V_find_nearby_colors_max_dist)
  (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist))))) 106)::(EA 106 ANone 111)::
  (EA 107 AWeaken 108)::(EA 108 (AAssign V_find_nearby_colors_tdist
  (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors_maxc2)) (ENum (1))))) 109)::(EA 109 (AAssign
  V_find_nearby_colors_max_dist
  (Some (EAdd (EVar V_find_nearby_colors_max_dist)
  (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist))))) 110)::(EA 110 ANone 111)::
  (EA 111 ANone 118)::(EA 112 AWeaken 113)::(EA 113 (AAssign
  V_find_nearby_colors_tdist (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors_maxc2)) (ENum (1))))) 114)::(EA 114 (AAssign
  V_find_nearby_colors_min_dist
  (Some (EAdd (EVar V_find_nearby_colors_min_dist)
  (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist))))) 115)::(EA 115 (AAssign
  V_find_nearby_colors_tdist (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors__tmp2)) (ENum (1))))) 116)::(EA 116 (AAssign
  V_find_nearby_colors_max_dist
  (Some (EAdd (EVar V_find_nearby_colors_max_dist)
  (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist))))) 117)::(EA 117 ANone 118)::
  (EA 118 ANone 119)::(EA 119 AWeaken 127)::(EA 120 AWeaken 121)::
  (EA 121 (AAssign V_find_nearby_colors_tdist
  (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors__tmp2)) (ENum (1))))) 122)::(EA 122 (AAssign
  V_find_nearby_colors_min_dist
  (Some (EAdd (EVar V_find_nearby_colors_min_dist)
  (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist))))) 123)::(EA 123 (AAssign
  V_find_nearby_colors_tdist (Some (EMul (ESub (EVar V_find_nearby_colors_x)
  (EVar V_find_nearby_colors_maxc2)) (ENum (1))))) 124)::(EA 124 (AAssign
  V_find_nearby_colors_max_dist
  (Some (EAdd (EVar V_find_nearby_colors_max_dist)
  (EMul (EVar V_find_nearby_colors_tdist)
  (EVar V_find_nearby_colors_tdist))))) 125)::(EA 125 ANone 126)::
  (EA 126 AWeaken 127)::(EA 127 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_max_dist) s) <
  (eval (EVar V_find_nearby_colors_minmaxdist) s))%Z)) 129)::(EA 127 (AGuard
  (fun s => ((eval (EVar V_find_nearby_colors_max_dist) s) >=
  (eval (EVar V_find_nearby_colors_minmaxdist) s))%Z)) 128)::
  (EA 128 AWeaken 132)::(EA 129 AWeaken 130)::(EA 130 (AAssign
  V_find_nearby_colors_minmaxdist
  (Some (EVar V_find_nearby_colors_max_dist))) 131)::(EA 131 ANone 132)::
  (EA 132 ANone 133)::(EA 133 (AAssign V_find_nearby_colors_i
  (Some (EAdd (EVar V_find_nearby_colors_i) (ENum (1))))) 134)::
  (EA 134 ANone 135)::(EA 135 ANone 136)::(EA 136 (AAssign
  V_find_nearby_colors_z (Some (EAdd (ENum (1))
  (EVar V_find_nearby_colors_z)))) 137)::(EA 137 AWeaken 16)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_find_nearby_colors => Pedges_find_nearby_colors
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_find_nearby_colors => 24
     end)%positive;
  var_global := var_global
}.

Definition ai_find_nearby_colors (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_z <= 0)%Z
   | 3 => (-1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_z <= 0)%Z
   | 4 => (1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_z <= 0)%Z
   | 5 => (-1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_z <= 0)%Z
   | 6 => (1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_z <= 0)%Z
   | 7 => (-1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_z <= 0)%Z
   | 8 => (1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_z <= 0)%Z
   | 9 => (-1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_z <= 0)%Z
   | 10 => (1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_z <= 0)%Z
   | 11 => (-1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_z <= 0)%Z
   | 12 => (1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_z <= 0)%Z
   | 13 => (-1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_z <= 0)%Z
   | 14 => (1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 15 => (-1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_z <= 0)%Z
   | 16 => (-1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 17 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i+ 1 * s V_find_nearby_colors_numcolors <= 0)%Z
   | 18 => (-1 * s V_find_nearby_colors_i+ 1 * s V_find_nearby_colors_numcolors <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 19 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i+ 1 * s V_find_nearby_colors_numcolors <= 0 /\ 1 * s V_find_nearby_colors_ncolors <= 0 /\ -1 * s V_find_nearby_colors_ncolors <= 0)%Z
   | 20 => (-1 * s V_find_nearby_colors_ncolors <= 0 /\ 1 * s V_find_nearby_colors_ncolors <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 21 => (-1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_ncolors <= 0 /\ -1 * s V_find_nearby_colors_ncolors <= 0)%Z
   | 22 => (-1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_ncolors <= 0)%Z
   | 23 => (-1 * s V_find_nearby_colors_ncolors <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i+ 1 * s V_find_nearby_colors_numcolors <= 0)%Z
   | 24 => (-1 * s V_find_nearby_colors_i+ 1 * s V_find_nearby_colors_numcolors <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_ncolors <= 0)%Z
   | 25 => (-1 * s V_find_nearby_colors_ncolors <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 26 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_ncolors <= 0)%Z
   | 27 => (-1 * s V_find_nearby_colors_ncolors <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 28 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_ncolors + 1 <= 0)%Z
   | 29 => (-1 * s V_find_nearby_colors_ncolors <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 30 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_ncolors <= 0)%Z
   | 31 => (-1 * s V_find_nearby_colors_ncolors <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors <= 0 /\ -1 * s V_find_nearby_colors_i + 1 <= 0)%Z
   | 32 => (-1 * s V_find_nearby_colors_i + 1 <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_ncolors <= 0)%Z
   | 33 => (-1 * s V_find_nearby_colors_ncolors <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors <= 0 /\ -1 * s V_find_nearby_colors_i + 1 <= 0)%Z
   | 34 => (-1 * s V_find_nearby_colors_i + 1 <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors <= 0 /\ -1 * s V_find_nearby_colors_ncolors <= 0 /\ -1 * s V_find_nearby_colors_z + 1 <= 0)%Z
   | 35 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 36 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 37 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 38 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 39 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0)%Z
   | 40 => (1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 41 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0)%Z
   | 42 => (-1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 43 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_min_dist <= 0)%Z
   | 44 => (-1 * s V_find_nearby_colors_min_dist <= 0 /\ 1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 45 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_min_dist <= 0 /\ 1 * s V_find_nearby_colors_centerc0+ -1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 46 => (1 * s V_find_nearby_colors_centerc0+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ -1 * s V_find_nearby_colors_min_dist <= 0 /\ 1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 47 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_min_dist <= 0 /\ 1 * s V_find_nearby_colors_centerc0+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 2 * s V_find_nearby_colors_centerc0+ -1 * s V_find_nearby_colors_tdist+ -2 * s V_find_nearby_colors_x + 2 <= 0 /\ 2 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_tdist+ -2 * s V_find_nearby_colors_x <= 0)%Z
   | 48 => (2 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_tdist+ -2 * s V_find_nearby_colors_x <= 0 /\ 2 * s V_find_nearby_colors_centerc0+ -1 * s V_find_nearby_colors_tdist+ -2 * s V_find_nearby_colors_x + 2 <= 0 /\ 1 * s V_find_nearby_colors_centerc0+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ -1 * s V_find_nearby_colors_min_dist <= 0 /\ 1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 49 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_centerc0+ 1 * s V_find_nearby_colors_x <= 0)%Z
   | 50 => (-1 * s V_find_nearby_colors_centerc0+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_min_dist <= 0 /\ 1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 51 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_centerc0+ 1 * s V_find_nearby_colors_x <= 0 /\ -2 * s V_find_nearby_colors_centerc0+ 1 * s V_find_nearby_colors_tdist+ 2 * s V_find_nearby_colors_x <= 0 /\ -2 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_tdist+ 2 * s V_find_nearby_colors_x <= 0)%Z
   | 52 => (-2 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_tdist+ 2 * s V_find_nearby_colors_x <= 0 /\ -2 * s V_find_nearby_colors_centerc0+ 1 * s V_find_nearby_colors_tdist+ 2 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_centerc0+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_min_dist <= 0 /\ 1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 53 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc0+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_min_dist <= 0 /\ -1 * s V_find_nearby_colors_min_dist <= 0)%Z
   | 54 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_maxc0+ -1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 55 => (1 * s V_find_nearby_colors_maxc0+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 56 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_maxc0+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 2 * s V_find_nearby_colors_maxc0+ -1 * s V_find_nearby_colors_tdist+ -2 * s V_find_nearby_colors_x + 4 <= 0 /\ 2 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_tdist+ -2 * s V_find_nearby_colors_x + 2 <= 0)%Z
   | 57 => (2 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_tdist+ -2 * s V_find_nearby_colors_x + 2 <= 0 /\ 2 * s V_find_nearby_colors_maxc0+ -1 * s V_find_nearby_colors_tdist+ -2 * s V_find_nearby_colors_x + 4 <= 0 /\ 1 * s V_find_nearby_colors_maxc0+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 58 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_maxc0+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 2 * s V_find_nearby_colors_maxc0+ -1 * s V_find_nearby_colors_tdist+ -2 * s V_find_nearby_colors_x + 2 <= 0 /\ 2 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_tdist+ -2 * s V_find_nearby_colors_x <= 0)%Z
   | 59 => (2 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_tdist+ -2 * s V_find_nearby_colors_x <= 0 /\ 2 * s V_find_nearby_colors_maxc0+ -1 * s V_find_nearby_colors_tdist+ -2 * s V_find_nearby_colors_x + 2 <= 0 /\ 1 * s V_find_nearby_colors_maxc0+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 60 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp+ -1 * s V_find_nearby_colors_x <= 0)%Z
   | 61 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors__tmp+ 1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 62 => (-1 * s V_find_nearby_colors__tmp+ 1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 63 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors__tmp+ 1 * s V_find_nearby_colors_x + 1 <= 0 /\ -2 * s V_find_nearby_colors__tmp+ 1 * s V_find_nearby_colors_tdist+ 2 * s V_find_nearby_colors_x + 4 <= 0)%Z
   | 64 => (-2 * s V_find_nearby_colors__tmp+ 1 * s V_find_nearby_colors_tdist+ 2 * s V_find_nearby_colors_x + 4 <= 0 /\ -1 * s V_find_nearby_colors__tmp+ 1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 65 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors__tmp+ 1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 66 => (-1 * s V_find_nearby_colors__tmp+ 1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 67 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 68 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 69 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 70 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0)%Z
   | 71 => (1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 72 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_x <= 0)%Z
   | 73 => (-1 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 74 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_centerc1+ -1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 75 => (1 * s V_find_nearby_colors_centerc1+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ -1 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 76 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_centerc1+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 3 * s V_find_nearby_colors_centerc1+ -1 * s V_find_nearby_colors_tdist+ -3 * s V_find_nearby_colors_x + 3 <= 0 /\ 3 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_tdist+ -3 * s V_find_nearby_colors_x <= 0)%Z
   | 77 => (3 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_tdist+ -3 * s V_find_nearby_colors_x <= 0 /\ 3 * s V_find_nearby_colors_centerc1+ -1 * s V_find_nearby_colors_tdist+ -3 * s V_find_nearby_colors_x + 3 <= 0 /\ 1 * s V_find_nearby_colors_centerc1+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ -1 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 78 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_centerc1+ 1 * s V_find_nearby_colors_x <= 0)%Z
   | 79 => (-1 * s V_find_nearby_colors_centerc1+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 80 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_centerc1+ 1 * s V_find_nearby_colors_x <= 0 /\ -3 * s V_find_nearby_colors_centerc1+ 1 * s V_find_nearby_colors_tdist+ 3 * s V_find_nearby_colors_x <= 0 /\ -3 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_tdist+ 3 * s V_find_nearby_colors_x <= 0)%Z
   | 81 => (-3 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_tdist+ 3 * s V_find_nearby_colors_x <= 0 /\ -3 * s V_find_nearby_colors_centerc1+ 1 * s V_find_nearby_colors_tdist+ 3 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_centerc1+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 82 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc1+ 1 * s V_find_nearby_colors_x <= 0)%Z
   | 83 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_maxc1+ -1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 84 => (1 * s V_find_nearby_colors_maxc1+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 85 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_maxc1+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 3 * s V_find_nearby_colors_maxc1+ -1 * s V_find_nearby_colors_tdist+ -3 * s V_find_nearby_colors_x + 6 <= 0 /\ 3 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_tdist+ -3 * s V_find_nearby_colors_x + 3 <= 0)%Z
   | 86 => (3 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_tdist+ -3 * s V_find_nearby_colors_x + 3 <= 0 /\ 3 * s V_find_nearby_colors_maxc1+ -1 * s V_find_nearby_colors_tdist+ -3 * s V_find_nearby_colors_x + 6 <= 0 /\ 1 * s V_find_nearby_colors_maxc1+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 87 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_maxc1+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 3 * s V_find_nearby_colors_maxc1+ -1 * s V_find_nearby_colors_tdist+ -3 * s V_find_nearby_colors_x + 3 <= 0 /\ 3 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_tdist+ -3 * s V_find_nearby_colors_x <= 0)%Z
   | 88 => (3 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_tdist+ -3 * s V_find_nearby_colors_x <= 0 /\ 3 * s V_find_nearby_colors_maxc1+ -1 * s V_find_nearby_colors_tdist+ -3 * s V_find_nearby_colors_x + 3 <= 0 /\ 1 * s V_find_nearby_colors_maxc1+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 89 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ 1 * s V_find_nearby_colors__tmp1+ -1 * s V_find_nearby_colors_x <= 0)%Z
   | 90 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors__tmp1+ 1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 91 => (-1 * s V_find_nearby_colors__tmp1+ 1 * s V_find_nearby_colors_x + 1 <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 92 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors__tmp1+ 1 * s V_find_nearby_colors_x + 1 <= 0 /\ -3 * s V_find_nearby_colors__tmp1+ 1 * s V_find_nearby_colors_tdist+ 3 * s V_find_nearby_colors_x + 6 <= 0)%Z
   | 93 => (-3 * s V_find_nearby_colors__tmp1+ 1 * s V_find_nearby_colors_tdist+ 3 * s V_find_nearby_colors_x + 6 <= 0 /\ -1 * s V_find_nearby_colors__tmp1+ 1 * s V_find_nearby_colors_x + 1 <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 94 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors__tmp1+ 1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 95 => (-1 * s V_find_nearby_colors__tmp1+ 1 * s V_find_nearby_colors_x + 1 <= 0 /\ -1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 96 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 97 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 98 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 99 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0)%Z
   | 100 => (1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 101 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_x <= 0)%Z
   | 102 => (-1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 103 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_centerc2+ -1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 104 => (1 * s V_find_nearby_colors_centerc2+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ -1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 105 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_centerc2+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors_centerc2+ -1 * s V_find_nearby_colors_tdist+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_tdist+ -1 * s V_find_nearby_colors_x <= 0)%Z
   | 106 => (1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_tdist+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_centerc2+ -1 * s V_find_nearby_colors_tdist+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors_centerc2+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ -1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 107 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_centerc2+ 1 * s V_find_nearby_colors_x <= 0)%Z
   | 108 => (-1 * s V_find_nearby_colors_centerc2+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 109 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_centerc2+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_centerc2+ 1 * s V_find_nearby_colors_tdist+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_tdist+ 1 * s V_find_nearby_colors_x <= 0)%Z
   | 110 => (-1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_tdist+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_centerc2+ 1 * s V_find_nearby_colors_tdist+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_centerc2+ 1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 111 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ -1 * s V_find_nearby_colors_maxc2+ 1 * s V_find_nearby_colors_x <= 0)%Z
   | 112 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_maxc2+ -1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 113 => (1 * s V_find_nearby_colors_maxc2+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 114 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_maxc2+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors_maxc2+ -1 * s V_find_nearby_colors_tdist+ -1 * s V_find_nearby_colors_x + 2 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_tdist+ -1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 115 => (1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_tdist+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors_maxc2+ -1 * s V_find_nearby_colors_tdist+ -1 * s V_find_nearby_colors_x + 2 <= 0 /\ 1 * s V_find_nearby_colors_maxc2+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 116 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_maxc2+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors_maxc2+ -1 * s V_find_nearby_colors_tdist+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_tdist+ -1 * s V_find_nearby_colors_x <= 0)%Z
   | 117 => (1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_tdist+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_maxc2+ -1 * s V_find_nearby_colors_tdist+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors_maxc2+ -1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 118 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0)%Z
   | 119 => (1 * s V_find_nearby_colors__tmp2+ -1 * s V_find_nearby_colors_x <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 120 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors__tmp2+ 1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 121 => (-1 * s V_find_nearby_colors__tmp2+ 1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 122 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors__tmp2+ 1 * s V_find_nearby_colors_x + 1 <= 0 /\ -1 * s V_find_nearby_colors__tmp2+ 1 * s V_find_nearby_colors_tdist+ 1 * s V_find_nearby_colors_x + 2 <= 0)%Z
   | 123 => (-1 * s V_find_nearby_colors__tmp2+ 1 * s V_find_nearby_colors_tdist+ 1 * s V_find_nearby_colors_x + 2 <= 0 /\ -1 * s V_find_nearby_colors__tmp2+ 1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 124 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors__tmp2+ 1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 125 => (-1 * s V_find_nearby_colors__tmp2+ 1 * s V_find_nearby_colors_x + 1 <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 126 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors__tmp2+ 1 * s V_find_nearby_colors_x + 1 <= 0)%Z
   | 127 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 128 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_max_dist+ 1 * s V_find_nearby_colors_minmaxdist <= 0)%Z
   | 129 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ 1 * s V_find_nearby_colors_max_dist+ -1 * s V_find_nearby_colors_minmaxdist + 1 <= 0)%Z
   | 130 => (1 * s V_find_nearby_colors_max_dist+ -1 * s V_find_nearby_colors_minmaxdist + 1 <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 131 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 132 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i <= 0)%Z
   | 133 => (-1 * s V_find_nearby_colors_i <= 0 /\ -1 * s V_find_nearby_colors_z <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors + 1 <= 0)%Z
   | 134 => (-1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i + 1 <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors <= 0)%Z
   | 135 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors <= 0 /\ -1 * s V_find_nearby_colors_i + 1 <= 0 /\ -1 * s V_find_nearby_colors_z <= 0)%Z
   | 136 => (-1 * s V_find_nearby_colors_z <= 0 /\ -1 * s V_find_nearby_colors_i + 1 <= 0 /\ 1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors <= 0)%Z
   | 137 => (1 * s V_find_nearby_colors_i+ -1 * s V_find_nearby_colors_numcolors <= 0 /\ -1 * s V_find_nearby_colors_i + 1 <= 0 /\ -1 * s V_find_nearby_colors_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_find_nearby_colors (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((2 # 1) * max0(s V_find_nearby_colors_cinfo_dref_off148) <= z)%Q
   | 2 => ((2 # 1) * max0(s V_find_nearby_colors_cinfo_dref_off148)
           + max0(s V_find_nearby_colors_z) <= z)%Q
   | 3 => ((2 # 1) * max0(s V_find_nearby_colors_cinfo_dref_off148)
           + max0(s V_find_nearby_colors_z) <= z)%Q
   | 4 => ((2 # 1) * max0(s V_find_nearby_colors_cinfo_dref_off148)
           + max0(s V_find_nearby_colors_z) <= z)%Q
   | 5 => ((2 # 1) * max0(s V_find_nearby_colors_cinfo_dref_off148)
           + max0(s V_find_nearby_colors_z) <= z)%Q
   | 6 => ((2 # 1) * max0(s V_find_nearby_colors_numcolors)
           + max0(s V_find_nearby_colors_z) <= z)%Q
   | 7 => ((2 # 1) * max0(s V_find_nearby_colors_numcolors)
           + max0(s V_find_nearby_colors_z) <= z)%Q
   | 8 => ((2 # 1) * max0(s V_find_nearby_colors_numcolors)
           + max0(s V_find_nearby_colors_z) <= z)%Q
   | 9 => ((2 # 1) * max0(s V_find_nearby_colors_numcolors)
           + max0(s V_find_nearby_colors_z) <= z)%Q
   | 10 => ((2 # 1) * max0(s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_z) <= z)%Q
   | 11 => ((2 # 1) * max0(s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_z) <= z)%Q
   | 12 => ((2 # 1) * max0(s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_z) <= z)%Q
   | 13 => ((2 # 1) * max0(s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_z) <= z)%Q
   | 14 => (max0(-s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_z) <= z)%Q
   | 15 => (max0(-s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_z) <= z)%Q
   | 16 => (max0(-s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_z) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_find_nearby_colors_i
                                             + s V_find_nearby_colors_numcolors) (-1
                                                                    - s V_find_nearby_colors_i
                                                                    + s V_find_nearby_colors_numcolors));
      (*-1 0*) F_max0_ge_0 (-1 - s V_find_nearby_colors_i
                            + s V_find_nearby_colors_numcolors)]
     (max0(-s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors)
      + max0(s V_find_nearby_colors_numcolors)
      + max0(s V_find_nearby_colors_z) <= z)%Q
   | 18 => (max0(s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_z) <= z)%Q
   | 19 => (max0(s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_z) <= z)%Q
   | 20 => (max0(-s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_z) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_find_nearby_colors_z)) (F_check_ge (s V_find_nearby_colors_z) (0))]
     (max0(-s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors)
      + max0(s V_find_nearby_colors_z) <= z)%Q
   | 22 => (s V_find_nearby_colors_z
            + max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_find_nearby_colors_i
                                             + s V_find_nearby_colors_numcolors) (-1
                                                                    - s V_find_nearby_colors_i
                                                                    + s V_find_nearby_colors_numcolors));
      (*-1 0*) F_max0_ge_0 (-1 - s V_find_nearby_colors_i
                            + s V_find_nearby_colors_numcolors)]
     (s V_find_nearby_colors_z
      + max0(-s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors) <= z)%Q
   | 24 => (s V_find_nearby_colors_z <= z)%Q
   | 25 => hints
     [(*0 1*) F_max0_pre_decrement 1 (-s V_find_nearby_colors_i
                                      + s V_find_nearby_colors_numcolors) (1)]
     (s V_find_nearby_colors_z
      + max0(-s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors) <= z)%Q
   | 26 => ((1 # 1) + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 27 => ((1 # 1) + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 28 => ((1 # 1) + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 29 => ((1 # 1) + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 30 => ((1 # 1) + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 31 => ((1 # 1) + s V_find_nearby_colors_z
            + max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 32 => ((1 # 1) + s V_find_nearby_colors_z
            + max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 33 => ((1 # 1) + s V_find_nearby_colors_z
            + max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 34 => (s V_find_nearby_colors_z
            + max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 35 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_find_nearby_colors_numcolors)) (F_check_ge (s V_find_nearby_colors_numcolors) (0))]
     (max0(-s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors)
      + max0(s V_find_nearby_colors_numcolors)
      + max0(s V_find_nearby_colors_z) <= z)%Q
   | 36 => (s V_find_nearby_colors_numcolors
            + max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            + max0(s V_find_nearby_colors_z) <= z)%Q
   | 37 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_find_nearby_colors_z)) (F_check_ge (s V_find_nearby_colors_z) (0));
      (*-8.78121e-12 1*) F_binom_monotonic 1 (F_max0_ge_arg (-s V_find_nearby_colors_i
                                                             + s V_find_nearby_colors_numcolors)) (F_check_ge (-
                                                                    s V_find_nearby_colors_i
                                                                    + s V_find_nearby_colors_numcolors) (0))]
     (s V_find_nearby_colors_numcolors
      + max0(-s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors)
      + max0(s V_find_nearby_colors_z) <= z)%Q
   | 38 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 39 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 40 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 41 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 42 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 43 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 44 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 45 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 46 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 47 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 48 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 49 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 50 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 51 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 52 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 53 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 54 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 55 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 56 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 57 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 58 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 59 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 60 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 61 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 62 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 63 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 64 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 65 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 66 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 67 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 68 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 69 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 70 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 71 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 72 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 73 => (-s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z <= z)%Q
   | 74 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_find_nearby_colors_i
                                       + s V_find_nearby_colors_numcolors) (1)]
     (-s V_find_nearby_colors_i + (2 # 1) * s V_find_nearby_colors_numcolors
      + s V_find_nearby_colors_z <= z)%Q
   | 75 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 76 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 77 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 78 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_find_nearby_colors_i
                                       + s V_find_nearby_colors_numcolors) (1)]
     (-s V_find_nearby_colors_i + (2 # 1) * s V_find_nearby_colors_numcolors
      + s V_find_nearby_colors_z <= z)%Q
   | 79 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 80 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 81 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 82 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 83 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_find_nearby_colors_i
                                       + s V_find_nearby_colors_numcolors) (1)]
     (-s V_find_nearby_colors_i + (2 # 1) * s V_find_nearby_colors_numcolors
      + s V_find_nearby_colors_z <= z)%Q
   | 84 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 85 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 86 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 87 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 88 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 89 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 90 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_find_nearby_colors_i
                                       + s V_find_nearby_colors_numcolors) (1)]
     (-s V_find_nearby_colors_i + (2 # 1) * s V_find_nearby_colors_numcolors
      + s V_find_nearby_colors_z <= z)%Q
   | 91 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 92 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 93 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 94 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 95 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 96 => ((1 # 1) - s V_find_nearby_colors_i
            + (2 # 1) * s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors)
            - max0(-s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 97 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_find_nearby_colors_i
                                                               + s V_find_nearby_colors_numcolors) (0))) (F_max0_ge_0 (-
                                                                    s V_find_nearby_colors_i
                                                                    + s V_find_nearby_colors_numcolors))]
     ((1 # 1) - s V_find_nearby_colors_i
      + (2 # 1) * s V_find_nearby_colors_numcolors + s V_find_nearby_colors_z
      + max0(-1 - s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors)
      - max0(-s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors) <= z)%Q
   | 98 => ((1 # 1) + s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 99 => ((1 # 1) + s V_find_nearby_colors_numcolors
            + s V_find_nearby_colors_z
            + max0(-1 - s V_find_nearby_colors_i
                   + s V_find_nearby_colors_numcolors) <= z)%Q
   | 100 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 101 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 102 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 103 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 104 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 105 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 106 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 107 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 108 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 109 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 110 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 111 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 112 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 113 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 114 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 115 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 116 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 117 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 118 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 119 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 120 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 121 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 122 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 123 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 124 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 125 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 126 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 127 => ((1 # 1) + s V_find_nearby_colors_numcolors
             + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors) <= z)%Q
   | 128 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_find_nearby_colors_numcolors) (0))) (F_max0_ge_0 (s V_find_nearby_colors_numcolors))]
     ((1 # 1) + s V_find_nearby_colors_numcolors + s V_find_nearby_colors_z
      + max0(-1 - s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors) <= z)%Q
   | 129 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_find_nearby_colors_numcolors) (0))) (F_max0_ge_0 (s V_find_nearby_colors_numcolors))]
     ((1 # 1) + s V_find_nearby_colors_numcolors + s V_find_nearby_colors_z
      + max0(-1 - s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors) <= z)%Q
   | 130 => ((1 # 1) + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors)
             + max0(s V_find_nearby_colors_numcolors) <= z)%Q
   | 131 => ((1 # 1) + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors)
             + max0(s V_find_nearby_colors_numcolors) <= z)%Q
   | 132 => ((1 # 1) + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors)
             + max0(s V_find_nearby_colors_numcolors) <= z)%Q
   | 133 => ((1 # 1) + s V_find_nearby_colors_z
             + max0(-1 - s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors)
             + max0(s V_find_nearby_colors_numcolors) <= z)%Q
   | 134 => ((1 # 1) + s V_find_nearby_colors_z
             + max0(-s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors)
             + max0(s V_find_nearby_colors_numcolors) <= z)%Q
   | 135 => ((1 # 1) + s V_find_nearby_colors_z
             + max0(-s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors)
             + max0(s V_find_nearby_colors_numcolors) <= z)%Q
   | 136 => ((1 # 1) + s V_find_nearby_colors_z
             + max0(-s V_find_nearby_colors_i
                    + s V_find_nearby_colors_numcolors)
             + max0(s V_find_nearby_colors_numcolors) <= z)%Q
   | 137 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_find_nearby_colors_z) (0))) (F_max0_ge_0 (s V_find_nearby_colors_z))]
     (s V_find_nearby_colors_z
      + max0(-s V_find_nearby_colors_i + s V_find_nearby_colors_numcolors)
      + max0(s V_find_nearby_colors_numcolors) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_find_nearby_colors =>
    [mkPA Q (fun n z s => ai_find_nearby_colors n s /\ annot0_find_nearby_colors n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_find_nearby_colors (proc_start P_find_nearby_colors) s1 (proc_end P_find_nearby_colors) s2 ->
    (s2 V_find_nearby_colors_z <= (2 # 1) * max0(s1 V_find_nearby_colors_cinfo_dref_off148))%Q.
Proof.
  prove_bound ipa admissible_ipa P_find_nearby_colors.
Qed.
