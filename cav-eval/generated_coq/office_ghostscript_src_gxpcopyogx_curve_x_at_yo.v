Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gx_curve_x_at_y.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gx_curve_x_at_y_z := 1%positive.
Notation V_gx_curve_x_at_y__tmp := 2%positive.
Notation V_gx_curve_x_at_y__tmp1 := 3%positive.
Notation V_gx_curve_x_at_y_a := 4%positive.
Notation V_gx_curve_x_at_y_b := 5%positive.
Notation V_gx_curve_x_at_y_c := 6%positive.
Notation V_gx_curve_x_at_y_cx0 := 7%positive.
Notation V_gx_curve_x_at_y_cy0 := 8%positive.
Notation V_gx_curve_x_at_y_cy1 := 9%positive.
Notation V_gx_curve_x_at_y_cy2 := 10%positive.
Notation V_gx_curve_x_at_y_cy3 := 11%positive.
Notation V_gx_curve_x_at_y_i := 12%positive.
Notation V_gx_curve_x_at_y_k := 13%positive.
Notation V_gx_curve_x_at_y_prc_dref_off0 := 14%positive.
Notation V_gx_curve_x_at_y_prc_dref_off32 := 15%positive.
Notation V_gx_curve_x_at_y_prc_dref_off40 := 16%positive.
Notation V_gx_curve_x_at_y_prc_dref_off48 := 17%positive.
Notation V_gx_curve_x_at_y_prc_dref_off80 := 18%positive.
Notation V_gx_curve_x_at_y_prc_dref_off84 := 19%positive.
Notation V_gx_curve_x_at_y_prc_dref_off88_off0 := 20%positive.
Notation V_gx_curve_x_at_y_prc_dref_off88_off16 := 21%positive.
Notation V_gx_curve_x_at_y_prc_dref_off88_off24 := 22%positive.
Notation V_gx_curve_x_at_y_prc_dref_off88_off8 := 23%positive.
Notation V_gx_curve_x_at_y_prc_dref_off8_off0 := 24%positive.
Notation V_gx_curve_x_at_y_prc_dref_off8_off8 := 25%positive.
Notation V_gx_curve_x_at_y_t := 26%positive.
Notation V_gx_curve_x_at_y_t2 := 27%positive.
Notation V_gx_curve_x_at_y_t21 := 28%positive.
Notation V_gx_curve_x_at_y_t2d := 29%positive.
Notation V_gx_curve_x_at_y_t2d4 := 30%positive.
Notation V_gx_curve_x_at_y_t3 := 31%positive.
Notation V_gx_curve_x_at_y_t32 := 32%positive.
Notation V_gx_curve_x_at_y_t3d := 33%positive.
Notation V_gx_curve_x_at_y_t3d3 := 34%positive.
Notation V_gx_curve_x_at_y_xd := 35%positive.
Notation V_gx_curve_x_at_y_xl := 36%positive.
Notation V_gx_curve_x_at_y_yd := 37%positive.
Notation V_gx_curve_x_at_y_ym := 38%positive.
Notation V_gx_curve_x_at_y_yn := 39%positive.
Notation V_gx_curve_x_at_y_yrel := 40%positive.
Notation V_gx_curve_x_at_y_prc := 41%positive.
Notation V_gx_curve_x_at_y_y := 42%positive.
Definition Pedges_gx_curve_x_at_y: list (edge proc) :=
  (EA 1 (AAssign V_gx_curve_x_at_y_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y_k) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_gx_curve_x_at_y__tmp
  (Some (EVar V_gx_curve_x_at_y_y))) 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y__tmp) s) >=
  (eval (EVar V_gx_curve_x_at_y_prc_dref_off88_off0) s))%Z)) 8)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_gx_curve_x_at_y__tmp) s) <
  (eval (EVar V_gx_curve_x_at_y_prc_dref_off88_off0) s))%Z)) 7)::
  (EA 7 AWeaken 11)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y__tmp) s) <=
  (eval (EVar V_gx_curve_x_at_y_prc_dref_off88_off8) s))%Z)) 102)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_gx_curve_x_at_y__tmp) s) >
  (eval (EVar V_gx_curve_x_at_y_prc_dref_off88_off8) s))%Z)) 10)::
  (EA 10 AWeaken 11)::(EA 11 (AAssign V_gx_curve_x_at_y_cy0
  (Some (EVar V_gx_curve_x_at_y_prc_dref_off8_off8))) 12)::(EA 12 (AAssign
  V_gx_curve_x_at_y_cy3 None) 13)::(EA 13 (AAssign V_gx_curve_x_at_y_t
  (Some (ENum (0)))) 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y_cy0) s) >
  (eval (EVar V_gx_curve_x_at_y_cy3) s))%Z)) 21)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y_cy0) s) <=
  (eval (EVar V_gx_curve_x_at_y_cy3) s))%Z)) 16)::(EA 16 AWeaken 17)::
  (EA 17 (AAssign V_gx_curve_x_at_y_cx0
  (Some (EVar V_gx_curve_x_at_y_prc_dref_off8_off0))) 18)::(EA 18 (AAssign
  V_gx_curve_x_at_y_cy1 None) 19)::(EA 19 (AAssign V_gx_curve_x_at_y_cy2
  None) 20)::(EA 20 ANone 28)::(EA 21 AWeaken 22)::(EA 22 (AAssign
  V_gx_curve_x_at_y_cx0 None) 23)::(EA 23 (AAssign V_gx_curve_x_at_y_cy0
  None) 24)::(EA 24 (AAssign V_gx_curve_x_at_y_cy1 None) 25)::(EA 25 (AAssign
  V_gx_curve_x_at_y_cy2 None) 26)::(EA 26 (AAssign V_gx_curve_x_at_y_cy3
  (Some (EVar V_gx_curve_x_at_y_prc_dref_off8_off8))) 27)::(EA 27 ANone 28)::
  (EA 28 (AAssign V_gx_curve_x_at_y_k
  (Some (EVar V_gx_curve_x_at_y_prc_dref_off0))) 29)::(EA 29 (AAssign
  V_gx_curve_x_at_y_i (Some (EVar V_gx_curve_x_at_y_prc_dref_off0))) 30)::
  (EA 30 ANone 31)::(EA 31 AWeaken 32)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y_i) s) > (eval (ENum (0))
  s))%Z)) 79)::(EA 32 (AGuard (fun s => ((eval (EVar V_gx_curve_x_at_y_i)
  s) <= (eval (ENum (0)) s))%Z)) 33)::(EA 33 AWeaken 34)::(EA 34 (AAssign
  V_gx_curve_x_at_y_a (Some (EVar V_gx_curve_x_at_y_prc_dref_off32))) 35)::
  (EA 35 (AAssign V_gx_curve_x_at_y_b
  (Some (EVar V_gx_curve_x_at_y_prc_dref_off40))) 36)::(EA 36 (AAssign
  V_gx_curve_x_at_y_c (Some (EVar V_gx_curve_x_at_y_prc_dref_off48))) 37)::
  (EA 37 AWeaken 38)::(EA 38 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y_t) s) <=
  (eval (EVar V_gx_curve_x_at_y_prc_dref_off84) s))%Z)) 63)::(EA 38 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y_t) s) >
  (eval (EVar V_gx_curve_x_at_y_prc_dref_off84) s))%Z)) 39)::
  (EA 39 AWeaken 40)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y_prc_dref_off80) s) <>
  (eval (ENum (0)) s))%Z)) 50)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y_prc_dref_off80) s) =
  (eval (ENum (0)) s))%Z)) 41)::(EA 41 AWeaken 42)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y_k) s) >= (eval (ENum (11))
  s))%Z)) 45)::(EA 42 (AGuard (fun s => ((eval (EVar V_gx_curve_x_at_y_k)
  s) < (eval (ENum (11)) s))%Z)) 43)::(EA 43 AWeaken 44)::(EA 44 ANone 47)::
  (EA 45 AWeaken 46)::(EA 46 ANone 47)::(EA 47 (AAssign
  V_gx_curve_x_at_y_prc_dref_off80 (Some (ENum (1)))) 48)::(EA 48 ANone 49)::
  (EA 49 AWeaken 51)::(EA 50 AWeaken 51)::(EA 51 ANone 55)::
  (EA 51 ANone 52)::(EA 52 (AAssign V_gx_curve_x_at_y_xl None) 53)::
  (EA 53 (AAssign V_gx_curve_x_at_y_xd None) 54)::(EA 54 ANone 62)::
  (EA 55 (AAssign V_gx_curve_x_at_y_t21
  (Some (EMul (EVar V_gx_curve_x_at_y_t) (EVar V_gx_curve_x_at_y_t)))) 56)::
  (EA 56 (AAssign V_gx_curve_x_at_y_t32
  (Some (EMul (EVar V_gx_curve_x_at_y_t21)
  (EVar V_gx_curve_x_at_y_t)))) 57)::(EA 57 (AAssign V_gx_curve_x_at_y_t3d3
  (Some (EAdd (EMul (EAdd (EVar V_gx_curve_x_at_y_t21)
  (EVar V_gx_curve_x_at_y_t)) (ENum (3))) (ENum (1))))) 58)::(EA 58 (AAssign
  V_gx_curve_x_at_y_t2d4 (Some (EAdd (EAdd (EVar V_gx_curve_x_at_y_t)
  (EVar V_gx_curve_x_at_y_t)) (ENum (1))))) 59)::(EA 59 (AAssign
  V_gx_curve_x_at_y_xl None) 60)::(EA 60 (AAssign V_gx_curve_x_at_y_xd
  None) 61)::(EA 61 ANone 62)::(EA 62 ANone 71)::(EA 63 AWeaken 64)::
  (EA 64 (AAssign V_gx_curve_x_at_y_t2 (Some (EMul (EVar V_gx_curve_x_at_y_t)
  (EVar V_gx_curve_x_at_y_t)))) 65)::(EA 65 (AAssign V_gx_curve_x_at_y_t3
  (Some (EMul (EVar V_gx_curve_x_at_y_t2) (EVar V_gx_curve_x_at_y_t)))) 66)::
  (EA 66 (AAssign V_gx_curve_x_at_y_t3d
  (Some (EAdd (EMul (EAdd (EVar V_gx_curve_x_at_y_t2)
  (EVar V_gx_curve_x_at_y_t)) (ENum (3))) (ENum (1))))) 67)::(EA 67 (AAssign
  V_gx_curve_x_at_y_t2d (Some (EAdd (EAdd (EVar V_gx_curve_x_at_y_t)
  (EVar V_gx_curve_x_at_y_t)) (ENum (1))))) 68)::(EA 68 (AAssign
  V_gx_curve_x_at_y_xl None) 69)::(EA 69 (AAssign V_gx_curve_x_at_y_xd
  None) 70)::(EA 70 ANone 71)::(EA 71 (AAssign
  V_gx_curve_x_at_y_prc_dref_off88_off0
  (Some (EVar V_gx_curve_x_at_y_cy0))) 72)::(EA 72 (AAssign
  V_gx_curve_x_at_y_prc_dref_off88_off8
  (Some (EVar V_gx_curve_x_at_y_cy3))) 73)::(EA 73 (AAssign
  V_gx_curve_x_at_y_prc_dref_off88_off16
  (Some (EVar V_gx_curve_x_at_y_xl))) 74)::(EA 74 (AAssign
  V_gx_curve_x_at_y_prc_dref_off88_off24
  (Some (EVar V_gx_curve_x_at_y_xd))) 75)::(EA 75 (AAssign
  V_gx_curve_x_at_y_yd (Some (ESub (EVar V_gx_curve_x_at_y_cy3)
  (EVar V_gx_curve_x_at_y_cy0)))) 76)::(EA 76 (AAssign V_gx_curve_x_at_y_yrel
  (Some (ESub (EVar V_gx_curve_x_at_y__tmp)
  (EVar V_gx_curve_x_at_y_cy0)))) 77)::(EA 77 ANone 78)::
  (EA 78 AWeaken 109)::(EA 79 AWeaken 80)::(EA 80 (AAssign
  V_gx_curve_x_at_y_ym None) 81)::(EA 81 (AAssign V_gx_curve_x_at_y_yn
  None) 82)::(EA 82 (AAssign V_gx_curve_x_at_y_t None) 83)::
  (EA 83 AWeaken 84)::(EA 84 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y__tmp) s) <
  (eval (EVar V_gx_curve_x_at_y_yn) s))%Z)) 91)::(EA 84 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y__tmp) s) >=
  (eval (EVar V_gx_curve_x_at_y_yn) s))%Z)) 85)::(EA 85 AWeaken 86)::
  (EA 86 (AAssign V_gx_curve_x_at_y_t (Some (EAdd (EVar V_gx_curve_x_at_y_t)
  (ENum (1))))) 87)::(EA 87 (AAssign V_gx_curve_x_at_y_cy2 None) 88)::
  (EA 88 (AAssign V_gx_curve_x_at_y_cy1 None) 89)::(EA 89 (AAssign
  V_gx_curve_x_at_y_cy0 (Some (EVar V_gx_curve_x_at_y_yn))) 90)::
  (EA 90 ANone 96)::(EA 91 AWeaken 92)::(EA 92 (AAssign V_gx_curve_x_at_y_cy1
  None) 93)::(EA 93 (AAssign V_gx_curve_x_at_y_cy2 None) 94)::(EA 94 (AAssign
  V_gx_curve_x_at_y_cy3 (Some (EVar V_gx_curve_x_at_y_yn))) 95)::
  (EA 95 ANone 96)::(EA 96 ANone 97)::(EA 97 (AAssign V_gx_curve_x_at_y_i
  (Some (EAdd (EVar V_gx_curve_x_at_y_i) (ENum (-1))))) 98)::
  (EA 98 ANone 99)::(EA 99 ANone 100)::(EA 100 (AAssign V_gx_curve_x_at_y_z
  (Some (EAdd (ENum (1)) (EVar V_gx_curve_x_at_y_z)))) 101)::
  (EA 101 AWeaken 32)::(EA 102 AWeaken 103)::(EA 103 (AAssign
  V_gx_curve_x_at_y_yd
  (Some (ESub (EVar V_gx_curve_x_at_y_prc_dref_off88_off8)
  (EVar V_gx_curve_x_at_y_prc_dref_off88_off0)))) 104)::(EA 104 (AAssign
  V_gx_curve_x_at_y_yrel (Some (ESub (EVar V_gx_curve_x_at_y__tmp)
  (EVar V_gx_curve_x_at_y_prc_dref_off88_off0)))) 105)::(EA 105 (AAssign
  V_gx_curve_x_at_y_xl
  (Some (EVar V_gx_curve_x_at_y_prc_dref_off88_off16))) 106)::
  (EA 106 (AAssign V_gx_curve_x_at_y_xd
  (Some (EVar V_gx_curve_x_at_y_prc_dref_off88_off24))) 107)::
  (EA 107 ANone 108)::(EA 108 AWeaken 109)::(EA 109 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y_yrel) s) = (eval (ENum (0))
  s))%Z)) 130)::(EA 109 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y_yrel) s) <> (eval (ENum (0))
  s))%Z)) 110)::(EA 110 AWeaken 111)::(EA 111 ANone 112)::
  (EA 111 ANone 124)::(EA 112 AWeaken 113)::(EA 113 (AGuard
  (fun s => ((eval (EVar V_gx_curve_x_at_y_xd) s) >= (eval (ENum (0))
  s))%Z)) 120)::(EA 113 (AGuard (fun s => ((eval (EVar V_gx_curve_x_at_y_xd)
  s) < (eval (ENum (0)) s))%Z)) 114)::(EA 114 AWeaken 115)::
  (EA 115 ANone 117)::(EA 115 ANone 116)::(EA 116 ANone 123)::
  (EA 117 (AAssign V_gx_curve_x_at_y__tmp1 None) 118)::(EA 118 ANone 119)::
  (EA 119 AWeaken 134)::(EA 120 AWeaken 121)::(EA 121 ANone 127)::
  (EA 121 ANone 122)::(EA 122 ANone 123)::(EA 123 ANone 124)::
  (EA 124 (AAssign V_gx_curve_x_at_y__tmp1 None) 125)::(EA 125 ANone 126)::
  (EA 126 AWeaken 134)::(EA 127 (AAssign V_gx_curve_x_at_y__tmp1 None) 128)::
  (EA 128 ANone 129)::(EA 129 AWeaken 134)::(EA 130 AWeaken 131)::
  (EA 131 (AAssign V_gx_curve_x_at_y__tmp1
  (Some (EVar V_gx_curve_x_at_y_xl))) 132)::(EA 132 ANone 133)::
  (EA 133 AWeaken 134)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gx_curve_x_at_y => Pedges_gx_curve_x_at_y
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gx_curve_x_at_y => 134
     end)%positive;
  var_global := var_global
}.

Definition ai_gx_curve_x_at_y (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 3 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0)%Z
   | 4 => (-1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 5 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0)%Z
   | 6 => (-1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 7 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_prc_dref_off88_off0 + 1 <= 0)%Z
   | 8 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_prc_dref_off88_off0 <= 0)%Z
   | 9 => (-1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_prc_dref_off88_off0 <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 10 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_prc_dref_off88_off0 <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_prc_dref_off88_off8 + 1 <= 0)%Z
   | 11 => (-1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 12 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0)%Z
   | 13 => (-1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 14 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0)%Z
   | 15 => (-1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 16 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_cy0+ -1 * s V_gx_curve_x_at_y_cy3 <= 0)%Z
   | 17 => (1 * s V_gx_curve_x_at_y_cy0+ -1 * s V_gx_curve_x_at_y_cy3 <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 18 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_cy0+ -1 * s V_gx_curve_x_at_y_cy3 <= 0)%Z
   | 19 => (1 * s V_gx_curve_x_at_y_cy0+ -1 * s V_gx_curve_x_at_y_cy3 <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 20 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_cy0+ -1 * s V_gx_curve_x_at_y_cy3 <= 0)%Z
   | 21 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_cy0+ 1 * s V_gx_curve_x_at_y_cy3 + 1 <= 0)%Z
   | 22 => (-1 * s V_gx_curve_x_at_y_cy0+ 1 * s V_gx_curve_x_at_y_cy3 + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 23 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_cy0+ 1 * s V_gx_curve_x_at_y_cy3 + 1 <= 0)%Z
   | 24 => (-1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 25 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0)%Z
   | 26 => (-1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 27 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0)%Z
   | 28 => (-1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 29 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0)%Z
   | 30 => (-1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 31 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_t <= 0 /\ -1 * s V_gx_curve_x_at_y_t <= 0)%Z
   | 32 => (-1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 33 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0)%Z
   | 34 => (1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 35 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0)%Z
   | 36 => (1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 37 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0)%Z
   | 38 => (1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 39 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0)%Z
   | 40 => (1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 41 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0 /\ -1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0)%Z
   | 42 => (-1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 43 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0 /\ -1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0 /\ 1 * s V_gx_curve_x_at_y_k + -10 <= 0)%Z
   | 44 => (1 * s V_gx_curve_x_at_y_k + -10 <= 0 /\ -1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 45 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0 /\ -1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0 /\ -1 * s V_gx_curve_x_at_y_k + 11 <= 0)%Z
   | 46 => (-1 * s V_gx_curve_x_at_y_k + 11 <= 0 /\ -1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 47 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0 /\ -1 * s V_gx_curve_x_at_y_prc_dref_off80 <= 0)%Z
   | 48 => (1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off80 + -1 <= 0 /\ -1 * s V_gx_curve_x_at_y_prc_dref_off80 + 1 <= 0)%Z
   | 49 => (-1 * s V_gx_curve_x_at_y_prc_dref_off80 + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off80 + -1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0)%Z
   | 50 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0)%Z
   | 51 => (1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 52 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0)%Z
   | 53 => (1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 54 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0)%Z
   | 55 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0)%Z
   | 56 => (1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 57 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0)%Z
   | 58 => (1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 59 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 2 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t2d4 + 3 <= 0)%Z
   | 60 => (2 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t2d4 + 3 <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 61 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ 1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 2 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t2d4 + 3 <= 0)%Z
   | 62 => (1 * s V_gx_curve_x_at_y_prc_dref_off84+ -1 * s V_gx_curve_x_at_y_t + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 63 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_prc_dref_off84+ 1 * s V_gx_curve_x_at_y_t <= 0)%Z
   | 64 => (-1 * s V_gx_curve_x_at_y_prc_dref_off84+ 1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 65 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_prc_dref_off84+ 1 * s V_gx_curve_x_at_y_t <= 0)%Z
   | 66 => (-1 * s V_gx_curve_x_at_y_prc_dref_off84+ 1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 67 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_prc_dref_off84+ 1 * s V_gx_curve_x_at_y_t <= 0)%Z
   | 68 => (-1 * s V_gx_curve_x_at_y_prc_dref_off84+ 1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0 /\ -2 * s V_gx_curve_x_at_y_prc_dref_off84+ 1 * s V_gx_curve_x_at_y_t2d + -1 <= 0)%Z
   | 69 => (-2 * s V_gx_curve_x_at_y_prc_dref_off84+ 1 * s V_gx_curve_x_at_y_t2d + -1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_prc_dref_off84+ 1 * s V_gx_curve_x_at_y_t <= 0)%Z
   | 70 => (-1 * s V_gx_curve_x_at_y_prc_dref_off84+ 1 * s V_gx_curve_x_at_y_t <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0 /\ -2 * s V_gx_curve_x_at_y_prc_dref_off84+ 1 * s V_gx_curve_x_at_y_t2d + -1 <= 0)%Z
   | 71 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0)%Z
   | 72 => (1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 73 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0)%Z
   | 74 => (1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 75 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0)%Z
   | 76 => (1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 77 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_i <= 0)%Z
   | 78 => (1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 79 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0)%Z
   | 80 => (-1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 81 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0)%Z
   | 82 => (-1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 83 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0)%Z
   | 84 => (-1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 85 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_yn <= 0)%Z
   | 86 => (-1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_yn <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 87 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_yn <= 0)%Z
   | 88 => (-1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_yn <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 89 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_yn <= 0)%Z
   | 90 => (-1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_yn <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_cy0 <= 0)%Z
   | 91 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_yn + 1 <= 0)%Z
   | 92 => (1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_yn + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 93 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_yn + 1 <= 0)%Z
   | 94 => (1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_yn + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 95 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_yn + 1 <= 0 /\ 1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_cy3 + 1 <= 0)%Z
   | 96 => (-1 * s V_gx_curve_x_at_y_i + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 97 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_i + 1 <= 0)%Z
   | 98 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_i <= 0)%Z
   | 99 => (-1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 100 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_i <= 0)%Z
   | 101 => (-1 * s V_gx_curve_x_at_y_i <= 0 /\ -1 * s V_gx_curve_x_at_y_z + 1 <= 0)%Z
   | 102 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_prc_dref_off88_off0 <= 0 /\ 1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_prc_dref_off88_off8 <= 0)%Z
   | 103 => (1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_prc_dref_off88_off8 <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_prc_dref_off88_off0 <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 104 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_prc_dref_off88_off0 <= 0 /\ 1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_prc_dref_off88_off8 <= 0 /\ -1 * s V_gx_curve_x_at_y_yd <= 0)%Z
   | 105 => (-1 * s V_gx_curve_x_at_y_yd <= 0 /\ 1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_prc_dref_off88_off8 <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_prc_dref_off88_off0 <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_yrel <= 0)%Z
   | 106 => (-1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_yrel <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_prc_dref_off88_off0 <= 0 /\ 1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_prc_dref_off88_off8 <= 0 /\ -1 * s V_gx_curve_x_at_y_yd <= 0)%Z
   | 107 => (-1 * s V_gx_curve_x_at_y_yd <= 0 /\ 1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_prc_dref_off88_off8 <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_prc_dref_off88_off0 <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_yrel <= 0)%Z
   | 108 => (-1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_yrel <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_k <= 0 /\ -1 * s V_gx_curve_x_at_y__tmp+ 1 * s V_gx_curve_x_at_y_prc_dref_off88_off0 <= 0 /\ 1 * s V_gx_curve_x_at_y__tmp+ -1 * s V_gx_curve_x_at_y_prc_dref_off88_off8 <= 0 /\ -1 * s V_gx_curve_x_at_y_yd <= 0)%Z
   | 109 => (-1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 110 => (-1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 111 => (-1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 112 => (-1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 113 => (-1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 114 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_xd + 1 <= 0)%Z
   | 115 => (1 * s V_gx_curve_x_at_y_xd + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 116 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_xd + 1 <= 0)%Z
   | 117 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_xd + 1 <= 0)%Z
   | 118 => (1 * s V_gx_curve_x_at_y_xd + 1 <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 119 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_xd + 1 <= 0)%Z
   | 120 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_xd <= 0)%Z
   | 121 => (-1 * s V_gx_curve_x_at_y_xd <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 122 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_xd <= 0)%Z
   | 123 => (-1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 124 => (-1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 125 => (-1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 126 => (-1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 127 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_xd <= 0)%Z
   | 128 => (-1 * s V_gx_curve_x_at_y_xd <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 129 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ -1 * s V_gx_curve_x_at_y_xd <= 0)%Z
   | 130 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_yrel <= 0 /\ -1 * s V_gx_curve_x_at_y_yrel <= 0)%Z
   | 131 => (-1 * s V_gx_curve_x_at_y_yrel <= 0 /\ 1 * s V_gx_curve_x_at_y_yrel <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 132 => (-1 * s V_gx_curve_x_at_y_z <= 0 /\ 1 * s V_gx_curve_x_at_y_yrel <= 0 /\ -1 * s V_gx_curve_x_at_y_yrel <= 0)%Z
   | 133 => (-1 * s V_gx_curve_x_at_y_yrel <= 0 /\ 1 * s V_gx_curve_x_at_y_yrel <= 0 /\ -1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | 134 => (-1 * s V_gx_curve_x_at_y_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gx_curve_x_at_y (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 2 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 3 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 4 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 5 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 6 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 7 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 8 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 9 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 10 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 11 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 12 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 13 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 14 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 15 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 16 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 17 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 18 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 19 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 20 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 21 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 22 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 23 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 24 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 25 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 26 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 27 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 28 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 29 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 30 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 31 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 32 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_gx_curve_x_at_y_i)) (F_check_ge (0) (0))]
     (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 34 => (s V_gx_curve_x_at_y_z - max0(-1 + s V_gx_curve_x_at_y_i)
            + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 35 => (s V_gx_curve_x_at_y_z - max0(-1 + s V_gx_curve_x_at_y_i)
            + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 36 => (s V_gx_curve_x_at_y_z - max0(-1 + s V_gx_curve_x_at_y_i)
            + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 37 => (s V_gx_curve_x_at_y_z - max0(-1 + s V_gx_curve_x_at_y_i)
            + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 38 => (s V_gx_curve_x_at_y_z - max0(-1 + s V_gx_curve_x_at_y_i)
            + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 39 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_gx_curve_x_at_y_i) (-1
                                                                    + 
                                                                    s V_gx_curve_x_at_y_i))]
     (s V_gx_curve_x_at_y_z - max0(-1 + s V_gx_curve_x_at_y_i)
      + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 40 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 41 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 42 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 43 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 44 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 45 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 46 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 47 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 48 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 49 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 50 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 51 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 52 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 53 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 54 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 55 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 56 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 57 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 58 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 59 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 60 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 61 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 62 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 63 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_gx_curve_x_at_y_i) (-1
                                                                    + 
                                                                    s V_gx_curve_x_at_y_i))]
     (s V_gx_curve_x_at_y_z - max0(-1 + s V_gx_curve_x_at_y_i)
      + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 64 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 65 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 66 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 67 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 68 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 69 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 70 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 71 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 72 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 73 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 74 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 75 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 76 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 77 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 78 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 79 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 80 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 81 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 82 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 83 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gx_curve_x_at_y_i)) (F_check_ge (s V_gx_curve_x_at_y_i) (0))]
     (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_i) <= z)%Q
   | 84 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 85 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 86 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 87 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 88 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 89 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 90 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 91 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 92 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 93 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 94 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 95 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 96 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 97 => (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 98 => ((1 # 1) + s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 99 => ((1 # 1) + s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 100 => ((1 # 1) + s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 101 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gx_curve_x_at_y_i) (0))) (F_max0_ge_0 (s V_gx_curve_x_at_y_i))]
     (s V_gx_curve_x_at_y_i + s V_gx_curve_x_at_y_z <= z)%Q
   | 102 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 103 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 104 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 105 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 106 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 107 => (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 108 => hints
     [(*-1 0*) F_max0_ge_0 (s V_gx_curve_x_at_y_prc_dref_off0)]
     (s V_gx_curve_x_at_y_z + max0(s V_gx_curve_x_at_y_prc_dref_off0) <= z)%Q
   | 109 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 110 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gx_curve_x_at_y_z) (0))) (F_max0_ge_0 (s V_gx_curve_x_at_y_z))]
     (s V_gx_curve_x_at_y_z <= z)%Q
   | 111 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 112 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 113 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 114 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 115 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 116 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 117 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 118 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 119 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gx_curve_x_at_y_z)) (F_check_ge (s V_gx_curve_x_at_y_z) (0))]
     (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 120 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 121 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 122 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 123 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 124 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 125 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 126 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gx_curve_x_at_y_z)) (F_check_ge (s V_gx_curve_x_at_y_z) (0))]
     (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 127 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 128 => (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 129 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gx_curve_x_at_y_z)) (F_check_ge (s V_gx_curve_x_at_y_z) (0))]
     (max0(s V_gx_curve_x_at_y_z) <= z)%Q
   | 130 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 131 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 132 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 133 => (s V_gx_curve_x_at_y_z <= z)%Q
   | 134 => (s V_gx_curve_x_at_y_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gx_curve_x_at_y =>
    [mkPA Q (fun n z s => ai_gx_curve_x_at_y n s /\ annot0_gx_curve_x_at_y n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gx_curve_x_at_y (proc_start P_gx_curve_x_at_y) s1 (proc_end P_gx_curve_x_at_y) s2 ->
    (s2 V_gx_curve_x_at_y_z <= max0(s1 V_gx_curve_x_at_y_prc_dref_off0))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gx_curve_x_at_y.
Qed.
