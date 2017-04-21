Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mainGtU.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mainGtU_z := 1%positive.
Notation V_mainGtU__tmp := 2%positive.
Notation V_mainGtU__tmp1 := 3%positive.
Notation V_mainGtU__tmp2 := 4%positive.
Notation V_mainGtU__tmp3 := 5%positive.
Notation V_mainGtU_budget_dref := 6%positive.
Notation V_mainGtU_c1 := 7%positive.
Notation V_mainGtU_c2 := 8%positive.
Notation V_mainGtU_k := 9%positive.
Notation V_mainGtU_s1 := 10%positive.
Notation V_mainGtU_s2 := 11%positive.
Notation V_mainGtU_block := 12%positive.
Notation V_mainGtU_budget := 13%positive.
Notation V_mainGtU_i1 := 14%positive.
Notation V_mainGtU_i2 := 15%positive.
Notation V_mainGtU_nblock := 16%positive.
Notation V_mainGtU_quadrant := 17%positive.
Definition Pedges_mainGtU: list (edge proc) :=
  (EA 1 (AAssign V_mainGtU_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_mainGtU__tmp3) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 (AGuard (fun s => ((eval (EVar V_mainGtU__tmp2) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_mainGtU__tmp1) s) >= (eval (ENum (0)) s))%Z)) 5)::
  (EA 5 AWeaken 6)::(EA 6 (AAssign V_mainGtU__tmp1
  (Some (EVar V_mainGtU_i1))) 7)::(EA 7 (AAssign V_mainGtU__tmp3
  (Some (EVar V_mainGtU_i2))) 8)::(EA 8 (AAssign V_mainGtU__tmp2
  (Some (EVar V_mainGtU_nblock))) 9)::(EA 9 (AAssign V_mainGtU_c1 None) 10)::
  (EA 10 (AAssign V_mainGtU_c2 None) 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 323)::(EA 12 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 (AAssign
  V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 15)::
  (EA 15 (AAssign V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3)
  (ENum (1))))) 16)::(EA 16 (AAssign V_mainGtU_c1 None) 17)::(EA 17 (AAssign
  V_mainGtU_c2 None) 18)::(EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 319)::(EA 19 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 20)::(EA 20 AWeaken 21)::(EA 21 (AAssign
  V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 22)::
  (EA 22 (AAssign V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3)
  (ENum (1))))) 23)::(EA 23 (AAssign V_mainGtU_c1 None) 24)::(EA 24 (AAssign
  V_mainGtU_c2 None) 25)::(EA 25 AWeaken 26)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 315)::(EA 26 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 27)::(EA 27 AWeaken 28)::(EA 28 (AAssign
  V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 29)::
  (EA 29 (AAssign V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3)
  (ENum (1))))) 30)::(EA 30 (AAssign V_mainGtU_c1 None) 31)::(EA 31 (AAssign
  V_mainGtU_c2 None) 32)::(EA 32 AWeaken 33)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 311)::(EA 33 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 34)::(EA 34 AWeaken 35)::(EA 35 (AAssign
  V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 36)::
  (EA 36 (AAssign V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3)
  (ENum (1))))) 37)::(EA 37 (AAssign V_mainGtU_c1 None) 38)::(EA 38 (AAssign
  V_mainGtU_c2 None) 39)::(EA 39 AWeaken 40)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 307)::(EA 40 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 41)::(EA 41 AWeaken 42)::(EA 42 (AAssign
  V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 43)::
  (EA 43 (AAssign V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3)
  (ENum (1))))) 44)::(EA 44 (AAssign V_mainGtU_c1 None) 45)::(EA 45 (AAssign
  V_mainGtU_c2 None) 46)::(EA 46 AWeaken 47)::(EA 47 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 303)::(EA 47 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 48)::(EA 48 AWeaken 49)::(EA 49 (AAssign
  V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 50)::
  (EA 50 (AAssign V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3)
  (ENum (1))))) 51)::(EA 51 (AAssign V_mainGtU_c1 None) 52)::(EA 52 (AAssign
  V_mainGtU_c2 None) 53)::(EA 53 AWeaken 54)::(EA 54 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 299)::(EA 54 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 55)::(EA 55 AWeaken 56)::(EA 56 (AAssign
  V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 57)::
  (EA 57 (AAssign V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3)
  (ENum (1))))) 58)::(EA 58 (AAssign V_mainGtU_c1 None) 59)::(EA 59 (AAssign
  V_mainGtU_c2 None) 60)::(EA 60 AWeaken 61)::(EA 61 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 295)::(EA 61 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 62)::(EA 62 AWeaken 63)::(EA 63 (AAssign
  V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 64)::
  (EA 64 (AAssign V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3)
  (ENum (1))))) 65)::(EA 65 (AAssign V_mainGtU_c1 None) 66)::(EA 66 (AAssign
  V_mainGtU_c2 None) 67)::(EA 67 AWeaken 68)::(EA 68 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 291)::(EA 68 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 69)::(EA 69 AWeaken 70)::(EA 70 (AAssign
  V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 71)::
  (EA 71 (AAssign V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3)
  (ENum (1))))) 72)::(EA 72 (AAssign V_mainGtU_c1 None) 73)::(EA 73 (AAssign
  V_mainGtU_c2 None) 74)::(EA 74 AWeaken 75)::(EA 75 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 287)::(EA 75 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 76)::(EA 76 AWeaken 77)::(EA 77 (AAssign
  V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 78)::
  (EA 78 (AAssign V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3)
  (ENum (1))))) 79)::(EA 79 (AAssign V_mainGtU_c1 None) 80)::(EA 80 (AAssign
  V_mainGtU_c2 None) 81)::(EA 81 AWeaken 82)::(EA 82 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 283)::(EA 82 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 83)::(EA 83 AWeaken 84)::(EA 84 (AAssign
  V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 85)::
  (EA 85 (AAssign V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3)
  (ENum (1))))) 86)::(EA 86 (AAssign V_mainGtU_c1 None) 87)::(EA 87 (AAssign
  V_mainGtU_c2 None) 88)::(EA 88 AWeaken 89)::(EA 89 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 279)::(EA 89 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 90)::(EA 90 AWeaken 91)::(EA 91 (AAssign
  V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 92)::
  (EA 92 (AAssign V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3)
  (ENum (1))))) 93)::(EA 93 (AAssign V_mainGtU_k
  (Some (EAdd (EVar V_mainGtU__tmp2) (ENum (8))))) 94)::(EA 94 ANone 95)::
  (EA 95 (AAssign V_mainGtU_c1 None) 96)::(EA 96 (AAssign V_mainGtU_c2
  None) 97)::(EA 97 AWeaken 98)::(EA 98 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 275)::(EA 98 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 99)::(EA 99 AWeaken 100)::
  (EA 100 (AAssign V_mainGtU_s1 None) 101)::(EA 101 (AAssign V_mainGtU_s2
  None) 102)::(EA 102 AWeaken 103)::(EA 103 (AGuard
  (fun s => ((eval (EVar V_mainGtU_s1) s) <> (eval (EVar V_mainGtU_s2)
  s))%Z)) 271)::(EA 103 (AGuard (fun s => ((eval (EVar V_mainGtU_s1) s) =
  (eval (EVar V_mainGtU_s2) s))%Z)) 104)::(EA 104 AWeaken 105)::
  (EA 105 (AAssign V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1)
  (ENum (1))))) 106)::(EA 106 (AAssign V_mainGtU__tmp3
  (Some (EAdd (EVar V_mainGtU__tmp3) (ENum (1))))) 107)::(EA 107 (AAssign
  V_mainGtU_c1 None) 108)::(EA 108 (AAssign V_mainGtU_c2 None) 109)::
  (EA 109 AWeaken 110)::(EA 110 (AGuard (fun s => ((eval (EVar V_mainGtU_c1)
  s) <> (eval (EVar V_mainGtU_c2) s))%Z)) 267)::(EA 110 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) = (eval (EVar V_mainGtU_c2)
  s))%Z)) 111)::(EA 111 AWeaken 112)::(EA 112 (AAssign V_mainGtU_s1
  None) 113)::(EA 113 (AAssign V_mainGtU_s2 None) 114)::
  (EA 114 AWeaken 115)::(EA 115 (AGuard (fun s => ((eval (EVar V_mainGtU_s1)
  s) <> (eval (EVar V_mainGtU_s2) s))%Z)) 263)::(EA 115 (AGuard
  (fun s => ((eval (EVar V_mainGtU_s1) s) = (eval (EVar V_mainGtU_s2)
  s))%Z)) 116)::(EA 116 AWeaken 117)::(EA 117 (AAssign V_mainGtU__tmp1
  (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 118)::(EA 118 (AAssign
  V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3) (ENum (1))))) 119)::
  (EA 119 (AAssign V_mainGtU_c1 None) 120)::(EA 120 (AAssign V_mainGtU_c2
  None) 121)::(EA 121 AWeaken 122)::(EA 122 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 259)::(EA 122 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 123)::(EA 123 AWeaken 124)::
  (EA 124 (AAssign V_mainGtU_s1 None) 125)::(EA 125 (AAssign V_mainGtU_s2
  None) 126)::(EA 126 AWeaken 127)::(EA 127 (AGuard
  (fun s => ((eval (EVar V_mainGtU_s1) s) <> (eval (EVar V_mainGtU_s2)
  s))%Z)) 255)::(EA 127 (AGuard (fun s => ((eval (EVar V_mainGtU_s1) s) =
  (eval (EVar V_mainGtU_s2) s))%Z)) 128)::(EA 128 AWeaken 129)::
  (EA 129 (AAssign V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1)
  (ENum (1))))) 130)::(EA 130 (AAssign V_mainGtU__tmp3
  (Some (EAdd (EVar V_mainGtU__tmp3) (ENum (1))))) 131)::(EA 131 (AAssign
  V_mainGtU_c1 None) 132)::(EA 132 (AAssign V_mainGtU_c2 None) 133)::
  (EA 133 AWeaken 134)::(EA 134 (AGuard (fun s => ((eval (EVar V_mainGtU_c1)
  s) <> (eval (EVar V_mainGtU_c2) s))%Z)) 251)::(EA 134 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) = (eval (EVar V_mainGtU_c2)
  s))%Z)) 135)::(EA 135 AWeaken 136)::(EA 136 (AAssign V_mainGtU_s1
  None) 137)::(EA 137 (AAssign V_mainGtU_s2 None) 138)::
  (EA 138 AWeaken 139)::(EA 139 (AGuard (fun s => ((eval (EVar V_mainGtU_s1)
  s) <> (eval (EVar V_mainGtU_s2) s))%Z)) 247)::(EA 139 (AGuard
  (fun s => ((eval (EVar V_mainGtU_s1) s) = (eval (EVar V_mainGtU_s2)
  s))%Z)) 140)::(EA 140 AWeaken 141)::(EA 141 (AAssign V_mainGtU__tmp1
  (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 142)::(EA 142 (AAssign
  V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3) (ENum (1))))) 143)::
  (EA 143 (AAssign V_mainGtU_c1 None) 144)::(EA 144 (AAssign V_mainGtU_c2
  None) 145)::(EA 145 AWeaken 146)::(EA 146 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 243)::(EA 146 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 147)::(EA 147 AWeaken 148)::
  (EA 148 (AAssign V_mainGtU_s1 None) 149)::(EA 149 (AAssign V_mainGtU_s2
  None) 150)::(EA 150 AWeaken 151)::(EA 151 (AGuard
  (fun s => ((eval (EVar V_mainGtU_s1) s) <> (eval (EVar V_mainGtU_s2)
  s))%Z)) 239)::(EA 151 (AGuard (fun s => ((eval (EVar V_mainGtU_s1) s) =
  (eval (EVar V_mainGtU_s2) s))%Z)) 152)::(EA 152 AWeaken 153)::
  (EA 153 (AAssign V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1)
  (ENum (1))))) 154)::(EA 154 (AAssign V_mainGtU__tmp3
  (Some (EAdd (EVar V_mainGtU__tmp3) (ENum (1))))) 155)::(EA 155 (AAssign
  V_mainGtU_c1 None) 156)::(EA 156 (AAssign V_mainGtU_c2 None) 157)::
  (EA 157 AWeaken 158)::(EA 158 (AGuard (fun s => ((eval (EVar V_mainGtU_c1)
  s) <> (eval (EVar V_mainGtU_c2) s))%Z)) 235)::(EA 158 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) = (eval (EVar V_mainGtU_c2)
  s))%Z)) 159)::(EA 159 AWeaken 160)::(EA 160 (AAssign V_mainGtU_s1
  None) 161)::(EA 161 (AAssign V_mainGtU_s2 None) 162)::
  (EA 162 AWeaken 163)::(EA 163 (AGuard (fun s => ((eval (EVar V_mainGtU_s1)
  s) <> (eval (EVar V_mainGtU_s2) s))%Z)) 231)::(EA 163 (AGuard
  (fun s => ((eval (EVar V_mainGtU_s1) s) = (eval (EVar V_mainGtU_s2)
  s))%Z)) 164)::(EA 164 AWeaken 165)::(EA 165 (AAssign V_mainGtU__tmp1
  (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 166)::(EA 166 (AAssign
  V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3) (ENum (1))))) 167)::
  (EA 167 (AAssign V_mainGtU_c1 None) 168)::(EA 168 (AAssign V_mainGtU_c2
  None) 169)::(EA 169 AWeaken 170)::(EA 170 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) <> (eval (EVar V_mainGtU_c2)
  s))%Z)) 227)::(EA 170 (AGuard (fun s => ((eval (EVar V_mainGtU_c1) s) =
  (eval (EVar V_mainGtU_c2) s))%Z)) 171)::(EA 171 AWeaken 172)::
  (EA 172 (AAssign V_mainGtU_s1 None) 173)::(EA 173 (AAssign V_mainGtU_s2
  None) 174)::(EA 174 AWeaken 175)::(EA 175 (AGuard
  (fun s => ((eval (EVar V_mainGtU_s1) s) <> (eval (EVar V_mainGtU_s2)
  s))%Z)) 223)::(EA 175 (AGuard (fun s => ((eval (EVar V_mainGtU_s1) s) =
  (eval (EVar V_mainGtU_s2) s))%Z)) 176)::(EA 176 AWeaken 177)::
  (EA 177 (AAssign V_mainGtU__tmp1 (Some (EAdd (EVar V_mainGtU__tmp1)
  (ENum (1))))) 178)::(EA 178 (AAssign V_mainGtU__tmp3
  (Some (EAdd (EVar V_mainGtU__tmp3) (ENum (1))))) 179)::(EA 179 (AAssign
  V_mainGtU_c1 None) 180)::(EA 180 (AAssign V_mainGtU_c2 None) 181)::
  (EA 181 AWeaken 182)::(EA 182 (AGuard (fun s => ((eval (EVar V_mainGtU_c1)
  s) <> (eval (EVar V_mainGtU_c2) s))%Z)) 219)::(EA 182 (AGuard
  (fun s => ((eval (EVar V_mainGtU_c1) s) = (eval (EVar V_mainGtU_c2)
  s))%Z)) 183)::(EA 183 AWeaken 184)::(EA 184 (AAssign V_mainGtU_s1
  None) 185)::(EA 185 (AAssign V_mainGtU_s2 None) 186)::
  (EA 186 AWeaken 187)::(EA 187 (AGuard (fun s => ((eval (EVar V_mainGtU_s1)
  s) <> (eval (EVar V_mainGtU_s2) s))%Z)) 215)::(EA 187 (AGuard
  (fun s => ((eval (EVar V_mainGtU_s1) s) = (eval (EVar V_mainGtU_s2)
  s))%Z)) 188)::(EA 188 AWeaken 189)::(EA 189 (AAssign V_mainGtU__tmp1
  (Some (EAdd (EVar V_mainGtU__tmp1) (ENum (1))))) 190)::(EA 190 (AAssign
  V_mainGtU__tmp3 (Some (EAdd (EVar V_mainGtU__tmp3) (ENum (1))))) 191)::
  (EA 191 AWeaken 192)::(EA 192 (AGuard
  (fun s => ((eval (EVar V_mainGtU__tmp1) s) >= (eval (EVar V_mainGtU__tmp2)
  s))%Z)) 194)::(EA 192 (AGuard (fun s => ((eval (EVar V_mainGtU__tmp1) s) <
  (eval (EVar V_mainGtU__tmp2) s))%Z)) 193)::(EA 193 AWeaken 198)::
  (EA 194 AWeaken 195)::(EA 195 (AAssign V_mainGtU__tmp1
  (Some (ESub (EVar V_mainGtU__tmp1) (EVar V_mainGtU__tmp2)))) 196)::
  (EA 196 ANone 197)::(EA 197 AWeaken 198)::(EA 198 (AGuard
  (fun s => ((eval (EVar V_mainGtU__tmp3) s) >= (eval (EVar V_mainGtU__tmp2)
  s))%Z)) 200)::(EA 198 (AGuard (fun s => ((eval (EVar V_mainGtU__tmp3) s) <
  (eval (EVar V_mainGtU__tmp2) s))%Z)) 199)::(EA 199 AWeaken 203)::
  (EA 200 AWeaken 201)::(EA 201 (AAssign V_mainGtU__tmp3
  (Some (ESub (EVar V_mainGtU__tmp3) (EVar V_mainGtU__tmp2)))) 202)::
  (EA 202 ANone 203)::(EA 203 (AAssign V_mainGtU_k
  (Some (ESub (EVar V_mainGtU_k) (ENum (8))))) 204)::(EA 204 (AAssign
  V_mainGtU_budget_dref (Some (EAdd (EVar V_mainGtU_budget_dref)
  (ENum (-1))))) 205)::(EA 205 ANone 206)::(EA 206 AWeaken 207)::
  (EA 207 (AGuard (fun s => ((eval (EVar V_mainGtU_k) s) >= (eval (ENum (0))
  s))%Z)) 212)::(EA 207 (AGuard (fun s => ((eval (EVar V_mainGtU_k) s) <
  (eval (ENum (0)) s))%Z)) 208)::(EA 208 AWeaken 209)::(EA 209 (AAssign
  V_mainGtU__tmp (Some (ENum (0)))) 210)::(EA 210 ANone 211)::
  (EA 211 AWeaken 327)::(EA 212 AWeaken 213)::(EA 213 ANone 214)::
  (EA 214 (AAssign V_mainGtU_z (Some (EAdd (ENum (1))
  (EVar V_mainGtU_z)))) 95)::(EA 215 AWeaken 216)::(EA 216 (AAssign
  V_mainGtU__tmp None) 217)::(EA 217 ANone 218)::(EA 218 AWeaken 327)::
  (EA 219 AWeaken 220)::(EA 220 (AAssign V_mainGtU__tmp None) 221)::
  (EA 221 ANone 222)::(EA 222 AWeaken 327)::(EA 223 AWeaken 224)::
  (EA 224 (AAssign V_mainGtU__tmp None) 225)::(EA 225 ANone 226)::
  (EA 226 AWeaken 327)::(EA 227 AWeaken 228)::(EA 228 (AAssign V_mainGtU__tmp
  None) 229)::(EA 229 ANone 230)::(EA 230 AWeaken 327)::
  (EA 231 AWeaken 232)::(EA 232 (AAssign V_mainGtU__tmp None) 233)::
  (EA 233 ANone 234)::(EA 234 AWeaken 327)::(EA 235 AWeaken 236)::
  (EA 236 (AAssign V_mainGtU__tmp None) 237)::(EA 237 ANone 238)::
  (EA 238 AWeaken 327)::(EA 239 AWeaken 240)::(EA 240 (AAssign V_mainGtU__tmp
  None) 241)::(EA 241 ANone 242)::(EA 242 AWeaken 327)::
  (EA 243 AWeaken 244)::(EA 244 (AAssign V_mainGtU__tmp None) 245)::
  (EA 245 ANone 246)::(EA 246 AWeaken 327)::(EA 247 AWeaken 248)::
  (EA 248 (AAssign V_mainGtU__tmp None) 249)::(EA 249 ANone 250)::
  (EA 250 AWeaken 327)::(EA 251 AWeaken 252)::(EA 252 (AAssign V_mainGtU__tmp
  None) 253)::(EA 253 ANone 254)::(EA 254 AWeaken 327)::
  (EA 255 AWeaken 256)::(EA 256 (AAssign V_mainGtU__tmp None) 257)::
  (EA 257 ANone 258)::(EA 258 AWeaken 327)::(EA 259 AWeaken 260)::
  (EA 260 (AAssign V_mainGtU__tmp None) 261)::(EA 261 ANone 262)::
  (EA 262 AWeaken 327)::(EA 263 AWeaken 264)::(EA 264 (AAssign V_mainGtU__tmp
  None) 265)::(EA 265 ANone 266)::(EA 266 AWeaken 327)::
  (EA 267 AWeaken 268)::(EA 268 (AAssign V_mainGtU__tmp None) 269)::
  (EA 269 ANone 270)::(EA 270 AWeaken 327)::(EA 271 AWeaken 272)::
  (EA 272 (AAssign V_mainGtU__tmp None) 273)::(EA 273 ANone 274)::
  (EA 274 AWeaken 327)::(EA 275 AWeaken 276)::(EA 276 (AAssign V_mainGtU__tmp
  None) 277)::(EA 277 ANone 278)::(EA 278 AWeaken 327)::
  (EA 279 AWeaken 280)::(EA 280 (AAssign V_mainGtU__tmp None) 281)::
  (EA 281 ANone 282)::(EA 282 AWeaken 327)::(EA 283 AWeaken 284)::
  (EA 284 (AAssign V_mainGtU__tmp None) 285)::(EA 285 ANone 286)::
  (EA 286 AWeaken 327)::(EA 287 AWeaken 288)::(EA 288 (AAssign V_mainGtU__tmp
  None) 289)::(EA 289 ANone 290)::(EA 290 AWeaken 327)::
  (EA 291 AWeaken 292)::(EA 292 (AAssign V_mainGtU__tmp None) 293)::
  (EA 293 ANone 294)::(EA 294 AWeaken 327)::(EA 295 AWeaken 296)::
  (EA 296 (AAssign V_mainGtU__tmp None) 297)::(EA 297 ANone 298)::
  (EA 298 AWeaken 327)::(EA 299 AWeaken 300)::(EA 300 (AAssign V_mainGtU__tmp
  None) 301)::(EA 301 ANone 302)::(EA 302 AWeaken 327)::
  (EA 303 AWeaken 304)::(EA 304 (AAssign V_mainGtU__tmp None) 305)::
  (EA 305 ANone 306)::(EA 306 AWeaken 327)::(EA 307 AWeaken 308)::
  (EA 308 (AAssign V_mainGtU__tmp None) 309)::(EA 309 ANone 310)::
  (EA 310 AWeaken 327)::(EA 311 AWeaken 312)::(EA 312 (AAssign V_mainGtU__tmp
  None) 313)::(EA 313 ANone 314)::(EA 314 AWeaken 327)::
  (EA 315 AWeaken 316)::(EA 316 (AAssign V_mainGtU__tmp None) 317)::
  (EA 317 ANone 318)::(EA 318 AWeaken 327)::(EA 319 AWeaken 320)::
  (EA 320 (AAssign V_mainGtU__tmp None) 321)::(EA 321 ANone 322)::
  (EA 322 AWeaken 327)::(EA 323 AWeaken 324)::(EA 324 (AAssign V_mainGtU__tmp
  None) 325)::(EA 325 ANone 326)::(EA 326 AWeaken 327)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mainGtU => Pedges_mainGtU
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mainGtU => 327
     end)%positive;
  var_global := var_global
}.

Definition ai_mainGtU (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 3 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU__tmp3 <= 0)%Z
   | 4 => (-1 * s V_mainGtU__tmp3 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU__tmp2 <= 0)%Z
   | 5 => (-1 * s V_mainGtU__tmp2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU__tmp3 <= 0 /\ -1 * s V_mainGtU__tmp1 <= 0)%Z
   | 6 => (-1 * s V_mainGtU__tmp1 <= 0 /\ -1 * s V_mainGtU__tmp3 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU__tmp2 <= 0)%Z
   | 7 => (-1 * s V_mainGtU__tmp2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU__tmp3 <= 0)%Z
   | 8 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU__tmp2 <= 0)%Z
   | 9 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 10 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 11 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 12 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 13 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 14 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 15 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 16 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 17 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 18 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 19 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 20 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 21 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 22 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 23 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 24 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 25 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 26 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 27 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 28 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 29 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 30 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 31 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 32 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 33 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 34 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 35 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 36 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 37 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 38 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 39 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 40 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 41 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 42 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 43 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 44 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 45 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 46 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 47 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 48 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 49 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 50 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 51 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 52 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 53 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 54 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 55 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 56 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 57 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 58 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 59 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 60 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 61 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 62 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 63 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 64 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 65 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 66 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 67 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 68 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 69 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 70 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 71 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 72 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 73 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 74 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 75 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 76 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 77 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 78 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 79 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 80 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 81 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 82 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 83 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 84 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 85 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 86 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 87 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 88 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 89 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 90 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 91 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 92 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 93 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 94 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 95 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 96 => (-1 * s V_mainGtU_z <= 0)%Z
   | 97 => (-1 * s V_mainGtU_z <= 0)%Z
   | 98 => (-1 * s V_mainGtU_z <= 0)%Z
   | 99 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 100 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 101 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 102 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 103 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 104 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 105 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 106 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 107 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 108 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 109 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 110 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 111 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 112 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 113 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 114 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 115 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 116 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 117 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 118 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 119 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 120 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 121 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 122 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 123 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 124 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 125 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 126 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 127 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 128 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 129 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 130 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 131 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 132 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 133 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 134 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 135 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 136 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 137 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 138 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 139 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 140 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 141 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 142 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 143 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 144 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 145 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 146 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 147 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 148 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 149 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 150 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 151 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 152 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 153 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 154 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 155 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 156 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 157 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 158 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 159 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 160 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 161 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 162 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 163 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 164 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 165 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 166 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 167 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 168 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 169 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 170 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 171 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 172 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 173 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 174 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 175 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 176 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 177 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 178 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 179 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 180 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 181 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 182 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 183 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 184 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 185 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 186 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 187 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 188 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 189 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 190 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 191 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 192 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 193 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU__tmp1+ -1 * s V_mainGtU__tmp2 + 1 <= 0)%Z
   | 194 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU__tmp1+ 1 * s V_mainGtU__tmp2 <= 0)%Z
   | 195 => (-1 * s V_mainGtU__tmp1+ 1 * s V_mainGtU__tmp2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 196 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU__tmp1 <= 0)%Z
   | 197 => (-1 * s V_mainGtU__tmp1 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 198 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 199 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU__tmp2+ 1 * s V_mainGtU__tmp3 + 1 <= 0)%Z
   | 200 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU__tmp2+ -1 * s V_mainGtU__tmp3 <= 0)%Z
   | 201 => (1 * s V_mainGtU__tmp2+ -1 * s V_mainGtU__tmp3 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 202 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU__tmp3 <= 0)%Z
   | 203 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 204 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 205 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 206 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 207 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 208 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_k + 1 <= 0)%Z
   | 209 => (1 * s V_mainGtU_k + 1 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 210 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_k + 1 <= 0 /\ 1 * s V_mainGtU__tmp <= 0 /\ -1 * s V_mainGtU__tmp <= 0)%Z
   | 211 => (-1 * s V_mainGtU__tmp <= 0 /\ 1 * s V_mainGtU__tmp <= 0 /\ 1 * s V_mainGtU_k + 1 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 212 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_k <= 0)%Z
   | 213 => (-1 * s V_mainGtU_k <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 214 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_k <= 0)%Z
   | 215 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 216 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 217 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 218 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 219 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 220 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 221 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 222 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 223 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 224 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 225 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 226 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 227 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 228 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 229 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 230 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 231 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 232 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 233 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 234 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 235 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 236 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 237 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 238 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 239 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 240 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 241 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 242 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 243 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 244 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 245 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 246 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 247 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 248 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 249 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 250 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 251 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 252 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 253 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 254 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 255 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 256 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 257 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 258 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 259 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 260 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 261 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 262 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 263 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 264 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 265 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 266 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 267 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 268 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 269 => (-1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 270 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_s1+ -1 * s V_mainGtU_s2 <= 0 /\ -1 * s V_mainGtU_s1+ 1 * s V_mainGtU_s2 <= 0)%Z
   | 271 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 272 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 273 => (-1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 274 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_c1+ -1 * s V_mainGtU_c2 <= 0 /\ -1 * s V_mainGtU_c1+ 1 * s V_mainGtU_c2 <= 0)%Z
   | 275 => (-1 * s V_mainGtU_z <= 0)%Z
   | 276 => (-1 * s V_mainGtU_z <= 0)%Z
   | 277 => (-1 * s V_mainGtU_z <= 0)%Z
   | 278 => (-1 * s V_mainGtU_z <= 0)%Z
   | 279 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 280 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 281 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 282 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 283 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 284 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 285 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 286 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 287 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 288 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 289 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 290 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 291 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 292 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 293 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 294 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 295 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 296 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 297 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 298 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 299 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 300 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 301 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 302 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 303 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 304 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 305 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 306 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 307 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 308 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 309 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 310 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 311 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 312 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 313 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 314 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 315 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 316 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 317 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 318 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 319 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 320 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 321 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 322 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 323 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 324 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 325 => (-1 * s V_mainGtU_z <= 0 /\ 1 * s V_mainGtU_z <= 0)%Z
   | 326 => (1 * s V_mainGtU_z <= 0 /\ -1 * s V_mainGtU_z <= 0)%Z
   | 327 => (-1 * s V_mainGtU_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mainGtU (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 8) * max0(8 + s V_mainGtU_nblock) <= z)%Q
   | 2 => ((1 # 8) * max0(8 + s V_mainGtU_nblock) <= z)%Q
   | 3 => ((1 # 8) * max0(8 + s V_mainGtU_nblock) <= z)%Q
   | 4 => ((1 # 8) * max0(8 + s V_mainGtU_nblock) <= z)%Q
   | 5 => ((1 # 8) * max0(8 + s V_mainGtU_nblock) <= z)%Q
   | 6 => ((1 # 8) * max0(8 + s V_mainGtU_nblock) <= z)%Q
   | 7 => ((1 # 8) * max0(8 + s V_mainGtU_nblock) <= z)%Q
   | 8 => ((1 # 8) * max0(8 + s V_mainGtU_nblock) <= z)%Q
   | 9 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 10 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 11 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 12 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 13 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 14 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 15 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 16 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 17 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 18 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 19 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 20 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 21 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 22 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 23 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 24 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 25 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 26 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 27 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 28 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 29 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 30 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 31 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 32 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 33 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 34 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 35 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 36 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 37 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 38 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 39 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 40 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 41 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 42 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 43 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 44 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 45 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 46 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 47 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 48 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 49 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 50 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 51 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 52 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 53 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 54 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 55 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 56 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 57 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 58 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 59 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 60 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 61 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 62 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 63 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 64 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 65 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 66 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 67 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 68 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 69 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 70 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 71 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 72 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 73 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 74 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 75 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 76 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 77 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 78 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 79 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 80 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 81 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 82 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 83 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 84 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 85 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 86 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 87 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 88 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 89 => ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 90 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 91 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 92 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 93 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 94 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 95 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 96 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 97 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 98 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 99 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 100 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 101 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 102 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 103 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 104 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 105 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 106 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 107 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 108 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 109 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 110 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 111 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 112 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 113 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 114 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 115 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 116 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 117 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 118 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 119 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 120 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 121 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 122 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 123 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 124 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 125 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 126 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 127 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 128 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 129 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 130 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 131 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 132 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 133 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 134 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 135 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 136 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 137 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 138 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 139 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 140 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 141 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 142 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 143 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 144 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 145 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 146 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 147 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 148 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 149 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 150 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 151 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 152 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 153 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 154 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 155 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 156 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 157 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 158 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 159 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 160 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 161 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 162 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 163 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 164 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 165 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 166 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 167 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 168 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 169 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 170 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 171 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 172 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 173 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 174 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 175 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 176 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mainGtU_z) (0))) (F_max0_ge_0 (s V_mainGtU_z))]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 177 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 178 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 179 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 180 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 181 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 182 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 183 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 184 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 185 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 186 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 187 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 188 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 189 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 190 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 191 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mainGtU_z)) (F_check_ge (s V_mainGtU_z) (0))]
     ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 192 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 193 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 194 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 195 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 196 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 197 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 198 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 199 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 200 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 201 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 202 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 203 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 204 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU_k) <= z)%Q
   | 205 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU_k) <= z)%Q
   | 206 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU_k) <= z)%Q
   | 207 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU_k) <= z)%Q
   | 208 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU_k) <= z)%Q
   | 209 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU_k) <= z)%Q
   | 210 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU_k) <= z)%Q
   | 211 => hints
     [(*-0.125 0*) F_max0_monotonic (F_check_ge (8 + s V_mainGtU_k) (s V_mainGtU_k));
      (*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU_k) <= z)%Q
   | 212 => hints
     [(*-0.125 0*) F_max0_pre_decrement 1 (8 + s V_mainGtU_k) (8)]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU_k) <= z)%Q
   | 213 => ((1 # 1) + s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 214 => ((1 # 1) + s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 215 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 216 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 217 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 218 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mainGtU_z)) (F_check_ge (s V_mainGtU_z) (0))]
     ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 219 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 220 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 221 => ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 222 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mainGtU_z)) (F_check_ge (s V_mainGtU_z) (0))]
     ((1 # 8) * max0(s V_mainGtU_k) + max0(s V_mainGtU_z) <= z)%Q
   | 223 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 224 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 225 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 226 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 227 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 228 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 229 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 230 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 231 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 232 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 233 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 234 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 235 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 236 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 237 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 238 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 239 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 240 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 241 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 242 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 243 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 244 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 245 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 246 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 247 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 248 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 249 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 250 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 251 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 252 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 253 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 254 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 255 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 256 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 257 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 258 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 259 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 260 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 261 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 262 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 263 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 264 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 265 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 266 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 267 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 268 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 269 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 270 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 271 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 272 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 273 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 274 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 275 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 276 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 277 => (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 278 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mainGtU_k)]
     (s V_mainGtU_z + (1 # 8) * max0(s V_mainGtU_k) <= z)%Q
   | 279 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 280 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 281 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 282 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 + s V_mainGtU__tmp2)) (F_check_ge (0) (0))]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
      + max0(-s V_mainGtU_z) <= z)%Q
   | 283 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 284 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 285 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 286 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 + s V_mainGtU__tmp2)) (F_check_ge (0) (0))]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
      + max0(-s V_mainGtU_z) <= z)%Q
   | 287 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 288 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 289 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 290 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 + s V_mainGtU__tmp2)) (F_check_ge (0) (0))]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
      + max0(-s V_mainGtU_z) <= z)%Q
   | 291 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 292 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 293 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 294 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 + s V_mainGtU__tmp2)) (F_check_ge (0) (0))]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
      + max0(-s V_mainGtU_z) <= z)%Q
   | 295 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 296 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 297 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 298 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 + s V_mainGtU__tmp2)) (F_check_ge (0) (0))]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
      + max0(-s V_mainGtU_z) <= z)%Q
   | 299 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 300 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 301 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 302 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 + s V_mainGtU__tmp2)) (F_check_ge (0) (0))]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
      + max0(-s V_mainGtU_z) <= z)%Q
   | 303 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 304 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 305 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 306 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 + s V_mainGtU__tmp2)) (F_check_ge (0) (0))]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
      + max0(-s V_mainGtU_z) <= z)%Q
   | 307 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 308 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 309 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 310 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 + s V_mainGtU__tmp2)) (F_check_ge (0) (0))]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
      + max0(-s V_mainGtU_z) <= z)%Q
   | 311 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 312 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 313 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 314 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 + s V_mainGtU__tmp2)) (F_check_ge (0) (0))]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
      + max0(-s V_mainGtU_z) <= z)%Q
   | 315 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 316 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 317 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 318 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 + s V_mainGtU__tmp2)) (F_check_ge (0) (0))]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
      + max0(-s V_mainGtU_z) <= z)%Q
   | 319 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 320 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 321 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 322 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 + s V_mainGtU__tmp2)) (F_check_ge (0) (0))]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
      + max0(-s V_mainGtU_z) <= z)%Q
   | 323 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mainGtU_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mainGtU_z))]
     ((1 # 8) * max0(8 + s V_mainGtU__tmp2) <= z)%Q
   | 324 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 325 => (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
             + max0(-s V_mainGtU_z) <= z)%Q
   | 326 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mainGtU_z)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 + s V_mainGtU__tmp2)) (F_check_ge (0) (0))]
     (s V_mainGtU_z + (1 # 8) * max0(8 + s V_mainGtU__tmp2)
      + max0(-s V_mainGtU_z) <= z)%Q
   | 327 => (s V_mainGtU_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mainGtU =>
    [mkPA Q (fun n z s => ai_mainGtU n s /\ annot0_mainGtU n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mainGtU (proc_start P_mainGtU) s1 (proc_end P_mainGtU) s2 ->
    (s2 V_mainGtU_z <= (1 # 8) * max0(8 + s1 V_mainGtU_nblock))%Q.
Proof.
  prove_bound ipa admissible_ipa P_mainGtU.
Qed.
