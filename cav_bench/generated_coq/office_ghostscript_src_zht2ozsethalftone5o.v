Require Import pasta.Pasta.

Inductive proc: Type :=
  P_zsethalftone5.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_zsethalftone5_z := 1%positive.
Notation V_zsethalftone5__tmp := 2%positive.
Notation V_zsethalftone5_code := 3%positive.
Notation V_zsethalftone5_count := 4%positive.
Notation V_zsethalftone5_edepth := 5%positive.
Notation V_zsethalftone5_es_code_ := 6%positive.
Notation V_zsethalftone5_es_code_4 := 7%positive.
Notation V_zsethalftone5_i := 8%positive.
Notation V_zsethalftone5_j := 9%positive.
Notation V_zsethalftone5_npop := 10%positive.
Notation V_zsethalftone5_odepth := 11%positive.
Notation V_zsethalftone5_op := 12%positive.
Definition Pedges_zsethalftone5: list (edge proc) :=
  (EA 1 (AAssign V_zsethalftone5_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_j) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_zsethalftone5_i) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_count) s) >= (eval (ENum (0))
  s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 (AAssign V_zsethalftone5_code
  (Some (ENum (0)))) 7)::(EA 7 (AAssign V_zsethalftone5_npop
  (Some (ENum (2)))) 8)::(EA 8 AWeaken 9)::(EA 9 ANone 13)::(EA 9 ANone 10)::
  (EA 10 (AAssign V_zsethalftone5__tmp None) 11)::(EA 11 ANone 12)::
  (EA 12 AWeaken 228)::(EA 13 AWeaken 14)::(EA 14 ANone 18)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_zsethalftone5__tmp
  (Some (ENum (-7)))) 16)::(EA 16 ANone 17)::(EA 17 AWeaken 228)::
  (EA 18 AWeaken 19)::(EA 19 ANone 23)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_zsethalftone5__tmp None) 21)::(EA 21 ANone 22)::(EA 22 AWeaken 228)::
  (EA 23 AWeaken 24)::(EA 24 ANone 28)::(EA 24 ANone 25)::(EA 25 (AAssign
  V_zsethalftone5__tmp (Some (ENum (-7)))) 26)::(EA 26 ANone 27)::
  (EA 27 AWeaken 228)::(EA 28 (AAssign V_zsethalftone5_count
  (Some (ENum (0)))) 29)::(EA 29 (AAssign V_zsethalftone5_i
  (Some (ENum (0)))) 30)::(EA 30 ANone 31)::(EA 31 AWeaken 32)::
  (EA 32 (AGuard (fun s => ((eval (EVar V_zsethalftone5_i) s) <
  (eval (ENum (9)) s))%Z)) 218)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_i) s) >= (eval (ENum (9))
  s))%Z)) 33)::(EA 33 AWeaken 34)::(EA 34 ANone 35)::(EA 34 ANone 40)::
  (EA 35 (AAssign V_zsethalftone5_es_code_ None) 36)::(EA 36 AWeaken 37)::
  (EA 37 (AGuard (fun s => ((eval (EVar V_zsethalftone5_es_code_) s) <
  (eval (ENum (0)) s))%Z)) 214)::(EA 37 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_es_code_) s) >= (eval (ENum (0))
  s))%Z)) 38)::(EA 38 AWeaken 39)::(EA 39 ANone 40)::(EA 40 ANone 41)::
  (EA 41 AWeaken 42)::(EA 42 ANone 44)::(EA 42 ANone 43)::(EA 43 ANone 45)::
  (EA 44 ANone 45)::(EA 45 ANone 46)::(EA 46 ANone 47)::(EA 47 AWeaken 48)::
  (EA 48 ANone 50)::(EA 48 ANone 49)::(EA 49 ANone 51)::(EA 50 ANone 51)::
  (EA 51 ANone 52)::(EA 52 AWeaken 53)::(EA 53 ANone 105)::(EA 53 ANone 54)::
  (EA 54 AWeaken 55)::(EA 55 ANone 105)::(EA 55 ANone 56)::
  (EA 56 AWeaken 57)::(EA 57 ANone 105)::(EA 57 ANone 58)::(EA 58 (AAssign
  V_zsethalftone5_i (Some (ENum (0)))) 59)::(EA 59 (AAssign V_zsethalftone5_j
  (Some (ENum (0)))) 60)::(EA 60 ANone 61)::(EA 61 AWeaken 62)::
  (EA 62 (AGuard (fun s => ((eval (EVar V_zsethalftone5_i) s) <
  (eval (ENum (9)) s))%Z)) 64)::(EA 62 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_i) s) >= (eval (ENum (9))
  s))%Z)) 63)::(EA 63 AWeaken 103)::(EA 64 AWeaken 65)::(EA 65 ANone 66)::
  (EA 65 ANone 93)::(EA 66 AWeaken 67)::(EA 67 ANone 71)::(EA 67 ANone 68)::
  (EA 68 (AAssign V_zsethalftone5__tmp (Some (ENum (-20)))) 69)::
  (EA 69 ANone 70)::(EA 70 AWeaken 228)::(EA 71 AWeaken 72)::
  (EA 72 ANone 76)::(EA 72 ANone 73)::(EA 73 (AAssign V_zsethalftone5__tmp
  (Some (ENum (-7)))) 74)::(EA 74 ANone 75)::(EA 75 AWeaken 228)::
  (EA 76 AWeaken 77)::(EA 77 ANone 101)::(EA 77 ANone 78)::
  (EA 78 AWeaken 79)::(EA 79 ANone 86)::(EA 79 ANone 83)::(EA 79 ANone 80)::
  (EA 80 (AAssign V_zsethalftone5_code None) 81)::(EA 81 ANone 82)::
  (EA 82 AWeaken 89)::(EA 83 (AAssign V_zsethalftone5_code None) 84)::
  (EA 84 ANone 85)::(EA 85 AWeaken 89)::(EA 86 (AAssign V_zsethalftone5_code
  (Some (ENum (-15)))) 87)::(EA 87 ANone 88)::(EA 88 AWeaken 89)::
  (EA 89 (AGuard (fun s => ((eval (EVar V_zsethalftone5_code) s) <
  (eval (ENum (0)) s))%Z)) 99)::(EA 89 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_code) s) >= (eval (ENum (0))
  s))%Z)) 90)::(EA 90 AWeaken 91)::(EA 91 (AAssign V_zsethalftone5_j
  (Some (EAdd (EVar V_zsethalftone5_j) (ENum (1))))) 92)::(EA 92 ANone 93)::
  (EA 93 ANone 94)::(EA 94 (AAssign V_zsethalftone5_i
  (Some (EAdd (EVar V_zsethalftone5_i) (ENum (1))))) 95)::(EA 95 ANone 96)::
  (EA 96 ANone 97)::(EA 97 (AAssign V_zsethalftone5_z (Some (EAdd (ENum (1))
  (EVar V_zsethalftone5_z)))) 98)::(EA 98 AWeaken 62)::(EA 99 AWeaken 100)::
  (EA 100 ANone 103)::(EA 101 (AAssign V_zsethalftone5_code
  (Some (ENum (-20)))) 102)::(EA 102 ANone 103)::(EA 103 ANone 104)::
  (EA 104 AWeaken 108)::(EA 105 (AAssign V_zsethalftone5_code
  (Some (ENum (-25)))) 106)::(EA 106 ANone 107)::(EA 107 AWeaken 108)::
  (EA 108 (AGuard (fun s => ((eval (EVar V_zsethalftone5_code) s) >=
  (eval (ENum (0)) s))%Z)) 110)::(EA 108 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_code) s) < (eval (ENum (0))
  s))%Z)) 109)::(EA 109 AWeaken 117)::(EA 110 AWeaken 111)::
  (EA 111 ANone 116)::(EA 111 ANone 112)::(EA 112 ANone 113)::
  (EA 113 (AAssign V_zsethalftone5_code None) 114)::(EA 114 ANone 115)::
  (EA 115 AWeaken 117)::(EA 116 AWeaken 117)::(EA 117 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_code) s) >= (eval (ENum (0))
  s))%Z)) 119)::(EA 117 (AGuard (fun s => ((eval (EVar V_zsethalftone5_code)
  s) < (eval (ENum (0)) s))%Z)) 118)::(EA 118 AWeaken 142)::
  (EA 119 AWeaken 120)::(EA 120 (AAssign V_zsethalftone5_j
  (Some (ENum (0)))) 121)::(EA 121 ANone 122)::(EA 122 AWeaken 123)::
  (EA 123 (AGuard (fun s => ((eval (EVar V_zsethalftone5_j) s) <
  (eval (EVar V_zsethalftone5_count) s))%Z)) 125)::(EA 123 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_j) s) >=
  (eval (EVar V_zsethalftone5_count) s))%Z)) 124)::(EA 124 AWeaken 140)::
  (EA 125 AWeaken 126)::(EA 126 ANone 127)::(EA 126 ANone 132)::
  (EA 127 (AAssign V_zsethalftone5_code None) 128)::(EA 128 AWeaken 129)::
  (EA 129 (AGuard (fun s => ((eval (EVar V_zsethalftone5_code) s) <
  (eval (ENum (0)) s))%Z)) 138)::(EA 129 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_code) s) >= (eval (ENum (0))
  s))%Z)) 130)::(EA 130 AWeaken 131)::(EA 131 ANone 132)::
  (EA 132 ANone 133)::(EA 133 (AAssign V_zsethalftone5_j
  (Some (EAdd (EVar V_zsethalftone5_j) (ENum (1))))) 134)::
  (EA 134 ANone 135)::(EA 135 ANone 136)::(EA 136 (AAssign V_zsethalftone5_z
  (Some (EAdd (ENum (1)) (EVar V_zsethalftone5_z)))) 137)::
  (EA 137 AWeaken 123)::(EA 138 AWeaken 139)::(EA 139 ANone 140)::
  (EA 140 ANone 141)::(EA 141 AWeaken 142)::(EA 142 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_code) s) >= (eval (ENum (0))
  s))%Z)) 144)::(EA 142 (AGuard (fun s => ((eval (EVar V_zsethalftone5_code)
  s) < (eval (ENum (0)) s))%Z)) 143)::(EA 143 AWeaken 205)::
  (EA 144 AWeaken 145)::(EA 145 (AAssign V_zsethalftone5_edepth None) 146)::
  (EA 146 (AAssign V_zsethalftone5_odepth None) 147)::(EA 147 (AAssign
  V_zsethalftone5_j (Some (ENum (0)))) 148)::(EA 148 ANone 149)::
  (EA 149 AWeaken 150)::(EA 150 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_j) s) <
  (eval (EVar V_zsethalftone5_count) s))%Z)) 152)::(EA 150 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_j) s) >=
  (eval (EVar V_zsethalftone5_count) s))%Z)) 151)::(EA 151 AWeaken 203)::
  (EA 152 AWeaken 153)::(EA 153 ANone 156)::(EA 153 ANone 154)::
  (EA 154 ANone 155)::(EA 155 AWeaken 158)::(EA 156 ANone 157)::
  (EA 157 AWeaken 158)::(EA 158 ANone 190)::(EA 158 ANone 160)::
  (EA 158 ANone 159)::(EA 159 AWeaken 166)::(EA 160 (AAssign
  V_zsethalftone5_code None) 161)::(EA 161 AWeaken 162)::(EA 162 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_code) s) < (eval (ENum (0))
  s))%Z)) 187)::(EA 162 (AGuard (fun s => ((eval (EVar V_zsethalftone5_code)
  s) >= (eval (ENum (0)) s))%Z)) 163)::(EA 163 AWeaken 164)::
  (EA 164 ANone 165)::(EA 165 AWeaken 166)::(EA 166 ANone 185)::
  (EA 166 ANone 167)::(EA 167 AWeaken 168)::(EA 168 ANone 182)::
  (EA 168 ANone 169)::(EA 169 AWeaken 170)::(EA 170 ANone 171)::
  (EA 170 ANone 176)::(EA 171 (AAssign V_zsethalftone5_es_code_4 None) 172)::
  (EA 172 AWeaken 173)::(EA 173 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_es_code_4) s) < (eval (ENum (0))
  s))%Z)) 178)::(EA 173 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_es_code_4) s) >= (eval (ENum (0))
  s))%Z)) 174)::(EA 174 AWeaken 175)::(EA 175 ANone 176)::(EA 176 (AAssign
  V_zsethalftone5_code None) 177)::(EA 177 ANone 185)::(EA 178 AWeaken 179)::
  (EA 179 (AAssign V_zsethalftone5__tmp
  (Some (EVar V_zsethalftone5_es_code_4))) 180)::(EA 180 ANone 181)::
  (EA 181 AWeaken 228)::(EA 182 (AAssign V_zsethalftone5__tmp
  (Some (ENum (-16)))) 183)::(EA 183 ANone 184)::(EA 184 AWeaken 228)::
  (EA 185 ANone 186)::(EA 186 AWeaken 192)::(EA 187 AWeaken 188)::
  (EA 188 ANone 189)::(EA 189 AWeaken 192)::(EA 190 ANone 191)::
  (EA 191 AWeaken 192)::(EA 192 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_code) s) < (eval (ENum (0))
  s))%Z)) 201)::(EA 192 (AGuard (fun s => ((eval (EVar V_zsethalftone5_code)
  s) >= (eval (ENum (0)) s))%Z)) 193)::(EA 193 AWeaken 194)::(EA 194 (AAssign
  V_zsethalftone5_npop (Some (ENum (0)))) 195)::(EA 195 ANone 196)::
  (EA 196 (AAssign V_zsethalftone5_j (Some (EAdd (EVar V_zsethalftone5_j)
  (ENum (1))))) 197)::(EA 197 ANone 198)::(EA 198 ANone 199)::
  (EA 199 (AAssign V_zsethalftone5_z (Some (EAdd (ENum (1))
  (EVar V_zsethalftone5_z)))) 200)::(EA 200 AWeaken 150)::
  (EA 201 AWeaken 202)::(EA 202 ANone 203)::(EA 203 ANone 204)::
  (EA 204 AWeaken 205)::(EA 205 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_code) s) < (eval (ENum (0))
  s))%Z)) 210)::(EA 205 (AGuard (fun s => ((eval (EVar V_zsethalftone5_code)
  s) >= (eval (ENum (0)) s))%Z)) 206)::(EA 206 AWeaken 207)::(EA 207 (AAssign
  V_zsethalftone5__tmp (Some (ENum (5)))) 208)::(EA 208 ANone 209)::
  (EA 209 AWeaken 228)::(EA 210 AWeaken 211)::(EA 211 (AAssign
  V_zsethalftone5__tmp (Some (EVar V_zsethalftone5_code))) 212)::
  (EA 212 ANone 213)::(EA 213 AWeaken 228)::(EA 214 AWeaken 215)::
  (EA 215 (AAssign V_zsethalftone5__tmp
  (Some (EVar V_zsethalftone5_es_code_))) 216)::(EA 216 ANone 217)::
  (EA 217 AWeaken 228)::(EA 218 AWeaken 219)::(EA 219 ANone 229)::
  (EA 219 ANone 220)::(EA 220 AWeaken 221)::(EA 221 (AGuard
  (fun s => ((eval (EVar V_zsethalftone5_i) s) = (eval (ENum (0))
  s))%Z)) 224)::(EA 221 (AGuard (fun s => ((eval (EVar V_zsethalftone5_i)
  s) <> (eval (ENum (0)) s))%Z)) 222)::(EA 222 AWeaken 223)::
  (EA 223 ANone 231)::(EA 224 AWeaken 225)::(EA 225 (AAssign
  V_zsethalftone5__tmp (Some (ENum (-20)))) 226)::(EA 226 ANone 227)::
  (EA 227 AWeaken 228)::(EA 229 (AAssign V_zsethalftone5_count
  (Some (EAdd (EVar V_zsethalftone5_count) (ENum (1))))) 230)::
  (EA 230 ANone 231)::(EA 231 ANone 232)::(EA 232 (AAssign V_zsethalftone5_i
  (Some (EAdd (EVar V_zsethalftone5_i) (ENum (1))))) 233)::
  (EA 233 ANone 234)::(EA 234 ANone 235)::(EA 235 (AAssign V_zsethalftone5_z
  (Some (EAdd (ENum (1)) (EVar V_zsethalftone5_z)))) 236)::
  (EA 236 AWeaken 32)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_zsethalftone5 => Pedges_zsethalftone5
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_zsethalftone5 => 228
     end)%positive;
  var_global := var_global
}.

Definition ai_zsethalftone5 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0)%Z
   | 3 => (-1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0)%Z
   | 4 => (-1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 5 => (-1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0)%Z
   | 6 => (-1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 7 => (-1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 8 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 9 => (-1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 10 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 11 => (-1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 12 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 13 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 14 => (-1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 15 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 16 => (-1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5__tmp + 7 <= 0 /\ -1 * s V_zsethalftone5__tmp + -7 <= 0)%Z
   | 17 => (-1 * s V_zsethalftone5__tmp + -7 <= 0 /\ 1 * s V_zsethalftone5__tmp + 7 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 18 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 19 => (-1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 20 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 21 => (-1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 22 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 23 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 24 => (-1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 25 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 26 => (-1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5__tmp + 7 <= 0 /\ -1 * s V_zsethalftone5__tmp + -7 <= 0)%Z
   | 27 => (-1 * s V_zsethalftone5__tmp + -7 <= 0 /\ 1 * s V_zsethalftone5__tmp + 7 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 28 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0)%Z
   | 29 => (-1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_count <= 0)%Z
   | 30 => (-1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 31 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_count <= 0)%Z
   | 32 => (-1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 33 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 34 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 35 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 36 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 37 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 38 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_es_code_ <= 0)%Z
   | 39 => (-1 * s V_zsethalftone5_es_code_ <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 40 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 41 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 42 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 43 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 44 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 45 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 46 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 47 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 48 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 49 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 50 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 51 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 52 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 53 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 54 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 55 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 56 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 57 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 58 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 59 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 60 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_j <= 0)%Z
   | 61 => (-1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 62 => (-1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 63 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 64 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 65 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 66 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 67 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 68 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 69 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5__tmp + 20 <= 0 /\ -1 * s V_zsethalftone5__tmp + -20 <= 0)%Z
   | 70 => (-1 * s V_zsethalftone5__tmp + -20 <= 0 /\ 1 * s V_zsethalftone5__tmp + 20 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 71 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 72 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 73 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 74 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5__tmp + 7 <= 0 /\ -1 * s V_zsethalftone5__tmp + -7 <= 0)%Z
   | 75 => (-1 * s V_zsethalftone5__tmp + -7 <= 0 /\ 1 * s V_zsethalftone5__tmp + 7 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 76 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 77 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 78 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 79 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 80 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 81 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 82 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 83 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 84 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 85 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 86 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 87 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_code + 15 <= 0 /\ -1 * s V_zsethalftone5_code + -15 <= 0)%Z
   | 88 => (-1 * s V_zsethalftone5_code + -15 <= 0 /\ 1 * s V_zsethalftone5_code + 15 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 89 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 90 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 91 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 92 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 93 => (-1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 94 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0)%Z
   | 95 => (-1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_i + 1 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 96 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0)%Z
   | 97 => (-1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_i + 1 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 98 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z + 1 <= 0)%Z
   | 99 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0 /\ 1 * s V_zsethalftone5_code + 1 <= 0)%Z
   | 100 => (1 * s V_zsethalftone5_code + 1 <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 101 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 102 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_code + 20 <= 0 /\ -1 * s V_zsethalftone5_code + -20 <= 0)%Z
   | 103 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 104 => (-1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 105 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 106 => (1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0 /\ 1 * s V_zsethalftone5_code + 25 <= 0 /\ -1 * s V_zsethalftone5_code + -25 <= 0)%Z
   | 107 => (-1 * s V_zsethalftone5_code + -25 <= 0 /\ 1 * s V_zsethalftone5_code + 25 <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0)%Z
   | 108 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0)%Z
   | 109 => (-1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_code + 1 <= 0)%Z
   | 110 => (-1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 111 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0)%Z
   | 112 => (-1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 113 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0)%Z
   | 114 => (-1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 115 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0)%Z
   | 116 => (-1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 117 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0)%Z
   | 118 => (-1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_code + 1 <= 0)%Z
   | 119 => (-1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 120 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0)%Z
   | 121 => (-1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_j <= 0)%Z
   | 122 => (-1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0)%Z
   | 123 => (-1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0)%Z
   | 124 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_count+ -1 * s V_zsethalftone5_j <= 0)%Z
   | 125 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 126 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 127 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 128 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 129 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 130 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 131 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 132 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 133 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 134 => (-1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0)%Z
   | 135 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0)%Z
   | 136 => (-1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0)%Z
   | 137 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_z + 1 <= 0)%Z
   | 138 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_code + 1 <= 0)%Z
   | 139 => (1 * s V_zsethalftone5_code + 1 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 140 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 141 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0)%Z
   | 142 => (-1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 143 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_code + 1 <= 0)%Z
   | 144 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 145 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 146 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 147 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 148 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_j <= 0)%Z
   | 149 => (-1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 150 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0)%Z
   | 151 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ 1 * s V_zsethalftone5_count+ -1 * s V_zsethalftone5_j <= 0)%Z
   | 152 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 153 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 154 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 155 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 156 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 157 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 158 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 159 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 160 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 161 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 162 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 163 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 164 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 165 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 166 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 167 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 168 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 169 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 170 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 171 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 172 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 173 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 174 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_es_code_4 <= 0)%Z
   | 175 => (-1 * s V_zsethalftone5_es_code_4 <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 176 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 177 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 178 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ 1 * s V_zsethalftone5_es_code_4 + 1 <= 0)%Z
   | 179 => (1 * s V_zsethalftone5_es_code_4 + 1 <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 180 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ 1 * s V_zsethalftone5_es_code_4 + 1 <= 0 /\ 1 * s V_zsethalftone5__tmp + 1 <= 0)%Z
   | 181 => (1 * s V_zsethalftone5__tmp + 1 <= 0 /\ 1 * s V_zsethalftone5_es_code_4 + 1 <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 182 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 183 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ 1 * s V_zsethalftone5__tmp + 16 <= 0 /\ -1 * s V_zsethalftone5__tmp + -16 <= 0)%Z
   | 184 => (-1 * s V_zsethalftone5__tmp + -16 <= 0 /\ 1 * s V_zsethalftone5__tmp + 16 <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 185 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 186 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 187 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_code + 1 <= 0)%Z
   | 188 => (1 * s V_zsethalftone5_code + 1 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 189 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_code + 1 <= 0)%Z
   | 190 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 191 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 192 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 193 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 194 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 195 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 196 => (-1 * s V_zsethalftone5_npop <= 0 /\ 1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 197 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 198 => (-1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ 1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0)%Z
   | 199 => (-1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_j + 1 <= 0)%Z
   | 200 => (-1 * s V_zsethalftone5_j + 1 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ 1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z + 1 <= 0)%Z
   | 201 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_code + 1 <= 0)%Z
   | 202 => (1 * s V_zsethalftone5_code + 1 <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j + 1 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0)%Z
   | 203 => (-1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 204 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_count+ 1 * s V_zsethalftone5_j <= 0)%Z
   | 205 => (-1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 206 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 207 => (-1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 208 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5__tmp + -5 <= 0 /\ -1 * s V_zsethalftone5__tmp + 5 <= 0)%Z
   | 209 => (-1 * s V_zsethalftone5__tmp + 5 <= 0 /\ 1 * s V_zsethalftone5__tmp + -5 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 210 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_code + 1 <= 0)%Z
   | 211 => (1 * s V_zsethalftone5_code + 1 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 212 => (1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ 1 * s V_zsethalftone5_code + 1 <= 0 /\ 1 * s V_zsethalftone5__tmp + 1 <= 0)%Z
   | 213 => (1 * s V_zsethalftone5__tmp + 1 <= 0 /\ 1 * s V_zsethalftone5_code + 1 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0)%Z
   | 214 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_es_code_ + 1 <= 0)%Z
   | 215 => (1 * s V_zsethalftone5_es_code_ + 1 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 216 => (-1 * s V_zsethalftone5_i + 9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ 1 * s V_zsethalftone5_es_code_ + 1 <= 0 /\ 1 * s V_zsethalftone5__tmp + 1 <= 0)%Z
   | 217 => (1 * s V_zsethalftone5__tmp + 1 <= 0 /\ 1 * s V_zsethalftone5_es_code_ + 1 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 9 <= 0)%Z
   | 218 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 219 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 220 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 221 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 222 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i + 1 <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 223 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_i + 1 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 224 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_i <= 0)%Z
   | 225 => (1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 226 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_i <= 0 /\ 1 * s V_zsethalftone5__tmp + 20 <= 0 /\ -1 * s V_zsethalftone5__tmp + -20 <= 0)%Z
   | 227 => (-1 * s V_zsethalftone5__tmp + -20 <= 0 /\ 1 * s V_zsethalftone5__tmp + 20 <= 0 /\ 1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0)%Z
   | 228 => (-1 * s V_zsethalftone5_npop <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0)%Z
   | 229 => (-1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 230 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count + 1 <= 0)%Z
   | 231 => (-1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_i + -8 <= 0)%Z
   | 232 => (1 * s V_zsethalftone5_i + -8 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_i <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0)%Z
   | 233 => (-1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i + 1 <= 0)%Z
   | 234 => (-1 * s V_zsethalftone5_i + 1 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0)%Z
   | 235 => (-1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ -1 * s V_zsethalftone5_z <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_i + 1 <= 0)%Z
   | 236 => (-1 * s V_zsethalftone5_i + 1 <= 0 /\ 1 * s V_zsethalftone5_i + -9 <= 0 /\ -1 * s V_zsethalftone5_npop + 2 <= 0 /\ 1 * s V_zsethalftone5_npop + -2 <= 0 /\ -1 * s V_zsethalftone5_j <= 0 /\ 1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_code <= 0 /\ -1 * s V_zsethalftone5_count <= 0 /\ -1 * s V_zsethalftone5_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_zsethalftone5 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((36 # 1) <= z)%Q
   | 2 => ((36 # 1) + s V_zsethalftone5_z <= z)%Q
   | 3 => ((36 # 1) + s V_zsethalftone5_z <= z)%Q
   | 4 => ((36 # 1) + s V_zsethalftone5_z <= z)%Q
   | 5 => ((36 # 1) + s V_zsethalftone5_z <= z)%Q
   | 6 => ((36 # 1) + s V_zsethalftone5_z <= z)%Q
   | 7 => ((36 # 1) - (36 # 5) * s V_zsethalftone5_code + s V_zsethalftone5_z
           + (36 # 5) * max0(s V_zsethalftone5_code) <= z)%Q
   | 8 => ((36 # 1) - (36 # 5) * s V_zsethalftone5_code + s V_zsethalftone5_z
           + (36 # 5) * max0(s V_zsethalftone5_code) <= z)%Q
   | 9 => ((36 # 1) - (36 # 5) * s V_zsethalftone5_code + s V_zsethalftone5_z
           + (36 # 5) * max0(s V_zsethalftone5_code) <= z)%Q
   | 10 => ((36 # 1) - (36 # 5) * s V_zsethalftone5_code
            + s V_zsethalftone5_z + (36 # 5) * max0(s V_zsethalftone5_code) <= z)%Q
   | 11 => ((36 # 1) - (36 # 5) * s V_zsethalftone5_code
            + s V_zsethalftone5_z + (36 # 5) * max0(s V_zsethalftone5_code) <= z)%Q
   | 12 => hints
     [(*0 7.2*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zsethalftone5_code)) (F_check_ge (0) (0));
      (*-7.2 0*) F_binom_monotonic 1 (F_max0_ge_0 (5 - s V_zsethalftone5_code)) (F_check_ge (0) (0));
      (*-7.2 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                 - s V_zsethalftone5_code) (0))) (F_max0_ge_0 (5
                                                                    - s V_zsethalftone5_code))]
     ((36 # 1) - (36 # 5) * s V_zsethalftone5_code + s V_zsethalftone5_z
      + (36 # 5) * max0(s V_zsethalftone5_code) <= z)%Q
   | 13 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zsethalftone5_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_z) (0))) (F_max0_ge_0 (s V_zsethalftone5_z));
      (*0 1.44*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (25
                                                                 + s V_zsethalftone5_code) (0))) (F_max0_ge_0 (25
                                                                    + s V_zsethalftone5_code))]
     ((36 # 1) - (36 # 5) * s V_zsethalftone5_code + s V_zsethalftone5_z
      + (36 # 5) * max0(s V_zsethalftone5_code) <= z)%Q
   | 14 => (-(216 # 25) * s V_zsethalftone5_code
            + (36 # 25) * max0(25 + s V_zsethalftone5_code)
            + (36 # 5) * max0(s V_zsethalftone5_code) <= z)%Q
   | 15 => (-(216 # 25) * s V_zsethalftone5_code
            + (36 # 25) * max0(25 + s V_zsethalftone5_code)
            + (36 # 5) * max0(s V_zsethalftone5_code) <= z)%Q
   | 16 => (-(36 # 1) - (216 # 25) * s V_zsethalftone5_code
            + (6 # 1) * max0(-1 - s V_zsethalftone5__tmp)
            + (36 # 25) * max0(25 + s V_zsethalftone5_code)
            + (36 # 5) * max0(s V_zsethalftone5_code) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zsethalftone5_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zsethalftone5_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zsethalftone5_z));
      (*-7.2 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsethalftone5_code)) (F_check_ge (s V_zsethalftone5_code) (0));
      (*-1.44 0*) F_binom_monotonic 1 (F_max0_ge_arg (25
                                                      + s V_zsethalftone5_code)) (F_check_ge (25
                                                                    + s V_zsethalftone5_code) (0));
      (*-6 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_zsethalftone5__tmp)) (F_check_ge (0) (0))]
     (-(36 # 1) - (216 # 25) * s V_zsethalftone5_code
      + (6 # 1) * max0(-1 - s V_zsethalftone5__tmp)
      + (36 # 25) * max0(25 + s V_zsethalftone5_code)
      + (36 # 5) * max0(s V_zsethalftone5_code) <= z)%Q
   | 18 => hints
     [(*0 7.2*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsethalftone5_code)) (F_check_ge (s V_zsethalftone5_code) (0));
      (*-1.44 0*) F_binom_monotonic 1 (F_max0_ge_arg (25
                                                      + s V_zsethalftone5_code)) (F_check_ge (25
                                                                    + s V_zsethalftone5_code) (0))]
     (-(216 # 25) * s V_zsethalftone5_code
      + (36 # 25) * max0(25 + s V_zsethalftone5_code)
      + (36 # 5) * max0(s V_zsethalftone5_code) <= z)%Q
   | 19 => ((36 # 1) <= z)%Q
   | 20 => ((36 # 1) <= z)%Q
   | 21 => ((36 # 1) <= z)%Q
   | 22 => hints
     [(*-36 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zsethalftone5_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zsethalftone5_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zsethalftone5_z))]
     ((36 # 1) <= z)%Q
   | 23 => ((36 # 1) <= z)%Q
   | 24 => ((36 # 1) <= z)%Q
   | 25 => ((36 # 1) <= z)%Q
   | 26 => ((6 # 1) * max0(-1 - s V_zsethalftone5__tmp) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zsethalftone5_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zsethalftone5_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zsethalftone5_z));
      (*-6 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_zsethalftone5__tmp)) (F_check_ge (0) (0))]
     ((6 # 1) * max0(-1 - s V_zsethalftone5__tmp) <= z)%Q
   | 28 => ((36 # 1) <= z)%Q
   | 29 => ((36 # 1) + (2 # 1) * s V_zsethalftone5_count <= z)%Q
   | 30 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zsethalftone5_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zsethalftone5_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zsethalftone5_z));
      (*-1.7 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                 + s V_zsethalftone5_code) (0))) (F_max0_ge_0 (20
                                                                    + s V_zsethalftone5_code))]
     ((9 # 1) + (2 # 1) * s V_zsethalftone5_count
      + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 32 => (-(25 # 1) - (17 # 10) * s V_zsethalftone5_code
            + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i)
            + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 33 => hints
     [(*0 1.7*) F_binom_monotonic 1 (F_max0_ge_arg (20
                                                    + s V_zsethalftone5_code)) (F_check_ge (20
                                                                    + s V_zsethalftone5_code) (0))]
     (-(25 # 1) - (17 # 10) * s V_zsethalftone5_code
      + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
      + (3 # 1) * max0(9 - s V_zsethalftone5_i)
      + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 34 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 35 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 36 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 37 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 38 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 39 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 40 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 41 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 42 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 43 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 44 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 45 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 46 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
            + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 47 => hints
     [(*0 4.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_npop) (0))) (F_max0_ge_0 (s V_zsethalftone5_npop));
      (*0 2*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_count) (0))) (F_max0_ge_0 (s V_zsethalftone5_count));
      (*0 4.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                - s V_zsethalftone5_npop) (0))) (F_max0_ge_0 (2
                                                                    - s V_zsethalftone5_npop))]
     ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
      + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 48 => (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
            + (3 # 1) * max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 49 => (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
            + (3 # 1) * max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 50 => (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
            + (3 # 1) * max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 51 => (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
            + (3 # 1) * max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 52 => hints
     [(*-3 0*) F_max0_monotonic (F_check_ge (9 - s V_zsethalftone5_i) (8
                                                                    - s V_zsethalftone5_i));
      (*-3 0*) F_max0_ge_0 (8 - s V_zsethalftone5_i)]
     (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
      + (3 # 1) * max0(9 - s V_zsethalftone5_i)
      + (2 # 1) * max0(s V_zsethalftone5_count)
      + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 53 => (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 54 => (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 55 => (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 56 => (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 57 => (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 58 => (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 59 => (-(9 # 1) + s V_zsethalftone5_z
            + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
            + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 60 => (-(9 # 1) + s V_zsethalftone5_z
            + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
            + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 61 => hints
     [(*-4.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsethalftone5_npop)) (F_check_ge (s V_zsethalftone5_npop) (0));
      (*-4.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                     - s V_zsethalftone5_npop)) (F_check_ge (2
                                                                    - s V_zsethalftone5_npop) (0))]
     (-(9 # 1) + s V_zsethalftone5_z
      + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
      + max0(9 - s V_zsethalftone5_i)
      + (2 # 1) * max0(s V_zsethalftone5_count)
      + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 62 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 63 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 64 => hints
     [(*-1 9.21579e-12*) F_max0_pre_decrement 1 (9 - s V_zsethalftone5_i) (1)]
     (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
      + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 65 => ((1 # 1) + s V_zsethalftone5_z + max0(8 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 66 => hints
     [(*0 0.125*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_i) (0))) (F_max0_ge_0 (s V_zsethalftone5_i))]
     ((1 # 1) + s V_zsethalftone5_z + max0(8 - s V_zsethalftone5_i)
      + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 67 => ((1 # 1) - (1 # 8) * s V_zsethalftone5_i + s V_zsethalftone5_z
            + max0(8 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (1 # 8) * max0(s V_zsethalftone5_i) <= z)%Q
   | 68 => ((1 # 1) - (1 # 8) * s V_zsethalftone5_i + s V_zsethalftone5_z
            + max0(8 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (1 # 8) * max0(s V_zsethalftone5_i) <= z)%Q
   | 69 => ((1 # 1) - (1 # 8) * s V_zsethalftone5_i + s V_zsethalftone5_z
            + max0(8 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count)
            + (1 # 8) * max0(s V_zsethalftone5_i) <= z)%Q
   | 70 => hints
     [(*-1.125 0*) F_max0_ge_0 (8 - s V_zsethalftone5_i);
      (*-2 0*) F_max0_ge_0 (s V_zsethalftone5_count);
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zsethalftone5_i)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                   - 
                                                                   s V_zsethalftone5_i) (0))) (F_max0_ge_0 (8
                                                                    - s V_zsethalftone5_i))]
     ((1 # 1) - (1 # 8) * s V_zsethalftone5_i + s V_zsethalftone5_z
      + max0(8 - s V_zsethalftone5_i)
      + (2 # 1) * max0(s V_zsethalftone5_count)
      + (1 # 8) * max0(s V_zsethalftone5_i) <= z)%Q
   | 71 => hints
     [(*-1 -0.875*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsethalftone5_i)) (F_check_ge (s V_zsethalftone5_i) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                              - s V_zsethalftone5_i) (0))) (F_max0_ge_0 (9
                                                                    - s V_zsethalftone5_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (8 - s V_zsethalftone5_i)) (F_check_ge (8
                                                                    - s V_zsethalftone5_i) (0))]
     ((1 # 1) - (1 # 8) * s V_zsethalftone5_i + s V_zsethalftone5_z
      + max0(8 - s V_zsethalftone5_i)
      + (2 # 1) * max0(s V_zsethalftone5_count)
      + (1 # 8) * max0(s V_zsethalftone5_i) <= z)%Q
   | 72 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 73 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 74 => (-(1 # 1) + s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (1 # 13) * max0(20 + s V_zsethalftone5__tmp)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 75 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (9 - s V_zsethalftone5_i) (1);
      (*-1 0*) F_max0_ge_0 (8 - s V_zsethalftone5_i);
      (*-2 0*) F_max0_ge_0 (s V_zsethalftone5_count);
      (*-0.0769231 0*) F_binom_monotonic 1 (F_max0_ge_0 (20
                                                         + s V_zsethalftone5__tmp)) (F_check_ge (0) (0))]
     (-(1 # 1) + s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
      + (1 # 13) * max0(20 + s V_zsethalftone5__tmp)
      + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 76 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 77 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 78 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 79 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 80 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 81 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 82 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 83 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 84 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 85 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 86 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 87 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 88 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 89 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 90 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (9 - s V_zsethalftone5_i) (1)]
     (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
      + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 91 => ((1 # 1) + s V_zsethalftone5_z + max0(8 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 92 => ((1 # 1) + s V_zsethalftone5_z + max0(8 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 93 => ((1 # 1) + s V_zsethalftone5_z + max0(8 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 94 => ((1 # 1) + s V_zsethalftone5_z + max0(8 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 95 => ((1 # 1) + s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 96 => ((1 # 1) + s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 97 => ((1 # 1) + s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 98 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 99 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
            + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 100 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
             + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 101 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
             + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 102 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
             + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 103 => (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
             + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 104 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (9 - s V_zsethalftone5_i) (8
                                                                    - s V_zsethalftone5_i));
      (*-1 0*) F_max0_ge_0 (8 - s V_zsethalftone5_i)]
     (s V_zsethalftone5_z + max0(9 - s V_zsethalftone5_i)
      + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 105 => (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (2 # 1) * max0(s V_zsethalftone5_count)
             + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 106 => (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (2 # 1) * max0(s V_zsethalftone5_count)
             + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 107 => hints
     [(*-4.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zsethalftone5_npop)) (F_check_ge (0) (0));
      (*-4.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (2 - s V_zsethalftone5_npop)) (F_check_ge (0) (0))]
     (s V_zsethalftone5_z + (9 # 2) * max0(2 - s V_zsethalftone5_npop)
      + (2 # 1) * max0(s V_zsethalftone5_count)
      + (9 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 108 => (s V_zsethalftone5_z + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 109 => (s V_zsethalftone5_z + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 110 => (s V_zsethalftone5_z + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 111 => (s V_zsethalftone5_z + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 112 => (s V_zsethalftone5_z + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 113 => (s V_zsethalftone5_z + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 114 => (s V_zsethalftone5_z + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 115 => (s V_zsethalftone5_z + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 116 => (s V_zsethalftone5_z + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 117 => (s V_zsethalftone5_z + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 118 => hints
     [(*-1 0*) F_max0_ge_0 (s V_zsethalftone5_count);
      (*-0.222222 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - s V_zsethalftone5_i) (0))) (F_max0_ge_0 (9
                                                                    - s V_zsethalftone5_i))]
     (s V_zsethalftone5_z + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 119 => hints
     [(*0 2*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsethalftone5_count)) (F_check_ge (s V_zsethalftone5_count) (0));
      (*0 0.222222*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - 
                                                                    s V_zsethalftone5_i) (0))) (F_max0_ge_0 (9
                                                                    - s V_zsethalftone5_i))]
     (s V_zsethalftone5_z + (2 # 1) * max0(s V_zsethalftone5_count) <= z)%Q
   | 120 => (-(2 # 1) + (2 # 1) * s V_zsethalftone5_count
             + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 121 => (-(2 # 1) + (2 # 1) * s V_zsethalftone5_count
             + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_count)
             + max0(s V_zsethalftone5_count - s V_zsethalftone5_j) <= z)%Q
   | 122 => (-(2 # 1) + (2 # 1) * s V_zsethalftone5_count
             + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_count)
             + max0(s V_zsethalftone5_count - s V_zsethalftone5_j) <= z)%Q
   | 123 => (-(2 # 1) + (2 # 1) * s V_zsethalftone5_count
             + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_count)
             + max0(s V_zsethalftone5_count - s V_zsethalftone5_j) <= z)%Q
   | 124 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_zsethalftone5_count
                                            - s V_zsethalftone5_j) (-1
                                                                    + 
                                                                    s V_zsethalftone5_count
                                                                    - 
                                                                    s V_zsethalftone5_j));
      (*0 2*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_count) (0))) (F_max0_ge_0 (s V_zsethalftone5_count));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zsethalftone5_count
                                                - s V_zsethalftone5_j)) (F_check_ge (0) (0))]
     (-(2 # 1) + (2 # 1) * s V_zsethalftone5_count
      + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
      + (2 # 9) * max0(9 - s V_zsethalftone5_i)
      - max0(s V_zsethalftone5_count)
      + max0(s V_zsethalftone5_count - s V_zsethalftone5_j) <= z)%Q
   | 125 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_zsethalftone5_count
                                       - s V_zsethalftone5_j) (1);
      (*-2 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_count) (0))) (F_max0_ge_0 (s V_zsethalftone5_count))]
     (-(2 # 1) + (2 # 1) * s V_zsethalftone5_count
      + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
      + (2 # 9) * max0(9 - s V_zsethalftone5_i)
      - max0(s V_zsethalftone5_count)
      + max0(s V_zsethalftone5_count - s V_zsethalftone5_j) <= z)%Q
   | 126 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + max0(-1 + s V_zsethalftone5_count - s V_zsethalftone5_j)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count) <= z)%Q
   | 127 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + max0(-1 + s V_zsethalftone5_count - s V_zsethalftone5_j)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count) <= z)%Q
   | 128 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + max0(-1 + s V_zsethalftone5_count - s V_zsethalftone5_j)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count) <= z)%Q
   | 129 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + max0(-1 + s V_zsethalftone5_count - s V_zsethalftone5_j)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count) <= z)%Q
   | 130 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + max0(-1 + s V_zsethalftone5_count - s V_zsethalftone5_j)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count) <= z)%Q
   | 131 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + max0(-1 + s V_zsethalftone5_count - s V_zsethalftone5_j)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count) <= z)%Q
   | 132 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + max0(-1 + s V_zsethalftone5_count - s V_zsethalftone5_j)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count) <= z)%Q
   | 133 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + max0(-1 + s V_zsethalftone5_count - s V_zsethalftone5_j)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count) <= z)%Q
   | 134 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count)
             + max0(s V_zsethalftone5_count - s V_zsethalftone5_j) <= z)%Q
   | 135 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count)
             + max0(s V_zsethalftone5_count - s V_zsethalftone5_j) <= z)%Q
   | 136 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count)
             + max0(s V_zsethalftone5_count - s V_zsethalftone5_j) <= z)%Q
   | 137 => hints
     [(*-2 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsethalftone5_count)) (F_check_ge (s V_zsethalftone5_count) (0))]
     (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
      + (2 # 9) * max0(9 - s V_zsethalftone5_i)
      + max0(s V_zsethalftone5_count)
      + max0(s V_zsethalftone5_count - s V_zsethalftone5_j) <= z)%Q
   | 138 => hints
     [(*-0.111111 0*) F_max0_monotonic (F_check_ge (9 - s V_zsethalftone5_i) (8
                                                                    - s V_zsethalftone5_i));
      (*0 0.111111*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zsethalftone5_i)) (F_check_ge (0) (0));
      (*-0.111111 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_i) (0))) (F_max0_ge_0 (s V_zsethalftone5_i));
      (*-0.111111 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - s V_zsethalftone5_i) (0))) (F_max0_ge_0 (9
                                                                    - s V_zsethalftone5_i));
      (*-0.111111 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                        - s V_zsethalftone5_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zsethalftone5_count
                                                 - s V_zsethalftone5_j)) (F_check_ge (0) (0))]
     (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
      + max0(-1 + s V_zsethalftone5_count - s V_zsethalftone5_j)
      + (2 # 9) * max0(9 - s V_zsethalftone5_i)
      + max0(s V_zsethalftone5_count) <= z)%Q
   | 139 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count) <= z)%Q
   | 140 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count) <= z)%Q
   | 141 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count) <= z)%Q
   | 142 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + max0(s V_zsethalftone5_count) <= z)%Q
   | 143 => hints
     [(*-1 0*) F_max0_ge_0 (s V_zsethalftone5_count);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsethalftone5_j)) (F_check_ge (s V_zsethalftone5_j) (0))]
     (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
      + (2 # 9) * max0(9 - s V_zsethalftone5_i)
      + max0(s V_zsethalftone5_count) <= z)%Q
   | 144 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsethalftone5_count)) (F_check_ge (s V_zsethalftone5_count) (0))]
     (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
      + (2 # 9) * max0(9 - s V_zsethalftone5_i)
      + max0(s V_zsethalftone5_count) <= z)%Q
   | 145 => (-(2 # 1) + s V_zsethalftone5_count
             + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 146 => (-(2 # 1) + s V_zsethalftone5_count
             + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 147 => (-(2 # 1) + s V_zsethalftone5_count
             + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 148 => (-(2 # 1) + s V_zsethalftone5_count
             + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_j) <= z)%Q
   | 149 => (-(2 # 1) + s V_zsethalftone5_count
             + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_j) <= z)%Q
   | 150 => (-(2 # 1) + s V_zsethalftone5_count
             + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_z
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_j) <= z)%Q
   | 151 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_zsethalftone5_count
                                            - s V_zsethalftone5_j) (-1
                                                                    + 
                                                                    s V_zsethalftone5_count
                                                                    - 
                                                                    s V_zsethalftone5_j));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_count
                                                               - s V_zsethalftone5_j) (0))) (F_max0_ge_0 (s V_zsethalftone5_count
                                                                    - s V_zsethalftone5_j))]
     (-(2 # 1) + s V_zsethalftone5_count + (2 # 9) * s V_zsethalftone5_i
      + s V_zsethalftone5_z + (2 # 9) * max0(9 - s V_zsethalftone5_i)
      - max0(s V_zsethalftone5_j) <= z)%Q
   | 152 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_j) (0))) (F_max0_ge_0 (s V_zsethalftone5_j));
      (*0 0.222222*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                         - s V_zsethalftone5_i)) (F_check_ge (9
                                                                    - s V_zsethalftone5_i) (0))]
     (-(2 # 1) + s V_zsethalftone5_count + (2 # 9) * s V_zsethalftone5_i
      + s V_zsethalftone5_z + (2 # 9) * max0(9 - s V_zsethalftone5_i)
      - max0(s V_zsethalftone5_j) <= z)%Q
   | 153 => (s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z <= z)%Q
   | 154 => (s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z <= z)%Q
   | 155 => (s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z <= z)%Q
   | 156 => (s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z <= z)%Q
   | 157 => (s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z <= z)%Q
   | 158 => (s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z <= z)%Q
   | 159 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_npop) (0))) (F_max0_ge_0 (s V_zsethalftone5_npop));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                 - s V_zsethalftone5_npop) (0))) (F_max0_ge_0 (2
                                                                    - s V_zsethalftone5_npop))]
     (s V_zsethalftone5_count - s V_zsethalftone5_j + s V_zsethalftone5_z <= z)%Q
   | 160 => (s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z <= z)%Q
   | 161 => (s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z <= z)%Q
   | 162 => (s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z <= z)%Q
   | 163 => (s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z <= z)%Q
   | 164 => (s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z <= z)%Q
   | 165 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_npop) (0))) (F_max0_ge_0 (s V_zsethalftone5_npop));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                 - s V_zsethalftone5_npop) (0))) (F_max0_ge_0 (2
                                                                    - s V_zsethalftone5_npop))]
     (s V_zsethalftone5_count - s V_zsethalftone5_j + s V_zsethalftone5_z <= z)%Q
   | 166 => (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 167 => (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 168 => (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 169 => (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 170 => (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 171 => (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 172 => hints
     [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsethalftone5_npop)) (F_check_ge (s V_zsethalftone5_npop) (0));
      (*0 0.05*) F_binom_monotonic 1 (F_max0_ge_arg (20
                                                     + s V_zsethalftone5_code)) (F_check_ge (20
                                                                    + s V_zsethalftone5_code) (0))]
     (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
      + s V_zsethalftone5_z + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
      + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 173 => ((1 # 20) * s V_zsethalftone5_code + s V_zsethalftone5_count
             - s V_zsethalftone5_j + (1 # 2) * s V_zsethalftone5_npop
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             - (1 # 20) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 174 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_npop) (0))) (F_max0_ge_0 (s V_zsethalftone5_npop));
      (*-0.05 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                  + s V_zsethalftone5_code) (0))) (F_max0_ge_0 (20
                                                                    + s V_zsethalftone5_code))]
     ((1 # 20) * s V_zsethalftone5_code + s V_zsethalftone5_count
      - s V_zsethalftone5_j + (1 # 2) * s V_zsethalftone5_npop
      + s V_zsethalftone5_z + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
      - (1 # 20) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 175 => (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 176 => (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 177 => (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 178 => ((1 # 20) * s V_zsethalftone5_code + s V_zsethalftone5_count
             - s V_zsethalftone5_j + (1 # 2) * s V_zsethalftone5_npop
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             - (1 # 20) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 179 => ((1 # 20) * s V_zsethalftone5_code + s V_zsethalftone5_count
             - s V_zsethalftone5_j + (1 # 2) * s V_zsethalftone5_npop
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             - (1 # 20) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 180 => ((1 # 20) * s V_zsethalftone5_code + s V_zsethalftone5_count
             - s V_zsethalftone5_j + (1 # 2) * s V_zsethalftone5_npop
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             - (1 # 20) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 181 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_zsethalftone5_count
                                       - s V_zsethalftone5_j) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_zsethalftone5_count
                            - s V_zsethalftone5_j);
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zsethalftone5_npop)) (F_check_ge (0) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_npop) (0))) (F_max0_ge_0 (s V_zsethalftone5_npop));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_count
                                                               - s V_zsethalftone5_j) (0))) (F_max0_ge_0 (s V_zsethalftone5_count
                                                                    - s V_zsethalftone5_j));
      (*-0.05 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                  + s V_zsethalftone5_code) (0))) (F_max0_ge_0 (20
                                                                    + s V_zsethalftone5_code));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (2 - s V_zsethalftone5_npop)) (F_check_ge (0) (0))]
     ((1 # 20) * s V_zsethalftone5_code + s V_zsethalftone5_count
      - s V_zsethalftone5_j + (1 # 2) * s V_zsethalftone5_npop
      + s V_zsethalftone5_z + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
      - (1 # 20) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 182 => (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 183 => (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 184 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_zsethalftone5_count
                            - s V_zsethalftone5_j);
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zsethalftone5_npop)) (F_check_ge (0) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (2 - s V_zsethalftone5_npop)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_zsethalftone5_count
                                                               - s V_zsethalftone5_j) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zsethalftone5_count
                                                                    - s V_zsethalftone5_j))]
     (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
      + s V_zsethalftone5_z + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
      + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 185 => (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 186 => hints
     [(*0 0.222222*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - 
                                                                    s V_zsethalftone5_i) (0))) (F_max0_ge_0 (9
                                                                    - s V_zsethalftone5_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_zsethalftone5_count) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zsethalftone5_count))]
     (-(1 # 1) + s V_zsethalftone5_count - s V_zsethalftone5_j
      + s V_zsethalftone5_z + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
      + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 187 => hints
     [(*-0.222222 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - s V_zsethalftone5_i) (0))) (F_max0_ge_0 (9
                                                                    - s V_zsethalftone5_i))]
     (s V_zsethalftone5_count - s V_zsethalftone5_j + s V_zsethalftone5_z <= z)%Q
   | 188 => (-(2 # 1) + s V_zsethalftone5_count
             + (2 # 9) * s V_zsethalftone5_i - s V_zsethalftone5_j
             + s V_zsethalftone5_z + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 189 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_npop) (0))) (F_max0_ge_0 (s V_zsethalftone5_npop));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                 - s V_zsethalftone5_npop) (0))) (F_max0_ge_0 (2
                                                                    - s V_zsethalftone5_npop));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_zsethalftone5_count) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zsethalftone5_count))]
     (-(2 # 1) + s V_zsethalftone5_count + (2 # 9) * s V_zsethalftone5_i
      - s V_zsethalftone5_j + s V_zsethalftone5_z
      + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 190 => (s V_zsethalftone5_count - s V_zsethalftone5_j
             + s V_zsethalftone5_z <= z)%Q
   | 191 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_npop) (0))) (F_max0_ge_0 (s V_zsethalftone5_npop));
      (*-0.222222 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                    - s V_zsethalftone5_i) (0))) (F_max0_ge_0 (9
                                                                    - s V_zsethalftone5_i));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                 - s V_zsethalftone5_npop) (0))) (F_max0_ge_0 (2
                                                                    - s V_zsethalftone5_npop));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              + s V_zsethalftone5_count) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zsethalftone5_count))]
     (s V_zsethalftone5_count - s V_zsethalftone5_j + s V_zsethalftone5_z <= z)%Q
   | 192 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i - s V_zsethalftone5_j
             + s V_zsethalftone5_z + max0(-1 + s V_zsethalftone5_count)
             + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 193 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsethalftone5_npop)) (F_check_ge (s V_zsethalftone5_npop) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                     - s V_zsethalftone5_npop)) (F_check_ge (2
                                                                    - s V_zsethalftone5_npop) (0))]
     (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i - s V_zsethalftone5_j
      + s V_zsethalftone5_z + max0(-1 + s V_zsethalftone5_count)
      + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
      + (2 # 9) * max0(9 - s V_zsethalftone5_i)
      + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 194 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i - s V_zsethalftone5_j
             + s V_zsethalftone5_z + max0(-1 + s V_zsethalftone5_count)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 195 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i - s V_zsethalftone5_j
             + s V_zsethalftone5_z + max0(-1 + s V_zsethalftone5_count)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 196 => (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i - s V_zsethalftone5_j
             + s V_zsethalftone5_z + max0(-1 + s V_zsethalftone5_count)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 197 => ((2 # 9) * s V_zsethalftone5_i - s V_zsethalftone5_j
             + s V_zsethalftone5_z + max0(-1 + s V_zsethalftone5_count)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 198 => ((2 # 9) * s V_zsethalftone5_i - s V_zsethalftone5_j
             + s V_zsethalftone5_z + max0(-1 + s V_zsethalftone5_count)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 199 => ((2 # 9) * s V_zsethalftone5_i - s V_zsethalftone5_j
             + s V_zsethalftone5_z + max0(-1 + s V_zsethalftone5_count)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 200 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsethalftone5_j)) (F_check_ge (s V_zsethalftone5_j) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_zsethalftone5_count)) (F_check_ge (-1
                                                                    + s V_zsethalftone5_count) (0))]
     (-(1 # 1) + (2 # 9) * s V_zsethalftone5_i - s V_zsethalftone5_j
      + s V_zsethalftone5_z + max0(-1 + s V_zsethalftone5_count)
      + (2 # 9) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 201 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zsethalftone5_npop)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsethalftone5_j)) (F_check_ge (s V_zsethalftone5_j) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (2 - s V_zsethalftone5_npop)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              + s V_zsethalftone5_count
                                                              - s V_zsethalftone5_j) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zsethalftone5_count
                                                                    - s V_zsethalftone5_j));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_zsethalftone5_count)) (F_check_ge (-1
                                                                    + s V_zsethalftone5_count) (0))]
     (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i - s V_zsethalftone5_j
      + s V_zsethalftone5_z + max0(-1 + s V_zsethalftone5_count)
      + (1 # 2) * max0(2 - s V_zsethalftone5_npop)
      + (2 # 9) * max0(9 - s V_zsethalftone5_i)
      + (1 # 2) * max0(s V_zsethalftone5_npop) <= z)%Q
   | 202 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + max0(-1 + s V_zsethalftone5_count - s V_zsethalftone5_j)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_j) <= z)%Q
   | 203 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_j
             + s V_zsethalftone5_z
             + max0(-1 + s V_zsethalftone5_count - s V_zsethalftone5_j)
             + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_j) <= z)%Q
   | 204 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_zsethalftone5_count
                            - s V_zsethalftone5_j)]
     (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_j
      + s V_zsethalftone5_z
      + max0(-1 + s V_zsethalftone5_count - s V_zsethalftone5_j)
      + (2 # 9) * max0(9 - s V_zsethalftone5_i) - max0(s V_zsethalftone5_j) <= z)%Q
   | 205 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_j
             + s V_zsethalftone5_z + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_j) <= z)%Q
   | 206 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_j
             + s V_zsethalftone5_z + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_j) <= z)%Q
   | 207 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_j
             + s V_zsethalftone5_z + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_j) <= z)%Q
   | 208 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_j
             + s V_zsethalftone5_z + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_j) <= z)%Q
   | 209 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_j) (0))) (F_max0_ge_0 (s V_zsethalftone5_j));
      (*-0.222222 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                          - s V_zsethalftone5_i)) (F_check_ge (9
                                                                    - s V_zsethalftone5_i) (0))]
     (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_j
      + s V_zsethalftone5_z + (2 # 9) * max0(9 - s V_zsethalftone5_i)
      - max0(s V_zsethalftone5_j) <= z)%Q
   | 210 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_j
             + s V_zsethalftone5_z + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_j) <= z)%Q
   | 211 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_j
             + s V_zsethalftone5_z + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_j) <= z)%Q
   | 212 => (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_j
             + s V_zsethalftone5_z + (2 # 9) * max0(9 - s V_zsethalftone5_i)
             - max0(s V_zsethalftone5_j) <= z)%Q
   | 213 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_j) (0))) (F_max0_ge_0 (s V_zsethalftone5_j));
      (*-0.222222 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                          - s V_zsethalftone5_i)) (F_check_ge (9
                                                                    - s V_zsethalftone5_i) (0))]
     (-(2 # 1) + (2 # 9) * s V_zsethalftone5_i + s V_zsethalftone5_j
      + s V_zsethalftone5_z + (2 # 9) * max0(9 - s V_zsethalftone5_i)
      - max0(s V_zsethalftone5_j) <= z)%Q
   | 214 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count
             + s V_zsethalftone5_z + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 215 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count
             + s V_zsethalftone5_z + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 216 => ((9 # 1) + (2 # 1) * s V_zsethalftone5_count
             + s V_zsethalftone5_z + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 217 => hints
     [(*-4.125 0*) F_max0_monotonic (F_check_ge (9 - s V_zsethalftone5_i) (8
                                                                    - s V_zsethalftone5_i));
      (*-4.125 0*) F_max0_ge_0 (8 - s V_zsethalftone5_i);
      (*-2 0*) F_max0_ge_0 (s V_zsethalftone5_count);
      (*-2 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_count) (0))) (F_max0_ge_0 (s V_zsethalftone5_count));
      (*-1.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                                   - 
                                                                   s V_zsethalftone5_i) (0))) (F_max0_ge_0 (9
                                                                    - s V_zsethalftone5_i));
      (*-1.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zsethalftone5_i)) (F_check_ge (0) (0));
      (*-1.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                   + 
                                                                   s V_zsethalftone5_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zsethalftone5_i))]
     ((9 # 1) + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
      + (3 # 1) * max0(9 - s V_zsethalftone5_i) <= z)%Q
   | 218 => hints
     [(*0 3*) F_max0_pre_decrement 1 (9 - s V_zsethalftone5_i) (1);
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zsethalftone5_code)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zsethalftone5_code) (0))) (F_max0_ge_0 (-
                                                                    s V_zsethalftone5_code));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                   + 
                                                                   s V_zsethalftone5_code) (0))) (F_max0_ge_0 (16
                                                                    + s V_zsethalftone5_code))]
     (-(25 # 1) - (17 # 10) * s V_zsethalftone5_code
      + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
      + (3 # 1) * max0(9 - s V_zsethalftone5_i)
      + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 219 => (-(24 # 1) - (17 # 10) * s V_zsethalftone5_code
             + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
             + (3 # 1) * max0(8 - s V_zsethalftone5_i)
             + (1 # 8) * max0(16 + s V_zsethalftone5_code)
             + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 220 => hints
     [(*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (16
                                                     + s V_zsethalftone5_code)) (F_check_ge (0) (0))]
     (-(24 # 1) - (17 # 10) * s V_zsethalftone5_code
      + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
      + (3 # 1) * max0(8 - s V_zsethalftone5_i)
      + (1 # 8) * max0(16 + s V_zsethalftone5_code)
      + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 221 => (-(24 # 1) - (17 # 10) * s V_zsethalftone5_code
             + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
             + (3 # 1) * max0(8 - s V_zsethalftone5_i)
             + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 222 => hints
     [(*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zsethalftone5_code)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zsethalftone5_code) (0))) (F_max0_ge_0 (-
                                                                    s V_zsethalftone5_code));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                   + 
                                                                   s V_zsethalftone5_code) (0))) (F_max0_ge_0 (16
                                                                    + s V_zsethalftone5_code))]
     (-(24 # 1) - (17 # 10) * s V_zsethalftone5_code
      + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
      + (3 # 1) * max0(8 - s V_zsethalftone5_i)
      + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 223 => (-(26 # 1) - (17 # 10) * s V_zsethalftone5_code
             + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
             + (3 # 1) * max0(8 - s V_zsethalftone5_i)
             + (1 # 8) * max0(16 + s V_zsethalftone5_code)
             + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 224 => hints
     [(*0 3*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zsethalftone5_i) (0))) (F_max0_ge_0 (-
                                                                    s V_zsethalftone5_i));
      (*0 3*) F_binom_monotonic 1 (F_max0_ge_arg (8 - s V_zsethalftone5_i)) (F_check_ge (8
                                                                    - s V_zsethalftone5_i) (0))]
     (-(24 # 1) - (17 # 10) * s V_zsethalftone5_code
      + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
      + (3 # 1) * max0(8 - s V_zsethalftone5_i)
      + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 225 => (-(17 # 10) * s V_zsethalftone5_code
             + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
             + (17 # 10) * max0(20 + s V_zsethalftone5_code)
             + (3 # 1) * max0(-s V_zsethalftone5_i) <= z)%Q
   | 226 => (-(34 # 1) - (17 # 10) * s V_zsethalftone5_code
             + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
             + (34 # 13) * max0(-7 - s V_zsethalftone5__tmp)
             + (17 # 10) * max0(20 + s V_zsethalftone5_code)
             + (3 # 1) * max0(-s V_zsethalftone5_i) <= z)%Q
   | 227 => hints
     [(*-2 0*) F_max0_ge_0 (s V_zsethalftone5_count);
      (*-3 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zsethalftone5_i)) (F_check_ge (0) (0));
      (*-2 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_count) (0))) (F_max0_ge_0 (s V_zsethalftone5_count));
      (*-1.7 0*) F_binom_monotonic 1 (F_max0_ge_arg (20
                                                     + s V_zsethalftone5_code)) (F_check_ge (20
                                                                    + s V_zsethalftone5_code) (0));
      (*-2.61538 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                       - s V_zsethalftone5__tmp)) (F_check_ge (0) (0))]
     (-(34 # 1) - (17 # 10) * s V_zsethalftone5_code
      + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
      + (34 # 13) * max0(-7 - s V_zsethalftone5__tmp)
      + (17 # 10) * max0(20 + s V_zsethalftone5_code)
      + (3 # 1) * max0(-s V_zsethalftone5_i) <= z)%Q
   | 228 => (s V_zsethalftone5_z <= z)%Q
   | 229 => (-(24 # 1) - (17 # 10) * s V_zsethalftone5_code
             + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
             + (3 # 1) * max0(8 - s V_zsethalftone5_i)
             + (1 # 8) * max0(16 + s V_zsethalftone5_code)
             + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 230 => (-(26 # 1) - (17 # 10) * s V_zsethalftone5_code
             + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
             + (3 # 1) * max0(8 - s V_zsethalftone5_i)
             + (1 # 8) * max0(16 + s V_zsethalftone5_code)
             + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 231 => (-(26 # 1) - (17 # 10) * s V_zsethalftone5_code
             + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
             + (3 # 1) * max0(8 - s V_zsethalftone5_i)
             + (1 # 8) * max0(16 + s V_zsethalftone5_code)
             + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 232 => (-(26 # 1) - (17 # 10) * s V_zsethalftone5_code
             + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
             + (3 # 1) * max0(8 - s V_zsethalftone5_i)
             + (1 # 8) * max0(16 + s V_zsethalftone5_code)
             + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 233 => (-(26 # 1) - (17 # 10) * s V_zsethalftone5_code
             + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
             + (3 # 1) * max0(9 - s V_zsethalftone5_i)
             + (1 # 8) * max0(16 + s V_zsethalftone5_code)
             + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 234 => (-(26 # 1) - (17 # 10) * s V_zsethalftone5_code
             + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
             + (3 # 1) * max0(9 - s V_zsethalftone5_i)
             + (1 # 8) * max0(16 + s V_zsethalftone5_code)
             + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 235 => (-(26 # 1) - (17 # 10) * s V_zsethalftone5_code
             + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
             + (3 # 1) * max0(9 - s V_zsethalftone5_i)
             + (1 # 8) * max0(16 + s V_zsethalftone5_code)
             + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | 236 => hints
     [(*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zsethalftone5_code)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsethalftone5_code) (0))) (F_max0_ge_0 (s V_zsethalftone5_code));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_arg (16
                                                       + s V_zsethalftone5_code)) (F_check_ge (16
                                                                    + s V_zsethalftone5_code) (0))]
     (-(27 # 1) - (17 # 10) * s V_zsethalftone5_code
      + (2 # 1) * s V_zsethalftone5_count + s V_zsethalftone5_z
      + (3 # 1) * max0(9 - s V_zsethalftone5_i)
      + (1 # 8) * max0(16 + s V_zsethalftone5_code)
      + (17 # 10) * max0(20 + s V_zsethalftone5_code) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_zsethalftone5 =>
    [mkPA Q (fun n z s => ai_zsethalftone5 n s /\ annot0_zsethalftone5 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_zsethalftone5 (proc_start P_zsethalftone5) s1 (proc_end P_zsethalftone5) s2 ->
    (s2 V_zsethalftone5_z <= (36 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_zsethalftone5.
Qed.
