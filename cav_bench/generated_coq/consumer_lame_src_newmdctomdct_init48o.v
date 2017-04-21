Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mdct_init48.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mdct_init48_z := 1%positive.
Notation V_mdct_init48_i := 2%positive.
Notation V_mdct_init48_j := 3%positive.
Notation V_mdct_init48_k := 4%positive.
Notation V_mdct_init48_m := 5%positive.
Definition Pedges_mdct_init48: list (edge proc) :=
  (EA 1 (AAssign V_mdct_init48_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_mdct_init48_k (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_mdct_init48_k) s) < (eval (ENum (8))
  s))%Z)) 278)::(EA 5 (AGuard (fun s => ((eval (EVar V_mdct_init48_k) s) >=
  (eval (ENum (8)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign
  V_mdct_init48_i (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_mdct_init48_i) s) <
  (eval (ENum (36)) s))%Z)) 271)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_i) s) >= (eval (ENum (36))
  s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 (AAssign V_mdct_init48_i
  (Some (ENum (0)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_mdct_init48_i) s) <
  (eval (ENum (18)) s))%Z)) 264)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_i) s) >= (eval (ENum (18))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 18)::(EA 18 AWeaken 19)::
  (EA 19 (AGuard (fun s => ((eval (EVar V_mdct_init48_i) s) <
  (eval (ENum (24)) s))%Z)) 257)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_i) s) >= (eval (ENum (24))
  s))%Z)) 20)::(EA 20 AWeaken 21)::(EA 21 ANone 22)::(EA 22 AWeaken 23)::
  (EA 23 (AGuard (fun s => ((eval (EVar V_mdct_init48_i) s) <
  (eval (ENum (30)) s))%Z)) 250)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_i) s) >= (eval (ENum (30))
  s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 ANone 26)::(EA 26 AWeaken 27)::
  (EA 27 (AGuard (fun s => ((eval (EVar V_mdct_init48_i) s) <
  (eval (ENum (36)) s))%Z)) 243)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_i) s) >= (eval (ENum (36))
  s))%Z)) 28)::(EA 28 AWeaken 29)::(EA 29 (AAssign V_mdct_init48_i
  (Some (ENum (0)))) 30)::(EA 30 ANone 31)::(EA 31 AWeaken 32)::
  (EA 32 (AGuard (fun s => ((eval (EVar V_mdct_init48_i) s) <
  (eval (ENum (36)) s))%Z)) 236)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_i) s) >= (eval (ENum (36))
  s))%Z)) 33)::(EA 33 AWeaken 34)::(EA 34 (AAssign V_mdct_init48_j
  (Some (ENum (11)))) 35)::(EA 35 ANone 36)::(EA 36 (AAssign V_mdct_init48_m
  None) 37)::(EA 37 (AAssign V_mdct_init48_k (Some (ENum (0)))) 38)::
  (EA 38 ANone 39)::(EA 39 AWeaken 40)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_k) s) < (eval (ENum (9))
  s))%Z)) 229)::(EA 40 (AGuard (fun s => ((eval (EVar V_mdct_init48_k) s) >=
  (eval (ENum (9)) s))%Z)) 41)::(EA 41 AWeaken 42)::(EA 42 (AAssign
  V_mdct_init48_k (Some (ENum (0)))) 43)::(EA 43 ANone 44)::
  (EA 44 AWeaken 45)::(EA 45 (AGuard (fun s => ((eval (EVar V_mdct_init48_k)
  s) < (eval (ENum (9)) s))%Z)) 222)::(EA 45 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_k) s) >= (eval (ENum (9))
  s))%Z)) 46)::(EA 46 AWeaken 47)::(EA 47 ANone 48)::(EA 48 (AAssign
  V_mdct_init48_j (Some (EAdd (EVar V_mdct_init48_j) (ENum (-1))))) 49)::
  (EA 49 AWeaken 50)::(EA 50 (AGuard
  (fun s => ((eval (EAdd (EVar V_mdct_init48_j) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 219)::(EA 50 (AGuard
  (fun s => ((eval (EAdd (EVar V_mdct_init48_j) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 51)::(EA 51 AWeaken 52)::(EA 52 (AAssign
  V_mdct_init48_j (Some (ENum (3)))) 53)::(EA 53 ANone 54)::(EA 54 (AAssign
  V_mdct_init48_m None) 55)::(EA 55 (AAssign V_mdct_init48_k
  (Some (ENum (0)))) 56)::(EA 56 ANone 57)::(EA 57 AWeaken 58)::
  (EA 58 (AGuard (fun s => ((eval (EVar V_mdct_init48_k) s) <
  (eval (ENum (3)) s))%Z)) 212)::(EA 58 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_k) s) >= (eval (ENum (3))
  s))%Z)) 59)::(EA 59 AWeaken 60)::(EA 60 (AAssign V_mdct_init48_k
  (Some (ENum (6)))) 61)::(EA 61 ANone 62)::(EA 62 AWeaken 63)::
  (EA 63 (AGuard (fun s => ((eval (EVar V_mdct_init48_k) s) <
  (eval (ENum (9)) s))%Z)) 205)::(EA 63 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_k) s) >= (eval (ENum (9))
  s))%Z)) 64)::(EA 64 AWeaken 65)::(EA 65 ANone 66)::(EA 66 (AAssign
  V_mdct_init48_j (Some (EAdd (EVar V_mdct_init48_j) (ENum (-1))))) 67)::
  (EA 67 AWeaken 68)::(EA 68 (AGuard
  (fun s => ((eval (EAdd (EVar V_mdct_init48_j) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 202)::(EA 68 (AGuard
  (fun s => ((eval (EAdd (EVar V_mdct_init48_j) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 69)::(EA 69 AWeaken 70)::(EA 70 (AAssign
  V_mdct_init48_j (Some (ENum (1)))) 71)::(EA 71 ANone 72)::(EA 72 (AAssign
  V_mdct_init48_m None) 73)::(EA 73 ANone 74)::(EA 74 (AAssign
  V_mdct_init48_j (Some (EAdd (EVar V_mdct_init48_j) (ENum (-1))))) 75)::
  (EA 75 AWeaken 76)::(EA 76 (AGuard
  (fun s => ((eval (EAdd (EVar V_mdct_init48_j) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 199)::(EA 76 (AGuard
  (fun s => ((eval (EAdd (EVar V_mdct_init48_j) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 77)::(EA 77 AWeaken 78)::(EA 78 (AAssign
  V_mdct_init48_k (Some (ENum (0)))) 79)::(EA 79 ANone 80)::
  (EA 80 AWeaken 81)::(EA 81 (AGuard (fun s => ((eval (EVar V_mdct_init48_k)
  s) < (eval (ENum (7)) s))%Z)) 192)::(EA 81 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_k) s) >= (eval (ENum (7))
  s))%Z)) 82)::(EA 82 AWeaken 83)::(EA 83 (AAssign V_mdct_init48_i
  (Some (ENum (14)))) 84)::(EA 84 ANone 85)::(EA 85 AWeaken 86)::
  (EA 86 (AGuard (fun s => ((eval (EVar V_mdct_init48_i) s) >=
  (eval (ENum (0)) s))%Z)) 173)::(EA 86 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_i) s) < (eval (ENum (0)) s))%Z)) 87)::
  (EA 87 AWeaken 88)::(EA 88 (AAssign V_mdct_init48_k
  (Some (ENum (0)))) 89)::(EA 89 ANone 90)::(EA 90 AWeaken 91)::
  (EA 91 (AGuard (fun s => ((eval (EVar V_mdct_init48_k) s) <
  (eval (ENum (7)) s))%Z)) 166)::(EA 91 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_k) s) >= (eval (ENum (7))
  s))%Z)) 92)::(EA 92 AWeaken 93)::(EA 93 (AAssign V_mdct_init48_i
  (Some (ENum (15)))) 94)::(EA 94 ANone 95)::(EA 95 AWeaken 96)::
  (EA 96 (AGuard (fun s => ((eval (EVar V_mdct_init48_i) s) >=
  (eval (ENum (0)) s))%Z)) 147)::(EA 96 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_i) s) < (eval (ENum (0)) s))%Z)) 97)::
  (EA 97 AWeaken 98)::(EA 98 (AAssign V_mdct_init48_k
  (Some (ENum (0)))) 99)::(EA 99 ANone 100)::(EA 100 AWeaken 101)::
  (EA 101 (AGuard (fun s => ((eval (EVar V_mdct_init48_k) s) <
  (eval (ENum (4)) s))%Z)) 140)::(EA 101 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_k) s) >= (eval (ENum (4))
  s))%Z)) 102)::(EA 102 AWeaken 103)::(EA 103 (AAssign V_mdct_init48_i
  (Some (ENum (0)))) 104)::(EA 104 ANone 105)::(EA 105 AWeaken 106)::
  (EA 106 (AGuard (fun s => ((eval (EVar V_mdct_init48_i) s) <
  (eval (ENum (36)) s))%Z)) 133)::(EA 106 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_i) s) >= (eval (ENum (36))
  s))%Z)) 107)::(EA 107 AWeaken 108)::(EA 108 (AAssign V_mdct_init48_i
  (Some (ENum (0)))) 109)::(EA 109 ANone 110)::(EA 110 AWeaken 111)::
  (EA 111 (AGuard (fun s => ((eval (EVar V_mdct_init48_i) s) <
  (eval (ENum (3)) s))%Z)) 114)::(EA 111 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_i) s) >= (eval (ENum (3))
  s))%Z)) 112)::(EA 112 AWeaken 113)::(EA 114 AWeaken 115)::(EA 115 (AAssign
  V_mdct_init48_m (Some (ENum (0)))) 116)::(EA 116 ANone 117)::
  (EA 117 AWeaken 118)::(EA 118 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_m) s) < (eval (ENum (6))
  s))%Z)) 126)::(EA 118 (AGuard (fun s => ((eval (EVar V_mdct_init48_m) s) >=
  (eval (ENum (6)) s))%Z)) 119)::(EA 119 AWeaken 120)::(EA 120 ANone 121)::
  (EA 121 (AAssign V_mdct_init48_i (Some (EAdd (EVar V_mdct_init48_i)
  (ENum (1))))) 122)::(EA 122 ANone 123)::(EA 123 ANone 124)::
  (EA 124 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 125)::(EA 125 AWeaken 111)::
  (EA 126 AWeaken 127)::(EA 127 ANone 128)::(EA 128 (AAssign V_mdct_init48_m
  (Some (EAdd (EVar V_mdct_init48_m) (ENum (1))))) 129)::(EA 129 ANone 130)::
  (EA 130 ANone 131)::(EA 131 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 132)::(EA 132 AWeaken 118)::
  (EA 133 AWeaken 134)::(EA 134 ANone 135)::(EA 135 (AAssign V_mdct_init48_i
  (Some (EAdd (EVar V_mdct_init48_i) (ENum (1))))) 136)::(EA 136 ANone 137)::
  (EA 137 ANone 138)::(EA 138 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 139)::(EA 139 AWeaken 106)::
  (EA 140 AWeaken 141)::(EA 141 ANone 142)::(EA 142 (AAssign V_mdct_init48_k
  (Some (EAdd (EVar V_mdct_init48_k) (ENum (1))))) 143)::(EA 143 ANone 144)::
  (EA 144 ANone 145)::(EA 145 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 146)::(EA 146 AWeaken 101)::
  (EA 147 AWeaken 148)::(EA 148 (AAssign V_mdct_init48_k
  (Some (ENum (1)))) 149)::(EA 149 ANone 150)::(EA 150 AWeaken 151)::
  (EA 151 (AGuard (fun s => ((eval (EVar V_mdct_init48_k) s) <
  (eval (ENum (32)) s))%Z)) 159)::(EA 151 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_k) s) >= (eval (ENum (32))
  s))%Z)) 152)::(EA 152 AWeaken 153)::(EA 153 ANone 154)::(EA 154 (AAssign
  V_mdct_init48_i (Some (EAdd (EVar V_mdct_init48_i) (ENum (-1))))) 155)::
  (EA 155 ANone 156)::(EA 156 ANone 157)::(EA 157 (AAssign V_mdct_init48_z
  (Some (EAdd (ENum (1)) (EVar V_mdct_init48_z)))) 158)::
  (EA 158 AWeaken 96)::(EA 159 AWeaken 160)::(EA 160 ANone 161)::
  (EA 161 (AAssign V_mdct_init48_k (Some (EAdd (EVar V_mdct_init48_k)
  (ENum (1))))) 162)::(EA 162 ANone 163)::(EA 163 ANone 164)::
  (EA 164 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 165)::(EA 165 AWeaken 151)::
  (EA 166 AWeaken 167)::(EA 167 ANone 168)::(EA 168 (AAssign V_mdct_init48_k
  (Some (EAdd (EVar V_mdct_init48_k) (ENum (1))))) 169)::(EA 169 ANone 170)::
  (EA 170 ANone 171)::(EA 171 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 172)::(EA 172 AWeaken 91)::(EA 173 AWeaken 174)::
  (EA 174 (AAssign V_mdct_init48_k (Some (ENum (0)))) 175)::
  (EA 175 ANone 176)::(EA 176 AWeaken 177)::(EA 177 (AGuard
  (fun s => ((eval (EVar V_mdct_init48_k) s) < (eval (ENum (15))
  s))%Z)) 185)::(EA 177 (AGuard (fun s => ((eval (EVar V_mdct_init48_k) s) >=
  (eval (ENum (15)) s))%Z)) 178)::(EA 178 AWeaken 179)::(EA 179 ANone 180)::
  (EA 180 (AAssign V_mdct_init48_i (Some (EAdd (EVar V_mdct_init48_i)
  (ENum (-1))))) 181)::(EA 181 ANone 182)::(EA 182 ANone 183)::
  (EA 183 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 184)::(EA 184 AWeaken 86)::(EA 185 AWeaken 186)::
  (EA 186 ANone 187)::(EA 187 (AAssign V_mdct_init48_k
  (Some (EAdd (EVar V_mdct_init48_k) (ENum (1))))) 188)::(EA 188 ANone 189)::
  (EA 189 ANone 190)::(EA 190 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 191)::(EA 191 AWeaken 177)::
  (EA 192 AWeaken 193)::(EA 193 ANone 194)::(EA 194 (AAssign V_mdct_init48_k
  (Some (EAdd (EVar V_mdct_init48_k) (ENum (1))))) 195)::(EA 195 ANone 196)::
  (EA 196 ANone 197)::(EA 197 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 198)::(EA 198 AWeaken 81)::(EA 199 AWeaken 200)::
  (EA 200 ANone 201)::(EA 201 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 72)::(EA 202 AWeaken 203)::(EA 203 ANone 204)::
  (EA 204 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 54)::(EA 205 AWeaken 206)::(EA 206 ANone 207)::
  (EA 207 (AAssign V_mdct_init48_k (Some (EAdd (EVar V_mdct_init48_k)
  (ENum (1))))) 208)::(EA 208 ANone 209)::(EA 209 ANone 210)::
  (EA 210 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 211)::(EA 211 AWeaken 63)::(EA 212 AWeaken 213)::
  (EA 213 ANone 214)::(EA 214 (AAssign V_mdct_init48_k
  (Some (EAdd (EVar V_mdct_init48_k) (ENum (1))))) 215)::(EA 215 ANone 216)::
  (EA 216 ANone 217)::(EA 217 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 218)::(EA 218 AWeaken 58)::(EA 219 AWeaken 220)::
  (EA 220 ANone 221)::(EA 221 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 36)::(EA 222 AWeaken 223)::(EA 223 ANone 224)::
  (EA 224 (AAssign V_mdct_init48_k (Some (EAdd (EVar V_mdct_init48_k)
  (ENum (1))))) 225)::(EA 225 ANone 226)::(EA 226 ANone 227)::
  (EA 227 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 228)::(EA 228 AWeaken 45)::(EA 229 AWeaken 230)::
  (EA 230 ANone 231)::(EA 231 (AAssign V_mdct_init48_k
  (Some (EAdd (EVar V_mdct_init48_k) (ENum (1))))) 232)::(EA 232 ANone 233)::
  (EA 233 ANone 234)::(EA 234 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 235)::(EA 235 AWeaken 40)::(EA 236 AWeaken 237)::
  (EA 237 ANone 238)::(EA 238 (AAssign V_mdct_init48_i
  (Some (EAdd (EVar V_mdct_init48_i) (ENum (1))))) 239)::(EA 239 ANone 240)::
  (EA 240 ANone 241)::(EA 241 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 242)::(EA 242 AWeaken 32)::(EA 243 AWeaken 244)::
  (EA 244 ANone 245)::(EA 245 (AAssign V_mdct_init48_i
  (Some (EAdd (EVar V_mdct_init48_i) (ENum (1))))) 246)::(EA 246 ANone 247)::
  (EA 247 ANone 248)::(EA 248 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 249)::(EA 249 AWeaken 27)::(EA 250 AWeaken 251)::
  (EA 251 ANone 252)::(EA 252 (AAssign V_mdct_init48_i
  (Some (EAdd (EVar V_mdct_init48_i) (ENum (1))))) 253)::(EA 253 ANone 254)::
  (EA 254 ANone 255)::(EA 255 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 256)::(EA 256 AWeaken 23)::(EA 257 AWeaken 258)::
  (EA 258 ANone 259)::(EA 259 (AAssign V_mdct_init48_i
  (Some (EAdd (EVar V_mdct_init48_i) (ENum (1))))) 260)::(EA 260 ANone 261)::
  (EA 261 ANone 262)::(EA 262 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 263)::(EA 263 AWeaken 19)::(EA 264 AWeaken 265)::
  (EA 265 ANone 266)::(EA 266 (AAssign V_mdct_init48_i
  (Some (EAdd (EVar V_mdct_init48_i) (ENum (1))))) 267)::(EA 267 ANone 268)::
  (EA 268 ANone 269)::(EA 269 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 270)::(EA 270 AWeaken 15)::(EA 271 AWeaken 272)::
  (EA 272 ANone 273)::(EA 273 (AAssign V_mdct_init48_i
  (Some (EAdd (EVar V_mdct_init48_i) (ENum (1))))) 274)::(EA 274 ANone 275)::
  (EA 275 ANone 276)::(EA 276 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 277)::(EA 277 AWeaken 10)::(EA 278 AWeaken 279)::
  (EA 279 ANone 280)::(EA 280 (AAssign V_mdct_init48_k
  (Some (EAdd (EVar V_mdct_init48_k) (ENum (1))))) 281)::(EA 281 ANone 282)::
  (EA 282 ANone 283)::(EA 283 (AAssign V_mdct_init48_z (Some (EAdd (ENum (1))
  (EVar V_mdct_init48_z)))) 284)::(EA 284 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mdct_init48 => Pedges_mdct_init48
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mdct_init48 => 113
     end)%positive;
  var_global := var_global
}.

Definition ai_mdct_init48 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 3 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_k <= 0)%Z
   | 4 => (-1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 5 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 6 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0)%Z
   | 7 => (-1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 8 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_i <= 0)%Z
   | 9 => (-1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 10 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 11 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0)%Z
   | 12 => (-1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 13 => (-1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_i <= 0)%Z
   | 14 => (-1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0)%Z
   | 15 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ 1 * s V_mdct_init48_i + -18 <= 0)%Z
   | 16 => (1 * s V_mdct_init48_i + -18 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 18 <= 0)%Z
   | 17 => (-1 * s V_mdct_init48_i + 18 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ 1 * s V_mdct_init48_i + -18 <= 0)%Z
   | 18 => (1 * s V_mdct_init48_i + -18 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 18 <= 0)%Z
   | 19 => (-1 * s V_mdct_init48_i + 18 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ 1 * s V_mdct_init48_i + -24 <= 0)%Z
   | 20 => (1 * s V_mdct_init48_i + -24 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 24 <= 0)%Z
   | 21 => (-1 * s V_mdct_init48_i + 24 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ 1 * s V_mdct_init48_i + -24 <= 0)%Z
   | 22 => (1 * s V_mdct_init48_i + -24 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 24 <= 0)%Z
   | 23 => (-1 * s V_mdct_init48_i + 24 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ 1 * s V_mdct_init48_i + -30 <= 0)%Z
   | 24 => (1 * s V_mdct_init48_i + -30 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 30 <= 0)%Z
   | 25 => (-1 * s V_mdct_init48_i + 30 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ 1 * s V_mdct_init48_i + -30 <= 0)%Z
   | 26 => (1 * s V_mdct_init48_i + -30 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 30 <= 0)%Z
   | 27 => (-1 * s V_mdct_init48_i + 30 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 28 => (1 * s V_mdct_init48_i + -36 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0)%Z
   | 29 => (-1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 30 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_i <= 0)%Z
   | 31 => (-1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 32 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 33 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0)%Z
   | 34 => (-1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 35 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_j + 11 <= 0)%Z
   | 36 => (1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 37 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0)%Z
   | 38 => (1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_k <= 0)%Z
   | 39 => (-1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0)%Z
   | 40 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0)%Z
   | 41 => (1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 42 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0)%Z
   | 43 => (1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_k <= 0)%Z
   | 44 => (-1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0)%Z
   | 45 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0)%Z
   | 46 => (1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 47 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0)%Z
   | 48 => (1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 49 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -10 <= 0)%Z
   | 50 => (1 * s V_mdct_init48_j + -10 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 51 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j <= 0)%Z
   | 52 => (1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 53 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_j + 3 <= 0)%Z
   | 54 => (1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 55 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0)%Z
   | 56 => (1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_k <= 0)%Z
   | 57 => (-1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0)%Z
   | 58 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ 1 * s V_mdct_init48_k + -3 <= 0)%Z
   | 59 => (1 * s V_mdct_init48_k + -3 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 3 <= 0)%Z
   | 60 => (-1 * s V_mdct_init48_k + 3 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ 1 * s V_mdct_init48_k + -3 <= 0)%Z
   | 61 => (1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -6 <= 0 /\ -1 * s V_mdct_init48_k + 6 <= 0)%Z
   | 62 => (-1 * s V_mdct_init48_k + 6 <= 0 /\ 1 * s V_mdct_init48_k + -6 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0)%Z
   | 63 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 6 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0)%Z
   | 64 => (1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 65 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0)%Z
   | 66 => (1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 67 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -2 <= 0)%Z
   | 68 => (1 * s V_mdct_init48_j + -2 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 69 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j <= 0)%Z
   | 70 => (1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 71 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -1 <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0)%Z
   | 72 => (-1 * s V_mdct_init48_j + 1 <= 0 /\ 1 * s V_mdct_init48_j + -1 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 73 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -1 <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0)%Z
   | 74 => (-1 * s V_mdct_init48_j + 1 <= 0 /\ 1 * s V_mdct_init48_j + -1 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 75 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0)%Z
   | 76 => (1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 77 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0)%Z
   | 78 => (1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 79 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_k <= 0)%Z
   | 80 => (-1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 81 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -7 <= 0)%Z
   | 82 => (1 * s V_mdct_init48_k + -7 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0)%Z
   | 83 => (-1 * s V_mdct_init48_k + 7 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -7 <= 0)%Z
   | 84 => (1 * s V_mdct_init48_k + -7 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_i + 14 <= 0)%Z
   | 85 => (-1 * s V_mdct_init48_i + 14 <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -7 <= 0)%Z
   | 86 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0)%Z
   | 87 => (1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0)%Z
   | 88 => (1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0)%Z
   | 89 => (1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_k <= 0)%Z
   | 90 => (-1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0)%Z
   | 91 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_k + -7 <= 0)%Z
   | 92 => (1 * s V_mdct_init48_k + -7 <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0)%Z
   | 93 => (-1 * s V_mdct_init48_k + 7 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_k + -7 <= 0)%Z
   | 94 => (1 * s V_mdct_init48_k + -7 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ -1 * s V_mdct_init48_i + 15 <= 0)%Z
   | 95 => (-1 * s V_mdct_init48_i + 15 <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -7 <= 0)%Z
   | 96 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0)%Z
   | 97 => (-1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0)%Z
   | 98 => (1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0)%Z
   | 99 => (-1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_k <= 0)%Z
   | 100 => (-1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0)%Z
   | 101 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0)%Z
   | 102 => (1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0)%Z
   | 103 => (-1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0)%Z
   | 104 => (1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ 1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_i <= 0)%Z
   | 105 => (-1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0)%Z
   | 106 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 107 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0)%Z
   | 108 => (-1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 109 => (-1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_i <= 0)%Z
   | 110 => (-1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0)%Z
   | 111 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0)%Z
   | 112 => (-1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 3 <= 0)%Z
   | 113 => (-1 * s V_mdct_init48_i + 3 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0)%Z
   | 114 => (-1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -2 <= 0)%Z
   | 115 => (1 * s V_mdct_init48_i + -2 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0)%Z
   | 116 => (-1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -2 <= 0 /\ 1 * s V_mdct_init48_m <= 0 /\ -1 * s V_mdct_init48_m <= 0)%Z
   | 117 => (-1 * s V_mdct_init48_m <= 0 /\ 1 * s V_mdct_init48_m <= 0 /\ 1 * s V_mdct_init48_i + -2 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0)%Z
   | 118 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_m <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ 1 * s V_mdct_init48_m + -6 <= 0)%Z
   | 119 => (1 * s V_mdct_init48_m + -6 <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_m + 6 <= 0)%Z
   | 120 => (-1 * s V_mdct_init48_m + 6 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ 1 * s V_mdct_init48_m + -6 <= 0)%Z
   | 121 => (1 * s V_mdct_init48_m + -6 <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_m + 6 <= 0)%Z
   | 122 => (-1 * s V_mdct_init48_m + 6 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ 1 * s V_mdct_init48_m + -6 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0)%Z
   | 123 => (-1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_m + -6 <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_m + 6 <= 0)%Z
   | 124 => (-1 * s V_mdct_init48_m + 6 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ 1 * s V_mdct_init48_m + -6 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0)%Z
   | 125 => (-1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_m + -6 <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_m + 6 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 126 => (-1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_m <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_m + -5 <= 0)%Z
   | 127 => (1 * s V_mdct_init48_m + -5 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_m <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0)%Z
   | 128 => (-1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_m <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_m + -5 <= 0)%Z
   | 129 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_m + 1 <= 0 /\ 1 * s V_mdct_init48_m + -6 <= 0)%Z
   | 130 => (1 * s V_mdct_init48_m + -6 <= 0 /\ -1 * s V_mdct_init48_m + 1 <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 131 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_m + 1 <= 0 /\ 1 * s V_mdct_init48_m + -6 <= 0)%Z
   | 132 => (1 * s V_mdct_init48_m + -6 <= 0 /\ -1 * s V_mdct_init48_m + 1 <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 133 => (-1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -35 <= 0)%Z
   | 134 => (1 * s V_mdct_init48_i + -35 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0)%Z
   | 135 => (-1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -35 <= 0)%Z
   | 136 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 137 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 138 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 139 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_k + 4 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 140 => (1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -3 <= 0)%Z
   | 141 => (1 * s V_mdct_init48_k + -3 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0)%Z
   | 142 => (1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -3 <= 0)%Z
   | 143 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0)%Z
   | 144 => (1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 145 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -4 <= 0)%Z
   | 146 => (1 * s V_mdct_init48_k + -4 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 147 => (-1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0)%Z
   | 148 => (-1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0)%Z
   | 149 => (-1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -1 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0)%Z
   | 150 => (-1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -1 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0)%Z
   | 151 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -32 <= 0)%Z
   | 152 => (1 * s V_mdct_init48_k + -32 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 32 <= 0)%Z
   | 153 => (-1 * s V_mdct_init48_k + 32 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -32 <= 0)%Z
   | 154 => (1 * s V_mdct_init48_k + -32 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 32 <= 0)%Z
   | 155 => (-1 * s V_mdct_init48_k + 32 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -32 <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0)%Z
   | 156 => (1 * s V_mdct_init48_i + -14 <= 0 /\ 1 * s V_mdct_init48_k + -32 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 32 <= 0)%Z
   | 157 => (-1 * s V_mdct_init48_k + 32 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -32 <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0)%Z
   | 158 => (1 * s V_mdct_init48_i + -14 <= 0 /\ 1 * s V_mdct_init48_k + -32 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 32 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 159 => (-1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -31 <= 0)%Z
   | 160 => (1 * s V_mdct_init48_k + -31 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0)%Z
   | 161 => (-1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -31 <= 0)%Z
   | 162 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 2 <= 0 /\ 1 * s V_mdct_init48_k + -32 <= 0)%Z
   | 163 => (1 * s V_mdct_init48_k + -32 <= 0 /\ -1 * s V_mdct_init48_k + 2 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 164 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 2 <= 0 /\ 1 * s V_mdct_init48_k + -32 <= 0)%Z
   | 165 => (1 * s V_mdct_init48_k + -32 <= 0 /\ -1 * s V_mdct_init48_k + 2 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -15 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 166 => (1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -6 <= 0)%Z
   | 167 => (1 * s V_mdct_init48_k + -6 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0)%Z
   | 168 => (1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -6 <= 0)%Z
   | 169 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -7 <= 0)%Z
   | 170 => (1 * s V_mdct_init48_k + -7 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 171 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -7 <= 0)%Z
   | 172 => (1 * s V_mdct_init48_k + -7 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 173 => (1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0)%Z
   | 174 => (-1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0)%Z
   | 175 => (1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_k <= 0)%Z
   | 176 => (-1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0)%Z
   | 177 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -15 <= 0)%Z
   | 178 => (1 * s V_mdct_init48_k + -15 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 15 <= 0)%Z
   | 179 => (-1 * s V_mdct_init48_k + 15 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -15 <= 0)%Z
   | 180 => (1 * s V_mdct_init48_k + -15 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 15 <= 0)%Z
   | 181 => (-1 * s V_mdct_init48_k + 15 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -15 <= 0 /\ 1 * s V_mdct_init48_i + -13 <= 0)%Z
   | 182 => (1 * s V_mdct_init48_i + -13 <= 0 /\ 1 * s V_mdct_init48_k + -15 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 15 <= 0)%Z
   | 183 => (-1 * s V_mdct_init48_k + 15 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_k + -15 <= 0 /\ 1 * s V_mdct_init48_i + -13 <= 0)%Z
   | 184 => (1 * s V_mdct_init48_i + -13 <= 0 /\ 1 * s V_mdct_init48_k + -15 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 15 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 185 => (1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -14 <= 0)%Z
   | 186 => (1 * s V_mdct_init48_k + -14 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0)%Z
   | 187 => (1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -14 <= 0)%Z
   | 188 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -15 <= 0)%Z
   | 189 => (1 * s V_mdct_init48_k + -15 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 190 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -15 <= 0)%Z
   | 191 => (1 * s V_mdct_init48_k + -15 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_i + -14 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 192 => (1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -6 <= 0)%Z
   | 193 => (1 * s V_mdct_init48_k + -6 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0)%Z
   | 194 => (1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -6 <= 0)%Z
   | 195 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -7 <= 0)%Z
   | 196 => (1 * s V_mdct_init48_k + -7 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 197 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -7 <= 0)%Z
   | 198 => (1 * s V_mdct_init48_k + -7 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_j <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 199 => (False)%Z
   | 200 => (False)%Z
   | 201 => (False)%Z
   | 202 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -2 <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0)%Z
   | 203 => (-1 * s V_mdct_init48_j + 1 <= 0 /\ 1 * s V_mdct_init48_j + -2 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 204 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -2 <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0)%Z
   | 205 => (1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k + 6 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 206 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 6 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0)%Z
   | 207 => (1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k + 6 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 208 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0)%Z
   | 209 => (1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 210 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0)%Z
   | 211 => (1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_k + 7 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 212 => (1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -2 <= 0)%Z
   | 213 => (1 * s V_mdct_init48_k + -2 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0)%Z
   | 214 => (1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -2 <= 0)%Z
   | 215 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -3 <= 0)%Z
   | 216 => (1 * s V_mdct_init48_k + -3 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 217 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -3 <= 0)%Z
   | 218 => (1 * s V_mdct_init48_k + -3 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_j + -3 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 219 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -10 <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0)%Z
   | 220 => (-1 * s V_mdct_init48_j + 1 <= 0 /\ 1 * s V_mdct_init48_j + -10 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 9 <= 0)%Z
   | 221 => (-1 * s V_mdct_init48_k + 9 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0 /\ 1 * s V_mdct_init48_j + -10 <= 0 /\ -1 * s V_mdct_init48_j + 1 <= 0)%Z
   | 222 => (1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 223 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0)%Z
   | 224 => (1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 225 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0)%Z
   | 226 => (1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 227 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0)%Z
   | 228 => (1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 229 => (1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 230 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0)%Z
   | 231 => (1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 232 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0)%Z
   | 233 => (1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 234 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -9 <= 0)%Z
   | 235 => (1 * s V_mdct_init48_k + -9 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_j + -11 <= 0 /\ -1 * s V_mdct_init48_i + 36 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 236 => (-1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -35 <= 0)%Z
   | 237 => (1 * s V_mdct_init48_i + -35 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0)%Z
   | 238 => (-1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -35 <= 0)%Z
   | 239 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 240 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 241 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 242 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 243 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 30 <= 0 /\ 1 * s V_mdct_init48_i + -35 <= 0)%Z
   | 244 => (1 * s V_mdct_init48_i + -35 <= 0 /\ -1 * s V_mdct_init48_i + 30 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 245 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 30 <= 0 /\ 1 * s V_mdct_init48_i + -35 <= 0)%Z
   | 246 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_i + 31 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 247 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 31 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 248 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_i + 31 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 249 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 31 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 250 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 24 <= 0 /\ 1 * s V_mdct_init48_i + -29 <= 0)%Z
   | 251 => (1 * s V_mdct_init48_i + -29 <= 0 /\ -1 * s V_mdct_init48_i + 24 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 252 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 24 <= 0 /\ 1 * s V_mdct_init48_i + -29 <= 0)%Z
   | 253 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_i + 25 <= 0 /\ 1 * s V_mdct_init48_i + -30 <= 0)%Z
   | 254 => (1 * s V_mdct_init48_i + -30 <= 0 /\ -1 * s V_mdct_init48_i + 25 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 255 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_i + 25 <= 0 /\ 1 * s V_mdct_init48_i + -30 <= 0)%Z
   | 256 => (1 * s V_mdct_init48_i + -30 <= 0 /\ -1 * s V_mdct_init48_i + 25 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 257 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 18 <= 0 /\ 1 * s V_mdct_init48_i + -23 <= 0)%Z
   | 258 => (1 * s V_mdct_init48_i + -23 <= 0 /\ -1 * s V_mdct_init48_i + 18 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 259 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i + 18 <= 0 /\ 1 * s V_mdct_init48_i + -23 <= 0)%Z
   | 260 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_i + 19 <= 0 /\ 1 * s V_mdct_init48_i + -24 <= 0)%Z
   | 261 => (1 * s V_mdct_init48_i + -24 <= 0 /\ -1 * s V_mdct_init48_i + 19 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 262 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_i + 19 <= 0 /\ 1 * s V_mdct_init48_i + -24 <= 0)%Z
   | 263 => (1 * s V_mdct_init48_i + -24 <= 0 /\ -1 * s V_mdct_init48_i + 19 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 264 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -17 <= 0)%Z
   | 265 => (1 * s V_mdct_init48_i + -17 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 266 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -17 <= 0)%Z
   | 267 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_i + -18 <= 0)%Z
   | 268 => (1 * s V_mdct_init48_i + -18 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 269 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_i + -18 <= 0)%Z
   | 270 => (1 * s V_mdct_init48_i + -18 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 271 => (-1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -35 <= 0)%Z
   | 272 => (1 * s V_mdct_init48_i + -35 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0)%Z
   | 273 => (-1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_i <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_i + -35 <= 0)%Z
   | 274 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 275 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 276 => (-1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ 1 * s V_mdct_init48_i + -36 <= 0)%Z
   | 277 => (1 * s V_mdct_init48_i + -36 <= 0 /\ -1 * s V_mdct_init48_i + 1 <= 0 /\ -1 * s V_mdct_init48_k + 8 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | 278 => (-1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -7 <= 0)%Z
   | 279 => (1 * s V_mdct_init48_k + -7 <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k <= 0)%Z
   | 280 => (-1 * s V_mdct_init48_k <= 0 /\ -1 * s V_mdct_init48_z <= 0 /\ 1 * s V_mdct_init48_k + -7 <= 0)%Z
   | 281 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 282 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ -1 * s V_mdct_init48_z <= 0)%Z
   | 283 => (-1 * s V_mdct_init48_z <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ 1 * s V_mdct_init48_k + -8 <= 0)%Z
   | 284 => (1 * s V_mdct_init48_k + -8 <= 0 /\ -1 * s V_mdct_init48_k + 1 <= 0 /\ -1 * s V_mdct_init48_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mdct_init48 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1171 # 1) <= z)%Q
   | 2 => ((1171 # 1) + s V_mdct_init48_z <= z)%Q
   | 3 => ((1171 # 1) + s V_mdct_init48_z - max0(s V_mdct_init48_k) <= z)%Q
   | 4 => ((1171 # 1) + s V_mdct_init48_z - max0(s V_mdct_init48_k) <= z)%Q
   | 5 => ((1171 # 1) + s V_mdct_init48_z - max0(s V_mdct_init48_k) <= z)%Q
   | 6 => ((1171 # 1) + s V_mdct_init48_z - max0(s V_mdct_init48_k) <= z)%Q
   | 7 => ((1171 # 1) + s V_mdct_init48_z - max0(s V_mdct_init48_k) <= z)%Q
   | 8 => ((1135 # 1) + s V_mdct_init48_z + max0(36 - s V_mdct_init48_i)
           - max0(s V_mdct_init48_k) <= z)%Q
   | 9 => ((1135 # 1) + s V_mdct_init48_z + max0(36 - s V_mdct_init48_i)
           - max0(s V_mdct_init48_k) <= z)%Q
   | 10 => ((1135 # 1) + s V_mdct_init48_z + max0(36 - s V_mdct_init48_i)
            - max0(s V_mdct_init48_k) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (36 - s V_mdct_init48_i) (35
                                                                    - s V_mdct_init48_i));
      (*-1 0*) F_max0_ge_0 (35 - s V_mdct_init48_i)]
     ((1135 # 1) + s V_mdct_init48_z + max0(36 - s V_mdct_init48_i)
      - max0(s V_mdct_init48_k) <= z)%Q
   | 12 => ((1135 # 1) + s V_mdct_init48_z - max0(s V_mdct_init48_k) <= z)%Q
   | 13 => ((2261 # 2) - (3 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
            + (1 # 4) * max0(18 - s V_mdct_init48_i)
            - max0(s V_mdct_init48_k) <= z)%Q
   | 14 => ((2261 # 2) - (3 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
            + (1 # 4) * max0(18 - s V_mdct_init48_i)
            - max0(s V_mdct_init48_k) <= z)%Q
   | 15 => ((2261 # 2) - (3 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
            + (1 # 4) * max0(18 - s V_mdct_init48_i)
            - max0(s V_mdct_init48_k) <= z)%Q
   | 16 => hints
     [(*0 0.75*) F_binom_monotonic 1 (F_max0_ge_arg (-14 + s V_mdct_init48_i)) (F_check_ge (-14
                                                                    + s V_mdct_init48_i) (0))]
     ((2261 # 2) - (3 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
      + (1 # 4) * max0(18 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 17 => ((1120 # 1) + s V_mdct_init48_z
            - (3 # 4) * max0(-14 + s V_mdct_init48_i)
            + (1 # 4) * max0(18 - s V_mdct_init48_i)
            - max0(s V_mdct_init48_k) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_ge_0 (7 - s V_mdct_init48_k);
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_0 (-18 + s V_mdct_init48_i)) (F_check_ge (0) (0))]
     ((1120 # 1) + s V_mdct_init48_z
      - (3 # 4) * max0(-14 + s V_mdct_init48_i)
      + (1 # 4) * max0(18 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 19 => ((1120 # 1) + s V_mdct_init48_z
            - (1 # 4) * max0(-18 + s V_mdct_init48_i)
            - (3 # 4) * max0(-14 + s V_mdct_init48_i)
            - max0(7 - s V_mdct_init48_k)
            + (1 # 4) * max0(18 - s V_mdct_init48_i)
            - max0(s V_mdct_init48_k) <= z)%Q
   | 20 => hints
     [(*0 0.25*) F_binom_monotonic 1 (F_max0_ge_arg (-19 + s V_mdct_init48_i)) (F_check_ge (-19
                                                                    + s V_mdct_init48_i) (0))]
     ((1120 # 1) + s V_mdct_init48_z
      - (1 # 4) * max0(-18 + s V_mdct_init48_i)
      - (3 # 4) * max0(-14 + s V_mdct_init48_i) - max0(7 - s V_mdct_init48_k)
      + (1 # 4) * max0(18 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 21 => ((4461 # 4) + (1 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
            - (1 # 4) * max0(-19 + s V_mdct_init48_i)
            - (1 # 4) * max0(-18 + s V_mdct_init48_i)
            - (3 # 4) * max0(-14 + s V_mdct_init48_i)
            - max0(7 - s V_mdct_init48_k)
            + (1 # 4) * max0(18 - s V_mdct_init48_i)
            - max0(s V_mdct_init48_k) <= z)%Q
   | 22 => hints
     [(*-0.25 0*) F_max0_monotonic (F_check_ge (18 - s V_mdct_init48_i) (17
                                                                    - s V_mdct_init48_i));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mdct_init48_i)) (F_check_ge (s V_mdct_init48_i) (0));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_mdct_init48_i)) (F_check_ge (-1
                                                                    + s V_mdct_init48_i) (0))]
     ((4461 # 4) + (1 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
      - (1 # 4) * max0(-19 + s V_mdct_init48_i)
      - (1 # 4) * max0(-18 + s V_mdct_init48_i)
      - (3 # 4) * max0(-14 + s V_mdct_init48_i) - max0(7 - s V_mdct_init48_k)
      + (1 # 4) * max0(18 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 23 => ((1115 # 1) + (3 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
            - (1 # 4) * max0(-19 + s V_mdct_init48_i)
            - (1 # 4) * max0(-18 + s V_mdct_init48_i)
            - (3 # 4) * max0(-14 + s V_mdct_init48_i)
            - (1 # 4) * max0(-1 + s V_mdct_init48_i)
            - max0(7 - s V_mdct_init48_k)
            + (1 # 4) * max0(17 - s V_mdct_init48_i)
            - (1 # 4) * max0(s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 24 => hints
     [(*0 0.75*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-14
                                                                 + s V_mdct_init48_i) (0))) (F_max0_ge_0 (-14
                                                                    + s V_mdct_init48_i))]
     ((1115 # 1) + (3 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
      - (1 # 4) * max0(-19 + s V_mdct_init48_i)
      - (1 # 4) * max0(-18 + s V_mdct_init48_i)
      - (3 # 4) * max0(-14 + s V_mdct_init48_i)
      - (1 # 4) * max0(-1 + s V_mdct_init48_i) - max0(7 - s V_mdct_init48_k)
      + (1 # 4) * max0(17 - s V_mdct_init48_i)
      - (1 # 4) * max0(s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 25 => ((2251 # 2) + s V_mdct_init48_z
            - (1 # 4) * max0(-19 + s V_mdct_init48_i)
            - (1 # 4) * max0(-18 + s V_mdct_init48_i)
            - (1 # 4) * max0(-1 + s V_mdct_init48_i)
            - max0(7 - s V_mdct_init48_k)
            + (1 # 4) * max0(17 - s V_mdct_init48_i)
            - (1 # 4) * max0(s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 26 => ((2251 # 2) + s V_mdct_init48_z
            - (1 # 4) * max0(-19 + s V_mdct_init48_i)
            - (1 # 4) * max0(-18 + s V_mdct_init48_i)
            - (1 # 4) * max0(-1 + s V_mdct_init48_i)
            - max0(7 - s V_mdct_init48_k)
            + (1 # 4) * max0(17 - s V_mdct_init48_i)
            - (1 # 4) * max0(s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 27 => ((2251 # 2) + s V_mdct_init48_z
            - (1 # 4) * max0(-19 + s V_mdct_init48_i)
            - (1 # 4) * max0(-18 + s V_mdct_init48_i)
            - (1 # 4) * max0(-1 + s V_mdct_init48_i)
            - max0(7 - s V_mdct_init48_k)
            + (1 # 4) * max0(17 - s V_mdct_init48_i)
            - (1 # 4) * max0(s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 28 => hints
     [(*-0.25 0*) F_max0_ge_0 (17 - s V_mdct_init48_i);
      (*-1 0*) F_max0_monotonic (F_check_ge (36 - s V_mdct_init48_i) (35
                                                                    - s V_mdct_init48_i));
      (*-1 0*) F_max0_ge_0 (35 - s V_mdct_init48_i);
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mdct_init48_i) (0))) (F_max0_ge_0 (s V_mdct_init48_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                               - s V_mdct_init48_i) (0))) (F_max0_ge_0 (36
                                                                    - s V_mdct_init48_i));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                  + s V_mdct_init48_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_mdct_init48_i));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-18
                                                                  + s V_mdct_init48_i) (0))) (F_max0_ge_0 (-18
                                                                    + s V_mdct_init48_i));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-19
                                                                  + s V_mdct_init48_i) (0))) (F_max0_ge_0 (-19
                                                                    + s V_mdct_init48_i))]
     ((2251 # 2) + s V_mdct_init48_z
      - (1 # 4) * max0(-19 + s V_mdct_init48_i)
      - (1 # 4) * max0(-18 + s V_mdct_init48_i)
      - (1 # 4) * max0(-1 + s V_mdct_init48_i) - max0(7 - s V_mdct_init48_k)
      + (1 # 4) * max0(17 - s V_mdct_init48_i)
      - (1 # 4) * max0(s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 29 => ((1099 # 1) + s V_mdct_init48_z - max0(7 - s V_mdct_init48_k)
            - max0(s V_mdct_init48_k) <= z)%Q
   | 30 => ((1063 # 1) + s V_mdct_init48_z - max0(7 - s V_mdct_init48_k)
            + max0(36 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 31 => ((1063 # 1) + s V_mdct_init48_z - max0(7 - s V_mdct_init48_k)
            + max0(36 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 32 => ((1063 # 1) + s V_mdct_init48_z - max0(7 - s V_mdct_init48_k)
            + max0(36 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (9 - s V_mdct_init48_k) (1);
      (*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_mdct_init48_k) (7
                                                                    - 
                                                                    s V_mdct_init48_k));
      (*-1 0*) F_max0_ge_0 (35 - s V_mdct_init48_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mdct_init48_k) (0))) (F_max0_ge_0 (s V_mdct_init48_k));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                               - s V_mdct_init48_k) (0))) (F_max0_ge_0 (9
                                                                    - s V_mdct_init48_k))]
     ((1063 # 1) + s V_mdct_init48_z - max0(7 - s V_mdct_init48_k)
      + max0(36 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 34 => ((1055 # 1) + s V_mdct_init48_z - max0(35 - s V_mdct_init48_i)
            + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 35 => ((865 # 1) - (19 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
            + (19 # 1) * max0(-1 + s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
            + (19 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 36 => ((865 # 1) - (19 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
            + (19 # 1) * max0(-1 + s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
            + (19 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 37 => ((865 # 1) - (19 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
            + (19 # 1) * max0(-1 + s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
            + (19 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 38 => ((865 # 1) - (19 # 1) * s V_mdct_init48_j - s V_mdct_init48_k
            + s V_mdct_init48_z + (19 # 1) * max0(-1 + s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
            + (19 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 39 => hints
     [(*-19 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mdct_init48_j)) (F_check_ge (s V_mdct_init48_j) (0))]
     ((865 # 1) - (19 # 1) * s V_mdct_init48_j - s V_mdct_init48_k
      + s V_mdct_init48_z + (19 # 1) * max0(-1 + s V_mdct_init48_j)
      - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
      + (19 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 40 => ((865 # 1) - s V_mdct_init48_k + s V_mdct_init48_z
            + (19 # 1) * max0(-1 + s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 41 => hints
     [(*-1 0*) F_max0_ge_0 (9 - s V_mdct_init48_k);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                               - s V_mdct_init48_k) (0))) (F_max0_ge_0 (9
                                                                    - s V_mdct_init48_k))]
     ((865 # 1) - s V_mdct_init48_k + s V_mdct_init48_z
      + (19 # 1) * max0(-1 + s V_mdct_init48_j)
      - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 42 => ((856 # 1) + s V_mdct_init48_z
            + (19 # 1) * max0(-1 + s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 43 => ((847 # 1) + s V_mdct_init48_z
            + (19 # 1) * max0(-1 + s V_mdct_init48_j)
            + max0(9 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
            + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 44 => ((847 # 1) + s V_mdct_init48_z
            + (19 # 1) * max0(-1 + s V_mdct_init48_j)
            + max0(9 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
            + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 45 => ((847 # 1) + s V_mdct_init48_z
            + (19 # 1) * max0(-1 + s V_mdct_init48_j)
            + max0(9 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
            + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 46 => ((847 # 1) + s V_mdct_init48_z
            + (19 # 1) * max0(-1 + s V_mdct_init48_j)
            + max0(9 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
            + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 47 => ((847 # 1) + s V_mdct_init48_z
            + (19 # 1) * max0(-1 + s V_mdct_init48_j)
            + max0(9 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
            + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 48 => ((847 # 1) + s V_mdct_init48_z
            + (19 # 1) * max0(-1 + s V_mdct_init48_j)
            + max0(9 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
            + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 49 => hints
     [(*0 1*) F_max0_ge_0 (9 - s V_mdct_init48_k)]
     ((847 # 1) + s V_mdct_init48_z + max0(9 - s V_mdct_init48_k)
      - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
      + (19 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 50 => ((847 # 1) + s V_mdct_init48_z - max0(35 - s V_mdct_init48_i)
            + max0(36 - s V_mdct_init48_i)
            + (19 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 51 => hints
     [(*-19 0*) F_max0_monotonic (F_check_ge (s V_mdct_init48_j) (-1
                                                                  + s V_mdct_init48_j));
      (*-19 0*) F_max0_ge_0 (-1 + s V_mdct_init48_j)]
     ((847 # 1) + s V_mdct_init48_z - max0(35 - s V_mdct_init48_i)
      + max0(36 - s V_mdct_init48_i) + (19 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 52 => ((847 # 1) + s V_mdct_init48_z - max0(35 - s V_mdct_init48_i)
            + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 53 => ((1519 # 2) + s V_mdct_init48_z
            + (7 # 1) * max0(-1 + s V_mdct_init48_j)
            + (7 # 2) * max0(10 - s V_mdct_init48_j)
            + (7 # 2) * max0(11 - s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
            + (7 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 54 => ((1519 # 2) + s V_mdct_init48_z
            + (7 # 1) * max0(-1 + s V_mdct_init48_j)
            + (7 # 2) * max0(10 - s V_mdct_init48_j)
            + (7 # 2) * max0(11 - s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
            + (7 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 55 => ((1519 # 2) + s V_mdct_init48_z
            + (7 # 1) * max0(-1 + s V_mdct_init48_j)
            + (7 # 2) * max0(10 - s V_mdct_init48_j)
            + (7 # 2) * max0(11 - s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
            + (7 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 56 => ((756 # 1) + s V_mdct_init48_z
            + (7 # 1) * max0(-1 + s V_mdct_init48_j)
            + (1 # 2) * max0(3 - s V_mdct_init48_k)
            + (1 # 2) * max0(4 - s V_mdct_init48_k)
            + (7 # 2) * max0(10 - s V_mdct_init48_j)
            + (7 # 2) * max0(11 - s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
            + (7 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 57 => hints
     [(*-7 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mdct_init48_j)) (F_check_ge (s V_mdct_init48_j) (0))]
     ((756 # 1) + s V_mdct_init48_z + (7 # 1) * max0(-1 + s V_mdct_init48_j)
      + (1 # 2) * max0(3 - s V_mdct_init48_k)
      + (1 # 2) * max0(4 - s V_mdct_init48_k)
      + (7 # 2) * max0(10 - s V_mdct_init48_j)
      + (7 # 2) * max0(11 - s V_mdct_init48_j) - max0(35 - s V_mdct_init48_i)
      + max0(36 - s V_mdct_init48_i) + (7 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 58 => ((756 # 1) + (7 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
            + (7 # 1) * max0(-1 + s V_mdct_init48_j)
            + (1 # 2) * max0(3 - s V_mdct_init48_k)
            + (1 # 2) * max0(4 - s V_mdct_init48_k)
            + (7 # 2) * max0(10 - s V_mdct_init48_j)
            + (7 # 2) * max0(11 - s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 59 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (4 - s V_mdct_init48_k) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3 - s V_mdct_init48_k)) (F_check_ge (0) (0))]
     ((756 # 1) + (7 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
      + (7 # 1) * max0(-1 + s V_mdct_init48_j)
      + (1 # 2) * max0(3 - s V_mdct_init48_k)
      + (1 # 2) * max0(4 - s V_mdct_init48_k)
      + (7 # 2) * max0(10 - s V_mdct_init48_j)
      + (7 # 2) * max0(11 - s V_mdct_init48_j) - max0(35 - s V_mdct_init48_i)
      + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 60 => ((1513 # 2) + (7 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
            + (7 # 1) * max0(-1 + s V_mdct_init48_j)
            + (7 # 2) * max0(10 - s V_mdct_init48_j)
            + (7 # 2) * max0(11 - s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 61 => ((1507 # 2) + (7 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
            + (7 # 1) * max0(-1 + s V_mdct_init48_j)
            + max0(9 - s V_mdct_init48_k)
            + (7 # 2) * max0(10 - s V_mdct_init48_j)
            + (7 # 2) * max0(11 - s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 62 => ((1507 # 2) + (7 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
            + (7 # 1) * max0(-1 + s V_mdct_init48_j)
            + max0(9 - s V_mdct_init48_k)
            + (7 # 2) * max0(10 - s V_mdct_init48_j)
            + (7 # 2) * max0(11 - s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 63 => ((1507 # 2) + (7 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
            + (7 # 1) * max0(-1 + s V_mdct_init48_j)
            + max0(9 - s V_mdct_init48_k)
            + (7 # 2) * max0(10 - s V_mdct_init48_j)
            + (7 # 2) * max0(11 - s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 64 => hints
     [(*-3.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (10 - s V_mdct_init48_j)) (F_check_ge (10
                                                                    - s V_mdct_init48_j) (0))]
     ((1507 # 2) + (7 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
      + (7 # 1) * max0(-1 + s V_mdct_init48_j) + max0(9 - s V_mdct_init48_k)
      + (7 # 2) * max0(10 - s V_mdct_init48_j)
      + (7 # 2) * max0(11 - s V_mdct_init48_j) - max0(35 - s V_mdct_init48_i)
      + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 65 => ((1577 # 2) + (7 # 2) * s V_mdct_init48_j + s V_mdct_init48_z
            + (7 # 1) * max0(-1 + s V_mdct_init48_j)
            + max0(9 - s V_mdct_init48_k)
            + (7 # 2) * max0(11 - s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 66 => ((1577 # 2) + (7 # 2) * s V_mdct_init48_j + s V_mdct_init48_z
            + (7 # 1) * max0(-1 + s V_mdct_init48_j)
            + max0(9 - s V_mdct_init48_k)
            + (7 # 2) * max0(11 - s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 67 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (9 - s V_mdct_init48_k) (8
                                                                    - 
                                                                    s V_mdct_init48_k));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (8 - s V_mdct_init48_k)) (F_check_ge (0) (0))]
     ((792 # 1) + (7 # 2) * s V_mdct_init48_j + s V_mdct_init48_z
      + max0(9 - s V_mdct_init48_k) + (7 # 2) * max0(10 - s V_mdct_init48_j)
      - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
      + (7 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 68 => ((792 # 1) + (7 # 2) * s V_mdct_init48_j + s V_mdct_init48_z
            + (7 # 2) * max0(10 - s V_mdct_init48_j)
            - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
            + (7 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 69 => hints
     [(*-7 0*) F_max0_monotonic (F_check_ge (s V_mdct_init48_j) (-1
                                                                 + s V_mdct_init48_j));
      (*-7 0*) F_max0_ge_0 (-1 + s V_mdct_init48_j);
      (*-1 0*) F_max0_monotonic (F_check_ge (36 - s V_mdct_init48_i) (35
                                                                    - s V_mdct_init48_i));
      (*-3.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (10 - s V_mdct_init48_j)) (F_check_ge (10
                                                                    - s V_mdct_init48_j) (0))]
     ((792 # 1) + (7 # 2) * s V_mdct_init48_j + s V_mdct_init48_z
      + (7 # 2) * max0(10 - s V_mdct_init48_j) - max0(35 - s V_mdct_init48_i)
      + max0(36 - s V_mdct_init48_i) + (7 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 70 => ((827 # 1) + s V_mdct_init48_z <= z)%Q
   | 71 => ((827 # 1) + s V_mdct_init48_z <= z)%Q
   | 72 => ((827 # 1) + s V_mdct_init48_z <= z)%Q
   | 73 => ((827 # 1) + s V_mdct_init48_z <= z)%Q
   | 74 => ((827 # 1) + s V_mdct_init48_z <= z)%Q
   | 75 => ((827 # 1) + s V_mdct_init48_z <= z)%Q
   | 76 => ((827 # 1) + s V_mdct_init48_z <= z)%Q
   | 77 => ((827 # 1) + s V_mdct_init48_z <= z)%Q
   | 78 => ((827 # 1) + s V_mdct_init48_z <= z)%Q
   | 79 => ((2456 # 3) + s V_mdct_init48_z
            + (1 # 3) * max0(7 - s V_mdct_init48_k)
            + (2 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 80 => hints
     [(*-0.333333 0*) F_max0_pre_decrement 1 (9 - s V_mdct_init48_k) (1)]
     ((2456 # 3) + s V_mdct_init48_z + (1 # 3) * max0(7 - s V_mdct_init48_k)
      + (2 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 81 => ((819 # 1) + s V_mdct_init48_z
            + (1 # 3) * max0(7 - s V_mdct_init48_k)
            + (1 # 3) * max0(8 - s V_mdct_init48_k)
            + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 82 => ((819 # 1) + s V_mdct_init48_z
            + (1 # 3) * max0(7 - s V_mdct_init48_k)
            + (1 # 3) * max0(8 - s V_mdct_init48_k)
            + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 83 => ((819 # 1) + s V_mdct_init48_z
            + (1 # 3) * max0(7 - s V_mdct_init48_k)
            + (1 # 3) * max0(8 - s V_mdct_init48_k)
            + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 84 => ((579 # 1) + s V_mdct_init48_z
            + (16 # 1) * max0(1 + s V_mdct_init48_i)
            + (1 # 3) * max0(7 - s V_mdct_init48_k)
            + (1 # 3) * max0(8 - s V_mdct_init48_k)
            + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 85 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (7 - s V_mdct_init48_k) (6
                                                                    - 
                                                                    s V_mdct_init48_k));
      (*-1 0*) F_max0_ge_0 (6 - s V_mdct_init48_k);
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                          - s V_mdct_init48_k)) (F_check_ge (9
                                                                    - s V_mdct_init48_k) (0));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                          - s V_mdct_init48_k)) (F_check_ge (8
                                                                    - s V_mdct_init48_k) (0));
      (*-0.666667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - s V_mdct_init48_k) (0))) (F_max0_ge_0 (7
                                                                    - s V_mdct_init48_k))]
     ((579 # 1) + s V_mdct_init48_z + (16 # 1) * max0(1 + s V_mdct_init48_i)
      + (1 # 3) * max0(7 - s V_mdct_init48_k)
      + (1 # 3) * max0(8 - s V_mdct_init48_k)
      + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 86 => ((580 # 1) + s V_mdct_init48_z
            + (16 # 1) * max0(1 + s V_mdct_init48_i) <= z)%Q
   | 87 => ((580 # 1) + s V_mdct_init48_z
            + (16 # 1) * max0(1 + s V_mdct_init48_i) <= z)%Q
   | 88 => ((580 # 1) + s V_mdct_init48_z
            + (16 # 1) * max0(1 + s V_mdct_init48_i) <= z)%Q
   | 89 => ((572 # 1) + s V_mdct_init48_z
            + (16 # 1) * max0(1 + s V_mdct_init48_i)
            + (1 # 3) * max0(7 - s V_mdct_init48_k)
            + (1 # 3) * max0(8 - s V_mdct_init48_k)
            + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 90 => ((572 # 1) + s V_mdct_init48_z
            + (16 # 1) * max0(1 + s V_mdct_init48_i)
            + (1 # 3) * max0(7 - s V_mdct_init48_k)
            + (1 # 3) * max0(8 - s V_mdct_init48_k)
            + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 91 => ((572 # 1) + s V_mdct_init48_z
            + (16 # 1) * max0(1 + s V_mdct_init48_i)
            + (1 # 3) * max0(7 - s V_mdct_init48_k)
            + (1 # 3) * max0(8 - s V_mdct_init48_k)
            + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 92 => hints
     [(*0 0.333333*) F_max0_pre_decrement 1 (9 - s V_mdct_init48_k) (1);
      (*-16 0*) F_max0_monotonic (F_check_ge (1 + s V_mdct_init48_i) (s V_mdct_init48_i));
      (*-16 0*) F_max0_ge_0 (s V_mdct_init48_i);
      (*0 61.4099*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + 
                                                                    s V_mdct_init48_k) (0))) (F_max0_ge_0 (-3
                                                                    + s V_mdct_init48_k))]
     ((572 # 1) + s V_mdct_init48_z + (16 # 1) * max0(1 + s V_mdct_init48_i)
      + (1 # 3) * max0(7 - s V_mdct_init48_k)
      + (1 # 3) * max0(8 - s V_mdct_init48_k)
      + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 93 => ((77926 # 103) - (3746 # 61) * s V_mdct_init48_k
            + s V_mdct_init48_z + (3746 # 61) * max0(-3 + s V_mdct_init48_k)
            + (1 # 3) * max0(7 - s V_mdct_init48_k)
            + (2 # 3) * max0(8 - s V_mdct_init48_k) <= z)%Q
   | 94 => ((21540 # 89) - (3746 # 61) * s V_mdct_init48_k
            + s V_mdct_init48_z + (3746 # 61) * max0(-3 + s V_mdct_init48_k)
            + (32 # 1) * max0(1 + s V_mdct_init48_i)
            + (1 # 3) * max0(7 - s V_mdct_init48_k)
            + (2 # 3) * max0(8 - s V_mdct_init48_k)
            - (94 # 37) * max0(35 - s V_mdct_init48_i)
            + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 95 => hints
     [(*-0.666667 0*) F_max0_pre_decrement 1 (8 - s V_mdct_init48_k) (1);
      (*-1 0*) F_max0_monotonic (F_check_ge (7 - s V_mdct_init48_k) (6
                                                                    - 
                                                                    s V_mdct_init48_k));
      (*-1 0*) F_max0_ge_0 (6 - s V_mdct_init48_k)]
     ((21540 # 89) - (3746 # 61) * s V_mdct_init48_k + s V_mdct_init48_z
      + (3746 # 61) * max0(-3 + s V_mdct_init48_k)
      + (32 # 1) * max0(1 + s V_mdct_init48_i)
      + (1 # 3) * max0(7 - s V_mdct_init48_k)
      + (2 # 3) * max0(8 - s V_mdct_init48_k)
      - (94 # 37) * max0(35 - s V_mdct_init48_i)
      + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 96 => ((17959 # 74) - (3746 # 61) * s V_mdct_init48_k
            + s V_mdct_init48_z + (3746 # 61) * max0(-3 + s V_mdct_init48_k)
            + (32 # 1) * max0(1 + s V_mdct_init48_i)
            - (94 # 37) * max0(35 - s V_mdct_init48_i)
            + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 97 => hints
     [(*-61.4099 0*) F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                         + s V_mdct_init48_k)) (F_check_ge (-3
                                                                    + s V_mdct_init48_k) (0))]
     ((17959 # 74) - (3746 # 61) * s V_mdct_init48_k + s V_mdct_init48_z
      + (3746 # 61) * max0(-3 + s V_mdct_init48_k)
      + (32 # 1) * max0(1 + s V_mdct_init48_i)
      - (94 # 37) * max0(35 - s V_mdct_init48_i)
      + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 98 => ((2163 # 37) + s V_mdct_init48_z
            + (32 # 1) * max0(1 + s V_mdct_init48_i)
            - (94 # 37) * max0(35 - s V_mdct_init48_i)
            + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 99 => ((2015 # 37) + s V_mdct_init48_z
            + (32 # 1) * max0(1 + s V_mdct_init48_i)
            + max0(4 - s V_mdct_init48_k)
            - (94 # 37) * max0(35 - s V_mdct_init48_i)
            + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 100 => ((2015 # 37) + s V_mdct_init48_z
             + (32 # 1) * max0(1 + s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k)
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 101 => ((2015 # 37) + s V_mdct_init48_z
             + (32 # 1) * max0(1 + s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k)
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 102 => hints
     [(*-32 0*) F_max0_monotonic (F_check_ge (1 + s V_mdct_init48_i) (s V_mdct_init48_i));
      (*-32 0*) F_max0_ge_0 (s V_mdct_init48_i);
      (*-2.54054 0*) F_binom_monotonic 1 (F_max0_ge_arg (36
                                                         - s V_mdct_init48_i)) (F_check_ge (36
                                                                    - s V_mdct_init48_i) (0));
      (*-2.54054 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (35
                                                                    - 
                                                                    s V_mdct_init48_i) (0))) (F_max0_ge_0 (35
                                                                    - s V_mdct_init48_i))]
     ((2015 # 37) + s V_mdct_init48_z
      + (32 # 1) * max0(1 + s V_mdct_init48_i) + max0(4 - s V_mdct_init48_k)
      - (94 # 37) * max0(35 - s V_mdct_init48_i)
      + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 103 => ((57 # 1) + s V_mdct_init48_z + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 104 => ((57 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 105 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mdct_init48_z) (0))) (F_max0_ge_0 (s V_mdct_init48_z))]
     ((57 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
      + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 106 => ((57 # 1) - s V_mdct_init48_i + max0(4 - s V_mdct_init48_k)
             + max0(s V_mdct_init48_z) <= z)%Q
   | 107 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (36 - s V_mdct_init48_i) (35
                                                                    - s V_mdct_init48_i));
      (*-1 0*) F_max0_ge_0 (35 - s V_mdct_init48_i);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                              - s V_mdct_init48_i) (0))) (F_max0_ge_0 (36
                                                                    - s V_mdct_init48_i))]
     ((57 # 1) - s V_mdct_init48_i + max0(4 - s V_mdct_init48_k)
      + max0(s V_mdct_init48_z) <= z)%Q
   | 108 => ((21 # 1) + max0(4 - s V_mdct_init48_k) + max0(s V_mdct_init48_z) <= z)%Q
   | 109 => ((7 # 1) * max0(3 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) + max0(s V_mdct_init48_z) <= z)%Q
   | 110 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mdct_init48_z)) (F_check_ge (s V_mdct_init48_z) (0))]
     ((7 # 1) * max0(3 - s V_mdct_init48_i) + max0(4 - s V_mdct_init48_k)
      + max0(s V_mdct_init48_z) <= z)%Q
   | 111 => (s V_mdct_init48_z + (7 # 1) * max0(3 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 112 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3 - s V_mdct_init48_k) (2
                                                                    - 
                                                                    s V_mdct_init48_k));
      (*-7 0*) F_max0_ge_0 (3 - s V_mdct_init48_i);
      (*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_mdct_init48_k) (3
                                                                    - 
                                                                    s V_mdct_init48_k));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (2 - s V_mdct_init48_k)) (F_check_ge (0) (0))]
     (s V_mdct_init48_z + (7 # 1) * max0(3 - s V_mdct_init48_i)
      + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 113 => (s V_mdct_init48_z <= z)%Q
   | 114 => (s V_mdct_init48_z + (7 # 1) * max0(3 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 115 => (s V_mdct_init48_z + (7 # 1) * max0(3 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 116 => (-s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(3 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 117 => hints
     [(*-7 0*) F_max0_pre_decrement 1 (3 - s V_mdct_init48_i) (1)]
     (-s V_mdct_init48_m + s V_mdct_init48_z
      + (7 # 1) * max0(3 - s V_mdct_init48_i) + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 118 => ((7 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(2 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 119 => ((7 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(2 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 120 => ((7 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(2 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 121 => ((7 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(2 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 122 => ((7 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(3 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 123 => ((7 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(3 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 124 => ((7 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(3 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 125 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (6 - s V_mdct_init48_m) (5
                                                                    - 
                                                                    s V_mdct_init48_m));
      (*-1 0*) F_max0_ge_0 (5 - s V_mdct_init48_m);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (6
                                                               - s V_mdct_init48_m) (0))) (F_max0_ge_0 (6
                                                                    - s V_mdct_init48_m))]
     ((6 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
      + (7 # 1) * max0(3 - s V_mdct_init48_i) + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 126 => ((7 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(2 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 127 => ((7 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(2 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 128 => ((7 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(2 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 129 => ((8 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(2 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 130 => ((8 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(2 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 131 => ((8 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(2 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 132 => ((7 # 1) - s V_mdct_init48_m + s V_mdct_init48_z
             + (7 # 1) * max0(2 - s V_mdct_init48_i)
             + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 133 => ((57 # 1) - s V_mdct_init48_i + max0(4 - s V_mdct_init48_k)
             + max0(s V_mdct_init48_z) <= z)%Q
   | 134 => ((57 # 1) - s V_mdct_init48_i + max0(4 - s V_mdct_init48_k)
             + max0(s V_mdct_init48_z) <= z)%Q
   | 135 => ((57 # 1) - s V_mdct_init48_i + max0(4 - s V_mdct_init48_k)
             + max0(s V_mdct_init48_z) <= z)%Q
   | 136 => ((58 # 1) - s V_mdct_init48_i + max0(4 - s V_mdct_init48_k)
             + max0(s V_mdct_init48_z) <= z)%Q
   | 137 => ((58 # 1) - s V_mdct_init48_i + max0(4 - s V_mdct_init48_k)
             + max0(s V_mdct_init48_z) <= z)%Q
   | 138 => ((58 # 1) - s V_mdct_init48_i + max0(4 - s V_mdct_init48_k)
             + max0(s V_mdct_init48_z) <= z)%Q
   | 139 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mdct_init48_z) (0))) (F_max0_ge_0 (s V_mdct_init48_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_mdct_init48_z)) (F_check_ge (-1
                                                                    + s V_mdct_init48_z) (0))]
     ((58 # 1) - s V_mdct_init48_i + max0(-1 + s V_mdct_init48_z)
      + max0(4 - s V_mdct_init48_k) <= z)%Q
   | 140 => hints
     [(*0 1*) F_max0_pre_decrement 1 (4 - s V_mdct_init48_k) (1);
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (3 - s V_mdct_init48_k)) (F_check_ge (3
                                                                    - s V_mdct_init48_k) (0))]
     ((2015 # 37) + s V_mdct_init48_z
      + (32 # 1) * max0(1 + s V_mdct_init48_i) + max0(4 - s V_mdct_init48_k)
      - (94 # 37) * max0(35 - s V_mdct_init48_i)
      + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 141 => ((2163 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             + (32 # 1) * max0(1 + s V_mdct_init48_i)
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 142 => ((2163 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             + (32 # 1) * max0(1 + s V_mdct_init48_i)
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 143 => ((2200 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             + (32 # 1) * max0(1 + s V_mdct_init48_i)
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 144 => ((2200 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             + (32 # 1) * max0(1 + s V_mdct_init48_i)
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 145 => ((2200 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             + (32 # 1) * max0(1 + s V_mdct_init48_i)
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 146 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_mdct_init48_k) (0))) (F_max0_ge_0 (4
                                                                    - s V_mdct_init48_k))]
     ((2163 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
      + (32 # 1) * max0(1 + s V_mdct_init48_i)
      - (94 # 37) * max0(35 - s V_mdct_init48_i)
      + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 147 => hints
     [(*0 32*) F_max0_pre_decrement 1 (1 + s V_mdct_init48_i) (1);
      (*0 2.54054*) F_max0_pre_increment 1 (36 - s V_mdct_init48_i) (1);
      (*0 61.4099*) F_binom_monotonic 1 (F_max0_ge_arg (-3
                                                        + s V_mdct_init48_k)) (F_check_ge (-3
                                                                    + s V_mdct_init48_k) (0))]
     ((17959 # 74) - (3746 # 61) * s V_mdct_init48_k + s V_mdct_init48_z
      + (3746 # 61) * max0(-3 + s V_mdct_init48_k)
      + (32 # 1) * max0(1 + s V_mdct_init48_i)
      - (94 # 37) * max0(35 - s V_mdct_init48_i)
      + (94 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 148 => ((3253 # 37) + s V_mdct_init48_z
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 149 => ((3290 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 150 => ((3290 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 151 => ((3290 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 152 => hints
     [(*-2.54054 0*) F_max0_pre_increment 1 (36 - s V_mdct_init48_i) (1);
      (*0 2.54054*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (35
                                                                    - 
                                                                    s V_mdct_init48_i) (0))) (F_max0_ge_0 (35
                                                                    - s V_mdct_init48_i))]
     ((3290 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
      - (94 # 37) * max0(35 - s V_mdct_init48_i)
      + (94 # 37) * max0(37 - s V_mdct_init48_i)
      + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 153 => (-(94 # 37) + (94 # 37) * s V_mdct_init48_i - s V_mdct_init48_k
             + s V_mdct_init48_z - (94 # 37) * max0(36 - s V_mdct_init48_i)
             + (188 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 154 => (-(94 # 37) + (94 # 37) * s V_mdct_init48_i - s V_mdct_init48_k
             + s V_mdct_init48_z - (94 # 37) * max0(36 - s V_mdct_init48_i)
             + (188 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 155 => ((94 # 37) * s V_mdct_init48_i - s V_mdct_init48_k
             + s V_mdct_init48_z + (32 # 1) * max0(1 + s V_mdct_init48_i)
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (188 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 156 => ((94 # 37) * s V_mdct_init48_i - s V_mdct_init48_k
             + s V_mdct_init48_z + (32 # 1) * max0(1 + s V_mdct_init48_i)
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (188 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 157 => ((94 # 37) * s V_mdct_init48_i - s V_mdct_init48_k
             + s V_mdct_init48_z + (32 # 1) * max0(1 + s V_mdct_init48_i)
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (188 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 158 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (32 - s V_mdct_init48_k) (31
                                                                    - 
                                                                    s V_mdct_init48_k));
      (*-1 0*) F_max0_ge_0 (31 - s V_mdct_init48_k);
      (*-2.54054 0*) F_binom_monotonic 1 (F_max0_ge_arg (36
                                                         - s V_mdct_init48_i)) (F_check_ge (36
                                                                    - s V_mdct_init48_i) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (32
                                                               - s V_mdct_init48_k) (0))) (F_max0_ge_0 (32
                                                                    - s V_mdct_init48_k));
      (*-61.4099 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + 
                                                                    s V_mdct_init48_k) (0))) (F_max0_ge_0 (-3
                                                                    + s V_mdct_init48_k))]
     (-(1 # 1) + (94 # 37) * s V_mdct_init48_i - s V_mdct_init48_k
      + s V_mdct_init48_z + (32 # 1) * max0(1 + s V_mdct_init48_i)
      - (94 # 37) * max0(35 - s V_mdct_init48_i)
      + (188 # 37) * max0(36 - s V_mdct_init48_i) <= z)%Q
   | 159 => ((3290 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 160 => ((3290 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 161 => ((3290 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 162 => ((3327 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 163 => ((3327 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 164 => ((3327 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 165 => ((3290 # 37) - s V_mdct_init48_k + s V_mdct_init48_z
             - (94 # 37) * max0(35 - s V_mdct_init48_i)
             + (94 # 37) * max0(37 - s V_mdct_init48_i)
             + (32 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 166 => hints
     [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                          - s V_mdct_init48_k)) (F_check_ge (9
                                                                    - s V_mdct_init48_k) (0))]
     ((572 # 1) + s V_mdct_init48_z + (16 # 1) * max0(1 + s V_mdct_init48_i)
      + (1 # 3) * max0(7 - s V_mdct_init48_k)
      + (1 # 3) * max0(8 - s V_mdct_init48_k)
      + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 167 => ((575 # 1) - (1 # 3) * s V_mdct_init48_k + s V_mdct_init48_z
             + (16 # 1) * max0(1 + s V_mdct_init48_i)
             + (1 # 3) * max0(7 - s V_mdct_init48_k)
             + (1 # 3) * max0(8 - s V_mdct_init48_k) <= z)%Q
   | 168 => ((575 # 1) - (1 # 3) * s V_mdct_init48_k + s V_mdct_init48_z
             + (16 # 1) * max0(1 + s V_mdct_init48_i)
             + (1 # 3) * max0(7 - s V_mdct_init48_k)
             + (1 # 3) * max0(8 - s V_mdct_init48_k) <= z)%Q
   | 169 => ((1726 # 3) - (1 # 3) * s V_mdct_init48_k + s V_mdct_init48_z
             + (16 # 1) * max0(1 + s V_mdct_init48_i)
             + (1 # 3) * max0(8 - s V_mdct_init48_k)
             + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 170 => ((1726 # 3) - (1 # 3) * s V_mdct_init48_k + s V_mdct_init48_z
             + (16 # 1) * max0(1 + s V_mdct_init48_i)
             + (1 # 3) * max0(8 - s V_mdct_init48_k)
             + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 171 => ((1726 # 3) - (1 # 3) * s V_mdct_init48_k + s V_mdct_init48_z
             + (16 # 1) * max0(1 + s V_mdct_init48_i)
             + (1 # 3) * max0(8 - s V_mdct_init48_k)
             + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 172 => hints
     [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - s V_mdct_init48_k) (0))) (F_max0_ge_0 (7
                                                                    - s V_mdct_init48_k))]
     ((1723 # 3) - (1 # 3) * s V_mdct_init48_k + s V_mdct_init48_z
      + (16 # 1) * max0(1 + s V_mdct_init48_i)
      + (1 # 3) * max0(8 - s V_mdct_init48_k)
      + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 173 => ((580 # 1) + s V_mdct_init48_z
             + (16 # 1) * max0(1 + s V_mdct_init48_i) <= z)%Q
   | 174 => ((580 # 1) + s V_mdct_init48_z
             + (16 # 1) * max0(1 + s V_mdct_init48_i) <= z)%Q
   | 175 => ((565 # 1) + s V_mdct_init48_z
             + (16 # 1) * max0(1 + s V_mdct_init48_i)
             + max0(15 - s V_mdct_init48_k) <= z)%Q
   | 176 => hints
     [(*-16 0*) F_max0_pre_decrement 1 (1 + s V_mdct_init48_i) (1)]
     ((565 # 1) + s V_mdct_init48_z + (16 # 1) * max0(1 + s V_mdct_init48_i)
      + max0(15 - s V_mdct_init48_k) <= z)%Q
   | 177 => ((581 # 1) + s V_mdct_init48_z + max0(15 - s V_mdct_init48_k)
             + (16 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 178 => ((581 # 1) + s V_mdct_init48_z + max0(15 - s V_mdct_init48_k)
             + (16 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 179 => ((581 # 1) + s V_mdct_init48_z + max0(15 - s V_mdct_init48_k)
             + (16 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 180 => ((581 # 1) + s V_mdct_init48_z + max0(15 - s V_mdct_init48_k)
             + (16 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 181 => ((581 # 1) + s V_mdct_init48_z
             + (16 # 1) * max0(1 + s V_mdct_init48_i)
             + max0(15 - s V_mdct_init48_k) <= z)%Q
   | 182 => ((581 # 1) + s V_mdct_init48_z
             + (16 # 1) * max0(1 + s V_mdct_init48_i)
             + max0(15 - s V_mdct_init48_k) <= z)%Q
   | 183 => ((581 # 1) + s V_mdct_init48_z
             + (16 # 1) * max0(1 + s V_mdct_init48_i)
             + max0(15 - s V_mdct_init48_k) <= z)%Q
   | 184 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (15 - s V_mdct_init48_k) (14
                                                                    - 
                                                                    s V_mdct_init48_k));
      (*-1 0*) F_max0_ge_0 (14 - s V_mdct_init48_k)]
     ((580 # 1) + s V_mdct_init48_z + (16 # 1) * max0(1 + s V_mdct_init48_i)
      + max0(15 - s V_mdct_init48_k) <= z)%Q
   | 185 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (15 - s V_mdct_init48_k) (1)]
     ((581 # 1) + s V_mdct_init48_z + max0(15 - s V_mdct_init48_k)
      + (16 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 186 => ((582 # 1) + s V_mdct_init48_z + max0(14 - s V_mdct_init48_k)
             + (16 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 187 => ((582 # 1) + s V_mdct_init48_z + max0(14 - s V_mdct_init48_k)
             + (16 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 188 => ((582 # 1) + s V_mdct_init48_z + max0(15 - s V_mdct_init48_k)
             + (16 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 189 => ((582 # 1) + s V_mdct_init48_z + max0(15 - s V_mdct_init48_k)
             + (16 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 190 => ((582 # 1) + s V_mdct_init48_z + max0(15 - s V_mdct_init48_k)
             + (16 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 191 => ((581 # 1) + s V_mdct_init48_z + max0(15 - s V_mdct_init48_k)
             + (16 # 1) * max0(s V_mdct_init48_i) <= z)%Q
   | 192 => hints
     [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                          - s V_mdct_init48_k)) (F_check_ge (9
                                                                    - s V_mdct_init48_k) (0))]
     ((819 # 1) + s V_mdct_init48_z + (1 # 3) * max0(7 - s V_mdct_init48_k)
      + (1 # 3) * max0(8 - s V_mdct_init48_k)
      + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 193 => ((822 # 1) - (1 # 3) * s V_mdct_init48_k + s V_mdct_init48_z
             + (1 # 3) * max0(7 - s V_mdct_init48_k)
             + (1 # 3) * max0(8 - s V_mdct_init48_k) <= z)%Q
   | 194 => ((822 # 1) - (1 # 3) * s V_mdct_init48_k + s V_mdct_init48_z
             + (1 # 3) * max0(7 - s V_mdct_init48_k)
             + (1 # 3) * max0(8 - s V_mdct_init48_k) <= z)%Q
   | 195 => ((2467 # 3) - (1 # 3) * s V_mdct_init48_k + s V_mdct_init48_z
             + (1 # 3) * max0(8 - s V_mdct_init48_k)
             + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 196 => ((2467 # 3) - (1 # 3) * s V_mdct_init48_k + s V_mdct_init48_z
             + (1 # 3) * max0(8 - s V_mdct_init48_k)
             + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 197 => ((2467 # 3) - (1 # 3) * s V_mdct_init48_k + s V_mdct_init48_z
             + (1 # 3) * max0(8 - s V_mdct_init48_k)
             + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 198 => hints
     [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - s V_mdct_init48_k) (0))) (F_max0_ge_0 (7
                                                                    - s V_mdct_init48_k))]
     ((2464 # 3) - (1 # 3) * s V_mdct_init48_k + s V_mdct_init48_z
      + (1 # 3) * max0(8 - s V_mdct_init48_k)
      + (1 # 3) * max0(9 - s V_mdct_init48_k) <= z)%Q
   | 199 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_mdct_init48_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_mdct_init48_z) (0))) (F_max0_ge_0 (-
                                                                    s V_mdct_init48_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_mdct_init48_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_mdct_init48_z) (0))) (F_max0_ge_0 (-1
                                                                    + s V_mdct_init48_z))]
     ((827 # 1) + s V_mdct_init48_z <= z)%Q
   | 200 => ((828 # 1) + s V_mdct_init48_z <= z)%Q
   | 201 => ((828 # 1) + s V_mdct_init48_z <= z)%Q
   | 202 => hints
     [(*-3.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (11
                                                                 - s V_mdct_init48_j) (0))) (F_max0_ge_0 (11
                                                                    - s V_mdct_init48_j));
      (*-7 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_mdct_init48_j) (0))) (F_max0_ge_0 (-1
                                                                    + s V_mdct_init48_j))]
     ((792 # 1) + (7 # 2) * s V_mdct_init48_j + s V_mdct_init48_z
      + (7 # 2) * max0(10 - s V_mdct_init48_j) - max0(35 - s V_mdct_init48_i)
      + max0(36 - s V_mdct_init48_i) + (7 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 203 => ((1521 # 2) + s V_mdct_init48_z
             + (7 # 1) * max0(-1 + s V_mdct_init48_j)
             + (7 # 2) * max0(10 - s V_mdct_init48_j)
             + (7 # 2) * max0(11 - s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
             + (7 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 204 => ((1521 # 2) + s V_mdct_init48_z
             + (7 # 1) * max0(-1 + s V_mdct_init48_j)
             + (7 # 2) * max0(10 - s V_mdct_init48_j)
             + (7 # 2) * max0(11 - s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
             + (7 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 205 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (9 - s V_mdct_init48_k)) (F_check_ge (9
                                                                    - s V_mdct_init48_k) (0))]
     ((1507 # 2) + (7 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
      + (7 # 1) * max0(-1 + s V_mdct_init48_j) + max0(9 - s V_mdct_init48_k)
      + (7 # 2) * max0(10 - s V_mdct_init48_j)
      + (7 # 2) * max0(11 - s V_mdct_init48_j) - max0(35 - s V_mdct_init48_i)
      + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 206 => ((1525 # 2) + (7 # 1) * s V_mdct_init48_j - s V_mdct_init48_k
             + s V_mdct_init48_z + (7 # 1) * max0(-1 + s V_mdct_init48_j)
             + (7 # 2) * max0(10 - s V_mdct_init48_j)
             + (7 # 2) * max0(11 - s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 207 => ((1525 # 2) + (7 # 1) * s V_mdct_init48_j - s V_mdct_init48_k
             + s V_mdct_init48_z + (7 # 1) * max0(-1 + s V_mdct_init48_j)
             + (7 # 2) * max0(10 - s V_mdct_init48_j)
             + (7 # 2) * max0(11 - s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 208 => ((1527 # 2) + (7 # 1) * s V_mdct_init48_j - s V_mdct_init48_k
             + s V_mdct_init48_z + (7 # 1) * max0(-1 + s V_mdct_init48_j)
             + (7 # 2) * max0(10 - s V_mdct_init48_j)
             + (7 # 2) * max0(11 - s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 209 => ((1527 # 2) + (7 # 1) * s V_mdct_init48_j - s V_mdct_init48_k
             + s V_mdct_init48_z + (7 # 1) * max0(-1 + s V_mdct_init48_j)
             + (7 # 2) * max0(10 - s V_mdct_init48_j)
             + (7 # 2) * max0(11 - s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 210 => ((1527 # 2) + (7 # 1) * s V_mdct_init48_j - s V_mdct_init48_k
             + s V_mdct_init48_z + (7 # 1) * max0(-1 + s V_mdct_init48_j)
             + (7 # 2) * max0(10 - s V_mdct_init48_j)
             + (7 # 2) * max0(11 - s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 211 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (9
                                                               - s V_mdct_init48_k) (0))) (F_max0_ge_0 (9
                                                                    - s V_mdct_init48_k))]
     ((1525 # 2) + (7 # 1) * s V_mdct_init48_j - s V_mdct_init48_k
      + s V_mdct_init48_z + (7 # 1) * max0(-1 + s V_mdct_init48_j)
      + (7 # 2) * max0(10 - s V_mdct_init48_j)
      + (7 # 2) * max0(11 - s V_mdct_init48_j) - max0(35 - s V_mdct_init48_i)
      + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 212 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (4 - s V_mdct_init48_k)) (F_check_ge (4
                                                                    - s V_mdct_init48_k) (0))]
     ((756 # 1) + (7 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
      + (7 # 1) * max0(-1 + s V_mdct_init48_j)
      + (1 # 2) * max0(3 - s V_mdct_init48_k)
      + (1 # 2) * max0(4 - s V_mdct_init48_k)
      + (7 # 2) * max0(10 - s V_mdct_init48_j)
      + (7 # 2) * max0(11 - s V_mdct_init48_j) - max0(35 - s V_mdct_init48_i)
      + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 213 => ((758 # 1) + (7 # 1) * s V_mdct_init48_j
             - (1 # 2) * s V_mdct_init48_k + s V_mdct_init48_z
             + (7 # 1) * max0(-1 + s V_mdct_init48_j)
             + (1 # 2) * max0(3 - s V_mdct_init48_k)
             + (7 # 2) * max0(10 - s V_mdct_init48_j)
             + (7 # 2) * max0(11 - s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 214 => ((758 # 1) + (7 # 1) * s V_mdct_init48_j
             - (1 # 2) * s V_mdct_init48_k + s V_mdct_init48_z
             + (7 # 1) * max0(-1 + s V_mdct_init48_j)
             + (1 # 2) * max0(3 - s V_mdct_init48_k)
             + (7 # 2) * max0(10 - s V_mdct_init48_j)
             + (7 # 2) * max0(11 - s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 215 => ((1517 # 2) + (7 # 1) * s V_mdct_init48_j
             - (1 # 2) * s V_mdct_init48_k + s V_mdct_init48_z
             + (7 # 1) * max0(-1 + s V_mdct_init48_j)
             + (1 # 2) * max0(4 - s V_mdct_init48_k)
             + (7 # 2) * max0(10 - s V_mdct_init48_j)
             + (7 # 2) * max0(11 - s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 216 => ((1517 # 2) + (7 # 1) * s V_mdct_init48_j
             - (1 # 2) * s V_mdct_init48_k + s V_mdct_init48_z
             + (7 # 1) * max0(-1 + s V_mdct_init48_j)
             + (1 # 2) * max0(4 - s V_mdct_init48_k)
             + (7 # 2) * max0(10 - s V_mdct_init48_j)
             + (7 # 2) * max0(11 - s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 217 => ((1517 # 2) + (7 # 1) * s V_mdct_init48_j
             - (1 # 2) * s V_mdct_init48_k + s V_mdct_init48_z
             + (7 # 1) * max0(-1 + s V_mdct_init48_j)
             + (1 # 2) * max0(4 - s V_mdct_init48_k)
             + (7 # 2) * max0(10 - s V_mdct_init48_j)
             + (7 # 2) * max0(11 - s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 218 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                 - s V_mdct_init48_k) (0))) (F_max0_ge_0 (3
                                                                    - s V_mdct_init48_k))]
     ((1515 # 2) + (7 # 1) * s V_mdct_init48_j - (1 # 2) * s V_mdct_init48_k
      + s V_mdct_init48_z + (7 # 1) * max0(-1 + s V_mdct_init48_j)
      + (1 # 2) * max0(4 - s V_mdct_init48_k)
      + (7 # 2) * max0(10 - s V_mdct_init48_j)
      + (7 # 2) * max0(11 - s V_mdct_init48_j) - max0(35 - s V_mdct_init48_i)
      + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 219 => hints
     [(*0 19*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_mdct_init48_j) (0))) (F_max0_ge_0 (-1
                                                                    + s V_mdct_init48_j))]
     ((847 # 1) + s V_mdct_init48_z - max0(35 - s V_mdct_init48_i)
      + max0(36 - s V_mdct_init48_i) + (19 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 220 => ((866 # 1) - (19 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
             + (19 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 221 => ((866 # 1) - (19 # 1) * s V_mdct_init48_j + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i)
             + (19 # 1) * max0(s V_mdct_init48_j) <= z)%Q
   | 222 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (9 - s V_mdct_init48_k) (1)]
     ((847 # 1) + s V_mdct_init48_z + (19 # 1) * max0(-1 + s V_mdct_init48_j)
      + max0(9 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
      + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 223 => ((848 # 1) + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             + max0(8 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
             + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 224 => ((848 # 1) + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             + max0(8 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
             + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 225 => ((848 # 1) + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             + max0(9 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
             + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 226 => ((848 # 1) + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             + max0(9 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
             + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 227 => ((848 # 1) + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             + max0(9 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
             + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 228 => ((847 # 1) + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             + max0(9 - s V_mdct_init48_k) - max0(35 - s V_mdct_init48_i)
             + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 229 => ((865 # 1) - s V_mdct_init48_k + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 230 => ((865 # 1) - s V_mdct_init48_k + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 231 => ((865 # 1) - s V_mdct_init48_k + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 232 => ((866 # 1) - s V_mdct_init48_k + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 233 => ((866 # 1) - s V_mdct_init48_k + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 234 => ((866 # 1) - s V_mdct_init48_k + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 235 => ((865 # 1) - s V_mdct_init48_k + s V_mdct_init48_z
             + (19 # 1) * max0(-1 + s V_mdct_init48_j)
             - max0(35 - s V_mdct_init48_i) + max0(36 - s V_mdct_init48_i) <= z)%Q
   | 236 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (36 - s V_mdct_init48_i)) (F_check_ge (36
                                                                    - s V_mdct_init48_i) (0))]
     ((1063 # 1) + s V_mdct_init48_z - max0(7 - s V_mdct_init48_k)
      + max0(36 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 237 => ((1099 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
             - max0(7 - s V_mdct_init48_k) - max0(s V_mdct_init48_k) <= z)%Q
   | 238 => ((1099 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
             - max0(7 - s V_mdct_init48_k) - max0(s V_mdct_init48_k) <= z)%Q
   | 239 => ((1100 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
             - max0(7 - s V_mdct_init48_k) - max0(s V_mdct_init48_k) <= z)%Q
   | 240 => ((1100 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
             - max0(7 - s V_mdct_init48_k) - max0(s V_mdct_init48_k) <= z)%Q
   | 241 => ((1100 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
             - max0(7 - s V_mdct_init48_k) - max0(s V_mdct_init48_k) <= z)%Q
   | 242 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (36
                                                               - s V_mdct_init48_i) (0))) (F_max0_ge_0 (36
                                                                    - s V_mdct_init48_i))]
     ((1099 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
      - max0(7 - s V_mdct_init48_k) - max0(s V_mdct_init48_k) <= z)%Q
   | 243 => hints
     [(*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                  + s V_mdct_init48_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_mdct_init48_i));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-19
                                                                  + s V_mdct_init48_i) (0))) (F_max0_ge_0 (-19
                                                                    + s V_mdct_init48_i))]
     ((2251 # 2) + s V_mdct_init48_z
      - (1 # 4) * max0(-19 + s V_mdct_init48_i)
      - (1 # 4) * max0(-18 + s V_mdct_init48_i)
      - (1 # 4) * max0(-1 + s V_mdct_init48_i) - max0(7 - s V_mdct_init48_k)
      + (1 # 4) * max0(17 - s V_mdct_init48_i)
      - (1 # 4) * max0(s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 244 => ((2261 # 2) - (1 # 2) * s V_mdct_init48_i + s V_mdct_init48_z
             - (1 # 4) * max0(-18 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(17 - s V_mdct_init48_i)
             - (1 # 4) * max0(s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 245 => ((2261 # 2) - (1 # 2) * s V_mdct_init48_i + s V_mdct_init48_z
             - (1 # 4) * max0(-18 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(17 - s V_mdct_init48_i)
             - (1 # 4) * max0(s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 246 => ((1131 # 1) - (1 # 2) * s V_mdct_init48_i + s V_mdct_init48_z
             - (1 # 4) * max0(-19 + s V_mdct_init48_i)
             - (1 # 4) * max0(-1 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(18 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 247 => ((1131 # 1) - (1 # 2) * s V_mdct_init48_i + s V_mdct_init48_z
             - (1 # 4) * max0(-19 + s V_mdct_init48_i)
             - (1 # 4) * max0(-1 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(18 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 248 => ((1131 # 1) - (1 # 2) * s V_mdct_init48_i + s V_mdct_init48_z
             - (1 # 4) * max0(-19 + s V_mdct_init48_i)
             - (1 # 4) * max0(-1 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(18 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 249 => hints
     [(*-0.25 0*) F_max0_monotonic (F_check_ge (18 - s V_mdct_init48_i) (17
                                                                    - s V_mdct_init48_i));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mdct_init48_i)) (F_check_ge (s V_mdct_init48_i) (0));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (-18 + s V_mdct_init48_i)) (F_check_ge (-18
                                                                    + s V_mdct_init48_i) (0))]
     ((1130 # 1) - (1 # 2) * s V_mdct_init48_i + s V_mdct_init48_z
      - (1 # 4) * max0(-19 + s V_mdct_init48_i)
      - (1 # 4) * max0(-1 + s V_mdct_init48_i) - max0(7 - s V_mdct_init48_k)
      + (1 # 4) * max0(18 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 250 => hints
     [(*0 0.25*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                 + s V_mdct_init48_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_mdct_init48_i));
      (*0 0.25*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-18
                                                                 + s V_mdct_init48_i) (0))) (F_max0_ge_0 (-18
                                                                    + s V_mdct_init48_i));
      (*0 0.25*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-19
                                                                 + s V_mdct_init48_i) (0))) (F_max0_ge_0 (-19
                                                                    + s V_mdct_init48_i))]
     ((1115 # 1) + (3 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
      - (1 # 4) * max0(-19 + s V_mdct_init48_i)
      - (1 # 4) * max0(-18 + s V_mdct_init48_i)
      - (3 # 4) * max0(-14 + s V_mdct_init48_i)
      - (1 # 4) * max0(-1 + s V_mdct_init48_i) - max0(7 - s V_mdct_init48_k)
      + (1 # 4) * max0(17 - s V_mdct_init48_i)
      - (1 # 4) * max0(s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 251 => ((2249 # 2) + s V_mdct_init48_z
             - (3 # 4) * max0(-14 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(17 - s V_mdct_init48_i)
             - (1 # 4) * max0(s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 252 => ((2249 # 2) + s V_mdct_init48_z
             - (3 # 4) * max0(-14 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(17 - s V_mdct_init48_i)
             - (1 # 4) * max0(s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 253 => ((2249 # 2) + s V_mdct_init48_z
             - (3 # 4) * max0(-15 + s V_mdct_init48_i)
             - (1 # 4) * max0(-1 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(18 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 254 => ((2249 # 2) + s V_mdct_init48_z
             - (3 # 4) * max0(-15 + s V_mdct_init48_i)
             - (1 # 4) * max0(-1 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(18 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 255 => ((2249 # 2) + s V_mdct_init48_z
             - (3 # 4) * max0(-15 + s V_mdct_init48_i)
             - (1 # 4) * max0(-1 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(18 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 256 => hints
     [(*-0.25 0*) F_max0_monotonic (F_check_ge (18 - s V_mdct_init48_i) (17
                                                                    - s V_mdct_init48_i));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mdct_init48_i)) (F_check_ge (s V_mdct_init48_i) (0));
      (*-0.75 0*) F_binom_monotonic 1 (F_max0_ge_arg (-14 + s V_mdct_init48_i)) (F_check_ge (-14
                                                                    + s V_mdct_init48_i) (0));
      (*-0.75 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-15
                                                                  + s V_mdct_init48_i) (0))) (F_max0_ge_0 (-15
                                                                    + s V_mdct_init48_i));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (-18 + s V_mdct_init48_i)) (F_check_ge (-18
                                                                    + s V_mdct_init48_i) (0));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (-19 + s V_mdct_init48_i)) (F_check_ge (-19
                                                                    + s V_mdct_init48_i) (0))]
     ((2247 # 2) + s V_mdct_init48_z
      - (3 # 4) * max0(-15 + s V_mdct_init48_i)
      - (1 # 4) * max0(-1 + s V_mdct_init48_i) - max0(7 - s V_mdct_init48_k)
      + (1 # 4) * max0(18 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 257 => hints
     [(*-0.25 0*) F_max0_monotonic (F_check_ge (18 - s V_mdct_init48_i) (17
                                                                    - s V_mdct_init48_i));
      (*0 0.25*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-18
                                                                 + s V_mdct_init48_i) (0))) (F_max0_ge_0 (-18
                                                                    + s V_mdct_init48_i))]
     ((1120 # 1) + s V_mdct_init48_z
      - (1 # 4) * max0(-18 + s V_mdct_init48_i)
      - (3 # 4) * max0(-14 + s V_mdct_init48_i) - max0(7 - s V_mdct_init48_k)
      + (1 # 4) * max0(18 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 258 => ((2249 # 2) - (1 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
             - (3 # 4) * max0(-14 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(17 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 259 => ((2249 # 2) - (1 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
             - (3 # 4) * max0(-14 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(17 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 260 => ((4499 # 4) - (1 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
             - (3 # 4) * max0(-15 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(18 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 261 => ((4499 # 4) - (1 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
             - (3 # 4) * max0(-15 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(18 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 262 => ((4499 # 4) - (1 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
             - (3 # 4) * max0(-15 + s V_mdct_init48_i)
             - max0(7 - s V_mdct_init48_k)
             + (1 # 4) * max0(18 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 263 => hints
     [(*-0.75 0*) F_binom_monotonic 1 (F_max0_ge_arg (-14 + s V_mdct_init48_i)) (F_check_ge (-14
                                                                    + s V_mdct_init48_i) (0));
      (*-0.75 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-15
                                                                  + s V_mdct_init48_i) (0))) (F_max0_ge_0 (-15
                                                                    + s V_mdct_init48_i));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (-18 + s V_mdct_init48_i)) (F_check_ge (-18
                                                                    + s V_mdct_init48_i) (0))]
     ((4495 # 4) - (1 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
      - (3 # 4) * max0(-15 + s V_mdct_init48_i) - max0(7 - s V_mdct_init48_k)
      + (1 # 4) * max0(18 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 264 => hints
     [(*0 0.25*) F_binom_monotonic 1 (F_max0_ge_arg (18 - s V_mdct_init48_i)) (F_check_ge (18
                                                                    - s V_mdct_init48_i) (0))]
     ((2261 # 2) - (3 # 4) * s V_mdct_init48_i + s V_mdct_init48_z
      + (1 # 4) * max0(18 - s V_mdct_init48_i) - max0(s V_mdct_init48_k) <= z)%Q
   | 265 => ((1135 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
             - max0(s V_mdct_init48_k) <= z)%Q
   | 266 => ((1135 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
             - max0(s V_mdct_init48_k) <= z)%Q
   | 267 => ((1136 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
             - max0(s V_mdct_init48_k) <= z)%Q
   | 268 => ((1136 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
             - max0(s V_mdct_init48_k) <= z)%Q
   | 269 => ((1136 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
             - max0(s V_mdct_init48_k) <= z)%Q
   | 270 => hints
     [(*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (18
                                                                  - s V_mdct_init48_i) (0))) (F_max0_ge_0 (18
                                                                    - s V_mdct_init48_i))]
     ((1135 # 1) - s V_mdct_init48_i + s V_mdct_init48_z
      - max0(s V_mdct_init48_k) <= z)%Q
   | 271 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (36 - s V_mdct_init48_i) (1)]
     ((1135 # 1) + s V_mdct_init48_z + max0(36 - s V_mdct_init48_i)
      - max0(s V_mdct_init48_k) <= z)%Q
   | 272 => ((1136 # 1) + s V_mdct_init48_z + max0(35 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 273 => ((1136 # 1) + s V_mdct_init48_z + max0(35 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 274 => ((1136 # 1) + s V_mdct_init48_z + max0(36 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 275 => ((1136 # 1) + s V_mdct_init48_z + max0(36 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 276 => ((1136 # 1) + s V_mdct_init48_z + max0(36 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 277 => ((1135 # 1) + s V_mdct_init48_z + max0(36 - s V_mdct_init48_i)
             - max0(s V_mdct_init48_k) <= z)%Q
   | 278 => ((1171 # 1) + s V_mdct_init48_z - max0(s V_mdct_init48_k) <= z)%Q
   | 279 => ((1171 # 1) + s V_mdct_init48_z - max0(s V_mdct_init48_k) <= z)%Q
   | 280 => ((1171 # 1) + s V_mdct_init48_z - max0(s V_mdct_init48_k) <= z)%Q
   | 281 => ((1171 # 1) + s V_mdct_init48_z - max0(-1 + s V_mdct_init48_k) <= z)%Q
   | 282 => ((1171 # 1) + s V_mdct_init48_z - max0(-1 + s V_mdct_init48_k) <= z)%Q
   | 283 => ((1171 # 1) + s V_mdct_init48_z - max0(-1 + s V_mdct_init48_k) <= z)%Q
   | 284 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mdct_init48_k)) (F_check_ge (s V_mdct_init48_k) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_mdct_init48_k) (0))) (F_max0_ge_0 (-1
                                                                    + s V_mdct_init48_k))]
     ((1170 # 1) + s V_mdct_init48_z - max0(-1 + s V_mdct_init48_k) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mdct_init48 =>
    [mkPA Q (fun n z s => ai_mdct_init48 n s /\ annot0_mdct_init48 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mdct_init48 (proc_start P_mdct_init48) s1 (proc_end P_mdct_init48) s2 ->
    (s2 V_mdct_init48_z <= (1171 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_mdct_init48.
Qed.
