Require Import pasta.Pasta.

Inductive proc: Type :=
  P_parse_switches.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_parse_switches_z := 1%positive.
Notation V_parse_switches__tmp := 2%positive.
Notation V_parse_switches__tmp1 := 3%positive.
Notation V_parse_switches__tmp2 := 4%positive.
Notation V_parse_switches_argn := 5%positive.
Notation V_parse_switches_force_baseline := 6%positive.
Notation V_parse_switches_is_targa := 7%positive.
Notation V_parse_switches_parse_switches_dot_printed_version := 8%positive.
Notation V_parse_switches_q_scale_factor := 9%positive.
Notation V_parse_switches_simple_progressive := 10%positive.
Notation V_parse_switches_argc := 11%positive.
Notation V_parse_switches_argv := 12%positive.
Notation V_parse_switches_cinfo := 13%positive.
Notation V_parse_switches_for_real := 14%positive.
Notation V_parse_switches_last_file_arg_seen := 15%positive.
Definition Pedges_parse_switches: list (edge proc) :=
  (EA 1 (AAssign V_parse_switches_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_parse_switches__tmp (Some (EVar V_parse_switches_argc))) 3)::
  (EA 3 (AAssign V_parse_switches__tmp1
  (Some (EVar V_parse_switches_last_file_arg_seen))) 4)::(EA 4 (AAssign
  V_parse_switches__tmp2 (Some (EVar V_parse_switches_for_real))) 5)::
  (EA 5 (AAssign V_parse_switches_q_scale_factor (Some (ENum (100)))) 6)::
  (EA 6 (AAssign V_parse_switches_force_baseline (Some (ENum (0)))) 7)::
  (EA 7 (AAssign V_parse_switches_simple_progressive (Some (ENum (0)))) 8)::
  (EA 8 (AAssign V_parse_switches_is_targa (Some (ENum (0)))) 9)::
  (EA 9 (AAssign V_parse_switches_argn (Some (ENum (1)))) 10)::
  (EA 10 ANone 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_parse_switches_argn) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 14)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_parse_switches_argn) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 13)::(EA 13 AWeaken 217)::
  (EA 14 AWeaken 15)::(EA 15 ANone 212)::(EA 15 ANone 16)::
  (EA 16 AWeaken 17)::(EA 17 ANone 210)::(EA 17 ANone 18)::
  (EA 18 AWeaken 19)::(EA 19 ANone 206)::(EA 19 ANone 20)::
  (EA 20 AWeaken 21)::(EA 21 ANone 186)::(EA 21 ANone 22)::
  (EA 22 AWeaken 23)::(EA 23 ANone 178)::(EA 23 ANone 24)::
  (EA 24 AWeaken 25)::(EA 25 ANone 177)::(EA 25 ANone 26)::
  (EA 26 AWeaken 27)::(EA 27 ANone 175)::(EA 27 ANone 28)::
  (EA 28 AWeaken 29)::(EA 29 ANone 175)::(EA 29 ANone 30)::
  (EA 30 AWeaken 31)::(EA 31 ANone 158)::(EA 31 ANone 32)::
  (EA 32 AWeaken 33)::(EA 33 ANone 156)::(EA 33 ANone 34)::
  (EA 34 AWeaken 35)::(EA 35 ANone 156)::(EA 35 ANone 36)::
  (EA 36 AWeaken 37)::(EA 37 ANone 148)::(EA 37 ANone 38)::
  (EA 38 AWeaken 39)::(EA 39 ANone 145)::(EA 39 ANone 40)::
  (EA 40 AWeaken 41)::(EA 41 ANone 133)::(EA 41 ANone 42)::
  (EA 42 AWeaken 43)::(EA 43 ANone 125)::(EA 43 ANone 44)::
  (EA 44 AWeaken 45)::(EA 45 ANone 117)::(EA 45 ANone 46)::
  (EA 46 AWeaken 47)::(EA 47 ANone 93)::(EA 47 ANone 48)::
  (EA 48 AWeaken 49)::(EA 49 ANone 85)::(EA 49 ANone 50)::
  (EA 50 AWeaken 51)::(EA 51 ANone 77)::(EA 51 ANone 52)::
  (EA 52 AWeaken 53)::(EA 53 ANone 60)::(EA 53 ANone 54)::
  (EA 54 AWeaken 55)::(EA 55 ANone 57)::(EA 55 ANone 56)::(EA 56 ANone 59)::
  (EA 57 (AAssign V_parse_switches_is_targa (Some (ENum (1)))) 58)::
  (EA 58 ANone 59)::(EA 59 ANone 76)::(EA 60 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 61)::
  (EA 61 AWeaken 62)::(EA 62 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 64)::(EA 62 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 63)::(EA 63 AWeaken 67)::
  (EA 64 AWeaken 65)::(EA 65 ANone 66)::(EA 66 AWeaken 67)::
  (EA 67 ANone 69)::(EA 67 ANone 68)::(EA 68 AWeaken 71)::(EA 69 ANone 70)::
  (EA 70 AWeaken 71)::(EA 71 ANone 74)::(EA 71 ANone 72)::
  (EA 72 AWeaken 73)::(EA 73 ANone 74)::(EA 73 ANone 75)::(EA 74 ANone 75)::
  (EA 75 ANone 76)::(EA 76 ANone 84)::(EA 77 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 78)::
  (EA 78 AWeaken 79)::(EA 79 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 81)::(EA 79 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 80)::(EA 80 AWeaken 83)::
  (EA 81 AWeaken 82)::(EA 82 ANone 83)::(EA 83 ANone 84)::(EA 84 ANone 92)::
  (EA 85 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 86)::
  (EA 86 AWeaken 87)::(EA 87 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 89)::(EA 87 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 88)::(EA 88 AWeaken 91)::
  (EA 89 AWeaken 90)::(EA 90 ANone 91)::(EA 91 ANone 92)::(EA 92 ANone 116)::
  (EA 93 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 94)::
  (EA 94 AWeaken 95)::(EA 95 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 97)::(EA 95 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 96)::(EA 96 AWeaken 100)::
  (EA 97 AWeaken 98)::(EA 98 ANone 99)::(EA 99 AWeaken 100)::
  (EA 100 ANone 102)::(EA 100 ANone 101)::(EA 101 AWeaken 104)::
  (EA 102 ANone 103)::(EA 103 AWeaken 104)::(EA 104 ANone 108)::
  (EA 104 ANone 105)::(EA 105 AWeaken 106)::(EA 106 ANone 108)::
  (EA 106 ANone 107)::(EA 107 AWeaken 110)::(EA 108 ANone 109)::
  (EA 109 AWeaken 110)::(EA 110 ANone 114)::(EA 110 ANone 111)::
  (EA 111 AWeaken 112)::(EA 112 ANone 114)::(EA 112 ANone 113)::
  (EA 113 ANone 115)::(EA 114 ANone 115)::(EA 115 ANone 116)::
  (EA 116 ANone 124)::(EA 117 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 118)::
  (EA 118 AWeaken 119)::(EA 119 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 121)::(EA 119 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 120)::(EA 120 AWeaken 123)::
  (EA 121 AWeaken 122)::(EA 122 ANone 123)::(EA 123 ANone 124)::
  (EA 124 ANone 132)::(EA 125 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 126)::
  (EA 126 AWeaken 127)::(EA 127 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 129)::(EA 127 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 128)::(EA 128 AWeaken 131)::
  (EA 129 AWeaken 130)::(EA 130 ANone 131)::(EA 131 ANone 132)::
  (EA 132 ANone 144)::(EA 133 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 134)::
  (EA 134 AWeaken 135)::(EA 135 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 137)::(EA 135 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 136)::(EA 136 AWeaken 140)::
  (EA 137 AWeaken 138)::(EA 138 ANone 139)::(EA 139 AWeaken 140)::
  (EA 140 ANone 141)::(EA 140 ANone 142)::(EA 141 ANone 142)::
  (EA 142 (AAssign V_parse_switches_q_scale_factor None) 143)::
  (EA 143 ANone 144)::(EA 144 ANone 147)::(EA 145 (AAssign
  V_parse_switches_simple_progressive (Some (ENum (1)))) 146)::
  (EA 146 ANone 147)::(EA 147 ANone 155)::(EA 148 (AAssign
  V_parse_switches_argn (Some (EAdd (EVar V_parse_switches_argn)
  (ENum (1))))) 149)::(EA 149 AWeaken 150)::(EA 150 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 152)::(EA 150 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 151)::(EA 151 AWeaken 154)::
  (EA 152 AWeaken 153)::(EA 153 ANone 154)::(EA 154 ANone 155)::
  (EA 155 ANone 157)::(EA 156 ANone 157)::(EA 157 ANone 174)::
  (EA 158 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 159)::
  (EA 159 AWeaken 160)::(EA 160 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 162)::(EA 160 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 161)::(EA 161 AWeaken 165)::
  (EA 162 AWeaken 163)::(EA 163 ANone 164)::(EA 164 AWeaken 165)::
  (EA 165 ANone 167)::(EA 165 ANone 166)::(EA 166 AWeaken 169)::
  (EA 167 ANone 168)::(EA 168 AWeaken 169)::(EA 169 ANone 172)::
  (EA 169 ANone 170)::(EA 170 AWeaken 171)::(EA 171 ANone 172)::
  (EA 171 ANone 173)::(EA 172 ANone 173)::(EA 173 ANone 174)::
  (EA 174 ANone 176)::(EA 175 ANone 176)::(EA 176 ANone 185)::
  (EA 177 AWeaken 179)::(EA 178 AWeaken 179)::(EA 179 (AGuard
  (fun s => ((eval (EVar V_parse_switches_parse_switches_dot_printed_version)
  s) <> (eval (ENum (0)) s))%Z)) 183)::(EA 179 (AGuard
  (fun s => ((eval (EVar V_parse_switches_parse_switches_dot_printed_version)
  s) = (eval (ENum (0)) s))%Z)) 180)::(EA 180 AWeaken 181)::(EA 181 (AAssign
  V_parse_switches_parse_switches_dot_printed_version
  (Some (ENum (1)))) 182)::(EA 182 ANone 184)::(EA 183 AWeaken 184)::
  (EA 184 ANone 185)::(EA 185 ANone 205)::(EA 186 (AAssign
  V_parse_switches_argn (Some (EAdd (EVar V_parse_switches_argn)
  (ENum (1))))) 187)::(EA 187 AWeaken 188)::(EA 188 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 190)::(EA 188 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 189)::(EA 189 AWeaken 193)::
  (EA 190 AWeaken 191)::(EA 191 ANone 192)::(EA 192 AWeaken 193)::
  (EA 193 ANone 203)::(EA 193 ANone 194)::(EA 194 AWeaken 195)::
  (EA 195 ANone 201)::(EA 195 ANone 196)::(EA 196 AWeaken 197)::
  (EA 197 ANone 199)::(EA 197 ANone 198)::(EA 198 ANone 200)::
  (EA 199 ANone 200)::(EA 200 ANone 202)::(EA 201 ANone 202)::
  (EA 202 ANone 204)::(EA 203 ANone 204)::(EA 204 ANone 205)::
  (EA 205 ANone 208)::(EA 206 (AAssign V_parse_switches_force_baseline
  (Some (ENum (1)))) 207)::(EA 207 ANone 208)::(EA 208 ANone 209)::
  (EA 209 ANone 256)::(EA 210 ANone 211)::(EA 211 AWeaken 253)::
  (EA 212 AWeaken 213)::(EA 213 (AGuard
  (fun s => ((eval (EVar V_parse_switches_argn) s) <=
  (eval (EVar V_parse_switches__tmp1) s))%Z)) 254)::(EA 213 (AGuard
  (fun s => ((eval (EVar V_parse_switches_argn) s) >
  (eval (EVar V_parse_switches__tmp1) s))%Z)) 214)::(EA 214 AWeaken 215)::
  (EA 215 ANone 216)::(EA 216 AWeaken 217)::(EA 217 (AGuard
  (fun s => ((eval (EVar V_parse_switches__tmp2) s) <> (eval (ENum (0))
  s))%Z)) 219)::(EA 217 (AGuard
  (fun s => ((eval (EVar V_parse_switches__tmp2) s) = (eval (ENum (0))
  s))%Z)) 218)::(EA 218 AWeaken 253)::(EA 219 AWeaken 220)::
  (EA 220 ANone 222)::(EA 220 ANone 221)::(EA 221 AWeaken 227)::
  (EA 222 AWeaken 223)::(EA 223 ANone 225)::(EA 223 ANone 224)::
  (EA 224 ANone 225)::(EA 225 ANone 226)::(EA 226 AWeaken 227)::
  (EA 227 ANone 229)::(EA 227 ANone 228)::(EA 228 AWeaken 234)::
  (EA 229 AWeaken 230)::(EA 230 ANone 232)::(EA 230 ANone 231)::
  (EA 231 ANone 232)::(EA 232 ANone 233)::(EA 233 AWeaken 234)::
  (EA 234 ANone 236)::(EA 234 ANone 235)::(EA 235 AWeaken 241)::
  (EA 236 AWeaken 237)::(EA 237 ANone 239)::(EA 237 ANone 238)::
  (EA 238 ANone 239)::(EA 239 ANone 240)::(EA 240 AWeaken 241)::
  (EA 241 (AGuard (fun s => ((eval (EVar V_parse_switches_simple_progressive)
  s) <> (eval (ENum (0)) s))%Z)) 243)::(EA 241 (AGuard
  (fun s => ((eval (EVar V_parse_switches_simple_progressive) s) =
  (eval (ENum (0)) s))%Z)) 242)::(EA 242 AWeaken 246)::(EA 243 AWeaken 244)::
  (EA 244 ANone 245)::(EA 245 AWeaken 246)::(EA 246 ANone 247)::
  (EA 246 ANone 251)::(EA 247 AWeaken 248)::(EA 248 ANone 250)::
  (EA 248 ANone 249)::(EA 249 ANone 250)::(EA 250 ANone 251)::
  (EA 251 ANone 252)::(EA 252 AWeaken 253)::(EA 254 AWeaken 255)::
  (EA 255 ANone 256)::(EA 256 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 257)::
  (EA 257 ANone 258)::(EA 258 ANone 259)::(EA 259 (AAssign V_parse_switches_z
  (Some (EAdd (ENum (1)) (EVar V_parse_switches_z)))) 260)::
  (EA 260 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_parse_switches => Pedges_parse_switches
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_parse_switches => 253
     end)%positive;
  var_global := var_global
}.

Definition ai_parse_switches (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 3 => (-1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_z <= 0)%Z
   | 4 => (1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 5 => (-1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_z <= 0)%Z
   | 6 => (1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_q_scale_factor + -100 <= 0 /\ -1 * s V_parse_switches_q_scale_factor + 100 <= 0)%Z
   | 7 => (-1 * s V_parse_switches_q_scale_factor + 100 <= 0 /\ 1 * s V_parse_switches_q_scale_factor + -100 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0)%Z
   | 8 => (-1 * s V_parse_switches_force_baseline <= 0 /\ 1 * s V_parse_switches_force_baseline <= 0 /\ 1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_q_scale_factor + -100 <= 0 /\ -1 * s V_parse_switches_q_scale_factor + 100 <= 0 /\ 1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 9 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ 1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_q_scale_factor + 100 <= 0 /\ 1 * s V_parse_switches_q_scale_factor + -100 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ 1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_is_targa <= 0)%Z
   | 10 => (-1 * s V_parse_switches_is_targa <= 0 /\ 1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ 1 * s V_parse_switches_force_baseline <= 0 /\ 1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_q_scale_factor + -100 <= 0 /\ -1 * s V_parse_switches_q_scale_factor + 100 <= 0 /\ 1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ 1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 11 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ 1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_q_scale_factor + 100 <= 0 /\ 1 * s V_parse_switches_q_scale_factor + -100 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ 1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_is_targa <= 0)%Z
   | 12 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 13 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn <= 0)%Z
   | 14 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 15 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 16 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 17 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 18 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 19 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 20 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 21 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 22 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 23 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 24 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 25 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 26 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 27 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 28 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 29 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 30 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 31 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 32 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 33 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 34 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 35 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 36 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 37 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 38 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 39 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 40 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 41 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 42 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 43 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 44 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 45 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 46 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 47 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 48 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 49 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 50 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 51 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 52 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 53 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 54 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 55 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 56 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 57 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 58 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ 1 * s V_parse_switches_is_targa + -1 <= 0 /\ -1 * s V_parse_switches_is_targa + 1 <= 0)%Z
   | 59 => (-1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 60 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 61 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 62 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 63 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 64 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 65 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 66 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 67 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 68 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 69 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 70 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 71 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 72 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 73 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 74 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 75 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 76 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 77 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 78 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 79 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 80 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 81 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 82 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 83 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 84 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 85 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 86 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 87 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 88 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 89 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 90 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 91 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 92 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 93 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 94 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 95 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 96 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 97 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 98 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 99 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 100 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 101 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 102 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 103 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 104 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 105 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 106 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 107 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 108 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 109 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 110 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 111 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 112 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 113 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 114 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 115 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 116 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 117 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 118 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 119 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 120 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 121 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 122 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 123 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 124 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 125 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 126 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 127 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 128 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 129 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 130 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 131 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 132 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 133 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 134 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 135 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 136 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 137 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 138 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 139 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 140 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 141 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 142 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 143 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 144 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 145 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 146 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ 1 * s V_parse_switches_simple_progressive + -1 <= 0 /\ -1 * s V_parse_switches_simple_progressive + 1 <= 0)%Z
   | 147 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 148 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 149 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 150 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 151 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 152 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 153 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 154 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 155 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 156 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 157 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 158 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 159 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 160 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 161 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 162 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 163 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 164 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 165 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 166 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 167 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 168 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 169 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 170 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 171 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 172 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 173 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 174 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 175 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 176 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 177 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 178 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 179 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 180 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches_parse_switches_dot_printed_version <= 0 /\ -1 * s V_parse_switches_parse_switches_dot_printed_version <= 0)%Z
   | 181 => (-1 * s V_parse_switches_parse_switches_dot_printed_version <= 0 /\ 1 * s V_parse_switches_parse_switches_dot_printed_version <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 182 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches_parse_switches_dot_printed_version + -1 <= 0 /\ -1 * s V_parse_switches_parse_switches_dot_printed_version + 1 <= 0)%Z
   | 183 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 184 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 185 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 186 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 187 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 188 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 189 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 190 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 191 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 192 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 193 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 194 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 195 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 196 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 197 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 198 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 199 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 200 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 201 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 202 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 203 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 204 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 205 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 206 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 207 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ 1 * s V_parse_switches_force_baseline + -1 <= 0 /\ -1 * s V_parse_switches_force_baseline + 1 <= 0)%Z
   | 208 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 209 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 210 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 211 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 212 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 213 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 214 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches__tmp1+ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 215 => (1 * s V_parse_switches__tmp1+ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 216 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches__tmp1+ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 217 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 218 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches__tmp2 <= 0 /\ -1 * s V_parse_switches__tmp2 <= 0)%Z
   | 219 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 220 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 221 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 222 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 223 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 224 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 225 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 226 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 227 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 228 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 229 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 230 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 231 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 232 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 233 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 234 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 235 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 236 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 237 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 238 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 239 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 240 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 241 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 242 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 243 => (-1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_simple_progressive + 1 <= 0)%Z
   | 244 => (-1 * s V_parse_switches_simple_progressive + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0)%Z
   | 245 => (-1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_simple_progressive + 1 <= 0)%Z
   | 246 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0)%Z
   | 247 => (-1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 248 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0)%Z
   | 249 => (-1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 250 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0)%Z
   | 251 => (-1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 252 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0)%Z
   | 253 => (-1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 254 => (-1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp1+ 1 * s V_parse_switches_argn <= 0)%Z
   | 255 => (-1 * s V_parse_switches__tmp1+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0)%Z
   | 256 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 257 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 258 => (-1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 259 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 260 => (-1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches_simple_progressive <= 0 /\ -1 * s V_parse_switches_force_baseline <= 0 /\ -1 * s V_parse_switches_is_targa <= 0 /\ -1 * s V_parse_switches_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_parse_switches (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_parse_switches_argc) <= z)%Q
   | 2 => (s V_parse_switches_z + max0(s V_parse_switches_argc) <= z)%Q
   | 3 => (s V_parse_switches_z + max0(s V_parse_switches__tmp) <= z)%Q
   | 4 => (s V_parse_switches_z + max0(s V_parse_switches__tmp) <= z)%Q
   | 5 => (s V_parse_switches_z + max0(s V_parse_switches__tmp) <= z)%Q
   | 6 => (s V_parse_switches_z + max0(s V_parse_switches__tmp) <= z)%Q
   | 7 => (s V_parse_switches_z + max0(s V_parse_switches__tmp) <= z)%Q
   | 8 => (s V_parse_switches_z + max0(s V_parse_switches__tmp) <= z)%Q
   | 9 => (s V_parse_switches_z + max0(s V_parse_switches__tmp) <= z)%Q
   | 10 => (s V_parse_switches_z
            + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 11 => (s V_parse_switches_z
            + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 12 => (s V_parse_switches_z
            + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_parse_switches__tmp
                                             - s V_parse_switches_argn) (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn))]
     (s V_parse_switches_z
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 14 => (s V_parse_switches_z
            + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 15 => (s V_parse_switches_z
            + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 16 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (1 + s V_parse_switches__tmp
                                                  - s V_parse_switches_argn)) (F_check_ge (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn))]
     (s V_parse_switches_z
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 17 => ((2 # 1) + s V_parse_switches_z
            + max0(-1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((2 # 1) + s V_parse_switches_z
      + max0(-1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 19 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 20 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 21 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 22 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 23 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 24 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 25 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 26 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 27 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 28 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 29 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 30 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 31 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 32 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 33 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 34 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 35 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 36 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 37 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 38 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 39 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 40 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 41 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 42 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 43 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 44 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 45 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 46 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 47 => ((2 # 1) * s V_parse_switches__tmp
            - (2 # 1) * s V_parse_switches_argn + s V_parse_switches_z
            - max0(-1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 48 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              + s V_parse_switches__tmp
                                                              - s V_parse_switches_argn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn))]
     ((2 # 1) * s V_parse_switches__tmp - (2 # 1) * s V_parse_switches_argn
      + s V_parse_switches_z
      - max0(-1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 49 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 50 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 51 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 52 => hints
     [(*0 2*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                              - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn))]
     ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 53 => ((1 # 1) - s V_parse_switches__tmp + s V_parse_switches_argn
            + s V_parse_switches_z
            + (2 # 1) * max0(s V_parse_switches__tmp
                             - s V_parse_switches_argn) <= z)%Q
   | 54 => hints
     [(*-2 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((1 # 1) - s V_parse_switches__tmp + s V_parse_switches_argn
      + s V_parse_switches_z
      + (2 # 1) * max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 55 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 56 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 57 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 58 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 59 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 60 => ((1 # 1) - s V_parse_switches__tmp + s V_parse_switches_argn
            + s V_parse_switches_z
            + (2 # 1) * max0(s V_parse_switches__tmp
                             - s V_parse_switches_argn) <= z)%Q
   | 61 => (-s V_parse_switches__tmp + s V_parse_switches_argn
            + s V_parse_switches_z
            + (2 # 1) * max0(1 + s V_parse_switches__tmp
                             - s V_parse_switches_argn) <= z)%Q
   | 62 => (-s V_parse_switches__tmp + s V_parse_switches_argn
            + s V_parse_switches_z
            + (2 # 1) * max0(1 + s V_parse_switches__tmp
                             - s V_parse_switches_argn) <= z)%Q
   | 63 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_parse_switches__tmp
                                             - s V_parse_switches_argn) (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   + s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     (-s V_parse_switches__tmp + s V_parse_switches_argn
      + s V_parse_switches_z
      + (2 # 1) * max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 64 => (-s V_parse_switches__tmp + s V_parse_switches_argn
            + s V_parse_switches_z
            + (2 # 1) * max0(1 + s V_parse_switches__tmp
                             - s V_parse_switches_argn) <= z)%Q
   | 65 => (-s V_parse_switches__tmp + s V_parse_switches_argn
            + s V_parse_switches_z
            + (2 # 1) * max0(1 + s V_parse_switches__tmp
                             - s V_parse_switches_argn) <= z)%Q
   | 66 => hints
     [(*-2 0*) F_max0_pre_decrement 1 (1 + s V_parse_switches__tmp
                                       - s V_parse_switches_argn) (1);
      (*-1 0*) F_max0_ge_0 (s V_parse_switches__tmp - s V_parse_switches_argn);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1 - s V_parse_switches__tmp
                                                 + s V_parse_switches_argn)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               - s V_parse_switches__tmp
                                                               + s V_parse_switches_argn) (0))) (F_max0_ge_0 (1
                                                                    - s V_parse_switches__tmp
                                                                    + s V_parse_switches_argn))]
     (-s V_parse_switches__tmp + s V_parse_switches_argn
      + s V_parse_switches_z
      + (2 # 1) * max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 67 => ((1 # 1) + s V_parse_switches_z
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 68 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((1 # 1) + s V_parse_switches_z
      + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 69 => ((1 # 1) + s V_parse_switches_z
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 70 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((1 # 1) + s V_parse_switches_z
      + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 71 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 72 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 73 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 74 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 75 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 76 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 77 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 78 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 79 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 80 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 81 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 82 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 83 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 84 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 85 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 86 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 87 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 88 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 89 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 90 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 91 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 92 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z <= z)%Q
   | 93 => ((2 # 1) * s V_parse_switches__tmp
            - (2 # 1) * s V_parse_switches_argn + s V_parse_switches_z
            - max0(-1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 94 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                              + s V_parse_switches__tmp
                                                              - s V_parse_switches_argn) (0))) (F_max0_ge_0 (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn))]
     ((2 # 1) + (2 # 1) * s V_parse_switches__tmp
      - (2 # 1) * s V_parse_switches_argn + s V_parse_switches_z
      - max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 95 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z
            + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn)
            - max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 96 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_parse_switches__tmp
                                             - s V_parse_switches_argn) (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn))]
     ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn)
      - max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 97 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z
            + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn)
            - max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 98 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
            + s V_parse_switches_z
            + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn)
            - max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 99 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1 + s V_parse_switches__tmp
                                       - s V_parse_switches_argn) (1);
      (*-1 0*) F_max0_ge_0 (s V_parse_switches__tmp - s V_parse_switches_argn);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1 - s V_parse_switches__tmp
                                                 + s V_parse_switches_argn)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               - s V_parse_switches__tmp
                                                               + s V_parse_switches_argn) (0))) (F_max0_ge_0 (1
                                                                    - s V_parse_switches__tmp
                                                                    + s V_parse_switches_argn))]
     ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn)
      - max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 100 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 101 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 102 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 103 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 104 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 105 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 106 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 107 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 108 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 109 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 110 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 111 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 112 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 113 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 114 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 115 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 116 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 117 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 118 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 119 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 120 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 121 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 122 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 123 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 124 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 125 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 126 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 127 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 128 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 129 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 130 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 131 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 132 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 133 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 134 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 135 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 136 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 137 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 138 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 139 => hints
     [(*-1 0*) F_max0_ge_0 (s V_parse_switches__tmp - s V_parse_switches_argn);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1 - s V_parse_switches__tmp
                                                 + s V_parse_switches_argn)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               - s V_parse_switches__tmp
                                                               + s V_parse_switches_argn) (0))) (F_max0_ge_0 (1
                                                                    - s V_parse_switches__tmp
                                                                    + s V_parse_switches_argn))]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 140 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 141 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 142 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 143 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 144 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 145 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 146 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 147 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 148 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 149 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 150 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 151 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 152 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 153 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 154 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 155 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 156 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 157 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 158 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 159 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 160 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 161 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 162 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 163 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 164 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 165 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 166 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 167 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 168 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 169 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 170 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 171 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 172 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 173 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 174 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 175 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 176 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 177 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 178 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 179 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 180 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 181 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 182 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 183 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 184 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 185 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 186 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 187 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 188 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 189 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 190 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 191 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 192 => hints
     [(*-1 0*) F_one]
     ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | 193 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 194 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 195 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 196 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 197 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 198 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 199 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 200 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 201 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 202 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 203 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 204 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 205 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 206 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 207 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 208 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 209 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 210 => ((2 # 1) + s V_parse_switches_z
             + max0(-1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 211 => hints
     [(*-2 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_parse_switches__tmp
                                                 - s V_parse_switches_argn)) (F_check_ge (0) (0))]
     ((2 # 1) + s V_parse_switches_z
      + max0(-1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 212 => (s V_parse_switches_z
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 213 => (s V_parse_switches_z
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 214 => (s V_parse_switches_z
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 215 => (s V_parse_switches_z
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 216 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (1 + s V_parse_switches__tmp
                                       - s V_parse_switches_argn) (1)]
     (s V_parse_switches_z
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 217 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 218 => hints
     [(*-1 0*) F_max0_ge_0 (s V_parse_switches__tmp - s V_parse_switches_argn)]
     (s V_parse_switches_z
      + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 219 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 220 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 221 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 222 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 223 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 224 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 225 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 226 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 227 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 228 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 229 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 230 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 231 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 232 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 233 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 234 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 235 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 236 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 237 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 238 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 239 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 240 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 241 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 242 => hints
     [(*-1 0*) F_max0_ge_0 (s V_parse_switches__tmp - s V_parse_switches_argn)]
     (s V_parse_switches_z
      + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 243 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 244 => (s V_parse_switches_z
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 245 => hints
     [(*-1 0*) F_max0_ge_0 (s V_parse_switches__tmp - s V_parse_switches_argn)]
     (s V_parse_switches_z
      + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 246 => (s V_parse_switches_z <= z)%Q
   | 247 => (s V_parse_switches_z <= z)%Q
   | 248 => (s V_parse_switches_z <= z)%Q
   | 249 => (s V_parse_switches_z <= z)%Q
   | 250 => (s V_parse_switches_z <= z)%Q
   | 251 => (s V_parse_switches_z <= z)%Q
   | 252 => (s V_parse_switches_z <= z)%Q
   | 253 => (s V_parse_switches_z <= z)%Q
   | 254 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1 + s V_parse_switches__tmp
                                       - s V_parse_switches_argn) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     (s V_parse_switches_z
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 255 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 256 => ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 257 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 258 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 259 => ((2 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
             + s V_parse_switches_z <= z)%Q
   | 260 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               + s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_parse_switches_z)) (F_check_ge (-1
                                                                    + s V_parse_switches_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              + s V_parse_switches_z) (0))) (F_max0_ge_0 (-1
                                                                    + s V_parse_switches_z))]
     ((1 # 1) + s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_parse_switches =>
    [mkPA Q (fun n z s => ai_parse_switches n s /\ annot0_parse_switches n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_parse_switches (proc_start P_parse_switches) s1 (proc_end P_parse_switches) s2 ->
    (s2 V_parse_switches_z <= max0(s1 V_parse_switches_argc))%Q.
Proof.
  prove_bound ipa admissible_ipa P_parse_switches.
Qed.
