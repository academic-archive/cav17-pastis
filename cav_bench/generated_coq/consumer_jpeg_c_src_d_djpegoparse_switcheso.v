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
Notation V_parse_switches_parse_switches_dot_printed_version := 6%positive.
Notation V_parse_switches_requested_fmt := 7%positive.
Notation V_parse_switches_argc := 8%positive.
Notation V_parse_switches_argv := 9%positive.
Notation V_parse_switches_cinfo := 10%positive.
Notation V_parse_switches_for_real := 11%positive.
Notation V_parse_switches_last_file_arg_seen := 12%positive.
Definition Pedges_parse_switches: list (edge proc) :=
  (EA 1 (AAssign V_parse_switches_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_parse_switches__tmp (Some (EVar V_parse_switches_argc))) 3)::
  (EA 3 (AAssign V_parse_switches__tmp1
  (Some (EVar V_parse_switches_last_file_arg_seen))) 4)::(EA 4 (AAssign
  V_parse_switches__tmp2 (Some (EVar V_parse_switches_for_real))) 5)::
  (EA 5 (AAssign V_parse_switches_requested_fmt (Some (ENum (3)))) 6)::
  (EA 6 (AAssign V_parse_switches_argn (Some (ENum (1)))) 7)::
  (EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_parse_switches_argn) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 11)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_parse_switches_argn) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 10)::(EA 10 AWeaken 208)::
  (EA 11 AWeaken 12)::(EA 12 ANone 203)::(EA 12 ANone 13)::
  (EA 13 AWeaken 14)::(EA 14 ANone 200)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::(EA 16 ANone 189)::(EA 16 ANone 17)::
  (EA 17 AWeaken 18)::(EA 18 ANone 189)::(EA 18 ANone 19)::
  (EA 19 AWeaken 20)::(EA 20 ANone 189)::(EA 20 ANone 21)::
  (EA 21 AWeaken 22)::(EA 22 ANone 189)::(EA 22 ANone 23)::
  (EA 23 AWeaken 24)::(EA 24 ANone 169)::(EA 24 ANone 25)::
  (EA 25 AWeaken 26)::(EA 26 ANone 149)::(EA 26 ANone 27)::
  (EA 27 AWeaken 28)::(EA 28 ANone 141)::(EA 28 ANone 29)::
  (EA 29 AWeaken 30)::(EA 30 ANone 140)::(EA 30 ANone 31)::
  (EA 31 AWeaken 32)::(EA 32 ANone 135)::(EA 32 ANone 33)::
  (EA 33 AWeaken 34)::(EA 34 ANone 132)::(EA 34 ANone 35)::
  (EA 35 AWeaken 36)::(EA 36 ANone 130)::(EA 36 ANone 37)::
  (EA 37 AWeaken 38)::(EA 38 ANone 130)::(EA 38 ANone 39)::
  (EA 39 AWeaken 40)::(EA 40 ANone 114)::(EA 40 ANone 41)::
  (EA 41 AWeaken 42)::(EA 42 ANone 97)::(EA 42 ANone 43)::
  (EA 43 AWeaken 44)::(EA 44 ANone 95)::(EA 44 ANone 45)::
  (EA 45 AWeaken 46)::(EA 46 ANone 93)::(EA 46 ANone 47)::
  (EA 47 AWeaken 48)::(EA 48 ANone 90)::(EA 48 ANone 49)::
  (EA 49 AWeaken 50)::(EA 50 ANone 82)::(EA 50 ANone 51)::
  (EA 51 AWeaken 52)::(EA 52 ANone 79)::(EA 52 ANone 53)::
  (EA 53 AWeaken 54)::(EA 54 ANone 79)::(EA 54 ANone 55)::
  (EA 55 AWeaken 56)::(EA 56 ANone 76)::(EA 56 ANone 57)::
  (EA 57 AWeaken 58)::(EA 58 ANone 65)::(EA 58 ANone 59)::
  (EA 59 AWeaken 60)::(EA 60 ANone 62)::(EA 60 ANone 61)::(EA 61 ANone 64)::
  (EA 62 (AAssign V_parse_switches_requested_fmt (Some (ENum (5)))) 63)::
  (EA 63 ANone 64)::(EA 64 ANone 75)::(EA 65 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 66)::
  (EA 66 AWeaken 67)::(EA 67 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 69)::(EA 67 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 68)::(EA 68 AWeaken 72)::
  (EA 69 AWeaken 70)::(EA 70 ANone 71)::(EA 71 AWeaken 72)::
  (EA 72 ANone 73)::(EA 72 ANone 74)::(EA 73 ANone 74)::(EA 74 ANone 75)::
  (EA 75 ANone 78)::(EA 76 (AAssign V_parse_switches_requested_fmt
  (Some (ENum (4)))) 77)::(EA 77 ANone 78)::(EA 78 ANone 81)::(EA 79 (AAssign
  V_parse_switches_requested_fmt (Some (ENum (3)))) 80)::(EA 80 ANone 81)::
  (EA 81 ANone 89)::(EA 82 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 83)::
  (EA 83 AWeaken 84)::(EA 84 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 86)::(EA 84 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 85)::(EA 85 AWeaken 88)::
  (EA 86 AWeaken 87)::(EA 87 ANone 88)::(EA 88 ANone 89)::(EA 89 ANone 92)::
  (EA 90 (AAssign V_parse_switches_requested_fmt (Some (ENum (2)))) 91)::
  (EA 91 ANone 92)::(EA 92 ANone 94)::(EA 93 ANone 94)::(EA 94 ANone 96)::
  (EA 95 ANone 96)::(EA 96 ANone 113)::(EA 97 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 98)::
  (EA 98 AWeaken 99)::(EA 99 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 101)::(EA 99 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 100)::(EA 100 AWeaken 104)::
  (EA 101 AWeaken 102)::(EA 102 ANone 103)::(EA 103 AWeaken 104)::
  (EA 104 ANone 106)::(EA 104 ANone 105)::(EA 105 AWeaken 108)::
  (EA 106 ANone 107)::(EA 107 AWeaken 108)::(EA 108 ANone 111)::
  (EA 108 ANone 109)::(EA 109 AWeaken 110)::(EA 110 ANone 111)::
  (EA 110 ANone 112)::(EA 111 ANone 112)::(EA 112 ANone 113)::
  (EA 113 ANone 127)::(EA 114 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 115)::
  (EA 115 AWeaken 116)::(EA 116 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 118)::(EA 116 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 117)::(EA 117 AWeaken 121)::
  (EA 118 AWeaken 119)::(EA 119 ANone 120)::(EA 120 AWeaken 121)::
  (EA 121 (AGuard (fun s => ((eval (EVar V_parse_switches__tmp2) s) <>
  (eval (ENum (0)) s))%Z)) 123)::(EA 121 (AGuard
  (fun s => ((eval (EVar V_parse_switches__tmp2) s) = (eval (ENum (0))
  s))%Z)) 122)::(EA 122 AWeaken 126)::(EA 123 AWeaken 124)::
  (EA 124 ANone 128)::(EA 124 ANone 125)::(EA 125 ANone 126)::
  (EA 126 ANone 127)::(EA 127 ANone 131)::(EA 128 ANone 129)::
  (EA 129 AWeaken 208)::(EA 130 ANone 131)::(EA 131 ANone 134)::
  (EA 132 (AAssign V_parse_switches_requested_fmt (Some (ENum (1)))) 133)::
  (EA 133 ANone 134)::(EA 134 ANone 139)::(EA 135 AWeaken 136)::
  (EA 136 ANone 138)::(EA 136 ANone 137)::(EA 137 ANone 138)::
  (EA 138 ANone 139)::(EA 139 ANone 148)::(EA 140 AWeaken 142)::
  (EA 141 AWeaken 142)::(EA 142 (AGuard
  (fun s => ((eval (EVar V_parse_switches_parse_switches_dot_printed_version)
  s) <> (eval (ENum (0)) s))%Z)) 146)::(EA 142 (AGuard
  (fun s => ((eval (EVar V_parse_switches_parse_switches_dot_printed_version)
  s) = (eval (ENum (0)) s))%Z)) 143)::(EA 143 AWeaken 144)::(EA 144 (AAssign
  V_parse_switches_parse_switches_dot_printed_version
  (Some (ENum (1)))) 145)::(EA 145 ANone 147)::(EA 146 AWeaken 147)::
  (EA 147 ANone 148)::(EA 148 ANone 168)::(EA 149 (AAssign
  V_parse_switches_argn (Some (EAdd (EVar V_parse_switches_argn)
  (ENum (1))))) 150)::(EA 150 AWeaken 151)::(EA 151 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 153)::(EA 151 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 152)::(EA 152 AWeaken 156)::
  (EA 153 AWeaken 154)::(EA 154 ANone 155)::(EA 155 AWeaken 156)::
  (EA 156 ANone 166)::(EA 156 ANone 157)::(EA 157 AWeaken 158)::
  (EA 158 ANone 164)::(EA 158 ANone 159)::(EA 159 AWeaken 160)::
  (EA 160 ANone 162)::(EA 160 ANone 161)::(EA 161 ANone 163)::
  (EA 162 ANone 163)::(EA 163 ANone 165)::(EA 164 ANone 165)::
  (EA 165 ANone 167)::(EA 166 ANone 167)::(EA 167 ANone 168)::
  (EA 168 ANone 188)::(EA 169 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 170)::
  (EA 170 AWeaken 171)::(EA 171 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 173)::(EA 171 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 172)::(EA 172 AWeaken 176)::
  (EA 173 AWeaken 174)::(EA 174 ANone 175)::(EA 175 AWeaken 176)::
  (EA 176 ANone 186)::(EA 176 ANone 177)::(EA 177 AWeaken 178)::
  (EA 178 ANone 184)::(EA 178 ANone 179)::(EA 179 AWeaken 180)::
  (EA 180 ANone 182)::(EA 180 ANone 181)::(EA 181 ANone 183)::
  (EA 182 ANone 183)::(EA 183 ANone 185)::(EA 184 ANone 185)::
  (EA 185 ANone 187)::(EA 186 ANone 187)::(EA 187 ANone 188)::
  (EA 188 ANone 199)::(EA 189 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 190)::
  (EA 190 AWeaken 191)::(EA 191 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) >=
  (eval (EVar V_parse_switches__tmp) s))%Z)) 193)::(EA 191 (AGuard
  (fun s => ((eval (EAdd (EVar V_parse_switches_argn) (ENum (1))) s) <
  (eval (EVar V_parse_switches__tmp) s))%Z)) 192)::(EA 192 AWeaken 196)::
  (EA 193 AWeaken 194)::(EA 194 ANone 195)::(EA 195 AWeaken 196)::
  (EA 196 ANone 197)::(EA 196 ANone 198)::(EA 197 ANone 198)::
  (EA 198 ANone 199)::(EA 199 ANone 202)::(EA 200 (AAssign
  V_parse_switches_requested_fmt (Some (ENum (0)))) 201)::
  (EA 201 ANone 202)::(EA 202 ANone 211)::(EA 203 AWeaken 204)::
  (EA 204 (AGuard (fun s => ((eval (EVar V_parse_switches_argn) s) <=
  (eval (EVar V_parse_switches__tmp1) s))%Z)) 209)::(EA 204 (AGuard
  (fun s => ((eval (EVar V_parse_switches_argn) s) >
  (eval (EVar V_parse_switches__tmp1) s))%Z)) 205)::(EA 205 AWeaken 206)::
  (EA 206 ANone 207)::(EA 207 AWeaken 208)::(EA 209 AWeaken 210)::
  (EA 210 ANone 211)::(EA 211 (AAssign V_parse_switches_argn
  (Some (EAdd (EVar V_parse_switches_argn) (ENum (1))))) 212)::
  (EA 212 ANone 213)::(EA 213 ANone 214)::(EA 214 (AAssign V_parse_switches_z
  (Some (EAdd (ENum (1)) (EVar V_parse_switches_z)))) 215)::
  (EA 215 AWeaken 9)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_parse_switches => Pedges_parse_switches
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_parse_switches => 208
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
   | 6 => (1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_requested_fmt + -3 <= 0 /\ -1 * s V_parse_switches_requested_fmt + 3 <= 0)%Z
   | 7 => (-1 * s V_parse_switches_requested_fmt + 3 <= 0 /\ 1 * s V_parse_switches_requested_fmt + -3 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 8 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches_argn + -1 <= 0 /\ 1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches_requested_fmt + -3 <= 0 /\ -1 * s V_parse_switches_requested_fmt + 3 <= 0)%Z
   | 9 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 10 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn <= 0)%Z
   | 11 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 12 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 13 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 14 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 15 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 16 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 17 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 18 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 19 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 20 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 21 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 22 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 23 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 24 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 25 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 26 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 27 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 28 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 29 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 30 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 31 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 32 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 33 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 34 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 35 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 36 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 37 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 38 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 39 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 40 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 41 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 42 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 43 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 44 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 45 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 46 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 47 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 48 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 49 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 50 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 51 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 52 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 53 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 54 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 55 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 56 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 57 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 58 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 59 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 60 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 61 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 62 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 63 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches_requested_fmt + -5 <= 0 /\ -1 * s V_parse_switches_requested_fmt + 5 <= 0)%Z
   | 64 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 65 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 66 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 67 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 68 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 69 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 70 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 71 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 72 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 73 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 74 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 75 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 76 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 77 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches_requested_fmt + -4 <= 0 /\ -1 * s V_parse_switches_requested_fmt + 4 <= 0)%Z
   | 78 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 79 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 80 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches_requested_fmt + -3 <= 0 /\ -1 * s V_parse_switches_requested_fmt + 3 <= 0)%Z
   | 81 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 82 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 83 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 84 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 85 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 86 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 87 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 88 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 89 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 90 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 91 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches_requested_fmt + -2 <= 0 /\ -1 * s V_parse_switches_requested_fmt + 2 <= 0)%Z
   | 92 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 93 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 94 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 95 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 96 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 97 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 98 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 99 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 100 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 101 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 102 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 103 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 104 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 105 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 106 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 107 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 108 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 109 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 110 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 111 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 112 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 113 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 114 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 115 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 116 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 117 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 118 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 119 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 120 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 121 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 122 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp2 <= 0 /\ -1 * s V_parse_switches__tmp2 <= 0)%Z
   | 123 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 124 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 125 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 126 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 127 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 128 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 129 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 130 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 131 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 132 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 133 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches_requested_fmt + -1 <= 0 /\ -1 * s V_parse_switches_requested_fmt + 1 <= 0)%Z
   | 134 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 135 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 136 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 137 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 138 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 139 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 140 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 141 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 142 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 143 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches_parse_switches_dot_printed_version <= 0 /\ -1 * s V_parse_switches_parse_switches_dot_printed_version <= 0)%Z
   | 144 => (-1 * s V_parse_switches_parse_switches_dot_printed_version <= 0 /\ 1 * s V_parse_switches_parse_switches_dot_printed_version <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 145 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches_parse_switches_dot_printed_version + -1 <= 0 /\ -1 * s V_parse_switches_parse_switches_dot_printed_version + 1 <= 0)%Z
   | 146 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 147 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 148 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 149 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 150 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 151 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 152 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 153 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 154 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 155 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 156 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 157 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 158 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 159 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 160 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 161 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 162 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 163 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 164 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 165 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 166 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 167 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 168 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 169 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 170 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 171 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 172 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 173 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 174 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 175 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 176 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 177 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 178 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 179 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 180 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 181 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 182 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 183 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 184 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 185 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 186 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 187 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 188 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 189 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 190 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 191 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 192 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 193 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 194 => (1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 195 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ 1 * s V_parse_switches__tmp+ -1 * s V_parse_switches_argn + -1 <= 0)%Z
   | 196 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 197 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 198 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 199 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0)%Z
   | 200 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 201 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches_requested_fmt <= 0 /\ -1 * s V_parse_switches_requested_fmt <= 0)%Z
   | 202 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 203 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 204 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 205 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches__tmp1+ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 206 => (1 * s V_parse_switches__tmp1+ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 207 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ 1 * s V_parse_switches__tmp1+ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 208 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 209 => (-1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches__tmp1+ 1 * s V_parse_switches_argn <= 0)%Z
   | 210 => (-1 * s V_parse_switches__tmp1+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0)%Z
   | 211 => (-1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn <= 0 /\ -1 * s V_parse_switches_argn + 1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 212 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 213 => (-1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches_z <= 0)%Z
   | 214 => (-1 * s V_parse_switches_z <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches_argn + 2 <= 0)%Z
   | 215 => (-1 * s V_parse_switches_argn + 2 <= 0 /\ -1 * s V_parse_switches__tmp+ 1 * s V_parse_switches_argn + -1 <= 0 /\ -1 * s V_parse_switches_z + 1 <= 0)%Z
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
   | 7 => (-(1 # 2) + (1 # 2) * s V_parse_switches_argn
           + s V_parse_switches_z
           - (1 # 2) * max0(-1 + s V_parse_switches_argn)
           + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 8 => (-(1 # 2) + (1 # 2) * s V_parse_switches_argn
           + s V_parse_switches_z
           - (1 # 2) * max0(-1 + s V_parse_switches_argn)
           + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 9 => (-(1 # 2) + (1 # 2) * s V_parse_switches_argn
           + s V_parse_switches_z
           - (1 # 2) * max0(-1 + s V_parse_switches_argn)
           + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_parse_switches__tmp
                                             - s V_parse_switches_argn) (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-1 0*) F_max0_ge_0 (s V_parse_switches__tmp - s V_parse_switches_argn);
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_parse_switches_argn))]
     (-(1 # 2) + (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-1 + s V_parse_switches_argn)
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 11 => hints
     [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (s V_parse_switches__tmp
                                                    - s V_parse_switches_argn)) (F_check_ge (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   + s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     (-(1 # 2) + (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-1 + s V_parse_switches_argn)
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 12 => ((1 # 2) + (3 # 2) * s V_parse_switches__tmp
            - s V_parse_switches_argn + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            - (1 # 2) * max0(s V_parse_switches__tmp
                             - s V_parse_switches_argn) <= z)%Q
   | 13 => hints
     [(*-1.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                                 - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn))]
     ((1 # 2) + (3 # 2) * s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z - (1 # 2) * max0(-1 + s V_parse_switches_argn)
      - (1 # 2) * max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 14 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 15 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 16 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 17 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 18 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 19 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 20 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 21 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 22 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 23 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 24 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 25 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 26 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 27 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 28 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 29 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 30 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 31 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 32 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 33 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 34 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 35 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 36 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 37 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 38 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 39 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 40 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 41 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 42 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 43 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 44 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 45 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 46 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 47 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 48 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 49 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 50 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 51 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 52 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 53 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 54 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 55 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 56 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 57 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 58 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 59 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 60 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 61 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 62 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 63 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 64 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 65 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 66 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   + s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 67 => ((1 # 1) + s V_parse_switches__tmp
            - (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
            - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 68 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 1) + s V_parse_switches__tmp - (1 # 2) * s V_parse_switches_argn
      + s V_parse_switches_z - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 69 => ((1 # 1) + s V_parse_switches__tmp
            - (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
            - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 70 => ((1 # 1) + s V_parse_switches__tmp
            - (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
            - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 71 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 1) + s V_parse_switches__tmp - (1 # 2) * s V_parse_switches_argn
      + s V_parse_switches_z - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 72 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 73 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 74 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 75 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 76 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 77 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 78 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 79 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 80 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 81 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 82 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 83 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   + s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 84 => ((1 # 1) + s V_parse_switches__tmp
            - (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
            - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 85 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 1) + s V_parse_switches__tmp - (1 # 2) * s V_parse_switches_argn
      + s V_parse_switches_z - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 86 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 1) + s V_parse_switches__tmp - (1 # 2) * s V_parse_switches_argn
      + s V_parse_switches_z - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 87 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 88 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 89 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 90 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 91 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 92 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 93 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 94 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 95 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 96 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 97 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
            + s V_parse_switches_z
            - (1 # 2) * max0(-1 + s V_parse_switches_argn)
            + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 98 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
            - (1 # 2) * max0(-2 + s V_parse_switches_argn)
            + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 99 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
            - (1 # 2) * max0(-2 + s V_parse_switches_argn)
            + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 100 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_parse_switches__tmp
                                             - s V_parse_switches_argn) (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn))]
     ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 101 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 102 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 103 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   + s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 104 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 105 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 106 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 107 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 108 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 109 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 110 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 111 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 112 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 113 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 114 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 115 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 116 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 117 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1 + s V_parse_switches__tmp
                                       - s V_parse_switches_argn) (1)]
     ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 118 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 119 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 120 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   + s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 121 => ((1 # 1) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 122 => hints
     [(*-1 0*) F_one;
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 1) + (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 123 => hints
     [(*-1 0*) F_one;
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 1) + (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 124 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 125 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 126 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 127 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 128 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 129 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (s V_parse_switches__tmp - s V_parse_switches_argn);
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_parse_switches_argn))]
     ((1 # 2) + (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-1 + s V_parse_switches_argn)
      + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 130 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 131 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 132 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 133 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 134 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 135 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 136 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 137 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 138 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 139 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 140 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 141 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 142 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 143 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 144 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 145 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 146 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 147 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 148 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 149 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 150 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   + s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 151 => ((1 # 1) + s V_parse_switches__tmp
             - (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 152 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 1) + s V_parse_switches__tmp - (1 # 2) * s V_parse_switches_argn
      + s V_parse_switches_z - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 153 => ((1 # 1) + s V_parse_switches__tmp
             - (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 154 => ((1 # 1) + s V_parse_switches__tmp
             - (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 155 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 1) + s V_parse_switches__tmp - (1 # 2) * s V_parse_switches_argn
      + s V_parse_switches_z - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 156 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 157 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 158 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 159 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 160 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 161 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 162 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 163 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 164 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 165 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 166 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 167 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 168 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 169 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 170 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   + s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 171 => ((1 # 1) + s V_parse_switches__tmp
             - (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 172 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 1) + s V_parse_switches__tmp - (1 # 2) * s V_parse_switches_argn
      + s V_parse_switches_z - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 173 => ((1 # 1) + s V_parse_switches__tmp
             - (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 174 => ((1 # 1) + s V_parse_switches__tmp
             - (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 175 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 1) + s V_parse_switches__tmp - (1 # 2) * s V_parse_switches_argn
      + s V_parse_switches_z - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 176 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 177 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 178 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 179 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 180 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 181 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 182 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 183 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 184 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 185 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 186 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 187 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 188 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 189 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 190 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   + s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 191 => ((1 # 1) + s V_parse_switches__tmp
             - (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 192 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 1) + s V_parse_switches__tmp - (1 # 2) * s V_parse_switches_argn
      + s V_parse_switches_z - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 193 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0))]
     ((1 # 1) + s V_parse_switches__tmp - (1 # 2) * s V_parse_switches_argn
      + s V_parse_switches_z - (1 # 2) * max0(-2 + s V_parse_switches_argn) <= z)%Q
   | 194 => ((1 # 2) + s V_parse_switches__tmp + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             - (1 # 2) * max0(-1 + s V_parse_switches_argn) <= z)%Q
   | 195 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     ((1 # 2) + s V_parse_switches__tmp + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      - (1 # 2) * max0(-1 + s V_parse_switches_argn) <= z)%Q
   | 196 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 197 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 198 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 199 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 200 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 201 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 202 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 203 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_parse_switches__tmp
                                                               - s V_parse_switches_argn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn))]
     ((1 # 2) + (3 # 2) * s V_parse_switches__tmp - s V_parse_switches_argn
      + s V_parse_switches_z - (1 # 2) * max0(-1 + s V_parse_switches_argn)
      - (1 # 2) * max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 204 => ((3 # 2) + (1 # 2) * s V_parse_switches__tmp
             + s V_parse_switches_z
             + max0(-1 + s V_parse_switches__tmp - s V_parse_switches_argn)
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             - (1 # 2) * max0(s V_parse_switches__tmp
                              - s V_parse_switches_argn) <= z)%Q
   | 205 => ((3 # 2) + (1 # 2) * s V_parse_switches__tmp
             + s V_parse_switches_z
             + max0(-1 + s V_parse_switches__tmp - s V_parse_switches_argn)
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             - (1 # 2) * max0(s V_parse_switches__tmp
                              - s V_parse_switches_argn) <= z)%Q
   | 206 => ((3 # 2) + (1 # 2) * s V_parse_switches__tmp
             + s V_parse_switches_z
             + max0(-1 + s V_parse_switches__tmp - s V_parse_switches_argn)
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             - (1 # 2) * max0(s V_parse_switches__tmp
                              - s V_parse_switches_argn) <= z)%Q
   | 207 => hints
     [(*-2 0*) F_one;
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_switches__tmp
                                                                 - s V_parse_switches_argn) (0))) (F_max0_ge_0 (s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-1
                                                                    + s V_parse_switches_argn));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_parse_switches__tmp
                                                 - s V_parse_switches_argn)) (F_check_ge (0) (0))]
     ((3 # 2) + (1 # 2) * s V_parse_switches__tmp + s V_parse_switches_z
      + max0(-1 + s V_parse_switches__tmp - s V_parse_switches_argn)
      - (1 # 2) * max0(-1 + s V_parse_switches_argn)
      - (1 # 2) * max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 208 => (s V_parse_switches_z <= z)%Q
   | 209 => hints
     [(*-1.5 0*) F_max0_pre_decrement 1 (1 + s V_parse_switches__tmp
                                         - s V_parse_switches_argn) (1);
      (*-1.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                 + s V_parse_switches__tmp
                                                                 - s V_parse_switches_argn) (0))) (F_max0_ge_0 (1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_parse_switches__tmp
                                                   - s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches__tmp
                                                                    - s V_parse_switches_argn) (0))]
     ((3 # 2) + (1 # 2) * s V_parse_switches__tmp + s V_parse_switches_z
      + max0(-1 + s V_parse_switches__tmp - s V_parse_switches_argn)
      - (1 # 2) * max0(-1 + s V_parse_switches_argn)
      - (1 # 2) * max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 210 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 211 => ((1 # 2) + (1 # 2) * s V_parse_switches_argn
             + s V_parse_switches_z
             - (1 # 2) * max0(-1 + s V_parse_switches_argn)
             + max0(s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 212 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 213 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 214 => ((1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
             - (1 # 2) * max0(-2 + s V_parse_switches_argn)
             + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
   | 215 => hints
     [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                    + s V_parse_switches_argn)) (F_check_ge (-1
                                                                    + s V_parse_switches_argn) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_parse_switches_argn) (0))) (F_max0_ge_0 (-2
                                                                    + s V_parse_switches_argn))]
     (-(1 # 1) + (1 # 2) * s V_parse_switches_argn + s V_parse_switches_z
      - (1 # 2) * max0(-2 + s V_parse_switches_argn)
      + max0(1 + s V_parse_switches__tmp - s V_parse_switches_argn) <= z)%Q
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
