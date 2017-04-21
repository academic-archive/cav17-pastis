Require Import pasta.Pasta.

Inductive proc: Type :=
  P_zchar_set_cache.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_zchar_set_cache_z := 1%positive.
Notation V_zchar_set_cache__tmp := 2%positive.
Notation V_zchar_set_cache_code := 3%positive.
Notation V_zchar_set_cache_code1 := 4%positive.
Notation V_zchar_set_cache_es_code_ := 5%positive.
Notation V_zchar_set_cache_have_cdevproc := 6%positive.
Notation V_zchar_set_cache_i := 7%positive.
Notation V_zchar_set_cache_metrics2 := 8%positive.
Notation V_zchar_set_cache_nparams := 9%positive.
Notation V_zchar_set_cache_pbfont_dref_off96 := 10%positive.
Notation V_zchar_set_cache_cont_fill := 11%positive.
Notation V_zchar_set_cache_cont_stroke := 12%positive.
Notation V_zchar_set_cache_op := 13%positive.
Notation V_zchar_set_cache_pbbox := 14%positive.
Notation V_zchar_set_cache_pbfont := 15%positive.
Notation V_zchar_set_cache_pcnref := 16%positive.
Notation V_zchar_set_cache_psb := 17%positive.
Notation V_zchar_set_cache_pwidth := 18%positive.
Definition Pedges_zchar_set_cache: list (edge proc) :=
  (EA 1 (AAssign V_zchar_set_cache_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_zchar_set_cache_metrics2 (Some (ENum (0)))) 3)::(EA 3 AWeaken 4)::
  (EA 4 (AGuard (fun s => ((eval (EVar V_zchar_set_cache_pbfont_dref_off96)
  s) = (eval (ENum (0)) s))%Z)) 11)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_pbfont_dref_off96) s) <>
  (eval (ENum (0)) s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 ANone 8)::
  (EA 6 ANone 7)::(EA 7 ANone 9)::(EA 8 ANone 9)::(EA 9 ANone 10)::
  (EA 10 AWeaken 14)::(EA 11 AWeaken 12)::(EA 12 ANone 13)::
  (EA 13 AWeaken 14)::(EA 14 ANone 15)::(EA 14 ANone 42)::
  (EA 15 AWeaken 16)::(EA 16 ANone 20)::(EA 16 ANone 17)::(EA 17 (AAssign
  V_zchar_set_cache__tmp (Some (ENum (-20)))) 18)::(EA 18 ANone 19)::
  (EA 19 AWeaken 150)::(EA 20 AWeaken 21)::(EA 21 ANone 25)::
  (EA 21 ANone 22)::(EA 22 (AAssign V_zchar_set_cache__tmp
  (Some (ENum (-7)))) 23)::(EA 23 ANone 24)::(EA 24 AWeaken 150)::
  (EA 25 AWeaken 26)::(EA 26 ANone 27)::(EA 26 ANone 41)::
  (EA 27 AWeaken 28)::(EA 28 ANone 32)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_zchar_set_cache__tmp None) 30)::(EA 30 ANone 31)::(EA 31 AWeaken 150)::
  (EA 32 AWeaken 33)::(EA 33 ANone 34)::(EA 33 ANone 40)::(EA 34 (AAssign
  V_zchar_set_cache_code None) 35)::(EA 35 AWeaken 36)::(EA 36 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_code) s) < (eval (ENum (0))
  s))%Z)) 146)::(EA 36 (AGuard (fun s => ((eval (EVar V_zchar_set_cache_code)
  s) >= (eval (ENum (0)) s))%Z)) 37)::(EA 37 AWeaken 38)::(EA 38 (AAssign
  V_zchar_set_cache_metrics2 (Some (ENum (1)))) 39)::(EA 39 ANone 40)::
  (EA 40 ANone 41)::(EA 41 ANone 42)::(EA 42 (AAssign
  V_zchar_set_cache_have_cdevproc None) 43)::(EA 43 AWeaken 44)::
  (EA 44 (AGuard (fun s => ((eval (EVar V_zchar_set_cache_have_cdevproc)
  s) <> (eval (ENum (0)) s))%Z)) 75)::(EA 44 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_have_cdevproc) s) =
  (eval (ENum (0)) s))%Z)) 45)::(EA 45 AWeaken 46)::(EA 46 ANone 74)::
  (EA 46 ANone 47)::(EA 47 AWeaken 48)::(EA 48 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_metrics2) s) <> (eval (ENum (0))
  s))%Z)) 51)::(EA 48 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_metrics2) s) = (eval (ENum (0))
  s))%Z)) 49)::(EA 49 AWeaken 50)::(EA 50 ANone 53)::(EA 51 AWeaken 52)::
  (EA 52 ANone 53)::(EA 53 (AAssign V_zchar_set_cache_code1 None) 54)::
  (EA 54 AWeaken 55)::(EA 55 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_code1) s) < (eval (ENum (0))
  s))%Z)) 70)::(EA 55 (AGuard (fun s => ((eval (EVar V_zchar_set_cache_code1)
  s) >= (eval (ENum (0)) s))%Z)) 56)::(EA 56 AWeaken 57)::(EA 57 ANone 58)::
  (EA 57 ANone 64)::(EA 58 ANone 59)::(EA 59 AWeaken 60)::(EA 60 ANone 67)::
  (EA 60 ANone 61)::(EA 61 ANone 62)::(EA 62 ANone 63)::(EA 63 ANone 64)::
  (EA 64 (AAssign V_zchar_set_cache__tmp None) 65)::(EA 65 ANone 66)::
  (EA 66 AWeaken 150)::(EA 67 (AAssign V_zchar_set_cache__tmp
  (Some (ENum (-16)))) 68)::(EA 68 ANone 69)::(EA 69 AWeaken 150)::
  (EA 70 AWeaken 71)::(EA 71 (AAssign V_zchar_set_cache__tmp
  (Some (EVar V_zchar_set_cache_code1))) 72)::(EA 72 ANone 73)::
  (EA 73 AWeaken 150)::(EA 74 AWeaken 76)::(EA 75 AWeaken 76)::(EA 76 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_have_cdevproc) s) <>
  (eval (ENum (0)) s))%Z)) 87)::(EA 76 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_have_cdevproc) s) =
  (eval (ENum (0)) s))%Z)) 77)::(EA 77 AWeaken 78)::(EA 78 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_metrics2) s) <> (eval (ENum (0))
  s))%Z)) 82)::(EA 78 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_metrics2) s) = (eval (ENum (0))
  s))%Z)) 79)::(EA 79 AWeaken 80)::(EA 80 (AAssign V_zchar_set_cache_nparams
  (Some (ENum (6)))) 81)::(EA 81 ANone 85)::(EA 82 AWeaken 83)::
  (EA 83 (AAssign V_zchar_set_cache_nparams (Some (ENum (10)))) 84)::
  (EA 84 ANone 85)::(EA 85 ANone 86)::(EA 86 AWeaken 100)::
  (EA 87 AWeaken 88)::(EA 88 ANone 92)::(EA 88 ANone 89)::(EA 89 (AAssign
  V_zchar_set_cache__tmp None) 90)::(EA 90 ANone 91)::(EA 91 AWeaken 150)::
  (EA 92 AWeaken 93)::(EA 93 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_metrics2) s) <> (eval (ENum (0))
  s))%Z)) 96)::(EA 93 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_metrics2) s) = (eval (ENum (0))
  s))%Z)) 94)::(EA 94 AWeaken 95)::(EA 95 ANone 97)::(EA 96 AWeaken 97)::
  (EA 97 (AAssign V_zchar_set_cache_nparams (Some (ENum (10)))) 98)::
  (EA 98 ANone 99)::(EA 99 AWeaken 100)::(EA 100 ANone 102)::
  (EA 100 ANone 101)::(EA 101 AWeaken 108)::(EA 102 (AAssign
  V_zchar_set_cache_es_code_ None) 103)::(EA 103 AWeaken 104)::
  (EA 104 (AGuard (fun s => ((eval (EVar V_zchar_set_cache_es_code_) s) <
  (eval (ENum (0)) s))%Z)) 142)::(EA 104 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_es_code_) s) >= (eval (ENum (0))
  s))%Z)) 105)::(EA 105 AWeaken 106)::(EA 106 ANone 107)::
  (EA 107 AWeaken 108)::(EA 108 ANone 118)::(EA 108 ANone 109)::
  (EA 109 ANone 110)::(EA 110 AWeaken 111)::(EA 111 ANone 115)::
  (EA 111 ANone 112)::(EA 112 ANone 113)::(EA 113 ANone 114)::
  (EA 114 ANone 124)::(EA 115 (AAssign V_zchar_set_cache__tmp
  (Some (ENum (-16)))) 116)::(EA 116 ANone 117)::(EA 117 AWeaken 150)::
  (EA 118 ANone 119)::(EA 119 AWeaken 120)::(EA 120 ANone 139)::
  (EA 120 ANone 121)::(EA 121 ANone 122)::(EA 122 ANone 123)::
  (EA 123 ANone 124)::(EA 124 (AAssign V_zchar_set_cache_i
  (Some (ENum (0)))) 125)::(EA 125 ANone 126)::(EA 126 AWeaken 127)::
  (EA 127 (AGuard (fun s => ((eval (EVar V_zchar_set_cache_i) s) <
  (eval (EVar V_zchar_set_cache_nparams) s))%Z)) 132)::(EA 127 (AGuard
  (fun s => ((eval (EVar V_zchar_set_cache_i) s) >=
  (eval (EVar V_zchar_set_cache_nparams) s))%Z)) 128)::(EA 128 AWeaken 129)::
  (EA 129 (AAssign V_zchar_set_cache__tmp (Some (ENum (5)))) 130)::
  (EA 130 ANone 131)::(EA 131 AWeaken 150)::(EA 132 AWeaken 133)::
  (EA 133 ANone 134)::(EA 134 (AAssign V_zchar_set_cache_i
  (Some (EAdd (EVar V_zchar_set_cache_i) (ENum (1))))) 135)::
  (EA 135 ANone 136)::(EA 136 ANone 137)::(EA 137 (AAssign
  V_zchar_set_cache_z (Some (EAdd (ENum (1))
  (EVar V_zchar_set_cache_z)))) 138)::(EA 138 AWeaken 127)::(EA 139 (AAssign
  V_zchar_set_cache__tmp (Some (ENum (-16)))) 140)::(EA 140 ANone 141)::
  (EA 141 AWeaken 150)::(EA 142 AWeaken 143)::(EA 143 (AAssign
  V_zchar_set_cache__tmp (Some (EVar V_zchar_set_cache_es_code_))) 144)::
  (EA 144 ANone 145)::(EA 145 AWeaken 150)::(EA 146 AWeaken 147)::
  (EA 147 (AAssign V_zchar_set_cache__tmp
  (Some (EVar V_zchar_set_cache_code))) 148)::(EA 148 ANone 149)::
  (EA 149 AWeaken 150)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_zchar_set_cache => Pedges_zchar_set_cache
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_zchar_set_cache => 150
     end)%positive;
  var_global := var_global
}.

Definition ai_zchar_set_cache (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 3 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 4 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 5 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 6 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 7 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 8 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 9 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 10 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 11 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_pbfont_dref_off96 <= 0 /\ -1 * s V_zchar_set_cache_pbfont_dref_off96 <= 0)%Z
   | 12 => (-1 * s V_zchar_set_cache_pbfont_dref_off96 <= 0 /\ 1 * s V_zchar_set_cache_pbfont_dref_off96 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 13 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_pbfont_dref_off96 <= 0 /\ -1 * s V_zchar_set_cache_pbfont_dref_off96 <= 0)%Z
   | 14 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 15 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 16 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 17 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 18 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache__tmp + 20 <= 0 /\ -1 * s V_zchar_set_cache__tmp + -20 <= 0)%Z
   | 19 => (-1 * s V_zchar_set_cache__tmp + -20 <= 0 /\ 1 * s V_zchar_set_cache__tmp + 20 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 20 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 21 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 22 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 23 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache__tmp + 7 <= 0 /\ -1 * s V_zchar_set_cache__tmp + -7 <= 0)%Z
   | 24 => (-1 * s V_zchar_set_cache__tmp + -7 <= 0 /\ 1 * s V_zchar_set_cache__tmp + 7 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 25 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 26 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 27 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 28 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 29 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 30 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 31 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 32 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 33 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 34 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 35 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 36 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 37 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_code <= 0)%Z
   | 38 => (-1 * s V_zchar_set_cache_code <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 39 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_code <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 + 1 <= 0)%Z
   | 40 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 41 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 42 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 43 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 44 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 45 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0)%Z
   | 46 => (-1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 47 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0)%Z
   | 48 => (-1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 49 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 50 => (1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 51 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_metrics2 + 1 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 52 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 + 1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 53 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 54 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 55 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 56 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_code1 <= 0)%Z
   | 57 => (-1 * s V_zchar_set_cache_code1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 58 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_code1 <= 0)%Z
   | 59 => (-1 * s V_zchar_set_cache_code1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 60 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_code1 <= 0)%Z
   | 61 => (-1 * s V_zchar_set_cache_code1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 62 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_code1 <= 0)%Z
   | 63 => (-1 * s V_zchar_set_cache_code1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 64 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_code1 <= 0)%Z
   | 65 => (-1 * s V_zchar_set_cache_code1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 66 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_code1 <= 0)%Z
   | 67 => (-1 * s V_zchar_set_cache_code1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 68 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_code1 <= 0 /\ 1 * s V_zchar_set_cache__tmp + 16 <= 0 /\ -1 * s V_zchar_set_cache__tmp + -16 <= 0)%Z
   | 69 => (-1 * s V_zchar_set_cache__tmp + -16 <= 0 /\ 1 * s V_zchar_set_cache__tmp + 16 <= 0 /\ -1 * s V_zchar_set_cache_code1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 70 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_code1 + 1 <= 0)%Z
   | 71 => (1 * s V_zchar_set_cache_code1 + 1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 72 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_code1 + 1 <= 0 /\ 1 * s V_zchar_set_cache__tmp + 1 <= 0)%Z
   | 73 => (1 * s V_zchar_set_cache__tmp + 1 <= 0 /\ 1 * s V_zchar_set_cache_code1 + 1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 74 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0)%Z
   | 75 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 76 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 77 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0)%Z
   | 78 => (-1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 79 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 80 => (1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 81 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -6 <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 82 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_metrics2 + 1 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 83 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 + 1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 84 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_metrics2 + 1 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_nparams + 10 <= 0)%Z
   | 85 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 86 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ -1 * s V_zchar_set_cache_have_cdevproc <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 87 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 88 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 89 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 90 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 91 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 92 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 93 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 94 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 95 => (1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 96 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_metrics2 + 1 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0)%Z
   | 97 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 98 => (1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_nparams + 10 <= 0)%Z
   | 99 => (-1 * s V_zchar_set_cache_nparams + 10 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0)%Z
   | 100 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0)%Z
   | 101 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 102 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 103 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0)%Z
   | 104 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 105 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_es_code_ <= 0)%Z
   | 106 => (-1 * s V_zchar_set_cache_es_code_ <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 107 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_es_code_ <= 0)%Z
   | 108 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 109 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0)%Z
   | 110 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 111 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0)%Z
   | 112 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 113 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0)%Z
   | 114 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 115 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 116 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ 1 * s V_zchar_set_cache__tmp + 16 <= 0 /\ -1 * s V_zchar_set_cache__tmp + -16 <= 0)%Z
   | 117 => (-1 * s V_zchar_set_cache__tmp + -16 <= 0 /\ 1 * s V_zchar_set_cache__tmp + 16 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 118 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0)%Z
   | 119 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 120 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0)%Z
   | 121 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 122 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0)%Z
   | 123 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 124 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0)%Z
   | 125 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_i <= 0 /\ -1 * s V_zchar_set_cache_i <= 0)%Z
   | 126 => (-1 * s V_zchar_set_cache_i <= 0 /\ 1 * s V_zchar_set_cache_i <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0)%Z
   | 127 => (-1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_i <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_i+ -1 * s V_zchar_set_cache_nparams <= 0)%Z
   | 128 => (1 * s V_zchar_set_cache_i+ -1 * s V_zchar_set_cache_nparams <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_i <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_i+ 1 * s V_zchar_set_cache_nparams <= 0)%Z
   | 129 => (-1 * s V_zchar_set_cache_i+ 1 * s V_zchar_set_cache_nparams <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_i <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_i+ -1 * s V_zchar_set_cache_nparams <= 0)%Z
   | 130 => (1 * s V_zchar_set_cache_i+ -1 * s V_zchar_set_cache_nparams <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_i <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_i+ 1 * s V_zchar_set_cache_nparams <= 0 /\ 1 * s V_zchar_set_cache__tmp + -5 <= 0 /\ -1 * s V_zchar_set_cache__tmp + 5 <= 0)%Z
   | 131 => (-1 * s V_zchar_set_cache__tmp + 5 <= 0 /\ 1 * s V_zchar_set_cache__tmp + -5 <= 0 /\ -1 * s V_zchar_set_cache_i+ 1 * s V_zchar_set_cache_nparams <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_i <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_i+ -1 * s V_zchar_set_cache_nparams <= 0)%Z
   | 132 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_i <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_i+ -1 * s V_zchar_set_cache_nparams + 1 <= 0)%Z
   | 133 => (1 * s V_zchar_set_cache_i+ -1 * s V_zchar_set_cache_nparams + 1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_i <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 134 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_i <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_i+ -1 * s V_zchar_set_cache_nparams + 1 <= 0)%Z
   | 135 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ -1 * s V_zchar_set_cache_i + 1 <= 0 /\ 1 * s V_zchar_set_cache_i+ -1 * s V_zchar_set_cache_nparams <= 0)%Z
   | 136 => (1 * s V_zchar_set_cache_i+ -1 * s V_zchar_set_cache_nparams <= 0 /\ -1 * s V_zchar_set_cache_i + 1 <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | 137 => (-1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ -1 * s V_zchar_set_cache_i + 1 <= 0 /\ 1 * s V_zchar_set_cache_i+ -1 * s V_zchar_set_cache_nparams <= 0)%Z
   | 138 => (1 * s V_zchar_set_cache_i+ -1 * s V_zchar_set_cache_nparams <= 0 /\ -1 * s V_zchar_set_cache_i + 1 <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_z + 1 <= 0)%Z
   | 139 => (1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 140 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ 1 * s V_zchar_set_cache__tmp + 16 <= 0 /\ -1 * s V_zchar_set_cache__tmp + -16 <= 0)%Z
   | 141 => (-1 * s V_zchar_set_cache__tmp + -16 <= 0 /\ 1 * s V_zchar_set_cache__tmp + 16 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 142 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ 1 * s V_zchar_set_cache_es_code_ + 1 <= 0)%Z
   | 143 => (1 * s V_zchar_set_cache_es_code_ + 1 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 144 => (-1 * s V_zchar_set_cache_nparams + 6 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ 1 * s V_zchar_set_cache_es_code_ + 1 <= 0 /\ 1 * s V_zchar_set_cache__tmp + 1 <= 0)%Z
   | 145 => (1 * s V_zchar_set_cache__tmp + 1 <= 0 /\ 1 * s V_zchar_set_cache_es_code_ + 1 <= 0 /\ 1 * s V_zchar_set_cache_nparams + -10 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_nparams + 6 <= 0)%Z
   | 146 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_code + 1 <= 0)%Z
   | 147 => (1 * s V_zchar_set_cache_code + 1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 148 => (-1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_code + 1 <= 0 /\ 1 * s V_zchar_set_cache__tmp + 1 <= 0)%Z
   | 149 => (1 * s V_zchar_set_cache__tmp + 1 <= 0 /\ 1 * s V_zchar_set_cache_code + 1 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_z <= 0 /\ 1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0)%Z
   | 150 => (1 * s V_zchar_set_cache_metrics2 + -1 <= 0 /\ -1 * s V_zchar_set_cache_metrics2 <= 0 /\ -1 * s V_zchar_set_cache_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_zchar_set_cache (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((10 # 1) <= z)%Q
   | 2 => ((10 # 1) <= z)%Q
   | 3 => ((10 # 1) <= z)%Q
   | 4 => ((10 # 1) <= z)%Q
   | 5 => ((10 # 1) <= z)%Q
   | 6 => ((10 # 1) <= z)%Q
   | 7 => ((10 # 1) <= z)%Q
   | 8 => ((10 # 1) <= z)%Q
   | 9 => ((10 # 1) <= z)%Q
   | 10 => ((10 # 1) <= z)%Q
   | 11 => ((10 # 1) <= z)%Q
   | 12 => ((10 # 1) <= z)%Q
   | 13 => ((10 # 1) <= z)%Q
   | 14 => ((10 # 1) <= z)%Q
   | 15 => ((10 # 1) <= z)%Q
   | 16 => ((10 # 1) <= z)%Q
   | 17 => ((10 # 1) <= z)%Q
   | 18 => ((5 # 2) * max0(-16 - s V_zchar_set_cache__tmp) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zchar_set_cache_z));
      (*-2.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (-16
                                                   - s V_zchar_set_cache__tmp)) (F_check_ge (0) (0))]
     ((5 # 2) * max0(-16 - s V_zchar_set_cache__tmp) <= z)%Q
   | 20 => ((10 # 1) <= z)%Q
   | 21 => ((10 # 1) <= z)%Q
   | 22 => ((10 # 1) <= z)%Q
   | 23 => ((5 # 3) * max0(-1 - s V_zchar_set_cache__tmp) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zchar_set_cache_z));
      (*-1.66667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                       - s V_zchar_set_cache__tmp)) (F_check_ge (0) (0))]
     ((5 # 3) * max0(-1 - s V_zchar_set_cache__tmp) <= z)%Q
   | 25 => ((10 # 1) <= z)%Q
   | 26 => ((10 # 1) <= z)%Q
   | 27 => ((10 # 1) <= z)%Q
   | 28 => ((10 # 1) <= z)%Q
   | 29 => ((10 # 1) <= z)%Q
   | 30 => ((10 # 1) <= z)%Q
   | 31 => hints
     [(*-10 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zchar_set_cache_z))]
     ((10 # 1) <= z)%Q
   | 32 => ((10 # 1) <= z)%Q
   | 33 => ((10 # 1) <= z)%Q
   | 34 => ((10 # 1) <= z)%Q
   | 35 => ((10 # 1) <= z)%Q
   | 36 => ((10 # 1) <= z)%Q
   | 37 => ((10 # 1) <= z)%Q
   | 38 => ((10 # 1) <= z)%Q
   | 39 => ((10 # 1) <= z)%Q
   | 40 => ((10 # 1) <= z)%Q
   | 41 => ((10 # 1) <= z)%Q
   | 42 => ((10 # 1) <= z)%Q
   | 43 => ((10 # 1) <= z)%Q
   | 44 => ((10 # 1) <= z)%Q
   | 45 => hints
     [(*-10 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                - s V_zchar_set_cache_metrics2) (0))) (F_max0_ge_0 (1
                                                                    - s V_zchar_set_cache_metrics2))]
     ((10 # 1) <= z)%Q
   | 46 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 47 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 48 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 49 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 50 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 51 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 52 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 53 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 54 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 55 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 56 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zchar_set_cache_z))]
     ((10 # 1) * s V_zchar_set_cache_metrics2
      + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 57 => ((10 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 58 => ((10 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 59 => ((10 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 60 => ((10 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 61 => ((10 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 62 => ((10 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 63 => ((10 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 64 => ((10 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 65 => ((10 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 66 => hints
     [(*-10 0*) F_one;
      (*-10 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                    - s V_zchar_set_cache_metrics2)) (F_check_ge (1
                                                                    - s V_zchar_set_cache_metrics2) (0))]
     ((10 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
      + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 67 => ((10 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 68 => (-(10 # 1) + (10 # 1) * s V_zchar_set_cache_metrics2
            + s V_zchar_set_cache_z
            + (10 # 9) * max0(-7 - s V_zchar_set_cache__tmp)
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 69 => hints
     [(*-10 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                    - s V_zchar_set_cache_metrics2)) (F_check_ge (1
                                                                    - s V_zchar_set_cache_metrics2) (0));
      (*-1.11111 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                       - s V_zchar_set_cache__tmp)) (F_check_ge (0) (0))]
     (-(10 # 1) + (10 # 1) * s V_zchar_set_cache_metrics2
      + s V_zchar_set_cache_z
      + (10 # 9) * max0(-7 - s V_zchar_set_cache__tmp)
      + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 70 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 71 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 72 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 73 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zchar_set_cache_z));
      (*-10 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zchar_set_cache_metrics2)) (F_check_ge (0) (0));
      (*-10 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zchar_set_cache_metrics2) (0))) (F_max0_ge_0 (s V_zchar_set_cache_metrics2));
      (*-10 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                  - s V_zchar_set_cache_metrics2)) (F_check_ge (0) (0))]
     ((10 # 1) * s V_zchar_set_cache_metrics2
      + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 74 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 75 => hints
     [(*-10 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                - s V_zchar_set_cache_metrics2) (0))) (F_max0_ge_0 (1
                                                                    - s V_zchar_set_cache_metrics2))]
     ((10 # 1) <= z)%Q
   | 76 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 77 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 78 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 79 => hints
     [(*0 4*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zchar_set_cache_metrics2)) (F_check_ge (0) (0));
      (*-4 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_zchar_set_cache_metrics2)) (F_check_ge (0) (0))]
     ((10 # 1) * s V_zchar_set_cache_metrics2
      + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 80 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
            - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 81 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + max0(-6 + s V_zchar_set_cache_nparams)
            + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
            - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 82 => hints
     [(*-4 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zchar_set_cache_metrics2)) (F_check_ge (s V_zchar_set_cache_metrics2) (0));
      (*0 4*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                  - s V_zchar_set_cache_metrics2)) (F_check_ge (1
                                                                    - s V_zchar_set_cache_metrics2) (0))]
     ((10 # 1) * s V_zchar_set_cache_metrics2
      + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 83 => ((4 # 1) + (10 # 1) * s V_zchar_set_cache_metrics2
            + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
            - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 84 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + max0(-6 + s V_zchar_set_cache_nparams)
            + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
            - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 85 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + max0(-6 + s V_zchar_set_cache_nparams)
            + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
            - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 86 => ((10 # 1) * s V_zchar_set_cache_metrics2
            + max0(-6 + s V_zchar_set_cache_nparams)
            + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
            - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 87 => hints
     [(*0 10*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   - s V_zchar_set_cache_metrics2)) (F_check_ge (1
                                                                    - s V_zchar_set_cache_metrics2) (0))]
     ((10 # 1) * s V_zchar_set_cache_metrics2
      + (10 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 88 => ((10 # 1) <= z)%Q
   | 89 => ((10 # 1) <= z)%Q
   | 90 => ((10 # 1) <= z)%Q
   | 91 => hints
     [(*-10 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zchar_set_cache_z))]
     ((10 # 1) <= z)%Q
   | 92 => hints
     [(*0 4*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zchar_set_cache_metrics2)) (F_check_ge (s V_zchar_set_cache_metrics2) (0))]
     ((10 # 1) <= z)%Q
   | 93 => ((10 # 1) + (4 # 1) * s V_zchar_set_cache_metrics2
            - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 94 => ((10 # 1) + (4 # 1) * s V_zchar_set_cache_metrics2
            - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 95 => ((10 # 1) + (4 # 1) * s V_zchar_set_cache_metrics2
            - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 96 => ((10 # 1) + (4 # 1) * s V_zchar_set_cache_metrics2
            - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 97 => ((10 # 1) + (4 # 1) * s V_zchar_set_cache_metrics2
            - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 98 => ((6 # 1) + (4 # 1) * s V_zchar_set_cache_metrics2
            + max0(-6 + s V_zchar_set_cache_nparams)
            - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 99 => hints
     [(*-6 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               - s V_zchar_set_cache_metrics2) (0))) (F_max0_ge_0 (1
                                                                    - s V_zchar_set_cache_metrics2))]
     ((6 # 1) + (4 # 1) * s V_zchar_set_cache_metrics2
      + max0(-6 + s V_zchar_set_cache_nparams)
      - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 100 => ((10 # 1) * s V_zchar_set_cache_metrics2
             + max0(-6 + s V_zchar_set_cache_nparams)
             + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
             - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 101 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*0 2*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zchar_set_cache_z))]
     ((10 # 1) * s V_zchar_set_cache_metrics2
      + max0(-6 + s V_zchar_set_cache_nparams)
      + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
      - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 102 => ((10 # 1) * s V_zchar_set_cache_metrics2
             + max0(-6 + s V_zchar_set_cache_nparams)
             + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
             - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 103 => hints
     [(*0 4*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zchar_set_cache_metrics2) (0))) (F_max0_ge_0 (s V_zchar_set_cache_metrics2))]
     ((10 # 1) * s V_zchar_set_cache_metrics2
      + max0(-6 + s V_zchar_set_cache_nparams)
      + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
      - (4 # 1) * max0(s V_zchar_set_cache_metrics2) <= z)%Q
   | 104 => ((6 # 1) * s V_zchar_set_cache_metrics2
             + max0(-6 + s V_zchar_set_cache_nparams)
             + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 105 => ((6 # 1) * s V_zchar_set_cache_metrics2
             + max0(-6 + s V_zchar_set_cache_nparams)
             + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 106 => ((6 # 1) * s V_zchar_set_cache_metrics2
             + max0(-6 + s V_zchar_set_cache_nparams)
             + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 107 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*0 2*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zchar_set_cache_z));
      (*-4 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zchar_set_cache_metrics2)) (F_check_ge (s V_zchar_set_cache_metrics2) (0))]
     ((6 # 1) * s V_zchar_set_cache_metrics2
      + max0(-6 + s V_zchar_set_cache_nparams)
      + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 108 => ((10 # 1) * s V_zchar_set_cache_metrics2
             + (2 # 1) * s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
             - (4 # 1) * max0(s V_zchar_set_cache_metrics2)
             + max0(-s V_zchar_set_cache_z) <= z)%Q
   | 109 => ((10 # 1) * s V_zchar_set_cache_metrics2
             + (2 # 1) * s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
             - (4 # 1) * max0(s V_zchar_set_cache_metrics2)
             + max0(-s V_zchar_set_cache_z) <= z)%Q
   | 110 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (s V_zchar_set_cache_z));
      (*0 4*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zchar_set_cache_metrics2) (0))) (F_max0_ge_0 (s V_zchar_set_cache_metrics2));
      (*-6 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   - s V_zchar_set_cache_metrics2)) (F_check_ge (1
                                                                    - s V_zchar_set_cache_metrics2) (0))]
     ((10 # 1) * s V_zchar_set_cache_metrics2
      + (2 # 1) * s V_zchar_set_cache_z
      + max0(-6 + s V_zchar_set_cache_nparams)
      + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
      - (4 # 1) * max0(s V_zchar_set_cache_metrics2)
      + max0(-s V_zchar_set_cache_z) <= z)%Q
   | 111 => ((6 # 1) + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 112 => ((6 # 1) + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 113 => ((6 # 1) + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 114 => ((6 # 1) + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 115 => ((6 # 1) + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 116 => (s V_zchar_set_cache_z
             + (2 # 3) * max0(-7 - s V_zchar_set_cache__tmp)
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 117 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                 + s V_zchar_set_cache_nparams)) (F_check_ge (0) (0));
      (*-0.666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                        - s V_zchar_set_cache__tmp)) (F_check_ge (0) (0))]
     (s V_zchar_set_cache_z + (2 # 3) * max0(-7 - s V_zchar_set_cache__tmp)
      + max0(-6 + s V_zchar_set_cache_nparams) + max0(-s V_zchar_set_cache_z)
      + max0(s V_zchar_set_cache_z) <= z)%Q
   | 118 => ((10 # 1) * s V_zchar_set_cache_metrics2
             + (2 # 1) * s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
             - (4 # 1) * max0(s V_zchar_set_cache_metrics2)
             + max0(-s V_zchar_set_cache_z) <= z)%Q
   | 119 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (s V_zchar_set_cache_z));
      (*0 4*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zchar_set_cache_metrics2) (0))) (F_max0_ge_0 (s V_zchar_set_cache_metrics2));
      (*0 6*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                  - s V_zchar_set_cache_metrics2)) (F_check_ge (1
                                                                    - s V_zchar_set_cache_metrics2) (0))]
     ((10 # 1) * s V_zchar_set_cache_metrics2
      + (2 # 1) * s V_zchar_set_cache_z
      + max0(-6 + s V_zchar_set_cache_nparams)
      + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
      - (4 # 1) * max0(s V_zchar_set_cache_metrics2)
      + max0(-s V_zchar_set_cache_z) <= z)%Q
   | 120 => ((6 # 1) + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 121 => ((6 # 1) + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 122 => ((6 # 1) + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 123 => ((6 # 1) + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 124 => ((6 # 1) + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 125 => ((6 # 1) - s V_zchar_set_cache_i + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 126 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-s V_zchar_set_cache_z)) (F_check_ge (-
                                                                    s V_zchar_set_cache_z) (0))]
     ((6 # 1) - s V_zchar_set_cache_i + s V_zchar_set_cache_z
      + max0(-6 + s V_zchar_set_cache_nparams) + max0(-s V_zchar_set_cache_z)
      + max0(s V_zchar_set_cache_z) <= z)%Q
   | 127 => ((6 # 1) - s V_zchar_set_cache_i
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(s V_zchar_set_cache_z) <= z)%Q
   | 128 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zchar_set_cache_i
                                                              + s V_zchar_set_cache_nparams) (0))) (F_max0_ge_0 (-
                                                                    s V_zchar_set_cache_i
                                                                    + s V_zchar_set_cache_nparams))]
     ((6 # 1) - s V_zchar_set_cache_i
      + max0(-6 + s V_zchar_set_cache_nparams) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 129 => ((6 # 1) - s V_zchar_set_cache_nparams
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_i + s V_zchar_set_cache_nparams)
             + max0(s V_zchar_set_cache_z) <= z)%Q
   | 130 => ((6 # 1) - s V_zchar_set_cache_nparams
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_i + s V_zchar_set_cache_nparams)
             + max0(s V_zchar_set_cache_z) <= z)%Q
   | 131 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_zchar_set_cache_i
                                             + s V_zchar_set_cache_nparams) (-1
                                                                    - s V_zchar_set_cache_i
                                                                    + s V_zchar_set_cache_nparams));
      (*-1 0*) F_max0_ge_0 (-1 - s V_zchar_set_cache_i
                            + s V_zchar_set_cache_nparams);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zchar_set_cache_z)) (F_check_ge (s V_zchar_set_cache_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-6
                                                   + s V_zchar_set_cache_nparams)) (F_check_ge (-6
                                                                    + s V_zchar_set_cache_nparams) (0))]
     ((6 # 1) - s V_zchar_set_cache_nparams
      + max0(-6 + s V_zchar_set_cache_nparams)
      + max0(-s V_zchar_set_cache_i + s V_zchar_set_cache_nparams)
      + max0(s V_zchar_set_cache_z) <= z)%Q
   | 132 => ((6 # 1) - s V_zchar_set_cache_i
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(s V_zchar_set_cache_z) <= z)%Q
   | 133 => ((6 # 1) - s V_zchar_set_cache_i
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(s V_zchar_set_cache_z) <= z)%Q
   | 134 => ((6 # 1) - s V_zchar_set_cache_i
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(s V_zchar_set_cache_z) <= z)%Q
   | 135 => ((7 # 1) - s V_zchar_set_cache_i
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(s V_zchar_set_cache_z) <= z)%Q
   | 136 => ((7 # 1) - s V_zchar_set_cache_i
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(s V_zchar_set_cache_z) <= z)%Q
   | 137 => ((7 # 1) - s V_zchar_set_cache_i
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(s V_zchar_set_cache_z) <= z)%Q
   | 138 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (s V_zchar_set_cache_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_zchar_set_cache_z)) (F_check_ge (-1
                                                                    + s V_zchar_set_cache_z) (0))]
     ((7 # 1) - s V_zchar_set_cache_i
      + max0(-6 + s V_zchar_set_cache_nparams)
      + max0(-1 + s V_zchar_set_cache_z) <= z)%Q
   | 139 => ((6 # 1) + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 140 => (s V_zchar_set_cache_z
             + (2 # 3) * max0(-7 - s V_zchar_set_cache__tmp)
             + max0(-6 + s V_zchar_set_cache_nparams)
             + max0(-s V_zchar_set_cache_z) + max0(s V_zchar_set_cache_z) <= z)%Q
   | 141 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                 + s V_zchar_set_cache_nparams)) (F_check_ge (0) (0));
      (*-0.666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-7
                                                        - s V_zchar_set_cache__tmp)) (F_check_ge (0) (0))]
     (s V_zchar_set_cache_z + (2 # 3) * max0(-7 - s V_zchar_set_cache__tmp)
      + max0(-6 + s V_zchar_set_cache_nparams) + max0(-s V_zchar_set_cache_z)
      + max0(s V_zchar_set_cache_z) <= z)%Q
   | 142 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zchar_set_cache_z))]
     ((6 # 1) * s V_zchar_set_cache_metrics2
      + max0(-6 + s V_zchar_set_cache_nparams)
      + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2) <= z)%Q
   | 143 => ((6 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
             + max0(-s V_zchar_set_cache_z) <= z)%Q
   | 144 => ((6 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
             + max0(-6 + s V_zchar_set_cache_nparams)
             + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
             + max0(-s V_zchar_set_cache_z) <= z)%Q
   | 145 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*-6 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_zchar_set_cache_metrics2)) (F_check_ge (0) (0));
      (*-6 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zchar_set_cache_metrics2) (0))) (F_max0_ge_0 (s V_zchar_set_cache_metrics2));
      (*-6 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_zchar_set_cache_metrics2)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-6
                                                 + s V_zchar_set_cache_nparams)) (F_check_ge (0) (0))]
     ((6 # 1) * s V_zchar_set_cache_metrics2 + s V_zchar_set_cache_z
      + max0(-6 + s V_zchar_set_cache_nparams)
      + (6 # 1) * max0(1 - s V_zchar_set_cache_metrics2)
      + max0(-s V_zchar_set_cache_z) <= z)%Q
   | 146 => ((10 # 1) <= z)%Q
   | 147 => ((10 # 1) <= z)%Q
   | 148 => ((10 # 1) <= z)%Q
   | 149 => hints
     [(*-10 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zchar_set_cache_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_zchar_set_cache_z) (0))) (F_max0_ge_0 (-
                                                                    s V_zchar_set_cache_z))]
     ((10 # 1) <= z)%Q
   | 150 => (s V_zchar_set_cache_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_zchar_set_cache =>
    [mkPA Q (fun n z s => ai_zchar_set_cache n s /\ annot0_zchar_set_cache n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_zchar_set_cache (proc_start P_zchar_set_cache) s1 (proc_end P_zchar_set_cache) s2 ->
    (s2 V_zchar_set_cache_z <= (10 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_zchar_set_cache.
Qed.
