Require Import pasta.Pasta.

Inductive proc: Type :=
  P_APCM_quantization.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_APCM_quantization_z := 1%positive.
Notation V_APCM_quantization_exp_out_dref := 2%positive.
Notation V_APCM_quantization_i := 3%positive.
Notation V_APCM_quantization_itest := 4%positive.
Notation V_APCM_quantization_mant_out_dref := 5%positive.
Notation V_APCM_quantization_temp := 6%positive.
Notation V_APCM_quantization_temp1 := 7%positive.
Notation V_APCM_quantization_temp2 := 8%positive.
Notation V_APCM_quantization_xmax := 9%positive.
Notation V_APCM_quantization_xmaxc := 10%positive.
Notation V_APCM_quantization_xmaxc_out_dref := 11%positive.
Notation V_APCM_quantization_exp_out := 12%positive.
Notation V_APCM_quantization_mant_out := 13%positive.
Notation V_APCM_quantization_xM := 14%positive.
Notation V_APCM_quantization_xMc := 15%positive.
Notation V_APCM_quantization_xmaxc_out := 16%positive.
Definition Pedges_APCM_quantization: list (edge proc) :=
  (EA 1 (AAssign V_APCM_quantization_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_APCM_quantization_xmax (Some (ENum (0)))) 3)::(EA 3 (AAssign
  V_APCM_quantization_i (Some (ENum (0)))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_i) s) <= (eval (ENum (12))
  s))%Z)) 97)::(EA 6 (AGuard (fun s => ((eval (EVar V_APCM_quantization_i)
  s) > (eval (ENum (12)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_APCM_quantization_temp None) 9)::(EA 9 (AAssign V_APCM_quantization_itest
  (Some (ENum (0)))) 10)::(EA 10 (AAssign V_APCM_quantization_i
  (Some (ENum (0)))) 11)::(EA 11 ANone 12)::(EA 12 AWeaken 13)::
  (EA 13 (AGuard (fun s => ((eval (EVar V_APCM_quantization_i) s) <=
  (eval (ENum (5)) s))%Z)) 78)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_i) s) > (eval (ENum (5))
  s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 ANone 16)::(EA 15 ANone 18)::
  (EA 16 AWeaken 17)::(EA 17 ANone 20)::(EA 17 ANone 18)::(EA 18 ANone 19)::
  (EA 19 AWeaken 84)::(EA 20 ANone 21)::(EA 21 (AAssign
  V_APCM_quantization_temp None) 22)::(EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_temp) s) <= (eval (ENum (11))
  s))%Z)) 25)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_temp) s) > (eval (ENum (11))
  s))%Z)) 24)::(EA 24 AWeaken 28)::(EA 25 AWeaken 26)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_temp) s) >= (eval (ENum (0))
  s))%Z)) 30)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_temp) s) < (eval (ENum (0))
  s))%Z)) 27)::(EA 27 AWeaken 28)::(EA 28 ANone 29)::(EA 29 AWeaken 84)::
  (EA 30 AWeaken 31)::(EA 31 ANone 32)::(EA 32 (AAssign
  V_APCM_quantization_xmaxc None) 33)::(EA 33 AWeaken 34)::(EA 34 ANone 35)::
  (EA 34 ANone 37)::(EA 35 AWeaken 36)::(EA 36 ANone 39)::(EA 36 ANone 37)::
  (EA 37 ANone 38)::(EA 38 AWeaken 84)::(EA 39 ANone 40)::
  (EA 40 AWeaken 41)::(EA 41 ANone 42)::(EA 41 ANone 44)::
  (EA 42 AWeaken 43)::(EA 43 ANone 46)::(EA 43 ANone 44)::(EA 44 ANone 45)::
  (EA 45 AWeaken 84)::(EA 46 ANone 47)::(EA 47 (AAssign
  V_APCM_quantization_temp1 None) 48)::(EA 48 (AAssign
  V_APCM_quantization_temp2 None) 49)::(EA 49 (AAssign V_APCM_quantization_i
  (Some (ENum (0)))) 50)::(EA 50 ANone 51)::(EA 51 AWeaken 52)::
  (EA 52 (AGuard (fun s => ((eval (EVar V_APCM_quantization_i) s) <=
  (eval (ENum (12)) s))%Z)) 59)::(EA 52 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_i) s) > (eval (ENum (12))
  s))%Z)) 53)::(EA 53 AWeaken 54)::(EA 54 (AAssign
  V_APCM_quantization_mant_out_dref None) 55)::(EA 55 (AAssign
  V_APCM_quantization_exp_out_dref None) 56)::(EA 56 (AAssign
  V_APCM_quantization_xmaxc_out_dref
  (Some (EVar V_APCM_quantization_xmaxc))) 57)::(EA 57 ANone 58)::
  (EA 58 AWeaken 84)::(EA 59 AWeaken 60)::(EA 60 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_temp1) s) >= (eval (ENum (0))
  s))%Z)) 62)::(EA 60 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_temp1) s) < (eval (ENum (0))
  s))%Z)) 61)::(EA 61 AWeaken 65)::(EA 62 AWeaken 63)::(EA 63 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_temp1) s) < (eval (ENum (16))
  s))%Z)) 67)::(EA 63 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_temp1) s) >= (eval (ENum (16))
  s))%Z)) 64)::(EA 64 AWeaken 65)::(EA 65 ANone 66)::(EA 66 AWeaken 84)::
  (EA 67 AWeaken 68)::(EA 68 ANone 69)::(EA 69 (AAssign
  V_APCM_quantization_temp None) 70)::(EA 70 (AAssign
  V_APCM_quantization_temp None) 71)::(EA 71 (AAssign
  V_APCM_quantization_temp None) 72)::(EA 72 ANone 73)::(EA 73 (AAssign
  V_APCM_quantization_i (Some (EAdd (EVar V_APCM_quantization_i)
  (ENum (1))))) 74)::(EA 74 ANone 75)::(EA 75 ANone 76)::(EA 76 (AAssign
  V_APCM_quantization_z (Some (EAdd (ENum (1))
  (EVar V_APCM_quantization_z)))) 77)::(EA 77 AWeaken 52)::
  (EA 78 AWeaken 79)::(EA 79 (AAssign V_APCM_quantization_itest None) 80)::
  (EA 80 (AAssign V_APCM_quantization_temp None) 81)::(EA 81 AWeaken 82)::
  (EA 82 ANone 85)::(EA 82 ANone 83)::(EA 83 AWeaken 84)::(EA 85 ANone 86)::
  (EA 86 AWeaken 87)::(EA 87 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_itest) s) = (eval (ENum (0))
  s))%Z)) 89)::(EA 87 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_itest) s) <> (eval (ENum (0))
  s))%Z)) 88)::(EA 88 AWeaken 91)::(EA 89 AWeaken 90)::(EA 90 ANone 91)::
  (EA 91 ANone 92)::(EA 92 (AAssign V_APCM_quantization_i
  (Some (EAdd (EVar V_APCM_quantization_i) (ENum (1))))) 93)::
  (EA 93 ANone 94)::(EA 94 ANone 95)::(EA 95 (AAssign V_APCM_quantization_z
  (Some (EAdd (ENum (1)) (EVar V_APCM_quantization_z)))) 96)::
  (EA 96 AWeaken 13)::(EA 97 AWeaken 98)::(EA 98 (AAssign
  V_APCM_quantization_temp None) 99)::(EA 99 AWeaken 100)::(EA 100 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_temp) s) < (eval (ENum (0))
  s))%Z)) 103)::(EA 100 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_temp) s) >= (eval (ENum (0))
  s))%Z)) 101)::(EA 101 AWeaken 102)::(EA 102 ANone 110)::
  (EA 103 AWeaken 104)::(EA 104 (AGuard (fun s => True)) 108)::
  (EA 104 ANone 105)::(EA 105 ANone 106)::(EA 106 (AGuard
  (fun s => True)) 107)::(EA 107 AWeaken 110)::(EA 108 AWeaken 109)::
  (EA 109 ANone 110)::(EA 110 (AAssign V_APCM_quantization_temp None) 111)::
  (EA 111 AWeaken 112)::(EA 112 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_temp) s) >
  (eval (EVar V_APCM_quantization_xmax) s))%Z)) 114)::(EA 112 (AGuard
  (fun s => ((eval (EVar V_APCM_quantization_temp) s) <=
  (eval (EVar V_APCM_quantization_xmax) s))%Z)) 113)::(EA 113 AWeaken 117)::
  (EA 114 AWeaken 115)::(EA 115 (AAssign V_APCM_quantization_xmax
  (Some (EVar V_APCM_quantization_temp))) 116)::(EA 116 ANone 117)::
  (EA 117 ANone 118)::(EA 118 (AAssign V_APCM_quantization_i
  (Some (EAdd (EVar V_APCM_quantization_i) (ENum (1))))) 119)::
  (EA 119 ANone 120)::(EA 120 ANone 121)::(EA 121 (AAssign
  V_APCM_quantization_z (Some (EAdd (ENum (1))
  (EVar V_APCM_quantization_z)))) 122)::(EA 122 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_APCM_quantization => Pedges_APCM_quantization
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_APCM_quantization => 84
     end)%positive;
  var_global := var_global
}.

Definition ai_APCM_quantization (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_z <= 0)%Z
   | 3 => (-1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_xmax <= 0 /\ -1 * s V_APCM_quantization_xmax <= 0)%Z
   | 4 => (-1 * s V_APCM_quantization_xmax <= 0 /\ 1 * s V_APCM_quantization_xmax <= 0 /\ 1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 5 => (-1 * s V_APCM_quantization_i <= 0 /\ 1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_xmax <= 0 /\ -1 * s V_APCM_quantization_xmax <= 0)%Z
   | 6 => (-1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0)%Z
   | 7 => (1 * s V_APCM_quantization_i + -13 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 13 <= 0)%Z
   | 8 => (-1 * s V_APCM_quantization_i + 13 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0)%Z
   | 9 => (1 * s V_APCM_quantization_i + -13 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 13 <= 0)%Z
   | 10 => (-1 * s V_APCM_quantization_i + 13 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0 /\ 1 * s V_APCM_quantization_itest <= 0 /\ -1 * s V_APCM_quantization_itest <= 0)%Z
   | 11 => (-1 * s V_APCM_quantization_itest <= 0 /\ 1 * s V_APCM_quantization_itest <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 12 => (-1 * s V_APCM_quantization_i <= 0 /\ 1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_itest <= 0 /\ -1 * s V_APCM_quantization_itest <= 0)%Z
   | 13 => (-1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 14 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0)%Z
   | 15 => (-1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 16 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0)%Z
   | 17 => (-1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 18 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0)%Z
   | 19 => (-1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 20 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0)%Z
   | 21 => (-1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 22 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0)%Z
   | 23 => (-1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 24 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_temp + 12 <= 0)%Z
   | 25 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0)%Z
   | 26 => (1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 27 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ 1 * s V_APCM_quantization_temp + 1 <= 0)%Z
   | 28 => (-1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 29 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0)%Z
   | 30 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_temp <= 0)%Z
   | 31 => (-1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 32 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_temp <= 0)%Z
   | 33 => (-1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 34 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_temp <= 0)%Z
   | 35 => (-1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 36 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_temp <= 0)%Z
   | 37 => (-1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 38 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_temp <= 0)%Z
   | 39 => (-1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 40 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_temp <= 0)%Z
   | 41 => (-1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 42 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_temp <= 0)%Z
   | 43 => (-1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 44 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_temp <= 0)%Z
   | 45 => (-1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 46 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_temp <= 0)%Z
   | 47 => (-1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 48 => (1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_temp <= 0)%Z
   | 49 => (-1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_i + 6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0)%Z
   | 50 => (-1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 51 => (-1 * s V_APCM_quantization_i <= 0 /\ 1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_temp + -11 <= 0 /\ -1 * s V_APCM_quantization_z <= 0)%Z
   | 52 => (-1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0)%Z
   | 53 => (1 * s V_APCM_quantization_i + -13 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 13 <= 0)%Z
   | 54 => (-1 * s V_APCM_quantization_i + 13 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0)%Z
   | 55 => (1 * s V_APCM_quantization_i + -13 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 13 <= 0)%Z
   | 56 => (-1 * s V_APCM_quantization_i + 13 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0)%Z
   | 57 => (1 * s V_APCM_quantization_i + -13 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 13 <= 0)%Z
   | 58 => (-1 * s V_APCM_quantization_i + 13 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0)%Z
   | 59 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0)%Z
   | 60 => (1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 61 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ 1 * s V_APCM_quantization_temp1 + 1 <= 0)%Z
   | 62 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_temp1 <= 0)%Z
   | 63 => (-1 * s V_APCM_quantization_temp1 <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 64 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_temp1 + 16 <= 0)%Z
   | 65 => (1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 66 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0)%Z
   | 67 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_temp1 <= 0 /\ 1 * s V_APCM_quantization_temp1 + -15 <= 0)%Z
   | 68 => (1 * s V_APCM_quantization_temp1 + -15 <= 0 /\ -1 * s V_APCM_quantization_temp1 <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 69 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_temp1 <= 0 /\ 1 * s V_APCM_quantization_temp1 + -15 <= 0)%Z
   | 70 => (1 * s V_APCM_quantization_temp1 + -15 <= 0 /\ -1 * s V_APCM_quantization_temp1 <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 71 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_temp1 <= 0 /\ 1 * s V_APCM_quantization_temp1 + -15 <= 0)%Z
   | 72 => (1 * s V_APCM_quantization_temp1 + -15 <= 0 /\ -1 * s V_APCM_quantization_temp1 <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 73 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_temp1 <= 0 /\ 1 * s V_APCM_quantization_temp1 + -15 <= 0)%Z
   | 74 => (1 * s V_APCM_quantization_temp1 + -15 <= 0 /\ -1 * s V_APCM_quantization_temp1 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 1 <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0)%Z
   | 75 => (1 * s V_APCM_quantization_i + -13 <= 0 /\ -1 * s V_APCM_quantization_i + 1 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_temp1 <= 0 /\ 1 * s V_APCM_quantization_temp1 + -15 <= 0)%Z
   | 76 => (1 * s V_APCM_quantization_temp1 + -15 <= 0 /\ -1 * s V_APCM_quantization_temp1 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i + 1 <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0)%Z
   | 77 => (1 * s V_APCM_quantization_i + -13 <= 0 /\ -1 * s V_APCM_quantization_i + 1 <= 0 /\ -1 * s V_APCM_quantization_temp1 <= 0 /\ 1 * s V_APCM_quantization_temp1 + -15 <= 0 /\ -1 * s V_APCM_quantization_z + 1 <= 0)%Z
   | 78 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -5 <= 0)%Z
   | 79 => (1 * s V_APCM_quantization_i + -5 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 80 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -5 <= 0)%Z
   | 81 => (1 * s V_APCM_quantization_i + -5 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 82 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -5 <= 0)%Z
   | 83 => (1 * s V_APCM_quantization_i + -5 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 84 => (1 * s V_APCM_quantization_i + -13 <= 0 /\ -1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0)%Z
   | 85 => (1 * s V_APCM_quantization_i + -5 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 86 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -5 <= 0)%Z
   | 87 => (1 * s V_APCM_quantization_i + -5 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 88 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -5 <= 0)%Z
   | 89 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -5 <= 0 /\ 1 * s V_APCM_quantization_itest <= 0 /\ -1 * s V_APCM_quantization_itest <= 0)%Z
   | 90 => (-1 * s V_APCM_quantization_itest <= 0 /\ 1 * s V_APCM_quantization_itest <= 0 /\ 1 * s V_APCM_quantization_i + -5 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 91 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -5 <= 0)%Z
   | 92 => (1 * s V_APCM_quantization_i + -5 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 93 => (-1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_i + 1 <= 0)%Z
   | 94 => (-1 * s V_APCM_quantization_i + 1 <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z <= 0)%Z
   | 95 => (-1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_i + 1 <= 0)%Z
   | 96 => (-1 * s V_APCM_quantization_i + 1 <= 0 /\ 1 * s V_APCM_quantization_i + -6 <= 0 /\ -1 * s V_APCM_quantization_z + 1 <= 0)%Z
   | 97 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0)%Z
   | 98 => (1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 99 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0)%Z
   | 100 => (1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 101 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_temp <= 0)%Z
   | 102 => (-1 * s V_APCM_quantization_temp <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 103 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ 1 * s V_APCM_quantization_temp + 1 <= 0)%Z
   | 104 => (1 * s V_APCM_quantization_temp + 1 <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 105 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ 1 * s V_APCM_quantization_temp + 1 <= 0)%Z
   | 106 => (1 * s V_APCM_quantization_temp + 1 <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 107 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ 1 * s V_APCM_quantization_temp + 1 <= 0)%Z
   | 108 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ 1 * s V_APCM_quantization_temp + 1 <= 0)%Z
   | 109 => (1 * s V_APCM_quantization_temp + 1 <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 110 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0)%Z
   | 111 => (1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 112 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0)%Z
   | 113 => (1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0 /\ 1 * s V_APCM_quantization_temp+ -1 * s V_APCM_quantization_xmax <= 0)%Z
   | 114 => (1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_temp+ 1 * s V_APCM_quantization_xmax + 1 <= 0)%Z
   | 115 => (-1 * s V_APCM_quantization_temp+ 1 * s V_APCM_quantization_xmax + 1 <= 0 /\ -1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0)%Z
   | 116 => (1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 117 => (-1 * s V_APCM_quantization_i <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -12 <= 0)%Z
   | 118 => (1 * s V_APCM_quantization_i + -12 <= 0 /\ -1 * s V_APCM_quantization_z <= 0 /\ -1 * s V_APCM_quantization_i <= 0)%Z
   | 119 => (-1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0 /\ -1 * s V_APCM_quantization_i + 1 <= 0)%Z
   | 120 => (-1 * s V_APCM_quantization_i + 1 <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0 /\ -1 * s V_APCM_quantization_z <= 0)%Z
   | 121 => (-1 * s V_APCM_quantization_z <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0 /\ -1 * s V_APCM_quantization_i + 1 <= 0)%Z
   | 122 => (-1 * s V_APCM_quantization_i + 1 <= 0 /\ 1 * s V_APCM_quantization_i + -13 <= 0 /\ -1 * s V_APCM_quantization_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_APCM_quantization (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((32 # 1) <= z)%Q
   | 2 => ((32 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 3 => ((32 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 4 => ((19 # 1) + max0(13 - s V_APCM_quantization_i)
           + max0(s V_APCM_quantization_z) <= z)%Q
   | 5 => ((19 # 1) + max0(13 - s V_APCM_quantization_i)
           + max0(s V_APCM_quantization_z) <= z)%Q
   | 6 => ((19 # 1) + max0(13 - s V_APCM_quantization_i)
           + max0(s V_APCM_quantization_z) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (13 - s V_APCM_quantization_i) (12
                                                                    - s V_APCM_quantization_i));
      (*-1 0*) F_max0_ge_0 (12 - s V_APCM_quantization_i)]
     ((19 # 1) + max0(13 - s V_APCM_quantization_i)
      + max0(s V_APCM_quantization_z) <= z)%Q
   | 8 => ((19 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 9 => ((19 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 10 => ((19 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 11 => ((209 # 42) - (19 # 14) * s V_APCM_quantization_i
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (19 # 42) * max0(13 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_APCM_quantization_z)) (F_check_ge (s V_APCM_quantization_z) (0))]
     ((209 # 42) - (19 # 14) * s V_APCM_quantization_i
      + (19 # 14) * max0(6 - s V_APCM_quantization_i)
      + (19 # 42) * max0(13 - s V_APCM_quantization_i)
      + (13 # 6) * max0(s V_APCM_quantization_i)
      + max0(s V_APCM_quantization_z) <= z)%Q
   | 13 => ((209 # 42) - (19 # 14) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (19 # 42) * max0(13 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 14 => hints
     [(*-0.452381 0*) F_max0_pre_decrement 1 (13 - s V_APCM_quantization_i) (1);
      (*-0.37013 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (12
                                                                    - 
                                                                    s V_APCM_quantization_i) (0))) (F_max0_ge_0 (12
                                                                    - s V_APCM_quantization_i))]
     ((209 # 42) - (19 # 14) * s V_APCM_quantization_i
      + s V_APCM_quantization_z
      + (19 # 14) * max0(6 - s V_APCM_quantization_i)
      + (19 # 42) * max0(13 - s V_APCM_quantization_i)
      + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 15 => ((76 # 77) - (76 # 77) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (51 # 62) * max0(12 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 16 => ((76 # 77) - (76 # 77) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (51 # 62) * max0(12 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 17 => ((76 # 77) - (76 # 77) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (51 # 62) * max0(12 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 18 => ((76 # 77) - (76 # 77) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (51 # 62) * max0(12 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 19 => hints
     [(*-3.16667 0*) F_max0_monotonic (F_check_ge (6
                                                   - s V_APCM_quantization_i) (5
                                                                    - s V_APCM_quantization_i));
      (*-3.16667 0*) F_max0_ge_0 (5 - s V_APCM_quantization_i);
      (*-2.16667 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_APCM_quantization_i)) (F_check_ge (0) (0));
      (*0 0.822511*) F_binom_monotonic 1 (F_max0_ge_arg (12
                                                         - s V_APCM_quantization_i)) (F_check_ge (12
                                                                    - s V_APCM_quantization_i) (0));
      (*0 1.80952*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (6
                                                                    - 
                                                                    s V_APCM_quantization_i) (0))) (F_max0_ge_0 (6
                                                                    - s V_APCM_quantization_i))]
     ((76 # 77) - (76 # 77) * s V_APCM_quantization_i
      + s V_APCM_quantization_z
      + (19 # 14) * max0(6 - s V_APCM_quantization_i)
      + (51 # 62) * max0(12 - s V_APCM_quantization_i)
      + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 20 => ((76 # 77) - (76 # 77) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (51 # 62) * max0(12 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 21 => ((76 # 77) - (76 # 77) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (51 # 62) * max0(12 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 22 => hints
     [(*-3.28415e-12 1*) F_max0_monotonic (F_check_ge (6
                                                       - s V_APCM_quantization_i) (5
                                                                    - s V_APCM_quantization_i));
      (*0 2.16667*) F_binom_monotonic 1 (F_max0_ge_arg (s V_APCM_quantization_i)) (F_check_ge (s V_APCM_quantization_i) (0));
      (*0 0.164502*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (6
                                                                    - 
                                                                    s V_APCM_quantization_i) (0))) (F_max0_ge_0 (6
                                                                    - s V_APCM_quantization_i));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (5 - s V_APCM_quantization_i)) (F_check_ge (0) (0))]
     ((76 # 77) - (76 # 77) * s V_APCM_quantization_i
      + s V_APCM_quantization_z
      + (19 # 14) * max0(6 - s V_APCM_quantization_i)
      + (51 # 62) * max0(12 - s V_APCM_quantization_i)
      + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 23 => ((125 # 93) * s V_APCM_quantization_i + s V_APCM_quantization_z
            + (12 # 23) * max0(6 - s V_APCM_quantization_i)
            + (51 # 62) * max0(12 - s V_APCM_quantization_i) <= z)%Q
   | 24 => hints
     [(*-4.34553e-12 0.521645*) F_max0_monotonic (F_check_ge (6
                                                              - s V_APCM_quantization_i) (5
                                                                    - s V_APCM_quantization_i));
      (*-1.34416 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_APCM_quantization_i)) (F_check_ge (0) (0));
      (*0 1.34416*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_APCM_quantization_i) (0))) (F_max0_ge_0 (s V_APCM_quantization_i));
      (*0 0.822511*) F_binom_monotonic 1 (F_max0_ge_0 (12
                                                       - s V_APCM_quantization_i)) (F_check_ge (0) (0));
      (*-0.521645 0*) F_binom_monotonic 1 (F_max0_ge_0 (5
                                                        - s V_APCM_quantization_i)) (F_check_ge (0) (0))]
     ((125 # 93) * s V_APCM_quantization_i + s V_APCM_quantization_z
      + (12 # 23) * max0(6 - s V_APCM_quantization_i)
      + (51 # 62) * max0(12 - s V_APCM_quantization_i) <= z)%Q
   | 25 => hints
     [(*0 0.822511*) F_binom_monotonic 1 (F_max0_ge_arg (12
                                                         - s V_APCM_quantization_i)) (F_check_ge (12
                                                                    - s V_APCM_quantization_i) (0));
      (*-0.521645 0*) F_binom_monotonic 1 (F_max0_ge_arg (6
                                                          - s V_APCM_quantization_i)) (F_check_ge (6
                                                                    - s V_APCM_quantization_i) (0))]
     ((125 # 93) * s V_APCM_quantization_i + s V_APCM_quantization_z
      + (12 # 23) * max0(6 - s V_APCM_quantization_i)
      + (51 # 62) * max0(12 - s V_APCM_quantization_i) <= z)%Q
   | 26 => ((13 # 1) + s V_APCM_quantization_z <= z)%Q
   | 27 => hints
     [(*-13 0*) F_one]
     ((13 # 1) + s V_APCM_quantization_z <= z)%Q
   | 28 => (s V_APCM_quantization_z <= z)%Q
   | 29 => (s V_APCM_quantization_z <= z)%Q
   | 30 => ((13 # 1) + s V_APCM_quantization_z <= z)%Q
   | 31 => ((13 # 1) + s V_APCM_quantization_z <= z)%Q
   | 32 => ((13 # 1) + s V_APCM_quantization_z <= z)%Q
   | 33 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_APCM_quantization_z) (0))) (F_max0_ge_0 (s V_APCM_quantization_z))]
     ((13 # 1) + s V_APCM_quantization_z <= z)%Q
   | 34 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 35 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 36 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 37 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 38 => hints
     [(*-13 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_APCM_quantization_z)) (F_check_ge (s V_APCM_quantization_z) (0))]
     ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 39 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 40 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 41 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 42 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 43 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 44 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 45 => hints
     [(*-13 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_APCM_quantization_z)) (F_check_ge (s V_APCM_quantization_z) (0))]
     ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 46 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 47 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 48 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 49 => ((13 # 1) + max0(s V_APCM_quantization_z) <= z)%Q
   | 50 => (max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 51 => (max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 52 => (max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 53 => (max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 54 => (max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 55 => (max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 56 => (max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 57 => (max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 58 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (13 - s V_APCM_quantization_i) (12
                                                                    - s V_APCM_quantization_i));
      (*-1 0*) F_max0_ge_0 (12 - s V_APCM_quantization_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_APCM_quantization_z)) (F_check_ge (s V_APCM_quantization_z) (0))]
     (max0(13 - s V_APCM_quantization_i) + max0(s V_APCM_quantization_z) <= z)%Q
   | 59 => (max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 60 => (max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 61 => hints
     [(*-0.0833333 1*) F_max0_pre_decrement 1 (13 - s V_APCM_quantization_i) (1);
      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_APCM_quantization_i)) (F_check_ge (0) (0));
      (*0 0.0833333*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_APCM_quantization_i) (0))) (F_max0_ge_0 (s V_APCM_quantization_i));
      (*-1.08333 0*) F_binom_monotonic 1 (F_max0_ge_0 (12
                                                       - s V_APCM_quantization_i)) (F_check_ge (0) (0))]
     (max0(13 - s V_APCM_quantization_i) + max0(s V_APCM_quantization_z) <= z)%Q
   | 62 => hints
     [(*0 1*) F_max0_pre_decrement 1 (13 - s V_APCM_quantization_i) (1)]
     (max0(13 - s V_APCM_quantization_i) + max0(s V_APCM_quantization_z) <= z)%Q
   | 63 => ((1 # 1) + max0(12 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 64 => hints
     [(*-0.0833333 0*) F_max0_pre_decrement 1 (13 - s V_APCM_quantization_i) (1);
      (*-1.08333 0*) F_max0_ge_0 (12 - s V_APCM_quantization_i);
      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_APCM_quantization_i)) (F_check_ge (0) (0));
      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_APCM_quantization_i) (0))) (F_max0_ge_0 (s V_APCM_quantization_i))]
     ((1 # 1) + max0(12 - s V_APCM_quantization_i)
      + max0(s V_APCM_quantization_z) <= z)%Q
   | 65 => ((13 # 12) - (1 # 12) * s V_APCM_quantization_i
            - (1 # 12) * max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 66 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_APCM_quantization_z)) (F_check_ge (s V_APCM_quantization_z) (0));
      (*-0.0833333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                                    - s V_APCM_quantization_i) (0))) (F_max0_ge_0 (13
                                                                    - s V_APCM_quantization_i))]
     ((13 # 12) - (1 # 12) * s V_APCM_quantization_i
      - (1 # 12) * max0(13 - s V_APCM_quantization_i)
      + max0(s V_APCM_quantization_z) <= z)%Q
   | 67 => ((1 # 1) + max0(12 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 68 => ((1 # 1) + max0(12 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 69 => ((1 # 1) + max0(12 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 70 => ((1 # 1) + max0(12 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 71 => ((1 # 1) + max0(12 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 72 => ((1 # 1) + max0(12 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 73 => ((1 # 1) + max0(12 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 74 => ((1 # 1) + max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 75 => ((1 # 1) + max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 76 => ((1 # 1) + max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 77 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_APCM_quantization_z) (0))) (F_max0_ge_0 (s V_APCM_quantization_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                  + s V_APCM_quantization_z)) (F_check_ge (-1
                                                                    + s V_APCM_quantization_z) (0))]
     ((1 # 1) + max0(-1 + s V_APCM_quantization_z)
      + max0(13 - s V_APCM_quantization_i) <= z)%Q
   | 78 => ((209 # 42) - (19 # 14) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (19 # 42) * max0(13 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 79 => ((209 # 42) - (19 # 14) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (19 # 42) * max0(13 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 80 => ((209 # 42) - (19 # 14) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (19 # 42) * max0(13 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 81 => hints
     [(*-1.35714 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                    - 
                                                                    s V_APCM_quantization_i) (0))) (F_max0_ge_0 (5
                                                                    - s V_APCM_quantization_i))]
     ((209 # 42) - (19 # 14) * s V_APCM_quantization_i
      + s V_APCM_quantization_z
      + (19 # 14) * max0(6 - s V_APCM_quantization_i)
      + (19 # 42) * max0(13 - s V_APCM_quantization_i)
      + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 82 => (-(38 # 21) + s V_APCM_quantization_z
            + (19 # 14) * max0(5 - s V_APCM_quantization_i)
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (19 # 42) * max0(13 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 83 => hints
     [(*-1.35714 0*) F_max0_pre_decrement 1 (6 - s V_APCM_quantization_i) (1);
      (*-0.452381 0*) F_max0_pre_decrement 1 (13 - s V_APCM_quantization_i) (1);
      (*-0.452381 0*) F_max0_ge_0 (12 - s V_APCM_quantization_i);
      (*-2.16667 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_APCM_quantization_i)) (F_check_ge (0) (0));
      (*-2.71429 0*) F_binom_monotonic 1 (F_max0_ge_0 (5
                                                       - s V_APCM_quantization_i)) (F_check_ge (0) (0))]
     (-(38 # 21) + s V_APCM_quantization_z
      + (19 # 14) * max0(5 - s V_APCM_quantization_i)
      + (19 # 14) * max0(6 - s V_APCM_quantization_i)
      + (19 # 42) * max0(13 - s V_APCM_quantization_i)
      + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 84 => (s V_APCM_quantization_z <= z)%Q
   | 85 => (-(38 # 21) + s V_APCM_quantization_z
            + (19 # 14) * max0(5 - s V_APCM_quantization_i)
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (19 # 42) * max0(13 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 86 => hints
     [(*-1.35714 0*) F_binom_monotonic 1 (F_max0_ge_arg (6
                                                         - s V_APCM_quantization_i)) (F_check_ge (6
                                                                    - s V_APCM_quantization_i) (0))]
     (-(38 # 21) + s V_APCM_quantization_z
      + (19 # 14) * max0(5 - s V_APCM_quantization_i)
      + (19 # 14) * max0(6 - s V_APCM_quantization_i)
      + (19 # 42) * max0(13 - s V_APCM_quantization_i)
      + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 87 => ((19 # 3) - (19 # 14) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(5 - s V_APCM_quantization_i)
            + (19 # 42) * max0(13 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 88 => hints
     [(*-0.452381 0*) F_max0_pre_decrement 1 (13 - s V_APCM_quantization_i) (1)]
     ((19 # 3) - (19 # 14) * s V_APCM_quantization_i
      + s V_APCM_quantization_z
      + (19 # 14) * max0(5 - s V_APCM_quantization_i)
      + (19 # 42) * max0(13 - s V_APCM_quantization_i)
      + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 89 => hints
     [(*-0.452381 0*) F_max0_pre_decrement 1 (13 - s V_APCM_quantization_i) (1)]
     ((19 # 3) - (19 # 14) * s V_APCM_quantization_i
      + s V_APCM_quantization_z
      + (19 # 14) * max0(5 - s V_APCM_quantization_i)
      + (19 # 42) * max0(13 - s V_APCM_quantization_i)
      + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 90 => ((95 # 14) - (19 # 14) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(5 - s V_APCM_quantization_i)
            + (19 # 42) * max0(12 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 91 => ((95 # 14) - (19 # 14) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(5 - s V_APCM_quantization_i)
            + (19 # 42) * max0(12 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 92 => ((95 # 14) - (19 # 14) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (19 # 14) * max0(5 - s V_APCM_quantization_i)
            + (19 # 42) * max0(12 - s V_APCM_quantization_i)
            + (13 # 6) * max0(s V_APCM_quantization_i) <= z)%Q
   | 93 => ((57 # 7) - (19 # 14) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (13 # 6) * max0(-1 + s V_APCM_quantization_i)
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (19 # 42) * max0(13 - s V_APCM_quantization_i) <= z)%Q
   | 94 => ((57 # 7) - (19 # 14) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (13 # 6) * max0(-1 + s V_APCM_quantization_i)
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (19 # 42) * max0(13 - s V_APCM_quantization_i) <= z)%Q
   | 95 => ((57 # 7) - (19 # 14) * s V_APCM_quantization_i
            + s V_APCM_quantization_z
            + (13 # 6) * max0(-1 + s V_APCM_quantization_i)
            + (19 # 14) * max0(6 - s V_APCM_quantization_i)
            + (19 # 42) * max0(13 - s V_APCM_quantization_i) <= z)%Q
   | 96 => hints
     [(*-2.16667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_APCM_quantization_i) (0))) (F_max0_ge_0 (s V_APCM_quantization_i));
      (*-2.16667 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                         + s V_APCM_quantization_i)) (F_check_ge (-1
                                                                    + s V_APCM_quantization_i) (0))]
     ((50 # 7) - (19 # 14) * s V_APCM_quantization_i
      + s V_APCM_quantization_z
      + (13 # 6) * max0(-1 + s V_APCM_quantization_i)
      + (19 # 14) * max0(6 - s V_APCM_quantization_i)
      + (19 # 42) * max0(13 - s V_APCM_quantization_i) <= z)%Q
   | 97 => ((19 # 1) + max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 98 => ((19 # 1) + max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 99 => ((19 # 1) + max0(13 - s V_APCM_quantization_i)
            + max0(s V_APCM_quantization_z) <= z)%Q
   | 100 => ((19 # 1) + max0(13 - s V_APCM_quantization_i)
             + max0(s V_APCM_quantization_z) <= z)%Q
   | 101 => ((19 # 1) + max0(13 - s V_APCM_quantization_i)
             + max0(s V_APCM_quantization_z) <= z)%Q
   | 102 => ((19 # 1) + max0(13 - s V_APCM_quantization_i)
             + max0(s V_APCM_quantization_z) <= z)%Q
   | 103 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (13
                                                  - s V_APCM_quantization_i)) (F_check_ge (13
                                                                    - s V_APCM_quantization_i) (0))]
     ((19 # 1) + max0(13 - s V_APCM_quantization_i)
      + max0(s V_APCM_quantization_z) <= z)%Q
   | 104 => ((32 # 1) - s V_APCM_quantization_i
             + max0(s V_APCM_quantization_z) <= z)%Q
   | 105 => ((32 # 1) - s V_APCM_quantization_i
             + max0(s V_APCM_quantization_z) <= z)%Q
   | 106 => ((32 # 1) - s V_APCM_quantization_i
             + max0(s V_APCM_quantization_z) <= z)%Q
   | 107 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                               - s V_APCM_quantization_i) (0))) (F_max0_ge_0 (13
                                                                    - s V_APCM_quantization_i))]
     ((32 # 1) - s V_APCM_quantization_i + max0(s V_APCM_quantization_z) <= z)%Q
   | 108 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                               - s V_APCM_quantization_i) (0))) (F_max0_ge_0 (13
                                                                    - s V_APCM_quantization_i))]
     ((32 # 1) - s V_APCM_quantization_i + max0(s V_APCM_quantization_z) <= z)%Q
   | 109 => ((19 # 1) + max0(13 - s V_APCM_quantization_i)
             + max0(s V_APCM_quantization_z) <= z)%Q
   | 110 => ((19 # 1) + max0(13 - s V_APCM_quantization_i)
             + max0(s V_APCM_quantization_z) <= z)%Q
   | 111 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (13
                                                  - s V_APCM_quantization_i)) (F_check_ge (13
                                                                    - s V_APCM_quantization_i) (0))]
     ((19 # 1) + max0(13 - s V_APCM_quantization_i)
      + max0(s V_APCM_quantization_z) <= z)%Q
   | 112 => ((32 # 1) - s V_APCM_quantization_i
             + max0(s V_APCM_quantization_z) <= z)%Q
   | 113 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_APCM_quantization_z)) (F_check_ge (s V_APCM_quantization_z) (0))]
     ((32 # 1) - s V_APCM_quantization_i + max0(s V_APCM_quantization_z) <= z)%Q
   | 114 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_APCM_quantization_z)) (F_check_ge (s V_APCM_quantization_z) (0))]
     ((32 # 1) - s V_APCM_quantization_i + max0(s V_APCM_quantization_z) <= z)%Q
   | 115 => ((32 # 1) - s V_APCM_quantization_i + s V_APCM_quantization_z <= z)%Q
   | 116 => ((32 # 1) - s V_APCM_quantization_i + s V_APCM_quantization_z <= z)%Q
   | 117 => ((32 # 1) - s V_APCM_quantization_i + s V_APCM_quantization_z <= z)%Q
   | 118 => ((32 # 1) - s V_APCM_quantization_i + s V_APCM_quantization_z <= z)%Q
   | 119 => ((33 # 1) - s V_APCM_quantization_i + s V_APCM_quantization_z <= z)%Q
   | 120 => ((33 # 1) - s V_APCM_quantization_i + s V_APCM_quantization_z <= z)%Q
   | 121 => ((33 # 1) - s V_APCM_quantization_i + s V_APCM_quantization_z <= z)%Q
   | 122 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_APCM_quantization_z) (0))) (F_max0_ge_0 (s V_APCM_quantization_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (13
                                                               - s V_APCM_quantization_i) (0))) (F_max0_ge_0 (13
                                                                    - s V_APCM_quantization_i))]
     ((32 # 1) - s V_APCM_quantization_i + s V_APCM_quantization_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_APCM_quantization =>
    [mkPA Q (fun n z s => ai_APCM_quantization n s /\ annot0_APCM_quantization n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_APCM_quantization (proc_start P_APCM_quantization) s1 (proc_end P_APCM_quantization) s2 ->
    (s2 V_APCM_quantization_z <= (32 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_APCM_quantization.
Qed.
