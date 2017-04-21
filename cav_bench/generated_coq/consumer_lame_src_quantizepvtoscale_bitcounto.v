Require Import pasta.Pasta.

Inductive proc: Type :=
  P_scale_bitcount.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_scale_bitcount_z := 1%positive.
Notation V_scale_bitcount_cod_info_dref_off16 := 2%positive.
Notation V_scale_bitcount_cod_info_dref_off24 := 3%positive.
Notation V_scale_bitcount_cod_info_dref_off64 := 4%positive.
Notation V_scale_bitcount_cod_info_dref_off76 := 5%positive.
Notation V_scale_bitcount_ep := 6%positive.
Notation V_scale_bitcount_i := 7%positive.
Notation V_scale_bitcount_k := 8%positive.
Notation V_scale_bitcount_max_slen1 := 9%positive.
Notation V_scale_bitcount_max_slen2 := 10%positive.
Notation V_scale_bitcount_sfb := 11%positive.
Notation V_scale_bitcount_cod_info := 12%positive.
Notation V_scale_bitcount_scalefac := 13%positive.
Definition Pedges_scale_bitcount: list (edge proc) :=
  (EA 1 (AAssign V_scale_bitcount_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_scale_bitcount_max_slen1 (Some (ENum (0)))) 3)::(EA 3 (AAssign
  V_scale_bitcount_max_slen2 (Some (ENum (0)))) 4)::(EA 4 (AAssign
  V_scale_bitcount_ep (Some (ENum (2)))) 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_cod_info_dref_off24) s) =
  (eval (ENum (2)) s))%Z)) 75)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_cod_info_dref_off24) s) <>
  (eval (ENum (2)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_scale_bitcount_sfb (Some (ENum (0)))) 9)::(EA 9 ANone 10)::
  (EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_sfb) s) < (eval (ENum (11))
  s))%Z)) 65)::(EA 11 (AGuard (fun s => ((eval (EVar V_scale_bitcount_sfb)
  s) >= (eval (ENum (11)) s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_cod_info_dref_off64) s) <>
  (eval (ENum (0)) s))%Z)) 48)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_cod_info_dref_off64) s) =
  (eval (ENum (0)) s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 (AAssign
  V_scale_bitcount_sfb (Some (ENum (11)))) 16)::(EA 16 ANone 17)::
  (EA 17 AWeaken 18)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_sfb) s) < (eval (ENum (21))
  s))%Z)) 20)::(EA 18 (AGuard (fun s => ((eval (EVar V_scale_bitcount_sfb)
  s) >= (eval (ENum (21)) s))%Z)) 19)::(EA 19 AWeaken 30)::
  (EA 20 AWeaken 21)::(EA 21 ANone 28)::(EA 21 ANone 22)::(EA 22 ANone 23)::
  (EA 23 (AAssign V_scale_bitcount_sfb
  (Some (EAdd (EVar V_scale_bitcount_sfb) (ENum (1))))) 24)::
  (EA 24 ANone 25)::(EA 25 ANone 26)::(EA 26 (AAssign V_scale_bitcount_z
  (Some (EAdd (ENum (1)) (EVar V_scale_bitcount_z)))) 27)::
  (EA 27 AWeaken 18)::(EA 28 ANone 29)::(EA 29 AWeaken 30)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_sfb) s) = (eval (ENum (21))
  s))%Z)) 32)::(EA 30 (AGuard (fun s => ((eval (EVar V_scale_bitcount_sfb)
  s) <> (eval (ENum (21)) s))%Z)) 31)::(EA 31 AWeaken 40)::
  (EA 32 AWeaken 33)::(EA 33 (AAssign V_scale_bitcount_cod_info_dref_off64
  (Some (ENum (1)))) 34)::(EA 34 (AAssign V_scale_bitcount_sfb
  (Some (ENum (11)))) 35)::(EA 35 ANone 36)::(EA 36 AWeaken 37)::
  (EA 37 (AGuard (fun s => ((eval (EVar V_scale_bitcount_sfb) s) <
  (eval (ENum (21)) s))%Z)) 41)::(EA 37 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_sfb) s) >= (eval (ENum (21))
  s))%Z)) 38)::(EA 38 AWeaken 39)::(EA 39 ANone 40)::(EA 40 ANone 49)::
  (EA 41 AWeaken 42)::(EA 42 ANone 43)::(EA 43 (AAssign V_scale_bitcount_sfb
  (Some (EAdd (EVar V_scale_bitcount_sfb) (ENum (1))))) 44)::
  (EA 44 ANone 45)::(EA 45 ANone 46)::(EA 46 (AAssign V_scale_bitcount_z
  (Some (EAdd (ENum (1)) (EVar V_scale_bitcount_z)))) 47)::
  (EA 47 AWeaken 37)::(EA 48 AWeaken 49)::(EA 49 (AAssign
  V_scale_bitcount_sfb (Some (ENum (11)))) 50)::(EA 50 ANone 51)::
  (EA 51 AWeaken 52)::(EA 52 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_sfb) s) < (eval (ENum (21))
  s))%Z)) 55)::(EA 52 (AGuard (fun s => ((eval (EVar V_scale_bitcount_sfb)
  s) >= (eval (ENum (21)) s))%Z)) 53)::(EA 53 AWeaken 54)::(EA 54 ANone 82)::
  (EA 55 AWeaken 56)::(EA 56 ANone 57)::(EA 56 ANone 59)::(EA 57 (AAssign
  V_scale_bitcount_max_slen2 None) 58)::(EA 58 ANone 59)::(EA 59 ANone 60)::
  (EA 60 (AAssign V_scale_bitcount_sfb
  (Some (EAdd (EVar V_scale_bitcount_sfb) (ENum (1))))) 61)::
  (EA 61 ANone 62)::(EA 62 ANone 63)::(EA 63 (AAssign V_scale_bitcount_z
  (Some (EAdd (ENum (1)) (EVar V_scale_bitcount_z)))) 64)::
  (EA 64 AWeaken 52)::(EA 65 AWeaken 66)::(EA 66 ANone 67)::
  (EA 66 ANone 69)::(EA 67 (AAssign V_scale_bitcount_max_slen1 None) 68)::
  (EA 68 ANone 69)::(EA 69 ANone 70)::(EA 70 (AAssign V_scale_bitcount_sfb
  (Some (EAdd (EVar V_scale_bitcount_sfb) (ENum (1))))) 71)::
  (EA 71 ANone 72)::(EA 72 ANone 73)::(EA 73 (AAssign V_scale_bitcount_z
  (Some (EAdd (ENum (1)) (EVar V_scale_bitcount_z)))) 74)::
  (EA 74 AWeaken 11)::(EA 75 AWeaken 76)::(EA 76 (AAssign V_scale_bitcount_i
  (Some (ENum (0)))) 77)::(EA 77 ANone 78)::(EA 78 AWeaken 79)::
  (EA 79 (AGuard (fun s => ((eval (EVar V_scale_bitcount_i) s) <
  (eval (ENum (3)) s))%Z)) 105)::(EA 79 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_i) s) >= (eval (ENum (3))
  s))%Z)) 80)::(EA 80 AWeaken 81)::(EA 81 ANone 82)::(EA 82 (AAssign
  V_scale_bitcount_cod_info_dref_off76 None) 83)::(EA 83 (AAssign
  V_scale_bitcount_k (Some (ENum (0)))) 84)::(EA 84 ANone 85)::
  (EA 85 AWeaken 86)::(EA 86 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_k) s) < (eval (ENum (16))
  s))%Z)) 89)::(EA 86 (AGuard (fun s => ((eval (EVar V_scale_bitcount_k)
  s) >= (eval (ENum (16)) s))%Z)) 87)::(EA 87 AWeaken 88)::
  (EA 89 AWeaken 90)::(EA 90 ANone 91)::(EA 90 ANone 99)::
  (EA 91 AWeaken 92)::(EA 92 ANone 93)::(EA 92 ANone 99)::
  (EA 93 AWeaken 94)::(EA 94 ANone 95)::(EA 94 ANone 99)::(EA 95 (AAssign
  V_scale_bitcount_cod_info_dref_off76 None) 96)::(EA 96 (AAssign
  V_scale_bitcount_cod_info_dref_off16
  (Some (EVar V_scale_bitcount_k))) 97)::(EA 97 (AAssign V_scale_bitcount_ep
  (Some (ENum (0)))) 98)::(EA 98 ANone 99)::(EA 99 ANone 100)::
  (EA 100 (AAssign V_scale_bitcount_k (Some (EAdd (EVar V_scale_bitcount_k)
  (ENum (1))))) 101)::(EA 101 ANone 102)::(EA 102 ANone 103)::
  (EA 103 (AAssign V_scale_bitcount_z (Some (EAdd (ENum (1))
  (EVar V_scale_bitcount_z)))) 104)::(EA 104 AWeaken 86)::
  (EA 105 AWeaken 106)::(EA 106 (AAssign V_scale_bitcount_sfb
  (Some (ENum (0)))) 107)::(EA 107 ANone 108)::(EA 108 AWeaken 109)::
  (EA 109 (AGuard (fun s => ((eval (EVar V_scale_bitcount_sfb) s) <
  (eval (ENum (6)) s))%Z)) 132)::(EA 109 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_sfb) s) >= (eval (ENum (6))
  s))%Z)) 110)::(EA 110 AWeaken 111)::(EA 111 (AAssign V_scale_bitcount_sfb
  (Some (ENum (6)))) 112)::(EA 112 ANone 113)::(EA 113 AWeaken 114)::
  (EA 114 (AGuard (fun s => ((eval (EVar V_scale_bitcount_sfb) s) <
  (eval (ENum (12)) s))%Z)) 122)::(EA 114 (AGuard
  (fun s => ((eval (EVar V_scale_bitcount_sfb) s) >= (eval (ENum (12))
  s))%Z)) 115)::(EA 115 AWeaken 116)::(EA 116 ANone 117)::(EA 117 (AAssign
  V_scale_bitcount_i (Some (EAdd (EVar V_scale_bitcount_i)
  (ENum (1))))) 118)::(EA 118 ANone 119)::(EA 119 ANone 120)::
  (EA 120 (AAssign V_scale_bitcount_z (Some (EAdd (ENum (1))
  (EVar V_scale_bitcount_z)))) 121)::(EA 121 AWeaken 79)::
  (EA 122 AWeaken 123)::(EA 123 ANone 124)::(EA 123 ANone 126)::
  (EA 124 (AAssign V_scale_bitcount_max_slen2 None) 125)::
  (EA 125 ANone 126)::(EA 126 ANone 127)::(EA 127 (AAssign
  V_scale_bitcount_sfb (Some (EAdd (EVar V_scale_bitcount_sfb)
  (ENum (1))))) 128)::(EA 128 ANone 129)::(EA 129 ANone 130)::
  (EA 130 (AAssign V_scale_bitcount_z (Some (EAdd (ENum (1))
  (EVar V_scale_bitcount_z)))) 131)::(EA 131 AWeaken 114)::
  (EA 132 AWeaken 133)::(EA 133 ANone 134)::(EA 133 ANone 136)::
  (EA 134 (AAssign V_scale_bitcount_max_slen1 None) 135)::
  (EA 135 ANone 136)::(EA 136 ANone 137)::(EA 137 (AAssign
  V_scale_bitcount_sfb (Some (EAdd (EVar V_scale_bitcount_sfb)
  (ENum (1))))) 138)::(EA 138 ANone 139)::(EA 139 ANone 140)::
  (EA 140 (AAssign V_scale_bitcount_z (Some (EAdd (ENum (1))
  (EVar V_scale_bitcount_z)))) 141)::(EA 141 AWeaken 109)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_scale_bitcount => Pedges_scale_bitcount
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_scale_bitcount => 88
     end)%positive;
  var_global := var_global
}.

Definition ai_scale_bitcount (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_z <= 0)%Z
   | 3 => (-1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_max_slen1 <= 0 /\ -1 * s V_scale_bitcount_max_slen1 <= 0)%Z
   | 4 => (-1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 5 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_max_slen1 <= 0 /\ -1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0)%Z
   | 6 => (-1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 7 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_max_slen1 <= 0 /\ -1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0)%Z
   | 8 => (-1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 9 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_max_slen1 <= 0 /\ -1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0)%Z
   | 10 => (-1 * s V_scale_bitcount_sfb <= 0 /\ 1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 11 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0)%Z
   | 12 => (1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0)%Z
   | 13 => (-1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0)%Z
   | 14 => (1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0)%Z
   | 15 => (-1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0)%Z
   | 16 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0)%Z
   | 17 => (-1 * s V_scale_bitcount_sfb + 11 <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 18 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0)%Z
   | 19 => (1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 21 <= 0)%Z
   | 20 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -20 <= 0)%Z
   | 21 => (1 * s V_scale_bitcount_sfb + -20 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 22 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -20 <= 0)%Z
   | 23 => (1 * s V_scale_bitcount_sfb + -20 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 24 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_sfb + 12 <= 0)%Z
   | 25 => (-1 * s V_scale_bitcount_sfb + 12 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 26 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_sfb + 12 <= 0)%Z
   | 27 => (-1 * s V_scale_bitcount_sfb + 12 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_z + 1 <= 0)%Z
   | 28 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -20 <= 0)%Z
   | 29 => (1 * s V_scale_bitcount_sfb + -20 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 30 => (1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0)%Z
   | 31 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -20 <= 0)%Z
   | 32 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_sfb + 21 <= 0)%Z
   | 33 => (-1 * s V_scale_bitcount_sfb + 21 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_z <= 0)%Z
   | 34 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_sfb + 21 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0)%Z
   | 35 => (-1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0)%Z
   | 36 => (-1 * s V_scale_bitcount_sfb + 11 <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0)%Z
   | 37 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0)%Z
   | 38 => (1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 21 <= 0)%Z
   | 39 => (-1 * s V_scale_bitcount_sfb + 21 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0)%Z
   | 40 => (-1 * s V_scale_bitcount_cod_info_dref_off64 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_z <= 0)%Z
   | 41 => (-1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -20 <= 0)%Z
   | 42 => (1 * s V_scale_bitcount_sfb + -20 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0)%Z
   | 43 => (-1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -20 <= 0)%Z
   | 44 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_sfb + 12 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0)%Z
   | 45 => (1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_sfb + 12 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0 /\ -1 * s V_scale_bitcount_z <= 0)%Z
   | 46 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_sfb + 12 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0)%Z
   | 47 => (1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_sfb + 12 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off64 + -1 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off64 + 1 <= 0 /\ -1 * s V_scale_bitcount_z + 1 <= 0)%Z
   | 48 => (1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0)%Z
   | 49 => (1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 50 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0)%Z
   | 51 => (-1 * s V_scale_bitcount_sfb + 11 <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 52 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0)%Z
   | 53 => (1 * s V_scale_bitcount_sfb + -21 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 21 <= 0)%Z
   | 54 => (-1 * s V_scale_bitcount_sfb + 21 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0)%Z
   | 55 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -20 <= 0)%Z
   | 56 => (1 * s V_scale_bitcount_sfb + -20 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 57 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -20 <= 0)%Z
   | 58 => (1 * s V_scale_bitcount_sfb + -20 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 59 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -20 <= 0)%Z
   | 60 => (1 * s V_scale_bitcount_sfb + -20 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 11 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 61 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_sfb + 12 <= 0)%Z
   | 62 => (-1 * s V_scale_bitcount_sfb + 12 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 63 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_sfb + 12 <= 0)%Z
   | 64 => (-1 * s V_scale_bitcount_sfb + 12 <= 0 /\ 1 * s V_scale_bitcount_sfb + -21 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_z + 1 <= 0)%Z
   | 65 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -10 <= 0)%Z
   | 66 => (1 * s V_scale_bitcount_sfb + -10 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 67 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -10 <= 0)%Z
   | 68 => (1 * s V_scale_bitcount_sfb + -10 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 69 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -10 <= 0)%Z
   | 70 => (1 * s V_scale_bitcount_sfb + -10 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 71 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_sfb + 1 <= 0)%Z
   | 72 => (-1 * s V_scale_bitcount_sfb + 1 <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 73 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_sfb + 1 <= 0)%Z
   | 74 => (-1 * s V_scale_bitcount_sfb + 1 <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_z + 1 <= 0)%Z
   | 75 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_max_slen1 <= 0 /\ -1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0)%Z
   | 76 => (-1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 77 => (-1 * s V_scale_bitcount_max_slen2 <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_max_slen1 <= 0 /\ -1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_i <= 0)%Z
   | 78 => (-1 * s V_scale_bitcount_i <= 0 /\ 1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_max_slen1 <= 0 /\ 1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_max_slen2 <= 0 /\ -1 * s V_scale_bitcount_max_slen2 <= 0)%Z
   | 79 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0)%Z
   | 80 => (-1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_i + 3 <= 0)%Z
   | 81 => (-1 * s V_scale_bitcount_i + 3 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0)%Z
   | 82 => (-1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0)%Z
   | 83 => (-1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0)%Z
   | 84 => (-1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_k <= 0 /\ -1 * s V_scale_bitcount_k <= 0)%Z
   | 85 => (-1 * s V_scale_bitcount_k <= 0 /\ 1 * s V_scale_bitcount_k <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0)%Z
   | 86 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_k + -16 <= 0)%Z
   | 87 => (1 * s V_scale_bitcount_k + -16 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_k + 16 <= 0)%Z
   | 88 => (-1 * s V_scale_bitcount_k + 16 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_k + -16 <= 0)%Z
   | 89 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_k + -15 <= 0)%Z
   | 90 => (1 * s V_scale_bitcount_k + -15 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 91 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_k + -15 <= 0)%Z
   | 92 => (1 * s V_scale_bitcount_k + -15 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 93 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_k + -15 <= 0)%Z
   | 94 => (1 * s V_scale_bitcount_k + -15 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 95 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_k + -15 <= 0)%Z
   | 96 => (1 * s V_scale_bitcount_k + -15 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 97 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_k + -15 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off16 + -15 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off16 <= 0)%Z
   | 98 => (-1 * s V_scale_bitcount_cod_info_dref_off16 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off16 + -15 <= 0 /\ 1 * s V_scale_bitcount_k + -15 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ 1 * s V_scale_bitcount_ep <= 0 /\ -1 * s V_scale_bitcount_ep <= 0)%Z
   | 99 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_k + -15 <= 0)%Z
   | 100 => (1 * s V_scale_bitcount_k + -15 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_k <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 101 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_k + -16 <= 0 /\ -1 * s V_scale_bitcount_k + 1 <= 0)%Z
   | 102 => (-1 * s V_scale_bitcount_k + 1 <= 0 /\ 1 * s V_scale_bitcount_k + -16 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 103 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_k + -16 <= 0 /\ -1 * s V_scale_bitcount_k + 1 <= 0)%Z
   | 104 => (-1 * s V_scale_bitcount_k + 1 <= 0 /\ 1 * s V_scale_bitcount_k + -16 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_z + 1 <= 0)%Z
   | 105 => (-1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_i + -2 <= 0)%Z
   | 106 => (1 * s V_scale_bitcount_i + -2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0)%Z
   | 107 => (-1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_i + -2 <= 0 /\ 1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0)%Z
   | 108 => (-1 * s V_scale_bitcount_sfb <= 0 /\ 1 * s V_scale_bitcount_sfb <= 0 /\ 1 * s V_scale_bitcount_i + -2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0)%Z
   | 109 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -6 <= 0)%Z
   | 110 => (1 * s V_scale_bitcount_sfb + -6 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 6 <= 0)%Z
   | 111 => (-1 * s V_scale_bitcount_sfb + 6 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -6 <= 0)%Z
   | 112 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -6 <= 0 /\ -1 * s V_scale_bitcount_sfb + 6 <= 0)%Z
   | 113 => (-1 * s V_scale_bitcount_sfb + 6 <= 0 /\ 1 * s V_scale_bitcount_sfb + -6 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 114 => (-1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 6 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -12 <= 0)%Z
   | 115 => (1 * s V_scale_bitcount_sfb + -12 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 12 <= 0)%Z
   | 116 => (-1 * s V_scale_bitcount_sfb + 12 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -12 <= 0)%Z
   | 117 => (1 * s V_scale_bitcount_sfb + -12 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 12 <= 0)%Z
   | 118 => (-1 * s V_scale_bitcount_sfb + 12 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -12 <= 0 /\ -1 * s V_scale_bitcount_i + 1 <= 0)%Z
   | 119 => (-1 * s V_scale_bitcount_i + 1 <= 0 /\ 1 * s V_scale_bitcount_sfb + -12 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 12 <= 0)%Z
   | 120 => (-1 * s V_scale_bitcount_sfb + 12 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ 1 * s V_scale_bitcount_sfb + -12 <= 0 /\ -1 * s V_scale_bitcount_i + 1 <= 0)%Z
   | 121 => (-1 * s V_scale_bitcount_i + 1 <= 0 /\ 1 * s V_scale_bitcount_sfb + -12 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_sfb + 12 <= 0 /\ -1 * s V_scale_bitcount_z + 1 <= 0)%Z
   | 122 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_sfb + 6 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0)%Z
   | 123 => (1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 6 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 124 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_sfb + 6 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0)%Z
   | 125 => (1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 6 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 126 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_sfb + 6 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -11 <= 0)%Z
   | 127 => (1 * s V_scale_bitcount_sfb + -11 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb + 6 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 128 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -12 <= 0 /\ -1 * s V_scale_bitcount_sfb + 7 <= 0)%Z
   | 129 => (-1 * s V_scale_bitcount_sfb + 7 <= 0 /\ 1 * s V_scale_bitcount_sfb + -12 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 130 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -12 <= 0 /\ -1 * s V_scale_bitcount_sfb + 7 <= 0)%Z
   | 131 => (-1 * s V_scale_bitcount_sfb + 7 <= 0 /\ 1 * s V_scale_bitcount_sfb + -12 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_z + 1 <= 0)%Z
   | 132 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -5 <= 0)%Z
   | 133 => (1 * s V_scale_bitcount_sfb + -5 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 134 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -5 <= 0)%Z
   | 135 => (1 * s V_scale_bitcount_sfb + -5 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 136 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -5 <= 0)%Z
   | 137 => (1 * s V_scale_bitcount_sfb + -5 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_sfb <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 138 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -6 <= 0 /\ -1 * s V_scale_bitcount_sfb + 1 <= 0)%Z
   | 139 => (-1 * s V_scale_bitcount_sfb + 1 <= 0 /\ 1 * s V_scale_bitcount_sfb + -6 <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0)%Z
   | 140 => (1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_z <= 0 /\ 1 * s V_scale_bitcount_sfb + -6 <= 0 /\ -1 * s V_scale_bitcount_sfb + 1 <= 0)%Z
   | 141 => (-1 * s V_scale_bitcount_sfb + 1 <= 0 /\ 1 * s V_scale_bitcount_sfb + -6 <= 0 /\ -1 * s V_scale_bitcount_i <= 0 /\ -1 * s V_scale_bitcount_cod_info_dref_off24 + 2 <= 0 /\ 1 * s V_scale_bitcount_cod_info_dref_off24 + -2 <= 0 /\ -1 * s V_scale_bitcount_ep + 2 <= 0 /\ 1 * s V_scale_bitcount_ep + -2 <= 0 /\ -1 * s V_scale_bitcount_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_scale_bitcount (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((55 # 1)
           + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64) <= z)%Q
   | 2 => ((55 # 1) + s V_scale_bitcount_z
           + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64) <= z)%Q
   | 3 => ((55 # 1) + s V_scale_bitcount_z
           + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64) <= z)%Q
   | 4 => ((55 # 1) + s V_scale_bitcount_z
           + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64) <= z)%Q
   | 5 => ((10 # 1) + (45 # 2) * s V_scale_bitcount_ep + s V_scale_bitcount_z
           + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
           + (45 # 2) * max0(2 - s V_scale_bitcount_ep) <= z)%Q
   | 6 => ((10 # 1) + (45 # 2) * s V_scale_bitcount_ep + s V_scale_bitcount_z
           + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
           + (45 # 2) * max0(2 - s V_scale_bitcount_ep) <= z)%Q
   | 7 => ((10 # 1) + (45 # 2) * s V_scale_bitcount_ep + s V_scale_bitcount_z
           + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
           + (45 # 2) * max0(2 - s V_scale_bitcount_ep) <= z)%Q
   | 8 => ((10 # 1) + (45 # 2) * s V_scale_bitcount_ep + s V_scale_bitcount_z
           + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
           + (45 # 2) * max0(2 - s V_scale_bitcount_ep) <= z)%Q
   | 9 => ((10 # 1) + (45 # 2) * s V_scale_bitcount_ep
           - s V_scale_bitcount_sfb + s V_scale_bitcount_z
           + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
           + (45 # 2) * max0(2 - s V_scale_bitcount_ep) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (11
                                                               - s V_scale_bitcount_sfb) (0))) (F_max0_ge_0 (11
                                                                    - s V_scale_bitcount_sfb));
      (*-13.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                      - s V_scale_bitcount_ep)) (F_check_ge (2
                                                                    - s V_scale_bitcount_ep) (0))]
     ((10 # 1) + (45 # 2) * s V_scale_bitcount_ep - s V_scale_bitcount_sfb
      + s V_scale_bitcount_z
      + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
      + (45 # 2) * max0(2 - s V_scale_bitcount_ep) <= z)%Q
   | 11 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 12 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 13 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (11 - s V_scale_bitcount_sfb) (10
                                                                    - s V_scale_bitcount_sfb));
      (*-1 0*) F_max0_ge_0 (10 - s V_scale_bitcount_sfb);
      (*0 2*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_scale_bitcount_cod_info_dref_off64) (0))) (F_max0_ge_0 (-
                                                                    s V_scale_bitcount_cod_info_dref_off64))]
     ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
      + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
      + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
      + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 15 => ((26 # 1) + (2 # 1) * s V_scale_bitcount_cod_info_dref_off64
            + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (2 # 1) * max0(-s V_scale_bitcount_cod_info_dref_off64) <= z)%Q
   | 16 => ((37 # 1) + (2 # 1) * s V_scale_bitcount_cod_info_dref_off64
            + (9 # 1) * s V_scale_bitcount_ep - s V_scale_bitcount_sfb
            + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (2 # 1) * max0(-s V_scale_bitcount_cod_info_dref_off64) <= z)%Q
   | 17 => hints
     [(*-5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_scale_bitcount_ep) (0))) (F_max0_ge_0 (s V_scale_bitcount_ep));
      (*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_scale_bitcount_cod_info_dref_off64)) (F_check_ge (0) (0));
      (*-4 0*) F_binom_monotonic 1 (F_max0_ge_arg (2 - s V_scale_bitcount_ep)) (F_check_ge (2
                                                                    - s V_scale_bitcount_ep) (0));
      (*-2 0*) F_binom_monotonic 1 (F_max0_ge_arg (1
                                                   - s V_scale_bitcount_cod_info_dref_off64)) (F_check_ge (1
                                                                    - s V_scale_bitcount_cod_info_dref_off64) (0))]
     ((37 # 1) + (2 # 1) * s V_scale_bitcount_cod_info_dref_off64
      + (9 # 1) * s V_scale_bitcount_ep - s V_scale_bitcount_sfb
      + s V_scale_bitcount_z
      + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
      + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
      + (2 # 1) * max0(-s V_scale_bitcount_cod_info_dref_off64) <= z)%Q
   | 18 => ((47 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (21 - s V_scale_bitcount_sfb) (20
                                                                    - s V_scale_bitcount_sfb));
      (*-1 0*) F_max0_ge_0 (20 - s V_scale_bitcount_sfb);
      (*-5.14286 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_scale_bitcount_sfb) (0))) (F_max0_ge_0 (s V_scale_bitcount_sfb));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (21
                                                              - s V_scale_bitcount_sfb) (0))) (F_max0_ge_0 (21
                                                                    - s V_scale_bitcount_sfb));
      (*-5 0*) F_binom_monotonic 1 (F_max0_ge_0 (2 - s V_scale_bitcount_ep)) (F_check_ge (0) (0))]
     ((47 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z
      + (5 # 1) * max0(2 - s V_scale_bitcount_ep)
      + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 20 => ((47 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 21 => ((47 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 22 => ((47 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 23 => ((47 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 24 => ((48 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 25 => ((48 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 26 => ((48 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 27 => ((47 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 28 => ((47 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 29 => hints
     [(*-1.05263 0*) F_max0_ge_0 (20 - s V_scale_bitcount_sfb);
      (*-5.14286 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_scale_bitcount_sfb) (0))) (F_max0_ge_0 (s V_scale_bitcount_sfb));
      (*0 1.05263*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                                    - 
                                                                    s V_scale_bitcount_sfb) (0))) (F_max0_ge_0 (20
                                                                    - s V_scale_bitcount_sfb));
      (*-5 0*) F_binom_monotonic 1 (F_max0_ge_0 (2 - s V_scale_bitcount_ep)) (F_check_ge (0) (0));
      (*-0.0526316 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                         + s V_scale_bitcount_sfb)) (F_check_ge (0) (0));
      (*0 0.0526316*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_scale_bitcount_sfb) (0))) (F_max0_ge_0 (-1
                                                                    + s V_scale_bitcount_sfb))]
     ((47 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z
      + (5 # 1) * max0(2 - s V_scale_bitcount_ep)
      + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 30 => ((26 # 1) - (36 # 7) * s V_scale_bitcount_sfb
            + s V_scale_bitcount_z + (5 # 1) * max0(s V_scale_bitcount_ep)
            + (36 # 7) * max0(s V_scale_bitcount_sfb) <= z)%Q
   | 31 => hints
     [(*-5.14286 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_scale_bitcount_sfb)) (F_check_ge (s V_scale_bitcount_sfb) (0));
      (*-5 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_scale_bitcount_ep)) (F_check_ge (0) (0));
      (*-9 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_scale_bitcount_ep)) (F_check_ge (s V_scale_bitcount_ep) (0))]
     ((26 # 1) - (36 # 7) * s V_scale_bitcount_sfb + s V_scale_bitcount_z
      + (5 # 1) * max0(s V_scale_bitcount_ep)
      + (36 # 7) * max0(s V_scale_bitcount_sfb) <= z)%Q
   | 32 => hints
     [(*-5.14286 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_scale_bitcount_sfb)) (F_check_ge (s V_scale_bitcount_sfb) (0));
      (*0 5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                              + s V_scale_bitcount_ep) (0))) (F_max0_ge_0 (-2
                                                                    + s V_scale_bitcount_ep))]
     ((26 # 1) - (36 # 7) * s V_scale_bitcount_sfb + s V_scale_bitcount_z
      + (5 # 1) * max0(s V_scale_bitcount_ep)
      + (36 # 7) * max0(s V_scale_bitcount_sfb) <= z)%Q
   | 33 => ((36 # 1) - (5 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 34 => ((36 # 1) - (5 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 35 => ((26 # 1) - (5 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
            + max0(21 - s V_scale_bitcount_sfb)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 36 => ((26 # 1) - (5 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
            + max0(21 - s V_scale_bitcount_sfb)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 37 => ((26 # 1) - (5 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
            + max0(21 - s V_scale_bitcount_sfb)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (21 - s V_scale_bitcount_sfb) (20
                                                                    - s V_scale_bitcount_sfb));
      (*-1 0*) F_max0_ge_0 (20 - s V_scale_bitcount_sfb);
      (*-14 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_scale_bitcount_ep)) (F_check_ge (s V_scale_bitcount_ep) (0));
      (*-5 0*) F_binom_monotonic 1 (F_max0_ge_0 (-2 + s V_scale_bitcount_ep)) (F_check_ge (0) (0))]
     ((26 # 1) - (5 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
      + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
      + max0(21 - s V_scale_bitcount_sfb)
      + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 39 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            - (9 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 40 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            - (9 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 41 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (21
                                                   - s V_scale_bitcount_sfb)) (F_check_ge (21
                                                                    - s V_scale_bitcount_sfb) (0))]
     ((26 # 1) - (5 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
      + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
      + max0(21 - s V_scale_bitcount_sfb)
      + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 42 => ((47 # 1) - (5 # 1) * s V_scale_bitcount_ep
            - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 43 => ((47 # 1) - (5 # 1) * s V_scale_bitcount_ep
            - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 44 => ((48 # 1) - (5 # 1) * s V_scale_bitcount_ep
            - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 45 => ((48 # 1) - (5 # 1) * s V_scale_bitcount_ep
            - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 46 => ((48 # 1) - (5 # 1) * s V_scale_bitcount_ep
            - s V_scale_bitcount_sfb + s V_scale_bitcount_z
            + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
            + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (21
                                                               - s V_scale_bitcount_sfb) (0))) (F_max0_ge_0 (21
                                                                    - s V_scale_bitcount_sfb))]
     ((47 # 1) - (5 # 1) * s V_scale_bitcount_ep - s V_scale_bitcount_sfb
      + s V_scale_bitcount_z + (5 # 1) * max0(-2 + s V_scale_bitcount_ep)
      + (5 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 48 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (11 - s V_scale_bitcount_sfb) (10
                                                                    - s V_scale_bitcount_sfb));
      (*-1 0*) F_max0_ge_0 (10 - s V_scale_bitcount_sfb);
      (*-9 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_scale_bitcount_ep)) (F_check_ge (0) (0));
      (*-9 0*) F_binom_monotonic 1 (F_max0_ge_0 (2 - s V_scale_bitcount_ep)) (F_check_ge (0) (0));
      (*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_scale_bitcount_cod_info_dref_off64)) (F_check_ge (0) (0))]
     ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
      + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
      + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
      + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 49 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            - (9 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 50 => ((16 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + max0(21 - s V_scale_bitcount_sfb)
            - (9 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 51 => hints
     [(*-9 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_scale_bitcount_ep) (0))) (F_max0_ge_0 (s V_scale_bitcount_ep))]
     ((16 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
      + max0(21 - s V_scale_bitcount_sfb)
      - (9 # 1) * max0(s V_scale_bitcount_ep) <= z)%Q
   | 52 => ((16 # 1) + s V_scale_bitcount_z
            + max0(21 - s V_scale_bitcount_sfb) <= z)%Q
   | 53 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (21 - s V_scale_bitcount_sfb) (20
                                                                    - s V_scale_bitcount_sfb));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (20 - s V_scale_bitcount_sfb)) (F_check_ge (0) (0))]
     ((16 # 1) + s V_scale_bitcount_z + max0(21 - s V_scale_bitcount_sfb) <= z)%Q
   | 54 => ((16 # 1) + s V_scale_bitcount_z <= z)%Q
   | 55 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (21 - s V_scale_bitcount_sfb)) (F_check_ge (21
                                                                    - s V_scale_bitcount_sfb) (0))]
     ((16 # 1) + s V_scale_bitcount_z + max0(21 - s V_scale_bitcount_sfb) <= z)%Q
   | 56 => ((37 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z <= z)%Q
   | 57 => ((37 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z <= z)%Q
   | 58 => ((37 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z <= z)%Q
   | 59 => ((37 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z <= z)%Q
   | 60 => ((37 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z <= z)%Q
   | 61 => ((38 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z <= z)%Q
   | 62 => ((38 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z <= z)%Q
   | 63 => ((38 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z <= z)%Q
   | 64 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (21
                                                               - s V_scale_bitcount_sfb) (0))) (F_max0_ge_0 (21
                                                                    - s V_scale_bitcount_sfb))]
     ((37 # 1) - s V_scale_bitcount_sfb + s V_scale_bitcount_z <= z)%Q
   | 65 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 66 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 67 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 68 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 69 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 70 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 71 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 72 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 73 => ((26 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
            + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 74 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (12 - s V_scale_bitcount_sfb) (1)]
     ((25 # 1) + (9 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
      + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
      + (9 # 1) * max0(2 - s V_scale_bitcount_ep)
      + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 75 => ((10 # 1) + (45 # 2) * s V_scale_bitcount_ep
            + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (45 # 2) * max0(2 - s V_scale_bitcount_ep) <= z)%Q
   | 76 => ((10 # 1) + (45 # 2) * s V_scale_bitcount_ep
            + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (45 # 2) * max0(2 - s V_scale_bitcount_ep) <= z)%Q
   | 77 => (-(29 # 1) + (45 # 2) * s V_scale_bitcount_ep
            + s V_scale_bitcount_z
            + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
            + (45 # 2) * max0(2 - s V_scale_bitcount_ep)
            + (13 # 1) * max0(3 - s V_scale_bitcount_i) <= z)%Q
   | 78 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_scale_bitcount_z) (0))) (F_max0_ge_0 (s V_scale_bitcount_z));
      (*-14.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                      - s V_scale_bitcount_ep)) (F_check_ge (2
                                                                    - s V_scale_bitcount_ep) (0));
      (*-2 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_scale_bitcount_cod_info_dref_off64)) (F_check_ge (0) (0))]
     (-(29 # 1) + (45 # 2) * s V_scale_bitcount_ep + s V_scale_bitcount_z
      + (2 # 1) * max0(1 - s V_scale_bitcount_cod_info_dref_off64)
      + (45 # 2) * max0(2 - s V_scale_bitcount_ep)
      + (13 # 1) * max0(3 - s V_scale_bitcount_i) <= z)%Q
   | 79 => ((8 # 1) * s V_scale_bitcount_ep
            + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
            + (13 # 1) * max0(3 - s V_scale_bitcount_i)
            + max0(s V_scale_bitcount_z) <= z)%Q
   | 80 => hints
     [(*-13 0*) F_max0_monotonic (F_check_ge (3 - s V_scale_bitcount_i) (2
                                                                    - s V_scale_bitcount_i));
      (*-13 0*) F_max0_ge_0 (2 - s V_scale_bitcount_i);
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_scale_bitcount_z)) (F_check_ge (s V_scale_bitcount_z) (0));
      (*0 8*) F_binom_monotonic 1 (F_max0_ge_arg (2 - s V_scale_bitcount_ep)) (F_check_ge (2
                                                                    - s V_scale_bitcount_ep) (0))]
     ((8 # 1) * s V_scale_bitcount_ep
      + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
      + (13 # 1) * max0(3 - s V_scale_bitcount_i)
      + max0(s V_scale_bitcount_z) <= z)%Q
   | 81 => ((16 # 1) + s V_scale_bitcount_z <= z)%Q
   | 82 => ((16 # 1) + s V_scale_bitcount_z <= z)%Q
   | 83 => ((16 # 1) + s V_scale_bitcount_z <= z)%Q
   | 84 => (s V_scale_bitcount_z + max0(16 - s V_scale_bitcount_k) <= z)%Q
   | 85 => (s V_scale_bitcount_z + max0(16 - s V_scale_bitcount_k) <= z)%Q
   | 86 => (s V_scale_bitcount_z + max0(16 - s V_scale_bitcount_k) <= z)%Q
   | 87 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (16 - s V_scale_bitcount_k) (15
                                                                    - s V_scale_bitcount_k));
      (*-1 0*) F_max0_ge_0 (15 - s V_scale_bitcount_k)]
     (s V_scale_bitcount_z + max0(16 - s V_scale_bitcount_k) <= z)%Q
   | 88 => (s V_scale_bitcount_z <= z)%Q
   | 89 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (16 - s V_scale_bitcount_k)) (F_check_ge (16
                                                                    - s V_scale_bitcount_k) (0))]
     (s V_scale_bitcount_z + max0(16 - s V_scale_bitcount_k) <= z)%Q
   | 90 => ((16 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 91 => ((16 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 92 => ((16 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 93 => ((16 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 94 => ((16 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 95 => ((16 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 96 => ((16 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 97 => ((16 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 98 => ((16 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 99 => ((16 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 100 => ((16 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 101 => ((17 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 102 => ((17 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 103 => ((17 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 104 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                               - s V_scale_bitcount_k) (0))) (F_max0_ge_0 (16
                                                                    - s V_scale_bitcount_k))]
     ((16 # 1) - s V_scale_bitcount_k + s V_scale_bitcount_z <= z)%Q
   | 105 => ((8 # 1) * s V_scale_bitcount_ep
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(3 - s V_scale_bitcount_i)
             + max0(s V_scale_bitcount_z) <= z)%Q
   | 106 => ((8 # 1) * s V_scale_bitcount_ep
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(3 - s V_scale_bitcount_i)
             + max0(s V_scale_bitcount_z) <= z)%Q
   | 107 => (-(12 # 1) + (8 # 1) * s V_scale_bitcount_ep
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(3 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) + max0(s V_scale_bitcount_z) <= z)%Q
   | 108 => hints
     [(*-13 0*) F_max0_pre_decrement 1 (3 - s V_scale_bitcount_i) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_scale_bitcount_z)) (F_check_ge (s V_scale_bitcount_z) (0))]
     (-(12 # 1) + (8 # 1) * s V_scale_bitcount_ep
      + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
      + (13 # 1) * max0(3 - s V_scale_bitcount_i)
      + max0(12 - s V_scale_bitcount_sfb) + max0(s V_scale_bitcount_z) <= z)%Q
   | 109 => ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 110 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (12 - s V_scale_bitcount_sfb) (6))]
     ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
      + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
      + (13 # 1) * max0(2 - s V_scale_bitcount_i)
      + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 111 => ((7 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i) <= z)%Q
   | 112 => ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 113 => ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 114 => ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 115 => ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 116 => ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 117 => ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 118 => ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(3 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 119 => ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(3 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 120 => ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(3 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 121 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (12 - s V_scale_bitcount_sfb) (11
                                                                    - s V_scale_bitcount_sfb));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_scale_bitcount_z) (0))) (F_max0_ge_0 (s V_scale_bitcount_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (11 - s V_scale_bitcount_sfb)) (F_check_ge (0) (0))]
     ((8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
      + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
      + (13 # 1) * max0(3 - s V_scale_bitcount_i)
      + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 122 => hints
     [(*0 1*) F_max0_pre_decrement 1 (12 - s V_scale_bitcount_sfb) (1)]
     ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
      + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
      + (13 # 1) * max0(2 - s V_scale_bitcount_i)
      + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 123 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 124 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 125 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 126 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 127 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 128 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 129 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 130 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 131 => ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 132 => hints
     [(*-1 2.15016e-12*) F_max0_pre_decrement 1 (12 - s V_scale_bitcount_sfb) (1)]
     ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
      + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
      + (13 # 1) * max0(2 - s V_scale_bitcount_i)
      + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 133 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 134 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 135 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 136 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 137 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(11 - s V_scale_bitcount_sfb) <= z)%Q
   | 138 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 139 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 140 => ((2 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | 141 => ((1 # 1) + (8 # 1) * s V_scale_bitcount_ep + s V_scale_bitcount_z
             + (8 # 1) * max0(2 - s V_scale_bitcount_ep)
             + (13 # 1) * max0(2 - s V_scale_bitcount_i)
             + max0(12 - s V_scale_bitcount_sfb) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_scale_bitcount =>
    [mkPA Q (fun n z s => ai_scale_bitcount n s /\ annot0_scale_bitcount n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_scale_bitcount (proc_start P_scale_bitcount) s1 (proc_end P_scale_bitcount) s2 ->
    (s2 V_scale_bitcount_z <= (55 # 1)
                              + (2 # 1) * max0(1
                                               - s1 V_scale_bitcount_cod_info_dref_off64))%Q.
Proof.
  prove_bound ipa admissible_ipa P_scale_bitcount.
Qed.
