Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cmd_put_color_mapping.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cmd_put_color_mapping_z := 1%positive.
Notation V_cmd_put_color_mapping__tmp := 2%positive.
Notation V_cmd_put_color_mapping__tmp1 := 3%positive.
Notation V_cmd_put_color_mapping_all_same := 4%positive.
Notation V_cmd_put_color_mapping_code := 5%positive.
Notation V_cmd_put_color_mapping_i := 6%positive.
Notation V_cmd_put_color_mapping_which := 7%positive.
Notation V_cmd_put_color_mapping_cldev := 8%positive.
Notation V_cmd_put_color_mapping_pis := 9%positive.
Notation V_cmd_put_color_mapping_write_rgb_to_cmyk := 10%positive.
Definition Pedges_cmd_put_color_mapping: list (edge proc) :=
  (EA 1 (AAssign V_cmd_put_color_mapping_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_cmd_put_color_mapping_i) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign
  V_cmd_put_color_mapping__tmp1
  (Some (EVar V_cmd_put_color_mapping_write_rgb_to_cmyk))) 5)::
  (EA 5 AWeaken 6)::(EA 6 ANone 8)::(EA 6 ANone 7)::(EA 7 AWeaken 14)::
  (EA 8 (AAssign V_cmd_put_color_mapping_code None) 9)::(EA 9 AWeaken 10)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_cmd_put_color_mapping_code) s) <
  (eval (ENum (0)) s))%Z)) 113)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_code) s) >= (eval (ENum (0))
  s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 ANone 13)::(EA 13 AWeaken 14)::
  (EA 14 (AGuard (fun s => ((eval (EVar V_cmd_put_color_mapping__tmp1) s) <>
  (eval (ENum (0)) s))%Z)) 16)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping__tmp1) s) = (eval (ENum (0))
  s))%Z)) 15)::(EA 15 AWeaken 26)::(EA 16 AWeaken 17)::(EA 17 (AAssign
  V_cmd_put_color_mapping_code None) 18)::(EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_code) s) < (eval (ENum (0))
  s))%Z)) 109)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_code) s) >= (eval (ENum (0))
  s))%Z)) 20)::(EA 20 AWeaken 21)::(EA 21 (AAssign
  V_cmd_put_color_mapping_code None) 22)::(EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_code) s) < (eval (ENum (0))
  s))%Z)) 105)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_code) s) >= (eval (ENum (0))
  s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 ANone 26)::(EA 26 (AAssign
  V_cmd_put_color_mapping_which (Some (ENum (0)))) 27)::(EA 27 (AAssign
  V_cmd_put_color_mapping_all_same (Some (ENum (1)))) 28)::(EA 28 (AAssign
  V_cmd_put_color_mapping_i (Some (ENum (0)))) 29)::(EA 29 ANone 30)::
  (EA 30 AWeaken 31)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_i) s) < (eval (ENum (4))
  s))%Z)) 90)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_i) s) >= (eval (ENum (4))
  s))%Z)) 32)::(EA 32 AWeaken 33)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_which) s) = (eval (ENum (0))
  s))%Z)) 86)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_which) s) <>
  (eval (ENum (0)) s))%Z)) 34)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_which) s) =
  (eval (ENum (15)) s))%Z)) 37)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_which) s) <>
  (eval (ENum (15)) s))%Z)) 36)::(EA 36 AWeaken 40)::(EA 37 AWeaken 38)::
  (EA 38 (AGuard (fun s => ((eval (EVar V_cmd_put_color_mapping_all_same)
  s) <> (eval (ENum (0)) s))%Z)) 61)::(EA 38 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_all_same) s) =
  (eval (ENum (0)) s))%Z)) 39)::(EA 39 AWeaken 40)::(EA 40 (AAssign
  V_cmd_put_color_mapping_i (Some (ENum (0)))) 41)::(EA 41 ANone 42)::
  (EA 42 AWeaken 43)::(EA 43 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_i) s) < (eval (ENum (4))
  s))%Z)) 46)::(EA 43 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_i) s) >= (eval (ENum (4))
  s))%Z)) 44)::(EA 44 AWeaken 45)::(EA 45 ANone 72)::(EA 46 AWeaken 47)::
  (EA 47 (AAssign V_cmd_put_color_mapping_code None) 48)::
  (EA 48 AWeaken 49)::(EA 49 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_code) s) < (eval (ENum (0))
  s))%Z)) 57)::(EA 49 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_code) s) >= (eval (ENum (0))
  s))%Z)) 50)::(EA 50 AWeaken 51)::(EA 51 ANone 52)::(EA 52 (AAssign
  V_cmd_put_color_mapping_i (Some (EAdd (EVar V_cmd_put_color_mapping_i)
  (ENum (1))))) 53)::(EA 53 ANone 54)::(EA 54 ANone 55)::(EA 55 (AAssign
  V_cmd_put_color_mapping_z (Some (EAdd (ENum (1))
  (EVar V_cmd_put_color_mapping_z)))) 56)::(EA 56 AWeaken 43)::
  (EA 57 AWeaken 58)::(EA 58 (AAssign V_cmd_put_color_mapping__tmp
  (Some (EVar V_cmd_put_color_mapping_code))) 59)::(EA 59 ANone 60)::
  (EA 60 AWeaken 117)::(EA 61 AWeaken 62)::(EA 62 (AAssign
  V_cmd_put_color_mapping_code None) 63)::(EA 63 AWeaken 64)::(EA 64 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_code) s) < (eval (ENum (0))
  s))%Z)) 82)::(EA 64 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_code) s) >= (eval (ENum (0))
  s))%Z)) 65)::(EA 65 AWeaken 66)::(EA 66 (AAssign V_cmd_put_color_mapping_i
  (Some (ENum (1)))) 67)::(EA 67 ANone 68)::(EA 68 AWeaken 69)::
  (EA 69 (AGuard (fun s => ((eval (EVar V_cmd_put_color_mapping_i) s) <
  (eval (ENum (4)) s))%Z)) 75)::(EA 69 (AGuard
  (fun s => ((eval (EVar V_cmd_put_color_mapping_i) s) >= (eval (ENum (4))
  s))%Z)) 70)::(EA 70 AWeaken 71)::(EA 71 ANone 72)::(EA 72 (AAssign
  V_cmd_put_color_mapping__tmp (Some (ENum (0)))) 73)::(EA 73 ANone 74)::
  (EA 74 AWeaken 117)::(EA 75 AWeaken 76)::(EA 76 ANone 77)::(EA 77 (AAssign
  V_cmd_put_color_mapping_i (Some (EAdd (EVar V_cmd_put_color_mapping_i)
  (ENum (1))))) 78)::(EA 78 ANone 79)::(EA 79 ANone 80)::(EA 80 (AAssign
  V_cmd_put_color_mapping_z (Some (EAdd (ENum (1))
  (EVar V_cmd_put_color_mapping_z)))) 81)::(EA 81 AWeaken 69)::
  (EA 82 AWeaken 83)::(EA 83 (AAssign V_cmd_put_color_mapping__tmp
  (Some (EVar V_cmd_put_color_mapping_code))) 84)::(EA 84 ANone 85)::
  (EA 85 AWeaken 117)::(EA 86 AWeaken 87)::(EA 87 (AAssign
  V_cmd_put_color_mapping__tmp (Some (ENum (0)))) 88)::(EA 88 ANone 89)::
  (EA 89 AWeaken 117)::(EA 90 AWeaken 91)::(EA 91 ANone 93)::
  (EA 91 ANone 92)::(EA 92 AWeaken 96)::(EA 93 (AAssign
  V_cmd_put_color_mapping_which None) 94)::(EA 94 ANone 95)::
  (EA 95 AWeaken 96)::(EA 96 ANone 97)::(EA 96 ANone 99)::(EA 97 (AAssign
  V_cmd_put_color_mapping_all_same (Some (ENum (0)))) 98)::(EA 98 ANone 99)::
  (EA 99 ANone 100)::(EA 100 (AAssign V_cmd_put_color_mapping_i
  (Some (EAdd (EVar V_cmd_put_color_mapping_i) (ENum (1))))) 101)::
  (EA 101 ANone 102)::(EA 102 ANone 103)::(EA 103 (AAssign
  V_cmd_put_color_mapping_z (Some (EAdd (ENum (1))
  (EVar V_cmd_put_color_mapping_z)))) 104)::(EA 104 AWeaken 31)::
  (EA 105 AWeaken 106)::(EA 106 (AAssign V_cmd_put_color_mapping__tmp
  (Some (EVar V_cmd_put_color_mapping_code))) 107)::(EA 107 ANone 108)::
  (EA 108 AWeaken 117)::(EA 109 AWeaken 110)::(EA 110 (AAssign
  V_cmd_put_color_mapping__tmp
  (Some (EVar V_cmd_put_color_mapping_code))) 111)::(EA 111 ANone 112)::
  (EA 112 AWeaken 117)::(EA 113 AWeaken 114)::(EA 114 (AAssign
  V_cmd_put_color_mapping__tmp
  (Some (EVar V_cmd_put_color_mapping_code))) 115)::(EA 115 ANone 116)::
  (EA 116 AWeaken 117)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cmd_put_color_mapping => Pedges_cmd_put_color_mapping
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cmd_put_color_mapping => 117
     end)%positive;
  var_global := var_global
}.

Definition ai_cmd_put_color_mapping (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0)%Z
   | 3 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 4 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0)%Z
   | 5 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 6 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0)%Z
   | 7 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 8 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 9 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0)%Z
   | 10 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 11 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0)%Z
   | 12 => (-1 * s V_cmd_put_color_mapping_code <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 13 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0)%Z
   | 14 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 15 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping__tmp1 <= 0 /\ -1 * s V_cmd_put_color_mapping__tmp1 <= 0)%Z
   | 16 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0)%Z
   | 17 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 18 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0)%Z
   | 19 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 20 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0)%Z
   | 21 => (-1 * s V_cmd_put_color_mapping_code <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 22 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0)%Z
   | 23 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 24 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0)%Z
   | 25 => (-1 * s V_cmd_put_color_mapping_code <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 26 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0)%Z
   | 27 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_which <= 0 /\ -1 * s V_cmd_put_color_mapping_which <= 0)%Z
   | 28 => (-1 * s V_cmd_put_color_mapping_which <= 0 /\ 1 * s V_cmd_put_color_mapping_which <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_all_same + 1 <= 0)%Z
   | 29 => (-1 * s V_cmd_put_color_mapping_all_same + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_which <= 0 /\ -1 * s V_cmd_put_color_mapping_which <= 0 /\ 1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 30 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_which <= 0 /\ 1 * s V_cmd_put_color_mapping_which <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_all_same + 1 <= 0)%Z
   | 31 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 32 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0)%Z
   | 33 => (-1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 34 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0)%Z
   | 35 => (-1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 36 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0)%Z
   | 37 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0)%Z
   | 38 => (-1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 39 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same <= 0 /\ -1 * s V_cmd_put_color_mapping_all_same <= 0)%Z
   | 40 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 41 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 42 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0)%Z
   | 43 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 44 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0)%Z
   | 45 => (-1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 46 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0)%Z
   | 47 => (1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0)%Z
   | 48 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0)%Z
   | 49 => (1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0)%Z
   | 50 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0)%Z
   | 51 => (-1 * s V_cmd_put_color_mapping_code <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0)%Z
   | 52 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0)%Z
   | 53 => (-1 * s V_cmd_put_color_mapping_code <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 54 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0)%Z
   | 55 => (-1 * s V_cmd_put_color_mapping_code <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 56 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0 /\ -1 * s V_cmd_put_color_mapping_z + 1 <= 0)%Z
   | 57 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0)%Z
   | 58 => (1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0)%Z
   | 59 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping__tmp + 1 <= 0)%Z
   | 60 => (1 * s V_cmd_put_color_mapping__tmp + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0)%Z
   | 61 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0)%Z
   | 62 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 63 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0)%Z
   | 64 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 65 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0)%Z
   | 66 => (-1 * s V_cmd_put_color_mapping_code <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 67 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 1 <= 0)%Z
   | 68 => (-1 * s V_cmd_put_color_mapping_i + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0)%Z
   | 69 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 70 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0)%Z
   | 71 => (-1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 72 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0)%Z
   | 73 => (-1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ 1 * s V_cmd_put_color_mapping__tmp <= 0 /\ -1 * s V_cmd_put_color_mapping__tmp <= 0)%Z
   | 74 => (-1 * s V_cmd_put_color_mapping__tmp <= 0 /\ 1 * s V_cmd_put_color_mapping__tmp <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0)%Z
   | 75 => (-1 * s V_cmd_put_color_mapping_code <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0)%Z
   | 76 => (1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0)%Z
   | 77 => (-1 * s V_cmd_put_color_mapping_code <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0)%Z
   | 78 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 2 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 79 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 2 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0)%Z
   | 80 => (-1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 2 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 81 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 2 <= 0 /\ -1 * s V_cmd_put_color_mapping_code <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_z + 1 <= 0)%Z
   | 82 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0)%Z
   | 83 => (1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 84 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping__tmp + 1 <= 0)%Z
   | 85 => (1 * s V_cmd_put_color_mapping__tmp + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_which + 15 <= 0 /\ 1 * s V_cmd_put_color_mapping_which + -15 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 86 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ 1 * s V_cmd_put_color_mapping_which <= 0 /\ -1 * s V_cmd_put_color_mapping_which <= 0)%Z
   | 87 => (-1 * s V_cmd_put_color_mapping_which <= 0 /\ 1 * s V_cmd_put_color_mapping_which <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 88 => (1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ 1 * s V_cmd_put_color_mapping_which <= 0 /\ -1 * s V_cmd_put_color_mapping_which <= 0 /\ 1 * s V_cmd_put_color_mapping__tmp <= 0 /\ -1 * s V_cmd_put_color_mapping__tmp <= 0)%Z
   | 89 => (-1 * s V_cmd_put_color_mapping__tmp <= 0 /\ 1 * s V_cmd_put_color_mapping__tmp <= 0 /\ -1 * s V_cmd_put_color_mapping_which <= 0 /\ 1 * s V_cmd_put_color_mapping_which <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0)%Z
   | 90 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0)%Z
   | 91 => (1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0)%Z
   | 92 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0)%Z
   | 93 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0)%Z
   | 94 => (1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0)%Z
   | 95 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0)%Z
   | 96 => (1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0)%Z
   | 97 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0)%Z
   | 98 => (1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same <= 0 /\ -1 * s V_cmd_put_color_mapping_all_same <= 0)%Z
   | 99 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -3 <= 0)%Z
   | 100 => (1 * s V_cmd_put_color_mapping_i + -3 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0)%Z
   | 101 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 1 <= 0)%Z
   | 102 => (-1 * s V_cmd_put_color_mapping_i + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0)%Z
   | 103 => (1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ -1 * s V_cmd_put_color_mapping_i + 1 <= 0)%Z
   | 104 => (-1 * s V_cmd_put_color_mapping_i + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_i + -4 <= 0 /\ 1 * s V_cmd_put_color_mapping_all_same + -1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z + 1 <= 0)%Z
   | 105 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0)%Z
   | 106 => (1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 107 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping__tmp + 1 <= 0)%Z
   | 108 => (1 * s V_cmd_put_color_mapping__tmp + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 109 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0)%Z
   | 110 => (1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 111 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping__tmp + 1 <= 0)%Z
   | 112 => (1 * s V_cmd_put_color_mapping__tmp + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 113 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0)%Z
   | 114 => (1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 115 => (-1 * s V_cmd_put_color_mapping_i <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping__tmp + 1 <= 0)%Z
   | 116 => (1 * s V_cmd_put_color_mapping__tmp + 1 <= 0 /\ 1 * s V_cmd_put_color_mapping_code + 1 <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0 /\ 1 * s V_cmd_put_color_mapping_z <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0)%Z
   | 117 => (1 * s V_cmd_put_color_mapping__tmp <= 0 /\ -1 * s V_cmd_put_color_mapping_i <= 0 /\ -1 * s V_cmd_put_color_mapping_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cmd_put_color_mapping (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_cmd_put_color_mapping_z <= z)%Q
   | 3 => ((8 # 1) + s V_cmd_put_color_mapping_z <= z)%Q
   | 4 => ((8 # 1) + s V_cmd_put_color_mapping_z <= z)%Q
   | 5 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_cmd_put_color_mapping_z) (0))) (F_max0_ge_0 (s V_cmd_put_color_mapping_z))]
     ((8 # 1) + s V_cmd_put_color_mapping_z <= z)%Q
   | 6 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 7 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 8 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 9 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 10 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 11 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 12 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 13 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 14 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 15 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cmd_put_color_mapping_z)) (F_check_ge (s V_cmd_put_color_mapping_z) (0))]
     ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 16 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 17 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 18 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 19 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 20 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 21 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 22 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 23 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cmd_put_color_mapping_z)) (F_check_ge (s V_cmd_put_color_mapping_z) (0))]
     ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 25 => ((8 # 1) + s V_cmd_put_color_mapping_z <= z)%Q
   | 26 => ((8 # 1) + s V_cmd_put_color_mapping_z <= z)%Q
   | 27 => ((8 # 1) + s V_cmd_put_color_mapping_z <= z)%Q
   | 28 => ((8 # 1) + s V_cmd_put_color_mapping_z <= z)%Q
   | 29 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
            + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 30 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
            + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 31 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
            + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_cmd_put_color_mapping_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_cmd_put_color_mapping_i))]
     (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
      + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 33 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(-1 + s V_cmd_put_color_mapping_i)
            + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 34 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (4 - s V_cmd_put_color_mapping_i) (3
                                                                    - s V_cmd_put_color_mapping_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_cmd_put_color_mapping_i)) (F_check_ge (-1
                                                                    + s V_cmd_put_color_mapping_i) (0))]
     ((1 # 1) + s V_cmd_put_color_mapping_z
      + max0(-1 + s V_cmd_put_color_mapping_i)
      + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 35 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i)
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_max0_ge_0 (3 - s V_cmd_put_color_mapping_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                   - s V_cmd_put_color_mapping_i)) (F_check_ge (4
                                                                    - s V_cmd_put_color_mapping_i) (0))]
     (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
      + max0(3 - s V_cmd_put_color_mapping_i)
      + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 37 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i)
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 38 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i)
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 39 => hints
     [(*-1 0*) F_max0_ge_0 (3 - s V_cmd_put_color_mapping_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                   - s V_cmd_put_color_mapping_i)) (F_check_ge (4
                                                                    - s V_cmd_put_color_mapping_i) (0))]
     (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
      + max0(3 - s V_cmd_put_color_mapping_i)
      + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 40 => ((4 # 1) + s V_cmd_put_color_mapping_z <= z)%Q
   | 41 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 42 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 43 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 44 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (4 - s V_cmd_put_color_mapping_i) (3
                                                                    - s V_cmd_put_color_mapping_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                 - s V_cmd_put_color_mapping_i)) (F_check_ge (0) (0))]
     (s V_cmd_put_color_mapping_z + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 45 => (s V_cmd_put_color_mapping_z <= z)%Q
   | 46 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 47 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 48 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 49 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 50 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_cmd_put_color_mapping_i) (1)]
     (s V_cmd_put_color_mapping_z + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 51 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 52 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 53 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 54 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 55 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 56 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 57 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 58 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 59 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 60 => hints
     [(*-1 0*) F_max0_ge_0 (4 - s V_cmd_put_color_mapping_i)]
     (s V_cmd_put_color_mapping_z + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 61 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i)
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 62 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i)
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 63 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                   - s V_cmd_put_color_mapping_i)) (F_check_ge (4
                                                                    - s V_cmd_put_color_mapping_i) (0))]
     (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
      + max0(3 - s V_cmd_put_color_mapping_i)
      + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 64 => ((4 # 1) + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 65 => hints
     [(*-1 0*) F_one; (*-1 0*) F_max0_ge_0 (3 - s V_cmd_put_color_mapping_i)]
     ((4 # 1) + s V_cmd_put_color_mapping_z
      + max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 66 => ((3 # 1) + s V_cmd_put_color_mapping_z <= z)%Q
   | 67 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 68 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 69 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 70 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_cmd_put_color_mapping_i) (3
                                                                    - s V_cmd_put_color_mapping_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_cmd_put_color_mapping_i)]
     (s V_cmd_put_color_mapping_z + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 71 => (s V_cmd_put_color_mapping_z <= z)%Q
   | 72 => (s V_cmd_put_color_mapping_z <= z)%Q
   | 73 => (s V_cmd_put_color_mapping_z <= z)%Q
   | 74 => (s V_cmd_put_color_mapping_z <= z)%Q
   | 75 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_cmd_put_color_mapping_i) (1)]
     (s V_cmd_put_color_mapping_z + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 76 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 77 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 78 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 79 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 80 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 81 => (s V_cmd_put_color_mapping_z
            + max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 82 => ((4 # 1) + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 83 => ((4 # 1) + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 84 => ((4 # 1) + s V_cmd_put_color_mapping_z
            + max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 85 => hints
     [(*-1 0*) F_max0_ge_0 (3 - s V_cmd_put_color_mapping_i);
      (*-0.266667 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_cmd_put_color_mapping_which)) (F_check_ge (0) (0));
      (*-0.266667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_cmd_put_color_mapping_which) (0))) (F_max0_ge_0 (s V_cmd_put_color_mapping_which));
      (*-0.266667 0*) F_binom_monotonic 1 (F_max0_ge_0 (15
                                                        - s V_cmd_put_color_mapping_which)) (F_check_ge (0) (0));
      (*-0.266667 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (15
                                                                    - s V_cmd_put_color_mapping_which) (0))) (F_max0_ge_0 (15
                                                                    - s V_cmd_put_color_mapping_which))]
     ((4 # 1) + s V_cmd_put_color_mapping_z
      + max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 86 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(-1 + s V_cmd_put_color_mapping_i)
            + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 87 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(-1 + s V_cmd_put_color_mapping_i)
            + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 88 => ((1 # 1) + s V_cmd_put_color_mapping_z
            + max0(-1 + s V_cmd_put_color_mapping_i)
            + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 89 => hints
     [(*-2 0*) F_max0_monotonic (F_check_ge (4 - s V_cmd_put_color_mapping_i) (3
                                                                    - s V_cmd_put_color_mapping_i));
      (*-2 0*) F_max0_ge_0 (3 - s V_cmd_put_color_mapping_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_cmd_put_color_mapping_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_cmd_put_color_mapping_i) (0))) (F_max0_ge_0 (s V_cmd_put_color_mapping_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_cmd_put_color_mapping_i)) (F_check_ge (-1
                                                                    + s V_cmd_put_color_mapping_i) (0))]
     ((1 # 1) + s V_cmd_put_color_mapping_z
      + max0(-1 + s V_cmd_put_color_mapping_i)
      + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 90 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
            + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 91 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
            + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 92 => hints
     [(*-2 0*) F_max0_pre_decrement 1 (4 - s V_cmd_put_color_mapping_i) (1)]
     (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
      + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 93 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
            + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 94 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
            + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 95 => hints
     [(*-2 0*) F_max0_pre_decrement 1 (4 - s V_cmd_put_color_mapping_i) (1)]
     (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
      + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 96 => ((2 # 1) + s V_cmd_put_color_mapping_i
            + s V_cmd_put_color_mapping_z
            + (2 # 1) * max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 97 => ((2 # 1) + s V_cmd_put_color_mapping_i
            + s V_cmd_put_color_mapping_z
            + (2 # 1) * max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 98 => ((2 # 1) + s V_cmd_put_color_mapping_i
            + s V_cmd_put_color_mapping_z
            + (2 # 1) * max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 99 => ((2 # 1) + s V_cmd_put_color_mapping_i
            + s V_cmd_put_color_mapping_z
            + (2 # 1) * max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 100 => ((2 # 1) + s V_cmd_put_color_mapping_i
             + s V_cmd_put_color_mapping_z
             + (2 # 1) * max0(3 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 101 => ((1 # 1) + s V_cmd_put_color_mapping_i
             + s V_cmd_put_color_mapping_z
             + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 102 => ((1 # 1) + s V_cmd_put_color_mapping_i
             + s V_cmd_put_color_mapping_z
             + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 103 => ((1 # 1) + s V_cmd_put_color_mapping_i
             + s V_cmd_put_color_mapping_z
             + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 104 => (s V_cmd_put_color_mapping_i + s V_cmd_put_color_mapping_z
             + (2 # 1) * max0(4 - s V_cmd_put_color_mapping_i) <= z)%Q
   | 105 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 106 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 107 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 108 => hints
     [(*-8 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cmd_put_color_mapping_z)) (F_check_ge (s V_cmd_put_color_mapping_z) (0))]
     ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 109 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 110 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 111 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 112 => hints
     [(*-8 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cmd_put_color_mapping_z)) (F_check_ge (s V_cmd_put_color_mapping_z) (0))]
     ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 113 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 114 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 115 => ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 116 => hints
     [(*-8 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cmd_put_color_mapping_z)) (F_check_ge (s V_cmd_put_color_mapping_z) (0))]
     ((8 # 1) + max0(s V_cmd_put_color_mapping_z) <= z)%Q
   | 117 => (s V_cmd_put_color_mapping_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cmd_put_color_mapping =>
    [mkPA Q (fun n z s => ai_cmd_put_color_mapping n s /\ annot0_cmd_put_color_mapping n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cmd_put_color_mapping (proc_start P_cmd_put_color_mapping) s1 (proc_end P_cmd_put_color_mapping) s2 ->
    (s2 V_cmd_put_color_mapping_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cmd_put_color_mapping.
Qed.
