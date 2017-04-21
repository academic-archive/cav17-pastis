Require Import pasta.Pasta.

Inductive proc: Type :=
  P_check_key_sig.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_check_key_sig_z := 1%positive.
Notation V_check_key_sig__tmp := 2%positive.
Notation V_check_key_sig__tmp1 := 3%positive.
Notation V_check_key_sig__tmp2 := 4%positive.
Notation V_check_key_sig__tmp3 := 5%positive.
Notation V_check_key_sig_algorithm := 6%positive.
Notation V_check_key_sig_cert_length := 7%positive.
Notation V_check_key_sig_class := 8%positive.
Notation V_check_key_sig_count := 9%positive.
Notation V_check_key_sig_global_precision := 10%positive.
Notation V_check_key_sig_i := 11%positive.
Notation V_check_key_sig_mdlen := 12%positive.
Notation V_check_key_sig_mdlow2_off0 := 13%positive.
Notation V_check_key_sig_mdlow2_off1 := 14%positive.
Notation V_check_key_sig_sigclass_dref := 15%positive.
Notation V_check_key_sig_version := 16%positive.
Notation V_check_key_sig_fkey := 17%positive.
Notation V_check_key_sig_fpkey := 18%positive.
Notation V_check_key_sig_fpsig := 19%positive.
Notation V_check_key_sig_fsig := 20%positive.
Notation V_check_key_sig_keyfile := 21%positive.
Notation V_check_key_sig_keypktlen := 22%positive.
Notation V_check_key_sig_keyuserid := 23%positive.
Notation V_check_key_sig_sigclass := 24%positive.
Notation V_check_key_sig_siguserid := 25%positive.
Notation V_check_key_sig_xtimestamp := 26%positive.
Definition Pedges_check_key_sig: list (edge proc) :=
  (EA 1 (AAssign V_check_key_sig_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_cert_length) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_check_key_sig__tmp3
  (Some (EVar V_check_key_sig_fpkey))) 5)::(EA 5 (AAssign
  V_check_key_sig__tmp2 (Some (EVar V_check_key_sig_keypktlen))) 6)::
  (EA 6 (AAssign V_check_key_sig__tmp1
  (Some (EVar V_check_key_sig_fpsig))) 7)::(EA 7 (AAssign
  V_check_key_sig_global_precision (Some (ENum (130)))) 8)::
  (EA 8 AWeaken 9)::(EA 9 ANone 10)::(EA 9 ANone 12)::(EA 10 AWeaken 11)::
  (EA 11 ANone 13)::(EA 11 ANone 12)::(EA 12 ANone 140)::(EA 13 (AAssign
  V_check_key_sig_cert_length None) 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_cert_length) s) > (eval (ENum (283))
  s))%Z)) 138)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_cert_length) s) <=
  (eval (ENum (283)) s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 137)::
  (EA 17 ANone 18)::(EA 18 (AAssign V_check_key_sig_version None) 19)::
  (EA 19 AWeaken 20)::(EA 20 ANone 134)::(EA 20 ANone 21)::(EA 21 (AAssign
  V_check_key_sig_mdlen None) 22)::(EA 22 AWeaken 23)::(EA 23 ANone 131)::
  (EA 23 ANone 24)::(EA 24 (AAssign V_check_key_sig_class None) 25)::
  (EA 25 (AAssign V_check_key_sig_sigclass_dref None) 26)::
  (EA 26 AWeaken 27)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_class) s) <> (eval (ENum (16))
  s))%Z)) 29)::(EA 27 (AGuard (fun s => ((eval (EVar V_check_key_sig_class)
  s) = (eval (ENum (16)) s))%Z)) 28)::(EA 28 AWeaken 41)::
  (EA 29 AWeaken 30)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_class) s) <> (eval (ENum (17))
  s))%Z)) 32)::(EA 30 (AGuard (fun s => ((eval (EVar V_check_key_sig_class)
  s) = (eval (ENum (17)) s))%Z)) 31)::(EA 31 AWeaken 41)::
  (EA 32 AWeaken 33)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_class) s) <> (eval (ENum (18))
  s))%Z)) 35)::(EA 33 (AGuard (fun s => ((eval (EVar V_check_key_sig_class)
  s) = (eval (ENum (18)) s))%Z)) 34)::(EA 34 AWeaken 41)::
  (EA 35 AWeaken 36)::(EA 36 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_class) s) <> (eval (ENum (19))
  s))%Z)) 38)::(EA 36 (AGuard (fun s => ((eval (EVar V_check_key_sig_class)
  s) = (eval (ENum (19)) s))%Z)) 37)::(EA 37 AWeaken 41)::
  (EA 38 AWeaken 39)::(EA 39 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_class) s) <> (eval (ENum (32))
  s))%Z)) 127)::(EA 39 (AGuard (fun s => ((eval (EVar V_check_key_sig_class)
  s) = (eval (ENum (32)) s))%Z)) 40)::(EA 40 AWeaken 41)::(EA 41 (AAssign
  V_check_key_sig_i (Some (ENum (0)))) 42)::(EA 42 ANone 43)::
  (EA 43 AWeaken 44)::(EA 44 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_i) s) < (eval (ENum (4))
  s))%Z)) 120)::(EA 44 (AGuard (fun s => ((eval (EVar V_check_key_sig_i)
  s) >= (eval (ENum (4)) s))%Z)) 45)::(EA 45 AWeaken 46)::(EA 46 (AAssign
  V_check_key_sig_i (Some (ENum (0)))) 47)::(EA 47 ANone 48)::
  (EA 48 AWeaken 49)::(EA 49 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_i) s) < (eval (ENum (8))
  s))%Z)) 113)::(EA 49 (AGuard (fun s => ((eval (EVar V_check_key_sig_i)
  s) >= (eval (ENum (8)) s))%Z)) 50)::(EA 50 AWeaken 51)::(EA 51 (AAssign
  V_check_key_sig_algorithm None) 52)::(EA 52 AWeaken 53)::
  (EA 53 ANone 110)::(EA 53 ANone 54)::(EA 54 (AAssign
  V_check_key_sig_algorithm None) 55)::(EA 55 AWeaken 56)::
  (EA 56 ANone 107)::(EA 56 ANone 57)::(EA 57 (AAssign
  V_check_key_sig_mdlow2_off0 None) 58)::(EA 58 (AAssign
  V_check_key_sig_mdlow2_off1 None) 59)::(EA 59 AWeaken 60)::
  (EA 60 ANone 104)::(EA 60 ANone 61)::(EA 61 AWeaken 62)::
  (EA 62 ANone 101)::(EA 62 ANone 63)::(EA 63 AWeaken 64)::(EA 64 ANone 98)::
  (EA 64 ANone 65)::(EA 65 (AAssign V_check_key_sig_count None) 66)::
  (EA 66 AWeaken 67)::(EA 67 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_count) s) < (eval (ENum (0))
  s))%Z)) 94)::(EA 67 (AGuard (fun s => ((eval (EVar V_check_key_sig_count)
  s) >= (eval (ENum (0)) s))%Z)) 68)::(EA 68 AWeaken 69)::(EA 69 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_count) s) <> (eval (ENum (16))
  s))%Z)) 90)::(EA 69 (AGuard (fun s => ((eval (EVar V_check_key_sig_count)
  s) = (eval (ENum (16)) s))%Z)) 70)::(EA 70 AWeaken 71)::(EA 71 ANone 87)::
  (EA 71 ANone 72)::(EA 72 AWeaken 73)::(EA 73 ANone 87)::(EA 73 ANone 74)::
  (EA 74 AWeaken 75)::(EA 75 (AGuard
  (fun s => ((eval (EVar V_check_key_sig_class) s) <> (eval (ENum (32))
  s))%Z)) 77)::(EA 75 (AGuard (fun s => ((eval (EVar V_check_key_sig_class)
  s) = (eval (ENum (32)) s))%Z)) 76)::(EA 76 AWeaken 80)::
  (EA 77 AWeaken 78)::(EA 78 ANone 79)::(EA 79 AWeaken 80)::
  (EA 80 ANone 84)::(EA 80 ANone 81)::(EA 81 (AAssign V_check_key_sig__tmp
  (Some (ENum (0)))) 82)::(EA 82 ANone 83)::(EA 83 AWeaken 143)::
  (EA 84 (AAssign V_check_key_sig__tmp (Some (ENum (-20)))) 85)::
  (EA 85 ANone 86)::(EA 86 AWeaken 143)::(EA 87 (AAssign V_check_key_sig__tmp
  (Some (ENum (-9)))) 88)::(EA 88 ANone 89)::(EA 89 AWeaken 143)::
  (EA 90 AWeaken 91)::(EA 91 (AAssign V_check_key_sig__tmp
  (Some (ENum (-9)))) 92)::(EA 92 ANone 93)::(EA 93 AWeaken 143)::
  (EA 94 AWeaken 95)::(EA 95 (AAssign V_check_key_sig__tmp
  (Some (EVar V_check_key_sig_count))) 96)::(EA 96 ANone 97)::
  (EA 97 AWeaken 143)::(EA 98 (AAssign V_check_key_sig__tmp
  (Some (ENum (-10)))) 99)::(EA 99 ANone 100)::(EA 100 AWeaken 143)::
  (EA 101 (AAssign V_check_key_sig__tmp (Some (ENum (-10)))) 102)::
  (EA 102 ANone 103)::(EA 103 AWeaken 143)::(EA 104 (AAssign
  V_check_key_sig__tmp (Some (ENum (-2)))) 105)::(EA 105 ANone 106)::
  (EA 106 AWeaken 143)::(EA 107 (AAssign V_check_key_sig__tmp
  (Some (ENum (-7)))) 108)::(EA 108 ANone 109)::(EA 109 AWeaken 143)::
  (EA 110 (AAssign V_check_key_sig__tmp (Some (ENum (-6)))) 111)::
  (EA 111 ANone 112)::(EA 112 AWeaken 143)::(EA 113 AWeaken 114)::
  (EA 114 ANone 115)::(EA 115 (AAssign V_check_key_sig_i
  (Some (EAdd (EVar V_check_key_sig_i) (ENum (1))))) 116)::
  (EA 116 ANone 117)::(EA 117 ANone 118)::(EA 118 (AAssign V_check_key_sig_z
  (Some (EAdd (ENum (1)) (EVar V_check_key_sig_z)))) 119)::
  (EA 119 AWeaken 49)::(EA 120 AWeaken 121)::(EA 121 ANone 122)::
  (EA 122 (AAssign V_check_key_sig_i (Some (EAdd (EVar V_check_key_sig_i)
  (ENum (1))))) 123)::(EA 123 ANone 124)::(EA 124 ANone 125)::
  (EA 125 (AAssign V_check_key_sig_z (Some (EAdd (ENum (1))
  (EVar V_check_key_sig_z)))) 126)::(EA 126 AWeaken 44)::
  (EA 127 AWeaken 128)::(EA 128 (AAssign V_check_key_sig__tmp
  (Some (ENum (-8)))) 129)::(EA 129 ANone 130)::(EA 130 AWeaken 143)::
  (EA 131 (AAssign V_check_key_sig__tmp (Some (ENum (-8)))) 132)::
  (EA 132 ANone 133)::(EA 133 AWeaken 143)::(EA 134 (AAssign
  V_check_key_sig__tmp (Some (ENum (-8)))) 135)::(EA 135 ANone 136)::
  (EA 136 AWeaken 143)::(EA 137 ANone 140)::(EA 138 AWeaken 139)::
  (EA 139 ANone 140)::(EA 140 (AAssign V_check_key_sig__tmp
  (Some (ENum (-10)))) 141)::(EA 141 ANone 142)::(EA 142 AWeaken 143)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_check_key_sig => Pedges_check_key_sig
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_check_key_sig => 143
     end)%positive;
  var_global := var_global
}.

Definition ai_check_key_sig (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0)%Z
   | 3 => (-1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_cert_length <= 0)%Z
   | 4 => (-1 * s V_check_key_sig_cert_length <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0)%Z
   | 5 => (-1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_cert_length <= 0)%Z
   | 6 => (-1 * s V_check_key_sig_cert_length <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0)%Z
   | 7 => (-1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_cert_length <= 0)%Z
   | 8 => (-1 * s V_check_key_sig_cert_length <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 9 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_cert_length <= 0)%Z
   | 10 => (-1 * s V_check_key_sig_cert_length <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 11 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_cert_length <= 0)%Z
   | 12 => (-1 * s V_check_key_sig_cert_length <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 13 => (-1 * s V_check_key_sig_cert_length <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 14 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0)%Z
   | 15 => (1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 16 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 17 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 18 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 19 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 20 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 21 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 22 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 23 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 24 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 25 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 26 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 27 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 28 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -16 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0)%Z
   | 29 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 30 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 31 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -17 <= 0 /\ -1 * s V_check_key_sig_class + 17 <= 0)%Z
   | 32 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 33 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 34 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -18 <= 0 /\ -1 * s V_check_key_sig_class + 18 <= 0)%Z
   | 35 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 36 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 37 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -19 <= 0 /\ -1 * s V_check_key_sig_class + 19 <= 0)%Z
   | 38 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 39 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 40 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 32 <= 0)%Z
   | 41 => (-1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 42 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_i <= 0 /\ -1 * s V_check_key_sig_i <= 0)%Z
   | 43 => (-1 * s V_check_key_sig_i <= 0 /\ 1 * s V_check_key_sig_i <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 44 => (-1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_i + -4 <= 0)%Z
   | 45 => (1 * s V_check_key_sig_i + -4 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 4 <= 0)%Z
   | 46 => (-1 * s V_check_key_sig_i + 4 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_i + -4 <= 0)%Z
   | 47 => (-1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_i <= 0 /\ -1 * s V_check_key_sig_i <= 0)%Z
   | 48 => (-1 * s V_check_key_sig_i <= 0 /\ 1 * s V_check_key_sig_i <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0)%Z
   | 49 => (-1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 50 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0)%Z
   | 51 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 52 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0)%Z
   | 53 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 54 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0)%Z
   | 55 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 56 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0)%Z
   | 57 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 58 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0)%Z
   | 59 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 60 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0)%Z
   | 61 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 62 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0)%Z
   | 63 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 64 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0)%Z
   | 65 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 66 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0)%Z
   | 67 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 68 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_count <= 0)%Z
   | 69 => (-1 * s V_check_key_sig_count <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 70 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0)%Z
   | 71 => (-1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 72 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0)%Z
   | 73 => (-1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 74 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0)%Z
   | 75 => (-1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 76 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0 /\ -1 * s V_check_key_sig_class + 32 <= 0)%Z
   | 77 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_class + -31 <= 0)%Z
   | 78 => (1 * s V_check_key_sig_class + -31 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 79 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_class + -31 <= 0)%Z
   | 80 => (1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 81 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0)%Z
   | 82 => (1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0 /\ 1 * s V_check_key_sig__tmp <= 0 /\ -1 * s V_check_key_sig__tmp <= 0)%Z
   | 83 => (-1 * s V_check_key_sig__tmp <= 0 /\ 1 * s V_check_key_sig__tmp <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0)%Z
   | 84 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0)%Z
   | 85 => (1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0 /\ 1 * s V_check_key_sig__tmp + 20 <= 0 /\ -1 * s V_check_key_sig__tmp + -20 <= 0)%Z
   | 86 => (-1 * s V_check_key_sig__tmp + -20 <= 0 /\ 1 * s V_check_key_sig__tmp + 20 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0)%Z
   | 87 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0)%Z
   | 88 => (-1 * s V_check_key_sig_count + 16 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0 /\ 1 * s V_check_key_sig__tmp + 9 <= 0 /\ -1 * s V_check_key_sig__tmp + -9 <= 0)%Z
   | 89 => (-1 * s V_check_key_sig__tmp + -9 <= 0 /\ 1 * s V_check_key_sig__tmp + 9 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + -16 <= 0 /\ -1 * s V_check_key_sig_count + 16 <= 0)%Z
   | 90 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_count <= 0)%Z
   | 91 => (-1 * s V_check_key_sig_count <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 92 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_count <= 0 /\ 1 * s V_check_key_sig__tmp + 9 <= 0 /\ -1 * s V_check_key_sig__tmp + -9 <= 0)%Z
   | 93 => (-1 * s V_check_key_sig__tmp + -9 <= 0 /\ 1 * s V_check_key_sig__tmp + 9 <= 0 /\ -1 * s V_check_key_sig_count <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 94 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + 1 <= 0)%Z
   | 95 => (1 * s V_check_key_sig_count + 1 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 96 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig_count + 1 <= 0 /\ 1 * s V_check_key_sig__tmp + 1 <= 0)%Z
   | 97 => (1 * s V_check_key_sig__tmp + 1 <= 0 /\ 1 * s V_check_key_sig_count + 1 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 98 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 99 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig__tmp + 10 <= 0 /\ -1 * s V_check_key_sig__tmp + -10 <= 0)%Z
   | 100 => (-1 * s V_check_key_sig__tmp + -10 <= 0 /\ 1 * s V_check_key_sig__tmp + 10 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 101 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 102 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig__tmp + 10 <= 0 /\ -1 * s V_check_key_sig__tmp + -10 <= 0)%Z
   | 103 => (-1 * s V_check_key_sig__tmp + -10 <= 0 /\ 1 * s V_check_key_sig__tmp + 10 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 104 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 105 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig__tmp + 2 <= 0 /\ -1 * s V_check_key_sig__tmp + -2 <= 0)%Z
   | 106 => (-1 * s V_check_key_sig__tmp + -2 <= 0 /\ 1 * s V_check_key_sig__tmp + 2 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 107 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 108 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ 1 * s V_check_key_sig__tmp + 7 <= 0 /\ -1 * s V_check_key_sig__tmp + -7 <= 0)%Z
   | 109 => (-1 * s V_check_key_sig__tmp + -7 <= 0 /\ 1 * s V_check_key_sig__tmp + 7 <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 110 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0)%Z
   | 111 => (-1 * s V_check_key_sig_i + 8 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0 /\ 1 * s V_check_key_sig__tmp + 6 <= 0 /\ -1 * s V_check_key_sig__tmp + -6 <= 0)%Z
   | 112 => (-1 * s V_check_key_sig__tmp + -6 <= 0 /\ 1 * s V_check_key_sig__tmp + 6 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i + 8 <= 0)%Z
   | 113 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_i <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_i + -7 <= 0)%Z
   | 114 => (1 * s V_check_key_sig_i + -7 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 115 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_i <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_i + -7 <= 0)%Z
   | 116 => (-1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ -1 * s V_check_key_sig_i + 1 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 117 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_i + 1 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z <= 0)%Z
   | 118 => (-1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ -1 * s V_check_key_sig_i + 1 <= 0 /\ 1 * s V_check_key_sig_i + -8 <= 0)%Z
   | 119 => (1 * s V_check_key_sig_i + -8 <= 0 /\ -1 * s V_check_key_sig_i + 1 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_z + 1 <= 0)%Z
   | 120 => (-1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ -1 * s V_check_key_sig_i <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_i + -3 <= 0)%Z
   | 121 => (1 * s V_check_key_sig_i + -3 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_i <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0)%Z
   | 122 => (-1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ -1 * s V_check_key_sig_i <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_i + -3 <= 0)%Z
   | 123 => (-1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_i + 1 <= 0 /\ 1 * s V_check_key_sig_i + -4 <= 0)%Z
   | 124 => (1 * s V_check_key_sig_i + -4 <= 0 /\ -1 * s V_check_key_sig_i + 1 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ -1 * s V_check_key_sig_z <= 0)%Z
   | 125 => (-1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ -1 * s V_check_key_sig_i + 1 <= 0 /\ 1 * s V_check_key_sig_i + -4 <= 0)%Z
   | 126 => (1 * s V_check_key_sig_i + -4 <= 0 /\ -1 * s V_check_key_sig_i + 1 <= 0 /\ -1 * s V_check_key_sig_class + 16 <= 0 /\ 1 * s V_check_key_sig_class + -32 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ -1 * s V_check_key_sig_z + 1 <= 0)%Z
   | 127 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 128 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 129 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig__tmp + 8 <= 0 /\ -1 * s V_check_key_sig__tmp + -8 <= 0)%Z
   | 130 => (-1 * s V_check_key_sig__tmp + -8 <= 0 /\ 1 * s V_check_key_sig__tmp + 8 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 131 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 132 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig__tmp + 8 <= 0 /\ -1 * s V_check_key_sig__tmp + -8 <= 0)%Z
   | 133 => (-1 * s V_check_key_sig__tmp + -8 <= 0 /\ 1 * s V_check_key_sig__tmp + 8 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 134 => (1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 135 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig__tmp + 8 <= 0 /\ -1 * s V_check_key_sig__tmp + -8 <= 0)%Z
   | 136 => (-1 * s V_check_key_sig__tmp + -8 <= 0 /\ 1 * s V_check_key_sig__tmp + 8 <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 137 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_cert_length + -283 <= 0)%Z
   | 138 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_cert_length + 284 <= 0)%Z
   | 139 => (-1 * s V_check_key_sig_cert_length + 284 <= 0 /\ 1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | 140 => (-1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0)%Z
   | 141 => (1 * s V_check_key_sig_z <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig__tmp + 10 <= 0 /\ -1 * s V_check_key_sig__tmp + -10 <= 0)%Z
   | 142 => (-1 * s V_check_key_sig__tmp + -10 <= 0 /\ 1 * s V_check_key_sig__tmp + 10 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_z <= 0)%Z
   | 143 => (1 * s V_check_key_sig__tmp <= 0 /\ -1 * s V_check_key_sig_z <= 0 /\ 1 * s V_check_key_sig_global_precision + -130 <= 0 /\ -1 * s V_check_key_sig_global_precision + 130 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_check_key_sig (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((12 # 1) <= z)%Q
   | 2 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 3 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 4 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 5 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 6 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 7 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 8 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 9 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 10 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 11 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 12 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 13 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 14 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 15 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 16 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 17 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 18 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 19 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_check_key_sig_z) (0))) (F_max0_ge_0 (s V_check_key_sig_z));
      (*-0.0923077 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (130
                                                                    - s V_check_key_sig_global_precision) (0))) (F_max0_ge_0 (130
                                                                    - s V_check_key_sig_global_precision))]
     ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 20 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
            + max0(s V_check_key_sig_z) <= z)%Q
   | 21 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
            + max0(s V_check_key_sig_z) <= z)%Q
   | 22 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
            + max0(s V_check_key_sig_z) <= z)%Q
   | 23 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
            + max0(s V_check_key_sig_z) <= z)%Q
   | 24 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
            + max0(s V_check_key_sig_z) <= z)%Q
   | 25 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
            + max0(s V_check_key_sig_z) <= z)%Q
   | 26 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
            + max0(s V_check_key_sig_z) <= z)%Q
   | 27 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
            + max0(s V_check_key_sig_z) <= z)%Q
   | 28 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_check_key_sig_z)) (F_check_ge (0) (0))]
     ((6 # 65) * s V_check_key_sig_global_precision
      + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
      + max0(s V_check_key_sig_z) <= z)%Q
   | 29 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
            + max0(s V_check_key_sig_z) <= z)%Q
   | 30 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
            + max0(s V_check_key_sig_z) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_check_key_sig_z)) (F_check_ge (0) (0))]
     ((6 # 65) * s V_check_key_sig_global_precision
      + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
      + max0(s V_check_key_sig_z) <= z)%Q
   | 32 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (s V_check_key_sig_z)) (F_check_ge (0) (0))]
     ((6 # 65) * s V_check_key_sig_global_precision
      + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
      + max0(s V_check_key_sig_z) <= z)%Q
   | 33 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision) <= z)%Q
   | 34 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision) <= z)%Q
   | 35 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision) <= z)%Q
   | 36 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision) <= z)%Q
   | 37 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision) <= z)%Q
   | 38 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision) <= z)%Q
   | 39 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision) <= z)%Q
   | 40 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision) <= z)%Q
   | 41 => ((6 # 65) * s V_check_key_sig_global_precision
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision) <= z)%Q
   | 42 => (-(92 # 7) + (6 # 65) * s V_check_key_sig_global_precision
            + (8 # 7) * s V_check_key_sig_i + max0(4 - s V_check_key_sig_i)
            + (8 # 7) * max0(8 - s V_check_key_sig_i)
            + (6 # 65) * max0(130 - s V_check_key_sig_global_precision) <= z)%Q
   | 43 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_check_key_sig_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_check_key_sig_z) (0))) (F_max0_ge_0 (-
                                                                    s V_check_key_sig_z));
      (*0 0.0923077*) F_binom_monotonic 1 (F_max0_ge_arg (130
                                                          - s V_check_key_sig_global_precision)) (F_check_ge (130
                                                                    - s V_check_key_sig_global_precision) (0));
      (*-1.14286 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                         - s V_check_key_sig_i)) (F_check_ge (8
                                                                    - s V_check_key_sig_i) (0))]
     (-(92 # 7) + (6 # 65) * s V_check_key_sig_global_precision
      + (8 # 7) * s V_check_key_sig_i + max0(4 - s V_check_key_sig_i)
      + (8 # 7) * max0(8 - s V_check_key_sig_i)
      + (6 # 65) * max0(130 - s V_check_key_sig_global_precision) <= z)%Q
   | 44 => ((8 # 1) + s V_check_key_sig_z + max0(4 - s V_check_key_sig_i) <= z)%Q
   | 45 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_check_key_sig_i) (3
                                                                    - s V_check_key_sig_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_check_key_sig_i)]
     ((8 # 1) + s V_check_key_sig_z + max0(4 - s V_check_key_sig_i) <= z)%Q
   | 46 => ((8 # 1) + s V_check_key_sig_z <= z)%Q
   | 47 => ((8 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 48 => ((8 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 49 => ((8 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 50 => ((8 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 51 => ((8 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 52 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                               - s V_check_key_sig_i) (0))) (F_max0_ge_0 (8
                                                                    - s V_check_key_sig_i))]
     ((8 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 53 => (s V_check_key_sig_z + max0(8 - s V_check_key_sig_i) <= z)%Q
   | 54 => (s V_check_key_sig_z + max0(8 - s V_check_key_sig_i) <= z)%Q
   | 55 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (8 - s V_check_key_sig_i) (7
                                                                    - s V_check_key_sig_i))]
     (s V_check_key_sig_z + max0(8 - s V_check_key_sig_i) <= z)%Q
   | 56 => (s V_check_key_sig_z + max0(7 - s V_check_key_sig_i) <= z)%Q
   | 57 => (s V_check_key_sig_z + max0(7 - s V_check_key_sig_i) <= z)%Q
   | 58 => (s V_check_key_sig_z + max0(7 - s V_check_key_sig_i) <= z)%Q
   | 59 => (s V_check_key_sig_z + max0(7 - s V_check_key_sig_i) <= z)%Q
   | 60 => (s V_check_key_sig_z + max0(7 - s V_check_key_sig_i) <= z)%Q
   | 61 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (7 - s V_check_key_sig_i)) (F_check_ge (0) (0))]
     (s V_check_key_sig_z + max0(7 - s V_check_key_sig_i) <= z)%Q
   | 62 => (s V_check_key_sig_z <= z)%Q
   | 63 => (s V_check_key_sig_z <= z)%Q
   | 64 => (s V_check_key_sig_z <= z)%Q
   | 65 => (s V_check_key_sig_z <= z)%Q
   | 66 => (s V_check_key_sig_z <= z)%Q
   | 67 => (s V_check_key_sig_z <= z)%Q
   | 68 => (s V_check_key_sig_z <= z)%Q
   | 69 => (s V_check_key_sig_z <= z)%Q
   | 70 => (s V_check_key_sig_z <= z)%Q
   | 71 => (s V_check_key_sig_z <= z)%Q
   | 72 => (s V_check_key_sig_z <= z)%Q
   | 73 => (s V_check_key_sig_z <= z)%Q
   | 74 => (s V_check_key_sig_z <= z)%Q
   | 75 => (s V_check_key_sig_z <= z)%Q
   | 76 => (s V_check_key_sig_z <= z)%Q
   | 77 => (s V_check_key_sig_z <= z)%Q
   | 78 => (s V_check_key_sig_z <= z)%Q
   | 79 => (s V_check_key_sig_z <= z)%Q
   | 80 => (s V_check_key_sig_z <= z)%Q
   | 81 => (s V_check_key_sig_z <= z)%Q
   | 82 => (s V_check_key_sig_z <= z)%Q
   | 83 => (s V_check_key_sig_z <= z)%Q
   | 84 => (s V_check_key_sig_z <= z)%Q
   | 85 => (s V_check_key_sig_z <= z)%Q
   | 86 => (s V_check_key_sig_z <= z)%Q
   | 87 => (s V_check_key_sig_z <= z)%Q
   | 88 => (s V_check_key_sig_z <= z)%Q
   | 89 => (s V_check_key_sig_z <= z)%Q
   | 90 => (s V_check_key_sig_z <= z)%Q
   | 91 => (s V_check_key_sig_z <= z)%Q
   | 92 => (s V_check_key_sig_z <= z)%Q
   | 93 => (s V_check_key_sig_z <= z)%Q
   | 94 => (s V_check_key_sig_z <= z)%Q
   | 95 => (s V_check_key_sig_z <= z)%Q
   | 96 => (s V_check_key_sig_z <= z)%Q
   | 97 => (s V_check_key_sig_z <= z)%Q
   | 98 => (s V_check_key_sig_z <= z)%Q
   | 99 => (s V_check_key_sig_z <= z)%Q
   | 100 => (s V_check_key_sig_z <= z)%Q
   | 101 => (s V_check_key_sig_z <= z)%Q
   | 102 => (s V_check_key_sig_z <= z)%Q
   | 103 => (s V_check_key_sig_z <= z)%Q
   | 104 => (s V_check_key_sig_z + max0(7 - s V_check_key_sig_i) <= z)%Q
   | 105 => (s V_check_key_sig_z + max0(7 - s V_check_key_sig_i) <= z)%Q
   | 106 => hints
     [(*-1 0*) F_max0_ge_0 (7 - s V_check_key_sig_i)]
     (s V_check_key_sig_z + max0(7 - s V_check_key_sig_i) <= z)%Q
   | 107 => (s V_check_key_sig_z + max0(7 - s V_check_key_sig_i) <= z)%Q
   | 108 => (s V_check_key_sig_z + max0(7 - s V_check_key_sig_i) <= z)%Q
   | 109 => hints
     [(*-1 0*) F_max0_ge_0 (7 - s V_check_key_sig_i)]
     (s V_check_key_sig_z + max0(7 - s V_check_key_sig_i) <= z)%Q
   | 110 => (s V_check_key_sig_z + max0(8 - s V_check_key_sig_i) <= z)%Q
   | 111 => (s V_check_key_sig_z + max0(8 - s V_check_key_sig_i) <= z)%Q
   | 112 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_check_key_sig_i) (7
                                                                    - s V_check_key_sig_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_check_key_sig_i)]
     (s V_check_key_sig_z + max0(8 - s V_check_key_sig_i) <= z)%Q
   | 113 => ((8 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 114 => ((8 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 115 => ((8 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 116 => ((9 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 117 => ((9 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 118 => ((9 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 119 => ((8 # 1) - s V_check_key_sig_i + s V_check_key_sig_z <= z)%Q
   | 120 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_check_key_sig_i) (1)]
     ((8 # 1) + s V_check_key_sig_z + max0(4 - s V_check_key_sig_i) <= z)%Q
   | 121 => ((9 # 1) + s V_check_key_sig_z + max0(3 - s V_check_key_sig_i) <= z)%Q
   | 122 => ((9 # 1) + s V_check_key_sig_z + max0(3 - s V_check_key_sig_i) <= z)%Q
   | 123 => ((9 # 1) + s V_check_key_sig_z + max0(4 - s V_check_key_sig_i) <= z)%Q
   | 124 => ((9 # 1) + s V_check_key_sig_z + max0(4 - s V_check_key_sig_i) <= z)%Q
   | 125 => ((9 # 1) + s V_check_key_sig_z + max0(4 - s V_check_key_sig_i) <= z)%Q
   | 126 => ((8 # 1) + s V_check_key_sig_z + max0(4 - s V_check_key_sig_i) <= z)%Q
   | 127 => hints
     [(*0 12*) F_one;
      (*0 0.0923077*) F_binom_monotonic 1 (F_max0_ge_arg (130
                                                          - s V_check_key_sig_global_precision)) (F_check_ge (130
                                                                    - s V_check_key_sig_global_precision) (0))]
     ((6 # 65) * s V_check_key_sig_global_precision
      + (6 # 65) * max0(130 - s V_check_key_sig_global_precision) <= z)%Q
   | 128 => (0 <= z)%Q
   | 129 => (0 <= z)%Q
   | 130 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_check_key_sig_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_check_key_sig_z) (0))) (F_max0_ge_0 (-
                                                                    s V_check_key_sig_z))]
     (0 <= z)%Q
   | 131 => ((6 # 65) * s V_check_key_sig_global_precision
             + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
             + max0(s V_check_key_sig_z) <= z)%Q
   | 132 => (-(12 # 1) + (6 # 65) * s V_check_key_sig_global_precision
             + (12 # 7) * max0(-1 - s V_check_key_sig__tmp)
             + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
             + max0(s V_check_key_sig_z) <= z)%Q
   | 133 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_check_key_sig_z)) (F_check_ge (s V_check_key_sig_z) (0));
      (*-0.0923077 0*) F_binom_monotonic 1 (F_max0_ge_arg (130
                                                           - s V_check_key_sig_global_precision)) (F_check_ge (130
                                                                    - s V_check_key_sig_global_precision) (0));
      (*-1.71429 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                       - s V_check_key_sig__tmp)) (F_check_ge (0) (0))]
     (-(12 # 1) + (6 # 65) * s V_check_key_sig_global_precision
      + (12 # 7) * max0(-1 - s V_check_key_sig__tmp)
      + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
      + max0(s V_check_key_sig_z) <= z)%Q
   | 134 => ((6 # 65) * s V_check_key_sig_global_precision
             + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
             + max0(s V_check_key_sig_z) <= z)%Q
   | 135 => (-(12 # 1) + (6 # 65) * s V_check_key_sig_global_precision
             + (12 # 7) * max0(-1 - s V_check_key_sig__tmp)
             + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
             + max0(s V_check_key_sig_z) <= z)%Q
   | 136 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_check_key_sig_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_check_key_sig_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_check_key_sig_z) (0))) (F_max0_ge_0 (-
                                                                    s V_check_key_sig_z));
      (*-0.0923077 0*) F_binom_monotonic 1 (F_max0_ge_arg (130
                                                           - s V_check_key_sig_global_precision)) (F_check_ge (130
                                                                    - s V_check_key_sig_global_precision) (0));
      (*-1.71429 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                       - s V_check_key_sig__tmp)) (F_check_ge (0) (0))]
     (-(12 # 1) + (6 # 65) * s V_check_key_sig_global_precision
      + (12 # 7) * max0(-1 - s V_check_key_sig__tmp)
      + (6 # 65) * max0(130 - s V_check_key_sig_global_precision)
      + max0(s V_check_key_sig_z) <= z)%Q
   | 137 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 138 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 139 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 140 => ((12 # 1) + s V_check_key_sig_z <= z)%Q
   | 141 => (s V_check_key_sig_z
             + (3 # 1) * max0(-6 - s V_check_key_sig__tmp) <= z)%Q
   | 142 => hints
     [(*-3 0*) F_binom_monotonic 1 (F_max0_ge_0 (-6 - s V_check_key_sig__tmp)) (F_check_ge (0) (0))]
     (s V_check_key_sig_z + (3 # 1) * max0(-6 - s V_check_key_sig__tmp) <= z)%Q
   | 143 => (s V_check_key_sig_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_check_key_sig =>
    [mkPA Q (fun n z s => ai_check_key_sig n s /\ annot0_check_key_sig n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_check_key_sig (proc_start P_check_key_sig) s1 (proc_end P_check_key_sig) s2 ->
    (s2 V_check_key_sig_z <= (12 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_check_key_sig.
Qed.
