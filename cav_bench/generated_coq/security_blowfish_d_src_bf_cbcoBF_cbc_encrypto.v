Require Import pasta.Pasta.

Inductive proc: Type :=
  P_BF_cbc_encrypt.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_BF_cbc_encrypt_z := 1%positive.
Notation V_BF_cbc_encrypt__tmp := 2%positive.
Notation V_BF_cbc_encrypt__tmp1 := 3%positive.
Notation V_BF_cbc_encrypt_l := 4%positive.
Notation V_BF_cbc_encrypt_tin0 := 5%positive.
Notation V_BF_cbc_encrypt_tin1 := 6%positive.
Notation V_BF_cbc_encrypt_tout0 := 7%positive.
Notation V_BF_cbc_encrypt_tout1 := 8%positive.
Notation V_BF_cbc_encrypt_xor0 := 9%positive.
Notation V_BF_cbc_encrypt_xor1 := 10%positive.
Notation V_BF_cbc_encrypt_encrypt := 11%positive.
Notation V_BF_cbc_encrypt_in := 12%positive.
Notation V_BF_cbc_encrypt_iv := 13%positive.
Notation V_BF_cbc_encrypt_ks := 14%positive.
Notation V_BF_cbc_encrypt_length := 15%positive.
Notation V_BF_cbc_encrypt_out := 16%positive.
Definition Pedges_BF_cbc_encrypt: list (edge proc) :=
  (EA 1 (AAssign V_BF_cbc_encrypt_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_BF_cbc_encrypt__tmp1 (Some (EVar V_BF_cbc_encrypt_length))) 3)::
  (EA 3 (AAssign V_BF_cbc_encrypt__tmp
  (Some (EVar V_BF_cbc_encrypt_encrypt))) 4)::(EA 4 (AAssign
  V_BF_cbc_encrypt_l (Some (EVar V_BF_cbc_encrypt__tmp1))) 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_BF_cbc_encrypt__tmp) s) <> (eval (ENum (0))
  s))%Z)) 67)::(EA 6 (AGuard (fun s => ((eval (EVar V_BF_cbc_encrypt__tmp)
  s) = (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_BF_cbc_encrypt_xor0 None) 9)::(EA 9 (AAssign V_BF_cbc_encrypt_xor0
  None) 10)::(EA 10 (AAssign V_BF_cbc_encrypt_xor0 None) 11)::(EA 11 (AAssign
  V_BF_cbc_encrypt_xor0 None) 12)::(EA 12 (AAssign V_BF_cbc_encrypt_xor1
  None) 13)::(EA 13 (AAssign V_BF_cbc_encrypt_xor1 None) 14)::(EA 14 (AAssign
  V_BF_cbc_encrypt_xor1 None) 15)::(EA 15 (AAssign V_BF_cbc_encrypt_xor1
  None) 16)::(EA 16 (AAssign V_BF_cbc_encrypt_l
  (Some (ESub (EVar V_BF_cbc_encrypt_l) (ENum (8))))) 17)::(EA 17 ANone 18)::
  (EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_BF_cbc_encrypt_l) s) >= (eval (ENum (0))
  s))%Z)) 48)::(EA 19 (AGuard (fun s => ((eval (EVar V_BF_cbc_encrypt_l) s) <
  (eval (ENum (0)) s))%Z)) 20)::(EA 20 AWeaken 21)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_BF_cbc_encrypt_l) s) <> (eval (ENum (-8))
  s))%Z)) 23)::(EA 21 (AGuard (fun s => ((eval (EVar V_BF_cbc_encrypt_l) s) =
  (eval (ENum (-8)) s))%Z)) 22)::(EA 22 AWeaken 47)::(EA 23 AWeaken 24)::
  (EA 24 (AAssign V_BF_cbc_encrypt_tin0 None) 25)::(EA 25 (AAssign
  V_BF_cbc_encrypt_tin0 None) 26)::(EA 26 (AAssign V_BF_cbc_encrypt_tin0
  None) 27)::(EA 27 (AAssign V_BF_cbc_encrypt_tin0 None) 28)::(EA 28 (AAssign
  V_BF_cbc_encrypt_tin1 None) 29)::(EA 29 (AAssign V_BF_cbc_encrypt_tin1
  None) 30)::(EA 30 (AAssign V_BF_cbc_encrypt_tin1 None) 31)::(EA 31 (AAssign
  V_BF_cbc_encrypt_tin1 None) 32)::(EA 32 (AAssign V_BF_cbc_encrypt_tout0
  None) 33)::(EA 33 (AAssign V_BF_cbc_encrypt_tout1 None) 34)::
  (EA 34 AWeaken 35)::(EA 35 ANone 44)::(EA 35 ANone 36)::(EA 35 ANone 37)::
  (EA 35 ANone 38)::(EA 35 ANone 39)::(EA 35 ANone 40)::(EA 35 ANone 41)::
  (EA 35 ANone 42)::(EA 35 ANone 43)::(EA 36 ANone 37)::(EA 37 ANone 38)::
  (EA 38 ANone 39)::(EA 39 ANone 40)::(EA 40 ANone 41)::(EA 41 ANone 42)::
  (EA 42 ANone 43)::(EA 43 ANone 44)::(EA 44 (AAssign V_BF_cbc_encrypt_xor0
  (Some (EVar V_BF_cbc_encrypt_tin0))) 45)::(EA 45 (AAssign
  V_BF_cbc_encrypt_xor1 (Some (EVar V_BF_cbc_encrypt_tin1))) 46)::
  (EA 46 ANone 47)::(EA 47 ANone 110)::(EA 48 AWeaken 49)::(EA 49 (AAssign
  V_BF_cbc_encrypt_tin0 None) 50)::(EA 50 (AAssign V_BF_cbc_encrypt_tin0
  None) 51)::(EA 51 (AAssign V_BF_cbc_encrypt_tin0 None) 52)::(EA 52 (AAssign
  V_BF_cbc_encrypt_tin0 None) 53)::(EA 53 (AAssign V_BF_cbc_encrypt_tin1
  None) 54)::(EA 54 (AAssign V_BF_cbc_encrypt_tin1 None) 55)::(EA 55 (AAssign
  V_BF_cbc_encrypt_tin1 None) 56)::(EA 56 (AAssign V_BF_cbc_encrypt_tin1
  None) 57)::(EA 57 (AAssign V_BF_cbc_encrypt_tout0 None) 58)::
  (EA 58 (AAssign V_BF_cbc_encrypt_tout1 None) 59)::(EA 59 (AAssign
  V_BF_cbc_encrypt_xor0 (Some (EVar V_BF_cbc_encrypt_tin0))) 60)::
  (EA 60 (AAssign V_BF_cbc_encrypt_xor1
  (Some (EVar V_BF_cbc_encrypt_tin1))) 61)::(EA 61 ANone 62)::(EA 62 (AAssign
  V_BF_cbc_encrypt_l (Some (ESub (EVar V_BF_cbc_encrypt_l)
  (ENum (8))))) 63)::(EA 63 ANone 64)::(EA 64 ANone 65)::(EA 65 (AAssign
  V_BF_cbc_encrypt_z (Some (EAdd (ENum (1))
  (EVar V_BF_cbc_encrypt_z)))) 66)::(EA 66 AWeaken 19)::(EA 67 AWeaken 68)::
  (EA 68 (AAssign V_BF_cbc_encrypt_tout0 None) 69)::(EA 69 (AAssign
  V_BF_cbc_encrypt_tout0 None) 70)::(EA 70 (AAssign V_BF_cbc_encrypt_tout0
  None) 71)::(EA 71 (AAssign V_BF_cbc_encrypt_tout0 None) 72)::
  (EA 72 (AAssign V_BF_cbc_encrypt_tout1 None) 73)::(EA 73 (AAssign
  V_BF_cbc_encrypt_tout1 None) 74)::(EA 74 (AAssign V_BF_cbc_encrypt_tout1
  None) 75)::(EA 75 (AAssign V_BF_cbc_encrypt_tout1 None) 76)::
  (EA 76 (AAssign V_BF_cbc_encrypt_l (Some (ESub (EVar V_BF_cbc_encrypt_l)
  (ENum (8))))) 77)::(EA 77 ANone 78)::(EA 78 AWeaken 79)::(EA 79 (AGuard
  (fun s => ((eval (EVar V_BF_cbc_encrypt_l) s) >= (eval (ENum (0))
  s))%Z)) 118)::(EA 79 (AGuard (fun s => ((eval (EVar V_BF_cbc_encrypt_l)
  s) < (eval (ENum (0)) s))%Z)) 80)::(EA 80 AWeaken 81)::(EA 81 (AGuard
  (fun s => ((eval (EVar V_BF_cbc_encrypt_l) s) <> (eval (ENum (-8))
  s))%Z)) 83)::(EA 81 (AGuard (fun s => ((eval (EVar V_BF_cbc_encrypt_l) s) =
  (eval (ENum (-8)) s))%Z)) 82)::(EA 82 AWeaken 109)::(EA 83 AWeaken 84)::
  (EA 84 (AAssign V_BF_cbc_encrypt_tin1 (Some (ENum (0)))) 85)::
  (EA 85 (AAssign V_BF_cbc_encrypt_tin0 (Some (ENum (0)))) 86)::
  (EA 86 AWeaken 87)::(EA 87 ANone 104)::(EA 87 ANone 88)::(EA 87 ANone 90)::
  (EA 87 ANone 92)::(EA 87 ANone 94)::(EA 87 ANone 96)::(EA 87 ANone 98)::
  (EA 87 ANone 100)::(EA 87 ANone 102)::(EA 88 (AAssign V_BF_cbc_encrypt_tin1
  None) 89)::(EA 89 ANone 90)::(EA 90 (AAssign V_BF_cbc_encrypt_tin1
  None) 91)::(EA 91 ANone 92)::(EA 92 (AAssign V_BF_cbc_encrypt_tin1
  None) 93)::(EA 93 ANone 94)::(EA 94 (AAssign V_BF_cbc_encrypt_tin1
  None) 95)::(EA 95 ANone 96)::(EA 96 (AAssign V_BF_cbc_encrypt_tin0
  None) 97)::(EA 97 ANone 98)::(EA 98 (AAssign V_BF_cbc_encrypt_tin0
  None) 99)::(EA 99 ANone 100)::(EA 100 (AAssign V_BF_cbc_encrypt_tin0
  None) 101)::(EA 101 ANone 102)::(EA 102 (AAssign V_BF_cbc_encrypt_tin0
  None) 103)::(EA 103 ANone 104)::(EA 104 (AAssign V_BF_cbc_encrypt_tin0
  None) 105)::(EA 105 (AAssign V_BF_cbc_encrypt_tin1 None) 106)::
  (EA 106 (AAssign V_BF_cbc_encrypt_tout0 None) 107)::(EA 107 (AAssign
  V_BF_cbc_encrypt_tout1 None) 108)::(EA 108 ANone 109)::(EA 109 ANone 110)::
  (EA 110 (AAssign V_BF_cbc_encrypt_xor1 (Some (ENum (0)))) 111)::
  (EA 111 (AAssign V_BF_cbc_encrypt_xor0 (Some (ENum (0)))) 112)::
  (EA 112 (AAssign V_BF_cbc_encrypt_tout1 (Some (ENum (0)))) 113)::
  (EA 113 (AAssign V_BF_cbc_encrypt_tout0 (Some (ENum (0)))) 114)::
  (EA 114 (AAssign V_BF_cbc_encrypt_tin1 (Some (ENum (0)))) 115)::
  (EA 115 (AAssign V_BF_cbc_encrypt_tin0 (Some (ENum (0)))) 116)::
  (EA 116 AWeaken 117)::(EA 118 AWeaken 119)::(EA 119 (AAssign
  V_BF_cbc_encrypt_tin0 None) 120)::(EA 120 (AAssign V_BF_cbc_encrypt_tin0
  None) 121)::(EA 121 (AAssign V_BF_cbc_encrypt_tin0 None) 122)::
  (EA 122 (AAssign V_BF_cbc_encrypt_tin0 None) 123)::(EA 123 (AAssign
  V_BF_cbc_encrypt_tin1 None) 124)::(EA 124 (AAssign V_BF_cbc_encrypt_tin1
  None) 125)::(EA 125 (AAssign V_BF_cbc_encrypt_tin1 None) 126)::
  (EA 126 (AAssign V_BF_cbc_encrypt_tin1 None) 127)::(EA 127 (AAssign
  V_BF_cbc_encrypt_tin0 None) 128)::(EA 128 (AAssign V_BF_cbc_encrypt_tin1
  None) 129)::(EA 129 (AAssign V_BF_cbc_encrypt_tout0 None) 130)::
  (EA 130 (AAssign V_BF_cbc_encrypt_tout1 None) 131)::(EA 131 ANone 132)::
  (EA 132 (AAssign V_BF_cbc_encrypt_l (Some (ESub (EVar V_BF_cbc_encrypt_l)
  (ENum (8))))) 133)::(EA 133 ANone 134)::(EA 134 ANone 135)::
  (EA 135 (AAssign V_BF_cbc_encrypt_z (Some (EAdd (ENum (1))
  (EVar V_BF_cbc_encrypt_z)))) 136)::(EA 136 AWeaken 79)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_BF_cbc_encrypt => Pedges_BF_cbc_encrypt
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_BF_cbc_encrypt => 117
     end)%positive;
  var_global := var_global
}.

Definition ai_BF_cbc_encrypt (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 3 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 4 => (1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 5 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 6 => (1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 7 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 8 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 9 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 10 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 11 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 12 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 13 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 14 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 15 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 16 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 17 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 18 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 19 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 20 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 21 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 22 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 8 <= 0 /\ -1 * s V_BF_cbc_encrypt_l + -8 <= 0)%Z
   | 23 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 24 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 25 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 26 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 27 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 28 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 29 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 30 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 31 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 32 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 33 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 34 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 35 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 36 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 37 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 38 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 39 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 40 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 41 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 42 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 43 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 44 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 45 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 46 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 47 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 48 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 49 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 50 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 51 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 52 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 53 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 54 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 55 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 56 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 57 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 58 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 59 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 60 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 61 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0)%Z
   | 62 => (-1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 63 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_l + -8 <= 0)%Z
   | 64 => (-1 * s V_BF_cbc_encrypt_l + -8 <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 65 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_l + -8 <= 0)%Z
   | 66 => (-1 * s V_BF_cbc_encrypt_l + -8 <= 0 /\ -1 * s V_BF_cbc_encrypt__tmp <= 0 /\ 1 * s V_BF_cbc_encrypt__tmp <= 0 /\ -1 * s V_BF_cbc_encrypt_z + 1 <= 0)%Z
   | 67 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 68 => (1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 69 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 70 => (1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 71 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 72 => (1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 73 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 74 => (1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 75 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 76 => (1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 77 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 78 => (1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 79 => (-1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 80 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 81 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 82 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 8 <= 0 /\ -1 * s V_BF_cbc_encrypt_l + -8 <= 0)%Z
   | 83 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 84 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 85 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin1 <= 0 /\ -1 * s V_BF_cbc_encrypt_tin1 <= 0)%Z
   | 86 => (-1 * s V_BF_cbc_encrypt_tin1 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin1 <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_tin0 <= 0)%Z
   | 87 => (-1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin1 <= 0 /\ -1 * s V_BF_cbc_encrypt_tin1 <= 0)%Z
   | 88 => (-1 * s V_BF_cbc_encrypt_tin1 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin1 <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_tin0 <= 0)%Z
   | 89 => (-1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 90 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_tin0 <= 0)%Z
   | 91 => (-1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 92 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_tin0 <= 0)%Z
   | 93 => (-1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 94 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_tin0 <= 0)%Z
   | 95 => (-1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 96 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_tin0 <= 0)%Z
   | 97 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 98 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 99 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 100 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 101 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 102 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 103 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 104 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 105 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 106 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 107 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 108 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 109 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0)%Z
   | 110 => (1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 111 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ 1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ -1 * s V_BF_cbc_encrypt_xor1 <= 0)%Z
   | 112 => (-1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ 1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_xor0 <= 0 /\ -1 * s V_BF_cbc_encrypt_xor0 <= 0)%Z
   | 113 => (-1 * s V_BF_cbc_encrypt_xor0 <= 0 /\ 1 * s V_BF_cbc_encrypt_xor0 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ 1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ -1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ 1 * s V_BF_cbc_encrypt_tout1 <= 0 /\ -1 * s V_BF_cbc_encrypt_tout1 <= 0)%Z
   | 114 => (-1 * s V_BF_cbc_encrypt_tout1 <= 0 /\ 1 * s V_BF_cbc_encrypt_tout1 <= 0 /\ -1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ 1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_xor0 <= 0 /\ -1 * s V_BF_cbc_encrypt_xor0 <= 0 /\ 1 * s V_BF_cbc_encrypt_tout0 <= 0 /\ -1 * s V_BF_cbc_encrypt_tout0 <= 0)%Z
   | 115 => (-1 * s V_BF_cbc_encrypt_tout0 <= 0 /\ 1 * s V_BF_cbc_encrypt_tout0 <= 0 /\ -1 * s V_BF_cbc_encrypt_xor0 <= 0 /\ 1 * s V_BF_cbc_encrypt_xor0 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ 1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ -1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ 1 * s V_BF_cbc_encrypt_tout1 <= 0 /\ -1 * s V_BF_cbc_encrypt_tout1 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin1 <= 0 /\ -1 * s V_BF_cbc_encrypt_tin1 <= 0)%Z
   | 116 => (-1 * s V_BF_cbc_encrypt_tin1 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin1 <= 0 /\ -1 * s V_BF_cbc_encrypt_tout1 <= 0 /\ 1 * s V_BF_cbc_encrypt_tout1 <= 0 /\ -1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ 1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_xor0 <= 0 /\ -1 * s V_BF_cbc_encrypt_xor0 <= 0 /\ 1 * s V_BF_cbc_encrypt_tout0 <= 0 /\ -1 * s V_BF_cbc_encrypt_tout0 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_tin0 <= 0)%Z
   | 117 => (-1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin0 <= 0 /\ -1 * s V_BF_cbc_encrypt_tout0 <= 0 /\ 1 * s V_BF_cbc_encrypt_tout0 <= 0 /\ -1 * s V_BF_cbc_encrypt_xor0 <= 0 /\ 1 * s V_BF_cbc_encrypt_xor0 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0 /\ 1 * s V_BF_cbc_encrypt_l + 1 <= 0 /\ 1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ -1 * s V_BF_cbc_encrypt_xor1 <= 0 /\ 1 * s V_BF_cbc_encrypt_tout1 <= 0 /\ -1 * s V_BF_cbc_encrypt_tout1 <= 0 /\ 1 * s V_BF_cbc_encrypt_tin1 <= 0 /\ -1 * s V_BF_cbc_encrypt_tin1 <= 0)%Z
   | 118 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 119 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 120 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 121 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 122 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 123 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 124 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 125 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 126 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 127 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 128 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 129 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 130 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 131 => (-1 * s V_BF_cbc_encrypt_l <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 132 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l <= 0)%Z
   | 133 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l + -8 <= 0)%Z
   | 134 => (-1 * s V_BF_cbc_encrypt_l + -8 <= 0 /\ -1 * s V_BF_cbc_encrypt_z <= 0)%Z
   | 135 => (-1 * s V_BF_cbc_encrypt_z <= 0 /\ -1 * s V_BF_cbc_encrypt_l + -8 <= 0)%Z
   | 136 => (-1 * s V_BF_cbc_encrypt_l + -8 <= 0 /\ -1 * s V_BF_cbc_encrypt_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_BF_cbc_encrypt (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 8) * max0(s V_BF_cbc_encrypt_length) <= z)%Q
   | 2 => ((1 # 8) * max0(s V_BF_cbc_encrypt_length) <= z)%Q
   | 3 => ((1 # 8) * max0(s V_BF_cbc_encrypt__tmp1) <= z)%Q
   | 4 => ((1 # 8) * max0(s V_BF_cbc_encrypt__tmp1) <= z)%Q
   | 5 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 6 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 7 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 8 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 9 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 10 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 11 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 12 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 13 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 14 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 15 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 16 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 17 => ((1 # 8) * max0(8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_BF_cbc_encrypt_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_BF_cbc_encrypt_z) (0))) (F_max0_ge_0 (-
                                                                    s V_BF_cbc_encrypt_z))]
     ((1 # 8) * max0(8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 19 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 20 => hints
     [(*0 0.125*) F_max0_monotonic (F_check_ge (8 + s V_BF_cbc_encrypt_l) (s V_BF_cbc_encrypt_l))]
     (s V_BF_cbc_encrypt_z + (1 # 8) * max0(8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 21 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 22 => hints
     [(*-0.125 0*) F_max0_monotonic (F_check_ge (s V_BF_cbc_encrypt_l) (-8
                                                                    + s V_BF_cbc_encrypt_l))]
     (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 23 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 24 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 25 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 26 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 27 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 28 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 29 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 30 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 31 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 32 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 33 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 34 => hints
     [(*-0.125 0*) F_max0_monotonic (F_check_ge (s V_BF_cbc_encrypt_l) (-8
                                                                    + s V_BF_cbc_encrypt_l))]
     (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 35 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 36 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 37 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 38 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 39 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 40 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 41 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 42 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 43 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 44 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 45 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 46 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 47 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 48 => hints
     [(*0 0.125*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                      + s V_BF_cbc_encrypt_l)) (F_check_ge (8
                                                                    + s V_BF_cbc_encrypt_l) (0))]
     (s V_BF_cbc_encrypt_z + (1 # 8) * max0(8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 49 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 50 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 51 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 52 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 53 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 54 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 55 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 56 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 57 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 58 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 59 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 60 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 61 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 62 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 63 => ((2 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 64 => ((2 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 65 => ((2 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 66 => hints
     [(*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                   + 
                                                                   s V_BF_cbc_encrypt_l) (0))) (F_max0_ge_0 (8
                                                                    + s V_BF_cbc_encrypt_l))]
     ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 67 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 68 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 69 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 70 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 71 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 72 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 73 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 74 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 75 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 76 => ((1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 77 => ((1 # 8) * max0(8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 78 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_BF_cbc_encrypt_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_BF_cbc_encrypt_z) (0))) (F_max0_ge_0 (-
                                                                    s V_BF_cbc_encrypt_z))]
     ((1 # 8) * max0(8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 79 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 80 => hints
     [(*0 0.125*) F_max0_monotonic (F_check_ge (8 + s V_BF_cbc_encrypt_l) (s V_BF_cbc_encrypt_l))]
     (s V_BF_cbc_encrypt_z + (1 # 8) * max0(8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 81 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 82 => hints
     [(*-0.125 0*) F_max0_monotonic (F_check_ge (s V_BF_cbc_encrypt_l) (-8
                                                                    + s V_BF_cbc_encrypt_l))]
     (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 83 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 84 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 85 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 86 => hints
     [(*-0.125 0*) F_max0_monotonic (F_check_ge (s V_BF_cbc_encrypt_l) (-8
                                                                    + s V_BF_cbc_encrypt_l))]
     (s V_BF_cbc_encrypt_z + (1 # 8) * max0(s V_BF_cbc_encrypt_l) <= z)%Q
   | 87 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 88 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 89 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 90 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 91 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 92 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 93 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 94 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 95 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 96 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 97 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 98 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 99 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 100 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 101 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 102 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 103 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 104 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 105 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 106 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 107 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 108 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 109 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 110 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 111 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 112 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 113 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 114 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 115 => (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 116 => hints
     [(*-0.125 0*) F_max0_ge_0 (-8 + s V_BF_cbc_encrypt_l)]
     (s V_BF_cbc_encrypt_z + (1 # 8) * max0(-8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 117 => (s V_BF_cbc_encrypt_z <= z)%Q
   | 118 => hints
     [(*0 0.125*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                      + s V_BF_cbc_encrypt_l)) (F_check_ge (8
                                                                    + s V_BF_cbc_encrypt_l) (0))]
     (s V_BF_cbc_encrypt_z + (1 # 8) * max0(8 + s V_BF_cbc_encrypt_l) <= z)%Q
   | 119 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 120 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 121 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 122 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 123 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 124 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 125 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 126 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 127 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 128 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 129 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 130 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 131 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 132 => ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 133 => ((2 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 134 => ((2 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 135 => ((2 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | 136 => hints
     [(*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                   + 
                                                                   s V_BF_cbc_encrypt_l) (0))) (F_max0_ge_0 (8
                                                                    + s V_BF_cbc_encrypt_l))]
     ((1 # 1) + (1 # 8) * s V_BF_cbc_encrypt_l + s V_BF_cbc_encrypt_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_BF_cbc_encrypt =>
    [mkPA Q (fun n z s => ai_BF_cbc_encrypt n s /\ annot0_BF_cbc_encrypt n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_BF_cbc_encrypt (proc_start P_BF_cbc_encrypt) s1 (proc_end P_BF_cbc_encrypt) s2 ->
    (s2 V_BF_cbc_encrypt_z <= (1 # 8) * max0(s1 V_BF_cbc_encrypt_length))%Q.
Proof.
  prove_bound ipa admissible_ipa P_BF_cbc_encrypt.
Qed.
