Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ideaCipher.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ideaCipher_z := 1%positive.
Notation V_ideaCipher_r := 2%positive.
Notation V_ideaCipher_s2 := 3%positive.
Notation V_ideaCipher_s3 := 4%positive.
Notation V_ideaCipher_t16 := 5%positive.
Notation V_ideaCipher_t32 := 6%positive.
Notation V_ideaCipher_x1 := 7%positive.
Notation V_ideaCipher_x2 := 8%positive.
Notation V_ideaCipher_x3 := 9%positive.
Notation V_ideaCipher_x4 := 10%positive.
Notation V_ideaCipher_inbuf := 11%positive.
Notation V_ideaCipher_key := 12%positive.
Notation V_ideaCipher_outbuf := 13%positive.
Definition Pedges_ideaCipher: list (edge proc) :=
  (EA 1 (AAssign V_ideaCipher_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_ideaCipher_r (Some (ENum (8)))) 3)::(EA 3 (AAssign V_ideaCipher_x1
  None) 4)::(EA 4 (AAssign V_ideaCipher_x2 None) 5)::(EA 5 (AAssign
  V_ideaCipher_x3 None) 6)::(EA 6 (AAssign V_ideaCipher_x4 None) 7)::
  (EA 7 (AAssign V_ideaCipher_x1 None) 8)::(EA 8 (AAssign V_ideaCipher_x2
  None) 9)::(EA 9 (AAssign V_ideaCipher_x3 None) 10)::(EA 10 (AAssign
  V_ideaCipher_x4 None) 11)::(EA 11 ANone 12)::(EA 12 (AAssign
  V_ideaCipher_t16 None) 13)::(EA 13 AWeaken 14)::(EA 14 ANone 17)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_ideaCipher_x1 (Some (ESub (ENum (1))
  (EVar V_ideaCipher_x1)))) 16)::(EA 16 ANone 30)::(EA 17 (AAssign
  V_ideaCipher_x1 (Some (EVar V_ideaCipher_x1))) 18)::(EA 18 AWeaken 19)::
  (EA 19 (AGuard (fun s => ((eval (EVar V_ideaCipher_x1) s) <>
  (eval (ENum (0)) s))%Z)) 23)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_ideaCipher_x1) s) = (eval (ENum (0)) s))%Z)) 20)::
  (EA 20 AWeaken 21)::(EA 21 (AAssign V_ideaCipher_x1 (Some (ESub (ENum (1))
  (EVar V_ideaCipher_t16)))) 22)::(EA 22 ANone 29)::(EA 23 AWeaken 24)::
  (EA 24 (AAssign V_ideaCipher_t32 (Some (EMul (EVar V_ideaCipher_x1)
  (EVar V_ideaCipher_t16)))) 25)::(EA 25 (AAssign V_ideaCipher_x1
  (Some (EVar V_ideaCipher_t32))) 26)::(EA 26 (AAssign V_ideaCipher_t16
  None) 27)::(EA 27 (AAssign V_ideaCipher_x1 None) 28)::(EA 28 ANone 29)::
  (EA 29 ANone 30)::(EA 30 (AAssign V_ideaCipher_x2 None) 31)::
  (EA 31 (AAssign V_ideaCipher_x3 None) 32)::(EA 32 (AAssign V_ideaCipher_t16
  None) 33)::(EA 33 AWeaken 34)::(EA 34 ANone 37)::(EA 34 ANone 35)::
  (EA 35 (AAssign V_ideaCipher_x4 (Some (ESub (ENum (1))
  (EVar V_ideaCipher_x4)))) 36)::(EA 36 ANone 50)::(EA 37 (AAssign
  V_ideaCipher_x4 (Some (EVar V_ideaCipher_x4))) 38)::(EA 38 AWeaken 39)::
  (EA 39 (AGuard (fun s => ((eval (EVar V_ideaCipher_x4) s) <>
  (eval (ENum (0)) s))%Z)) 43)::(EA 39 (AGuard
  (fun s => ((eval (EVar V_ideaCipher_x4) s) = (eval (ENum (0)) s))%Z)) 40)::
  (EA 40 AWeaken 41)::(EA 41 (AAssign V_ideaCipher_x4 (Some (ESub (ENum (1))
  (EVar V_ideaCipher_t16)))) 42)::(EA 42 ANone 49)::(EA 43 AWeaken 44)::
  (EA 44 (AAssign V_ideaCipher_t32 (Some (EMul (EVar V_ideaCipher_x4)
  (EVar V_ideaCipher_t16)))) 45)::(EA 45 (AAssign V_ideaCipher_x4
  (Some (EVar V_ideaCipher_t32))) 46)::(EA 46 (AAssign V_ideaCipher_t16
  None) 47)::(EA 47 (AAssign V_ideaCipher_x4 None) 48)::(EA 48 ANone 49)::
  (EA 49 ANone 50)::(EA 50 (AAssign V_ideaCipher_s3
  (Some (EVar V_ideaCipher_x3))) 51)::(EA 51 (AAssign V_ideaCipher_x3
  None) 52)::(EA 52 (AAssign V_ideaCipher_t16 None) 53)::(EA 53 AWeaken 54)::
  (EA 54 ANone 57)::(EA 54 ANone 55)::(EA 55 (AAssign V_ideaCipher_x3
  (Some (ESub (ENum (1)) (EVar V_ideaCipher_x3)))) 56)::(EA 56 ANone 70)::
  (EA 57 (AAssign V_ideaCipher_x3 (Some (EVar V_ideaCipher_x3))) 58)::
  (EA 58 AWeaken 59)::(EA 59 (AGuard (fun s => ((eval (EVar V_ideaCipher_x3)
  s) <> (eval (ENum (0)) s))%Z)) 63)::(EA 59 (AGuard
  (fun s => ((eval (EVar V_ideaCipher_x3) s) = (eval (ENum (0)) s))%Z)) 60)::
  (EA 60 AWeaken 61)::(EA 61 (AAssign V_ideaCipher_x3 (Some (ESub (ENum (1))
  (EVar V_ideaCipher_t16)))) 62)::(EA 62 ANone 69)::(EA 63 AWeaken 64)::
  (EA 64 (AAssign V_ideaCipher_t32 (Some (EMul (EVar V_ideaCipher_x3)
  (EVar V_ideaCipher_t16)))) 65)::(EA 65 (AAssign V_ideaCipher_x3
  (Some (EVar V_ideaCipher_t32))) 66)::(EA 66 (AAssign V_ideaCipher_t16
  None) 67)::(EA 67 (AAssign V_ideaCipher_x3 None) 68)::(EA 68 ANone 69)::
  (EA 69 ANone 70)::(EA 70 (AAssign V_ideaCipher_s2
  (Some (EVar V_ideaCipher_x2))) 71)::(EA 71 (AAssign V_ideaCipher_x2
  None) 72)::(EA 72 (AAssign V_ideaCipher_x2
  (Some (EAdd (EVar V_ideaCipher_x2) (EVar V_ideaCipher_x3)))) 73)::
  (EA 73 (AAssign V_ideaCipher_t16 None) 74)::(EA 74 AWeaken 75)::
  (EA 75 ANone 78)::(EA 75 ANone 76)::(EA 76 (AAssign V_ideaCipher_x2
  (Some (ESub (ENum (1)) (EVar V_ideaCipher_x2)))) 77)::(EA 77 ANone 91)::
  (EA 78 (AAssign V_ideaCipher_x2 (Some (EVar V_ideaCipher_x2))) 79)::
  (EA 79 AWeaken 80)::(EA 80 (AGuard (fun s => ((eval (EVar V_ideaCipher_x2)
  s) <> (eval (ENum (0)) s))%Z)) 84)::(EA 80 (AGuard
  (fun s => ((eval (EVar V_ideaCipher_x2) s) = (eval (ENum (0)) s))%Z)) 81)::
  (EA 81 AWeaken 82)::(EA 82 (AAssign V_ideaCipher_x2 (Some (ESub (ENum (1))
  (EVar V_ideaCipher_t16)))) 83)::(EA 83 ANone 90)::(EA 84 AWeaken 85)::
  (EA 85 (AAssign V_ideaCipher_t32 (Some (EMul (EVar V_ideaCipher_x2)
  (EVar V_ideaCipher_t16)))) 86)::(EA 86 (AAssign V_ideaCipher_x2
  (Some (EVar V_ideaCipher_t32))) 87)::(EA 87 (AAssign V_ideaCipher_t16
  None) 88)::(EA 88 (AAssign V_ideaCipher_x2 None) 89)::(EA 89 ANone 90)::
  (EA 90 ANone 91)::(EA 91 (AAssign V_ideaCipher_x3
  (Some (EAdd (EVar V_ideaCipher_x3) (EVar V_ideaCipher_x2)))) 92)::
  (EA 92 (AAssign V_ideaCipher_x1 None) 93)::(EA 93 (AAssign V_ideaCipher_x4
  None) 94)::(EA 94 (AAssign V_ideaCipher_x2 None) 95)::(EA 95 (AAssign
  V_ideaCipher_x3 None) 96)::(EA 96 ANone 97)::(EA 97 (AAssign V_ideaCipher_r
  (Some (EAdd (EVar V_ideaCipher_r) (ENum (-1))))) 98)::(EA 98 AWeaken 99)::
  (EA 99 (AGuard (fun s => ((eval (EAdd (EVar V_ideaCipher_r) (ENum (-1)))
  s) <> (eval (ENum (0)) s))%Z)) 145)::(EA 99 (AGuard
  (fun s => ((eval (EAdd (EVar V_ideaCipher_r) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 100)::(EA 100 AWeaken 101)::(EA 101 (AAssign
  V_ideaCipher_t16 None) 102)::(EA 102 AWeaken 103)::(EA 103 ANone 106)::
  (EA 103 ANone 104)::(EA 104 (AAssign V_ideaCipher_x1 (Some (ESub (ENum (1))
  (EVar V_ideaCipher_x1)))) 105)::(EA 105 ANone 119)::(EA 106 (AAssign
  V_ideaCipher_x1 (Some (EVar V_ideaCipher_x1))) 107)::(EA 107 AWeaken 108)::
  (EA 108 (AGuard (fun s => ((eval (EVar V_ideaCipher_x1) s) <>
  (eval (ENum (0)) s))%Z)) 112)::(EA 108 (AGuard
  (fun s => ((eval (EVar V_ideaCipher_x1) s) = (eval (ENum (0))
  s))%Z)) 109)::(EA 109 AWeaken 110)::(EA 110 (AAssign V_ideaCipher_x1
  (Some (ESub (ENum (1)) (EVar V_ideaCipher_t16)))) 111)::
  (EA 111 ANone 118)::(EA 112 AWeaken 113)::(EA 113 (AAssign V_ideaCipher_t32
  (Some (EMul (EVar V_ideaCipher_x1) (EVar V_ideaCipher_t16)))) 114)::
  (EA 114 (AAssign V_ideaCipher_x1 (Some (EVar V_ideaCipher_t32))) 115)::
  (EA 115 (AAssign V_ideaCipher_t16 None) 116)::(EA 116 (AAssign
  V_ideaCipher_x1 None) 117)::(EA 117 ANone 118)::(EA 118 ANone 119)::
  (EA 119 (AAssign V_ideaCipher_x3 None) 120)::(EA 120 (AAssign
  V_ideaCipher_x2 None) 121)::(EA 121 (AAssign V_ideaCipher_t16 None) 122)::
  (EA 122 AWeaken 123)::(EA 123 ANone 126)::(EA 123 ANone 124)::
  (EA 124 (AAssign V_ideaCipher_x4 (Some (ESub (ENum (1))
  (EVar V_ideaCipher_x4)))) 125)::(EA 125 ANone 139)::(EA 126 (AAssign
  V_ideaCipher_x4 (Some (EVar V_ideaCipher_x4))) 127)::(EA 127 AWeaken 128)::
  (EA 128 (AGuard (fun s => ((eval (EVar V_ideaCipher_x4) s) <>
  (eval (ENum (0)) s))%Z)) 132)::(EA 128 (AGuard
  (fun s => ((eval (EVar V_ideaCipher_x4) s) = (eval (ENum (0))
  s))%Z)) 129)::(EA 129 AWeaken 130)::(EA 130 (AAssign V_ideaCipher_x4
  (Some (ESub (ENum (1)) (EVar V_ideaCipher_t16)))) 131)::
  (EA 131 ANone 138)::(EA 132 AWeaken 133)::(EA 133 (AAssign V_ideaCipher_t32
  (Some (EMul (EVar V_ideaCipher_x4) (EVar V_ideaCipher_t16)))) 134)::
  (EA 134 (AAssign V_ideaCipher_x4 (Some (EVar V_ideaCipher_t32))) 135)::
  (EA 135 (AAssign V_ideaCipher_t16 None) 136)::(EA 136 (AAssign
  V_ideaCipher_x4 None) 137)::(EA 137 ANone 138)::(EA 138 ANone 139)::
  (EA 139 (AAssign V_ideaCipher_x1 (Some (EVar V_ideaCipher_x1))) 140)::
  (EA 140 (AAssign V_ideaCipher_x2 (Some (EVar V_ideaCipher_x2))) 141)::
  (EA 141 (AAssign V_ideaCipher_x3 (Some (EVar V_ideaCipher_x3))) 142)::
  (EA 142 (AAssign V_ideaCipher_x4 (Some (EVar V_ideaCipher_x4))) 143)::
  (EA 143 AWeaken 144)::(EA 145 AWeaken 146)::(EA 146 ANone 147)::
  (EA 147 (AAssign V_ideaCipher_z (Some (EAdd (ENum (1))
  (EVar V_ideaCipher_z)))) 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ideaCipher => Pedges_ideaCipher
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ideaCipher => 144
     end)%positive;
  var_global := var_global
}.

Definition ai_ideaCipher (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ideaCipher_z <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 3 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_r + 8 <= 0)%Z
   | 4 => (-1 * s V_ideaCipher_r + 8 <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0 /\ 1 * s V_ideaCipher_z <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 5 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_r + 8 <= 0)%Z
   | 6 => (-1 * s V_ideaCipher_r + 8 <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0 /\ 1 * s V_ideaCipher_z <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 7 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_r + 8 <= 0)%Z
   | 8 => (-1 * s V_ideaCipher_r + 8 <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0 /\ 1 * s V_ideaCipher_z <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 9 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_r + 8 <= 0)%Z
   | 10 => (-1 * s V_ideaCipher_r + 8 <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0 /\ 1 * s V_ideaCipher_z <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 11 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_r + 8 <= 0)%Z
   | 12 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 13 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 14 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 15 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 16 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 17 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 18 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 19 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 20 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_x1 <= 0 /\ -1 * s V_ideaCipher_x1 <= 0)%Z
   | 21 => (-1 * s V_ideaCipher_x1 <= 0 /\ 1 * s V_ideaCipher_x1 <= 0 /\ -1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 22 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 23 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 24 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 25 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 26 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 27 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 28 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 29 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 30 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 31 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 32 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 33 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 34 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 35 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 36 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 37 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 38 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 39 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 40 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0 /\ 1 * s V_ideaCipher_x4 <= 0 /\ -1 * s V_ideaCipher_x4 <= 0)%Z
   | 41 => (-1 * s V_ideaCipher_x4 <= 0 /\ 1 * s V_ideaCipher_x4 <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 42 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 43 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 44 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 45 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 46 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 47 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 48 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 49 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 50 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 51 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 52 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 53 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 54 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 55 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 56 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 57 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 58 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 59 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 60 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_x3 <= 0 /\ -1 * s V_ideaCipher_x3 <= 0)%Z
   | 61 => (-1 * s V_ideaCipher_x3 <= 0 /\ 1 * s V_ideaCipher_x3 <= 0 /\ -1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 62 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 63 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 64 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 65 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 66 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 67 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 68 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 69 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 70 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 71 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 72 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 73 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 74 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 75 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 76 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 77 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 78 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 79 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 80 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 81 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_x2 <= 0 /\ -1 * s V_ideaCipher_x2 <= 0)%Z
   | 82 => (-1 * s V_ideaCipher_x2 <= 0 /\ 1 * s V_ideaCipher_x2 <= 0 /\ -1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 83 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 84 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 85 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 86 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 87 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 88 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 89 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 90 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 91 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 92 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 93 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 94 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 95 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 96 => (1 * s V_ideaCipher_r + -8 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 97 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -8 <= 0)%Z
   | 98 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -7 <= 0)%Z
   | 99 => (1 * s V_ideaCipher_r + -7 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 100 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 101 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 102 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 103 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 104 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 105 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 106 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 107 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 108 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 109 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_x1 <= 0 /\ -1 * s V_ideaCipher_x1 <= 0)%Z
   | 110 => (-1 * s V_ideaCipher_x1 <= 0 /\ 1 * s V_ideaCipher_x1 <= 0 /\ -1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 111 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 112 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 113 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 114 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 115 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 116 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 117 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 118 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 119 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 120 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 121 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 122 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 123 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 124 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 125 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 126 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 127 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 128 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 129 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_x4 <= 0 /\ -1 * s V_ideaCipher_x4 <= 0)%Z
   | 130 => (-1 * s V_ideaCipher_x4 <= 0 /\ 1 * s V_ideaCipher_x4 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 131 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 132 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 133 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 134 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 135 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 136 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 137 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 138 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 139 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 140 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 141 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 142 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 143 => (-1 * s V_ideaCipher_r + 1 <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 144 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -1 <= 0 /\ -1 * s V_ideaCipher_r + 1 <= 0)%Z
   | 145 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -7 <= 0)%Z
   | 146 => (1 * s V_ideaCipher_r + -7 <= 0 /\ -1 * s V_ideaCipher_z <= 0)%Z
   | 147 => (-1 * s V_ideaCipher_z <= 0 /\ 1 * s V_ideaCipher_r + -7 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ideaCipher (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((6 # 1) <= z)%Q
   | 2 => ((6 # 1) + s V_ideaCipher_z <= z)%Q
   | 3 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 4 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 5 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 6 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 7 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 8 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 9 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 10 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 11 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 12 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 13 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 14 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 15 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 16 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 17 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 18 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 19 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 20 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 21 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 22 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 23 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 24 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 25 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 26 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 27 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 28 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 29 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 30 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 31 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 32 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 33 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 34 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 35 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 36 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 37 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 38 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 39 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 40 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 41 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 42 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 43 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 44 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 45 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 46 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 47 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 48 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 49 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 50 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 51 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 52 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 53 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 54 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 55 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 56 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 57 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 58 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 59 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 60 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 61 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 62 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 63 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 64 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 65 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 66 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 67 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 68 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 69 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 70 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 71 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 72 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 73 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 74 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 75 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 76 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 77 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 78 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 79 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 80 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 81 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 82 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 83 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 84 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 85 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 86 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 87 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 88 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 89 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 90 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 91 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 92 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 93 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 94 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 95 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 96 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 97 => ((6 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 98 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 99 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 100 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 101 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 102 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 103 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 104 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 105 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 106 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 107 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 108 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 109 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 110 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 111 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 112 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 113 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 114 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 115 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 116 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 117 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 118 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 119 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 120 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 121 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 122 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 123 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 124 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 125 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 126 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 127 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 128 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 129 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 130 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 131 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 132 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 133 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 134 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 135 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 136 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 137 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 138 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 139 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 140 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 141 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 142 => ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 143 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                               - s V_ideaCipher_r) (0))) (F_max0_ge_0 (7
                                                                    - s V_ideaCipher_r));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_ideaCipher_r)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_ideaCipher_r) (0))) (F_max0_ge_0 (-1
                                                                    + s V_ideaCipher_r))]
     ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 144 => (s V_ideaCipher_z <= z)%Q
   | 145 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (8 - s V_ideaCipher_r)) (F_check_ge (8
                                                                    - s V_ideaCipher_r) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                               - s V_ideaCipher_r) (0))) (F_max0_ge_0 (7
                                                                    - s V_ideaCipher_r))]
     ((6 # 1) + s V_ideaCipher_z - max0(7 - s V_ideaCipher_r) <= z)%Q
   | 146 => ((7 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | 147 => ((7 # 1) + s V_ideaCipher_z - max0(8 - s V_ideaCipher_r) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ideaCipher =>
    [mkPA Q (fun n z s => ai_ideaCipher n s /\ annot0_ideaCipher n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ideaCipher (proc_start P_ideaCipher) s1 (proc_end P_ideaCipher) s2 ->
    (s2 V_ideaCipher_z <= (6 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ideaCipher.
Qed.
