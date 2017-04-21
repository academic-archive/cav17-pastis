Require Import pasta.Pasta.

Inductive proc: Type :=
  P_best_huffman_divide.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_best_huffman_divide_z := 1%positive.
Notation V_best_huffman_divide__tmp := 2%positive.
Notation V_best_huffman_divide__tmp1 := 3%positive.
Notation V_best_huffman_divide_a1 := 4%positive.
Notation V_best_huffman_divide_a2 := 5%positive.
Notation V_best_huffman_divide_bigv := 6%positive.
Notation V_best_huffman_divide_r0 := 7%positive.
Notation V_best_huffman_divide_r1 := 8%positive.
Notation V_best_huffman_divide_ch := 9%positive.
Notation V_best_huffman_divide_gi := 10%positive.
Notation V_best_huffman_divide_gr := 11%positive.
Notation V_best_huffman_divide_ix := 12%positive.
Definition Pedges_best_huffman_divide: list (edge proc) :=
  (EA 1 (AAssign V_best_huffman_divide_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_best_huffman_divide__tmp1
  (Some (EVar V_best_huffman_divide_gr))) 3)::(EA 3 (AAssign
  V_best_huffman_divide__tmp (Some (EVar V_best_huffman_divide_ch))) 4)::
  (EA 4 (AAssign V_best_huffman_divide_bigv None) 5)::(EA 5 (AAssign
  V_best_huffman_divide_r0 (Some (ENum (2)))) 6)::(EA 6 ANone 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_best_huffman_divide_r0) s) < (eval (ENum (23))
  s))%Z)) 10)::(EA 8 (AGuard (fun s => ((eval (EVar V_best_huffman_divide_r0)
  s) >= (eval (ENum (23)) s))%Z)) 9)::(EA 9 AWeaken 23)::(EA 10 AWeaken 11)::
  (EA 11 (AAssign V_best_huffman_divide_a2 None) 12)::(EA 12 AWeaken 13)::
  (EA 13 (AGuard (fun s => ((eval (EVar V_best_huffman_divide_a2) s) >
  (eval (EVar V_best_huffman_divide_bigv) s))%Z)) 21)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_best_huffman_divide_a2) s) <=
  (eval (EVar V_best_huffman_divide_bigv) s))%Z)) 14)::(EA 14 AWeaken 15)::
  (EA 15 ANone 16)::(EA 16 (AAssign V_best_huffman_divide_r0
  (Some (EAdd (EVar V_best_huffman_divide_r0) (ENum (1))))) 17)::
  (EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 (AAssign V_best_huffman_divide_z
  (Some (EAdd (ENum (1)) (EVar V_best_huffman_divide_z)))) 20)::
  (EA 20 AWeaken 8)::(EA 21 AWeaken 22)::(EA 22 ANone 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 25)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_best_huffman_divide_r0) s) <= (eval (ENum (24))
  s))%Z)) 68)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_best_huffman_divide_r0) s) > (eval (ENum (24))
  s))%Z)) 26)::(EA 26 AWeaken 27)::(EA 27 (AAssign V_best_huffman_divide_r0
  (Some (ENum (0)))) 28)::(EA 28 ANone 29)::(EA 29 AWeaken 30)::
  (EA 30 (AGuard (fun s => ((eval (EVar V_best_huffman_divide_r0) s) <
  (eval (ENum (16)) s))%Z)) 32)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_best_huffman_divide_r0) s) >= (eval (ENum (16))
  s))%Z)) 31)::(EA 31 AWeaken 67)::(EA 32 AWeaken 33)::(EA 33 (AAssign
  V_best_huffman_divide_a1 None) 34)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_best_huffman_divide_a1) s) >
  (eval (EVar V_best_huffman_divide_bigv) s))%Z)) 64)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_best_huffman_divide_a1) s) <=
  (eval (EVar V_best_huffman_divide_bigv) s))%Z)) 36)::(EA 36 AWeaken 37)::
  (EA 37 ANone 62)::(EA 37 ANone 38)::(EA 38 (AAssign
  V_best_huffman_divide_r1 (Some (ENum (0)))) 39)::(EA 39 ANone 40)::
  (EA 40 AWeaken 41)::(EA 41 (AGuard
  (fun s => ((eval (EVar V_best_huffman_divide_r1) s) < (eval (ENum (8))
  s))%Z)) 49)::(EA 41 (AGuard
  (fun s => ((eval (EVar V_best_huffman_divide_r1) s) >= (eval (ENum (8))
  s))%Z)) 42)::(EA 42 AWeaken 43)::(EA 43 ANone 44)::(EA 44 (AAssign
  V_best_huffman_divide_r0 (Some (EAdd (EVar V_best_huffman_divide_r0)
  (ENum (1))))) 45)::(EA 45 ANone 46)::(EA 46 ANone 47)::(EA 47 (AAssign
  V_best_huffman_divide_z (Some (EAdd (ENum (1))
  (EVar V_best_huffman_divide_z)))) 48)::(EA 48 AWeaken 30)::
  (EA 49 AWeaken 50)::(EA 50 ANone 56)::(EA 50 ANone 51)::(EA 51 (AAssign
  V_best_huffman_divide_a2 None) 52)::(EA 52 AWeaken 53)::(EA 53 ANone 55)::
  (EA 53 ANone 54)::(EA 54 ANone 57)::(EA 55 ANone 57)::(EA 56 ANone 57)::
  (EA 57 (AAssign V_best_huffman_divide_r1
  (Some (EAdd (EVar V_best_huffman_divide_r1) (ENum (1))))) 58)::
  (EA 58 ANone 59)::(EA 59 ANone 60)::(EA 60 (AAssign V_best_huffman_divide_z
  (Some (EAdd (ENum (1)) (EVar V_best_huffman_divide_z)))) 61)::
  (EA 61 AWeaken 41)::(EA 62 ANone 63)::(EA 63 AWeaken 67)::
  (EA 64 AWeaken 65)::(EA 65 ANone 66)::(EA 66 AWeaken 67)::
  (EA 68 AWeaken 69)::(EA 69 ANone 70)::(EA 70 (AAssign
  V_best_huffman_divide_r0 (Some (EAdd (EVar V_best_huffman_divide_r0)
  (ENum (1))))) 71)::(EA 71 ANone 72)::(EA 72 ANone 73)::(EA 73 (AAssign
  V_best_huffman_divide_z (Some (EAdd (ENum (1))
  (EVar V_best_huffman_divide_z)))) 74)::(EA 74 AWeaken 25)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_best_huffman_divide => Pedges_best_huffman_divide
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_best_huffman_divide => 67
     end)%positive;
  var_global := var_global
}.

Definition ai_best_huffman_divide (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_z <= 0)%Z
   | 3 => (-1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_z <= 0)%Z
   | 4 => (1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_z <= 0)%Z
   | 5 => (-1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_z <= 0)%Z
   | 6 => (1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -2 <= 0 /\ -1 * s V_best_huffman_divide_r0 + 2 <= 0)%Z
   | 7 => (-1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ 1 * s V_best_huffman_divide_r0 + -2 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_z <= 0)%Z
   | 8 => (-1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ 1 * s V_best_huffman_divide_r0 + -23 <= 0)%Z
   | 9 => (1 * s V_best_huffman_divide_r0 + -23 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 23 <= 0)%Z
   | 10 => (-1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -22 <= 0)%Z
   | 11 => (1 * s V_best_huffman_divide_r0 + -22 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 2 <= 0)%Z
   | 12 => (-1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -22 <= 0)%Z
   | 13 => (1 * s V_best_huffman_divide_r0 + -22 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 2 <= 0)%Z
   | 14 => (-1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -22 <= 0 /\ 1 * s V_best_huffman_divide_a2+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 15 => (1 * s V_best_huffman_divide_a2+ -1 * s V_best_huffman_divide_bigv <= 0 /\ 1 * s V_best_huffman_divide_r0 + -22 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 2 <= 0)%Z
   | 16 => (-1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -22 <= 0 /\ 1 * s V_best_huffman_divide_a2+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 17 => (1 * s V_best_huffman_divide_a2+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 3 <= 0 /\ 1 * s V_best_huffman_divide_r0 + -23 <= 0)%Z
   | 18 => (1 * s V_best_huffman_divide_r0 + -23 <= 0 /\ -1 * s V_best_huffman_divide_r0 + 3 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_a2+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 19 => (1 * s V_best_huffman_divide_a2+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 3 <= 0 /\ 1 * s V_best_huffman_divide_r0 + -23 <= 0)%Z
   | 20 => (1 * s V_best_huffman_divide_r0 + -23 <= 0 /\ -1 * s V_best_huffman_divide_r0 + 3 <= 0 /\ 1 * s V_best_huffman_divide_a2+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_z + 1 <= 0)%Z
   | 21 => (-1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -22 <= 0 /\ -1 * s V_best_huffman_divide_a2+ 1 * s V_best_huffman_divide_bigv + 1 <= 0)%Z
   | 22 => (-1 * s V_best_huffman_divide_a2+ 1 * s V_best_huffman_divide_bigv + 1 <= 0 /\ 1 * s V_best_huffman_divide_r0 + -22 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 2 <= 0)%Z
   | 23 => (1 * s V_best_huffman_divide_r0 + -23 <= 0 /\ -1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0)%Z
   | 24 => (-1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ 1 * s V_best_huffman_divide_r0 + -23 <= 0)%Z
   | 25 => (-1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -25 <= 0)%Z
   | 26 => (1 * s V_best_huffman_divide_r0 + -25 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 25 <= 0)%Z
   | 27 => (-1 * s V_best_huffman_divide_r0 + 25 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -25 <= 0)%Z
   | 28 => (-1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0)%Z
   | 29 => (-1 * s V_best_huffman_divide_r0 <= 0 /\ 1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0)%Z
   | 30 => (-1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0)%Z
   | 31 => (-1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 16 <= 0)%Z
   | 32 => (-1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -15 <= 0)%Z
   | 33 => (1 * s V_best_huffman_divide_r0 + -15 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0)%Z
   | 34 => (-1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -15 <= 0)%Z
   | 35 => (1 * s V_best_huffman_divide_r0 + -15 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0)%Z
   | 36 => (-1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -15 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 37 => (1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ 1 * s V_best_huffman_divide_r0 + -15 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0)%Z
   | 38 => (-1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -15 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 39 => (1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ 1 * s V_best_huffman_divide_r0 + -15 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ 1 * s V_best_huffman_divide_r1 <= 0 /\ -1 * s V_best_huffman_divide_r1 <= 0)%Z
   | 40 => (-1 * s V_best_huffman_divide_r1 <= 0 /\ 1 * s V_best_huffman_divide_r1 <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -15 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 41 => (-1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r1 <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ 1 * s V_best_huffman_divide_r1 + -8 <= 0)%Z
   | 42 => (1 * s V_best_huffman_divide_r1 + -8 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r1 + 8 <= 0)%Z
   | 43 => (-1 * s V_best_huffman_divide_r1 + 8 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ 1 * s V_best_huffman_divide_r1 + -8 <= 0)%Z
   | 44 => (1 * s V_best_huffman_divide_r1 + -8 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r1 + 8 <= 0)%Z
   | 45 => (-1 * s V_best_huffman_divide_r1 + 8 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ 1 * s V_best_huffman_divide_r1 + -8 <= 0 /\ -1 * s V_best_huffman_divide_r0 + 1 <= 0)%Z
   | 46 => (-1 * s V_best_huffman_divide_r0 + 1 <= 0 /\ 1 * s V_best_huffman_divide_r1 + -8 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r1 + 8 <= 0)%Z
   | 47 => (-1 * s V_best_huffman_divide_r1 + 8 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ 1 * s V_best_huffman_divide_r1 + -8 <= 0 /\ -1 * s V_best_huffman_divide_r0 + 1 <= 0)%Z
   | 48 => (-1 * s V_best_huffman_divide_r0 + 1 <= 0 /\ 1 * s V_best_huffman_divide_r1 + -8 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_r1 + 8 <= 0 /\ -1 * s V_best_huffman_divide_z + 1 <= 0)%Z
   | 49 => (1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_r1 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r1 + -7 <= 0)%Z
   | 50 => (1 * s V_best_huffman_divide_r1 + -7 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r1 <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 51 => (1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_r1 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r1 + -7 <= 0)%Z
   | 52 => (1 * s V_best_huffman_divide_r1 + -7 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r1 <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 53 => (1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_r1 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r1 + -7 <= 0)%Z
   | 54 => (1 * s V_best_huffman_divide_r1 + -7 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r1 <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 55 => (1 * s V_best_huffman_divide_r1 + -7 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r1 <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 56 => (1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_r1 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r1 + -7 <= 0)%Z
   | 57 => (1 * s V_best_huffman_divide_r1 + -7 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r1 <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 58 => (1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r1 + -8 <= 0 /\ -1 * s V_best_huffman_divide_r1 + 1 <= 0)%Z
   | 59 => (-1 * s V_best_huffman_divide_r1 + 1 <= 0 /\ 1 * s V_best_huffman_divide_r1 + -8 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 60 => (1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r1 + -8 <= 0 /\ -1 * s V_best_huffman_divide_r1 + 1 <= 0)%Z
   | 61 => (-1 * s V_best_huffman_divide_r1 + 1 <= 0 /\ 1 * s V_best_huffman_divide_r1 + -8 <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ -1 * s V_best_huffman_divide_z + 1 <= 0)%Z
   | 62 => (-1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -15 <= 0 /\ 1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0)%Z
   | 63 => (1 * s V_best_huffman_divide_a1+ -1 * s V_best_huffman_divide_bigv <= 0 /\ 1 * s V_best_huffman_divide_r0 + -15 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0)%Z
   | 64 => (-1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -15 <= 0 /\ -1 * s V_best_huffman_divide_a1+ 1 * s V_best_huffman_divide_bigv + 1 <= 0)%Z
   | 65 => (-1 * s V_best_huffman_divide_a1+ 1 * s V_best_huffman_divide_bigv + 1 <= 0 /\ 1 * s V_best_huffman_divide_r0 + -15 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0)%Z
   | 66 => (-1 * s V_best_huffman_divide_r0 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0 /\ 1 * s V_best_huffman_divide_r0 + -15 <= 0 /\ -1 * s V_best_huffman_divide_a1+ 1 * s V_best_huffman_divide_bigv + 1 <= 0)%Z
   | 67 => (-1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 <= 0)%Z
   | 68 => (-1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ 1 * s V_best_huffman_divide_r0 + -24 <= 0)%Z
   | 69 => (1 * s V_best_huffman_divide_r0 + -24 <= 0 /\ -1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0)%Z
   | 70 => (-1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 2 <= 0 /\ 1 * s V_best_huffman_divide_r0 + -24 <= 0)%Z
   | 71 => (-1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 3 <= 0 /\ 1 * s V_best_huffman_divide_r0 + -25 <= 0)%Z
   | 72 => (1 * s V_best_huffman_divide_r0 + -25 <= 0 /\ -1 * s V_best_huffman_divide_r0 + 3 <= 0 /\ -1 * s V_best_huffman_divide_z <= 0)%Z
   | 73 => (-1 * s V_best_huffman_divide_z <= 0 /\ -1 * s V_best_huffman_divide_r0 + 3 <= 0 /\ 1 * s V_best_huffman_divide_r0 + -25 <= 0)%Z
   | 74 => (1 * s V_best_huffman_divide_r0 + -25 <= 0 /\ -1 * s V_best_huffman_divide_r0 + 3 <= 0 /\ -1 * s V_best_huffman_divide_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_best_huffman_divide (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((167 # 1) <= z)%Q
   | 2 => ((167 # 1) + s V_best_huffman_divide_z <= z)%Q
   | 3 => ((167 # 1) + s V_best_huffman_divide_z <= z)%Q
   | 4 => ((167 # 1) + s V_best_huffman_divide_z <= z)%Q
   | 5 => ((167 # 1) + s V_best_huffman_divide_z <= z)%Q
   | 6 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
           + s V_best_huffman_divide_z
           - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
           - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 7 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
           + s V_best_huffman_divide_z
           - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
           - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 8 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
           + s V_best_huffman_divide_z
           - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
           - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 9 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
           + s V_best_huffman_divide_z
           - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
           - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 10 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 11 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 12 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 13 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 14 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 15 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 16 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 17 => ((17743 # 107) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(-1 + s V_best_huffman_divide_r0) <= z)%Q
   | 18 => ((17743 # 107) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(-1 + s V_best_huffman_divide_r0) <= z)%Q
   | 19 => ((17743 # 107) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(-1 + s V_best_huffman_divide_r0) <= z)%Q
   | 20 => hints
     [(*-3.51086 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_best_huffman_divide_r0)) (F_check_ge (s V_best_huffman_divide_r0) (0));
      (*-3.17752 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + 
                                                                    s V_best_huffman_divide_r0) (0))) (F_max0_ge_0 (-1
                                                                    + s V_best_huffman_divide_r0));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_best_huffman_divide_r0) (0))) (F_max0_ge_0 (-2
                                                                    + s V_best_huffman_divide_r0))]
     ((17636 # 107) + (219 # 77) * s V_best_huffman_divide_r0
      + s V_best_huffman_divide_z
      - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
      - (323 # 92) * max0(-1 + s V_best_huffman_divide_r0) <= z)%Q
   | 21 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 22 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 23 => ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 24 => hints
     [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (-2
                                                          + s V_best_huffman_divide_r0)) (F_check_ge (-2
                                                                    + s V_best_huffman_divide_r0) (0))]
     ((506 # 3) + (219 # 77) * s V_best_huffman_divide_r0
      + s V_best_huffman_divide_z
      - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
      - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 25 => ((168 # 1) + (340 # 107) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (25 - s V_best_huffman_divide_r0) (24
                                                                    - s V_best_huffman_divide_r0));
      (*-1 0*) F_max0_ge_0 (24 - s V_best_huffman_divide_r0);
      (*-3.51086 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_best_huffman_divide_r0) (0))) (F_max0_ge_0 (s V_best_huffman_divide_r0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (25
                                                               - s V_best_huffman_divide_r0) (0))) (F_max0_ge_0 (25
                                                                    - s V_best_huffman_divide_r0));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_best_huffman_divide_r0) (0))) (F_max0_ge_0 (-1
                                                                    + s V_best_huffman_divide_r0));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_best_huffman_divide_r0) (0))) (F_max0_ge_0 (-2
                                                                    + s V_best_huffman_divide_r0))]
     ((168 # 1) + (340 # 107) * s V_best_huffman_divide_r0
      + s V_best_huffman_divide_z
      - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
      - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
      - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 27 => ((144 # 1) + s V_best_huffman_divide_z <= z)%Q
   | 28 => (-(38 # 113) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            + (9 # 1) * max0(16 - s V_best_huffman_divide_r0)
            - (353 # 42) * max0(22 - s V_best_huffman_divide_r0)
            + (353 # 42) * max0(23 - s V_best_huffman_divide_r0)
            - (38 # 113) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 29 => hints
     [(*-8.40467 0*) F_max0_pre_decrement 1 (23 - s V_best_huffman_divide_r0) (1);
      (*-0.336187 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (25
                                                                    - s V_best_huffman_divide_r0) (0))) (F_max0_ge_0 (25
                                                                    - s V_best_huffman_divide_r0))]
     (-(38 # 113) * s V_best_huffman_divide_r0 + s V_best_huffman_divide_z
      + (9 # 1) * max0(16 - s V_best_huffman_divide_r0)
      - (353 # 42) * max0(22 - s V_best_huffman_divide_r0)
      + (353 # 42) * max0(23 - s V_best_huffman_divide_r0)
      - (38 # 113) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 30 => (s V_best_huffman_divide_z
            + (9 # 1) * max0(16 - s V_best_huffman_divide_r0) <= z)%Q
   | 31 => hints
     [(*-9 0*) F_max0_monotonic (F_check_ge (16 - s V_best_huffman_divide_r0) (15
                                                                    - s V_best_huffman_divide_r0));
      (*-9 0*) F_max0_ge_0 (15 - s V_best_huffman_divide_r0)]
     (s V_best_huffman_divide_z
      + (9 # 1) * max0(16 - s V_best_huffman_divide_r0) <= z)%Q
   | 32 => hints
     [(*0 9*) F_max0_pre_decrement 1 (16 - s V_best_huffman_divide_r0) (1);
      (*0 0.36*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (25
                                                                 - s V_best_huffman_divide_r0) (0))) (F_max0_ge_0 (25
                                                                    - s V_best_huffman_divide_r0))]
     (s V_best_huffman_divide_z
      + (9 # 1) * max0(16 - s V_best_huffman_divide_r0) <= z)%Q
   | 33 => ((9 # 25) * s V_best_huffman_divide_r0 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0)
            + (9 # 25) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 34 => hints
     [(*-1.28571 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (22
                                                                    - 
                                                                    s V_best_huffman_divide_r0) (0))) (F_max0_ge_0 (22
                                                                    - s V_best_huffman_divide_r0))]
     ((9 # 25) * s V_best_huffman_divide_r0 + s V_best_huffman_divide_z
      + (9 # 1) * max0(15 - s V_best_huffman_divide_r0)
      + (9 # 25) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 35 => (-(198 # 7) + (209 # 127) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0)
            + (9 # 7) * max0(22 - s V_best_huffman_divide_r0)
            + (9 # 25) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 36 => hints
     [(*0 1.28571*) F_binom_monotonic 1 (F_max0_ge_arg (22
                                                        - s V_best_huffman_divide_r0)) (F_check_ge (22
                                                                    - s V_best_huffman_divide_r0) (0))]
     (-(198 # 7) + (209 # 127) * s V_best_huffman_divide_r0
      + s V_best_huffman_divide_z
      + (9 # 1) * max0(15 - s V_best_huffman_divide_r0)
      + (9 # 7) * max0(22 - s V_best_huffman_divide_r0)
      + (9 # 25) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 37 => ((9 # 25) * s V_best_huffman_divide_r0 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0)
            + (9 # 25) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 38 => ((9 # 25) * s V_best_huffman_divide_r0 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0)
            + (9 # 25) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 39 => ((9 # 25) * s V_best_huffman_divide_r0
            - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0)
            + (9 # 25) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 40 => hints
     [(*-0.36 0*) F_max0_pre_decrement 1 (25 - s V_best_huffman_divide_r0) (1);
      (*-0.36 0*) F_binom_monotonic 1 (F_max0_ge_arg (24
                                                      - s V_best_huffman_divide_r0)) (F_check_ge (24
                                                                    - s V_best_huffman_divide_r0) (0))]
     ((9 # 25) * s V_best_huffman_divide_r0 - s V_best_huffman_divide_r1
      + s V_best_huffman_divide_z
      + (9 # 1) * max0(15 - s V_best_huffman_divide_r0)
      + (9 # 25) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 41 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 42 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 43 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 44 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 45 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(16 - s V_best_huffman_divide_r0) <= z)%Q
   | 46 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(16 - s V_best_huffman_divide_r0) <= z)%Q
   | 47 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(16 - s V_best_huffman_divide_r0) <= z)%Q
   | 48 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (8 - s V_best_huffman_divide_r1) (7
                                                                    - s V_best_huffman_divide_r1));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                               - s V_best_huffman_divide_r1) (0))) (F_max0_ge_0 (8
                                                                    - s V_best_huffman_divide_r1));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (7
                                                 - s V_best_huffman_divide_r1)) (F_check_ge (0) (0))]
     ((8 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
      + (9 # 1) * max0(16 - s V_best_huffman_divide_r0) <= z)%Q
   | 49 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 50 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 51 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 52 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 53 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 54 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 55 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 56 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 57 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 58 => ((10 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 59 => ((10 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 60 => ((10 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 61 => ((9 # 1) - s V_best_huffman_divide_r1 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0) <= z)%Q
   | 62 => ((9 # 25) * s V_best_huffman_divide_r0 + s V_best_huffman_divide_z
            + (9 # 1) * max0(15 - s V_best_huffman_divide_r0)
            + (9 # 25) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 63 => hints
     [(*-0.36 0*) F_one;
      (*-0.36 0*) F_max0_pre_decrement 1 (25 - s V_best_huffman_divide_r0) (1);
      (*-9 0*) F_max0_ge_0 (15 - s V_best_huffman_divide_r0);
      (*-0.36 0*) F_max0_ge_0 (24 - s V_best_huffman_divide_r0);
      (*-0.36 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_best_huffman_divide_r0)) (F_check_ge (0) (0));
      (*-0.36 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_best_huffman_divide_r0) (0))) (F_max0_ge_0 (s V_best_huffman_divide_r0))]
     ((9 # 25) * s V_best_huffman_divide_r0 + s V_best_huffman_divide_z
      + (9 # 1) * max0(15 - s V_best_huffman_divide_r0)
      + (9 # 25) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 64 => hints
     [(*0 7.71429*) F_max0_ge_0 (15 - s V_best_huffman_divide_r0);
      (*0 0.36*) F_binom_monotonic 1 (F_max0_ge_arg (25
                                                     - s V_best_huffman_divide_r0)) (F_check_ge (25
                                                                    - s V_best_huffman_divide_r0) (0))]
     (-(198 # 7) + (209 # 127) * s V_best_huffman_divide_r0
      + s V_best_huffman_divide_z
      + (9 # 1) * max0(15 - s V_best_huffman_divide_r0)
      + (9 # 7) * max0(22 - s V_best_huffman_divide_r0)
      + (9 # 25) * max0(25 - s V_best_huffman_divide_r0) <= z)%Q
   | 65 => (-(135 # 7) + (9 # 7) * s V_best_huffman_divide_r0
            + s V_best_huffman_divide_z
            + (9 # 7) * max0(15 - s V_best_huffman_divide_r0)
            + (9 # 7) * max0(22 - s V_best_huffman_divide_r0) <= z)%Q
   | 66 => hints
     [(*-1.28571 0*) F_max0_ge_0 (22 - s V_best_huffman_divide_r0);
      (*-1.28571 0*) F_binom_monotonic 1 (F_max0_ge_arg (15
                                                         - s V_best_huffman_divide_r0)) (F_check_ge (15
                                                                    - s V_best_huffman_divide_r0) (0))]
     (-(135 # 7) + (9 # 7) * s V_best_huffman_divide_r0
      + s V_best_huffman_divide_z
      + (9 # 7) * max0(15 - s V_best_huffman_divide_r0)
      + (9 # 7) * max0(22 - s V_best_huffman_divide_r0) <= z)%Q
   | 67 => (s V_best_huffman_divide_z <= z)%Q
   | 68 => hints
     [(*0 3.17752*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_best_huffman_divide_r0) (0))) (F_max0_ge_0 (s V_best_huffman_divide_r0))]
     ((168 # 1) + (340 # 107) * s V_best_huffman_divide_r0
      + s V_best_huffman_divide_z
      - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
      - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
      - (323 # 92) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 69 => ((168 # 1) + s V_best_huffman_divide_z
            - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (1 # 3) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 70 => ((168 # 1) + s V_best_huffman_divide_z
            - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0)
            - (1 # 3) * max0(s V_best_huffman_divide_r0) <= z)%Q
   | 71 => ((168 # 1) + s V_best_huffman_divide_z
            - (1 # 3) * max0(-3 + s V_best_huffman_divide_r0)
            - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0) <= z)%Q
   | 72 => ((168 # 1) + s V_best_huffman_divide_z
            - (1 # 3) * max0(-3 + s V_best_huffman_divide_r0)
            - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0) <= z)%Q
   | 73 => ((168 # 1) + s V_best_huffman_divide_z
            - (1 # 3) * max0(-3 + s V_best_huffman_divide_r0)
            - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
            - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0) <= z)%Q
   | 74 => hints
     [(*-3.51086 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_best_huffman_divide_r0)) (F_check_ge (s V_best_huffman_divide_r0) (0));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-3
                                                                    + s V_best_huffman_divide_r0) (0))) (F_max0_ge_0 (-3
                                                                    + s V_best_huffman_divide_r0))]
     ((167 # 1) + s V_best_huffman_divide_z
      - (1 # 3) * max0(-3 + s V_best_huffman_divide_r0)
      - (1 # 3) * max0(-2 + s V_best_huffman_divide_r0)
      - (1 # 3) * max0(-1 + s V_best_huffman_divide_r0) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_best_huffman_divide =>
    [mkPA Q (fun n z s => ai_best_huffman_divide n s /\ annot0_best_huffman_divide n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_best_huffman_divide (proc_start P_best_huffman_divide) s1 (proc_end P_best_huffman_divide) s2 ->
    (s2 V_best_huffman_divide_z <= (167 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_best_huffman_divide.
Qed.
