Require Import pasta.Pasta.

Inductive proc: Type :=
  P_fft_short.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_fft_short_z := 1%positive.
Notation V_fft_short__tmp := 2%positive.
Notation V_fft_short_b := 3%positive.
Notation V_fft_short_i := 4%positive.
Notation V_fft_short_j := 5%positive.
Notation V_fft_short_k := 6%positive.
Notation V_fft_short_buffer := 7%positive.
Notation V_fft_short_chn := 8%positive.
Notation V_fft_short_x_real := 9%positive.
Definition Pedges_fft_short: list (edge proc) :=
  (EA 1 (AAssign V_fft_short_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_fft_short__tmp (Some (EVar V_fft_short_chn))) 3)::(EA 3 (AAssign
  V_fft_short_b (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_fft_short_b) s) < (eval (ENum (3))
  s))%Z)) 9)::(EA 6 (AGuard (fun s => ((eval (EVar V_fft_short_b) s) >=
  (eval (ENum (3)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 (AAssign V_fft_short_k (Some (EMul (ENum (192))
  (EAdd (EVar V_fft_short_b) (ENum (1)))))) 11)::(EA 11 (AAssign
  V_fft_short_j (Some (ENum (31)))) 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_fft_short__tmp) s) < (eval (ENum (2))
  s))%Z)) 41)::(EA 13 (AGuard (fun s => ((eval (EVar V_fft_short__tmp) s) >=
  (eval (ENum (2)) s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_fft_short__tmp) s) = (eval (ENum (2))
  s))%Z)) 28)::(EA 15 (AGuard (fun s => ((eval (EVar V_fft_short__tmp) s) <>
  (eval (ENum (2)) s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 18)::
  (EA 18 (AAssign V_fft_short_i None) 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_fft_short_j (Some (EAdd (EVar V_fft_short_j) (ENum (-1))))) 21)::
  (EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EAdd (EVar V_fft_short_j) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 25)::(EA 22 (AGuard
  (fun s => ((eval (EAdd (EVar V_fft_short_j) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 23)::(EA 23 AWeaken 24)::(EA 24 ANone 37)::
  (EA 25 AWeaken 26)::(EA 26 ANone 27)::(EA 27 (AAssign V_fft_short_z
  (Some (EAdd (ENum (1)) (EVar V_fft_short_z)))) 18)::(EA 28 AWeaken 29)::
  (EA 29 ANone 30)::(EA 30 (AAssign V_fft_short_i None) 31)::
  (EA 31 ANone 32)::(EA 32 (AAssign V_fft_short_j
  (Some (EAdd (EVar V_fft_short_j) (ENum (-1))))) 33)::(EA 33 AWeaken 34)::
  (EA 34 (AGuard (fun s => ((eval (EAdd (EVar V_fft_short_j) (ENum (-1)))
  s) >= (eval (ENum (0)) s))%Z)) 38)::(EA 34 (AGuard
  (fun s => ((eval (EAdd (EVar V_fft_short_j) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 35)::(EA 35 AWeaken 36)::(EA 36 ANone 37)::
  (EA 37 ANone 50)::(EA 38 AWeaken 39)::(EA 39 ANone 40)::(EA 40 (AAssign
  V_fft_short_z (Some (EAdd (ENum (1)) (EVar V_fft_short_z)))) 30)::
  (EA 41 AWeaken 42)::(EA 42 ANone 43)::(EA 43 (AAssign V_fft_short_i
  None) 44)::(EA 44 ANone 45)::(EA 45 (AAssign V_fft_short_j
  (Some (EAdd (EVar V_fft_short_j) (ENum (-1))))) 46)::(EA 46 AWeaken 47)::
  (EA 47 (AGuard (fun s => ((eval (EAdd (EVar V_fft_short_j) (ENum (-1)))
  s) >= (eval (ENum (0)) s))%Z)) 56)::(EA 47 (AGuard
  (fun s => ((eval (EAdd (EVar V_fft_short_j) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 48)::(EA 48 AWeaken 49)::(EA 49 ANone 50)::
  (EA 50 ANone 51)::(EA 51 (AAssign V_fft_short_b
  (Some (EAdd (EVar V_fft_short_b) (ENum (1))))) 52)::(EA 52 ANone 53)::
  (EA 53 ANone 54)::(EA 54 (AAssign V_fft_short_z (Some (EAdd (ENum (1))
  (EVar V_fft_short_z)))) 55)::(EA 55 AWeaken 6)::(EA 56 AWeaken 57)::
  (EA 57 ANone 58)::(EA 58 (AAssign V_fft_short_z (Some (EAdd (ENum (1))
  (EVar V_fft_short_z)))) 43)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_fft_short => Pedges_fft_short
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_fft_short => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_fft_short (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_z <= 0)%Z
   | 3 => (-1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_z <= 0)%Z
   | 4 => (1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_b <= 0)%Z
   | 5 => (-1 * s V_fft_short_b <= 0 /\ 1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_z <= 0)%Z
   | 6 => (-1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_b <= 0)%Z
   | 7 => (-1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_b + 3 <= 0)%Z
   | 8 => (-1 * s V_fft_short_b + 3 <= 0 /\ -1 * s V_fft_short_z <= 0)%Z
   | 9 => (-1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_b + -2 <= 0)%Z
   | 10 => (1 * s V_fft_short_b + -2 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_b <= 0)%Z
   | 11 => (-1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_b + -2 <= 0 /\ 1 * s V_fft_short_k + -576 <= 0 /\ -1 * s V_fft_short_k + 192 <= 0)%Z
   | 12 => (-1 * s V_fft_short_k + 192 <= 0 /\ 1 * s V_fft_short_k + -576 <= 0 /\ 1 * s V_fft_short_b + -2 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_b <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_j + 31 <= 0)%Z
   | 13 => (-1 * s V_fft_short_j + 31 <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_b + -2 <= 0 /\ 1 * s V_fft_short_k + -576 <= 0 /\ -1 * s V_fft_short_k + 192 <= 0)%Z
   | 14 => (-1 * s V_fft_short_k + 192 <= 0 /\ 1 * s V_fft_short_k + -576 <= 0 /\ 1 * s V_fft_short_b + -2 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_b <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_j + 31 <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0)%Z
   | 15 => (-1 * s V_fft_short__tmp + 2 <= 0 /\ -1 * s V_fft_short_j + 31 <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_b + -2 <= 0 /\ 1 * s V_fft_short_k + -576 <= 0 /\ -1 * s V_fft_short_k + 192 <= 0)%Z
   | 16 => (-1 * s V_fft_short_k + 192 <= 0 /\ 1 * s V_fft_short_k + -576 <= 0 /\ 1 * s V_fft_short_b + -2 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_b <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_j + 31 <= 0 /\ -1 * s V_fft_short__tmp + 3 <= 0)%Z
   | 17 => (-1 * s V_fft_short__tmp + 3 <= 0 /\ -1 * s V_fft_short_j + 31 <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_b + -2 <= 0 /\ 1 * s V_fft_short_k + -576 <= 0 /\ -1 * s V_fft_short_k + 192 <= 0)%Z
   | 18 => (-1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short__tmp + 3 <= 0)%Z
   | 19 => (-1 * s V_fft_short__tmp + 3 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_z <= 0)%Z
   | 20 => (-1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short__tmp + 3 <= 0)%Z
   | 21 => (-1 * s V_fft_short__tmp + 3 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_j <= 0)%Z
   | 22 => (-1 * s V_fft_short_j <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short__tmp + 3 <= 0)%Z
   | 23 => (-1 * s V_fft_short__tmp + 3 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_j <= 0 /\ 1 * s V_fft_short_j <= 0)%Z
   | 24 => (1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short__tmp + 3 <= 0)%Z
   | 25 => (-1 * s V_fft_short__tmp + 3 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0)%Z
   | 26 => (-1 * s V_fft_short_j + 1 <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short__tmp + 3 <= 0)%Z
   | 27 => (-1 * s V_fft_short__tmp + 3 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0)%Z
   | 28 => (-1 * s V_fft_short_k + 192 <= 0 /\ 1 * s V_fft_short_k + -576 <= 0 /\ 1 * s V_fft_short_b + -2 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_b <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_j + 31 <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0 /\ 1 * s V_fft_short__tmp + -2 <= 0)%Z
   | 29 => (1 * s V_fft_short__tmp + -2 <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0 /\ -1 * s V_fft_short_j + 31 <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_b + -2 <= 0 /\ 1 * s V_fft_short_k + -576 <= 0 /\ -1 * s V_fft_short_k + 192 <= 0)%Z
   | 30 => (-1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0 /\ 1 * s V_fft_short__tmp + -2 <= 0)%Z
   | 31 => (1 * s V_fft_short__tmp + -2 <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_z <= 0)%Z
   | 32 => (-1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0 /\ 1 * s V_fft_short__tmp + -2 <= 0)%Z
   | 33 => (1 * s V_fft_short__tmp + -2 <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_j <= 0)%Z
   | 34 => (-1 * s V_fft_short_j <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0 /\ 1 * s V_fft_short__tmp + -2 <= 0)%Z
   | 35 => (1 * s V_fft_short__tmp + -2 <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_j <= 0 /\ 1 * s V_fft_short_j <= 0)%Z
   | 36 => (1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0 /\ 1 * s V_fft_short__tmp + -2 <= 0)%Z
   | 37 => (-1 * s V_fft_short__tmp + 2 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_j <= 0 /\ 1 * s V_fft_short_j <= 0)%Z
   | 38 => (1 * s V_fft_short__tmp + -2 <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0)%Z
   | 39 => (-1 * s V_fft_short_j + 1 <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0 /\ 1 * s V_fft_short__tmp + -2 <= 0)%Z
   | 40 => (1 * s V_fft_short__tmp + -2 <= 0 /\ -1 * s V_fft_short__tmp + 2 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0)%Z
   | 41 => (-1 * s V_fft_short_k + 192 <= 0 /\ 1 * s V_fft_short_k + -576 <= 0 /\ 1 * s V_fft_short_b + -2 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_b <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_j + 31 <= 0 /\ 1 * s V_fft_short__tmp + -1 <= 0)%Z
   | 42 => (1 * s V_fft_short__tmp + -1 <= 0 /\ -1 * s V_fft_short_j + 31 <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_b + -2 <= 0 /\ 1 * s V_fft_short_k + -576 <= 0 /\ -1 * s V_fft_short_k + 192 <= 0)%Z
   | 43 => (-1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ 1 * s V_fft_short__tmp + -1 <= 0)%Z
   | 44 => (1 * s V_fft_short__tmp + -1 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_z <= 0)%Z
   | 45 => (-1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -31 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ 1 * s V_fft_short__tmp + -1 <= 0)%Z
   | 46 => (1 * s V_fft_short__tmp + -1 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_j <= 0)%Z
   | 47 => (-1 * s V_fft_short_j <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ 1 * s V_fft_short__tmp + -1 <= 0)%Z
   | 48 => (1 * s V_fft_short__tmp + -1 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_j <= 0 /\ 1 * s V_fft_short_j <= 0)%Z
   | 49 => (1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ 1 * s V_fft_short__tmp + -1 <= 0)%Z
   | 50 => (-1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_j <= 0 /\ 1 * s V_fft_short_j <= 0)%Z
   | 51 => (1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0)%Z
   | 52 => (-1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_j <= 0 /\ 1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_b + 1 <= 0)%Z
   | 53 => (-1 * s V_fft_short_b + 1 <= 0 /\ 1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_k + 192 <= 0)%Z
   | 54 => (-1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_j <= 0 /\ 1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_b + 1 <= 0)%Z
   | 55 => (-1 * s V_fft_short_b + 1 <= 0 /\ 1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_j <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z + 1 <= 0)%Z
   | 56 => (1 * s V_fft_short__tmp + -1 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0)%Z
   | 57 => (-1 * s V_fft_short_j + 1 <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ 1 * s V_fft_short__tmp + -1 <= 0)%Z
   | 58 => (1 * s V_fft_short__tmp + -1 <= 0 /\ -1 * s V_fft_short_b <= 0 /\ -1 * s V_fft_short_k + 192 <= 0 /\ -1 * s V_fft_short_z <= 0 /\ 1 * s V_fft_short_j + -30 <= 0 /\ -1 * s V_fft_short_j + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_fft_short (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((93 # 1) <= z)%Q
   | 2 => ((93 # 1) + s V_fft_short_z <= z)%Q
   | 3 => ((93 # 1) + s V_fft_short_z <= z)%Q
   | 4 => (s V_fft_short_z + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 5 => (s V_fft_short_z + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 6 => (s V_fft_short_z + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 7 => hints
     [(*-31 0*) F_max0_monotonic (F_check_ge (3 - s V_fft_short_b) (2
                                                                    - 
                                                                    s V_fft_short_b));
      (*-31 0*) F_max0_ge_0 (2 - s V_fft_short_b)]
     (s V_fft_short_z + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 8 => (s V_fft_short_z <= z)%Q
   | 9 => (s V_fft_short_z + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 10 => (s V_fft_short_z + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 11 => (s V_fft_short_z + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 12 => (-(31 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 13 => (-(31 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 14 => (-(31 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 15 => (-(31 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 16 => hints
     [(*-31 0*) F_max0_pre_decrement 1 (3 - s V_fft_short_b) (1)]
     (-(31 # 1) + s V_fft_short_j + s V_fft_short_z
      + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 17 => (s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 18 => (s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 19 => (s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 20 => (s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 21 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 22 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_fft_short_j) (-1
                                                               + s V_fft_short_j));
      (*-1 0*) F_max0_ge_0 (-1 + s V_fft_short_j)]
     ((1 # 1) + s V_fft_short_j + s V_fft_short_z
      + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 24 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) - max0(s V_fft_short_j) <= z)%Q
   | 25 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 26 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 27 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 28 => hints
     [(*-31 0*) F_max0_pre_decrement 1 (3 - s V_fft_short_b) (1)]
     (-(31 # 1) + s V_fft_short_j + s V_fft_short_z
      + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 29 => (s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 30 => (s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 31 => (s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 32 => (s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 33 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 34 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_fft_short_j) (-1
                                                               + s V_fft_short_j));
      (*-1 0*) F_max0_ge_0 (-1 + s V_fft_short_j)]
     ((1 # 1) + s V_fft_short_j + s V_fft_short_z
      + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 36 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) - max0(s V_fft_short_j) <= z)%Q
   | 37 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) - max0(s V_fft_short_j) <= z)%Q
   | 38 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 39 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 40 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 41 => hints
     [(*-31 0*) F_max0_pre_decrement 1 (3 - s V_fft_short_b) (1)]
     (-(31 # 1) + s V_fft_short_j + s V_fft_short_z
      + (31 # 1) * max0(3 - s V_fft_short_b) <= z)%Q
   | 42 => (s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 43 => (s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 44 => (s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 45 => (s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 46 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 47 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 48 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_fft_short_j) (-1
                                                               + s V_fft_short_j));
      (*-1 0*) F_max0_ge_0 (-1 + s V_fft_short_j)]
     ((1 # 1) + s V_fft_short_j + s V_fft_short_z
      + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 49 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) - max0(s V_fft_short_j) <= z)%Q
   | 50 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) - max0(s V_fft_short_j) <= z)%Q
   | 51 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) - max0(s V_fft_short_j) <= z)%Q
   | 52 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(3 - s V_fft_short_b) - max0(s V_fft_short_j) <= z)%Q
   | 53 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(3 - s V_fft_short_b) - max0(s V_fft_short_j) <= z)%Q
   | 54 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(3 - s V_fft_short_b) - max0(s V_fft_short_j) <= z)%Q
   | 55 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fft_short_j) (0))) (F_max0_ge_0 (s V_fft_short_j))]
     (s V_fft_short_j + s V_fft_short_z
      + (31 # 1) * max0(3 - s V_fft_short_b) - max0(s V_fft_short_j) <= z)%Q
   | 56 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 57 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | 58 => ((1 # 1) + s V_fft_short_j + s V_fft_short_z
            + (31 # 1) * max0(2 - s V_fft_short_b) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_fft_short =>
    [mkPA Q (fun n z s => ai_fft_short n s /\ annot0_fft_short n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_fft_short (proc_start P_fft_short) s1 (proc_end P_fft_short) s2 ->
    (s2 V_fft_short_z <= (93 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_fft_short.
Qed.
