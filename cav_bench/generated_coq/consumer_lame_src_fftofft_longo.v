Require Import pasta.Pasta.

Inductive proc: Type :=
  P_fft_long.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_fft_long_z := 1%positive.
Notation V_fft_long__tmp := 2%positive.
Notation V_fft_long_i := 3%positive.
Notation V_fft_long_jj := 4%positive.
Notation V_fft_long_buffer := 5%positive.
Notation V_fft_long_chn := 6%positive.
Notation V_fft_long_x := 7%positive.
Definition Pedges_fft_long: list (edge proc) :=
  (EA 1 (AAssign V_fft_long_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_fft_long__tmp (Some (EVar V_fft_long_chn))) 3)::(EA 3 (AAssign
  V_fft_long_jj (Some (ENum (127)))) 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_fft_long__tmp) s) < (eval (ENum (2)) s))%Z)) 34)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_fft_long__tmp) s) >=
  (eval (ENum (2)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_fft_long__tmp) s) = (eval (ENum (2)) s))%Z)) 20)::
  (EA 7 (AGuard (fun s => ((eval (EVar V_fft_long__tmp) s) <>
  (eval (ENum (2)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 ANone 10)::
  (EA 10 (AAssign V_fft_long_i None) 11)::(EA 11 ANone 12)::(EA 12 (AAssign
  V_fft_long_jj (Some (EAdd (EVar V_fft_long_jj) (ENum (-1))))) 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EAdd (EVar V_fft_long_jj) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 17)::(EA 14 (AGuard
  (fun s => ((eval (EAdd (EVar V_fft_long_jj) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 16 ANone 29)::
  (EA 17 AWeaken 18)::(EA 18 ANone 19)::(EA 19 (AAssign V_fft_long_z
  (Some (EAdd (ENum (1)) (EVar V_fft_long_z)))) 10)::(EA 20 AWeaken 21)::
  (EA 21 ANone 22)::(EA 22 (AAssign V_fft_long_i None) 23)::
  (EA 23 ANone 24)::(EA 24 (AAssign V_fft_long_jj
  (Some (EAdd (EVar V_fft_long_jj) (ENum (-1))))) 25)::(EA 25 AWeaken 26)::
  (EA 26 (AGuard (fun s => ((eval (EAdd (EVar V_fft_long_jj) (ENum (-1)))
  s) >= (eval (ENum (0)) s))%Z)) 31)::(EA 26 (AGuard
  (fun s => ((eval (EAdd (EVar V_fft_long_jj) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 27)::(EA 27 AWeaken 28)::(EA 28 ANone 29)::
  (EA 29 ANone 30)::(EA 30 AWeaken 44)::(EA 31 AWeaken 32)::
  (EA 32 ANone 33)::(EA 33 (AAssign V_fft_long_z (Some (EAdd (ENum (1))
  (EVar V_fft_long_z)))) 22)::(EA 34 AWeaken 35)::(EA 35 ANone 36)::
  (EA 36 (AAssign V_fft_long_i None) 37)::(EA 37 ANone 38)::(EA 38 (AAssign
  V_fft_long_jj (Some (EAdd (EVar V_fft_long_jj) (ENum (-1))))) 39)::
  (EA 39 AWeaken 40)::(EA 40 (AGuard
  (fun s => ((eval (EAdd (EVar V_fft_long_jj) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 45)::(EA 40 (AGuard
  (fun s => ((eval (EAdd (EVar V_fft_long_jj) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 41)::(EA 41 AWeaken 42)::(EA 42 ANone 43)::
  (EA 43 AWeaken 44)::(EA 45 AWeaken 46)::(EA 46 ANone 47)::(EA 47 (AAssign
  V_fft_long_z (Some (EAdd (ENum (1)) (EVar V_fft_long_z)))) 36)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_fft_long => Pedges_fft_long
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_fft_long => 44
     end)%positive;
  var_global := var_global
}.

Definition ai_fft_long (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long_z <= 0)%Z
   | 3 => (-1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_z <= 0)%Z
   | 4 => (1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_jj + 127 <= 0)%Z
   | 5 => (-1 * s V_fft_long_jj + 127 <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_z <= 0)%Z
   | 6 => (1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_jj + 127 <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0)%Z
   | 7 => (-1 * s V_fft_long__tmp + 2 <= 0 /\ -1 * s V_fft_long_jj + 127 <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_z <= 0)%Z
   | 8 => (1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_jj + 127 <= 0 /\ -1 * s V_fft_long__tmp + 3 <= 0)%Z
   | 9 => (-1 * s V_fft_long__tmp + 3 <= 0 /\ -1 * s V_fft_long_jj + 127 <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_z <= 0)%Z
   | 10 => (-1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0 /\ -1 * s V_fft_long__tmp + 3 <= 0)%Z
   | 11 => (-1 * s V_fft_long__tmp + 3 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_z <= 0)%Z
   | 12 => (-1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0 /\ -1 * s V_fft_long__tmp + 3 <= 0)%Z
   | 13 => (-1 * s V_fft_long__tmp + 3 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_jj <= 0)%Z
   | 14 => (-1 * s V_fft_long_jj <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long__tmp + 3 <= 0)%Z
   | 15 => (-1 * s V_fft_long__tmp + 3 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long_jj <= 0 /\ 1 * s V_fft_long_jj <= 0)%Z
   | 16 => (1 * s V_fft_long_jj <= 0 /\ -1 * s V_fft_long_jj <= 0 /\ -1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long__tmp + 3 <= 0)%Z
   | 17 => (-1 * s V_fft_long__tmp + 3 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0)%Z
   | 18 => (-1 * s V_fft_long_jj + 1 <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long__tmp + 3 <= 0)%Z
   | 19 => (-1 * s V_fft_long__tmp + 3 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0)%Z
   | 20 => (1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_jj + 127 <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0 /\ 1 * s V_fft_long__tmp + -2 <= 0)%Z
   | 21 => (1 * s V_fft_long__tmp + -2 <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0 /\ -1 * s V_fft_long_jj + 127 <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_z <= 0)%Z
   | 22 => (-1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0 /\ 1 * s V_fft_long__tmp + -2 <= 0)%Z
   | 23 => (1 * s V_fft_long__tmp + -2 <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_z <= 0)%Z
   | 24 => (-1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0 /\ 1 * s V_fft_long__tmp + -2 <= 0)%Z
   | 25 => (1 * s V_fft_long__tmp + -2 <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_jj <= 0)%Z
   | 26 => (-1 * s V_fft_long_jj <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0 /\ 1 * s V_fft_long__tmp + -2 <= 0)%Z
   | 27 => (1 * s V_fft_long__tmp + -2 <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long_jj <= 0 /\ 1 * s V_fft_long_jj <= 0)%Z
   | 28 => (1 * s V_fft_long_jj <= 0 /\ -1 * s V_fft_long_jj <= 0 /\ -1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0 /\ 1 * s V_fft_long__tmp + -2 <= 0)%Z
   | 29 => (-1 * s V_fft_long__tmp + 2 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long_jj <= 0 /\ 1 * s V_fft_long_jj <= 0)%Z
   | 30 => (1 * s V_fft_long_jj <= 0 /\ -1 * s V_fft_long_jj <= 0 /\ -1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0)%Z
   | 31 => (1 * s V_fft_long__tmp + -2 <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0)%Z
   | 32 => (-1 * s V_fft_long_jj + 1 <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0 /\ 1 * s V_fft_long__tmp + -2 <= 0)%Z
   | 33 => (1 * s V_fft_long__tmp + -2 <= 0 /\ -1 * s V_fft_long__tmp + 2 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0)%Z
   | 34 => (1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_jj + 127 <= 0 /\ 1 * s V_fft_long__tmp + -1 <= 0)%Z
   | 35 => (1 * s V_fft_long__tmp + -1 <= 0 /\ -1 * s V_fft_long_jj + 127 <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_z <= 0)%Z
   | 36 => (-1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0 /\ 1 * s V_fft_long__tmp + -1 <= 0)%Z
   | 37 => (1 * s V_fft_long__tmp + -1 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_z <= 0)%Z
   | 38 => (-1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -127 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0 /\ 1 * s V_fft_long__tmp + -1 <= 0)%Z
   | 39 => (1 * s V_fft_long__tmp + -1 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_jj <= 0)%Z
   | 40 => (-1 * s V_fft_long_jj <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long__tmp + -1 <= 0)%Z
   | 41 => (1 * s V_fft_long__tmp + -1 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long_jj <= 0 /\ 1 * s V_fft_long_jj <= 0)%Z
   | 42 => (1 * s V_fft_long_jj <= 0 /\ -1 * s V_fft_long_jj <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long__tmp + -1 <= 0)%Z
   | 43 => (1 * s V_fft_long__tmp + -1 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ -1 * s V_fft_long_jj <= 0 /\ 1 * s V_fft_long_jj <= 0)%Z
   | 44 => (1 * s V_fft_long_jj <= 0 /\ -1 * s V_fft_long_jj <= 0 /\ -1 * s V_fft_long_z <= 0)%Z
   | 45 => (1 * s V_fft_long__tmp + -1 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0)%Z
   | 46 => (-1 * s V_fft_long_jj + 1 <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long__tmp + -1 <= 0)%Z
   | 47 => (1 * s V_fft_long__tmp + -1 <= 0 /\ -1 * s V_fft_long_z <= 0 /\ 1 * s V_fft_long_jj + -126 <= 0 /\ -1 * s V_fft_long_jj + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_fft_long (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((126 # 1) <= z)%Q
   | 2 => ((126 # 1) + s V_fft_long_z <= z)%Q
   | 3 => ((126 # 1) + s V_fft_long_z <= z)%Q
   | 4 => hints
     [(*-0.492126 0*) F_binom_monotonic 1 (F_max0_ge_0 (127 - s V_fft_long_jj)) (F_check_ge (0) (0))]
     (s V_fft_long_z + max0(-1 + s V_fft_long_jj)
      + (31 # 63) * max0(127 - s V_fft_long_jj) <= z)%Q
   | 5 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 6 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 7 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 8 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 9 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 10 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 11 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 12 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 13 => (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 14 => (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 15 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (s V_fft_long_jj)) (F_check_ge (0) (0))]
     (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 16 => (s V_fft_long_z <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_fft_long_jj) (1)]
     (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 18 => ((1 # 1) + s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 19 => ((1 # 1) + s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 20 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 21 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 22 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 23 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 24 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 25 => (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 26 => (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_fft_long_jj) (-1
                                                               + s V_fft_long_jj));
      (*-1 0*) F_max0_ge_0 (-1 + s V_fft_long_jj)]
     (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 28 => (s V_fft_long_z <= z)%Q
   | 29 => (s V_fft_long_z <= z)%Q
   | 30 => (s V_fft_long_z <= z)%Q
   | 31 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_fft_long_jj) (1)]
     (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 32 => ((1 # 1) + s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 33 => ((1 # 1) + s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 34 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 35 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 36 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 37 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 38 => (s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 39 => (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 40 => (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 41 => (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 42 => (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 43 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_fft_long_jj) (-1
                                                               + s V_fft_long_jj));
      (*-1 0*) F_max0_ge_0 (-1 + s V_fft_long_jj)]
     (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 44 => (s V_fft_long_z <= z)%Q
   | 45 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_fft_long_jj) (1)]
     (s V_fft_long_z + max0(s V_fft_long_jj) <= z)%Q
   | 46 => ((1 # 1) + s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | 47 => ((1 # 1) + s V_fft_long_z + max0(-1 + s V_fft_long_jj) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_fft_long =>
    [mkPA Q (fun n z s => ai_fft_long n s /\ annot0_fft_long n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_fft_long (proc_start P_fft_long) s1 (proc_end P_fft_long) s2 ->
    (s2 V_fft_long_z <= (126 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_fft_long.
Qed.
