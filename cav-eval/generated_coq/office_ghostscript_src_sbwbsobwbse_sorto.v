Require Import pasta.Pasta.

Inductive proc: Type :=
  P_bwbse_sort.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_bwbse_sort_z := 1%positive.
Notation V_bwbse_sort__tmp := 2%positive.
Notation V_bwbse_sort_ch := 3%positive.
Notation V_bwbse_sort_j := 4%positive.
Notation V_bwbse_sort_sum := 5%positive.
Notation V_bwbse_sort_N := 6%positive.
Notation V_bwbse_sort_buffer := 7%positive.
Notation V_bwbse_sort_indices := 8%positive.
Definition Pedges_bwbse_sort: list (edge proc) :=
  (EA 1 (AAssign V_bwbse_sort_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_bwbse_sort_j) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 (AGuard (fun s => ((eval (EVar V_bwbse_sort_ch) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_bwbse_sort__tmp) s) >= (eval (ENum (0))
  s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 (AAssign V_bwbse_sort__tmp
  (Some (EVar V_bwbse_sort_N))) 7)::(EA 7 (AAssign V_bwbse_sort_sum
  (Some (ENum (0)))) 8)::(EA 8 (AAssign V_bwbse_sort_j
  (Some (ENum (0)))) 9)::(EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_bwbse_sort_j) s) < (eval (EVar V_bwbse_sort__tmp)
  s))%Z)) 53)::(EA 11 (AGuard (fun s => ((eval (EVar V_bwbse_sort_j) s) >=
  (eval (EVar V_bwbse_sort__tmp) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 13 (AAssign V_bwbse_sort_ch (Some (ENum (0)))) 14)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::(EA 16 (AGuard (fun s => ((eval (EVar V_bwbse_sort_ch)
  s) <= (eval (ENum (255)) s))%Z)) 45)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_bwbse_sort_ch) s) > (eval (ENum (255))
  s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 18 (AAssign V_bwbse_sort_j
  (Some (ENum (0)))) 19)::(EA 19 ANone 20)::(EA 20 AWeaken 21)::
  (EA 21 (AGuard (fun s => ((eval (EVar V_bwbse_sort_j) s) <
  (eval (EVar V_bwbse_sort__tmp) s))%Z)) 38)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_bwbse_sort_j) s) >= (eval (EVar V_bwbse_sort__tmp)
  s))%Z)) 22)::(EA 22 AWeaken 23)::(EA 23 (AAssign V_bwbse_sort_sum
  (Some (ENum (0)))) 24)::(EA 24 (AAssign V_bwbse_sort_ch
  (Some (ENum (0)))) 25)::(EA 25 ANone 26)::(EA 26 AWeaken 27)::
  (EA 27 (AGuard (fun s => ((eval (EVar V_bwbse_sort_ch) s) <=
  (eval (ENum (255)) s))%Z)) 30)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_bwbse_sort_ch) s) > (eval (ENum (255))
  s))%Z)) 28)::(EA 28 AWeaken 29)::(EA 30 AWeaken 31)::(EA 31 ANone 32)::
  (EA 32 (AAssign V_bwbse_sort_sum None) 33)::(EA 33 (AAssign V_bwbse_sort_ch
  (Some (EAdd (EVar V_bwbse_sort_ch) (ENum (1))))) 34)::(EA 34 ANone 35)::
  (EA 35 ANone 36)::(EA 36 (AAssign V_bwbse_sort_z (Some (EAdd (ENum (1))
  (EVar V_bwbse_sort_z)))) 37)::(EA 37 AWeaken 27)::(EA 38 AWeaken 39)::
  (EA 39 ANone 40)::(EA 40 (AAssign V_bwbse_sort_j
  (Some (EAdd (EVar V_bwbse_sort_j) (ENum (1))))) 41)::(EA 41 ANone 42)::
  (EA 42 ANone 43)::(EA 43 (AAssign V_bwbse_sort_z (Some (EAdd (ENum (1))
  (EVar V_bwbse_sort_z)))) 44)::(EA 44 AWeaken 21)::(EA 45 AWeaken 46)::
  (EA 46 (AAssign V_bwbse_sort_sum None) 47)::(EA 47 ANone 48)::
  (EA 48 (AAssign V_bwbse_sort_ch (Some (EAdd (EVar V_bwbse_sort_ch)
  (ENum (1))))) 49)::(EA 49 ANone 50)::(EA 50 ANone 51)::(EA 51 (AAssign
  V_bwbse_sort_z (Some (EAdd (ENum (1)) (EVar V_bwbse_sort_z)))) 52)::
  (EA 52 AWeaken 16)::(EA 53 AWeaken 54)::(EA 54 ANone 55)::(EA 55 (AAssign
  V_bwbse_sort_j (Some (EAdd (EVar V_bwbse_sort_j) (ENum (1))))) 56)::
  (EA 56 ANone 57)::(EA 57 ANone 58)::(EA 58 (AAssign V_bwbse_sort_z
  (Some (EAdd (ENum (1)) (EVar V_bwbse_sort_z)))) 59)::(EA 59 AWeaken 11)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_bwbse_sort => Pedges_bwbse_sort
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_bwbse_sort => 29
     end)%positive;
  var_global := var_global
}.

Definition ai_bwbse_sort (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_z <= 0)%Z
   | 3 => (-1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0)%Z
   | 4 => (-1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch <= 0)%Z
   | 5 => (-1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort__tmp <= 0)%Z
   | 6 => (-1 * s V_bwbse_sort__tmp <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch <= 0)%Z
   | 7 => (-1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0)%Z
   | 8 => (-1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_sum <= 0)%Z
   | 9 => (-1 * s V_bwbse_sort_sum <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0)%Z
   | 10 => (-1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_sum <= 0)%Z
   | 11 => (-1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_sum <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_ch <= 0)%Z
   | 12 => (-1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0)%Z
   | 13 => (1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_sum <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_ch <= 0)%Z
   | 14 => (1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_ch <= 0)%Z
   | 15 => (-1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_sum <= 0 /\ 1 * s V_bwbse_sort_sum <= 0)%Z
   | 16 => (-1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0)%Z
   | 17 => (1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch + 256 <= 0)%Z
   | 18 => (-1 * s V_bwbse_sort_ch + 256 <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0)%Z
   | 19 => (1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch + 256 <= 0 /\ 1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0)%Z
   | 20 => (-1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_ch + 256 <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0)%Z
   | 21 => (-1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_ch + 256 <= 0)%Z
   | 22 => (-1 * s V_bwbse_sort_ch + 256 <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0)%Z
   | 23 => (1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_ch + 256 <= 0)%Z
   | 24 => (-1 * s V_bwbse_sort_ch + 256 <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_sum <= 0)%Z
   | 25 => (-1 * s V_bwbse_sort_sum <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_ch <= 0)%Z
   | 26 => (-1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_sum <= 0)%Z
   | 27 => (-1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0)%Z
   | 28 => (1 * s V_bwbse_sort_ch + -256 <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch + 256 <= 0)%Z
   | 29 => (-1 * s V_bwbse_sort_ch + 256 <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0)%Z
   | 30 => (1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_ch + -255 <= 0)%Z
   | 31 => (1 * s V_bwbse_sort_ch + -255 <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0)%Z
   | 32 => (1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_ch + -255 <= 0)%Z
   | 33 => (1 * s V_bwbse_sort_ch + -255 <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0)%Z
   | 34 => (1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_ch + 1 <= 0)%Z
   | 35 => (-1 * s V_bwbse_sort_ch + 1 <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0)%Z
   | 36 => (1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_ch + 1 <= 0)%Z
   | 37 => (-1 * s V_bwbse_sort_ch + 1 <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z + 1 <= 0)%Z
   | 38 => (-1 * s V_bwbse_sort_ch + 256 <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j + 1 <= 0)%Z
   | 39 => (-1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j + 1 <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_ch + 256 <= 0)%Z
   | 40 => (-1 * s V_bwbse_sort_ch + 256 <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j + 1 <= 0)%Z
   | 41 => (-1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_ch + 256 <= 0 /\ -1 * s V_bwbse_sort_j + 1 <= 0 /\ -1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j <= 0)%Z
   | 42 => (-1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j + 1 <= 0 /\ -1 * s V_bwbse_sort_ch + 256 <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_z <= 0)%Z
   | 43 => (-1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_ch + 256 <= 0 /\ -1 * s V_bwbse_sort_j + 1 <= 0 /\ -1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j <= 0)%Z
   | 44 => (-1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j + 1 <= 0 /\ -1 * s V_bwbse_sort_ch + 256 <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_z + 1 <= 0)%Z
   | 45 => (-1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_ch + -255 <= 0)%Z
   | 46 => (1 * s V_bwbse_sort_ch + -255 <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0)%Z
   | 47 => (-1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_ch + -255 <= 0)%Z
   | 48 => (1 * s V_bwbse_sort_ch + -255 <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0)%Z
   | 49 => (-1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_ch + 1 <= 0)%Z
   | 50 => (-1 * s V_bwbse_sort_ch + 1 <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0)%Z
   | 51 => (-1 * s V_bwbse_sort_j <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ -1 * s V_bwbse_sort_ch + 1 <= 0)%Z
   | 52 => (-1 * s V_bwbse_sort_ch + 1 <= 0 /\ 1 * s V_bwbse_sort_ch + -256 <= 0 /\ 1 * s V_bwbse_sort__tmp+ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z + 1 <= 0)%Z
   | 53 => (-1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j + 1 <= 0)%Z
   | 54 => (-1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j + 1 <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_sum <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_ch <= 0)%Z
   | 55 => (-1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j + 1 <= 0)%Z
   | 56 => (-1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_sum <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_j + 1 <= 0 /\ -1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j <= 0)%Z
   | 57 => (-1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j + 1 <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_z <= 0)%Z
   | 58 => (-1 * s V_bwbse_sort_z <= 0 /\ -1 * s V_bwbse_sort_sum <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ -1 * s V_bwbse_sort_j + 1 <= 0 /\ -1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j <= 0)%Z
   | 59 => (-1 * s V_bwbse_sort__tmp+ 1 * s V_bwbse_sort_j <= 0 /\ -1 * s V_bwbse_sort_j + 1 <= 0 /\ -1 * s V_bwbse_sort_ch <= 0 /\ 1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_sum <= 0 /\ -1 * s V_bwbse_sort_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_bwbse_sort (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((512 # 1) + (2 # 1) * max0(s V_bwbse_sort_N) <= z)%Q
   | 2 => ((512 # 1) + s V_bwbse_sort_z + (2 # 1) * max0(s V_bwbse_sort_N) <= z)%Q
   | 3 => ((512 # 1) + s V_bwbse_sort_z + (2 # 1) * max0(s V_bwbse_sort_N) <= z)%Q
   | 4 => ((512 # 1) + s V_bwbse_sort_z + (2 # 1) * max0(s V_bwbse_sort_N) <= z)%Q
   | 5 => ((512 # 1) + s V_bwbse_sort_z + (2 # 1) * max0(s V_bwbse_sort_N) <= z)%Q
   | 6 => ((512 # 1) + s V_bwbse_sort_z + (2 # 1) * max0(s V_bwbse_sort_N) <= z)%Q
   | 7 => ((512 # 1) + s V_bwbse_sort_z + (2 # 1) * max0(s V_bwbse_sort__tmp) <= z)%Q
   | 8 => ((512 # 1) + s V_bwbse_sort_z + (2 # 1) * max0(s V_bwbse_sort__tmp) <= z)%Q
   | 9 => ((512 # 1) + s V_bwbse_sort_z + max0(s V_bwbse_sort__tmp)
           + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 10 => ((512 # 1) + s V_bwbse_sort_z + max0(s V_bwbse_sort__tmp)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 11 => ((512 # 1) + s V_bwbse_sort_z + max0(s V_bwbse_sort__tmp)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 12 => ((512 # 1) + s V_bwbse_sort_z + max0(s V_bwbse_sort__tmp)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 13 => ((512 # 1) + s V_bwbse_sort_z + max0(s V_bwbse_sort__tmp)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 14 => ((512 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(s V_bwbse_sort__tmp)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_bwbse_sort__tmp
                                             - s V_bwbse_sort_j) (-1
                                                                  + s V_bwbse_sort__tmp
                                                                  - s V_bwbse_sort_j))]
     ((512 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
      + max0(s V_bwbse_sort__tmp)
      + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 16 => ((512 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)]
     ((512 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
      + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
      + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 18 => ((512 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 19 => ((512 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (256 - s V_bwbse_sort_ch) (255
                                                                    - s V_bwbse_sort_ch));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (256
                                                               - s V_bwbse_sort_ch) (0))) (F_max0_ge_0 (256
                                                                    - s V_bwbse_sort_ch))]
     ((512 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
      + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 21 => ((256 # 1) + s V_bwbse_sort_z + max0(255 - s V_bwbse_sort_ch)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_ge_0 (255 - s V_bwbse_sort_ch)]
     ((256 # 1) + s V_bwbse_sort_z + max0(255 - s V_bwbse_sort_ch)
      + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 23 => ((256 # 1) + s V_bwbse_sort_z
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 24 => ((256 # 1) + s V_bwbse_sort_z
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 25 => ((256 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_bwbse_sort__tmp
                                             - s V_bwbse_sort_j) (-1
                                                                  + s V_bwbse_sort__tmp
                                                                  - s V_bwbse_sort_j))]
     ((256 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
      + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 27 => ((256 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 28 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j);
      (*-1 0*) F_max0_monotonic (F_check_ge (256 - s V_bwbse_sort_ch) (255
                                                                    - s V_bwbse_sort_ch));
      (*-1 0*) F_max0_ge_0 (255 - s V_bwbse_sort_ch);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (256
                                                               - s V_bwbse_sort_ch) (0))) (F_max0_ge_0 (256
                                                                    - s V_bwbse_sort_ch))]
     ((256 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
      + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 29 => (s V_bwbse_sort_z <= z)%Q
   | 30 => ((256 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 31 => ((256 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 32 => ((256 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 33 => ((256 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 34 => ((257 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 35 => ((257 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 36 => ((257 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 37 => ((256 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_bwbse_sort__tmp - s V_bwbse_sort_j) (1)]
     ((256 # 1) + s V_bwbse_sort_z + max0(255 - s V_bwbse_sort_ch)
      + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 39 => ((257 # 1) + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(255 - s V_bwbse_sort_ch) <= z)%Q
   | 40 => ((257 # 1) + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(255 - s V_bwbse_sort_ch) <= z)%Q
   | 41 => ((257 # 1) + s V_bwbse_sort_z + max0(255 - s V_bwbse_sort_ch)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 42 => ((257 # 1) + s V_bwbse_sort_z + max0(255 - s V_bwbse_sort_ch)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 43 => ((257 # 1) + s V_bwbse_sort_z + max0(255 - s V_bwbse_sort_ch)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 44 => ((256 # 1) + s V_bwbse_sort_z + max0(255 - s V_bwbse_sort_ch)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 45 => ((512 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 46 => ((512 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 47 => ((512 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 48 => ((512 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 49 => ((513 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 50 => ((513 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 51 => ((513 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 52 => ((512 # 1) - s V_bwbse_sort_ch + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 53 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_bwbse_sort__tmp - s V_bwbse_sort_j) (1)]
     ((512 # 1) + s V_bwbse_sort_z + max0(s V_bwbse_sort__tmp)
      + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 54 => ((513 # 1) + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 55 => ((513 # 1) + s V_bwbse_sort_z
            + max0(-1 + s V_bwbse_sort__tmp - s V_bwbse_sort_j)
            + max0(s V_bwbse_sort__tmp) <= z)%Q
   | 56 => ((513 # 1) + s V_bwbse_sort_z + max0(s V_bwbse_sort__tmp)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 57 => ((513 # 1) + s V_bwbse_sort_z + max0(s V_bwbse_sort__tmp)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 58 => ((513 # 1) + s V_bwbse_sort_z + max0(s V_bwbse_sort__tmp)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | 59 => ((512 # 1) + s V_bwbse_sort_z + max0(s V_bwbse_sort__tmp)
            + max0(s V_bwbse_sort__tmp - s V_bwbse_sort_j) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_bwbse_sort =>
    [mkPA Q (fun n z s => ai_bwbse_sort n s /\ annot0_bwbse_sort n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_bwbse_sort (proc_start P_bwbse_sort) s1 (proc_end P_bwbse_sort) s2 ->
    (s2 V_bwbse_sort_z <= (512 # 1) + (2 # 1) * max0(s1 V_bwbse_sort_N))%Q.
Proof.
  prove_bound ipa admissible_ipa P_bwbse_sort.
Qed.
