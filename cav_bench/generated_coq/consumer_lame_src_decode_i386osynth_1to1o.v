Require Import pasta.Pasta.

Inductive proc: Type :=
  P_synth_1to1.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_synth_1to1_z := 1%positive.
Notation V_synth_1to1__tmp := 2%positive.
Notation V_synth_1to1_bo := 3%positive.
Notation V_synth_1to1_bo1 := 4%positive.
Notation V_synth_1to1_clip := 5%positive.
Notation V_synth_1to1_gmp_dref_off31872 := 6%positive.
Notation V_synth_1to1_j := 7%positive.
Notation V_synth_1to1_pnt_dref := 8%positive.
Notation V_synth_1to1_bandPtr := 9%positive.
Notation V_synth_1to1_channel := 10%positive.
Notation V_synth_1to1_out := 11%positive.
Notation V_synth_1to1_pnt := 12%positive.
Definition Pedges_synth_1to1: list (edge proc) :=
  (EA 1 (AAssign V_synth_1to1_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_synth_1to1__tmp (Some (EVar V_synth_1to1_channel))) 3)::(EA 3 (AAssign
  V_synth_1to1_clip (Some (ENum (0)))) 4)::(EA 4 (AAssign V_synth_1to1_bo
  (Some (EVar V_synth_1to1_gmp_dref_off31872))) 5)::(EA 5 AWeaken 6)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_synth_1to1__tmp) s) <>
  (eval (ENum (0)) s))%Z)) 12)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_synth_1to1__tmp) s) = (eval (ENum (0))
  s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign V_synth_1to1_bo
  (Some (EAdd (EVar V_synth_1to1_bo) (ENum (-1))))) 9)::(EA 9 (AAssign
  V_synth_1to1_bo None) 10)::(EA 10 ANone 11)::(EA 11 AWeaken 15)::
  (EA 12 AWeaken 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 ANone 18)::(EA 15 ANone 16)::(EA 16 (AAssign V_synth_1to1_bo1
  (Some (EAdd (EVar V_synth_1to1_bo) (ENum (1))))) 17)::(EA 17 ANone 20)::
  (EA 18 (AAssign V_synth_1to1_bo1 (Some (EVar V_synth_1to1_bo))) 19)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_synth_1to1_gmp_dref_off31872
  (Some (EVar V_synth_1to1_bo))) 21)::(EA 21 (AAssign V_synth_1to1_j
  (Some (ENum (16)))) 22)::(EA 22 ANone 23)::(EA 23 AWeaken 24)::
  (EA 24 (AGuard (fun s => ((eval (EVar V_synth_1to1_j) s) <>
  (eval (ENum (0)) s))%Z)) 59)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_synth_1to1_j) s) = (eval (ENum (0)) s))%Z)) 25)::
  (EA 25 AWeaken 26)::(EA 26 ANone 33)::(EA 26 ANone 27)::
  (EA 27 AWeaken 28)::(EA 28 ANone 30)::(EA 28 ANone 29)::(EA 29 ANone 32)::
  (EA 30 (AAssign V_synth_1to1_clip (Some (EAdd (EVar V_synth_1to1_clip)
  (ENum (1))))) 31)::(EA 31 ANone 32)::(EA 32 ANone 35)::(EA 33 (AAssign
  V_synth_1to1_clip (Some (EAdd (EVar V_synth_1to1_clip) (ENum (1))))) 34)::
  (EA 34 ANone 35)::(EA 35 (AAssign V_synth_1to1_j (Some (ENum (15)))) 36)::
  (EA 36 ANone 37)::(EA 37 AWeaken 38)::(EA 38 (AGuard
  (fun s => ((eval (EVar V_synth_1to1_j) s) <> (eval (ENum (0)) s))%Z)) 43)::
  (EA 38 (AGuard (fun s => ((eval (EVar V_synth_1to1_j) s) = (eval (ENum (0))
  s))%Z)) 39)::(EA 39 AWeaken 40)::(EA 40 (AAssign V_synth_1to1_pnt_dref
  (Some (EAdd (EVar V_synth_1to1_pnt_dref) (ENum (128))))) 41)::
  (EA 41 AWeaken 42)::(EA 43 AWeaken 44)::(EA 44 ANone 51)::
  (EA 44 ANone 45)::(EA 45 AWeaken 46)::(EA 46 ANone 48)::(EA 46 ANone 47)::
  (EA 47 ANone 50)::(EA 48 (AAssign V_synth_1to1_clip
  (Some (EAdd (EVar V_synth_1to1_clip) (ENum (1))))) 49)::(EA 49 ANone 50)::
  (EA 50 ANone 53)::(EA 51 (AAssign V_synth_1to1_clip
  (Some (EAdd (EVar V_synth_1to1_clip) (ENum (1))))) 52)::(EA 52 ANone 53)::
  (EA 53 ANone 54)::(EA 54 (AAssign V_synth_1to1_j
  (Some (EAdd (EVar V_synth_1to1_j) (ENum (-1))))) 55)::(EA 55 ANone 56)::
  (EA 56 ANone 57)::(EA 57 (AAssign V_synth_1to1_z (Some (EAdd (ENum (1))
  (EVar V_synth_1to1_z)))) 58)::(EA 58 AWeaken 38)::(EA 59 AWeaken 60)::
  (EA 60 ANone 67)::(EA 60 ANone 61)::(EA 61 AWeaken 62)::(EA 62 ANone 64)::
  (EA 62 ANone 63)::(EA 63 ANone 66)::(EA 64 (AAssign V_synth_1to1_clip
  (Some (EAdd (EVar V_synth_1to1_clip) (ENum (1))))) 65)::(EA 65 ANone 66)::
  (EA 66 ANone 69)::(EA 67 (AAssign V_synth_1to1_clip
  (Some (EAdd (EVar V_synth_1to1_clip) (ENum (1))))) 68)::(EA 68 ANone 69)::
  (EA 69 ANone 70)::(EA 70 (AAssign V_synth_1to1_j
  (Some (EAdd (EVar V_synth_1to1_j) (ENum (-1))))) 71)::(EA 71 ANone 72)::
  (EA 72 ANone 73)::(EA 73 (AAssign V_synth_1to1_z (Some (EAdd (ENum (1))
  (EVar V_synth_1to1_z)))) 74)::(EA 74 AWeaken 24)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_synth_1to1 => Pedges_synth_1to1
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_synth_1to1 => 42
     end)%positive;
  var_global := var_global
}.

Definition ai_synth_1to1 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_z <= 0)%Z
   | 3 => (-1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_z <= 0)%Z
   | 4 => (1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 5 => (-1 * s V_synth_1to1_clip <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_z <= 0)%Z
   | 6 => (1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 7 => (-1 * s V_synth_1to1_clip <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1__tmp <= 0 /\ -1 * s V_synth_1to1__tmp <= 0)%Z
   | 8 => (-1 * s V_synth_1to1__tmp <= 0 /\ 1 * s V_synth_1to1__tmp <= 0 /\ 1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 9 => (-1 * s V_synth_1to1_clip <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1__tmp <= 0 /\ -1 * s V_synth_1to1__tmp <= 0)%Z
   | 10 => (-1 * s V_synth_1to1__tmp <= 0 /\ 1 * s V_synth_1to1__tmp <= 0 /\ 1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 11 => (-1 * s V_synth_1to1_clip <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1__tmp <= 0 /\ -1 * s V_synth_1to1__tmp <= 0)%Z
   | 12 => (-1 * s V_synth_1to1_clip <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_z <= 0)%Z
   | 13 => (1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 14 => (-1 * s V_synth_1to1_clip <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_z <= 0)%Z
   | 15 => (1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 16 => (-1 * s V_synth_1to1_clip <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_z <= 0)%Z
   | 17 => (1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 18 => (-1 * s V_synth_1to1_clip <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_z <= 0)%Z
   | 19 => (1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 20 => (-1 * s V_synth_1to1_clip <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_z <= 0)%Z
   | 21 => (1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 22 => (-1 * s V_synth_1to1_clip <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -16 <= 0 /\ -1 * s V_synth_1to1_j + 16 <= 0)%Z
   | 23 => (-1 * s V_synth_1to1_j + 16 <= 0 /\ 1 * s V_synth_1to1_j + -16 <= 0 /\ 1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 24 => (-1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -16 <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 25 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_j <= 0)%Z
   | 26 => (-1 * s V_synth_1to1_j <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 27 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_j <= 0)%Z
   | 28 => (-1 * s V_synth_1to1_j <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 29 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_j <= 0)%Z
   | 30 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_j <= 0)%Z
   | 31 => (-1 * s V_synth_1to1_j <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip + 1 <= 0)%Z
   | 32 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_j <= 0)%Z
   | 33 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_j <= 0)%Z
   | 34 => (-1 * s V_synth_1to1_j <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip + 1 <= 0)%Z
   | 35 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_j <= 0)%Z
   | 36 => (-1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0 /\ 1 * s V_synth_1to1_j + -15 <= 0 /\ -1 * s V_synth_1to1_j + 15 <= 0)%Z
   | 37 => (-1 * s V_synth_1to1_j + 15 <= 0 /\ 1 * s V_synth_1to1_j + -15 <= 0 /\ -1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0)%Z
   | 38 => (-1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -15 <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 39 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_j <= 0)%Z
   | 40 => (-1 * s V_synth_1to1_j <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 41 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_j <= 0)%Z
   | 42 => (-1 * s V_synth_1to1_j <= 0 /\ 1 * s V_synth_1to1_j <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 43 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -15 <= 0)%Z
   | 44 => (1 * s V_synth_1to1_j + -15 <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 45 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -15 <= 0)%Z
   | 46 => (1 * s V_synth_1to1_j + -15 <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 47 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -15 <= 0)%Z
   | 48 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -15 <= 0)%Z
   | 49 => (1 * s V_synth_1to1_j + -15 <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip + 1 <= 0)%Z
   | 50 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -15 <= 0)%Z
   | 51 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -15 <= 0)%Z
   | 52 => (1 * s V_synth_1to1_j + -15 <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip + 1 <= 0)%Z
   | 53 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -15 <= 0)%Z
   | 54 => (1 * s V_synth_1to1_j + -15 <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 55 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -14 <= 0)%Z
   | 56 => (1 * s V_synth_1to1_j + -14 <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 57 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -14 <= 0)%Z
   | 58 => (1 * s V_synth_1to1_j + -14 <= 0 /\ -1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z + 1 <= 0)%Z
   | 59 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -16 <= 0)%Z
   | 60 => (1 * s V_synth_1to1_j + -16 <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 61 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -16 <= 0)%Z
   | 62 => (1 * s V_synth_1to1_j + -16 <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 63 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -16 <= 0)%Z
   | 64 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -16 <= 0)%Z
   | 65 => (1 * s V_synth_1to1_j + -16 <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip + 1 <= 0)%Z
   | 66 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -16 <= 0)%Z
   | 67 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -16 <= 0)%Z
   | 68 => (1 * s V_synth_1to1_j + -16 <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip + 1 <= 0)%Z
   | 69 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -16 <= 0)%Z
   | 70 => (1 * s V_synth_1to1_j + -16 <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 71 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -15 <= 0)%Z
   | 72 => (1 * s V_synth_1to1_j + -15 <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ -1 * s V_synth_1to1_clip <= 0)%Z
   | 73 => (-1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z <= 0 /\ 1 * s V_synth_1to1_j + -15 <= 0)%Z
   | 74 => (1 * s V_synth_1to1_j + -15 <= 0 /\ -1 * s V_synth_1to1_clip <= 0 /\ -1 * s V_synth_1to1_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_synth_1to1 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((31 # 1) <= z)%Q
   | 2 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 3 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 4 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 5 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 6 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 7 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 8 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 9 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 10 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 11 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 12 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_1to1_z)) (F_check_ge (0) (0))]
     ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 13 => ((31 # 1) <= z)%Q
   | 14 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_1to1_z) (0))) (F_max0_ge_0 (s V_synth_1to1_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_synth_1to1_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_synth_1to1_z) (0))) (F_max0_ge_0 (-
                                                                    s V_synth_1to1_z))]
     ((31 # 1) <= z)%Q
   | 15 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 16 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 17 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 18 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 19 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 20 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 21 => ((31 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 22 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 23 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 24 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_1to1_j)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_1to1_j) (0))) (F_max0_ge_0 (s V_synth_1to1_j))]
     ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 26 => ((15 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 27 => ((15 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 28 => ((15 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 29 => ((15 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 30 => ((15 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 31 => ((15 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 32 => ((15 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 33 => ((15 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 34 => ((15 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 35 => ((15 # 1) + max0(s V_synth_1to1_z) <= z)%Q
   | 36 => (s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 37 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_synth_1to1_z)) (F_check_ge (s V_synth_1to1_z) (0))]
     (s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 38 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 39 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 40 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 41 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_synth_1to1_j)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_1to1_j) (0))) (F_max0_ge_0 (s V_synth_1to1_j))]
     (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 42 => (s V_synth_1to1_z <= z)%Q
   | 43 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 44 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 45 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 46 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 47 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 48 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 49 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 50 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 51 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 52 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 53 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 54 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 55 => ((1 # 1) + s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 56 => ((1 # 1) + s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 57 => ((1 # 1) + s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 58 => (s V_synth_1to1_j + s V_synth_1to1_z <= z)%Q
   | 59 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 60 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 61 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 62 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 63 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 64 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 65 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 66 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 67 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 68 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 69 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 70 => ((15 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 71 => ((16 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 72 => ((16 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 73 => ((16 # 1) + s V_synth_1to1_j + max0(s V_synth_1to1_z) <= z)%Q
   | 74 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_synth_1to1_z) (0))) (F_max0_ge_0 (s V_synth_1to1_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_synth_1to1_z)) (F_check_ge (-1
                                                                    + s V_synth_1to1_z) (0))]
     ((16 # 1) + s V_synth_1to1_j + max0(-1 + s V_synth_1to1_z) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_synth_1to1 =>
    [mkPA Q (fun n z s => ai_synth_1to1 n s /\ annot0_synth_1to1 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_synth_1to1 (proc_start P_synth_1to1) s1 (proc_end P_synth_1to1) s2 ->
    (s2 V_synth_1to1_z <= (31 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_synth_1to1.
Qed.
