Require Import pasta.Pasta.

Inductive proc: Type :=
  P_suspect_word.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_suspect_word_z := 1%positive.
Notation V_suspect_word__tmp := 2%positive.
Notation V_suspect_word_ch := 3%positive.
Notation V_suspect_word_i := 4%positive.
Notation V_suspect_word_last := 5%positive.
Notation V_suspect_word_seen_lower := 6%positive.
Notation V_suspect_word_seen_upper := 7%positive.
Notation V_suspect_word_seen_vowel := 8%positive.
Notation V_suspect_word_n := 9%positive.
Notation V_suspect_word_s := 10%positive.
Definition Pedges_suspect_word: list (edge proc) :=
  (EA 1 (AAssign V_suspect_word_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_suspect_word__tmp (Some (EVar V_suspect_word_n))) 3)::(EA 3 (AAssign
  V_suspect_word_i (Some (ENum (0)))) 4)::(EA 4 (AAssign
  V_suspect_word_seen_lower (Some (ENum (0)))) 5)::(EA 5 (AAssign
  V_suspect_word_seen_upper (Some (ENum (0)))) 6)::(EA 6 (AAssign
  V_suspect_word_seen_vowel (Some (ENum (0)))) 7)::(EA 7 (AAssign
  V_suspect_word_last (Some (ENum (0)))) 8)::(EA 8 (AAssign V_suspect_word_i
  (Some (ENum (0)))) 9)::(EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_suspect_word_i) s) <
  (eval (EVar V_suspect_word__tmp) s))%Z)) 25)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_suspect_word_i) s) >=
  (eval (EVar V_suspect_word__tmp) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 13 (AGuard (fun s => ((eval (EVar V_suspect_word_seen_vowel) s) <>
  (eval (ENum (0)) s))%Z)) 15)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_suspect_word_seen_vowel) s) = (eval (ENum (0))
  s))%Z)) 14)::(EA 14 AWeaken 24)::(EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_suspect_word_seen_upper) s) <> (eval (ENum (0))
  s))%Z)) 18)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_suspect_word_seen_upper) s) = (eval (ENum (0))
  s))%Z)) 17)::(EA 17 AWeaken 21)::(EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_suspect_word_seen_lower) s) <> (eval (ENum (0))
  s))%Z)) 23)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_suspect_word_seen_lower) s) = (eval (ENum (0))
  s))%Z)) 20)::(EA 20 AWeaken 21)::(EA 21 ANone 22)::(EA 22 AWeaken 24)::
  (EA 23 AWeaken 24)::(EA 25 AWeaken 26)::(EA 26 (AAssign V_suspect_word_ch
  None) 27)::(EA 27 AWeaken 28)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_suspect_word_i) s) <> (eval (ENum (0))
  s))%Z)) 30)::(EA 28 (AGuard (fun s => ((eval (EVar V_suspect_word_i) s) =
  (eval (ENum (0)) s))%Z)) 29)::(EA 29 AWeaken 39)::(EA 30 AWeaken 31)::
  (EA 31 (AGuard (fun s => ((eval (EVar V_suspect_word_last) s) <>
  (eval (ENum (45)) s))%Z)) 33)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_suspect_word_last) s) = (eval (ENum (45))
  s))%Z)) 32)::(EA 32 AWeaken 39)::(EA 33 AWeaken 34)::(EA 34 ANone 36)::
  (EA 34 ANone 35)::(EA 35 AWeaken 39)::(EA 36 (AAssign
  V_suspect_word_seen_upper (Some (ENum (1)))) 37)::(EA 37 ANone 38)::
  (EA 38 AWeaken 39)::(EA 39 ANone 41)::(EA 39 ANone 40)::
  (EA 40 AWeaken 45)::(EA 41 (AAssign V_suspect_word_seen_lower
  (Some (ENum (1)))) 42)::(EA 42 (AAssign V_suspect_word_ch None) 43)::
  (EA 43 ANone 44)::(EA 44 AWeaken 45)::(EA 45 (AGuard
  (fun s => ((eval (EVar V_suspect_word_ch) s) = (eval (ENum (65))
  s))%Z)) 62)::(EA 45 (AGuard (fun s => ((eval (EVar V_suspect_word_ch) s) <>
  (eval (ENum (65)) s))%Z)) 46)::(EA 46 AWeaken 47)::(EA 47 (AGuard
  (fun s => ((eval (EVar V_suspect_word_ch) s) = (eval (ENum (69))
  s))%Z)) 61)::(EA 47 (AGuard (fun s => ((eval (EVar V_suspect_word_ch) s) <>
  (eval (ENum (69)) s))%Z)) 48)::(EA 48 AWeaken 49)::(EA 49 (AGuard
  (fun s => ((eval (EVar V_suspect_word_ch) s) = (eval (ENum (73))
  s))%Z)) 60)::(EA 49 (AGuard (fun s => ((eval (EVar V_suspect_word_ch) s) <>
  (eval (ENum (73)) s))%Z)) 50)::(EA 50 AWeaken 51)::(EA 51 (AGuard
  (fun s => ((eval (EVar V_suspect_word_ch) s) = (eval (ENum (79))
  s))%Z)) 59)::(EA 51 (AGuard (fun s => ((eval (EVar V_suspect_word_ch) s) <>
  (eval (ENum (79)) s))%Z)) 52)::(EA 52 AWeaken 53)::(EA 53 (AGuard
  (fun s => ((eval (EVar V_suspect_word_ch) s) = (eval (ENum (85))
  s))%Z)) 58)::(EA 53 (AGuard (fun s => ((eval (EVar V_suspect_word_ch) s) <>
  (eval (ENum (85)) s))%Z)) 54)::(EA 54 AWeaken 55)::(EA 55 (AGuard
  (fun s => ((eval (EVar V_suspect_word_ch) s) = (eval (ENum (89))
  s))%Z)) 57)::(EA 55 (AGuard (fun s => ((eval (EVar V_suspect_word_ch) s) <>
  (eval (ENum (89)) s))%Z)) 56)::(EA 56 AWeaken 65)::(EA 57 AWeaken 63)::
  (EA 58 AWeaken 63)::(EA 59 AWeaken 63)::(EA 60 AWeaken 63)::
  (EA 61 AWeaken 63)::(EA 62 AWeaken 63)::(EA 63 (AAssign
  V_suspect_word_seen_vowel (Some (ENum (1)))) 64)::(EA 64 ANone 65)::
  (EA 65 (AAssign V_suspect_word_last (Some (EVar V_suspect_word_ch))) 66)::
  (EA 66 ANone 67)::(EA 67 (AAssign V_suspect_word_i
  (Some (EAdd (EVar V_suspect_word_i) (ENum (1))))) 68)::(EA 68 ANone 69)::
  (EA 69 ANone 70)::(EA 70 (AAssign V_suspect_word_z (Some (EAdd (ENum (1))
  (EVar V_suspect_word_z)))) 71)::(EA 71 AWeaken 11)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_suspect_word => Pedges_suspect_word
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_suspect_word => 24
     end)%positive;
  var_global := var_global
}.

Definition ai_suspect_word (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_z <= 0)%Z
   | 3 => (-1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word_z <= 0)%Z
   | 4 => (1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 5 => (-1 * s V_suspect_word_i <= 0 /\ 1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0)%Z
   | 6 => (-1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ 1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0)%Z
   | 7 => (-1 * s V_suspect_word_seen_upper <= 0 /\ 1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ 1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0)%Z
   | 8 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ 1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ 1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ 1 * s V_suspect_word_last <= 0 /\ -1 * s V_suspect_word_last <= 0)%Z
   | 9 => (-1 * s V_suspect_word_last <= 0 /\ 1 * s V_suspect_word_last <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ 1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ 1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 10 => (-1 * s V_suspect_word_i <= 0 /\ 1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ 1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ 1 * s V_suspect_word_last <= 0 /\ -1 * s V_suspect_word_last <= 0)%Z
   | 11 => (-1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0)%Z
   | 12 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0)%Z
   | 13 => (1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0)%Z
   | 14 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0 /\ 1 * s V_suspect_word_seen_vowel <= 0)%Z
   | 15 => (-1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_vowel + 1 <= 0)%Z
   | 16 => (-1 * s V_suspect_word_seen_vowel + 1 <= 0 /\ 1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0)%Z
   | 17 => (-1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_vowel + 1 <= 0 /\ 1 * s V_suspect_word_seen_upper <= 0)%Z
   | 18 => (-1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_vowel + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper + 1 <= 0)%Z
   | 19 => (-1 * s V_suspect_word_seen_upper + 1 <= 0 /\ -1 * s V_suspect_word_seen_vowel + 1 <= 0 /\ 1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0)%Z
   | 20 => (-1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_vowel + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper + 1 <= 0 /\ 1 * s V_suspect_word_seen_lower <= 0)%Z
   | 21 => (-1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_seen_vowel + 1 <= 0 /\ 1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0)%Z
   | 22 => (-1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_vowel + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0)%Z
   | 23 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ 1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_vowel + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper + 1 <= 0 /\ -1 * s V_suspect_word_seen_lower + 1 <= 0)%Z
   | 24 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word__tmp+ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 25 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0)%Z
   | 26 => (-1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0)%Z
   | 27 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0)%Z
   | 28 => (-1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0)%Z
   | 29 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ 1 * s V_suspect_word_i <= 0)%Z
   | 30 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_i + 1 <= 0)%Z
   | 31 => (-1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0)%Z
   | 32 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_i + 1 <= 0 /\ 1 * s V_suspect_word_last + -45 <= 0 /\ -1 * s V_suspect_word_last + 45 <= 0)%Z
   | 33 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_i + 1 <= 0)%Z
   | 34 => (-1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0)%Z
   | 35 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_i + 1 <= 0)%Z
   | 36 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_i + 1 <= 0)%Z
   | 37 => (-1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ 1 * s V_suspect_word_seen_upper + -1 <= 0 /\ -1 * s V_suspect_word_seen_upper + 1 <= 0)%Z
   | 38 => (-1 * s V_suspect_word_seen_upper + 1 <= 0 /\ 1 * s V_suspect_word_seen_upper + -1 <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_i + 1 <= 0)%Z
   | 39 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0)%Z
   | 40 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 41 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 42 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ 1 * s V_suspect_word_seen_lower + -1 <= 0 /\ -1 * s V_suspect_word_seen_lower + 1 <= 0)%Z
   | 43 => (-1 * s V_suspect_word_seen_lower + 1 <= 0 /\ 1 * s V_suspect_word_seen_lower + -1 <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 44 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ 1 * s V_suspect_word_seen_lower + -1 <= 0 /\ -1 * s V_suspect_word_seen_lower + 1 <= 0)%Z
   | 45 => (-1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 46 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0)%Z
   | 47 => (-1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 48 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0)%Z
   | 49 => (-1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 50 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0)%Z
   | 51 => (-1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 52 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0)%Z
   | 53 => (-1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 54 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0)%Z
   | 55 => (-1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 56 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0)%Z
   | 57 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_ch + -89 <= 0 /\ -1 * s V_suspect_word_ch + 89 <= 0)%Z
   | 58 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_ch + -85 <= 0 /\ -1 * s V_suspect_word_ch + 85 <= 0)%Z
   | 59 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_ch + -79 <= 0 /\ -1 * s V_suspect_word_ch + 79 <= 0)%Z
   | 60 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_ch + -73 <= 0 /\ -1 * s V_suspect_word_ch + 73 <= 0)%Z
   | 61 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_ch + -69 <= 0 /\ -1 * s V_suspect_word_ch + 69 <= 0)%Z
   | 62 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ 1 * s V_suspect_word_ch + -65 <= 0 /\ -1 * s V_suspect_word_ch + 65 <= 0)%Z
   | 63 => (1 * s V_suspect_word_ch + -89 <= 0 /\ -1 * s V_suspect_word_ch + 65 <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 64 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_ch + 65 <= 0 /\ 1 * s V_suspect_word_ch + -89 <= 0 /\ 1 * s V_suspect_word_seen_vowel + -1 <= 0 /\ -1 * s V_suspect_word_seen_vowel + 1 <= 0)%Z
   | 65 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 66 => (-1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0)%Z
   | 67 => (-1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_i <= 0)%Z
   | 68 => (-1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_i + 1 <= 0)%Z
   | 69 => (-1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0)%Z
   | 70 => (-1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_z <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_i + 1 <= 0)%Z
   | 71 => (-1 * s V_suspect_word_i + 1 <= 0 /\ -1 * s V_suspect_word__tmp+ 1 * s V_suspect_word_i <= 0 /\ -1 * s V_suspect_word_seen_vowel <= 0 /\ -1 * s V_suspect_word_seen_lower <= 0 /\ -1 * s V_suspect_word_seen_upper <= 0 /\ -1 * s V_suspect_word_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_suspect_word (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_suspect_word_n) <= z)%Q
   | 2 => (s V_suspect_word_z + max0(s V_suspect_word_n) <= z)%Q
   | 3 => (s V_suspect_word_z + max0(s V_suspect_word__tmp) <= z)%Q
   | 4 => (s V_suspect_word_z + max0(s V_suspect_word__tmp) <= z)%Q
   | 5 => (s V_suspect_word_z + max0(s V_suspect_word__tmp) <= z)%Q
   | 6 => (s V_suspect_word_z + max0(s V_suspect_word__tmp) <= z)%Q
   | 7 => (s V_suspect_word_z + max0(s V_suspect_word__tmp) <= z)%Q
   | 8 => (s V_suspect_word_z + max0(s V_suspect_word__tmp) <= z)%Q
   | 9 => (s V_suspect_word_z
           + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 10 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 11 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 12 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 13 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_suspect_word__tmp
                                             - s V_suspect_word_i) (-1
                                                                    + 
                                                                    s V_suspect_word__tmp
                                                                    - 
                                                                    s V_suspect_word_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_suspect_word__tmp - s V_suspect_word_i)]
     (s V_suspect_word_z + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 15 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 16 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 17 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_suspect_word__tmp
                                            - s V_suspect_word_i) (-1
                                                                   + 
                                                                   s V_suspect_word__tmp
                                                                   - 
                                                                   s V_suspect_word_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_suspect_word__tmp
                                                 - s V_suspect_word_i)) (F_check_ge (0) (0))]
     (s V_suspect_word_z + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 18 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 19 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_suspect_word__tmp
                                             - s V_suspect_word_i) (-1
                                                                    + 
                                                                    s V_suspect_word__tmp
                                                                    - 
                                                                    s V_suspect_word_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_suspect_word__tmp - s V_suspect_word_i)]
     (s V_suspect_word_z + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 21 => (s V_suspect_word_z <= z)%Q
   | 22 => (s V_suspect_word_z <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_suspect_word__tmp
                                             - s V_suspect_word_i) (-1
                                                                    + 
                                                                    s V_suspect_word__tmp
                                                                    - 
                                                                    s V_suspect_word_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_suspect_word__tmp - s V_suspect_word_i)]
     (s V_suspect_word_z + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 24 => (s V_suspect_word_z <= z)%Q
   | 25 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 26 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 27 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 28 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 29 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_suspect_word__tmp
                                       - s V_suspect_word_i) (1)]
     (s V_suspect_word_z + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 30 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 31 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_suspect_word__tmp
                                       - s V_suspect_word_i) (1)]
     (s V_suspect_word_z + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 33 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 34 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_suspect_word__tmp
                                       - s V_suspect_word_i) (1)]
     (s V_suspect_word_z + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 36 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 37 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_suspect_word__tmp
                                       - s V_suspect_word_i) (1)]
     (s V_suspect_word_z + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 39 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 40 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 41 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 42 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 43 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 44 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 45 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 46 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 47 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 48 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 49 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 50 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 51 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 52 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 53 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 54 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 55 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 56 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 57 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 58 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 59 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 60 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 61 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 62 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 63 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 64 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 65 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 66 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 67 => ((1 # 1) + s V_suspect_word_z
            + max0(-1 + s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 68 => ((1 # 1) + s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 69 => ((1 # 1) + s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 70 => ((1 # 1) + s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | 71 => (s V_suspect_word_z
            + max0(s V_suspect_word__tmp - s V_suspect_word_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_suspect_word =>
    [mkPA Q (fun n z s => ai_suspect_word n s /\ annot0_suspect_word n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_suspect_word (proc_start P_suspect_word) s1 (proc_end P_suspect_word) s2 ->
    (s2 V_suspect_word_z <= max0(s1 V_suspect_word_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_suspect_word.
Qed.
