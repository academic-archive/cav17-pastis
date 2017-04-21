Require Import pasta.Pasta.

Inductive proc: Type :=
  P_id3_tag_findframe.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_id3_tag_findframe_z := 1%positive.
Notation V_id3_tag_findframe__tmp := 2%positive.
Notation V_id3_tag_findframe_i := 3%positive.
Notation V_id3_tag_findframe_len := 4%positive.
Notation V_id3_tag_findframe_tag_dref_off24 := 5%positive.
Notation V_id3_tag_findframe_id := 6%positive.
Notation V_id3_tag_findframe_index := 7%positive.
Notation V_id3_tag_findframe_tag := 8%positive.
Definition Pedges_id3_tag_findframe: list (edge proc) :=
  (EA 1 (AAssign V_id3_tag_findframe_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_id3_tag_findframe_tag_dref_off24) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_id3_tag_findframe_i) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 (AGuard (fun s => ((eval (EVar V_id3_tag_findframe__tmp)
  s) >= (eval (ENum (0)) s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 (AAssign
  V_id3_tag_findframe__tmp (Some (EVar V_id3_tag_findframe_index))) 7)::
  (EA 7 AWeaken 8)::(EA 8 ANone 47)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::
  (EA 10 ANone 46)::(EA 10 ANone 11)::(EA 11 (AAssign V_id3_tag_findframe_len
  None) 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_id3_tag_findframe_len) s) = (eval (ENum (4))
  s))%Z)) 15)::(EA 13 (AGuard (fun s => ((eval (EVar V_id3_tag_findframe_len)
  s) <> (eval (ENum (4)) s))%Z)) 14)::(EA 14 AWeaken 24)::
  (EA 15 AWeaken 16)::(EA 16 ANone 17)::(EA 16 ANone 23)::
  (EA 17 AWeaken 18)::(EA 18 ANone 19)::(EA 18 ANone 23)::
  (EA 19 AWeaken 20)::(EA 20 ANone 23)::(EA 20 ANone 21)::(EA 21 (AAssign
  V_id3_tag_findframe_len None) 22)::(EA 22 ANone 23)::(EA 23 ANone 24)::
  (EA 24 (AAssign V_id3_tag_findframe_i (Some (ENum (0)))) 25)::
  (EA 25 ANone 26)::(EA 26 AWeaken 27)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_id3_tag_findframe_i) s) <
  (eval (EVar V_id3_tag_findframe_tag_dref_off24) s))%Z)) 31)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_id3_tag_findframe_i) s) >=
  (eval (EVar V_id3_tag_findframe_tag_dref_off24) s))%Z)) 28)::
  (EA 28 AWeaken 29)::(EA 29 ANone 30)::(EA 30 AWeaken 55)::
  (EA 31 AWeaken 32)::(EA 32 ANone 33)::(EA 32 ANone 37)::(EA 33 (AAssign
  V_id3_tag_findframe__tmp (Some (EAdd (EVar V_id3_tag_findframe__tmp)
  (ENum (-1))))) 34)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_id3_tag_findframe__tmp) s) = (eval (ENum (0))
  s))%Z)) 43)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_id3_tag_findframe__tmp) s) <> (eval (ENum (0))
  s))%Z)) 36)::(EA 36 AWeaken 37)::(EA 37 ANone 38)::(EA 38 (AAssign
  V_id3_tag_findframe_i (Some (EAdd (EVar V_id3_tag_findframe_i)
  (ENum (1))))) 39)::(EA 39 ANone 40)::(EA 40 ANone 41)::(EA 41 (AAssign
  V_id3_tag_findframe_z (Some (EAdd (ENum (1))
  (EVar V_id3_tag_findframe_z)))) 42)::(EA 42 AWeaken 27)::
  (EA 43 AWeaken 44)::(EA 44 ANone 45)::(EA 45 AWeaken 55)::
  (EA 46 AWeaken 48)::(EA 47 AWeaken 48)::(EA 48 (AGuard
  (fun s => ((eval (EVar V_id3_tag_findframe__tmp) s) <
  (eval (EVar V_id3_tag_findframe_tag_dref_off24) s))%Z)) 51)::(EA 48 (AGuard
  (fun s => ((eval (EVar V_id3_tag_findframe__tmp) s) >=
  (eval (EVar V_id3_tag_findframe_tag_dref_off24) s))%Z)) 49)::
  (EA 49 AWeaken 50)::(EA 50 ANone 53)::(EA 51 AWeaken 52)::
  (EA 52 ANone 53)::(EA 53 ANone 54)::(EA 54 AWeaken 55)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_id3_tag_findframe => Pedges_id3_tag_findframe
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_id3_tag_findframe => 55
     end)%positive;
  var_global := var_global
}.

Definition ai_id3_tag_findframe (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0)%Z
   | 3 => (-1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 4 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 5 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_findframe__tmp <= 0)%Z
   | 6 => (-1 * s V_id3_tag_findframe__tmp <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 7 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 8 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 9 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 10 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 11 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 12 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 13 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 14 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 15 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ 1 * s V_id3_tag_findframe_len + -4 <= 0 /\ -1 * s V_id3_tag_findframe_len + 4 <= 0)%Z
   | 16 => (-1 * s V_id3_tag_findframe_len + 4 <= 0 /\ 1 * s V_id3_tag_findframe_len + -4 <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 17 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ 1 * s V_id3_tag_findframe_len + -4 <= 0 /\ -1 * s V_id3_tag_findframe_len + 4 <= 0)%Z
   | 18 => (-1 * s V_id3_tag_findframe_len + 4 <= 0 /\ 1 * s V_id3_tag_findframe_len + -4 <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 19 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ 1 * s V_id3_tag_findframe_len + -4 <= 0 /\ -1 * s V_id3_tag_findframe_len + 4 <= 0)%Z
   | 20 => (-1 * s V_id3_tag_findframe_len + 4 <= 0 /\ 1 * s V_id3_tag_findframe_len + -4 <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 21 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ 1 * s V_id3_tag_findframe_len + -4 <= 0 /\ -1 * s V_id3_tag_findframe_len + 4 <= 0)%Z
   | 22 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 23 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 24 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 25 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 26 => (-1 * s V_id3_tag_findframe_i <= 0 /\ 1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 27 => (-1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ 1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 28 => (1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i+ 1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 29 => (-1 * s V_id3_tag_findframe_i+ 1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ 1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 30 => (1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i+ 1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 31 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0)%Z
   | 32 => (1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 33 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0)%Z
   | 34 => (1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 35 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0)%Z
   | 36 => (1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 37 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0)%Z
   | 38 => (1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 39 => (-1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_findframe_i + 1 <= 0)%Z
   | 40 => (-1 * s V_id3_tag_findframe_i + 1 <= 0 /\ 1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0)%Z
   | 41 => (-1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_findframe_i + 1 <= 0)%Z
   | 42 => (-1 * s V_id3_tag_findframe_i + 1 <= 0 /\ 1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_findframe_z + 1 <= 0)%Z
   | 43 => (1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ 1 * s V_id3_tag_findframe__tmp <= 0 /\ -1 * s V_id3_tag_findframe__tmp <= 0)%Z
   | 44 => (-1 * s V_id3_tag_findframe__tmp <= 0 /\ 1 * s V_id3_tag_findframe__tmp <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0)%Z
   | 45 => (1 * s V_id3_tag_findframe_i+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0 /\ 1 * s V_id3_tag_findframe__tmp <= 0 /\ -1 * s V_id3_tag_findframe__tmp <= 0)%Z
   | 46 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 47 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 48 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 49 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_findframe__tmp+ 1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 50 => (-1 * s V_id3_tag_findframe__tmp+ 1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 51 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe__tmp+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0)%Z
   | 52 => (1 * s V_id3_tag_findframe__tmp+ -1 * s V_id3_tag_findframe_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 53 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | 54 => (-1 * s V_id3_tag_findframe_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_i <= 0)%Z
   | 55 => (-1 * s V_id3_tag_findframe_i <= 0 /\ -1 * s V_id3_tag_findframe_z <= 0 /\ -1 * s V_id3_tag_findframe_tag_dref_off24 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_id3_tag_findframe (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 2 => (s V_id3_tag_findframe_z
           + max0(s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 3 => (s V_id3_tag_findframe_z
           + max0(s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 4 => (s V_id3_tag_findframe_z
           + max0(s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 5 => (s V_id3_tag_findframe_z
           + max0(s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 6 => (s V_id3_tag_findframe_z
           + max0(s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 7 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_id3_tag_findframe_tag_dref_off24)) (F_check_ge (s V_id3_tag_findframe_tag_dref_off24) (0))]
     (s V_id3_tag_findframe_z + max0(s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 8 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 9 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 10 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 11 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 12 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 13 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 14 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 15 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 16 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 17 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 18 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 19 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 20 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 21 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 22 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 23 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 24 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 25 => (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z
            + max0(-s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24)
            - max0(s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_id3_tag_findframe_z) (0))) (F_max0_ge_0 (s V_id3_tag_findframe_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_id3_tag_findframe_tag_dref_off24) (0))) (F_max0_ge_0 (s V_id3_tag_findframe_tag_dref_off24))]
     (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z
      + max0(-s V_id3_tag_findframe_i + s V_id3_tag_findframe_tag_dref_off24)
      - max0(s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 27 => (max0(-s V_id3_tag_findframe_i
                 + s V_id3_tag_findframe_tag_dref_off24)
            + max0(s V_id3_tag_findframe_z) <= z)%Q
   | 28 => (max0(-s V_id3_tag_findframe_i
                 + s V_id3_tag_findframe_tag_dref_off24)
            + max0(s V_id3_tag_findframe_z) <= z)%Q
   | 29 => (max0(-s V_id3_tag_findframe_i
                 + s V_id3_tag_findframe_tag_dref_off24)
            + max0(s V_id3_tag_findframe_z) <= z)%Q
   | 30 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_id3_tag_findframe_i
                                             + s V_id3_tag_findframe_tag_dref_off24) (-1
                                                                    - s V_id3_tag_findframe_i
                                                                    + s V_id3_tag_findframe_tag_dref_off24));
      (*-1 0*) F_max0_ge_0 (-1 - s V_id3_tag_findframe_i
                            + s V_id3_tag_findframe_tag_dref_off24);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_id3_tag_findframe_z)) (F_check_ge (s V_id3_tag_findframe_z) (0))]
     (max0(-s V_id3_tag_findframe_i + s V_id3_tag_findframe_tag_dref_off24)
      + max0(s V_id3_tag_findframe_z) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_id3_tag_findframe_i
                                       + s V_id3_tag_findframe_tag_dref_off24) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_id3_tag_findframe_z)) (F_check_ge (s V_id3_tag_findframe_z) (0))]
     (max0(-s V_id3_tag_findframe_i + s V_id3_tag_findframe_tag_dref_off24)
      + max0(s V_id3_tag_findframe_z) <= z)%Q
   | 32 => ((1 # 1) + s V_id3_tag_findframe_z
            + max0(-1 - s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 33 => ((1 # 1) + s V_id3_tag_findframe_z
            + max0(-1 - s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 34 => ((1 # 1) + s V_id3_tag_findframe_z
            + max0(-1 - s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 35 => ((1 # 1) + s V_id3_tag_findframe_z
            + max0(-1 - s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 36 => ((1 # 1) + s V_id3_tag_findframe_z
            + max0(-1 - s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 37 => ((1 # 1) + s V_id3_tag_findframe_z
            + max0(-1 - s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 38 => ((1 # 1) + s V_id3_tag_findframe_z
            + max0(-1 - s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 39 => ((1 # 1) + s V_id3_tag_findframe_z
            + max0(-s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 40 => ((1 # 1) + s V_id3_tag_findframe_z
            + max0(-s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 41 => ((1 # 1) + s V_id3_tag_findframe_z
            + max0(-s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 42 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_id3_tag_findframe_z) (0))) (F_max0_ge_0 (s V_id3_tag_findframe_z))]
     (s V_id3_tag_findframe_z
      + max0(-s V_id3_tag_findframe_i + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 43 => ((1 # 1) + s V_id3_tag_findframe_z
            + max0(-1 - s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 44 => ((1 # 1) + s V_id3_tag_findframe_z
            + max0(-1 - s V_id3_tag_findframe_i
                   + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 45 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 - s V_id3_tag_findframe_i
                            + s V_id3_tag_findframe_tag_dref_off24)]
     ((1 # 1) + s V_id3_tag_findframe_z
      + max0(-1 - s V_id3_tag_findframe_i
             + s V_id3_tag_findframe_tag_dref_off24) <= z)%Q
   | 46 => hints
     [(*-1 0*) F_max0_ge_0 (s V_id3_tag_findframe_tag_dref_off24);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_id3_tag_findframe_tag_dref_off24) (0))) (F_max0_ge_0 (s V_id3_tag_findframe_tag_dref_off24))]
     (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 47 => hints
     [(*-1 0*) F_max0_ge_0 (s V_id3_tag_findframe_tag_dref_off24);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_id3_tag_findframe_tag_dref_off24) (0))) (F_max0_ge_0 (s V_id3_tag_findframe_tag_dref_off24))]
     (s V_id3_tag_findframe_tag_dref_off24 + s V_id3_tag_findframe_z <= z)%Q
   | 48 => (s V_id3_tag_findframe_z <= z)%Q
   | 49 => (s V_id3_tag_findframe_z <= z)%Q
   | 50 => (s V_id3_tag_findframe_z <= z)%Q
   | 51 => (s V_id3_tag_findframe_z <= z)%Q
   | 52 => (s V_id3_tag_findframe_z <= z)%Q
   | 53 => (s V_id3_tag_findframe_z <= z)%Q
   | 54 => (s V_id3_tag_findframe_z <= z)%Q
   | 55 => (s V_id3_tag_findframe_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_id3_tag_findframe =>
    [mkPA Q (fun n z s => ai_id3_tag_findframe n s /\ annot0_id3_tag_findframe n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_id3_tag_findframe (proc_start P_id3_tag_findframe) s1 (proc_end P_id3_tag_findframe) s2 ->
    (s2 V_id3_tag_findframe_z <= max0(s1 V_id3_tag_findframe_tag_dref_off24))%Q.
Proof.
  prove_bound ipa admissible_ipa P_id3_tag_findframe.
Qed.
