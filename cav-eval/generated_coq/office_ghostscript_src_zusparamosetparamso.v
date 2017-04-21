Require Import pasta.Pasta.

Inductive proc: Type :=
  P_setparams.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_setparams_z := 1%positive.
Notation V_setparams__tmp := 2%positive.
Notation V_setparams_code := 3%positive.
Notation V_setparams_i := 4%positive.
Notation V_setparams_pset_dref_off24 := 5%positive.
Notation V_setparams_pset_dref_off8 := 6%positive.
Notation V_setparams_plist := 7%positive.
Notation V_setparams_pset := 8%positive.
Definition Pedges_setparams: list (edge proc) :=
  (EA 1 (AAssign V_setparams_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_setparams_pset_dref_off8) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_setparams_pset_dref_off24) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 (AGuard (fun s => ((eval (EVar V_setparams_i) s) >=
  (eval (ENum (0)) s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 (AAssign V_setparams_i
  (Some (ENum (0)))) 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_setparams_i) s) <
  (eval (EVar V_setparams_pset_dref_off8) s))%Z)) 42)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_setparams_i) s) >=
  (eval (EVar V_setparams_pset_dref_off8) s))%Z)) 10)::(EA 10 AWeaken 11)::
  (EA 11 (AAssign V_setparams_i (Some (ENum (0)))) 12)::(EA 12 ANone 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard (fun s => ((eval (EVar V_setparams_i)
  s) < (eval (EVar V_setparams_pset_dref_off24) s))%Z)) 19)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_setparams_i) s) >=
  (eval (EVar V_setparams_pset_dref_off24) s))%Z)) 15)::(EA 15 AWeaken 16)::
  (EA 16 (AAssign V_setparams__tmp (Some (ENum (0)))) 17)::(EA 17 ANone 18)::
  (EA 18 AWeaken 68)::(EA 19 AWeaken 20)::(EA 20 ANone 36)::
  (EA 20 ANone 21)::(EA 21 (AAssign V_setparams_code None) 22)::
  (EA 22 AWeaken 23)::(EA 23 (AGuard (fun s => ((eval (EVar V_setparams_code)
  s) = (eval (ENum (0)) s))%Z)) 25)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_setparams_code) s) <> (eval (ENum (0))
  s))%Z)) 24)::(EA 24 AWeaken 29)::(EA 25 AWeaken 26)::(EA 26 (AAssign
  V_setparams_code None) 27)::(EA 27 ANone 28)::(EA 28 AWeaken 29)::
  (EA 29 (AGuard (fun s => ((eval (EVar V_setparams_code) s) <
  (eval (ENum (0)) s))%Z)) 32)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_setparams_code) s) >= (eval (ENum (0))
  s))%Z)) 30)::(EA 30 AWeaken 31)::(EA 31 ANone 37)::(EA 32 AWeaken 33)::
  (EA 33 (AAssign V_setparams__tmp (Some (EVar V_setparams_code))) 34)::
  (EA 34 ANone 35)::(EA 35 AWeaken 68)::(EA 36 ANone 37)::(EA 37 (AAssign
  V_setparams_i (Some (EAdd (EVar V_setparams_i) (ENum (1))))) 38)::
  (EA 38 ANone 39)::(EA 39 ANone 40)::(EA 40 (AAssign V_setparams_z
  (Some (EAdd (ENum (1)) (EVar V_setparams_z)))) 41)::(EA 41 AWeaken 14)::
  (EA 42 AWeaken 43)::(EA 43 ANone 69)::(EA 43 ANone 44)::(EA 44 (AAssign
  V_setparams_code None) 45)::(EA 45 AWeaken 46)::(EA 46 ANone 65)::
  (EA 46 ANone 63)::(EA 46 ANone 47)::(EA 47 AWeaken 48)::(EA 48 ANone 60)::
  (EA 48 ANone 49)::(EA 49 AWeaken 50)::(EA 50 ANone 60)::(EA 50 ANone 51)::
  (EA 51 (AAssign V_setparams_code None) 52)::(EA 52 AWeaken 53)::
  (EA 53 (AGuard (fun s => ((eval (EVar V_setparams_code) s) <
  (eval (ENum (0)) s))%Z)) 56)::(EA 53 (AGuard
  (fun s => ((eval (EVar V_setparams_code) s) >= (eval (ENum (0))
  s))%Z)) 54)::(EA 54 AWeaken 55)::(EA 55 ANone 64)::(EA 56 AWeaken 57)::
  (EA 57 (AAssign V_setparams__tmp (Some (EVar V_setparams_code))) 58)::
  (EA 58 ANone 59)::(EA 59 AWeaken 68)::(EA 60 (AAssign V_setparams__tmp
  (Some (ENum (-15)))) 61)::(EA 61 ANone 62)::(EA 62 AWeaken 68)::
  (EA 63 ANone 64)::(EA 64 ANone 70)::(EA 65 (AAssign V_setparams__tmp
  (Some (EVar V_setparams_code))) 66)::(EA 66 ANone 67)::(EA 67 AWeaken 68)::
  (EA 69 ANone 70)::(EA 70 (AAssign V_setparams_i
  (Some (EAdd (EVar V_setparams_i) (ENum (1))))) 71)::(EA 71 ANone 72)::
  (EA 72 ANone 73)::(EA 73 (AAssign V_setparams_z (Some (EAdd (ENum (1))
  (EVar V_setparams_z)))) 74)::(EA 74 AWeaken 9)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_setparams => Pedges_setparams
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_setparams => 68
     end)%positive;
  var_global := var_global
}.

Definition ai_setparams (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_z <= 0)%Z
   | 3 => (-1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_pset_dref_off8 <= 0)%Z
   | 4 => (-1 * s V_setparams_pset_dref_off8 <= 0 /\ 1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 5 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_pset_dref_off8 <= 0 /\ -1 * s V_setparams_i <= 0)%Z
   | 6 => (-1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off8 <= 0 /\ 1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 7 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_pset_dref_off8 <= 0 /\ 1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_i <= 0)%Z
   | 8 => (-1 * s V_setparams_i <= 0 /\ 1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off8 <= 0 /\ 1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 9 => (-1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 <= 0)%Z
   | 10 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i+ 1 * s V_setparams_pset_dref_off8 <= 0)%Z
   | 11 => (-1 * s V_setparams_i+ 1 * s V_setparams_pset_dref_off8 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 <= 0)%Z
   | 12 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_i <= 0)%Z
   | 13 => (-1 * s V_setparams_i <= 0 /\ 1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 14 => (-1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 15 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i+ 1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 16 => (-1 * s V_setparams_i+ 1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 17 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i+ 1 * s V_setparams_pset_dref_off24 <= 0 /\ 1 * s V_setparams__tmp <= 0 /\ -1 * s V_setparams__tmp <= 0)%Z
   | 18 => (-1 * s V_setparams__tmp <= 0 /\ 1 * s V_setparams__tmp <= 0 /\ -1 * s V_setparams_i+ 1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 19 => (-1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0)%Z
   | 20 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0)%Z
   | 21 => (-1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0)%Z
   | 22 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0)%Z
   | 23 => (-1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0)%Z
   | 24 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0)%Z
   | 25 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ 1 * s V_setparams_code <= 0 /\ -1 * s V_setparams_code <= 0)%Z
   | 26 => (-1 * s V_setparams_code <= 0 /\ 1 * s V_setparams_code <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0)%Z
   | 27 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0)%Z
   | 28 => (-1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0)%Z
   | 29 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0)%Z
   | 30 => (-1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ -1 * s V_setparams_code <= 0)%Z
   | 31 => (-1 * s V_setparams_code <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0)%Z
   | 32 => (-1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ 1 * s V_setparams_code + 1 <= 0)%Z
   | 33 => (1 * s V_setparams_code + 1 <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0)%Z
   | 34 => (-1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ 1 * s V_setparams_code + 1 <= 0 /\ 1 * s V_setparams__tmp + 1 <= 0)%Z
   | 35 => (1 * s V_setparams__tmp + 1 <= 0 /\ 1 * s V_setparams_code + 1 <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0)%Z
   | 36 => (-1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0)%Z
   | 37 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0)%Z
   | 38 => (-1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i + 1 <= 0)%Z
   | 39 => (-1 * s V_setparams_i + 1 <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_z <= 0)%Z
   | 40 => (-1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i + 1 <= 0)%Z
   | 41 => (-1 * s V_setparams_i + 1 <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_z + 1 <= 0)%Z
   | 42 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0)%Z
   | 43 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 44 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0)%Z
   | 45 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 46 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0)%Z
   | 47 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 48 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0)%Z
   | 49 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 50 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0)%Z
   | 51 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 52 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0)%Z
   | 53 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 54 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_code <= 0)%Z
   | 55 => (-1 * s V_setparams_code <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 56 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ 1 * s V_setparams_code + 1 <= 0)%Z
   | 57 => (1 * s V_setparams_code + 1 <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 58 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ 1 * s V_setparams_code + 1 <= 0 /\ 1 * s V_setparams__tmp + 1 <= 0)%Z
   | 59 => (1 * s V_setparams__tmp + 1 <= 0 /\ 1 * s V_setparams_code + 1 <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 60 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 61 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ 1 * s V_setparams__tmp + 15 <= 0 /\ -1 * s V_setparams__tmp + -15 <= 0)%Z
   | 62 => (-1 * s V_setparams__tmp + -15 <= 0 /\ 1 * s V_setparams__tmp + 15 <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 63 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 64 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0)%Z
   | 65 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 66 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0)%Z
   | 67 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 68 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0)%Z
   | 69 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0)%Z
   | 70 => (1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 + 1 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_i <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 71 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 <= 0 /\ -1 * s V_setparams_i + 1 <= 0)%Z
   | 72 => (-1 * s V_setparams_i + 1 <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 <= 0 /\ -1 * s V_setparams_z <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0)%Z
   | 73 => (-1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_z <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 <= 0 /\ -1 * s V_setparams_i + 1 <= 0)%Z
   | 74 => (-1 * s V_setparams_i + 1 <= 0 /\ 1 * s V_setparams_i+ -1 * s V_setparams_pset_dref_off8 <= 0 /\ -1 * s V_setparams_pset_dref_off24 <= 0 /\ -1 * s V_setparams_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_setparams (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_setparams_pset_dref_off24)
           + max0(s V_setparams_pset_dref_off8) <= z)%Q
   | 2 => (s V_setparams_z + max0(s V_setparams_pset_dref_off24)
           + max0(s V_setparams_pset_dref_off8) <= z)%Q
   | 3 => (s V_setparams_z + max0(s V_setparams_pset_dref_off24)
           + max0(s V_setparams_pset_dref_off8) <= z)%Q
   | 4 => (s V_setparams_z + max0(s V_setparams_pset_dref_off24)
           + max0(s V_setparams_pset_dref_off8) <= z)%Q
   | 5 => (s V_setparams_z + max0(s V_setparams_pset_dref_off24)
           + max0(s V_setparams_pset_dref_off8) <= z)%Q
   | 6 => (s V_setparams_z + max0(s V_setparams_pset_dref_off24)
           + max0(s V_setparams_pset_dref_off8) <= z)%Q
   | 7 => (s V_setparams_z
           + max0(-s V_setparams_i + s V_setparams_pset_dref_off8)
           + max0(s V_setparams_pset_dref_off24) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_setparams_pset_dref_off24)) (F_check_ge (s V_setparams_pset_dref_off24) (0))]
     (s V_setparams_z + max0(-s V_setparams_i + s V_setparams_pset_dref_off8)
      + max0(s V_setparams_pset_dref_off24) <= z)%Q
   | 9 => (s V_setparams_pset_dref_off24 + s V_setparams_z
           + max0(-s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_setparams_i
                                             + s V_setparams_pset_dref_off8) (-1
                                                                    - s V_setparams_i
                                                                    + s V_setparams_pset_dref_off8));
      (*-1 0*) F_max0_ge_0 (-1 - s V_setparams_i
                            + s V_setparams_pset_dref_off8)]
     (s V_setparams_pset_dref_off24 + s V_setparams_z
      + max0(-s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 11 => (s V_setparams_pset_dref_off24 + s V_setparams_z <= z)%Q
   | 12 => (s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off24)
            - max0(s V_setparams_pset_dref_off24) <= z)%Q
   | 13 => (s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off24)
            - max0(s V_setparams_pset_dref_off24) <= z)%Q
   | 14 => (s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off24)
            - max0(s V_setparams_pset_dref_off24) <= z)%Q
   | 15 => (s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off24)
            - max0(s V_setparams_pset_dref_off24) <= z)%Q
   | 16 => (s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off24)
            - max0(s V_setparams_pset_dref_off24) <= z)%Q
   | 17 => (s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off24)
            - max0(s V_setparams_pset_dref_off24) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_setparams_i
                                             + s V_setparams_pset_dref_off24) (-1
                                                                    - s V_setparams_i
                                                                    + s V_setparams_pset_dref_off24));
      (*-1 0*) F_max0_ge_0 (-1 - s V_setparams_i
                            + s V_setparams_pset_dref_off24);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_setparams_pset_dref_off24) (0))) (F_max0_ge_0 (s V_setparams_pset_dref_off24))]
     (s V_setparams_pset_dref_off24 + s V_setparams_z
      + max0(-s V_setparams_i + s V_setparams_pset_dref_off24)
      - max0(s V_setparams_pset_dref_off24) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_setparams_i
                                       + s V_setparams_pset_dref_off24) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_setparams_pset_dref_off24) (0))) (F_max0_ge_0 (s V_setparams_pset_dref_off24))]
     (s V_setparams_pset_dref_off24 + s V_setparams_z
      + max0(-s V_setparams_i + s V_setparams_pset_dref_off24)
      - max0(s V_setparams_pset_dref_off24) <= z)%Q
   | 20 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 21 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 22 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 23 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 24 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 25 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 26 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 27 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 28 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 29 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 30 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 31 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_max0_ge_0 (-1 - s V_setparams_i
                            + s V_setparams_pset_dref_off24)]
     ((1 # 1) + s V_setparams_z
      + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 33 => ((1 # 1) + s V_setparams_z <= z)%Q
   | 34 => ((1 # 1) + s V_setparams_z <= z)%Q
   | 35 => hints
     [(*-1 0*) F_one]
     ((1 # 1) + s V_setparams_z <= z)%Q
   | 36 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 37 => ((1 # 1) + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 38 => ((1 # 1) + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 39 => ((1 # 1) + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 40 => ((1 # 1) + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 41 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_setparams_pset_dref_off24)) (F_check_ge (s V_setparams_pset_dref_off24) (0))]
     (s V_setparams_z
      + max0(-s V_setparams_i + s V_setparams_pset_dref_off24) <= z)%Q
   | 42 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_setparams_i
                                       + s V_setparams_pset_dref_off8) (1)]
     (s V_setparams_pset_dref_off24 + s V_setparams_z
      + max0(-s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 43 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 44 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 45 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 46 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 47 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 48 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 49 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 50 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 51 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 52 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 53 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 54 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 55 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 56 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_setparams_i
                                                + s V_setparams_pset_dref_off8)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
      + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 57 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z <= z)%Q
   | 58 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z <= z)%Q
   | 59 => hints
     [(*-1 0*) F_one; (*-1 0*) F_max0_ge_0 (s V_setparams_pset_dref_off24);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_setparams_pset_dref_off24) (0))) (F_max0_ge_0 (s V_setparams_pset_dref_off24))]
     ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z <= z)%Q
   | 60 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 61 => (s V_setparams_pset_dref_off24 + s V_setparams_z
            + (1 # 14) * max0(-1 - s V_setparams__tmp)
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 62 => hints
     [(*-1 0*) F_max0_ge_0 (s V_setparams_pset_dref_off24);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_setparams_pset_dref_off24) (0))) (F_max0_ge_0 (s V_setparams_pset_dref_off24));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_setparams_i
                                                + s V_setparams_pset_dref_off8)) (F_check_ge (0) (0));
      (*-0.0714286 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                         - s V_setparams__tmp)) (F_check_ge (0) (0))]
     (s V_setparams_pset_dref_off24 + s V_setparams_z
      + (1 # 14) * max0(-1 - s V_setparams__tmp)
      + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 63 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 64 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 65 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 66 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 67 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 - s V_setparams_i
                            + s V_setparams_pset_dref_off8);
      (*-1 0*) F_max0_ge_0 (s V_setparams_pset_dref_off24);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_setparams_pset_dref_off24) (0))) (F_max0_ge_0 (s V_setparams_pset_dref_off24))]
     ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
      + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 68 => (s V_setparams_z <= z)%Q
   | 69 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 70 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-1 - s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 71 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 72 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 73 => ((1 # 1) + s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | 74 => (s V_setparams_pset_dref_off24 + s V_setparams_z
            + max0(-s V_setparams_i + s V_setparams_pset_dref_off8) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_setparams =>
    [mkPA Q (fun n z s => ai_setparams n s /\ annot0_setparams n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_setparams (proc_start P_setparams) s1 (proc_end P_setparams) s2 ->
    (s2 V_setparams_z <= max0(s1 V_setparams_pset_dref_off24)
                         + max0(s1 V_setparams_pset_dref_off8))%Q.
Proof.
  prove_bound ipa admissible_ipa P_setparams.
Qed.
