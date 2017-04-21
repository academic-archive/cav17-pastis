Require Import pasta.Pasta.

Inductive proc: Type :=
  P_read_scan_script.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_read_scan_script_z := 1%positive.
Notation V_read_scan_script__tmp := 2%positive.
Notation V_read_scan_script_ncomps := 3%positive.
Notation V_read_scan_script_scanno := 4%positive.
Notation V_read_scan_script_cinfo := 5%positive.
Notation V_read_scan_script_filename := 6%positive.
Definition Pedges_read_scan_script: list (edge proc) :=
  (EA 1 (AAssign V_read_scan_script_z (Some (ENum (0)))) 2)::
  (EA 2 AWeaken 3)::(EA 3 ANone 82)::(EA 3 ANone 4)::(EA 4 (AAssign
  V_read_scan_script_scanno (Some (ENum (0)))) 5)::(EA 5 ANone 6)::
  (EA 6 AWeaken 7)::(EA 7 ANone 21)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::
  (EA 9 ANone 18)::(EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_read_scan_script_scanno) s) > (eval (ENum (0))
  s))%Z)) 13)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_read_scan_script_scanno) s) <= (eval (ENum (0))
  s))%Z)) 12)::(EA 12 AWeaken 15)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::
  (EA 15 (AAssign V_read_scan_script__tmp (Some (ENum (1)))) 16)::
  (EA 16 ANone 17)::(EA 17 AWeaken 85)::(EA 18 (AAssign
  V_read_scan_script__tmp (Some (ENum (0)))) 19)::(EA 19 ANone 20)::
  (EA 20 AWeaken 85)::(EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_read_scan_script_scanno) s) >= (eval (ENum (100))
  s))%Z)) 78)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_read_scan_script_scanno) s) < (eval (ENum (100))
  s))%Z)) 23)::(EA 23 AWeaken 24)::(EA 24 (AAssign V_read_scan_script_ncomps
  (Some (ENum (1)))) 25)::(EA 25 ANone 26)::(EA 26 AWeaken 27)::
  (EA 27 ANone 61)::(EA 27 ANone 28)::(EA 28 AWeaken 29)::(EA 29 ANone 32)::
  (EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 AWeaken 49)::
  (EA 32 AWeaken 33)::(EA 33 ANone 34)::(EA 33 ANone 60)::
  (EA 34 AWeaken 35)::(EA 35 ANone 60)::(EA 35 ANone 36)::
  (EA 36 AWeaken 37)::(EA 37 ANone 38)::(EA 37 ANone 59)::
  (EA 38 AWeaken 39)::(EA 39 ANone 59)::(EA 39 ANone 40)::
  (EA 40 AWeaken 41)::(EA 41 ANone 42)::(EA 41 ANone 58)::
  (EA 42 AWeaken 43)::(EA 43 ANone 58)::(EA 43 ANone 44)::
  (EA 44 AWeaken 45)::(EA 45 ANone 47)::(EA 45 ANone 46)::(EA 46 ANone 66)::
  (EA 47 ANone 48)::(EA 48 AWeaken 49)::(EA 49 ANone 50)::(EA 49 ANone 52)::
  (EA 50 AWeaken 51)::(EA 51 ANone 57)::(EA 51 ANone 52)::(EA 52 (AAssign
  V_read_scan_script_scanno (Some (EAdd (EVar V_read_scan_script_scanno)
  (ENum (1))))) 53)::(EA 53 ANone 54)::(EA 54 ANone 55)::(EA 55 (AAssign
  V_read_scan_script_z (Some (EAdd (ENum (1))
  (EVar V_read_scan_script_z)))) 56)::(EA 56 AWeaken 7)::(EA 57 ANone 66)::
  (EA 58 ANone 66)::(EA 59 ANone 66)::(EA 60 ANone 66)::(EA 61 AWeaken 62)::
  (EA 62 (AGuard (fun s => ((eval (EVar V_read_scan_script_ncomps) s) >=
  (eval (ENum (4)) s))%Z)) 74)::(EA 62 (AGuard
  (fun s => ((eval (EVar V_read_scan_script_ncomps) s) < (eval (ENum (4))
  s))%Z)) 63)::(EA 63 AWeaken 64)::(EA 64 ANone 69)::(EA 64 ANone 65)::
  (EA 65 ANone 66)::(EA 66 (AAssign V_read_scan_script__tmp
  (Some (ENum (0)))) 67)::(EA 67 ANone 68)::(EA 68 AWeaken 85)::
  (EA 69 (AAssign V_read_scan_script_ncomps
  (Some (EAdd (EVar V_read_scan_script_ncomps) (ENum (1))))) 70)::
  (EA 70 ANone 71)::(EA 71 ANone 72)::(EA 72 (AAssign V_read_scan_script_z
  (Some (EAdd (ENum (1)) (EVar V_read_scan_script_z)))) 73)::
  (EA 73 AWeaken 27)::(EA 74 AWeaken 75)::(EA 75 (AAssign
  V_read_scan_script__tmp (Some (ENum (0)))) 76)::(EA 76 ANone 77)::
  (EA 77 AWeaken 85)::(EA 78 AWeaken 79)::(EA 79 (AAssign
  V_read_scan_script__tmp (Some (ENum (0)))) 80)::(EA 80 ANone 81)::
  (EA 81 AWeaken 85)::(EA 82 (AAssign V_read_scan_script__tmp
  (Some (ENum (0)))) 83)::(EA 83 ANone 84)::(EA 84 AWeaken 85)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_read_scan_script => Pedges_read_scan_script
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_read_scan_script => 85
     end)%positive;
  var_global := var_global
}.

Definition ai_read_scan_script (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 3 => (-1 * s V_read_scan_script_z <= 0 /\ 1 * s V_read_scan_script_z <= 0)%Z
   | 4 => (1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 5 => (-1 * s V_read_scan_script_z <= 0 /\ 1 * s V_read_scan_script_z <= 0 /\ 1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_scanno <= 0)%Z
   | 6 => (-1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 7 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno <= 0)%Z
   | 8 => (-1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 9 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno <= 0)%Z
   | 10 => (-1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 11 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno <= 0)%Z
   | 12 => (-1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ 1 * s V_read_scan_script_scanno <= 0)%Z
   | 13 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno + 1 <= 0)%Z
   | 14 => (-1 * s V_read_scan_script_scanno + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 15 => (-1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 16 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script__tmp + -1 <= 0 /\ -1 * s V_read_scan_script__tmp + 1 <= 0)%Z
   | 17 => (-1 * s V_read_scan_script__tmp + 1 <= 0 /\ 1 * s V_read_scan_script__tmp + -1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 18 => (-1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 19 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script__tmp <= 0 /\ -1 * s V_read_scan_script__tmp <= 0)%Z
   | 20 => (-1 * s V_read_scan_script__tmp <= 0 /\ 1 * s V_read_scan_script__tmp <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 21 => (-1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 22 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno <= 0)%Z
   | 23 => (-1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ 1 * s V_read_scan_script_scanno + -99 <= 0)%Z
   | 24 => (1 * s V_read_scan_script_scanno + -99 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno <= 0)%Z
   | 25 => (-1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ 1 * s V_read_scan_script_scanno + -99 <= 0 /\ 1 * s V_read_scan_script_ncomps + -1 <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0)%Z
   | 26 => (-1 * s V_read_scan_script_ncomps + 1 <= 0 /\ 1 * s V_read_scan_script_ncomps + -1 <= 0 /\ 1 * s V_read_scan_script_scanno + -99 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno <= 0)%Z
   | 27 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 28 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 29 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 30 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 31 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 32 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 33 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 34 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 35 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 36 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 37 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 38 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 39 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 40 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 41 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 42 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 43 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 44 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 45 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 46 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 47 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 48 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 49 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 50 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 51 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 52 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 53 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno + 1 <= 0)%Z
   | 54 => (-1 * s V_read_scan_script_scanno + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 55 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno + 1 <= 0)%Z
   | 56 => (-1 * s V_read_scan_script_scanno + 1 <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_z + 1 <= 0)%Z
   | 57 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 58 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 59 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 60 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 61 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 62 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 63 => (-1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ 1 * s V_read_scan_script_ncomps + -3 <= 0)%Z
   | 64 => (1 * s V_read_scan_script_ncomps + -3 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0)%Z
   | 65 => (-1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ 1 * s V_read_scan_script_ncomps + -3 <= 0)%Z
   | 66 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0)%Z
   | 67 => (-1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0 /\ 1 * s V_read_scan_script__tmp <= 0 /\ -1 * s V_read_scan_script__tmp <= 0)%Z
   | 68 => (-1 * s V_read_scan_script__tmp <= 0 /\ 1 * s V_read_scan_script__tmp <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0)%Z
   | 69 => (-1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 1 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ 1 * s V_read_scan_script_ncomps + -3 <= 0)%Z
   | 70 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 2 <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 71 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_ncomps + 2 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 72 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_ncomps + 2 <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 73 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_ncomps + 2 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z + 1 <= 0)%Z
   | 74 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 4 <= 0)%Z
   | 75 => (-1 * s V_read_scan_script_ncomps + 4 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 76 => (1 * s V_read_scan_script_ncomps + -4 <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_ncomps + 4 <= 0 /\ 1 * s V_read_scan_script__tmp <= 0 /\ -1 * s V_read_scan_script__tmp <= 0)%Z
   | 77 => (-1 * s V_read_scan_script__tmp <= 0 /\ 1 * s V_read_scan_script__tmp <= 0 /\ -1 * s V_read_scan_script_ncomps + 4 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno <= 0 /\ 1 * s V_read_scan_script_ncomps + -4 <= 0)%Z
   | 78 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno + 100 <= 0)%Z
   | 79 => (-1 * s V_read_scan_script_scanno + 100 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 80 => (-1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_scanno + 100 <= 0 /\ 1 * s V_read_scan_script__tmp <= 0 /\ -1 * s V_read_scan_script__tmp <= 0)%Z
   | 81 => (-1 * s V_read_scan_script__tmp <= 0 /\ 1 * s V_read_scan_script__tmp <= 0 /\ -1 * s V_read_scan_script_scanno + 100 <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 82 => (1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 83 => (-1 * s V_read_scan_script_z <= 0 /\ 1 * s V_read_scan_script_z <= 0 /\ 1 * s V_read_scan_script__tmp <= 0 /\ -1 * s V_read_scan_script__tmp <= 0)%Z
   | 84 => (-1 * s V_read_scan_script__tmp <= 0 /\ 1 * s V_read_scan_script__tmp <= 0 /\ 1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script_z <= 0)%Z
   | 85 => (1 * s V_read_scan_script__tmp + -1 <= 0 /\ -1 * s V_read_scan_script_z <= 0 /\ -1 * s V_read_scan_script__tmp <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_read_scan_script (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((400 # 1) <= z)%Q
   | 2 => ((400 # 1) + s V_read_scan_script_z <= z)%Q
   | 3 => ((400 # 1) + s V_read_scan_script_z <= z)%Q
   | 4 => ((400 # 1) + s V_read_scan_script_z <= z)%Q
   | 5 => (s V_read_scan_script_z
           + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 6 => (s V_read_scan_script_z
           + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 7 => (s V_read_scan_script_z
           + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 8 => hints
     [(*-4 0*) F_max0_ge_0 (100 - s V_read_scan_script_scanno)]
     (s V_read_scan_script_z
      + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 9 => (s V_read_scan_script_z <= z)%Q
   | 10 => (s V_read_scan_script_z <= z)%Q
   | 11 => (s V_read_scan_script_z <= z)%Q
   | 12 => (s V_read_scan_script_z <= z)%Q
   | 13 => (s V_read_scan_script_z <= z)%Q
   | 14 => (s V_read_scan_script_z <= z)%Q
   | 15 => (s V_read_scan_script_z <= z)%Q
   | 16 => (s V_read_scan_script_z <= z)%Q
   | 17 => (s V_read_scan_script_z <= z)%Q
   | 18 => (s V_read_scan_script_z <= z)%Q
   | 19 => (s V_read_scan_script_z <= z)%Q
   | 20 => (s V_read_scan_script_z <= z)%Q
   | 21 => (s V_read_scan_script_z
            + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 22 => (s V_read_scan_script_z
            + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 23 => (s V_read_scan_script_z
            + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 24 => (s V_read_scan_script_z
            + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 25 => ((1 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            + s V_read_scan_script_z
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 26 => hints
     [(*-4 0*) F_max0_pre_decrement 1 (100 - s V_read_scan_script_scanno) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_read_scan_script_z) (0))) (F_max0_ge_0 (s V_read_scan_script_z))]
     ((1 # 2) - (1 # 2) * s V_read_scan_script_ncomps
      + s V_read_scan_script_z
      - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
      + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 27 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 28 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 29 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 30 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 31 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 32 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 33 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 34 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 35 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 36 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 37 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 38 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 39 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 40 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 41 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 42 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 43 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 44 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 45 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 46 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 47 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 48 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 49 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 50 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 51 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 52 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 53 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(100 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 54 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(100 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 55 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(100 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 56 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                 - s V_read_scan_script_ncomps)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_read_scan_script_ncomps) (0))) (F_max0_ge_0 (4
                                                                    - s V_read_scan_script_ncomps));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_read_scan_script_z)) (F_check_ge (-1
                                                                    + s V_read_scan_script_z) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                 + s V_read_scan_script_ncomps) (0))) (F_max0_ge_0 (-1
                                                                    + s V_read_scan_script_ncomps))]
     ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
      - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
      + max0(-1 + s V_read_scan_script_z)
      + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 57 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 58 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 59 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 60 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 61 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 62 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 63 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 64 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 65 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 66 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 67 => ((7 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + max0(1 - s V_read_scan_script__tmp)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 68 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_read_scan_script_ncomps) (3
                                                                    - s V_read_scan_script_ncomps));
      (*-1 0*) F_max0_ge_0 (3 - s V_read_scan_script_ncomps);
      (*-4 0*) F_max0_ge_0 (99 - s V_read_scan_script_scanno);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_read_scan_script_z)) (F_check_ge (s V_read_scan_script_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_read_scan_script_ncomps) (0))) (F_max0_ge_0 (4
                                                                    - s V_read_scan_script_ncomps));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1
                                                 - s V_read_scan_script__tmp)) (F_check_ge (0) (0));
      (*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                + s V_read_scan_script_ncomps) (0))) (F_max0_ge_0 (-1
                                                                    + s V_read_scan_script_ncomps))]
     ((7 # 2) - (1 # 2) * s V_read_scan_script_ncomps
      - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
      + max0(1 - s V_read_scan_script__tmp)
      + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
      + max0(s V_read_scan_script_z) <= z)%Q
   | 69 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 70 => ((5 # 1) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-2 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 71 => ((5 # 1) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-2 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 72 => ((5 # 1) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-2 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 73 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_read_scan_script_z) (0))) (F_max0_ge_0 (s V_read_scan_script_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_read_scan_script_z)) (F_check_ge (-1
                                                                    + s V_read_scan_script_z) (0));
      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                    + s V_read_scan_script_ncomps)) (F_check_ge (-1
                                                                    + s V_read_scan_script_ncomps) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                 + s V_read_scan_script_ncomps) (0))) (F_max0_ge_0 (-2
                                                                    + s V_read_scan_script_ncomps))]
     ((5 # 1) - (1 # 2) * s V_read_scan_script_ncomps
      - (1 # 2) * max0(-2 + s V_read_scan_script_ncomps)
      + max0(-1 + s V_read_scan_script_z)
      + (4 # 1) * max0(99 - s V_read_scan_script_scanno) <= z)%Q
   | 74 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 75 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 76 => ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
            - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
            + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
            + max0(s V_read_scan_script_z) <= z)%Q
   | 77 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_read_scan_script_ncomps) (3
                                                                    - s V_read_scan_script_ncomps));
      (*-1 0*) F_max0_ge_0 (3 - s V_read_scan_script_ncomps);
      (*-4 0*) F_max0_ge_0 (99 - s V_read_scan_script_scanno);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_read_scan_script_z)) (F_check_ge (s V_read_scan_script_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_read_scan_script_ncomps) (0))) (F_max0_ge_0 (4
                                                                    - s V_read_scan_script_ncomps));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                 + s V_read_scan_script_ncomps) (0))) (F_max0_ge_0 (-1
                                                                    + s V_read_scan_script_ncomps))]
     ((9 # 2) - (1 # 2) * s V_read_scan_script_ncomps
      - (1 # 2) * max0(-1 + s V_read_scan_script_ncomps)
      + (4 # 1) * max0(99 - s V_read_scan_script_scanno)
      + max0(s V_read_scan_script_z) <= z)%Q
   | 78 => (s V_read_scan_script_z
            + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 79 => (s V_read_scan_script_z
            + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 80 => (s V_read_scan_script_z
            + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 81 => hints
     [(*-4 0*) F_max0_monotonic (F_check_ge (100
                                             - s V_read_scan_script_scanno) (99
                                                                    - s V_read_scan_script_scanno));
      (*-4 0*) F_max0_ge_0 (99 - s V_read_scan_script_scanno)]
     (s V_read_scan_script_z
      + (4 # 1) * max0(100 - s V_read_scan_script_scanno) <= z)%Q
   | 82 => ((400 # 1) + s V_read_scan_script_z <= z)%Q
   | 83 => ((400 # 1) + s V_read_scan_script_z <= z)%Q
   | 84 => hints
     [(*-400 0*) F_one]
     ((400 # 1) + s V_read_scan_script_z <= z)%Q
   | 85 => (s V_read_scan_script_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_read_scan_script =>
    [mkPA Q (fun n z s => ai_read_scan_script n s /\ annot0_read_scan_script n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_read_scan_script (proc_start P_read_scan_script) s1 (proc_end P_read_scan_script) s2 ->
    (s2 V_read_scan_script_z <= (400 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_read_scan_script.
Qed.
