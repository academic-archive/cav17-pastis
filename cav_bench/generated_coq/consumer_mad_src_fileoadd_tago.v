Require Import pasta.Pasta.

Inductive proc: Type :=
  P_add_tag.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_add_tag_z := 1%positive.
Notation V_add_tag__tmp := 2%positive.
Notation V_add_tag__tmp1 := 3%positive.
Notation V_add_tag_begin1 := 4%positive.
Notation V_add_tag_begin2 := 5%positive.
Notation V_add_tag_end1 := 6%positive.
Notation V_add_tag_end2 := 7%positive.
Notation V_add_tag_file_dref_off32 := 8%positive.
Notation V_add_tag_i := 9%positive.
Notation V_add_tag_location := 10%positive.
Notation V_add_tag_file := 11%positive.
Notation V_add_tag_length := 12%positive.
Definition Pedges_add_tag: list (edge proc) :=
  (EA 1 (AAssign V_add_tag_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_add_tag_i) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 (AGuard (fun s => ((eval (EVar V_add_tag_file_dref_off32) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_add_tag_end2) s) >= (eval (ENum (0)) s))%Z)) 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_add_tag_end1) s) >= (eval (ENum (0))
  s))%Z)) 6)::(EA 6 (AGuard (fun s => ((eval (EVar V_add_tag_begin2) s) >=
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_add_tag_begin1) s) >= (eval (ENum (0))
  s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 (AAssign V_add_tag__tmp1
  (Some (EVar V_add_tag_length))) 10)::(EA 10 (AAssign V_add_tag_location
  None) 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_add_tag_location) s) = (eval (ENum (-1))
  s))%Z)) 67)::(EA 12 (AGuard (fun s => ((eval (EVar V_add_tag_location)
  s) <> (eval (ENum (-1)) s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 (AAssign
  V_add_tag_begin1 (Some (EVar V_add_tag_location))) 15)::(EA 15 (AAssign
  V_add_tag_end1 (Some (EAdd (EVar V_add_tag_begin1)
  (EVar V_add_tag__tmp1)))) 16)::(EA 16 (AAssign V_add_tag_i
  (Some (ENum (0)))) 17)::(EA 17 ANone 18)::(EA 18 AWeaken 19)::
  (EA 19 (AGuard (fun s => ((eval (EVar V_add_tag_i) s) <
  (eval (EVar V_add_tag_file_dref_off32) s))%Z)) 39)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_add_tag_i) s) >=
  (eval (EVar V_add_tag_file_dref_off32) s))%Z)) 20)::(EA 20 AWeaken 21)::
  (EA 21 ANone 36)::(EA 21 ANone 22)::(EA 22 AWeaken 23)::(EA 23 ANone 33)::
  (EA 23 ANone 24)::(EA 24 AWeaken 25)::(EA 25 ANone 30)::(EA 25 ANone 26)::
  (EA 26 (AAssign V_add_tag_file_dref_off32
  (Some (EAdd (EVar V_add_tag_file_dref_off32) (ENum (1))))) 27)::
  (EA 27 (AAssign V_add_tag__tmp (Some (ENum (0)))) 28)::(EA 28 ANone 29)::
  (EA 29 AWeaken 71)::(EA 30 (AAssign V_add_tag__tmp
  (Some (ENum (-1)))) 31)::(EA 31 ANone 32)::(EA 32 AWeaken 71)::
  (EA 33 (AAssign V_add_tag__tmp (Some (ENum (-1)))) 34)::(EA 34 ANone 35)::
  (EA 35 AWeaken 71)::(EA 36 (AAssign V_add_tag__tmp
  (Some (ENum (-1)))) 37)::(EA 37 ANone 38)::(EA 38 AWeaken 71)::
  (EA 39 AWeaken 40)::(EA 40 (AAssign V_add_tag_begin2 None) 41)::
  (EA 41 (AAssign V_add_tag_end2 None) 42)::(EA 42 AWeaken 43)::
  (EA 43 (AGuard (fun s => ((eval (EVar V_add_tag_begin1) s) =
  (eval (EVar V_add_tag_begin2) s))%Z)) 45)::(EA 43 (AGuard
  (fun s => ((eval (EVar V_add_tag_begin1) s) <>
  (eval (EVar V_add_tag_begin2) s))%Z)) 44)::(EA 44 AWeaken 48)::
  (EA 45 AWeaken 46)::(EA 46 (AGuard (fun s => ((eval (EVar V_add_tag_end1)
  s) = (eval (EVar V_add_tag_end2) s))%Z)) 63)::(EA 46 (AGuard
  (fun s => ((eval (EVar V_add_tag_end1) s) <> (eval (EVar V_add_tag_end2)
  s))%Z)) 47)::(EA 47 AWeaken 48)::(EA 48 (AGuard
  (fun s => ((eval (EVar V_add_tag_begin1) s) < (eval (EVar V_add_tag_end2)
  s))%Z)) 50)::(EA 48 (AGuard (fun s => ((eval (EVar V_add_tag_begin1) s) >=
  (eval (EVar V_add_tag_end2) s))%Z)) 49)::(EA 49 AWeaken 53)::
  (EA 50 AWeaken 51)::(EA 51 (AGuard (fun s => ((eval (EVar V_add_tag_end1)
  s) > (eval (EVar V_add_tag_begin2) s))%Z)) 59)::(EA 51 (AGuard
  (fun s => ((eval (EVar V_add_tag_end1) s) <= (eval (EVar V_add_tag_begin2)
  s))%Z)) 52)::(EA 52 AWeaken 53)::(EA 53 ANone 54)::(EA 54 (AAssign
  V_add_tag_i (Some (EAdd (EVar V_add_tag_i) (ENum (1))))) 55)::
  (EA 55 ANone 56)::(EA 56 ANone 57)::(EA 57 (AAssign V_add_tag_z
  (Some (EAdd (ENum (1)) (EVar V_add_tag_z)))) 58)::(EA 58 AWeaken 19)::
  (EA 59 AWeaken 60)::(EA 60 (AAssign V_add_tag__tmp
  (Some (ENum (-1)))) 61)::(EA 61 ANone 62)::(EA 62 AWeaken 71)::
  (EA 63 AWeaken 64)::(EA 64 (AAssign V_add_tag__tmp (Some (ENum (0)))) 65)::
  (EA 65 ANone 66)::(EA 66 AWeaken 71)::(EA 67 AWeaken 68)::(EA 68 (AAssign
  V_add_tag__tmp (Some (ENum (-1)))) 69)::(EA 69 ANone 70)::
  (EA 70 AWeaken 71)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_add_tag => Pedges_add_tag
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_add_tag => 71
     end)%positive;
  var_global := var_global
}.

Definition ai_add_tag (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_z <= 0)%Z
   | 3 => (-1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0)%Z
   | 4 => (-1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0)%Z
   | 5 => (-1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_end2 <= 0)%Z
   | 6 => (-1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_end1 <= 0)%Z
   | 7 => (-1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_begin2 <= 0)%Z
   | 8 => (-1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_begin1 <= 0)%Z
   | 9 => (-1 * s V_add_tag_begin1 <= 0 /\ -1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_begin2 <= 0)%Z
   | 10 => (-1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_begin1 <= 0)%Z
   | 11 => (-1 * s V_add_tag_begin1 <= 0 /\ -1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_begin2 <= 0)%Z
   | 12 => (-1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_begin1 <= 0)%Z
   | 13 => (-1 * s V_add_tag_begin1 <= 0 /\ -1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_begin2 <= 0)%Z
   | 14 => (-1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_begin1 <= 0)%Z
   | 15 => (-1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_begin2 <= 0)%Z
   | 16 => (-1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0)%Z
   | 17 => (-1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_begin2 <= 0 /\ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i <= 0)%Z
   | 18 => (-1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0)%Z
   | 19 => (-1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0)%Z
   | 20 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0)%Z
   | 21 => (1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0)%Z
   | 22 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0)%Z
   | 23 => (1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0)%Z
   | 24 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0)%Z
   | 25 => (1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0)%Z
   | 26 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0)%Z
   | 27 => (-1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ 1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i + -1 <= 0)%Z
   | 28 => (1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i + -1 <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag__tmp <= 0 /\ -1 * s V_add_tag__tmp <= 0)%Z
   | 29 => (-1 * s V_add_tag__tmp <= 0 /\ 1 * s V_add_tag__tmp <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ 1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i + -1 <= 0)%Z
   | 30 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0)%Z
   | 31 => (1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag__tmp + 1 <= 0 /\ -1 * s V_add_tag__tmp + -1 <= 0)%Z
   | 32 => (-1 * s V_add_tag__tmp + -1 <= 0 /\ 1 * s V_add_tag__tmp + 1 <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0)%Z
   | 33 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0)%Z
   | 34 => (1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag__tmp + 1 <= 0 /\ -1 * s V_add_tag__tmp + -1 <= 0)%Z
   | 35 => (-1 * s V_add_tag__tmp + -1 <= 0 /\ 1 * s V_add_tag__tmp + 1 <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0)%Z
   | 36 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0)%Z
   | 37 => (1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag__tmp + 1 <= 0 /\ -1 * s V_add_tag__tmp + -1 <= 0)%Z
   | 38 => (-1 * s V_add_tag__tmp + -1 <= 0 /\ 1 * s V_add_tag__tmp + 1 <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_file_dref_off32+ -1 * s V_add_tag_i <= 0)%Z
   | 39 => (-1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0)%Z
   | 40 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0)%Z
   | 41 => (-1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0)%Z
   | 42 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0)%Z
   | 43 => (-1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0)%Z
   | 44 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0)%Z
   | 45 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_begin1+ 1 * s V_add_tag_begin2 <= 0)%Z
   | 46 => (-1 * s V_add_tag_begin1+ 1 * s V_add_tag_begin2 <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0)%Z
   | 47 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_begin1+ 1 * s V_add_tag_begin2 <= 0)%Z
   | 48 => (-1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0)%Z
   | 49 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_begin1+ 1 * s V_add_tag_end2 <= 0)%Z
   | 50 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_end2 + 1 <= 0)%Z
   | 51 => (1 * s V_add_tag_begin1+ -1 * s V_add_tag_end2 + 1 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0)%Z
   | 52 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_end2 + 1 <= 0 /\ -1 * s V_add_tag_begin2+ 1 * s V_add_tag_end1 <= 0)%Z
   | 53 => (-1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0)%Z
   | 54 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0)%Z
   | 55 => (-1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i + 1 <= 0)%Z
   | 56 => (-1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0)%Z
   | 57 => (-1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_i + 1 <= 0)%Z
   | 58 => (-1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z + 1 <= 0)%Z
   | 59 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_end2 + 1 <= 0 /\ 1 * s V_add_tag_begin2+ -1 * s V_add_tag_end1 + 1 <= 0)%Z
   | 60 => (1 * s V_add_tag_begin2+ -1 * s V_add_tag_end1 + 1 <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_end2 + 1 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0)%Z
   | 61 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_end2 + 1 <= 0 /\ 1 * s V_add_tag_begin2+ -1 * s V_add_tag_end1 + 1 <= 0 /\ 1 * s V_add_tag__tmp + 1 <= 0 /\ -1 * s V_add_tag__tmp + -1 <= 0)%Z
   | 62 => (-1 * s V_add_tag__tmp + -1 <= 0 /\ 1 * s V_add_tag__tmp + 1 <= 0 /\ 1 * s V_add_tag_begin2+ -1 * s V_add_tag_end1 + 1 <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_end2 + 1 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0)%Z
   | 63 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_begin1+ 1 * s V_add_tag_begin2 <= 0 /\ 1 * s V_add_tag_end1+ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_end1+ 1 * s V_add_tag_end2 <= 0)%Z
   | 64 => (-1 * s V_add_tag_end1+ 1 * s V_add_tag_end2 <= 0 /\ 1 * s V_add_tag_end1+ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_begin1+ 1 * s V_add_tag_begin2 <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0)%Z
   | 65 => (-1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_begin1+ 1 * s V_add_tag_begin2 <= 0 /\ 1 * s V_add_tag_end1+ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_end1+ 1 * s V_add_tag_end2 <= 0 /\ 1 * s V_add_tag__tmp <= 0 /\ -1 * s V_add_tag__tmp <= 0)%Z
   | 66 => (-1 * s V_add_tag__tmp <= 0 /\ 1 * s V_add_tag__tmp <= 0 /\ -1 * s V_add_tag_end1+ 1 * s V_add_tag_end2 <= 0 /\ 1 * s V_add_tag_end1+ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_begin1+ 1 * s V_add_tag_begin2 <= 0 /\ 1 * s V_add_tag_begin1+ -1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32+ 1 * s V_add_tag_i + 1 <= 0)%Z
   | 67 => (-1 * s V_add_tag_begin1 <= 0 /\ -1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_begin2 <= 0 /\ 1 * s V_add_tag_location + 1 <= 0 /\ -1 * s V_add_tag_location + -1 <= 0)%Z
   | 68 => (-1 * s V_add_tag_location + -1 <= 0 /\ 1 * s V_add_tag_location + 1 <= 0 /\ -1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_begin1 <= 0)%Z
   | 69 => (-1 * s V_add_tag_begin1 <= 0 /\ -1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_begin2 <= 0 /\ 1 * s V_add_tag_location + 1 <= 0 /\ -1 * s V_add_tag_location + -1 <= 0 /\ 1 * s V_add_tag__tmp + 1 <= 0 /\ -1 * s V_add_tag__tmp + -1 <= 0)%Z
   | 70 => (-1 * s V_add_tag__tmp + -1 <= 0 /\ 1 * s V_add_tag__tmp + 1 <= 0 /\ -1 * s V_add_tag_location + -1 <= 0 /\ 1 * s V_add_tag_location + 1 <= 0 /\ -1 * s V_add_tag_begin2 <= 0 /\ -1 * s V_add_tag_end2 <= 0 /\ -1 * s V_add_tag_i <= 0 /\ 1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_end1 <= 0 /\ -1 * s V_add_tag_begin1 <= 0)%Z
   | 71 => (1 * s V_add_tag__tmp <= 0 /\ -1 * s V_add_tag_file_dref_off32 <= 0 /\ -1 * s V_add_tag_z <= 0 /\ -1 * s V_add_tag_i <= 0 /\ -1 * s V_add_tag__tmp + -1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_add_tag (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 2 => (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 3 => (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 4 => (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 5 => (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 6 => (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 7 => (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 8 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_add_tag_file_dref_off32) (0))) (F_max0_ge_0 (s V_add_tag_file_dref_off32))]
     (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 9 => (-s V_add_tag_file_dref_off32
           + (2 # 1) * max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 10 => (-s V_add_tag_file_dref_off32
            + (2 # 1) * max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 11 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_add_tag_file_dref_off32)) (F_check_ge (s V_add_tag_file_dref_off32) (0))]
     (-s V_add_tag_file_dref_off32
      + (2 # 1) * max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 12 => (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 13 => (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 14 => (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 15 => (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 16 => (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 17 => (max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_add_tag_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_add_tag_z) (0))) (F_max0_ge_0 (-
                                                                    s V_add_tag_z))]
     (max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 19 => (s V_add_tag_z + max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 20 => (s V_add_tag_z + max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 21 => (s V_add_tag_z + max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 22 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_add_tag_file_dref_off32
                                            - s V_add_tag_i) (-1
                                                              + s V_add_tag_file_dref_off32
                                                              - s V_add_tag_i))]
     (s V_add_tag_z + max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 23 => (s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i)]
     (s V_add_tag_z + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 25 => (s V_add_tag_z <= z)%Q
   | 26 => (s V_add_tag_z <= z)%Q
   | 27 => (s V_add_tag_z <= z)%Q
   | 28 => (s V_add_tag_z <= z)%Q
   | 29 => (s V_add_tag_z <= z)%Q
   | 30 => (s V_add_tag_z <= z)%Q
   | 31 => (s V_add_tag_z <= z)%Q
   | 32 => (s V_add_tag_z <= z)%Q
   | 33 => (s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 34 => (s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i)]
     (s V_add_tag_z + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 36 => (s V_add_tag_z + max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 37 => (s V_add_tag_z + max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_add_tag_file_dref_off32
                                             - s V_add_tag_i) (-1
                                                               + s V_add_tag_file_dref_off32
                                                               - s V_add_tag_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i)]
     (s V_add_tag_z + max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 39 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_add_tag_file_dref_off32
                                      - s V_add_tag_i) (1)]
     (s V_add_tag_z + max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 40 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 41 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 42 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 43 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 44 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 45 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 46 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 47 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 48 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 49 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 50 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 51 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 52 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 53 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 54 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 55 => ((1 # 1) + s V_add_tag_z
            + max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 56 => ((1 # 1) + s V_add_tag_z
            + max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 57 => ((1 # 1) + s V_add_tag_z
            + max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 58 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_add_tag_z)) (F_check_ge (s V_add_tag_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_add_tag_z) (0))) (F_max0_ge_0 (s V_add_tag_z))]
     (s V_add_tag_z + max0(s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 59 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 60 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 61 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 62 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i)]
     ((1 # 1) + s V_add_tag_z
      + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 63 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 64 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 65 => ((1 # 1) + s V_add_tag_z
            + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 66 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i)]
     ((1 # 1) + s V_add_tag_z
      + max0(-1 + s V_add_tag_file_dref_off32 - s V_add_tag_i) <= z)%Q
   | 67 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_add_tag_z) (0))) (F_max0_ge_0 (-
                                                                    s V_add_tag_z))]
     (max0(s V_add_tag_file_dref_off32) <= z)%Q
   | 68 => (s V_add_tag_z + max0(s V_add_tag_file_dref_off32)
            + max0(-s V_add_tag_z) <= z)%Q
   | 69 => (s V_add_tag_z + max0(s V_add_tag_file_dref_off32)
            + max0(-s V_add_tag_z) <= z)%Q
   | 70 => hints
     [(*-1 0*) F_max0_ge_0 (s V_add_tag_file_dref_off32);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_add_tag_z)) (F_check_ge (0) (0))]
     (s V_add_tag_z + max0(s V_add_tag_file_dref_off32)
      + max0(-s V_add_tag_z) <= z)%Q
   | 71 => (s V_add_tag_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_add_tag =>
    [mkPA Q (fun n z s => ai_add_tag n s /\ annot0_add_tag n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_add_tag (proc_start P_add_tag) s1 (proc_end P_add_tag) s2 ->
    (s2 V_add_tag_z <= max0(s1 V_add_tag_file_dref_off32))%Q.
Proof.
  prove_bound ipa admissible_ipa P_add_tag.
Qed.
