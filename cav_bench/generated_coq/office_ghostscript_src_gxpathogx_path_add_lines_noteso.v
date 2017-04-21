Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gx_path_add_lines_notes.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gx_path_add_lines_notes_z := 1%positive.
Notation V_gx_path_add_lines_notes__tmp := 2%positive.
Notation V_gx_path_add_lines_notes__tmp1 := 3%positive.
Notation V_gx_path_add_lines_notes__tmp2 := 4%positive.
Notation V_gx_path_add_lines_notes_code := 5%positive.
Notation V_gx_path_add_lines_notes_code1 := 6%positive.
Notation V_gx_path_add_lines_notes_i := 7%positive.
Notation V_gx_path_add_lines_notes_x := 8%positive.
Notation V_gx_path_add_lines_notes_y := 9%positive.
Notation V_gx_path_add_lines_notes_count := 10%positive.
Notation V_gx_path_add_lines_notes_notes := 11%positive.
Notation V_gx_path_add_lines_notes_ppath := 12%positive.
Notation V_gx_path_add_lines_notes_ppts := 13%positive.
Definition Pedges_gx_path_add_lines_notes: list (edge proc) :=
  (EA 1 (AAssign V_gx_path_add_lines_notes_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_gx_path_add_lines_notes__tmp
  (Some (EVar V_gx_path_add_lines_notes_count))) 3)::(EA 3 (AAssign
  V_gx_path_add_lines_notes__tmp2
  (Some (EVar V_gx_path_add_lines_notes_notes))) 4)::(EA 4 (AAssign
  V_gx_path_add_lines_notes_code (Some (ENum (0)))) 5)::(EA 5 AWeaken 6)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_gx_path_add_lines_notes__tmp) s) <=
  (eval (ENum (0)) s))%Z)) 72)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_gx_path_add_lines_notes__tmp) s) >
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 ANone 10)::
  (EA 8 ANone 9)::(EA 9 AWeaken 17)::(EA 10 AWeaken 11)::(EA 11 ANone 15)::
  (EA 11 ANone 12)::(EA 12 (AAssign V_gx_path_add_lines_notes__tmp1
  (Some (ENum (-25)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 76)::
  (EA 15 ANone 16)::(EA 16 AWeaken 17)::(EA 17 ANone 32)::(EA 17 ANone 18)::
  (EA 18 AWeaken 19)::(EA 19 ANone 23)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_gx_path_add_lines_notes__tmp1 (Some (ENum (-14)))) 21)::
  (EA 21 ANone 22)::(EA 22 AWeaken 76)::(EA 23 (AAssign
  V_gx_path_add_lines_notes_code1 None) 24)::(EA 24 AWeaken 25)::
  (EA 25 (AGuard (fun s => ((eval (EVar V_gx_path_add_lines_notes_code1) s) <
  (eval (ENum (0)) s))%Z)) 28)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_gx_path_add_lines_notes_code1) s) >=
  (eval (ENum (0)) s))%Z)) 26)::(EA 26 AWeaken 27)::(EA 27 ANone 32)::
  (EA 28 AWeaken 29)::(EA 29 (AAssign V_gx_path_add_lines_notes__tmp1
  (Some (EVar V_gx_path_add_lines_notes_code1))) 30)::(EA 30 ANone 31)::
  (EA 31 AWeaken 76)::(EA 32 (AAssign V_gx_path_add_lines_notes_i
  (Some (ENum (0)))) 33)::(EA 33 ANone 34)::(EA 34 AWeaken 35)::
  (EA 35 (AGuard (fun s => ((eval (EVar V_gx_path_add_lines_notes_i) s) <
  (eval (EVar V_gx_path_add_lines_notes__tmp) s))%Z)) 37)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_gx_path_add_lines_notes_i) s) >=
  (eval (EVar V_gx_path_add_lines_notes__tmp) s))%Z)) 36)::
  (EA 36 AWeaken 67)::(EA 37 AWeaken 38)::(EA 38 (AAssign
  V_gx_path_add_lines_notes_x None) 39)::(EA 39 (AAssign
  V_gx_path_add_lines_notes_y None) 40)::(EA 40 AWeaken 41)::
  (EA 41 ANone 43)::(EA 41 ANone 42)::(EA 42 AWeaken 52)::
  (EA 43 AWeaken 44)::(EA 44 ANone 64)::(EA 44 ANone 45)::
  (EA 45 AWeaken 46)::(EA 46 ANone 64)::(EA 46 ANone 47)::
  (EA 47 AWeaken 48)::(EA 48 ANone 64)::(EA 48 ANone 49)::
  (EA 49 AWeaken 50)::(EA 50 ANone 64)::(EA 50 ANone 51)::
  (EA 51 AWeaken 52)::(EA 52 ANone 56)::(EA 52 ANone 53)::(EA 53 (AAssign
  V_gx_path_add_lines_notes_code (Some (ENum (-25)))) 54)::(EA 54 ANone 55)::
  (EA 55 AWeaken 67)::(EA 56 ANone 57)::(EA 57 ANone 58)::(EA 58 ANone 59)::
  (EA 59 (AAssign V_gx_path_add_lines_notes_i
  (Some (EAdd (EVar V_gx_path_add_lines_notes_i) (ENum (1))))) 60)::
  (EA 60 ANone 61)::(EA 61 ANone 62)::(EA 62 (AAssign
  V_gx_path_add_lines_notes_z (Some (EAdd (ENum (1))
  (EVar V_gx_path_add_lines_notes_z)))) 63)::(EA 63 AWeaken 35)::
  (EA 64 (AAssign V_gx_path_add_lines_notes_code (Some (ENum (-15)))) 65)::
  (EA 65 ANone 66)::(EA 66 AWeaken 67)::(EA 67 ANone 68)::(EA 67 ANone 69)::
  (EA 68 ANone 69)::(EA 69 (AAssign V_gx_path_add_lines_notes__tmp1
  (Some (EVar V_gx_path_add_lines_notes_code))) 70)::(EA 70 ANone 71)::
  (EA 71 AWeaken 76)::(EA 72 AWeaken 73)::(EA 73 (AAssign
  V_gx_path_add_lines_notes__tmp1 (Some (ENum (0)))) 74)::(EA 74 ANone 75)::
  (EA 75 AWeaken 76)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gx_path_add_lines_notes => Pedges_gx_path_add_lines_notes
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gx_path_add_lines_notes => 76
     end)%positive;
  var_global := var_global
}.

Definition ai_gx_path_add_lines_notes (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 3 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 4 => (1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 5 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 6 => (-1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 7 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 8 => (-1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 9 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 10 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 11 => (-1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 12 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 13 => (-1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp1 + 25 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp1 + -25 <= 0)%Z
   | 14 => (-1 * s V_gx_path_add_lines_notes__tmp1 + -25 <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp1 + 25 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 15 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 16 => (-1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 17 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 18 => (-1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 19 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 20 => (-1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 21 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp1 + 14 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp1 + -14 <= 0)%Z
   | 22 => (-1 * s V_gx_path_add_lines_notes__tmp1 + -14 <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp1 + 14 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 23 => (-1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 24 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 25 => (-1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 26 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code1 <= 0)%Z
   | 27 => (-1 * s V_gx_path_add_lines_notes_code1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 28 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ 1 * s V_gx_path_add_lines_notes_code1 + 1 <= 0)%Z
   | 29 => (1 * s V_gx_path_add_lines_notes_code1 + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 30 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ 1 * s V_gx_path_add_lines_notes_code1 + 1 <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp1 + 1 <= 0)%Z
   | 31 => (1 * s V_gx_path_add_lines_notes__tmp1 + 1 <= 0 /\ 1 * s V_gx_path_add_lines_notes_code1 + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 32 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 33 => (-1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0)%Z
   | 34 => (-1 * s V_gx_path_add_lines_notes_i <= 0 /\ 1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 35 => (-1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i <= 0)%Z
   | 36 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp+ -1 * s V_gx_path_add_lines_notes_i <= 0)%Z
   | 37 => (1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0)%Z
   | 38 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 39 => (1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0)%Z
   | 40 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 41 => (1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0)%Z
   | 42 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 43 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 44 => (1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0)%Z
   | 45 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 46 => (1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0)%Z
   | 47 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 48 => (1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0)%Z
   | 49 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 50 => (1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0)%Z
   | 51 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 52 => (1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0)%Z
   | 53 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 54 => (-1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ 1 * s V_gx_path_add_lines_notes_code + 25 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code + -25 <= 0)%Z
   | 55 => (-1 * s V_gx_path_add_lines_notes_code + -25 <= 0 /\ 1 * s V_gx_path_add_lines_notes_code + 25 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0)%Z
   | 56 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 57 => (1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0)%Z
   | 58 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 59 => (1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0)%Z
   | 60 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i <= 0)%Z
   | 61 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 62 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i <= 0)%Z
   | 63 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_z + 1 <= 0)%Z
   | 64 => (-1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 65 => (-1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ 1 * s V_gx_path_add_lines_notes_code + 15 <= 0 /\ -1 * s V_gx_path_add_lines_notes_code + -15 <= 0)%Z
   | 66 => (-1 * s V_gx_path_add_lines_notes_code + -15 <= 0 /\ 1 * s V_gx_path_add_lines_notes_code + 15 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0)%Z
   | 67 => (1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code + -25 <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 68 => (-1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code + -25 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0)%Z
   | 69 => (1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code + -25 <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 70 => (-1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code + -25 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp1 + -25 <= 0)%Z
   | 71 => (-1 * s V_gx_path_add_lines_notes__tmp1 + -25 <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp1 <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp+ 1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_code + -25 <= 0 /\ -1 * s V_gx_path_add_lines_notes_i <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp + 1 <= 0)%Z
   | 72 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp <= 0)%Z
   | 73 => (1 * s V_gx_path_add_lines_notes__tmp <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 74 => (-1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp1 <= 0 /\ -1 * s V_gx_path_add_lines_notes__tmp1 <= 0)%Z
   | 75 => (-1 * s V_gx_path_add_lines_notes__tmp1 <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp1 <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp <= 0 /\ -1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes_z <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0)%Z
   | 76 => (-1 * s V_gx_path_add_lines_notes_code + -25 <= 0 /\ -1 * s V_gx_path_add_lines_notes_z <= 0 /\ 1 * s V_gx_path_add_lines_notes_code <= 0 /\ 1 * s V_gx_path_add_lines_notes__tmp1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gx_path_add_lines_notes (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_gx_path_add_lines_notes_count) <= z)%Q
   | 2 => (s V_gx_path_add_lines_notes_z
           + max0(s V_gx_path_add_lines_notes_count) <= z)%Q
   | 3 => (s V_gx_path_add_lines_notes_z
           + max0(s V_gx_path_add_lines_notes__tmp) <= z)%Q
   | 4 => (s V_gx_path_add_lines_notes_z
           + max0(s V_gx_path_add_lines_notes__tmp) <= z)%Q
   | 5 => (s V_gx_path_add_lines_notes_z
           + max0(s V_gx_path_add_lines_notes__tmp) <= z)%Q
   | 6 => (s V_gx_path_add_lines_notes_z
           + max0(s V_gx_path_add_lines_notes__tmp) <= z)%Q
   | 7 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gx_path_add_lines_notes__tmp)) (F_check_ge (s V_gx_path_add_lines_notes__tmp) (0))]
     (s V_gx_path_add_lines_notes_z + max0(s V_gx_path_add_lines_notes__tmp) <= z)%Q
   | 8 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 9 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 10 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 11 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 12 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 13 => (-(1 # 1) + s V_gx_path_add_lines_notes__tmp
            + s V_gx_path_add_lines_notes_z
            + (1 # 11) * max0(-14 - s V_gx_path_add_lines_notes__tmp1) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_gx_path_add_lines_notes__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_gx_path_add_lines_notes__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_gx_path_add_lines_notes__tmp));
      (*-0.0909091 0*) F_binom_monotonic 1 (F_max0_ge_0 (-14
                                                         - s V_gx_path_add_lines_notes__tmp1)) (F_check_ge (0) (0))]
     (-(1 # 1) + s V_gx_path_add_lines_notes__tmp
      + s V_gx_path_add_lines_notes_z
      + (1 # 11) * max0(-14 - s V_gx_path_add_lines_notes__tmp1) <= z)%Q
   | 15 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 16 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 17 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 18 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 19 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 20 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 21 => (-(1 # 1) + s V_gx_path_add_lines_notes__tmp
            + s V_gx_path_add_lines_notes_z
            + (1 # 13) * max0(-1 - s V_gx_path_add_lines_notes__tmp1) <= z)%Q
   | 22 => hints
     [(*-0.0769231 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                         - s V_gx_path_add_lines_notes__tmp1)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_gx_path_add_lines_notes__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_gx_path_add_lines_notes__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_gx_path_add_lines_notes__tmp))]
     (-(1 # 1) + s V_gx_path_add_lines_notes__tmp
      + s V_gx_path_add_lines_notes_z
      + (1 # 13) * max0(-1 - s V_gx_path_add_lines_notes__tmp1) <= z)%Q
   | 23 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 24 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (s V_gx_path_add_lines_notes_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gx_path_add_lines_notes_z) (0))) (F_max0_ge_0 (s V_gx_path_add_lines_notes_z))]
     (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 25 => (s V_gx_path_add_lines_notes__tmp <= z)%Q
   | 26 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_gx_path_add_lines_notes_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_gx_path_add_lines_notes_z) (0))) (F_max0_ge_0 (-
                                                                    s V_gx_path_add_lines_notes_z))]
     (s V_gx_path_add_lines_notes__tmp <= z)%Q
   | 27 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 28 => (s V_gx_path_add_lines_notes__tmp <= z)%Q
   | 29 => (s V_gx_path_add_lines_notes__tmp <= z)%Q
   | 30 => (s V_gx_path_add_lines_notes__tmp <= z)%Q
   | 31 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_gx_path_add_lines_notes_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_gx_path_add_lines_notes_z) (0))) (F_max0_ge_0 (-
                                                                    s V_gx_path_add_lines_notes_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_gx_path_add_lines_notes__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_gx_path_add_lines_notes__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_gx_path_add_lines_notes__tmp))]
     (s V_gx_path_add_lines_notes__tmp <= z)%Q
   | 32 => (s V_gx_path_add_lines_notes__tmp + s V_gx_path_add_lines_notes_z <= z)%Q
   | 33 => (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
            + s V_gx_path_add_lines_notes_z <= z)%Q
   | 34 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gx_path_add_lines_notes__tmp
                                                               - s V_gx_path_add_lines_notes_i) (0))) (F_max0_ge_0 (s V_gx_path_add_lines_notes__tmp
                                                                    - s V_gx_path_add_lines_notes_i))]
     (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
      + s V_gx_path_add_lines_notes_z <= z)%Q
   | 35 => (s V_gx_path_add_lines_notes_z
            + max0(s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_gx_path_add_lines_notes__tmp
                                             - s V_gx_path_add_lines_notes_i) (-1
                                                                    + s V_gx_path_add_lines_notes__tmp
                                                                    - s V_gx_path_add_lines_notes_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_gx_path_add_lines_notes__tmp
                            - s V_gx_path_add_lines_notes_i)]
     (s V_gx_path_add_lines_notes_z
      + max0(s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 37 => (s V_gx_path_add_lines_notes_z
            + max0(s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 38 => (s V_gx_path_add_lines_notes_z
            + max0(s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 39 => (s V_gx_path_add_lines_notes_z
            + max0(s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 40 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gx_path_add_lines_notes__tmp
                                                  - s V_gx_path_add_lines_notes_i)) (F_check_ge (s V_gx_path_add_lines_notes__tmp
                                                                    - s V_gx_path_add_lines_notes_i) (0))]
     (s V_gx_path_add_lines_notes_z
      + max0(s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 41 => (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
            + s V_gx_path_add_lines_notes_z <= z)%Q
   | 42 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              + s V_gx_path_add_lines_notes__tmp
                                                              - s V_gx_path_add_lines_notes_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_gx_path_add_lines_notes__tmp
                                                                    - s V_gx_path_add_lines_notes_i))]
     (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
      + s V_gx_path_add_lines_notes_z <= z)%Q
   | 43 => (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
            + s V_gx_path_add_lines_notes_z <= z)%Q
   | 44 => (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
            + s V_gx_path_add_lines_notes_z <= z)%Q
   | 45 => (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
            + s V_gx_path_add_lines_notes_z <= z)%Q
   | 46 => (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
            + s V_gx_path_add_lines_notes_z <= z)%Q
   | 47 => (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
            + s V_gx_path_add_lines_notes_z <= z)%Q
   | 48 => (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
            + s V_gx_path_add_lines_notes_z <= z)%Q
   | 49 => (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
            + s V_gx_path_add_lines_notes_z <= z)%Q
   | 50 => (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
            + s V_gx_path_add_lines_notes_z <= z)%Q
   | 51 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_gx_path_add_lines_notes__tmp
                                       - s V_gx_path_add_lines_notes_i) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gx_path_add_lines_notes__tmp
                                                               - s V_gx_path_add_lines_notes_i) (0))) (F_max0_ge_0 (s V_gx_path_add_lines_notes__tmp
                                                                    - s V_gx_path_add_lines_notes_i))]
     (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
      + s V_gx_path_add_lines_notes_z <= z)%Q
   | 52 => ((1 # 1) + s V_gx_path_add_lines_notes_z
            + max0(-1 + s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 53 => ((1 # 1) + s V_gx_path_add_lines_notes_z
            + max0(-1 + s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 54 => (s V_gx_path_add_lines_notes_z
            + max0(-1 + s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i)
            + (1 # 24) * max0(-1 - s V_gx_path_add_lines_notes_code) <= z)%Q
   | 55 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_gx_path_add_lines_notes__tmp
                            - s V_gx_path_add_lines_notes_i);
      (*-0.0416667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                         - s V_gx_path_add_lines_notes_code)) (F_check_ge (0) (0))]
     (s V_gx_path_add_lines_notes_z
      + max0(-1 + s V_gx_path_add_lines_notes__tmp
             - s V_gx_path_add_lines_notes_i)
      + (1 # 24) * max0(-1 - s V_gx_path_add_lines_notes_code) <= z)%Q
   | 56 => ((1 # 1) + s V_gx_path_add_lines_notes_z
            + max0(-1 + s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 57 => ((1 # 1) + s V_gx_path_add_lines_notes_z
            + max0(-1 + s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 58 => ((1 # 1) + s V_gx_path_add_lines_notes_z
            + max0(-1 + s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 59 => ((1 # 1) + s V_gx_path_add_lines_notes_z
            + max0(-1 + s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 60 => ((1 # 1) + s V_gx_path_add_lines_notes_z
            + max0(s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 61 => ((1 # 1) + s V_gx_path_add_lines_notes_z
            + max0(s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 62 => ((1 # 1) + s V_gx_path_add_lines_notes_z
            + max0(s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 63 => (s V_gx_path_add_lines_notes_z
            + max0(s V_gx_path_add_lines_notes__tmp
                   - s V_gx_path_add_lines_notes_i) <= z)%Q
   | 64 => (s V_gx_path_add_lines_notes__tmp - s V_gx_path_add_lines_notes_i
            + s V_gx_path_add_lines_notes_z <= z)%Q
   | 65 => (-(1 # 1) + s V_gx_path_add_lines_notes__tmp
            - s V_gx_path_add_lines_notes_i + s V_gx_path_add_lines_notes_z
            + max0(-14 - s V_gx_path_add_lines_notes_code) <= z)%Q
   | 66 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_gx_path_add_lines_notes__tmp
                                       - s V_gx_path_add_lines_notes_i) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_gx_path_add_lines_notes__tmp
                            - s V_gx_path_add_lines_notes_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gx_path_add_lines_notes__tmp
                                                               - s V_gx_path_add_lines_notes_i) (0))) (F_max0_ge_0 (s V_gx_path_add_lines_notes__tmp
                                                                    - s V_gx_path_add_lines_notes_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-14
                                                 - s V_gx_path_add_lines_notes_code)) (F_check_ge (0) (0))]
     (-(1 # 1) + s V_gx_path_add_lines_notes__tmp
      - s V_gx_path_add_lines_notes_i + s V_gx_path_add_lines_notes_z
      + max0(-14 - s V_gx_path_add_lines_notes_code) <= z)%Q
   | 67 => (s V_gx_path_add_lines_notes_z <= z)%Q
   | 68 => (s V_gx_path_add_lines_notes_z <= z)%Q
   | 69 => (s V_gx_path_add_lines_notes_z <= z)%Q
   | 70 => (s V_gx_path_add_lines_notes_z <= z)%Q
   | 71 => (s V_gx_path_add_lines_notes_z <= z)%Q
   | 72 => (s V_gx_path_add_lines_notes_z
            + max0(s V_gx_path_add_lines_notes__tmp) <= z)%Q
   | 73 => (s V_gx_path_add_lines_notes_z
            + max0(s V_gx_path_add_lines_notes__tmp) <= z)%Q
   | 74 => (s V_gx_path_add_lines_notes_z
            + max0(s V_gx_path_add_lines_notes__tmp) <= z)%Q
   | 75 => hints
     [(*-1 0*) F_max0_ge_0 (s V_gx_path_add_lines_notes__tmp)]
     (s V_gx_path_add_lines_notes_z + max0(s V_gx_path_add_lines_notes__tmp) <= z)%Q
   | 76 => (s V_gx_path_add_lines_notes_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gx_path_add_lines_notes =>
    [mkPA Q (fun n z s => ai_gx_path_add_lines_notes n s /\ annot0_gx_path_add_lines_notes n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gx_path_add_lines_notes (proc_start P_gx_path_add_lines_notes) s1 (proc_end P_gx_path_add_lines_notes) s2 ->
    (s2 V_gx_path_add_lines_notes_z <= max0(s1 V_gx_path_add_lines_notes_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gx_path_add_lines_notes.
Qed.
