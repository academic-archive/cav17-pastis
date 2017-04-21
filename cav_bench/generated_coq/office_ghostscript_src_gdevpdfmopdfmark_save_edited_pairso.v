Require Import pasta.Pasta.

Inductive proc: Type :=
  P_pdfmark_save_edited_pairs.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_pdfmark_save_edited_pairs_z := 1%positive.
Notation V_pdfmark_save_edited_pairs__tmp := 2%positive.
Notation V_pdfmark_save_edited_pairs__tmp1 := 3%positive.
Notation V_pdfmark_save_edited_pairs__tmp2 := 4%positive.
Notation V_pdfmark_save_edited_pairs_i := 5%positive.
Notation V_pdfmark_save_edited_pairs_len := 6%positive.
Notation V_pdfmark_save_edited_pairs_len1 := 7%positive.
Notation V_pdfmark_save_edited_pairs_pstr_dref_off8 := 8%positive.
Notation V_pdfmark_save_edited_pairs_size := 9%positive.
Notation V_pdfmark_save_edited_pairs_add_count := 10%positive.
Notation V_pdfmark_save_edited_pairs_add_pairs := 11%positive.
Notation V_pdfmark_save_edited_pairs_count := 12%positive.
Notation V_pdfmark_save_edited_pairs_pairs := 13%positive.
Notation V_pdfmark_save_edited_pairs_pdev := 14%positive.
Notation V_pdfmark_save_edited_pairs_pstr := 15%positive.
Notation V_pdfmark_save_edited_pairs_skip_keys := 16%positive.
Definition Pedges_pdfmark_save_edited_pairs: list (edge proc) :=
  (EA 1 (AAssign V_pdfmark_save_edited_pairs_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_pdfmark_save_edited_pairs_i) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_pdfmark_save_edited_pairs__tmp1) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_pdfmark_save_edited_pairs__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 (AAssign
  V_pdfmark_save_edited_pairs__tmp
  (Some (EVar V_pdfmark_save_edited_pairs_count))) 7)::(EA 7 (AAssign
  V_pdfmark_save_edited_pairs__tmp1
  (Some (EVar V_pdfmark_save_edited_pairs_add_count))) 8)::(EA 8 (AAssign
  V_pdfmark_save_edited_pairs_i (Some (ENum (0)))) 9)::(EA 9 (AAssign
  V_pdfmark_save_edited_pairs_size (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_pdfmark_save_edited_pairs_i) s) <
  (eval (EVar V_pdfmark_save_edited_pairs__tmp) s))%Z)) 71)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_pdfmark_save_edited_pairs_i) s) >=
  (eval (EVar V_pdfmark_save_edited_pairs__tmp) s))%Z)) 13)::
  (EA 13 AWeaken 14)::(EA 14 (AAssign V_pdfmark_save_edited_pairs_i
  (Some (ENum (0)))) 15)::(EA 15 ANone 16)::(EA 16 AWeaken 17)::
  (EA 17 (AGuard (fun s => ((eval (EVar V_pdfmark_save_edited_pairs_i) s) <
  (eval (EVar V_pdfmark_save_edited_pairs__tmp1) s))%Z)) 63)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_pdfmark_save_edited_pairs_i) s) >=
  (eval (EVar V_pdfmark_save_edited_pairs__tmp1) s))%Z)) 18)::
  (EA 18 AWeaken 19)::(EA 19 ANone 22)::(EA 19 ANone 20)::(EA 20 ANone 21)::
  (EA 21 AWeaken 24)::(EA 22 ANone 23)::(EA 23 AWeaken 24)::
  (EA 24 ANone 59)::(EA 24 ANone 25)::(EA 25 (AAssign
  V_pdfmark_save_edited_pairs_i (Some (ENum (0)))) 26)::(EA 26 ANone 27)::
  (EA 27 AWeaken 28)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_pdfmark_save_edited_pairs_i) s) <
  (eval (EVar V_pdfmark_save_edited_pairs__tmp) s))%Z)) 48)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_pdfmark_save_edited_pairs_i) s) >=
  (eval (EVar V_pdfmark_save_edited_pairs__tmp) s))%Z)) 29)::
  (EA 29 AWeaken 30)::(EA 30 (AAssign V_pdfmark_save_edited_pairs_i
  (Some (ENum (0)))) 31)::(EA 31 ANone 32)::(EA 32 AWeaken 33)::
  (EA 33 (AGuard (fun s => ((eval (EVar V_pdfmark_save_edited_pairs_i) s) <
  (eval (EVar V_pdfmark_save_edited_pairs__tmp1) s))%Z)) 39)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_pdfmark_save_edited_pairs_i) s) >=
  (eval (EVar V_pdfmark_save_edited_pairs__tmp1) s))%Z)) 34)::
  (EA 34 AWeaken 35)::(EA 35 (AAssign
  V_pdfmark_save_edited_pairs_pstr_dref_off8
  (Some (EVar V_pdfmark_save_edited_pairs_size))) 36)::(EA 36 (AAssign
  V_pdfmark_save_edited_pairs__tmp2 (Some (ENum (0)))) 37)::
  (EA 37 ANone 38)::(EA 38 AWeaken 62)::(EA 39 AWeaken 40)::(EA 40 (AAssign
  V_pdfmark_save_edited_pairs_len1 None) 41)::(EA 41 (AAssign
  V_pdfmark_save_edited_pairs_len1 None) 42)::(EA 42 ANone 43)::
  (EA 43 (AAssign V_pdfmark_save_edited_pairs_i
  (Some (EAdd (EVar V_pdfmark_save_edited_pairs_i) (ENum (2))))) 44)::
  (EA 44 ANone 45)::(EA 45 ANone 46)::(EA 46 (AAssign
  V_pdfmark_save_edited_pairs_z (Some (EAdd (ENum (1))
  (EVar V_pdfmark_save_edited_pairs_z)))) 47)::(EA 47 AWeaken 33)::
  (EA 48 AWeaken 49)::(EA 49 ANone 53)::(EA 49 ANone 50)::(EA 50 (AAssign
  V_pdfmark_save_edited_pairs_len None) 51)::(EA 51 (AAssign
  V_pdfmark_save_edited_pairs_len None) 52)::(EA 52 ANone 53)::
  (EA 53 ANone 54)::(EA 54 (AAssign V_pdfmark_save_edited_pairs_i
  (Some (EAdd (EVar V_pdfmark_save_edited_pairs_i) (ENum (2))))) 55)::
  (EA 55 ANone 56)::(EA 56 ANone 57)::(EA 57 (AAssign
  V_pdfmark_save_edited_pairs_z (Some (EAdd (ENum (1))
  (EVar V_pdfmark_save_edited_pairs_z)))) 58)::(EA 58 AWeaken 28)::
  (EA 59 (AAssign V_pdfmark_save_edited_pairs__tmp2
  (Some (ENum (-25)))) 60)::(EA 60 ANone 61)::(EA 61 AWeaken 62)::
  (EA 63 AWeaken 64)::(EA 64 (AAssign V_pdfmark_save_edited_pairs_size
  None) 65)::(EA 65 ANone 66)::(EA 66 (AAssign V_pdfmark_save_edited_pairs_i
  (Some (EAdd (EVar V_pdfmark_save_edited_pairs_i) (ENum (2))))) 67)::
  (EA 67 ANone 68)::(EA 68 ANone 69)::(EA 69 (AAssign
  V_pdfmark_save_edited_pairs_z (Some (EAdd (ENum (1))
  (EVar V_pdfmark_save_edited_pairs_z)))) 70)::(EA 70 AWeaken 17)::
  (EA 71 AWeaken 72)::(EA 72 ANone 75)::(EA 72 ANone 73)::(EA 73 (AAssign
  V_pdfmark_save_edited_pairs_size None) 74)::(EA 74 ANone 75)::
  (EA 75 ANone 76)::(EA 76 (AAssign V_pdfmark_save_edited_pairs_i
  (Some (EAdd (EVar V_pdfmark_save_edited_pairs_i) (ENum (2))))) 77)::
  (EA 77 ANone 78)::(EA 78 ANone 79)::(EA 79 (AAssign
  V_pdfmark_save_edited_pairs_z (Some (EAdd (ENum (1))
  (EVar V_pdfmark_save_edited_pairs_z)))) 80)::(EA 80 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_pdfmark_save_edited_pairs => Pedges_pdfmark_save_edited_pairs
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_pdfmark_save_edited_pairs => 62
     end)%positive;
  var_global := var_global
}.

Definition ai_pdfmark_save_edited_pairs (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 3 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 4 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1 <= 0)%Z
   | 5 => (-1 * s V_pdfmark_save_edited_pairs__tmp1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp <= 0)%Z
   | 6 => (-1 * s V_pdfmark_save_edited_pairs__tmp <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1 <= 0)%Z
   | 7 => (-1 * s V_pdfmark_save_edited_pairs__tmp1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 8 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 9 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 10 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_size <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_size <= 0)%Z
   | 11 => (-1 * s V_pdfmark_save_edited_pairs_size <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_size <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 12 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 13 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp+ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 14 => (1 * s V_pdfmark_save_edited_pairs__tmp+ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 15 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 16 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 17 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 18 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 19 => (1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 20 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 21 => (1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 22 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 23 => (1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 24 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 25 => (1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 26 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 27 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 28 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 29 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp+ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 30 => (1 * s V_pdfmark_save_edited_pairs__tmp+ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 31 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 32 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ 1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 33 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 34 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 35 => (1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 36 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 37 => (1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp2 <= 0)%Z
   | 38 => (-1 * s V_pdfmark_save_edited_pairs__tmp2 <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 39 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0)%Z
   | 40 => (-1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 41 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0)%Z
   | 42 => (-1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 43 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0)%Z
   | 44 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0)%Z
   | 45 => (-1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 46 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0)%Z
   | 47 => (-1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z + 1 <= 0)%Z
   | 48 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0)%Z
   | 49 => (-1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 50 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0)%Z
   | 51 => (-1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 52 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0)%Z
   | 53 => (-1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 54 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0)%Z
   | 55 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0)%Z
   | 56 => (-1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 57 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0)%Z
   | 58 => (-1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z + 1 <= 0)%Z
   | 59 => (1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 60 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp2 + 25 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp2 + -25 <= 0)%Z
   | 61 => (-1 * s V_pdfmark_save_edited_pairs__tmp2 + -25 <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp2 + 25 <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 62 => (1 * s V_pdfmark_save_edited_pairs__tmp2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ 1 * s V_pdfmark_save_edited_pairs__tmp1+ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp2 + -25 <= 0)%Z
   | 63 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0)%Z
   | 64 => (-1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 65 => (-1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0)%Z
   | 66 => (-1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0)%Z
   | 67 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0)%Z
   | 68 => (-1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 69 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0)%Z
   | 70 => (-1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp1+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z + 1 <= 0)%Z
   | 71 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0)%Z
   | 72 => (-1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 73 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0)%Z
   | 74 => (-1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 75 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0)%Z
   | 76 => (-1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + 1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 77 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0)%Z
   | 78 => (-1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z <= 0)%Z
   | 79 => (-1 * s V_pdfmark_save_edited_pairs_z <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0)%Z
   | 80 => (-1 * s V_pdfmark_save_edited_pairs_i + 2 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs__tmp+ 1 * s V_pdfmark_save_edited_pairs_i + -1 <= 0 /\ -1 * s V_pdfmark_save_edited_pairs_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_pdfmark_save_edited_pairs (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(1 + s V_pdfmark_save_edited_pairs_add_count)
           + max0(1 + s V_pdfmark_save_edited_pairs_count) <= z)%Q
   | 2 => (s V_pdfmark_save_edited_pairs_z
           + max0(1 + s V_pdfmark_save_edited_pairs_add_count)
           + max0(1 + s V_pdfmark_save_edited_pairs_count) <= z)%Q
   | 3 => (s V_pdfmark_save_edited_pairs_z
           + max0(1 + s V_pdfmark_save_edited_pairs_add_count)
           + max0(1 + s V_pdfmark_save_edited_pairs_count) <= z)%Q
   | 4 => (s V_pdfmark_save_edited_pairs_z
           + max0(1 + s V_pdfmark_save_edited_pairs_add_count)
           + max0(1 + s V_pdfmark_save_edited_pairs_count) <= z)%Q
   | 5 => (s V_pdfmark_save_edited_pairs_z
           + max0(1 + s V_pdfmark_save_edited_pairs_add_count)
           + max0(1 + s V_pdfmark_save_edited_pairs_count) <= z)%Q
   | 6 => (s V_pdfmark_save_edited_pairs_z
           + max0(1 + s V_pdfmark_save_edited_pairs_add_count)
           + max0(1 + s V_pdfmark_save_edited_pairs_count) <= z)%Q
   | 7 => (s V_pdfmark_save_edited_pairs_z
           + max0(1 + s V_pdfmark_save_edited_pairs__tmp)
           + max0(1 + s V_pdfmark_save_edited_pairs_add_count) <= z)%Q
   | 8 => (s V_pdfmark_save_edited_pairs_z
           + max0(1 + s V_pdfmark_save_edited_pairs__tmp)
           + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 9 => (s V_pdfmark_save_edited_pairs_z
           + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
           + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                            - s V_pdfmark_save_edited_pairs_i)
           + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 10 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 11 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 12 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 13 => hints
     [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                               + s V_pdfmark_save_edited_pairs__tmp
                                               - s V_pdfmark_save_edited_pairs_i) (-1
                                                                    + s V_pdfmark_save_edited_pairs__tmp
                                                                    - s V_pdfmark_save_edited_pairs_i));
      (*-0.5 0*) F_max0_ge_0 (-1 + s V_pdfmark_save_edited_pairs__tmp
                              - s V_pdfmark_save_edited_pairs_i)]
     (s V_pdfmark_save_edited_pairs_z
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                       - s V_pdfmark_save_edited_pairs_i)
      + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 14 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 15 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 16 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 17 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 18 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 19 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 20 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 21 => hints
     [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                               + s V_pdfmark_save_edited_pairs__tmp1
                                               - s V_pdfmark_save_edited_pairs_i) (-1
                                                                    + s V_pdfmark_save_edited_pairs__tmp1
                                                                    - s V_pdfmark_save_edited_pairs_i));
      (*-0.5 0*) F_max0_ge_0 (-1 + s V_pdfmark_save_edited_pairs__tmp1
                              - s V_pdfmark_save_edited_pairs_i)]
     (s V_pdfmark_save_edited_pairs_z
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                       - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 22 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 23 => hints
     [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                               + s V_pdfmark_save_edited_pairs__tmp1
                                               - s V_pdfmark_save_edited_pairs_i) (-1
                                                                    + s V_pdfmark_save_edited_pairs__tmp1
                                                                    - s V_pdfmark_save_edited_pairs_i));
      (*-0.5 0*) F_max0_ge_0 (-1 + s V_pdfmark_save_edited_pairs__tmp1
                              - s V_pdfmark_save_edited_pairs_i)]
     (s V_pdfmark_save_edited_pairs_z
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                       - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 24 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 25 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 26 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 27 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 28 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 29 => hints
     [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                               + s V_pdfmark_save_edited_pairs__tmp
                                               - s V_pdfmark_save_edited_pairs_i) (-1
                                                                    + s V_pdfmark_save_edited_pairs__tmp
                                                                    - s V_pdfmark_save_edited_pairs_i));
      (*-0.5 0*) F_max0_ge_0 (-1 + s V_pdfmark_save_edited_pairs__tmp
                              - s V_pdfmark_save_edited_pairs_i)]
     (s V_pdfmark_save_edited_pairs_z
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                       - s V_pdfmark_save_edited_pairs_i)
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 30 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 31 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 32 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 33 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 34 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 35 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 36 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 37 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 38 => hints
     [(*-0.5 0*) F_max0_monotonic (F_check_ge (1
                                               + s V_pdfmark_save_edited_pairs__tmp1
                                               - s V_pdfmark_save_edited_pairs_i) (-1
                                                                    + s V_pdfmark_save_edited_pairs__tmp1
                                                                    - s V_pdfmark_save_edited_pairs_i));
      (*-0.5 0*) F_max0_ge_0 (-1 + s V_pdfmark_save_edited_pairs__tmp1
                              - s V_pdfmark_save_edited_pairs_i)]
     (s V_pdfmark_save_edited_pairs_z
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                       - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 39 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (1
                                         + s V_pdfmark_save_edited_pairs__tmp1
                                         - s V_pdfmark_save_edited_pairs_i) (2)]
     (s V_pdfmark_save_edited_pairs_z
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                       - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 40 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 41 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 42 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 43 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 44 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 45 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 46 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 47 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 48 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (1
                                         + s V_pdfmark_save_edited_pairs__tmp
                                         - s V_pdfmark_save_edited_pairs_i) (2)]
     (s V_pdfmark_save_edited_pairs_z
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                       - s V_pdfmark_save_edited_pairs_i)
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 49 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 50 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 51 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 52 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 53 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 54 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 55 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 56 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 57 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 58 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 59 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 60 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 61 => hints
     [(*-0.5 0*) F_max0_ge_0 (1 + s V_pdfmark_save_edited_pairs__tmp);
      (*-0.5 0*) F_max0_ge_0 (1 + s V_pdfmark_save_edited_pairs__tmp1)]
     (s V_pdfmark_save_edited_pairs_z
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 62 => (s V_pdfmark_save_edited_pairs_z <= z)%Q
   | 63 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (1
                                         + s V_pdfmark_save_edited_pairs__tmp1
                                         - s V_pdfmark_save_edited_pairs_i) (2)]
     (s V_pdfmark_save_edited_pairs_z
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                       - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 64 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 65 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 66 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 67 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 68 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 69 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 70 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp1
                             - s V_pdfmark_save_edited_pairs_i) <= z)%Q
   | 71 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (1
                                         + s V_pdfmark_save_edited_pairs__tmp
                                         - s V_pdfmark_save_edited_pairs_i) (2)]
     (s V_pdfmark_save_edited_pairs_z
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
      + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                       - s V_pdfmark_save_edited_pairs_i)
      + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 72 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 73 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 74 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 75 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 76 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(-1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 77 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 78 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 79 => ((1 # 1) + s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | 80 => (s V_pdfmark_save_edited_pairs_z
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp)
            + (1 # 2) * max0(1 + s V_pdfmark_save_edited_pairs__tmp
                             - s V_pdfmark_save_edited_pairs_i)
            + max0(1 + s V_pdfmark_save_edited_pairs__tmp1) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_pdfmark_save_edited_pairs =>
    [mkPA Q (fun n z s => ai_pdfmark_save_edited_pairs n s /\ annot0_pdfmark_save_edited_pairs n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_pdfmark_save_edited_pairs (proc_start P_pdfmark_save_edited_pairs) s1 (proc_end P_pdfmark_save_edited_pairs) s2 ->
    (s2 V_pdfmark_save_edited_pairs_z <= max0(1
                                              + s1 V_pdfmark_save_edited_pairs_add_count)
                                         + max0(1
                                                + s1 V_pdfmark_save_edited_pairs_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_pdfmark_save_edited_pairs.
Qed.
