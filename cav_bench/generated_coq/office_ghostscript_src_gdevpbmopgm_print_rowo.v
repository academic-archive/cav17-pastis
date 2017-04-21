Require Import pasta.Pasta.

Inductive proc: Type :=
  P_pgm_print_row.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_pgm_print_row_z := 1%positive.
Notation V_pgm_print_row__tmp := 2%positive.
Notation V_pgm_print_row_mask := 3%positive.
Notation V_pgm_print_row_pdev_dref_off1984 := 4%positive.
Notation V_pgm_print_row_pdev_dref_off64 := 5%positive.
Notation V_pgm_print_row_pixel := 6%positive.
Notation V_pgm_print_row_shift := 7%positive.
Notation V_pgm_print_row_x := 8%positive.
Notation V_pgm_print_row_data := 9%positive.
Notation V_pgm_print_row_depth := 10%positive.
Notation V_pgm_print_row_pdev := 11%positive.
Notation V_pgm_print_row_pstream := 12%positive.
Definition Pedges_pgm_print_row: list (edge proc) :=
  (EA 1 (AAssign V_pgm_print_row_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_pgm_print_row_x) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_pgm_print_row_pdev_dref_off64) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_pgm_print_row__tmp (Some (EVar V_pgm_print_row_depth))) 6)::
  (EA 6 (AAssign V_pgm_print_row_mask None) 7)::(EA 7 AWeaken 8)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_pgm_print_row_pdev_dref_off1984)
  s) <> (eval (ENum (0)) s))%Z)) 10)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_pgm_print_row_pdev_dref_off1984) s) =
  (eval (ENum (0)) s))%Z)) 9)::(EA 9 AWeaken 13)::(EA 10 AWeaken 11)::
  (EA 11 (AGuard (fun s => ((eval (EVar V_pgm_print_row__tmp) s) =
  (eval (ENum (8)) s))%Z)) 51)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_pgm_print_row__tmp) s) <> (eval (ENum (8))
  s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 (AAssign V_pgm_print_row_x
  (Some (ENum (0)))) 14)::(EA 14 (AAssign V_pgm_print_row_shift
  (Some (ESub (ENum (8)) (EVar V_pgm_print_row__tmp)))) 15)::
  (EA 15 ANone 16)::(EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_pgm_print_row_x) s) <
  (eval (EVar V_pgm_print_row_pdev_dref_off64) s))%Z)) 21)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_pgm_print_row_x) s) >=
  (eval (EVar V_pgm_print_row_pdev_dref_off64) s))%Z)) 18)::
  (EA 18 AWeaken 19)::(EA 19 ANone 20)::(EA 20 AWeaken 54)::
  (EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_pgm_print_row_shift) s) < (eval (ENum (0))
  s))%Z)) 33)::(EA 22 (AGuard (fun s => ((eval (EVar V_pgm_print_row_shift)
  s) >= (eval (ENum (0)) s))%Z)) 23)::(EA 23 AWeaken 24)::(EA 24 (AAssign
  V_pgm_print_row_pixel None) 25)::(EA 25 (AAssign V_pgm_print_row_shift
  (Some (ESub (EVar V_pgm_print_row_shift)
  (EVar V_pgm_print_row__tmp)))) 26)::(EA 26 AWeaken 27)::(EA 27 (AGuard
  (fun s => ((eval (ESub (EVar V_pgm_print_row_shift)
  (EVar V_pgm_print_row__tmp)) s) < (eval (ENum (0)) s))%Z)) 29)::
  (EA 27 (AGuard (fun s => ((eval (ESub (EVar V_pgm_print_row_shift)
  (EVar V_pgm_print_row__tmp)) s) >= (eval (ENum (0)) s))%Z)) 28)::
  (EA 28 AWeaken 32)::(EA 29 AWeaken 30)::(EA 30 (AAssign
  V_pgm_print_row_shift (Some (EAdd (EVar V_pgm_print_row_shift)
  (ENum (8))))) 31)::(EA 31 ANone 32)::(EA 32 ANone 36)::(EA 33 AWeaken 34)::
  (EA 34 (AAssign V_pgm_print_row_pixel None) 35)::(EA 35 ANone 36)::
  (EA 36 (AAssign V_pgm_print_row_x (Some (EAdd (EVar V_pgm_print_row_x)
  (ENum (1))))) 37)::(EA 37 AWeaken 38)::(EA 38 (AGuard
  (fun s => ((eval (EVar V_pgm_print_row_pdev_dref_off1984) s) <>
  (eval (ENum (0)) s))%Z)) 45)::(EA 38 (AGuard
  (fun s => ((eval (EVar V_pgm_print_row_pdev_dref_off1984) s) =
  (eval (ENum (0)) s))%Z)) 39)::(EA 39 AWeaken 40)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_pgm_print_row_x) s) =
  (eval (EVar V_pgm_print_row_pdev_dref_off64) s))%Z)) 44)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_pgm_print_row_x) s) <>
  (eval (EVar V_pgm_print_row_pdev_dref_off64) s))%Z)) 41)::
  (EA 41 AWeaken 42)::(EA 42 ANone 43)::(EA 43 ANone 47)::
  (EA 44 AWeaken 47)::(EA 45 AWeaken 46)::(EA 46 ANone 47)::
  (EA 47 ANone 48)::(EA 48 ANone 49)::(EA 49 (AAssign V_pgm_print_row_z
  (Some (EAdd (ENum (1)) (EVar V_pgm_print_row_z)))) 50)::
  (EA 50 AWeaken 17)::(EA 51 AWeaken 52)::(EA 52 ANone 53)::
  (EA 53 AWeaken 54)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_pgm_print_row => Pedges_pgm_print_row
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_pgm_print_row => 54
     end)%positive;
  var_global := var_global
}.

Definition ai_pgm_print_row (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_z <= 0)%Z
   | 3 => (-1 * s V_pgm_print_row_z <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0)%Z
   | 4 => (-1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64 <= 0)%Z
   | 5 => (-1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0)%Z
   | 6 => (-1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64 <= 0)%Z
   | 7 => (-1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0)%Z
   | 8 => (-1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64 <= 0)%Z
   | 9 => (-1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off1984 <= 0)%Z
   | 10 => (-1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0)%Z
   | 11 => (-1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64 <= 0)%Z
   | 12 => (-1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0)%Z
   | 13 => (-1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64 <= 0)%Z
   | 14 => (-1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_x <= 0)%Z
   | 15 => (-1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64 <= 0)%Z
   | 16 => (-1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_x <= 0)%Z
   | 17 => (-1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0)%Z
   | 18 => (-1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row_pdev_dref_off64+ -1 * s V_pgm_print_row_x <= 0)%Z
   | 19 => (1 * s V_pgm_print_row_pdev_dref_off64+ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0)%Z
   | 20 => (-1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row_pdev_dref_off64+ -1 * s V_pgm_print_row_x <= 0)%Z
   | 21 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0)%Z
   | 22 => (-1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0)%Z
   | 23 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_shift <= 0)%Z
   | 24 => (-1 * s V_pgm_print_row_shift <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0)%Z
   | 25 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_shift <= 0)%Z
   | 26 => (-1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row__tmp+ -1 * s V_pgm_print_row_shift <= 0)%Z
   | 27 => (-1 * s V_pgm_print_row__tmp+ -1 * s V_pgm_print_row_shift <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0)%Z
   | 28 => (-1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row__tmp+ -1 * s V_pgm_print_row_shift <= 0 /\ 1 * s V_pgm_print_row__tmp+ -1 * s V_pgm_print_row_shift <= 0)%Z
   | 29 => (-1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row__tmp+ -1 * s V_pgm_print_row_shift <= 0 /\ -1 * s V_pgm_print_row__tmp+ 1 * s V_pgm_print_row_shift + 1 <= 0)%Z
   | 30 => (-1 * s V_pgm_print_row__tmp+ 1 * s V_pgm_print_row_shift + 1 <= 0 /\ -1 * s V_pgm_print_row__tmp+ -1 * s V_pgm_print_row_shift <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0)%Z
   | 31 => (-1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row__tmp+ 1 * s V_pgm_print_row_shift + -7 <= 0 /\ -1 * s V_pgm_print_row__tmp+ -1 * s V_pgm_print_row_shift + 8 <= 0)%Z
   | 32 => (-1 * s V_pgm_print_row__tmp+ -1 * s V_pgm_print_row_shift <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0)%Z
   | 33 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ 1 * s V_pgm_print_row_shift + 1 <= 0)%Z
   | 34 => (1 * s V_pgm_print_row_shift + 1 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0)%Z
   | 35 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ 1 * s V_pgm_print_row_shift + 1 <= 0)%Z
   | 36 => (-1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0)%Z
   | 37 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_x + 1 <= 0)%Z
   | 38 => (-1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0)%Z
   | 39 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_x + 1 <= 0 /\ 1 * s V_pgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off1984 <= 0)%Z
   | 40 => (-1 * s V_pgm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0)%Z
   | 41 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x + 1 <= 0 /\ 1 * s V_pgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0)%Z
   | 42 => (-1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_z <= 0)%Z
   | 43 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x + 1 <= 0 /\ 1 * s V_pgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x + 1 <= 0)%Z
   | 44 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_x + 1 <= 0 /\ 1 * s V_pgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pgm_print_row_pdev_dref_off64+ -1 * s V_pgm_print_row_x <= 0)%Z
   | 45 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_x + 1 <= 0)%Z
   | 46 => (-1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0)%Z
   | 47 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_x + 1 <= 0)%Z
   | 48 => (-1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0)%Z
   | 49 => (-1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_x + 1 <= 0)%Z
   | 50 => (-1 * s V_pgm_print_row_x + 1 <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64+ 1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z + 1 <= 0)%Z
   | 51 => (-1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row__tmp + -8 <= 0 /\ -1 * s V_pgm_print_row__tmp + 8 <= 0)%Z
   | 52 => (-1 * s V_pgm_print_row__tmp + 8 <= 0 /\ 1 * s V_pgm_print_row__tmp + -8 <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64 <= 0)%Z
   | 53 => (-1 * s V_pgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ 1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_x <= 0 /\ 1 * s V_pgm_print_row__tmp + -8 <= 0 /\ -1 * s V_pgm_print_row__tmp + 8 <= 0)%Z
   | 54 => (-1 * s V_pgm_print_row_x <= 0 /\ -1 * s V_pgm_print_row_z <= 0 /\ -1 * s V_pgm_print_row_pdev_dref_off64 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_pgm_print_row (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_pgm_print_row_pdev_dref_off64) <= z)%Q
   | 2 => (s V_pgm_print_row_z + max0(s V_pgm_print_row_pdev_dref_off64) <= z)%Q
   | 3 => (s V_pgm_print_row_z + max0(s V_pgm_print_row_pdev_dref_off64) <= z)%Q
   | 4 => (s V_pgm_print_row_z + max0(s V_pgm_print_row_pdev_dref_off64) <= z)%Q
   | 5 => (s V_pgm_print_row_z + max0(s V_pgm_print_row_pdev_dref_off64) <= z)%Q
   | 6 => (s V_pgm_print_row_z + max0(s V_pgm_print_row_pdev_dref_off64) <= z)%Q
   | 7 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_pgm_print_row_pdev_dref_off64)) (F_check_ge (s V_pgm_print_row_pdev_dref_off64) (0))]
     (s V_pgm_print_row_z + max0(s V_pgm_print_row_pdev_dref_off64) <= z)%Q
   | 8 => (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z <= z)%Q
   | 9 => (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z <= z)%Q
   | 10 => (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z <= z)%Q
   | 11 => (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z <= z)%Q
   | 12 => (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z <= z)%Q
   | 13 => (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z <= z)%Q
   | 14 => (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x) <= z)%Q
   | 15 => (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_pgm_print_row_z) (0))) (F_max0_ge_0 (s V_pgm_print_row_z))]
     (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z
      - max0(s V_pgm_print_row_pdev_dref_off64)
      + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x) <= z)%Q
   | 17 => (s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 18 => (s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 19 => (s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_pgm_print_row_pdev_dref_off64
                                             - s V_pgm_print_row_x) (-1
                                                                    + 
                                                                    s V_pgm_print_row_pdev_dref_off64
                                                                    - 
                                                                    s V_pgm_print_row_x));
      (*-1 0*) F_max0_ge_0 (-1 + s V_pgm_print_row_pdev_dref_off64
                            - s V_pgm_print_row_x);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_pgm_print_row_pdev_dref_off64) (0))) (F_max0_ge_0 (s V_pgm_print_row_pdev_dref_off64));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_pgm_print_row_z)) (F_check_ge (s V_pgm_print_row_z) (0))]
     (s V_pgm_print_row_pdev_dref_off64
      - max0(s V_pgm_print_row_pdev_dref_off64)
      + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
      + max0(s V_pgm_print_row_z) <= z)%Q
   | 21 => (s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 22 => (s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 23 => (s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 24 => (s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 25 => (s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 26 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_pgm_print_row_z)) (F_check_ge (s V_pgm_print_row_z) (0))]
     (s V_pgm_print_row_pdev_dref_off64
      - max0(s V_pgm_print_row_pdev_dref_off64)
      + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
      + max0(s V_pgm_print_row_z) <= z)%Q
   | 27 => (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x) <= z)%Q
   | 28 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_pgm_print_row_pdev_dref_off64
                                       - s V_pgm_print_row_x) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_pgm_print_row_z) (0))) (F_max0_ge_0 (s V_pgm_print_row_z))]
     (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z
      - max0(s V_pgm_print_row_pdev_dref_off64)
      + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x) <= z)%Q
   | 29 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_pgm_print_row_pdev_dref_off64
                                       - s V_pgm_print_row_x) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_pgm_print_row_z) (0))) (F_max0_ge_0 (s V_pgm_print_row_z))]
     (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z
      - max0(s V_pgm_print_row_pdev_dref_off64)
      + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x) <= z)%Q
   | 30 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            + max0(-1 + s V_pgm_print_row_pdev_dref_off64
                   - s V_pgm_print_row_x)
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 31 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            + max0(-1 + s V_pgm_print_row_pdev_dref_off64
                   - s V_pgm_print_row_x)
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 32 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            + max0(-1 + s V_pgm_print_row_pdev_dref_off64
                   - s V_pgm_print_row_x)
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_pgm_print_row_pdev_dref_off64
                                       - s V_pgm_print_row_x) (1)]
     (s V_pgm_print_row_pdev_dref_off64
      - max0(s V_pgm_print_row_pdev_dref_off64)
      + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
      + max0(s V_pgm_print_row_z) <= z)%Q
   | 34 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            + max0(-1 + s V_pgm_print_row_pdev_dref_off64
                   - s V_pgm_print_row_x)
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 35 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            + max0(-1 + s V_pgm_print_row_pdev_dref_off64
                   - s V_pgm_print_row_x)
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 36 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            + max0(-1 + s V_pgm_print_row_pdev_dref_off64
                   - s V_pgm_print_row_x)
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 37 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 38 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 39 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 40 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 41 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 42 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 43 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 44 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 45 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 46 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 47 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 48 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 49 => ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
            - max0(s V_pgm_print_row_pdev_dref_off64)
            + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x)
            + max0(s V_pgm_print_row_z) <= z)%Q
   | 50 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_pgm_print_row_z)) (F_check_ge (-1
                                                                    + s V_pgm_print_row_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_pgm_print_row_z) (0))) (F_max0_ge_0 (s V_pgm_print_row_z))]
     ((1 # 1) + s V_pgm_print_row_pdev_dref_off64
      + max0(-1 + s V_pgm_print_row_z)
      - max0(s V_pgm_print_row_pdev_dref_off64)
      + max0(s V_pgm_print_row_pdev_dref_off64 - s V_pgm_print_row_x) <= z)%Q
   | 51 => (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z <= z)%Q
   | 52 => (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z <= z)%Q
   | 53 => hints
     [(*-1 0*) F_max0_ge_0 (s V_pgm_print_row_pdev_dref_off64);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_pgm_print_row_pdev_dref_off64) (0))) (F_max0_ge_0 (s V_pgm_print_row_pdev_dref_off64))]
     (s V_pgm_print_row_pdev_dref_off64 + s V_pgm_print_row_z <= z)%Q
   | 54 => (s V_pgm_print_row_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_pgm_print_row =>
    [mkPA Q (fun n z s => ai_pgm_print_row n s /\ annot0_pgm_print_row n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_pgm_print_row (proc_start P_pgm_print_row) s1 (proc_end P_pgm_print_row) s2 ->
    (s2 V_pgm_print_row_z <= max0(s1 V_pgm_print_row_pdev_dref_off64))%Q.
Proof.
  prove_bound ipa admissible_ipa P_pgm_print_row.
Qed.
