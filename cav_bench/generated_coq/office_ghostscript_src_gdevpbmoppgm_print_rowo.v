Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ppgm_print_row.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ppgm_print_row_z := 1%positive.
Notation V_ppgm_print_row__tmp := 2%positive.
Notation V_ppgm_print_row__tmp1 := 3%positive.
Notation V_ppgm_print_row_b := 4%positive.
Notation V_ppgm_print_row_bpe := 5%positive.
Notation V_ppgm_print_row_eol_mask := 6%positive.
Notation V_ppgm_print_row_g := 7%positive.
Notation V_ppgm_print_row_mask := 8%positive.
Notation V_ppgm_print_row_pdev_dref_off1984 := 9%positive.
Notation V_ppgm_print_row_pdev_dref_off64 := 10%positive.
Notation V_ppgm_print_row_pixel := 11%positive.
Notation V_ppgm_print_row_r := 12%positive.
Notation V_ppgm_print_row_shift := 13%positive.
Notation V_ppgm_print_row_x := 14%positive.
Notation V_ppgm_print_row_color := 15%positive.
Notation V_ppgm_print_row_data := 16%positive.
Notation V_ppgm_print_row_depth := 17%positive.
Notation V_ppgm_print_row_pdev := 18%positive.
Notation V_ppgm_print_row_pstream := 19%positive.
Definition Pedges_ppgm_print_row: list (edge proc) :=
  (EA 1 (AAssign V_ppgm_print_row_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row_x) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row_pdev_dref_off64) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_ppgm_print_row__tmp (Some (EVar V_ppgm_print_row_depth))) 6)::
  (EA 6 (AAssign V_ppgm_print_row__tmp1
  (Some (EVar V_ppgm_print_row_color))) 7)::(EA 7 (AAssign
  V_ppgm_print_row_bpe None) 8)::(EA 8 (AAssign V_ppgm_print_row_mask
  None) 9)::(EA 9 (AAssign V_ppgm_print_row_eol_mask None) 10)::
  (EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row_pdev_dref_off1984) s) <>
  (eval (ENum (0)) s))%Z)) 13)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row_pdev_dref_off1984) s) =
  (eval (ENum (0)) s))%Z)) 12)::(EA 12 AWeaken 19)::(EA 13 AWeaken 14)::
  (EA 14 (AGuard (fun s => ((eval (EVar V_ppgm_print_row__tmp) s) =
  (eval (ENum (24)) s))%Z)) 16)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row__tmp) s) <> (eval (ENum (24))
  s))%Z)) 15)::(EA 15 AWeaken 19)::(EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row__tmp1) s) <> (eval (ENum (0))
  s))%Z)) 77)::(EA 17 (AGuard (fun s => ((eval (EVar V_ppgm_print_row__tmp1)
  s) = (eval (ENum (0)) s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 (AAssign
  V_ppgm_print_row_x (Some (ENum (0)))) 20)::(EA 20 (AAssign
  V_ppgm_print_row_shift (Some (ESub (ENum (8))
  (EVar V_ppgm_print_row__tmp)))) 21)::(EA 21 ANone 22)::(EA 22 AWeaken 23)::
  (EA 23 (AGuard (fun s => ((eval (EVar V_ppgm_print_row_x) s) <
  (eval (EVar V_ppgm_print_row_pdev_dref_off64) s))%Z)) 27)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row_x) s) >=
  (eval (EVar V_ppgm_print_row_pdev_dref_off64) s))%Z)) 24)::
  (EA 24 AWeaken 25)::(EA 25 ANone 26)::(EA 26 AWeaken 80)::
  (EA 27 AWeaken 28)::(EA 28 (AAssign V_ppgm_print_row_pixel
  (Some (ENum (0)))) 29)::(EA 29 AWeaken 30)::(EA 30 ANone 48)::
  (EA 30 ANone 40)::(EA 30 ANone 42)::(EA 30 ANone 44)::(EA 30 ANone 46)::
  (EA 30 ANone 31)::(EA 31 (AAssign V_ppgm_print_row_pixel None) 32)::
  (EA 32 (AAssign V_ppgm_print_row_shift
  (Some (ESub (EVar V_ppgm_print_row_shift)
  (EVar V_ppgm_print_row__tmp)))) 33)::(EA 33 AWeaken 34)::(EA 34 (AGuard
  (fun s => ((eval (ESub (EVar V_ppgm_print_row_shift)
  (EVar V_ppgm_print_row__tmp)) s) < (eval (ENum (0)) s))%Z)) 36)::
  (EA 34 (AGuard (fun s => ((eval (ESub (EVar V_ppgm_print_row_shift)
  (EVar V_ppgm_print_row__tmp)) s) >= (eval (ENum (0)) s))%Z)) 35)::
  (EA 35 AWeaken 39)::(EA 36 AWeaken 37)::(EA 37 (AAssign
  V_ppgm_print_row_shift (Some (EAdd (EVar V_ppgm_print_row_shift)
  (ENum (8))))) 38)::(EA 38 ANone 39)::(EA 39 ANone 48)::(EA 40 (AAssign
  V_ppgm_print_row_pixel None) 41)::(EA 41 ANone 42)::(EA 42 (AAssign
  V_ppgm_print_row_pixel None) 43)::(EA 43 ANone 44)::(EA 44 (AAssign
  V_ppgm_print_row_pixel None) 45)::(EA 45 ANone 46)::(EA 46 (AAssign
  V_ppgm_print_row_pixel None) 47)::(EA 47 ANone 48)::(EA 48 (AAssign
  V_ppgm_print_row_x (Some (EAdd (EVar V_ppgm_print_row_x)
  (ENum (1))))) 49)::(EA 49 (AAssign V_ppgm_print_row_b None) 50)::
  (EA 50 (AAssign V_ppgm_print_row_pixel None) 51)::(EA 51 (AAssign
  V_ppgm_print_row_g None) 52)::(EA 52 (AAssign V_ppgm_print_row_pixel
  None) 53)::(EA 53 (AAssign V_ppgm_print_row_r None) 54)::
  (EA 54 AWeaken 55)::(EA 55 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row_pdev_dref_off1984) s) <>
  (eval (ENum (0)) s))%Z)) 67)::(EA 55 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row_pdev_dref_off1984) s) =
  (eval (ENum (0)) s))%Z)) 56)::(EA 56 AWeaken 57)::(EA 57 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row__tmp1) s) <> (eval (ENum (0))
  s))%Z)) 59)::(EA 57 (AGuard (fun s => ((eval (EVar V_ppgm_print_row__tmp1)
  s) = (eval (ENum (0)) s))%Z)) 58)::(EA 58 AWeaken 62)::(EA 59 AWeaken 60)::
  (EA 60 ANone 61)::(EA 61 AWeaken 62)::(EA 62 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row_x) s) =
  (eval (EVar V_ppgm_print_row_pdev_dref_off64) s))%Z)) 66)::(EA 62 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row_x) s) <>
  (eval (EVar V_ppgm_print_row_pdev_dref_off64) s))%Z)) 63)::
  (EA 63 AWeaken 64)::(EA 64 ANone 65)::(EA 65 ANone 73)::
  (EA 66 AWeaken 73)::(EA 67 AWeaken 68)::(EA 68 (AGuard
  (fun s => ((eval (EVar V_ppgm_print_row__tmp1) s) <> (eval (ENum (0))
  s))%Z)) 70)::(EA 68 (AGuard (fun s => ((eval (EVar V_ppgm_print_row__tmp1)
  s) = (eval (ENum (0)) s))%Z)) 69)::(EA 69 AWeaken 72)::(EA 70 AWeaken 71)::
  (EA 71 ANone 72)::(EA 72 ANone 73)::(EA 73 ANone 74)::(EA 74 ANone 75)::
  (EA 75 (AAssign V_ppgm_print_row_z (Some (EAdd (ENum (1))
  (EVar V_ppgm_print_row_z)))) 76)::(EA 76 AWeaken 23)::(EA 77 AWeaken 78)::
  (EA 78 ANone 79)::(EA 79 AWeaken 80)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ppgm_print_row => Pedges_ppgm_print_row
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ppgm_print_row => 80
     end)%positive;
  var_global := var_global
}.

Definition ai_ppgm_print_row (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 3 => (-1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 4 => (-1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0)%Z
   | 5 => (-1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 6 => (-1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0)%Z
   | 7 => (-1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 8 => (-1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0)%Z
   | 9 => (-1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 10 => (-1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0)%Z
   | 11 => (-1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 12 => (-1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0)%Z
   | 13 => (-1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0)%Z
   | 14 => (-1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 15 => (-1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0)%Z
   | 16 => (-1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ 1 * s V_ppgm_print_row__tmp + -24 <= 0 /\ -1 * s V_ppgm_print_row__tmp + 24 <= 0)%Z
   | 17 => (-1 * s V_ppgm_print_row__tmp + 24 <= 0 /\ 1 * s V_ppgm_print_row__tmp + -24 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 18 => (-1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ 1 * s V_ppgm_print_row__tmp + -24 <= 0 /\ -1 * s V_ppgm_print_row__tmp + 24 <= 0 /\ 1 * s V_ppgm_print_row__tmp1 <= 0 /\ -1 * s V_ppgm_print_row__tmp1 <= 0)%Z
   | 19 => (-1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 20 => (1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 21 => (-1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_z <= 0)%Z
   | 22 => (1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 23 => (-1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 24 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off64+ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 25 => (1 * s V_ppgm_print_row_pdev_dref_off64+ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 26 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off64+ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 27 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0)%Z
   | 28 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 29 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ 1 * s V_ppgm_print_row_pixel <= 0 /\ -1 * s V_ppgm_print_row_pixel <= 0)%Z
   | 30 => (-1 * s V_ppgm_print_row_pixel <= 0 /\ 1 * s V_ppgm_print_row_pixel <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 31 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ 1 * s V_ppgm_print_row_pixel <= 0 /\ -1 * s V_ppgm_print_row_pixel <= 0)%Z
   | 32 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 33 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0)%Z
   | 34 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 35 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ 1 * s V_ppgm_print_row__tmp+ -1 * s V_ppgm_print_row_shift <= 0)%Z
   | 36 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row__tmp+ 1 * s V_ppgm_print_row_shift + 1 <= 0)%Z
   | 37 => (-1 * s V_ppgm_print_row__tmp+ 1 * s V_ppgm_print_row_shift + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 38 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row__tmp+ 1 * s V_ppgm_print_row_shift + -7 <= 0)%Z
   | 39 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 40 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ 1 * s V_ppgm_print_row_pixel <= 0 /\ -1 * s V_ppgm_print_row_pixel <= 0)%Z
   | 41 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 42 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0)%Z
   | 43 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 44 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0)%Z
   | 45 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 46 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0)%Z
   | 47 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 48 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0)%Z
   | 49 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 50 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 51 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 52 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 53 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 54 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 55 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 56 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0)%Z
   | 57 => (-1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 58 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_ppgm_print_row__tmp1 <= 0 /\ -1 * s V_ppgm_print_row__tmp1 <= 0)%Z
   | 59 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0)%Z
   | 60 => (-1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 61 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0)%Z
   | 62 => (-1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 63 => (-1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0)%Z
   | 64 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0)%Z
   | 65 => (-1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x + 1 <= 0)%Z
   | 66 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_ppgm_print_row_pdev_dref_off64+ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 67 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 68 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 69 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row__tmp1 <= 0 /\ -1 * s V_ppgm_print_row__tmp1 <= 0)%Z
   | 70 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 71 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 72 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 73 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 74 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0)%Z
   | 75 => (-1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0)%Z
   | 76 => (-1 * s V_ppgm_print_row_pdev_dref_off64+ 1 * s V_ppgm_print_row_x <= 0 /\ -1 * s V_ppgm_print_row_x + 1 <= 0 /\ -1 * s V_ppgm_print_row_z + 1 <= 0)%Z
   | 77 => (-1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ 1 * s V_ppgm_print_row__tmp + -24 <= 0 /\ -1 * s V_ppgm_print_row__tmp + 24 <= 0)%Z
   | 78 => (-1 * s V_ppgm_print_row__tmp + 24 <= 0 /\ 1 * s V_ppgm_print_row__tmp + -24 <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0)%Z
   | 79 => (-1 * s V_ppgm_print_row_x <= 0 /\ 1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ 1 * s V_ppgm_print_row__tmp + -24 <= 0 /\ -1 * s V_ppgm_print_row__tmp + 24 <= 0)%Z
   | 80 => (-1 * s V_ppgm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_ppgm_print_row_z <= 0 /\ -1 * s V_ppgm_print_row_x <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ppgm_print_row (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_ppgm_print_row_pdev_dref_off64) <= z)%Q
   | 2 => (s V_ppgm_print_row_z + max0(s V_ppgm_print_row_pdev_dref_off64) <= z)%Q
   | 3 => (s V_ppgm_print_row_z + max0(s V_ppgm_print_row_pdev_dref_off64) <= z)%Q
   | 4 => (s V_ppgm_print_row_z + max0(s V_ppgm_print_row_pdev_dref_off64) <= z)%Q
   | 5 => (s V_ppgm_print_row_z + max0(s V_ppgm_print_row_pdev_dref_off64) <= z)%Q
   | 6 => (s V_ppgm_print_row_z + max0(s V_ppgm_print_row_pdev_dref_off64) <= z)%Q
   | 7 => (s V_ppgm_print_row_z + max0(s V_ppgm_print_row_pdev_dref_off64) <= z)%Q
   | 8 => (s V_ppgm_print_row_z + max0(s V_ppgm_print_row_pdev_dref_off64) <= z)%Q
   | 9 => (s V_ppgm_print_row_z + max0(s V_ppgm_print_row_pdev_dref_off64) <= z)%Q
   | 10 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_ppgm_print_row_pdev_dref_off64)) (F_check_ge (s V_ppgm_print_row_pdev_dref_off64) (0))]
     (s V_ppgm_print_row_z + max0(s V_ppgm_print_row_pdev_dref_off64) <= z)%Q
   | 11 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z <= z)%Q
   | 12 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z <= z)%Q
   | 13 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z <= z)%Q
   | 14 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z <= z)%Q
   | 15 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z <= z)%Q
   | 16 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z <= z)%Q
   | 17 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z <= z)%Q
   | 18 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z <= z)%Q
   | 19 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z <= z)%Q
   | 20 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
            - max0(s V_ppgm_print_row_pdev_dref_off64)
            + max0(s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x) <= z)%Q
   | 21 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
            - max0(s V_ppgm_print_row_pdev_dref_off64)
            + max0(s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ppgm_print_row_z) (0))) (F_max0_ge_0 (s V_ppgm_print_row_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ppgm_print_row_pdev_dref_off64) (0))) (F_max0_ge_0 (s V_ppgm_print_row_pdev_dref_off64))]
     (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
      - max0(s V_ppgm_print_row_pdev_dref_off64)
      + max0(s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x) <= z)%Q
   | 23 => (max0(s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x)
            + max0(s V_ppgm_print_row_z) <= z)%Q
   | 24 => (max0(s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x)
            + max0(s V_ppgm_print_row_z) <= z)%Q
   | 25 => (max0(s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x)
            + max0(s V_ppgm_print_row_z) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_ppgm_print_row_pdev_dref_off64
                                             - s V_ppgm_print_row_x) (-1
                                                                    + s V_ppgm_print_row_pdev_dref_off64
                                                                    - s V_ppgm_print_row_x));
      (*-1 0*) F_max0_ge_0 (-1 + s V_ppgm_print_row_pdev_dref_off64
                            - s V_ppgm_print_row_x);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_ppgm_print_row_z)) (F_check_ge (s V_ppgm_print_row_z) (0))]
     (max0(s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x)
      + max0(s V_ppgm_print_row_z) <= z)%Q
   | 27 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_ppgm_print_row_pdev_dref_off64
                                                  - s V_ppgm_print_row_x)) (F_check_ge (s V_ppgm_print_row_pdev_dref_off64
                                                                    - s V_ppgm_print_row_x) (0))]
     (max0(s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x)
      + max0(s V_ppgm_print_row_z) <= z)%Q
   | 28 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + max0(s V_ppgm_print_row_z) <= z)%Q
   | 29 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_ppgm_print_row_z)) (F_check_ge (s V_ppgm_print_row_z) (0))]
     (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
      + max0(s V_ppgm_print_row_z) <= z)%Q
   | 30 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 31 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 32 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 33 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 34 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 35 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 36 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 37 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 38 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 39 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 40 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 41 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 42 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 43 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 44 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 45 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 46 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 47 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 48 => (s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
            + s V_ppgm_print_row_z <= z)%Q
   | 49 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 50 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 51 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 52 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 53 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 54 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 55 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 56 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 57 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 58 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_ppgm_print_row_x)) (F_check_ge (-1
                                                                    + s V_ppgm_print_row_x) (0))]
     ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
      + s V_ppgm_print_row_z <= z)%Q
   | 59 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 60 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 61 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_ppgm_print_row_x)) (F_check_ge (-1
                                                                    + s V_ppgm_print_row_x) (0))]
     ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
      + s V_ppgm_print_row_z <= z)%Q
   | 62 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
            - max0(-1 + s V_ppgm_print_row_x) <= z)%Q
   | 63 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
            - max0(-1 + s V_ppgm_print_row_x) <= z)%Q
   | 64 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
            - max0(-1 + s V_ppgm_print_row_x) <= z)%Q
   | 65 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
            - max0(-1 + s V_ppgm_print_row_x) <= z)%Q
   | 66 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
            - max0(-1 + s V_ppgm_print_row_x) <= z)%Q
   | 67 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 68 => ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64
            - s V_ppgm_print_row_x + s V_ppgm_print_row_z <= z)%Q
   | 69 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_ppgm_print_row_x)) (F_check_ge (-1
                                                                    + s V_ppgm_print_row_x) (0))]
     ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
      + s V_ppgm_print_row_z <= z)%Q
   | 70 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_ppgm_print_row_x)) (F_check_ge (-1
                                                                    + s V_ppgm_print_row_x) (0))]
     ((1 # 1) + s V_ppgm_print_row_pdev_dref_off64 - s V_ppgm_print_row_x
      + s V_ppgm_print_row_z <= z)%Q
   | 71 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
            - max0(-1 + s V_ppgm_print_row_x) <= z)%Q
   | 72 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
            - max0(-1 + s V_ppgm_print_row_x) <= z)%Q
   | 73 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
            - max0(-1 + s V_ppgm_print_row_x) <= z)%Q
   | 74 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
            - max0(-1 + s V_ppgm_print_row_x) <= z)%Q
   | 75 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
            - max0(-1 + s V_ppgm_print_row_x) <= z)%Q
   | 76 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ppgm_print_row_z) (0))) (F_max0_ge_0 (s V_ppgm_print_row_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ppgm_print_row_pdev_dref_off64
                                                               - s V_ppgm_print_row_x) (0))) (F_max0_ge_0 (s V_ppgm_print_row_pdev_dref_off64
                                                                    - s V_ppgm_print_row_x));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_ppgm_print_row_x) (0))) (F_max0_ge_0 (-1
                                                                    + s V_ppgm_print_row_x))]
     (-(1 # 1) + s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z
      - max0(-1 + s V_ppgm_print_row_x) <= z)%Q
   | 77 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z <= z)%Q
   | 78 => (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z <= z)%Q
   | 79 => hints
     [(*-1 0*) F_max0_ge_0 (s V_ppgm_print_row_pdev_dref_off64);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ppgm_print_row_pdev_dref_off64) (0))) (F_max0_ge_0 (s V_ppgm_print_row_pdev_dref_off64))]
     (s V_ppgm_print_row_pdev_dref_off64 + s V_ppgm_print_row_z <= z)%Q
   | 80 => (s V_ppgm_print_row_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ppgm_print_row =>
    [mkPA Q (fun n z s => ai_ppgm_print_row n s /\ annot0_ppgm_print_row n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ppgm_print_row (proc_start P_ppgm_print_row) s1 (proc_end P_ppgm_print_row) s2 ->
    (s2 V_ppgm_print_row_z <= max0(s1 V_ppgm_print_row_pdev_dref_off64))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ppgm_print_row.
Qed.
