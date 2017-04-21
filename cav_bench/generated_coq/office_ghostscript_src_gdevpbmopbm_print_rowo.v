Require Import pasta.Pasta.

Inductive proc: Type :=
  P_pbm_print_row.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_pbm_print_row_z := 1%positive.
Notation V_pbm_print_row__tmp := 2%positive.
Notation V_pbm_print_row_mask := 3%positive.
Notation V_pbm_print_row_pdev_dref_off1984 := 4%positive.
Notation V_pbm_print_row_pdev_dref_off64 := 5%positive.
Notation V_pbm_print_row_x := 6%positive.
Notation V_pbm_print_row_data := 7%positive.
Notation V_pbm_print_row_depth := 8%positive.
Notation V_pbm_print_row_pdev := 9%positive.
Notation V_pbm_print_row_pstream := 10%positive.
Definition Pedges_pbm_print_row: list (edge proc) :=
  (EA 1 (AAssign V_pbm_print_row_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_pbm_print_row_x) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_pbm_print_row_pdev_dref_off64) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_pbm_print_row__tmp (Some (EVar V_pbm_print_row_depth))) 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_pbm_print_row_pdev_dref_off1984) s) <>
  (eval (ENum (0)) s))%Z)) 34)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_pbm_print_row_pdev_dref_off1984) s) =
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 (AAssign
  V_pbm_print_row_x (Some (ENum (0)))) 10)::(EA 10 (AAssign
  V_pbm_print_row_mask (Some (ENum (128)))) 11)::(EA 11 ANone 12)::
  (EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_pbm_print_row_x) s) <
  (eval (EVar V_pbm_print_row_pdev_dref_off64) s))%Z)) 17)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_pbm_print_row_x) s) >=
  (eval (EVar V_pbm_print_row_pdev_dref_off64) s))%Z)) 14)::
  (EA 14 AWeaken 15)::(EA 15 ANone 16)::(EA 16 AWeaken 37)::
  (EA 17 AWeaken 18)::(EA 18 (AAssign V_pbm_print_row_x
  (Some (EAdd (EVar V_pbm_print_row_x) (ENum (1))))) 19)::
  (EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EAdd (EVar V_pbm_print_row_x) (ENum (1))) s) =
  (eval (EVar V_pbm_print_row_pdev_dref_off64) s))%Z)) 23)::(EA 20 (AGuard
  (fun s => ((eval (EAdd (EVar V_pbm_print_row_x) (ENum (1))) s) <>
  (eval (EVar V_pbm_print_row_pdev_dref_off64) s))%Z)) 21)::
  (EA 21 AWeaken 22)::(EA 22 ANone 25)::(EA 22 ANone 24)::
  (EA 23 AWeaken 24)::(EA 24 ANone 25)::(EA 25 (AAssign V_pbm_print_row_mask
  None) 26)::(EA 26 AWeaken 27)::(EA 27 ANone 28)::(EA 27 ANone 30)::
  (EA 28 (AAssign V_pbm_print_row_mask (Some (ENum (128)))) 29)::
  (EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 ANone 32)::(EA 32 (AAssign
  V_pbm_print_row_z (Some (EAdd (ENum (1)) (EVar V_pbm_print_row_z)))) 33)::
  (EA 33 AWeaken 13)::(EA 34 AWeaken 35)::(EA 35 ANone 36)::
  (EA 36 AWeaken 37)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_pbm_print_row => Pedges_pbm_print_row
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_pbm_print_row => 37
     end)%positive;
  var_global := var_global
}.

Definition ai_pbm_print_row (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_z <= 0)%Z
   | 3 => (-1 * s V_pbm_print_row_z <= 0 /\ 1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x <= 0)%Z
   | 4 => (-1 * s V_pbm_print_row_x <= 0 /\ 1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0)%Z
   | 5 => (-1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ 1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x <= 0)%Z
   | 6 => (-1 * s V_pbm_print_row_x <= 0 /\ 1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0)%Z
   | 7 => (-1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ 1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x <= 0)%Z
   | 8 => (-1 * s V_pbm_print_row_x <= 0 /\ 1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0)%Z
   | 9 => (-1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ 1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x <= 0)%Z
   | 10 => (1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_x <= 0)%Z
   | 11 => (-1 * s V_pbm_print_row_x <= 0 /\ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ 1 * s V_pbm_print_row_z <= 0 /\ 1 * s V_pbm_print_row_mask + -128 <= 0 /\ -1 * s V_pbm_print_row_mask + 128 <= 0)%Z
   | 12 => (-1 * s V_pbm_print_row_mask + 128 <= 0 /\ 1 * s V_pbm_print_row_mask + -128 <= 0 /\ 1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_x <= 0)%Z
   | 13 => (-1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0)%Z
   | 14 => (1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pbm_print_row_x <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off64+ -1 * s V_pbm_print_row_x <= 0)%Z
   | 15 => (1 * s V_pbm_print_row_pdev_dref_off64+ -1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0)%Z
   | 16 => (1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pbm_print_row_x <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off64+ -1 * s V_pbm_print_row_x <= 0)%Z
   | 17 => (1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x + 1 <= 0)%Z
   | 18 => (-1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0)%Z
   | 19 => (1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0)%Z
   | 20 => (-1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0)%Z
   | 21 => (1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0)%Z
   | 22 => (-1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0)%Z
   | 23 => (1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x + 1 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off64+ -1 * s V_pbm_print_row_x + -1 <= 0)%Z
   | 24 => (-1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0)%Z
   | 25 => (1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0)%Z
   | 26 => (-1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0)%Z
   | 27 => (1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0)%Z
   | 28 => (-1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0)%Z
   | 29 => (1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ 1 * s V_pbm_print_row_mask + -128 <= 0 /\ -1 * s V_pbm_print_row_mask + 128 <= 0)%Z
   | 30 => (-1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0)%Z
   | 31 => (1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0)%Z
   | 32 => (-1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ 1 * s V_pbm_print_row_pdev_dref_off1984 <= 0)%Z
   | 33 => (1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off1984 <= 0 /\ -1 * s V_pbm_print_row_x + 1 <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64+ 1 * s V_pbm_print_row_x <= 0 /\ -1 * s V_pbm_print_row_z + 1 <= 0)%Z
   | 34 => (-1 * s V_pbm_print_row_x <= 0 /\ 1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0)%Z
   | 35 => (-1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ 1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x <= 0)%Z
   | 36 => (-1 * s V_pbm_print_row_x <= 0 /\ 1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_pdev_dref_off64 <= 0)%Z
   | 37 => (-1 * s V_pbm_print_row_pdev_dref_off64 <= 0 /\ -1 * s V_pbm_print_row_z <= 0 /\ -1 * s V_pbm_print_row_x <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_pbm_print_row (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_pbm_print_row_pdev_dref_off64) <= z)%Q
   | 2 => (s V_pbm_print_row_z + max0(s V_pbm_print_row_pdev_dref_off64) <= z)%Q
   | 3 => (s V_pbm_print_row_z + max0(s V_pbm_print_row_pdev_dref_off64) <= z)%Q
   | 4 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_pbm_print_row_pdev_dref_off64) (0))) (F_max0_ge_0 (s V_pbm_print_row_pdev_dref_off64))]
     (s V_pbm_print_row_z + max0(s V_pbm_print_row_pdev_dref_off64) <= z)%Q
   | 5 => (-s V_pbm_print_row_pdev_dref_off64 + s V_pbm_print_row_z
           + (2 # 1) * max0(s V_pbm_print_row_pdev_dref_off64) <= z)%Q
   | 6 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_pbm_print_row_pdev_dref_off64)) (F_check_ge (s V_pbm_print_row_pdev_dref_off64) (0))]
     (-s V_pbm_print_row_pdev_dref_off64 + s V_pbm_print_row_z
      + (2 # 1) * max0(s V_pbm_print_row_pdev_dref_off64) <= z)%Q
   | 7 => (s V_pbm_print_row_z + max0(s V_pbm_print_row_pdev_dref_off64) <= z)%Q
   | 8 => (s V_pbm_print_row_z + max0(s V_pbm_print_row_pdev_dref_off64) <= z)%Q
   | 9 => (s V_pbm_print_row_z + max0(s V_pbm_print_row_pdev_dref_off64) <= z)%Q
   | 10 => (s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 11 => (s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 12 => (s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 13 => (s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 14 => (s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 15 => (s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_pbm_print_row_pdev_dref_off64
                                             - s V_pbm_print_row_x) (-1
                                                                    + 
                                                                    s V_pbm_print_row_pdev_dref_off64
                                                                    - 
                                                                    s V_pbm_print_row_x));
      (*-1 0*) F_max0_ge_0 (-1 + s V_pbm_print_row_pdev_dref_off64
                            - s V_pbm_print_row_x)]
     (s V_pbm_print_row_z
      + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_pbm_print_row_pdev_dref_off64
                                                   - s V_pbm_print_row_x)) (F_check_ge (s V_pbm_print_row_pdev_dref_off64
                                                                    - s V_pbm_print_row_x) (0))]
     (s V_pbm_print_row_z
      + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 18 => (s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x
            + s V_pbm_print_row_z <= z)%Q
   | 19 => ((1 # 1) + s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x
            + s V_pbm_print_row_z <= z)%Q
   | 20 => ((1 # 1) + s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x
            + s V_pbm_print_row_z <= z)%Q
   | 21 => ((1 # 1) + s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x
            + s V_pbm_print_row_z <= z)%Q
   | 22 => ((1 # 1) + s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x
            + s V_pbm_print_row_z <= z)%Q
   | 23 => ((1 # 1) + s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x
            + s V_pbm_print_row_z <= z)%Q
   | 24 => ((1 # 1) + s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x
            + s V_pbm_print_row_z <= z)%Q
   | 25 => ((1 # 1) + s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x
            + s V_pbm_print_row_z <= z)%Q
   | 26 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_pbm_print_row_pdev_dref_off64
                                                               - s V_pbm_print_row_x) (0))) (F_max0_ge_0 (s V_pbm_print_row_pdev_dref_off64
                                                                    - s V_pbm_print_row_x))]
     ((1 # 1) + s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x
      + s V_pbm_print_row_z <= z)%Q
   | 27 => ((1 # 1) + s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 28 => ((1 # 1) + s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 29 => ((1 # 1) + s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 30 => ((1 # 1) + s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 31 => ((1 # 1) + s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 32 => ((1 # 1) + s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 33 => (s V_pbm_print_row_z
            + max0(s V_pbm_print_row_pdev_dref_off64 - s V_pbm_print_row_x) <= z)%Q
   | 34 => (s V_pbm_print_row_z + max0(s V_pbm_print_row_pdev_dref_off64) <= z)%Q
   | 35 => (s V_pbm_print_row_z + max0(s V_pbm_print_row_pdev_dref_off64) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_max0_ge_0 (s V_pbm_print_row_pdev_dref_off64)]
     (s V_pbm_print_row_z + max0(s V_pbm_print_row_pdev_dref_off64) <= z)%Q
   | 37 => (s V_pbm_print_row_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_pbm_print_row =>
    [mkPA Q (fun n z s => ai_pbm_print_row n s /\ annot0_pbm_print_row n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_pbm_print_row (proc_start P_pbm_print_row) s1 (proc_end P_pbm_print_row) s2 ->
    (s2 V_pbm_print_row_z <= max0(s1 V_pbm_print_row_pdev_dref_off64))%Q.
Proof.
  prove_bound ipa admissible_ipa P_pbm_print_row.
Qed.
