Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gs_type0_adjust_matrix.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gs_type0_adjust_matrix_z := 1%positive.
Notation V_gs_type0_adjust_matrix__tmp := 2%positive.
Notation V_gs_type0_adjust_matrix_code := 3%positive.
Notation V_gs_type0_adjust_matrix_fdep_size := 4%positive.
Notation V_gs_type0_adjust_matrix_i := 5%positive.
Notation V_gs_type0_adjust_matrix_pfont_dref_off280_off56 := 6%positive.
Notation V_gs_type0_adjust_matrix_pdir := 7%positive.
Notation V_gs_type0_adjust_matrix_pfont := 8%positive.
Notation V_gs_type0_adjust_matrix_pmat := 9%positive.
Definition Pedges_gs_type0_adjust_matrix: list (edge proc) :=
  (EA 1 (AAssign V_gs_type0_adjust_matrix_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_gs_type0_adjust_matrix_i) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_gs_type0_adjust_matrix_fdep_size) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_gs_type0_adjust_matrix_fdep_size
  (Some (EVar V_gs_type0_adjust_matrix_pfont_dref_off280_off56))) 6)::
  (EA 6 (AAssign V_gs_type0_adjust_matrix_i (Some (ENum (0)))) 7)::
  (EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_gs_type0_adjust_matrix_i) s) <
  (eval (EVar V_gs_type0_adjust_matrix_fdep_size) s))%Z)) 11)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_gs_type0_adjust_matrix_i) s) >=
  (eval (EVar V_gs_type0_adjust_matrix_fdep_size) s))%Z)) 10)::
  (EA 10 AWeaken 21)::(EA 11 AWeaken 12)::(EA 12 ANone 19)::
  (EA 12 ANone 13)::(EA 13 ANone 14)::(EA 14 (AAssign
  V_gs_type0_adjust_matrix_i (Some (EAdd (EVar V_gs_type0_adjust_matrix_i)
  (ENum (1))))) 15)::(EA 15 ANone 16)::(EA 16 ANone 17)::(EA 17 (AAssign
  V_gs_type0_adjust_matrix_z (Some (EAdd (ENum (1))
  (EVar V_gs_type0_adjust_matrix_z)))) 18)::(EA 18 AWeaken 9)::
  (EA 19 ANone 20)::(EA 20 AWeaken 21)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_gs_type0_adjust_matrix_i) s) =
  (eval (EVar V_gs_type0_adjust_matrix_fdep_size) s))%Z)) 51)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_gs_type0_adjust_matrix_i) s) <>
  (eval (EVar V_gs_type0_adjust_matrix_fdep_size) s))%Z)) 22)::
  (EA 22 AWeaken 23)::(EA 23 ANone 48)::(EA 23 ANone 24)::(EA 24 ANone 25)::
  (EA 25 AWeaken 26)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_gs_type0_adjust_matrix_i) s) <
  (eval (EVar V_gs_type0_adjust_matrix_fdep_size) s))%Z)) 31)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_gs_type0_adjust_matrix_i) s) >=
  (eval (EVar V_gs_type0_adjust_matrix_fdep_size) s))%Z)) 27)::
  (EA 27 AWeaken 28)::(EA 28 (AAssign V_gs_type0_adjust_matrix__tmp
  (Some (ENum (0)))) 29)::(EA 29 ANone 30)::(EA 30 AWeaken 55)::
  (EA 31 AWeaken 32)::(EA 32 ANone 33)::(EA 32 ANone 38)::(EA 33 (AAssign
  V_gs_type0_adjust_matrix_code None) 34)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_gs_type0_adjust_matrix_code) s) < (eval (ENum (0))
  s))%Z)) 44)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_gs_type0_adjust_matrix_code) s) >=
  (eval (ENum (0)) s))%Z)) 36)::(EA 36 AWeaken 37)::(EA 37 ANone 38)::
  (EA 38 ANone 39)::(EA 39 (AAssign V_gs_type0_adjust_matrix_i
  (Some (EAdd (EVar V_gs_type0_adjust_matrix_i) (ENum (1))))) 40)::
  (EA 40 ANone 41)::(EA 41 ANone 42)::(EA 42 (AAssign
  V_gs_type0_adjust_matrix_z (Some (EAdd (ENum (1))
  (EVar V_gs_type0_adjust_matrix_z)))) 43)::(EA 43 AWeaken 26)::
  (EA 44 AWeaken 45)::(EA 45 (AAssign V_gs_type0_adjust_matrix__tmp
  (Some (EVar V_gs_type0_adjust_matrix_code))) 46)::(EA 46 ANone 47)::
  (EA 47 AWeaken 55)::(EA 48 (AAssign V_gs_type0_adjust_matrix__tmp
  (Some (ENum (-25)))) 49)::(EA 49 ANone 50)::(EA 50 AWeaken 55)::
  (EA 51 AWeaken 52)::(EA 52 (AAssign V_gs_type0_adjust_matrix__tmp
  (Some (ENum (0)))) 53)::(EA 53 ANone 54)::(EA 54 AWeaken 55)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gs_type0_adjust_matrix => Pedges_gs_type0_adjust_matrix
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gs_type0_adjust_matrix => 55
     end)%positive;
  var_global := var_global
}.

Definition ai_gs_type0_adjust_matrix (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0)%Z
   | 3 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ 1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 4 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ 1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size <= 0)%Z
   | 5 => (-1 * s V_gs_type0_adjust_matrix_fdep_size <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ 1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 6 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ 1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0)%Z
   | 7 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ 1 * s V_gs_type0_adjust_matrix_z <= 0 /\ 1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 8 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ 1 * s V_gs_type0_adjust_matrix_i <= 0 /\ 1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0)%Z
   | 9 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 10 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ 1 * s V_gs_type0_adjust_matrix_fdep_size+ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 11 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0)%Z
   | 12 => (-1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 13 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0)%Z
   | 14 => (-1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 15 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i + 1 <= 0)%Z
   | 16 => (-1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0)%Z
   | 17 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i + 1 <= 0)%Z
   | 18 => (-1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z + 1 <= 0)%Z
   | 19 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0)%Z
   | 20 => (-1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 21 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0)%Z
   | 22 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 23 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0)%Z
   | 24 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 25 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0)%Z
   | 26 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 27 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ 1 * s V_gs_type0_adjust_matrix_fdep_size+ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 28 => (1 * s V_gs_type0_adjust_matrix_fdep_size+ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 29 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ 1 * s V_gs_type0_adjust_matrix_fdep_size+ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ 1 * s V_gs_type0_adjust_matrix__tmp <= 0 /\ -1 * s V_gs_type0_adjust_matrix__tmp <= 0)%Z
   | 30 => (-1 * s V_gs_type0_adjust_matrix__tmp <= 0 /\ 1 * s V_gs_type0_adjust_matrix__tmp <= 0 /\ 1 * s V_gs_type0_adjust_matrix_fdep_size+ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 31 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0)%Z
   | 32 => (-1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 33 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0)%Z
   | 34 => (-1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 35 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0)%Z
   | 36 => (-1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_code <= 0)%Z
   | 37 => (-1 * s V_gs_type0_adjust_matrix_code <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0)%Z
   | 38 => (-1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 39 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0)%Z
   | 40 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 41 => (-1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0)%Z
   | 42 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 43 => (-1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z + 1 <= 0)%Z
   | 44 => (-1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ 1 * s V_gs_type0_adjust_matrix_code + 1 <= 0)%Z
   | 45 => (1 * s V_gs_type0_adjust_matrix_code + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0)%Z
   | 46 => (-1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ 1 * s V_gs_type0_adjust_matrix_code + 1 <= 0 /\ 1 * s V_gs_type0_adjust_matrix__tmp + 1 <= 0)%Z
   | 47 => (1 * s V_gs_type0_adjust_matrix__tmp + 1 <= 0 /\ 1 * s V_gs_type0_adjust_matrix_code + 1 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i + 1 <= 0)%Z
   | 48 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 49 => (-1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ 1 * s V_gs_type0_adjust_matrix__tmp + 25 <= 0 /\ -1 * s V_gs_type0_adjust_matrix__tmp + -25 <= 0)%Z
   | 50 => (-1 * s V_gs_type0_adjust_matrix__tmp + -25 <= 0 /\ 1 * s V_gs_type0_adjust_matrix__tmp + 25 <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 51 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i <= 0 /\ 1 * s V_gs_type0_adjust_matrix_fdep_size+ -1 * s V_gs_type0_adjust_matrix_i <= 0)%Z
   | 52 => (1 * s V_gs_type0_adjust_matrix_fdep_size+ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0)%Z
   | 53 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i <= 0 /\ 1 * s V_gs_type0_adjust_matrix_fdep_size+ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ 1 * s V_gs_type0_adjust_matrix__tmp <= 0 /\ -1 * s V_gs_type0_adjust_matrix__tmp <= 0)%Z
   | 54 => (-1 * s V_gs_type0_adjust_matrix__tmp <= 0 /\ 1 * s V_gs_type0_adjust_matrix__tmp <= 0 /\ 1 * s V_gs_type0_adjust_matrix_fdep_size+ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_fdep_size+ 1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ -1 * s V_gs_type0_adjust_matrix_z <= 0)%Z
   | 55 => (-1 * s V_gs_type0_adjust_matrix_z <= 0 /\ -1 * s V_gs_type0_adjust_matrix_i <= 0 /\ 1 * s V_gs_type0_adjust_matrix__tmp <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gs_type0_adjust_matrix (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_gs_type0_adjust_matrix_pfont_dref_off280_off56) <= z)%Q
   | 2 => (s V_gs_type0_adjust_matrix_z
           + max0(s V_gs_type0_adjust_matrix_pfont_dref_off280_off56) <= z)%Q
   | 3 => (s V_gs_type0_adjust_matrix_z
           + max0(s V_gs_type0_adjust_matrix_pfont_dref_off280_off56) <= z)%Q
   | 4 => (s V_gs_type0_adjust_matrix_z
           + max0(s V_gs_type0_adjust_matrix_pfont_dref_off280_off56) <= z)%Q
   | 5 => (s V_gs_type0_adjust_matrix_z
           + max0(s V_gs_type0_adjust_matrix_pfont_dref_off280_off56) <= z)%Q
   | 6 => (s V_gs_type0_adjust_matrix_z
           + max0(s V_gs_type0_adjust_matrix_fdep_size) <= z)%Q
   | 7 => (s V_gs_type0_adjust_matrix_z
           + max0(s V_gs_type0_adjust_matrix_fdep_size
                  - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 8 => (s V_gs_type0_adjust_matrix_z
           + max0(s V_gs_type0_adjust_matrix_fdep_size
                  - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 9 => (s V_gs_type0_adjust_matrix_z
           + max0(s V_gs_type0_adjust_matrix_fdep_size
                  - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 10 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 11 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_gs_type0_adjust_matrix_fdep_size
                                      - s V_gs_type0_adjust_matrix_i) (1)]
     (s V_gs_type0_adjust_matrix_z
      + max0(s V_gs_type0_adjust_matrix_fdep_size
             - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 12 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 13 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 14 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 15 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 16 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 17 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 18 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 19 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_type0_adjust_matrix_fdep_size
                                                               - s V_gs_type0_adjust_matrix_i) (0))) (F_max0_ge_0 (s V_gs_type0_adjust_matrix_fdep_size
                                                                    - s V_gs_type0_adjust_matrix_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_gs_type0_adjust_matrix_fdep_size
                                                   - s V_gs_type0_adjust_matrix_i)) (F_check_ge (-1
                                                                    + s V_gs_type0_adjust_matrix_fdep_size
                                                                    - s V_gs_type0_adjust_matrix_i) (0))]
     ((1 # 1) + s V_gs_type0_adjust_matrix_z
      + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
             - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 21 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 22 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 23 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 24 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 25 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 26 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 27 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 28 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 29 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 30 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_gs_type0_adjust_matrix_fdep_size
                                             - s V_gs_type0_adjust_matrix_i) (-1
                                                                    + s V_gs_type0_adjust_matrix_fdep_size
                                                                    - s V_gs_type0_adjust_matrix_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_gs_type0_adjust_matrix_fdep_size
                            - s V_gs_type0_adjust_matrix_i)]
     (s V_gs_type0_adjust_matrix_z
      + max0(s V_gs_type0_adjust_matrix_fdep_size
             - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_gs_type0_adjust_matrix_fdep_size
                                       - s V_gs_type0_adjust_matrix_i) (1)]
     (s V_gs_type0_adjust_matrix_z
      + max0(s V_gs_type0_adjust_matrix_fdep_size
             - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 32 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 33 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 34 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 35 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 36 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 37 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 38 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 39 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 40 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 41 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 42 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 43 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 44 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 45 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 46 => ((1 # 1) + s V_gs_type0_adjust_matrix_z
            + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 + s V_gs_type0_adjust_matrix_fdep_size
                            - s V_gs_type0_adjust_matrix_i)]
     ((1 # 1) + s V_gs_type0_adjust_matrix_z
      + max0(-1 + s V_gs_type0_adjust_matrix_fdep_size
             - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 48 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 49 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 50 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_gs_type0_adjust_matrix_fdep_size
                                             - s V_gs_type0_adjust_matrix_i) (-1
                                                                    + s V_gs_type0_adjust_matrix_fdep_size
                                                                    - s V_gs_type0_adjust_matrix_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_gs_type0_adjust_matrix_fdep_size
                            - s V_gs_type0_adjust_matrix_i)]
     (s V_gs_type0_adjust_matrix_z
      + max0(s V_gs_type0_adjust_matrix_fdep_size
             - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 51 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 52 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 53 => (s V_gs_type0_adjust_matrix_z
            + max0(s V_gs_type0_adjust_matrix_fdep_size
                   - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 54 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_gs_type0_adjust_matrix_fdep_size
                                             - s V_gs_type0_adjust_matrix_i) (-1
                                                                    + s V_gs_type0_adjust_matrix_fdep_size
                                                                    - s V_gs_type0_adjust_matrix_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_gs_type0_adjust_matrix_fdep_size
                            - s V_gs_type0_adjust_matrix_i)]
     (s V_gs_type0_adjust_matrix_z
      + max0(s V_gs_type0_adjust_matrix_fdep_size
             - s V_gs_type0_adjust_matrix_i) <= z)%Q
   | 55 => (s V_gs_type0_adjust_matrix_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gs_type0_adjust_matrix =>
    [mkPA Q (fun n z s => ai_gs_type0_adjust_matrix n s /\ annot0_gs_type0_adjust_matrix n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gs_type0_adjust_matrix (proc_start P_gs_type0_adjust_matrix) s1 (proc_end P_gs_type0_adjust_matrix) s2 ->
    (s2 V_gs_type0_adjust_matrix_z <= max0(s1 V_gs_type0_adjust_matrix_pfont_dref_off280_off56))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gs_type0_adjust_matrix.
Qed.
