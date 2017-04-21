Require Import pasta.Pasta.

Inductive proc: Type :=
  P_compute_snaps.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_compute_snaps_z := 1%positive.
Notation V_compute_snaps__tmp := 2%positive.
Notation V_compute_snaps__tmp1 := 3%positive.
Notation V_compute_snaps_code := 4%positive.
Notation V_compute_snaps_i := 5%positive.
Notation V_compute_snaps_j := 6%positive.
Notation V_compute_snaps_psst_dref_off0 := 7%positive.
Notation V_compute_snaps_pst_dref_off0 := 8%positive.
Notation V_compute_snaps_from_y := 9%positive.
Notation V_compute_snaps_pmat := 10%positive.
Notation V_compute_snaps_psst := 11%positive.
Notation V_compute_snaps_pst := 12%positive.
Notation V_compute_snaps_tname := 13%positive.
Notation V_compute_snaps_to_y := 14%positive.
Definition Pedges_compute_snaps: list (edge proc) :=
  (EA 1 (AAssign V_compute_snaps_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_compute_snaps__tmp1 (Some (EVar V_compute_snaps_from_y))) 3)::
  (EA 3 (AAssign V_compute_snaps__tmp
  (Some (EVar V_compute_snaps_to_y))) 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_compute_snaps__tmp) s) <> (eval (ENum (0))
  s))%Z)) 8)::(EA 5 (AGuard (fun s => ((eval (EVar V_compute_snaps__tmp) s) =
  (eval (ENum (0)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 ANone 10)::
  (EA 8 AWeaken 9)::(EA 9 ANone 10)::(EA 10 (AAssign V_compute_snaps_j
  (Some (EVar V_compute_snaps_psst_dref_off0))) 11)::(EA 11 (AAssign
  V_compute_snaps_i (Some (ENum (0)))) 12)::(EA 12 ANone 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_compute_snaps_i) s) <
  (eval (EVar V_compute_snaps_pst_dref_off0) s))%Z)) 19)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_compute_snaps_i) s) >=
  (eval (EVar V_compute_snaps_pst_dref_off0) s))%Z)) 15)::
  (EA 15 AWeaken 16)::(EA 16 (AAssign V_compute_snaps_psst_dref_off0
  (Some (EVar V_compute_snaps_j))) 17)::(EA 17 AWeaken 18)::
  (EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_compute_snaps__tmp1) s) <> (eval (ENum (0))
  s))%Z)) 23)::(EA 20 (AGuard (fun s => ((eval (EVar V_compute_snaps__tmp1)
  s) = (eval (ENum (0)) s))%Z)) 21)::(EA 21 AWeaken 22)::(EA 22 ANone 25)::
  (EA 23 AWeaken 24)::(EA 24 ANone 25)::(EA 25 (AAssign V_compute_snaps_code
  None) 26)::(EA 26 AWeaken 27)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_compute_snaps_code) s) < (eval (ENum (0))
  s))%Z)) 36)::(EA 27 (AGuard (fun s => ((eval (EVar V_compute_snaps_code)
  s) >= (eval (ENum (0)) s))%Z)) 28)::(EA 28 AWeaken 29)::(EA 29 ANone 31)::
  (EA 29 ANone 30)::(EA 30 ANone 32)::(EA 31 ANone 32)::(EA 32 ANone 33)::
  (EA 33 ANone 34)::(EA 34 (AAssign V_compute_snaps_j
  (Some (EAdd (EVar V_compute_snaps_j) (ENum (1))))) 35)::(EA 35 ANone 38)::
  (EA 36 AWeaken 37)::(EA 37 ANone 38)::(EA 38 (AAssign V_compute_snaps_i
  (Some (EAdd (EVar V_compute_snaps_i) (ENum (1))))) 39)::(EA 39 ANone 40)::
  (EA 40 ANone 41)::(EA 41 (AAssign V_compute_snaps_z (Some (EAdd (ENum (1))
  (EVar V_compute_snaps_z)))) 42)::(EA 42 AWeaken 14)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_compute_snaps => Pedges_compute_snaps
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_compute_snaps => 18
     end)%positive;
  var_global := var_global
}.

Definition ai_compute_snaps (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_z <= 0)%Z
   | 3 => (-1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_z <= 0)%Z
   | 4 => (1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_z <= 0)%Z
   | 5 => (-1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_z <= 0)%Z
   | 6 => (1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps__tmp <= 0 /\ -1 * s V_compute_snaps__tmp <= 0)%Z
   | 7 => (-1 * s V_compute_snaps__tmp <= 0 /\ 1 * s V_compute_snaps__tmp <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_z <= 0)%Z
   | 8 => (1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_z <= 0)%Z
   | 9 => (-1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_z <= 0)%Z
   | 10 => (1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_z <= 0)%Z
   | 11 => (-1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_z <= 0)%Z
   | 12 => (1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_i <= 0)%Z
   | 13 => (-1 * s V_compute_snaps_i <= 0 /\ 1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_z <= 0)%Z
   | 14 => (-1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0)%Z
   | 15 => (-1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i+ 1 * s V_compute_snaps_pst_dref_off0 <= 0)%Z
   | 16 => (-1 * s V_compute_snaps_i+ 1 * s V_compute_snaps_pst_dref_off0 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0)%Z
   | 17 => (-1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i+ 1 * s V_compute_snaps_pst_dref_off0 <= 0)%Z
   | 18 => (-1 * s V_compute_snaps_i+ 1 * s V_compute_snaps_pst_dref_off0 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0)%Z
   | 19 => (-1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0)%Z
   | 20 => (1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0)%Z
   | 21 => (-1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0 /\ 1 * s V_compute_snaps__tmp1 <= 0 /\ -1 * s V_compute_snaps__tmp1 <= 0)%Z
   | 22 => (-1 * s V_compute_snaps__tmp1 <= 0 /\ 1 * s V_compute_snaps__tmp1 <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0)%Z
   | 23 => (-1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0)%Z
   | 24 => (1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0)%Z
   | 25 => (-1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0)%Z
   | 26 => (1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0)%Z
   | 27 => (-1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0)%Z
   | 28 => (1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_code <= 0)%Z
   | 29 => (-1 * s V_compute_snaps_code <= 0 /\ -1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0)%Z
   | 30 => (1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_code <= 0)%Z
   | 31 => (1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_code <= 0)%Z
   | 32 => (-1 * s V_compute_snaps_code <= 0 /\ -1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0)%Z
   | 33 => (1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_code <= 0)%Z
   | 34 => (-1 * s V_compute_snaps_code <= 0 /\ -1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0)%Z
   | 35 => (1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_code <= 0)%Z
   | 36 => (1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0 /\ 1 * s V_compute_snaps_code + 1 <= 0)%Z
   | 37 => (1 * s V_compute_snaps_code + 1 <= 0 /\ -1 * s V_compute_snaps_i <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0)%Z
   | 38 => (1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 + 1 <= 0 /\ -1 * s V_compute_snaps_z <= 0 /\ -1 * s V_compute_snaps_i <= 0)%Z
   | 39 => (-1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 <= 0 /\ -1 * s V_compute_snaps_i + 1 <= 0)%Z
   | 40 => (-1 * s V_compute_snaps_i + 1 <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 <= 0 /\ -1 * s V_compute_snaps_z <= 0)%Z
   | 41 => (-1 * s V_compute_snaps_z <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 <= 0 /\ -1 * s V_compute_snaps_i + 1 <= 0)%Z
   | 42 => (-1 * s V_compute_snaps_i + 1 <= 0 /\ 1 * s V_compute_snaps_i+ -1 * s V_compute_snaps_pst_dref_off0 <= 0 /\ -1 * s V_compute_snaps_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_compute_snaps (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 2 => (s V_compute_snaps_z + max0(s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 3 => (s V_compute_snaps_z + max0(s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 4 => (s V_compute_snaps_z + max0(s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 5 => (s V_compute_snaps_z + max0(s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 6 => (s V_compute_snaps_z + max0(s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 7 => (s V_compute_snaps_z + max0(s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 8 => (s V_compute_snaps_z + max0(s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 9 => (s V_compute_snaps_z + max0(s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 10 => (s V_compute_snaps_z + max0(s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 11 => (s V_compute_snaps_z + max0(s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 12 => (s V_compute_snaps_z
            + max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_compute_snaps_z) (0))) (F_max0_ge_0 (s V_compute_snaps_z))]
     (s V_compute_snaps_z
      + max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 14 => (max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
            + max0(s V_compute_snaps_z) <= z)%Q
   | 15 => (max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
            + max0(s V_compute_snaps_z) <= z)%Q
   | 16 => (max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
            + max0(s V_compute_snaps_z) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_compute_snaps_i
                                             + s V_compute_snaps_pst_dref_off0) (-1
                                                                    - s V_compute_snaps_i
                                                                    + s V_compute_snaps_pst_dref_off0));
      (*-1 0*) F_max0_ge_0 (-1 - s V_compute_snaps_i
                            + s V_compute_snaps_pst_dref_off0);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_compute_snaps_z)) (F_check_ge (s V_compute_snaps_z) (0))]
     (max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
      + max0(s V_compute_snaps_z) <= z)%Q
   | 18 => (s V_compute_snaps_z <= z)%Q
   | 19 => (max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
            + max0(s V_compute_snaps_z) <= z)%Q
   | 20 => (max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
            + max0(s V_compute_snaps_z) <= z)%Q
   | 21 => hints
     [(*0 1*) F_max0_pre_decrement 1 (-s V_compute_snaps_i
                                      + s V_compute_snaps_pst_dref_off0) (1)]
     (max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
      + max0(s V_compute_snaps_z) <= z)%Q
   | 22 => ((1 # 1)
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
            + max0(s V_compute_snaps_z) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_compute_snaps_i
                                       + s V_compute_snaps_pst_dref_off0) (1)]
     (max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
      + max0(s V_compute_snaps_z) <= z)%Q
   | 24 => ((1 # 1)
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
            + max0(s V_compute_snaps_z) <= z)%Q
   | 25 => ((1 # 1)
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
            + max0(s V_compute_snaps_z) <= z)%Q
   | 26 => ((1 # 1)
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
            + max0(s V_compute_snaps_z) <= z)%Q
   | 27 => ((1 # 1)
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
            + max0(s V_compute_snaps_z) <= z)%Q
   | 28 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_compute_snaps_z)) (F_check_ge (s V_compute_snaps_z) (0))]
     ((1 # 1)
      + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
      + max0(s V_compute_snaps_z) <= z)%Q
   | 29 => ((1 # 1) + s V_compute_snaps_z
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 30 => ((1 # 1) + s V_compute_snaps_z
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 31 => ((1 # 1) + s V_compute_snaps_z
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 32 => ((1 # 1) + s V_compute_snaps_z
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 33 => ((1 # 1) + s V_compute_snaps_z
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 34 => ((1 # 1) + s V_compute_snaps_z
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 35 => ((1 # 1) + s V_compute_snaps_z
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_compute_snaps_z)) (F_check_ge (s V_compute_snaps_z) (0))]
     ((1 # 1)
      + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0)
      + max0(s V_compute_snaps_z) <= z)%Q
   | 37 => ((1 # 1) + s V_compute_snaps_z
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 38 => ((1 # 1) + s V_compute_snaps_z
            + max0(-1 - s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 39 => ((1 # 1) + s V_compute_snaps_z
            + max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 40 => ((1 # 1) + s V_compute_snaps_z
            + max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 41 => ((1 # 1) + s V_compute_snaps_z
            + max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | 42 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_compute_snaps_z) (0))) (F_max0_ge_0 (s V_compute_snaps_z))]
     (s V_compute_snaps_z
      + max0(-s V_compute_snaps_i + s V_compute_snaps_pst_dref_off0) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_compute_snaps =>
    [mkPA Q (fun n z s => ai_compute_snaps n s /\ annot0_compute_snaps n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_compute_snaps (proc_start P_compute_snaps) s1 (proc_end P_compute_snaps) s2 ->
    (s2 V_compute_snaps_z <= max0(s1 V_compute_snaps_pst_dref_off0))%Q.
Proof.
  prove_bound ipa admissible_ipa P_compute_snaps.
Qed.
