Require Import pasta.Pasta.

Inductive proc: Type :=
  P_find_snap.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_find_snap_z := 1%positive.
Notation V_find_snap__tmp := 2%positive.
Notation V_find_snap_adj_dv := 3%positive.
Notation V_find_snap_best := 4%positive.
Notation V_find_snap_diff := 5%positive.
Notation V_find_snap_i := 6%positive.
Notation V_find_snap_pps_dref_off0 := 7%positive.
Notation V_find_snap_psst_dref_off0 := 8%positive.
Notation V_find_snap_dv := 9%positive.
Notation V_find_snap_pps := 10%positive.
Notation V_find_snap_psst := 11%positive.
Definition Pedges_find_snap: list (edge proc) :=
  (EA 1 (AAssign V_find_snap_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_find_snap__tmp (Some (EVar V_find_snap_dv))) 3)::(EA 3 (AAssign
  V_find_snap_best (Some (EVar V_find_snap_pps_dref_off0))) 4)::
  (EA 4 (AAssign V_find_snap_i (Some (ENum (0)))) 5)::(EA 5 ANone 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard (fun s => ((eval (EVar V_find_snap_i) s) <
  (eval (EVar V_find_snap_psst_dref_off0) s))%Z)) 28)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_find_snap_i) s) >=
  (eval (EVar V_find_snap_psst_dref_off0) s))%Z)) 8)::(EA 8 AWeaken 9)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_find_snap_best) s) <
  (eval (ENum (0)) s))%Z)) 13)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_find_snap_best) s) >= (eval (ENum (0))
  s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 ANone 12)::(EA 12 AWeaken 16)::
  (EA 13 AWeaken 14)::(EA 14 ANone 15)::(EA 15 AWeaken 16)::
  (EA 16 ANone 18)::(EA 16 ANone 17)::(EA 17 ANone 19)::(EA 18 ANone 19)::
  (EA 19 (AAssign V_find_snap_adj_dv None) 20)::(EA 20 AWeaken 21)::
  (EA 21 (AGuard (fun s => ((eval (EVar V_find_snap_adj_dv) s) =
  (eval (ENum (0)) s))%Z)) 23)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_find_snap_adj_dv) s) <> (eval (ENum (0))
  s))%Z)) 22)::(EA 22 AWeaken 27)::(EA 23 AWeaken 24)::(EA 24 (AAssign
  V_find_snap_adj_dv (Some (EVar V_find_snap_pps_dref_off0))) 25)::
  (EA 25 ANone 26)::(EA 26 AWeaken 27)::(EA 28 AWeaken 29)::(EA 29 (AAssign
  V_find_snap_diff None) 30)::(EA 30 AWeaken 31)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_find_snap_diff) s) < (eval (ENum (0))
  s))%Z)) 35)::(EA 31 (AGuard (fun s => ((eval (EVar V_find_snap_diff) s) >=
  (eval (ENum (0)) s))%Z)) 32)::(EA 32 AWeaken 33)::(EA 33 ANone 34)::
  (EA 34 AWeaken 38)::(EA 35 AWeaken 36)::(EA 36 ANone 37)::
  (EA 37 AWeaken 38)::(EA 38 (AGuard (fun s => ((eval (EVar V_find_snap_best)
  s) < (eval (ENum (0)) s))%Z)) 42)::(EA 38 (AGuard
  (fun s => ((eval (EVar V_find_snap_best) s) >= (eval (ENum (0))
  s))%Z)) 39)::(EA 39 AWeaken 40)::(EA 40 ANone 41)::(EA 41 AWeaken 45)::
  (EA 42 AWeaken 43)::(EA 43 ANone 44)::(EA 44 AWeaken 45)::
  (EA 45 ANone 46)::(EA 45 ANone 50)::(EA 46 ANone 47)::(EA 47 ANone 48)::
  (EA 48 (AAssign V_find_snap_best (Some (EVar V_find_snap_diff))) 49)::
  (EA 49 ANone 50)::(EA 50 ANone 51)::(EA 51 (AAssign V_find_snap_i
  (Some (EAdd (EVar V_find_snap_i) (ENum (1))))) 52)::(EA 52 ANone 53)::
  (EA 53 ANone 54)::(EA 54 (AAssign V_find_snap_z (Some (EAdd (ENum (1))
  (EVar V_find_snap_z)))) 55)::(EA 55 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_find_snap => Pedges_find_snap
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_find_snap => 27
     end)%positive;
  var_global := var_global
}.

Definition ai_find_snap (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_z <= 0)%Z
   | 3 => (-1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_z <= 0)%Z
   | 4 => (1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_z <= 0)%Z
   | 5 => (-1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 6 => (-1 * s V_find_snap_i <= 0 /\ 1 * s V_find_snap_i <= 0 /\ 1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_z <= 0)%Z
   | 7 => (-1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 8 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0)%Z
   | 9 => (-1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 10 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_best <= 0)%Z
   | 11 => (-1 * s V_find_snap_best <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 12 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_best <= 0)%Z
   | 13 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ 1 * s V_find_snap_best + 1 <= 0)%Z
   | 14 => (1 * s V_find_snap_best + 1 <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 15 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ 1 * s V_find_snap_best + 1 <= 0)%Z
   | 16 => (-1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 17 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0)%Z
   | 18 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0)%Z
   | 19 => (-1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 20 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0)%Z
   | 21 => (-1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 22 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0)%Z
   | 23 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ 1 * s V_find_snap_adj_dv <= 0 /\ -1 * s V_find_snap_adj_dv <= 0)%Z
   | 24 => (-1 * s V_find_snap_adj_dv <= 0 /\ 1 * s V_find_snap_adj_dv <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 25 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0)%Z
   | 26 => (-1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 27 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i+ 1 * s V_find_snap_psst_dref_off0 <= 0)%Z
   | 28 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0)%Z
   | 29 => (1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 30 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0)%Z
   | 31 => (1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 32 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_diff <= 0)%Z
   | 33 => (-1 * s V_find_snap_diff <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 34 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_diff <= 0)%Z
   | 35 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ 1 * s V_find_snap_diff + 1 <= 0)%Z
   | 36 => (1 * s V_find_snap_diff + 1 <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 37 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ 1 * s V_find_snap_diff + 1 <= 0)%Z
   | 38 => (1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 39 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_best <= 0)%Z
   | 40 => (-1 * s V_find_snap_best <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 41 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_best <= 0)%Z
   | 42 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ 1 * s V_find_snap_best + 1 <= 0)%Z
   | 43 => (1 * s V_find_snap_best + 1 <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 44 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ 1 * s V_find_snap_best + 1 <= 0)%Z
   | 45 => (1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 46 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0)%Z
   | 47 => (1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 48 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0)%Z
   | 49 => (1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 50 => (-1 * s V_find_snap_i <= 0 /\ -1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0)%Z
   | 51 => (1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 + 1 <= 0 /\ -1 * s V_find_snap_z <= 0 /\ -1 * s V_find_snap_i <= 0)%Z
   | 52 => (-1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_i + 1 <= 0)%Z
   | 53 => (-1 * s V_find_snap_i + 1 <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_z <= 0)%Z
   | 54 => (-1 * s V_find_snap_z <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_i + 1 <= 0)%Z
   | 55 => (-1 * s V_find_snap_i + 1 <= 0 /\ 1 * s V_find_snap_i+ -1 * s V_find_snap_psst_dref_off0 <= 0 /\ -1 * s V_find_snap_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_find_snap (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_find_snap_psst_dref_off0) <= z)%Q
   | 2 => (s V_find_snap_z + max0(s V_find_snap_psst_dref_off0) <= z)%Q
   | 3 => (s V_find_snap_z + max0(s V_find_snap_psst_dref_off0) <= z)%Q
   | 4 => (s V_find_snap_z + max0(s V_find_snap_psst_dref_off0) <= z)%Q
   | 5 => (s V_find_snap_z
           + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 6 => (s V_find_snap_z
           + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 7 => (s V_find_snap_z
           + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 8 => (s V_find_snap_z
           + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 9 => (s V_find_snap_z
           + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 10 => (s V_find_snap_z
            + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 11 => (s V_find_snap_z
            + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 12 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (-s V_find_snap_i
                                            + s V_find_snap_psst_dref_off0) (-1
                                                                    - s V_find_snap_i
                                                                    + s V_find_snap_psst_dref_off0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_find_snap_i
                                                 + s V_find_snap_psst_dref_off0)) (F_check_ge (0) (0))]
     (s V_find_snap_z + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 13 => (s V_find_snap_z
            + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 14 => (s V_find_snap_z
            + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_find_snap_i
                                             + s V_find_snap_psst_dref_off0) (-1
                                                                    - s V_find_snap_i
                                                                    + s V_find_snap_psst_dref_off0));
      (*-1 0*) F_max0_ge_0 (-1 - s V_find_snap_i
                            + s V_find_snap_psst_dref_off0)]
     (s V_find_snap_z + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 16 => (s V_find_snap_z <= z)%Q
   | 17 => (s V_find_snap_z <= z)%Q
   | 18 => (s V_find_snap_z <= z)%Q
   | 19 => (s V_find_snap_z <= z)%Q
   | 20 => (s V_find_snap_z <= z)%Q
   | 21 => (s V_find_snap_z <= z)%Q
   | 22 => (s V_find_snap_z <= z)%Q
   | 23 => (s V_find_snap_z <= z)%Q
   | 24 => (s V_find_snap_z <= z)%Q
   | 25 => (s V_find_snap_z <= z)%Q
   | 26 => (s V_find_snap_z <= z)%Q
   | 27 => (s V_find_snap_z <= z)%Q
   | 28 => hints
     [(*0 1*) F_max0_pre_decrement 1 (-s V_find_snap_i
                                      + s V_find_snap_psst_dref_off0) (1)]
     (s V_find_snap_z + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 29 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 30 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 31 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 32 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 33 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 34 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 35 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 36 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 37 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 38 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 39 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 40 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 41 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 42 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 43 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 44 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 45 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 46 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 47 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 48 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 49 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 50 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 51 => ((1 # 1) + s V_find_snap_z
            + max0(-1 - s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 52 => ((1 # 1) + s V_find_snap_z
            + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 53 => ((1 # 1) + s V_find_snap_z
            + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 54 => ((1 # 1) + s V_find_snap_z
            + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | 55 => (s V_find_snap_z
            + max0(-s V_find_snap_i + s V_find_snap_psst_dref_off0) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_find_snap =>
    [mkPA Q (fun n z s => ai_find_snap n s /\ annot0_find_snap n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_find_snap (proc_start P_find_snap) s1 (proc_end P_find_snap) s2 ->
    (s2 V_find_snap_z <= max0(s1 V_find_snap_psst_dref_off0))%Q.
Proof.
  prove_bound ipa admissible_ipa P_find_snap.
Qed.
