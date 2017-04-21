Require Import pasta.Pasta.

Inductive proc: Type :=
  P_array_get.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_array_get_z := 1%positive.
Notation V_array_get__tmp := 2%positive.
Notation V_array_get__tmp1 := 3%positive.
Notation V_array_get_aref_dref_off0_off2 := 4%positive.
Notation V_array_get_index := 5%positive.
Notation V_array_get_aref := 6%positive.
Notation V_array_get_index_long := 7%positive.
Notation V_array_get_pref := 8%positive.
Definition Pedges_array_get: list (edge proc) :=
  (EA 1 (AAssign V_array_get_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_array_get_aref_dref_off0_off2) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_array_get__tmp) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_array_get__tmp
  (Some (EVar V_array_get_index_long))) 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_array_get__tmp) s) >=
  (eval (EVar V_array_get_aref_dref_off0_off2) s))%Z)) 35)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_array_get__tmp) s) <
  (eval (EVar V_array_get_aref_dref_off0_off2) s))%Z)) 8)::(EA 8 AWeaken 9)::
  (EA 9 ANone 32)::(EA 9 ANone 29)::(EA 9 ANone 13)::(EA 9 ANone 10)::
  (EA 10 (AAssign V_array_get__tmp1 (Some (ENum (0)))) 11)::
  (EA 11 ANone 12)::(EA 12 AWeaken 39)::(EA 13 (AAssign V_array_get_index
  (Some (EVar V_array_get__tmp))) 14)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_array_get_index (Some (EAdd (EVar V_array_get_index) (ENum (-1))))) 16)::
  (EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_array_get_index) s) <> (eval (ENum (0))
  s))%Z)) 22)::(EA 17 (AGuard (fun s => ((eval (EVar V_array_get_index) s) =
  (eval (ENum (0)) s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 (AAssign
  V_array_get__tmp1 (Some (ENum (0)))) 20)::(EA 20 ANone 21)::
  (EA 21 AWeaken 39)::(EA 22 AWeaken 23)::(EA 23 ANone 25)::
  (EA 23 ANone 24)::(EA 24 ANone 26)::(EA 25 ANone 26)::(EA 26 ANone 27)::
  (EA 27 ANone 28)::(EA 28 (AAssign V_array_get_z (Some (EAdd (ENum (1))
  (EVar V_array_get_z)))) 15)::(EA 29 (AAssign V_array_get__tmp1
  (Some (ENum (0)))) 30)::(EA 30 ANone 31)::(EA 31 AWeaken 39)::
  (EA 32 (AAssign V_array_get__tmp1 (Some (ENum (-20)))) 33)::
  (EA 33 ANone 34)::(EA 34 AWeaken 39)::(EA 35 AWeaken 36)::(EA 36 (AAssign
  V_array_get__tmp1 (Some (ENum (-15)))) 37)::(EA 37 ANone 38)::
  (EA 38 AWeaken 39)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_array_get => Pedges_array_get
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_array_get => 39
     end)%positive;
  var_global := var_global
}.

Definition ai_array_get (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0)%Z
   | 3 => (-1 * s V_array_get_z <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0)%Z
   | 4 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0 /\ -1 * s V_array_get__tmp <= 0)%Z
   | 5 => (-1 * s V_array_get__tmp <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0)%Z
   | 6 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0)%Z
   | 7 => (-1 * s V_array_get_z <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0)%Z
   | 8 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0)%Z
   | 9 => (1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0)%Z
   | 10 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0)%Z
   | 11 => (1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get__tmp1 <= 0 /\ -1 * s V_array_get__tmp1 <= 0)%Z
   | 12 => (-1 * s V_array_get__tmp1 <= 0 /\ 1 * s V_array_get__tmp1 <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0)%Z
   | 13 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0)%Z
   | 14 => (1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 1 <= 0)%Z
   | 15 => (-1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0)%Z
   | 16 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0)%Z
   | 17 => (-1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0)%Z
   | 18 => (1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0 /\ 1 * s V_array_get_index <= 0 /\ -1 * s V_array_get_index <= 0)%Z
   | 19 => (-1 * s V_array_get_index <= 0 /\ 1 * s V_array_get_index <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0)%Z
   | 20 => (1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0 /\ 1 * s V_array_get_index <= 0 /\ -1 * s V_array_get_index <= 0 /\ 1 * s V_array_get__tmp1 <= 0 /\ -1 * s V_array_get__tmp1 <= 0)%Z
   | 21 => (-1 * s V_array_get__tmp1 <= 0 /\ 1 * s V_array_get__tmp1 <= 0 /\ -1 * s V_array_get_index <= 0 /\ 1 * s V_array_get_index <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0)%Z
   | 22 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0)%Z
   | 23 => (-1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0)%Z
   | 24 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0)%Z
   | 25 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0)%Z
   | 26 => (-1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0)%Z
   | 27 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0)%Z
   | 28 => (-1 * s V_array_get_aref_dref_off0_off2+ 1 * s V_array_get_index + 2 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0)%Z
   | 29 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0)%Z
   | 30 => (1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get__tmp1 <= 0 /\ -1 * s V_array_get__tmp1 <= 0)%Z
   | 31 => (-1 * s V_array_get__tmp1 <= 0 /\ 1 * s V_array_get__tmp1 <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0)%Z
   | 32 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0)%Z
   | 33 => (1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get__tmp1 + 20 <= 0 /\ -1 * s V_array_get__tmp1 + -20 <= 0)%Z
   | 34 => (-1 * s V_array_get__tmp1 + -20 <= 0 /\ 1 * s V_array_get__tmp1 + 20 <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get__tmp+ -1 * s V_array_get_aref_dref_off0_off2 + 1 <= 0)%Z
   | 35 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0 /\ -1 * s V_array_get__tmp+ 1 * s V_array_get_aref_dref_off0_off2 <= 0)%Z
   | 36 => (-1 * s V_array_get__tmp+ 1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0)%Z
   | 37 => (-1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_z <= 0 /\ -1 * s V_array_get__tmp+ 1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ 1 * s V_array_get__tmp1 + 15 <= 0 /\ -1 * s V_array_get__tmp1 + -15 <= 0)%Z
   | 38 => (-1 * s V_array_get__tmp1 + -15 <= 0 /\ 1 * s V_array_get__tmp1 + 15 <= 0 /\ -1 * s V_array_get__tmp+ 1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ -1 * s V_array_get_z <= 0 /\ 1 * s V_array_get_z <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0)%Z
   | 39 => (1 * s V_array_get__tmp1 <= 0 /\ -1 * s V_array_get__tmp1 + -20 <= 0 /\ -1 * s V_array_get_aref_dref_off0_off2 <= 0 /\ -1 * s V_array_get_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_array_get (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 2 => (s V_array_get_z + max0(s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 3 => (s V_array_get_z + max0(s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 4 => (s V_array_get_z + max0(s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 5 => (s V_array_get_z + max0(s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 6 => (s V_array_get_z + max0(s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 7 => (s V_array_get_z + max0(s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_array_get_aref_dref_off0_off2)) (F_check_ge (s V_array_get_aref_dref_off0_off2) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               - s V_array_get__tmp
                                                               + s V_array_get_aref_dref_off0_off2) (0))) (F_max0_ge_0 (-1
                                                                    - s V_array_get__tmp
                                                                    + s V_array_get_aref_dref_off0_off2))]
     (s V_array_get_z + max0(s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 9 => ((1 # 1) + s V_array_get__tmp + s V_array_get_z
           + max0(-1 - s V_array_get__tmp + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 10 => ((1 # 1) + s V_array_get__tmp + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 11 => ((1 # 1) + s V_array_get__tmp + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_array_get_aref_dref_off0_off2)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_array_get_aref_dref_off0_off2) (0))) (F_max0_ge_0 (s V_array_get_aref_dref_off0_off2));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 - s V_array_get__tmp
                                                   + s V_array_get_aref_dref_off0_off2)) (F_check_ge (-1
                                                                    - s V_array_get__tmp
                                                                    + s V_array_get_aref_dref_off0_off2) (0))]
     ((1 # 1) + s V_array_get__tmp + s V_array_get_z
      + max0(-1 - s V_array_get__tmp + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 13 => ((1 # 1) + s V_array_get__tmp + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 14 => ((1 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 15 => ((1 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 16 => ((2 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 17 => ((2 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 18 => ((2 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 19 => ((2 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 20 => (s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2)
            + (2 # 15) * max0(15 + s V_array_get__tmp1) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_array_get_index)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_array_get_index) (0))) (F_max0_ge_0 (s V_array_get_index));
      (*-0.133333 0*) F_binom_monotonic 1 (F_max0_ge_0 (15
                                                        + s V_array_get__tmp1)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_array_get__tmp
                                                 + s V_array_get_aref_dref_off0_off2)) (F_check_ge (0) (0))]
     (s V_array_get_index + s V_array_get_z
      + max0(-1 - s V_array_get__tmp + s V_array_get_aref_dref_off0_off2)
      + (2 # 15) * max0(15 + s V_array_get__tmp1) <= z)%Q
   | 22 => ((2 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 23 => ((2 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 24 => ((2 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 25 => ((2 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 26 => ((2 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 27 => ((2 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 28 => ((2 # 1) + s V_array_get_index + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 29 => ((1 # 1) + s V_array_get__tmp + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 30 => ((1 # 1) + s V_array_get__tmp + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_array_get_aref_dref_off0_off2)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_array_get_aref_dref_off0_off2) (0))) (F_max0_ge_0 (s V_array_get_aref_dref_off0_off2));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 - s V_array_get__tmp
                                                   + s V_array_get_aref_dref_off0_off2)) (F_check_ge (-1
                                                                    - s V_array_get__tmp
                                                                    + s V_array_get_aref_dref_off0_off2) (0))]
     ((1 # 1) + s V_array_get__tmp + s V_array_get_z
      + max0(-1 - s V_array_get__tmp + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 32 => ((1 # 1) + s V_array_get__tmp + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 33 => ((1 # 1) + s V_array_get__tmp + s V_array_get_z
            + max0(-1 - s V_array_get__tmp
                   + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 34 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_array_get_aref_dref_off0_off2)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_array_get_aref_dref_off0_off2) (0))) (F_max0_ge_0 (s V_array_get_aref_dref_off0_off2));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 - s V_array_get__tmp
                                                  + s V_array_get_aref_dref_off0_off2)) (F_check_ge (-1
                                                                    - s V_array_get__tmp
                                                                    + s V_array_get_aref_dref_off0_off2) (0))]
     ((1 # 1) + s V_array_get__tmp + s V_array_get_z
      + max0(-1 - s V_array_get__tmp + s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 35 => (s V_array_get_z + max0(s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 36 => (s V_array_get_z + max0(s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 37 => (s V_array_get_z + max0(s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_array_get_aref_dref_off0_off2)) (F_check_ge (0) (0))]
     (s V_array_get_z + max0(s V_array_get_aref_dref_off0_off2) <= z)%Q
   | 39 => (s V_array_get_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_array_get =>
    [mkPA Q (fun n z s => ai_array_get n s /\ annot0_array_get n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_array_get (proc_start P_array_get) s1 (proc_end P_array_get) s2 ->
    (s2 V_array_get_z <= max0(s1 V_array_get_aref_dref_off0_off2))%Q.
Proof.
  prove_bound ipa admissible_ipa P_array_get.
Qed.
