Require Import pasta.Pasta.

Inductive proc: Type :=
  P_Short_term_synthesis_filtering.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_Short_term_synthesis_filtering_z := 1%positive.
Notation V_Short_term_synthesis_filtering__tmp := 2%positive.
Notation V_Short_term_synthesis_filtering_i := 3%positive.
Notation V_Short_term_synthesis_filtering_ltmp := 4%positive.
Notation V_Short_term_synthesis_filtering_sri := 5%positive.
Notation V_Short_term_synthesis_filtering_tmp1 := 6%positive.
Notation V_Short_term_synthesis_filtering_tmp2 := 7%positive.
Notation V_Short_term_synthesis_filtering_S := 8%positive.
Notation V_Short_term_synthesis_filtering_k := 9%positive.
Notation V_Short_term_synthesis_filtering_rrp := 10%positive.
Notation V_Short_term_synthesis_filtering_sr := 11%positive.
Notation V_Short_term_synthesis_filtering_wt := 12%positive.
Definition Pedges_Short_term_synthesis_filtering: list (edge proc) :=
  (EA 1 (AAssign V_Short_term_synthesis_filtering_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_Short_term_synthesis_filtering__tmp
  (Some (EVar V_Short_term_synthesis_filtering_k))) 3)::(EA 3 ANone 4)::
  (EA 4 (AAssign V_Short_term_synthesis_filtering__tmp
  (Some (EAdd (EVar V_Short_term_synthesis_filtering__tmp)
  (ENum (-1))))) 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_Short_term_synthesis_filtering__tmp) s) <>
  (eval (ENum (0)) s))%Z)) 9)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_Short_term_synthesis_filtering__tmp) s) =
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 (AAssign V_Short_term_synthesis_filtering_sri None) 11)::
  (EA 11 (AAssign V_Short_term_synthesis_filtering_i (Some (ENum (8)))) 12)::
  (EA 12 ANone 13)::(EA 13 (AAssign V_Short_term_synthesis_filtering_i
  (Some (EAdd (EVar V_Short_term_synthesis_filtering_i) (ENum (-1))))) 14)::
  (EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_Short_term_synthesis_filtering_i) s) <>
  (eval (ENum (0)) s))%Z)) 20)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_Short_term_synthesis_filtering_i) s) =
  (eval (ENum (0)) s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 18)::
  (EA 18 ANone 19)::(EA 19 (AAssign V_Short_term_synthesis_filtering_z
  (Some (EAdd (ENum (1)) (EVar V_Short_term_synthesis_filtering_z)))) 4)::
  (EA 20 AWeaken 21)::(EA 21 (AAssign V_Short_term_synthesis_filtering_tmp1
  None) 22)::(EA 22 (AAssign V_Short_term_synthesis_filtering_tmp2
  None) 23)::(EA 23 AWeaken 24)::(EA 24 ANone 25)::(EA 24 ANone 27)::
  (EA 25 AWeaken 26)::(EA 26 ANone 45)::(EA 26 ANone 27)::(EA 27 ANone 28)::
  (EA 28 (AAssign V_Short_term_synthesis_filtering_tmp2 None) 29)::
  (EA 29 (AAssign V_Short_term_synthesis_filtering_ltmp
  (Some (ESub (EVar V_Short_term_synthesis_filtering_sri)
  (EVar V_Short_term_synthesis_filtering_tmp2)))) 30)::(EA 30 AWeaken 31)::
  (EA 31 (AGuard (fun s => True)) 44)::(EA 31 (AGuard (fun s => True)) 32)::
  (EA 32 AWeaken 33)::(EA 33 (AGuard (fun s => True)) 37)::(EA 33 ANone 34)::
  (EA 34 ANone 35)::(EA 35 (AGuard (fun s => True)) 36)::(EA 36 AWeaken 39)::
  (EA 37 AWeaken 38)::(EA 38 ANone 39)::(EA 39 (AAssign
  V_Short_term_synthesis_filtering_sri None) 40)::(EA 40 AWeaken 41)::
  (EA 41 (AGuard (fun s => True)) 43)::(EA 41 (AGuard (fun s => True)) 42)::
  (EA 42 AWeaken 49)::(EA 43 AWeaken 48)::(EA 44 AWeaken 46)::
  (EA 45 ANone 46)::(EA 46 ANone 47)::(EA 47 AWeaken 48)::(EA 48 ANone 57)::
  (EA 48 ANone 49)::(EA 49 ANone 50)::(EA 50 (AAssign
  V_Short_term_synthesis_filtering_tmp1 None) 51)::(EA 51 (AAssign
  V_Short_term_synthesis_filtering_ltmp None) 52)::(EA 52 AWeaken 53)::
  (EA 53 (AGuard (fun s => True)) 56)::(EA 53 (AGuard (fun s => True)) 54)::
  (EA 54 AWeaken 55)::(EA 55 ANone 59)::(EA 56 AWeaken 58)::
  (EA 57 ANone 58)::(EA 58 ANone 59)::(EA 59 ANone 60)::(EA 60 ANone 61)::
  (EA 61 (AAssign V_Short_term_synthesis_filtering_z (Some (EAdd (ENum (1))
  (EVar V_Short_term_synthesis_filtering_z)))) 13)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_Short_term_synthesis_filtering => Pedges_Short_term_synthesis_filtering
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_Short_term_synthesis_filtering => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_Short_term_synthesis_filtering (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_Short_term_synthesis_filtering_z <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 3 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 4 => (-1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 5 => (-1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 6 => (-1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 7 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering__tmp <= 0 /\ -1 * s V_Short_term_synthesis_filtering__tmp <= 0)%Z
   | 8 => (-1 * s V_Short_term_synthesis_filtering__tmp <= 0 /\ 1 * s V_Short_term_synthesis_filtering__tmp <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 9 => (-1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 10 => (-1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 11 => (-1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 12 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -8 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_i + 8 <= 0)%Z
   | 13 => (1 * s V_Short_term_synthesis_filtering_i + -8 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 14 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 15 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 16 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i <= 0 /\ -1 * s V_Short_term_synthesis_filtering_i <= 0)%Z
   | 17 => (-1 * s V_Short_term_synthesis_filtering_i <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 18 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i <= 0 /\ -1 * s V_Short_term_synthesis_filtering_i <= 0)%Z
   | 19 => (-1 * s V_Short_term_synthesis_filtering_i <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 20 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 21 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 22 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 23 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 24 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 25 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 26 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 27 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 28 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 29 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 30 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 31 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 32 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 33 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 34 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 35 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 36 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 37 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 38 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 39 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 40 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 41 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 42 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 43 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 44 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 45 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 46 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 47 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 48 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 49 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 50 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 51 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 52 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 53 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 54 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 55 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 56 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 57 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 58 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 59 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | 60 => (-1 * s V_Short_term_synthesis_filtering_z <= 0 /\ 1 * s V_Short_term_synthesis_filtering_i + -7 <= 0)%Z
   | 61 => (1 * s V_Short_term_synthesis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_synthesis_filtering_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_Short_term_synthesis_filtering (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) * s V_Short_term_synthesis_filtering_k <= z)%Q
   | 2 => ((8 # 1) * s V_Short_term_synthesis_filtering_k
           + s V_Short_term_synthesis_filtering_z <= z)%Q
   | 3 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
           + s V_Short_term_synthesis_filtering_z <= z)%Q
   | 4 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
           + s V_Short_term_synthesis_filtering_z <= z)%Q
   | 5 => ((8 # 1) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
           + s V_Short_term_synthesis_filtering_z <= z)%Q
   | 6 => ((8 # 1) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
           + s V_Short_term_synthesis_filtering_z <= z)%Q
   | 7 => hints
     [(*-8 0*) F_one;
      (*-8 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_Short_term_synthesis_filtering__tmp)) (F_check_ge (0) (0));
      (*-8 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_Short_term_synthesis_filtering__tmp) (0))) (F_max0_ge_0 (s V_Short_term_synthesis_filtering__tmp))]
     ((8 # 1) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
      + s V_Short_term_synthesis_filtering_z <= z)%Q
   | 8 => (s V_Short_term_synthesis_filtering_z <= z)%Q
   | 9 => ((8 # 1) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
           + s V_Short_term_synthesis_filtering_z <= z)%Q
   | 10 => ((8 # 1) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + s V_Short_term_synthesis_filtering_z <= z)%Q
   | 11 => ((8 # 1) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + s V_Short_term_synthesis_filtering_z <= z)%Q
   | 12 => (-(8 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 13 => (-(8 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 14 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 15 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_Short_term_synthesis_filtering_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_Short_term_synthesis_filtering_i) (0))) (F_max0_ge_0 (s V_Short_term_synthesis_filtering_i));
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_arg (7
                                                          - s V_Short_term_synthesis_filtering_i)) (F_check_ge (7
                                                                    - s V_Short_term_synthesis_filtering_i) (0))]
     ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
      + (8 # 7) * s V_Short_term_synthesis_filtering_i
      + s V_Short_term_synthesis_filtering_z
      + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 17 => ((1 # 1) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + s V_Short_term_synthesis_filtering_z <= z)%Q
   | 18 => ((1 # 1) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + s V_Short_term_synthesis_filtering_z <= z)%Q
   | 19 => ((1 # 1) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + s V_Short_term_synthesis_filtering_z <= z)%Q
   | 20 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 21 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 22 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 23 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 24 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 25 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 26 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 27 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 28 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 29 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 30 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 31 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 32 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 33 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 34 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 35 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 36 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 37 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 38 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 39 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 40 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 41 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 42 => hints
     [(*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - s V_Short_term_synthesis_filtering_i) (0))) (F_max0_ge_0 (8
                                                                    - s V_Short_term_synthesis_filtering_i));
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_arg (7
                                                          - s V_Short_term_synthesis_filtering_i)) (F_check_ge (7
                                                                    - s V_Short_term_synthesis_filtering_i) (0))]
     ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
      + (8 # 7) * s V_Short_term_synthesis_filtering_i
      + s V_Short_term_synthesis_filtering_z
      + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 43 => hints
     [(*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - s V_Short_term_synthesis_filtering_i) (0))) (F_max0_ge_0 (8
                                                                    - s V_Short_term_synthesis_filtering_i));
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_arg (7
                                                          - s V_Short_term_synthesis_filtering_i)) (F_check_ge (7
                                                                    - s V_Short_term_synthesis_filtering_i) (0))]
     ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
      + (8 # 7) * s V_Short_term_synthesis_filtering_i
      + s V_Short_term_synthesis_filtering_z
      + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 44 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 45 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 46 => ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 47 => hints
     [(*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - s V_Short_term_synthesis_filtering_i) (0))) (F_max0_ge_0 (8
                                                                    - s V_Short_term_synthesis_filtering_i));
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_arg (7
                                                          - s V_Short_term_synthesis_filtering_i)) (F_check_ge (7
                                                                    - s V_Short_term_synthesis_filtering_i) (0))]
     ((8 # 1) * s V_Short_term_synthesis_filtering__tmp
      + (8 # 7) * s V_Short_term_synthesis_filtering_i
      + s V_Short_term_synthesis_filtering_z
      + (1 # 7) * max0(7 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 48 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 49 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 50 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 51 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 52 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 53 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 54 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 55 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 56 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 57 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 58 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 59 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 60 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | 61 => (-(1 # 7) + (8 # 1) * s V_Short_term_synthesis_filtering__tmp
            + (8 # 7) * s V_Short_term_synthesis_filtering_i
            + s V_Short_term_synthesis_filtering_z
            + (1 # 7) * max0(8 - s V_Short_term_synthesis_filtering_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_Short_term_synthesis_filtering =>
    [mkPA Q (fun n z s => ai_Short_term_synthesis_filtering n s /\ annot0_Short_term_synthesis_filtering n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_Short_term_synthesis_filtering (proc_start P_Short_term_synthesis_filtering) s1 (proc_end P_Short_term_synthesis_filtering) s2 ->
    (s2 V_Short_term_synthesis_filtering_z <= (8 # 1) * s1 V_Short_term_synthesis_filtering_k)%Q.
Proof.
  prove_bound ipa admissible_ipa P_Short_term_synthesis_filtering.
Qed.
