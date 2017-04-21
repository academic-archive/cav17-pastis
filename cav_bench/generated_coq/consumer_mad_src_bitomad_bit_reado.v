Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mad_bit_read.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mad_bit_read_z := 1%positive.
Notation V_mad_bit_read__tmp := 2%positive.
Notation V_mad_bit_read__tmp1 := 3%positive.
Notation V_mad_bit_read_bitptr_dref_off10 := 4%positive.
Notation V_mad_bit_read_bitptr_dref_off8 := 5%positive.
Notation V_mad_bit_read_value := 6%positive.
Notation V_mad_bit_read_bitptr := 7%positive.
Notation V_mad_bit_read_len := 8%positive.
Definition Pedges_mad_bit_read: list (edge proc) :=
  (EA 1 (AAssign V_mad_bit_read_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_mad_bit_read_bitptr_dref_off10) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_mad_bit_read__tmp) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_mad_bit_read__tmp
  (Some (EVar V_mad_bit_read_len))) 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_mad_bit_read_bitptr_dref_off10) s) =
  (eval (ENum (8)) s))%Z)) 9)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_mad_bit_read_bitptr_dref_off10) s) <>
  (eval (ENum (8)) s))%Z)) 8)::(EA 8 AWeaken 13)::(EA 9 AWeaken 10)::
  (EA 10 (AAssign V_mad_bit_read_bitptr_dref_off8 None) 11)::
  (EA 11 ANone 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_mad_bit_read__tmp) s) <
  (eval (EVar V_mad_bit_read_bitptr_dref_off10) s))%Z)) 39)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_mad_bit_read__tmp) s) >=
  (eval (EVar V_mad_bit_read_bitptr_dref_off10) s))%Z)) 14)::
  (EA 14 AWeaken 15)::(EA 15 (AAssign V_mad_bit_read_value None) 16)::
  (EA 16 (AAssign V_mad_bit_read__tmp (Some (ESub (EVar V_mad_bit_read__tmp)
  (EVar V_mad_bit_read_bitptr_dref_off10)))) 17)::(EA 17 (AAssign
  V_mad_bit_read_bitptr_dref_off10 (Some (ENum (8)))) 18)::(EA 18 ANone 19)::
  (EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_mad_bit_read__tmp) s) >= (eval (ENum (8))
  s))%Z)) 32)::(EA 20 (AGuard (fun s => ((eval (EVar V_mad_bit_read__tmp)
  s) < (eval (ENum (8)) s))%Z)) 21)::(EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_mad_bit_read__tmp) s) > (eval (ENum (0))
  s))%Z)) 24)::(EA 22 (AGuard (fun s => ((eval (EVar V_mad_bit_read__tmp)
  s) <= (eval (ENum (0)) s))%Z)) 23)::(EA 23 AWeaken 29)::
  (EA 24 AWeaken 25)::(EA 25 (AAssign V_mad_bit_read_bitptr_dref_off8
  None) 26)::(EA 26 (AAssign V_mad_bit_read_value None) 27)::(EA 27 (AAssign
  V_mad_bit_read_bitptr_dref_off10
  (Some (ESub (EVar V_mad_bit_read_bitptr_dref_off10)
  (EVar V_mad_bit_read__tmp)))) 28)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_mad_bit_read__tmp1 (Some (EVar V_mad_bit_read_value))) 30)::
  (EA 30 ANone 31)::(EA 31 AWeaken 45)::(EA 32 AWeaken 33)::(EA 33 (AAssign
  V_mad_bit_read_value None) 34)::(EA 34 (AAssign V_mad_bit_read__tmp
  (Some (ESub (EVar V_mad_bit_read__tmp) (ENum (8))))) 35)::
  (EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign V_mad_bit_read_z
  (Some (EAdd (ENum (1)) (EVar V_mad_bit_read_z)))) 38)::(EA 38 AWeaken 20)::
  (EA 39 AWeaken 40)::(EA 40 (AAssign V_mad_bit_read_value None) 41)::
  (EA 41 (AAssign V_mad_bit_read_bitptr_dref_off10
  (Some (ESub (EVar V_mad_bit_read_bitptr_dref_off10)
  (EVar V_mad_bit_read__tmp)))) 42)::(EA 42 (AAssign V_mad_bit_read__tmp1
  (Some (EVar V_mad_bit_read_value))) 43)::(EA 43 ANone 44)::
  (EA 44 AWeaken 45)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mad_bit_read => Pedges_mad_bit_read
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mad_bit_read => 45
     end)%positive;
  var_global := var_global
}.

Definition ai_mad_bit_read (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0)%Z
   | 3 => (-1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0)%Z
   | 4 => (-1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read__tmp <= 0)%Z
   | 5 => (-1 * s V_mad_bit_read__tmp <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0)%Z
   | 6 => (-1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0)%Z
   | 7 => (-1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0)%Z
   | 8 => (-1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0)%Z
   | 9 => (1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0)%Z
   | 10 => (-1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_z <= 0)%Z
   | 11 => (1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0)%Z
   | 12 => (-1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_z <= 0)%Z
   | 13 => (-1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0)%Z
   | 14 => (-1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ -1 * s V_mad_bit_read__tmp+ 1 * s V_mad_bit_read_bitptr_dref_off10 <= 0)%Z
   | 15 => (-1 * s V_mad_bit_read__tmp+ 1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0)%Z
   | 16 => (-1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ -1 * s V_mad_bit_read__tmp+ 1 * s V_mad_bit_read_bitptr_dref_off10 <= 0)%Z
   | 17 => (-1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read__tmp <= 0)%Z
   | 18 => (-1 * s V_mad_bit_read__tmp <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0)%Z
   | 19 => (-1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read__tmp <= 0)%Z
   | 20 => (-1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ -1 * s V_mad_bit_read__tmp <= 0)%Z
   | 21 => (-1 * s V_mad_bit_read__tmp <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read__tmp + -7 <= 0)%Z
   | 22 => (1 * s V_mad_bit_read__tmp + -7 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ -1 * s V_mad_bit_read__tmp <= 0)%Z
   | 23 => (-1 * s V_mad_bit_read__tmp <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read__tmp <= 0)%Z
   | 24 => (-1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read__tmp + -7 <= 0 /\ -1 * s V_mad_bit_read__tmp + 1 <= 0)%Z
   | 25 => (-1 * s V_mad_bit_read__tmp + 1 <= 0 /\ 1 * s V_mad_bit_read__tmp + -7 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0)%Z
   | 26 => (-1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read__tmp + -7 <= 0 /\ -1 * s V_mad_bit_read__tmp + 1 <= 0)%Z
   | 27 => (-1 * s V_mad_bit_read__tmp + 1 <= 0 /\ 1 * s V_mad_bit_read__tmp + -7 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0)%Z
   | 28 => (-1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read__tmp + -7 <= 0 /\ -1 * s V_mad_bit_read__tmp + 1 <= 0 /\ 1 * s V_mad_bit_read__tmp+ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read__tmp+ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0)%Z
   | 29 => (-1 * s V_mad_bit_read__tmp <= 0 /\ -1 * s V_mad_bit_read__tmp+ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read__tmp+ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ 1 * s V_mad_bit_read__tmp + -7 <= 0 /\ -1 * s V_mad_bit_read_z <= 0)%Z
   | 30 => (-1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read__tmp + -7 <= 0 /\ 1 * s V_mad_bit_read__tmp+ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read__tmp+ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ -1 * s V_mad_bit_read__tmp <= 0)%Z
   | 31 => (-1 * s V_mad_bit_read__tmp <= 0 /\ -1 * s V_mad_bit_read__tmp+ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read__tmp+ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ 1 * s V_mad_bit_read__tmp + -7 <= 0 /\ -1 * s V_mad_bit_read_z <= 0)%Z
   | 32 => (-1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read__tmp + 8 <= 0)%Z
   | 33 => (-1 * s V_mad_bit_read__tmp + 8 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0)%Z
   | 34 => (-1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read__tmp + 8 <= 0)%Z
   | 35 => (-1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ -1 * s V_mad_bit_read__tmp <= 0)%Z
   | 36 => (-1 * s V_mad_bit_read__tmp <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_z <= 0)%Z
   | 37 => (-1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ -1 * s V_mad_bit_read__tmp <= 0)%Z
   | 38 => (-1 * s V_mad_bit_read__tmp <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 8 <= 0 /\ 1 * s V_mad_bit_read_bitptr_dref_off10 + -8 <= 0 /\ -1 * s V_mad_bit_read_z + 1 <= 0)%Z
   | 39 => (-1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ 1 * s V_mad_bit_read__tmp+ -1 * s V_mad_bit_read_bitptr_dref_off10 + 1 <= 0)%Z
   | 40 => (1 * s V_mad_bit_read__tmp+ -1 * s V_mad_bit_read_bitptr_dref_off10 + 1 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0)%Z
   | 41 => (-1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ 1 * s V_mad_bit_read__tmp+ -1 * s V_mad_bit_read_bitptr_dref_off10 + 1 <= 0)%Z
   | 42 => (1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read__tmp+ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 1 <= 0)%Z
   | 43 => (-1 * s V_mad_bit_read_bitptr_dref_off10 + 1 <= 0 /\ -1 * s V_mad_bit_read__tmp+ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ 1 * s V_mad_bit_read_z <= 0)%Z
   | 44 => (1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read_z <= 0 /\ -1 * s V_mad_bit_read__tmp+ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ -1 * s V_mad_bit_read_bitptr_dref_off10 + 1 <= 0)%Z
   | 45 => (-1 * s V_mad_bit_read_bitptr_dref_off10 + 1 <= 0 /\ -1 * s V_mad_bit_read__tmp+ -1 * s V_mad_bit_read_bitptr_dref_off10 <= 0 /\ -1 * s V_mad_bit_read_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mad_bit_read (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 8) * max0(s V_mad_bit_read_len) <= z)%Q
   | 2 => (s V_mad_bit_read_z + (1 # 8) * max0(s V_mad_bit_read_len) <= z)%Q
   | 3 => (s V_mad_bit_read_z + (1 # 8) * max0(s V_mad_bit_read_len) <= z)%Q
   | 4 => (s V_mad_bit_read_z + (1 # 8) * max0(s V_mad_bit_read_len) <= z)%Q
   | 5 => (s V_mad_bit_read_z + (1 # 8) * max0(s V_mad_bit_read_len) <= z)%Q
   | 6 => (s V_mad_bit_read_z + (1 # 8) * max0(s V_mad_bit_read__tmp) <= z)%Q
   | 7 => (s V_mad_bit_read_z + (1 # 8) * max0(s V_mad_bit_read__tmp) <= z)%Q
   | 8 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mad_bit_read_z) (0))) (F_max0_ge_0 (s V_mad_bit_read_z))]
     (s V_mad_bit_read_z + (1 # 8) * max0(s V_mad_bit_read__tmp) <= z)%Q
   | 9 => (s V_mad_bit_read_z + (1 # 8) * max0(s V_mad_bit_read__tmp) <= z)%Q
   | 10 => (s V_mad_bit_read_z + (1 # 8) * max0(s V_mad_bit_read__tmp) <= z)%Q
   | 11 => (s V_mad_bit_read_z + (1 # 8) * max0(s V_mad_bit_read__tmp) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mad_bit_read_z) (0))) (F_max0_ge_0 (s V_mad_bit_read_z))]
     (s V_mad_bit_read_z + (1 # 8) * max0(s V_mad_bit_read__tmp) <= z)%Q
   | 13 => ((1 # 8) * max0(s V_mad_bit_read__tmp) + max0(s V_mad_bit_read_z) <= z)%Q
   | 14 => hints
     [(*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_mad_bit_read_bitptr_dref_off10)) (F_check_ge (0) (0));
      (*2.2175e-11 0.125*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mad_bit_read_bitptr_dref_off10) (0))) (F_max0_ge_0 (s V_mad_bit_read_bitptr_dref_off10));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mad_bit_read__tmp)) (F_check_ge (s V_mad_bit_read__tmp) (0))]
     ((1 # 8) * max0(s V_mad_bit_read__tmp) + max0(s V_mad_bit_read_z) <= z)%Q
   | 15 => ((1 # 8) * s V_mad_bit_read__tmp
            - (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
            + max0(s V_mad_bit_read_z) <= z)%Q
   | 16 => ((1 # 8) * s V_mad_bit_read__tmp
            - (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
            + max0(s V_mad_bit_read_z) <= z)%Q
   | 17 => ((1 # 8) * s V_mad_bit_read__tmp + max0(s V_mad_bit_read_z) <= z)%Q
   | 18 => ((1 # 8) * s V_mad_bit_read__tmp
            + (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
            - (1 # 8) * max0(s V_mad_bit_read_bitptr_dref_off10)
            + max0(s V_mad_bit_read_z) <= z)%Q
   | 19 => ((1 # 8) * s V_mad_bit_read__tmp
            + (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
            - (1 # 8) * max0(s V_mad_bit_read_bitptr_dref_off10)
            + max0(s V_mad_bit_read_z) <= z)%Q
   | 20 => ((1 # 8) * s V_mad_bit_read__tmp
            + (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
            - (1 # 8) * max0(s V_mad_bit_read_bitptr_dref_off10)
            + max0(s V_mad_bit_read_z) <= z)%Q
   | 21 => hints
     [(*-0.125 0*) F_max0_ge_0 (s V_mad_bit_read__tmp);
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mad_bit_read_z)) (F_check_ge (s V_mad_bit_read_z) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mad_bit_read_bitptr_dref_off10) (0))) (F_max0_ge_0 (s V_mad_bit_read_bitptr_dref_off10))]
     ((1 # 8) * s V_mad_bit_read__tmp
      + (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
      - (1 # 8) * max0(s V_mad_bit_read_bitptr_dref_off10)
      + max0(s V_mad_bit_read_z) <= z)%Q
   | 22 => ((1 # 8) * s V_mad_bit_read__tmp + s V_mad_bit_read_z
            - (1 # 8) * max0(s V_mad_bit_read__tmp) <= z)%Q
   | 23 => hints
     [(*1e-12 0.125*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mad_bit_read__tmp) (0))) (F_max0_ge_0 (s V_mad_bit_read__tmp))]
     ((1 # 8) * s V_mad_bit_read__tmp + s V_mad_bit_read_z
      - (1 # 8) * max0(s V_mad_bit_read__tmp) <= z)%Q
   | 24 => hints
     [(*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mad_bit_read__tmp) (0))) (F_max0_ge_0 (s V_mad_bit_read__tmp))]
     ((1 # 8) * s V_mad_bit_read__tmp + s V_mad_bit_read_z
      - (1 # 8) * max0(s V_mad_bit_read__tmp) <= z)%Q
   | 25 => (s V_mad_bit_read_z <= z)%Q
   | 26 => (s V_mad_bit_read_z <= z)%Q
   | 27 => (s V_mad_bit_read_z <= z)%Q
   | 28 => (s V_mad_bit_read_z <= z)%Q
   | 29 => (s V_mad_bit_read_z <= z)%Q
   | 30 => (s V_mad_bit_read_z <= z)%Q
   | 31 => (s V_mad_bit_read_z <= z)%Q
   | 32 => ((1 # 8) * s V_mad_bit_read__tmp
            + (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
            - (1 # 8) * max0(s V_mad_bit_read_bitptr_dref_off10)
            + max0(s V_mad_bit_read_z) <= z)%Q
   | 33 => ((1 # 8) * s V_mad_bit_read__tmp
            + (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
            - (1 # 8) * max0(s V_mad_bit_read_bitptr_dref_off10)
            + max0(s V_mad_bit_read_z) <= z)%Q
   | 34 => ((1 # 8) * s V_mad_bit_read__tmp
            + (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
            - (1 # 8) * max0(s V_mad_bit_read_bitptr_dref_off10)
            + max0(s V_mad_bit_read_z) <= z)%Q
   | 35 => ((1 # 1) + (1 # 8) * s V_mad_bit_read__tmp
            + (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
            - (1 # 8) * max0(s V_mad_bit_read_bitptr_dref_off10)
            + max0(s V_mad_bit_read_z) <= z)%Q
   | 36 => ((1 # 1) + (1 # 8) * s V_mad_bit_read__tmp
            + (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
            - (1 # 8) * max0(s V_mad_bit_read_bitptr_dref_off10)
            + max0(s V_mad_bit_read_z) <= z)%Q
   | 37 => ((1 # 1) + (1 # 8) * s V_mad_bit_read__tmp
            + (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
            - (1 # 8) * max0(s V_mad_bit_read_bitptr_dref_off10)
            + max0(s V_mad_bit_read_z) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mad_bit_read_z) (0))) (F_max0_ge_0 (s V_mad_bit_read_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_mad_bit_read_z)) (F_check_ge (-1
                                                                    + s V_mad_bit_read_z) (0))]
     ((1 # 1) + (1 # 8) * s V_mad_bit_read__tmp
      + (1 # 8) * s V_mad_bit_read_bitptr_dref_off10
      + max0(-1 + s V_mad_bit_read_z)
      - (1 # 8) * max0(s V_mad_bit_read_bitptr_dref_off10) <= z)%Q
   | 39 => ((1 # 8) * max0(s V_mad_bit_read__tmp) + max0(s V_mad_bit_read_z) <= z)%Q
   | 40 => ((1 # 8) * max0(s V_mad_bit_read__tmp) + max0(s V_mad_bit_read_z) <= z)%Q
   | 41 => ((1 # 8) * max0(s V_mad_bit_read__tmp) + max0(s V_mad_bit_read_z) <= z)%Q
   | 42 => ((1 # 8) * max0(s V_mad_bit_read__tmp) + max0(s V_mad_bit_read_z) <= z)%Q
   | 43 => ((1 # 8) * max0(s V_mad_bit_read__tmp) + max0(s V_mad_bit_read_z) <= z)%Q
   | 44 => hints
     [(*-0.125 0*) F_max0_monotonic (F_check_ge (s V_mad_bit_read__tmp) (-8
                                                                    + s V_mad_bit_read__tmp));
      (*-0.125 0*) F_max0_ge_0 (-8 + s V_mad_bit_read__tmp);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_mad_bit_read_z)) (F_check_ge (s V_mad_bit_read_z) (0))]
     ((1 # 8) * max0(s V_mad_bit_read__tmp) + max0(s V_mad_bit_read_z) <= z)%Q
   | 45 => (s V_mad_bit_read_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mad_bit_read =>
    [mkPA Q (fun n z s => ai_mad_bit_read n s /\ annot0_mad_bit_read n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mad_bit_read (proc_start P_mad_bit_read) s1 (proc_end P_mad_bit_read) s2 ->
    (s2 V_mad_bit_read_z <= (1 # 8) * max0(s1 V_mad_bit_read_len))%Q.
Proof.
  prove_bound ipa admissible_ipa P_mad_bit_read.
Qed.
