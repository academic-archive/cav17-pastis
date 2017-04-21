Require Import pasta.Pasta.

Inductive proc: Type :=
  P_count_bits.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_count_bits_z := 1%positive.
Notation V_count_bits__tmp := 2%positive.
Notation V_count_bits_gfp_dref_off260 := 3%positive.
Notation V_count_bits_i := 4%positive.
Notation V_count_bits_cod_info := 5%positive.
Notation V_count_bits_gfp := 6%positive.
Notation V_count_bits_ix := 7%positive.
Notation V_count_bits_xr := 8%positive.
Definition Pedges_count_bits: list (edge proc) :=
  (EA 1 (AAssign V_count_bits_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_count_bits_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_count_bits_i) s) <
  (eval (ENum (576)) s))%Z)) 20)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_count_bits_i) s) >= (eval (ENum (576))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_count_bits_gfp_dref_off260) s) <> (eval (ENum (0))
  s))%Z)) 11)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_count_bits_gfp_dref_off260) s) = (eval (ENum (0))
  s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 ANone 10)::(EA 10 AWeaken 14)::
  (EA 11 AWeaken 12)::(EA 12 ANone 13)::(EA 13 AWeaken 14)::
  (EA 14 ANone 16)::(EA 14 ANone 15)::(EA 15 ANone 17)::(EA 16 ANone 17)::
  (EA 17 (AAssign V_count_bits__tmp None) 18)::(EA 18 ANone 19)::
  (EA 19 AWeaken 31)::(EA 20 AWeaken 21)::(EA 21 ANone 28)::
  (EA 21 ANone 22)::(EA 22 ANone 23)::(EA 23 (AAssign V_count_bits_i
  (Some (EAdd (EVar V_count_bits_i) (ENum (1))))) 24)::(EA 24 ANone 25)::
  (EA 25 ANone 26)::(EA 26 (AAssign V_count_bits_z (Some (EAdd (ENum (1))
  (EVar V_count_bits_z)))) 27)::(EA 27 AWeaken 5)::(EA 28 (AAssign
  V_count_bits__tmp None) 29)::(EA 29 ANone 30)::(EA 30 AWeaken 31)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_count_bits => Pedges_count_bits
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_count_bits => 31
     end)%positive;
  var_global := var_global
}.

Definition ai_count_bits (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_z <= 0)%Z
   | 3 => (-1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i <= 0 /\ -1 * s V_count_bits_i <= 0)%Z
   | 4 => (-1 * s V_count_bits_i <= 0 /\ 1 * s V_count_bits_i <= 0 /\ 1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_z <= 0)%Z
   | 5 => (-1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i <= 0 /\ 1 * s V_count_bits_i + -576 <= 0)%Z
   | 6 => (1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i + 576 <= 0)%Z
   | 7 => (-1 * s V_count_bits_i + 576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i + -576 <= 0)%Z
   | 8 => (1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i + 576 <= 0 /\ 1 * s V_count_bits_gfp_dref_off260 <= 0 /\ -1 * s V_count_bits_gfp_dref_off260 <= 0)%Z
   | 9 => (-1 * s V_count_bits_gfp_dref_off260 <= 0 /\ 1 * s V_count_bits_gfp_dref_off260 <= 0 /\ -1 * s V_count_bits_i + 576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i + -576 <= 0)%Z
   | 10 => (1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i + 576 <= 0 /\ 1 * s V_count_bits_gfp_dref_off260 <= 0 /\ -1 * s V_count_bits_gfp_dref_off260 <= 0)%Z
   | 11 => (1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i + 576 <= 0)%Z
   | 12 => (-1 * s V_count_bits_i + 576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i + -576 <= 0)%Z
   | 13 => (1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i + 576 <= 0)%Z
   | 14 => (-1 * s V_count_bits_i + 576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i + -576 <= 0)%Z
   | 15 => (1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i + 576 <= 0)%Z
   | 16 => (1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i + 576 <= 0)%Z
   | 17 => (-1 * s V_count_bits_i + 576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i + -576 <= 0)%Z
   | 18 => (1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i + 576 <= 0)%Z
   | 19 => (-1 * s V_count_bits_i + 576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i + -576 <= 0)%Z
   | 20 => (-1 * s V_count_bits_i <= 0 /\ -1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i + -575 <= 0)%Z
   | 21 => (1 * s V_count_bits_i + -575 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i <= 0)%Z
   | 22 => (-1 * s V_count_bits_i <= 0 /\ -1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i + -575 <= 0)%Z
   | 23 => (1 * s V_count_bits_i + -575 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i <= 0)%Z
   | 24 => (-1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_i + 1 <= 0)%Z
   | 25 => (-1 * s V_count_bits_i + 1 <= 0 /\ 1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_z <= 0)%Z
   | 26 => (-1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_i + 1 <= 0)%Z
   | 27 => (-1 * s V_count_bits_i + 1 <= 0 /\ 1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_z + 1 <= 0)%Z
   | 28 => (-1 * s V_count_bits_i <= 0 /\ -1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i + -575 <= 0)%Z
   | 29 => (1 * s V_count_bits_i + -575 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i <= 0)%Z
   | 30 => (-1 * s V_count_bits_i <= 0 /\ -1 * s V_count_bits_z <= 0 /\ 1 * s V_count_bits_i + -575 <= 0)%Z
   | 31 => (1 * s V_count_bits_i + -576 <= 0 /\ -1 * s V_count_bits_z <= 0 /\ -1 * s V_count_bits_i <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_count_bits (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((576 # 1) <= z)%Q
   | 2 => ((576 # 1) + s V_count_bits_z <= z)%Q
   | 3 => (s V_count_bits_z + max0(576 - s V_count_bits_i) <= z)%Q
   | 4 => (s V_count_bits_z + max0(576 - s V_count_bits_i) <= z)%Q
   | 5 => (s V_count_bits_z + max0(576 - s V_count_bits_i) <= z)%Q
   | 6 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (576 - s V_count_bits_i)) (F_check_ge (0) (0))]
     (s V_count_bits_z + max0(576 - s V_count_bits_i) <= z)%Q
   | 7 => (s V_count_bits_z <= z)%Q
   | 8 => (s V_count_bits_z <= z)%Q
   | 9 => (s V_count_bits_z <= z)%Q
   | 10 => (s V_count_bits_z <= z)%Q
   | 11 => (s V_count_bits_z <= z)%Q
   | 12 => (s V_count_bits_z <= z)%Q
   | 13 => (s V_count_bits_z <= z)%Q
   | 14 => (s V_count_bits_z <= z)%Q
   | 15 => (s V_count_bits_z <= z)%Q
   | 16 => (s V_count_bits_z <= z)%Q
   | 17 => (s V_count_bits_z <= z)%Q
   | 18 => (s V_count_bits_z <= z)%Q
   | 19 => (s V_count_bits_z <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (576 - s V_count_bits_i) (1);
      (*-0.00173913 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (575
                                                                    - s V_count_bits_i) (0))) (F_max0_ge_0 (575
                                                                    - s V_count_bits_i))]
     (s V_count_bits_z + max0(576 - s V_count_bits_i) <= z)%Q
   | 21 => ((0 # 1) * s V_count_bits_i + s V_count_bits_z
            + (1 # 1) * max0(575 - s V_count_bits_i) <= z)%Q
   | 22 => ((0 # 1) * s V_count_bits_i + s V_count_bits_z
            + (1 # 1) * max0(575 - s V_count_bits_i) <= z)%Q
   | 23 => ((0 # 1) * s V_count_bits_i + s V_count_bits_z
            + (1 # 1) * max0(575 - s V_count_bits_i) <= z)%Q
   | 24 => (-(0 # 1) + (0 # 1) * s V_count_bits_i + s V_count_bits_z
            + (1 # 1) * max0(576 - s V_count_bits_i) <= z)%Q
   | 25 => (-(0 # 1) + (0 # 1) * s V_count_bits_i + s V_count_bits_z
            + (1 # 1) * max0(576 - s V_count_bits_i) <= z)%Q
   | 26 => (-(0 # 1) + (0 # 1) * s V_count_bits_i + s V_count_bits_z
            + (1 # 1) * max0(576 - s V_count_bits_i) <= z)%Q
   | 27 => hints
     [(*-0.00173913 0*) F_binom_monotonic 1 (F_max0_ge_arg (576
                                                            - s V_count_bits_i)) (F_check_ge (576
                                                                    - s V_count_bits_i) (0))]
     (-(1 # 1) + (0 # 1) * s V_count_bits_i + s V_count_bits_z
      + (1 # 1) * max0(576 - s V_count_bits_i) <= z)%Q
   | 28 => ((0 # 1) * s V_count_bits_i + s V_count_bits_z
            + (1 # 1) * max0(575 - s V_count_bits_i) <= z)%Q
   | 29 => ((0 # 1) * s V_count_bits_i + s V_count_bits_z
            + (1 # 1) * max0(575 - s V_count_bits_i) <= z)%Q
   | 30 => hints
     [(*-1.00174 0*) F_max0_ge_0 (575 - s V_count_bits_i);
      (*-0.00173913 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_count_bits_i)) (F_check_ge (0) (0));
      (*-0.00173913 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_count_bits_i) (0))) (F_max0_ge_0 (s V_count_bits_i))]
     ((0 # 1) * s V_count_bits_i + s V_count_bits_z
      + (1 # 1) * max0(575 - s V_count_bits_i) <= z)%Q
   | 31 => (s V_count_bits_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_count_bits =>
    [mkPA Q (fun n z s => ai_count_bits n s /\ annot0_count_bits n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_count_bits (proc_start P_count_bits) s1 (proc_end P_count_bits) s2 ->
    (s2 V_count_bits_z <= (576 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_count_bits.
Qed.
