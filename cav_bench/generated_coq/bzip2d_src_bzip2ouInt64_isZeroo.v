Require Import pasta.Pasta.

Inductive proc: Type :=
  P_uInt64_isZero.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_uInt64_isZero_z := 1%positive.
Notation V_uInt64_isZero__tmp := 2%positive.
Notation V_uInt64_isZero_i := 3%positive.
Notation V_uInt64_isZero_n := 4%positive.
Definition Pedges_uInt64_isZero: list (edge proc) :=
  (EA 1 (AAssign V_uInt64_isZero_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_uInt64_isZero_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_uInt64_isZero_i) s) <
  (eval (ENum (8)) s))%Z)) 10)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_uInt64_isZero_i) s) >= (eval (ENum (8))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign V_uInt64_isZero__tmp
  (Some (ENum (1)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 21)::
  (EA 10 AWeaken 11)::(EA 11 ANone 18)::(EA 11 ANone 12)::(EA 12 ANone 13)::
  (EA 13 (AAssign V_uInt64_isZero_i (Some (EAdd (EVar V_uInt64_isZero_i)
  (ENum (1))))) 14)::(EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_uInt64_isZero_z (Some (EAdd (ENum (1)) (EVar V_uInt64_isZero_z)))) 17)::
  (EA 17 AWeaken 5)::(EA 18 (AAssign V_uInt64_isZero__tmp
  (Some (ENum (0)))) 19)::(EA 19 ANone 20)::(EA 20 AWeaken 21)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_uInt64_isZero => Pedges_uInt64_isZero
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_uInt64_isZero => 21
     end)%positive;
  var_global := var_global
}.

Definition ai_uInt64_isZero (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_uInt64_isZero_z <= 0 /\ -1 * s V_uInt64_isZero_z <= 0)%Z
   | 3 => (-1 * s V_uInt64_isZero_z <= 0 /\ 1 * s V_uInt64_isZero_z <= 0 /\ 1 * s V_uInt64_isZero_i <= 0 /\ -1 * s V_uInt64_isZero_i <= 0)%Z
   | 4 => (-1 * s V_uInt64_isZero_i <= 0 /\ 1 * s V_uInt64_isZero_i <= 0 /\ 1 * s V_uInt64_isZero_z <= 0 /\ -1 * s V_uInt64_isZero_z <= 0)%Z
   | 5 => (-1 * s V_uInt64_isZero_z <= 0 /\ -1 * s V_uInt64_isZero_i <= 0 /\ 1 * s V_uInt64_isZero_i + -8 <= 0)%Z
   | 6 => (1 * s V_uInt64_isZero_i + -8 <= 0 /\ -1 * s V_uInt64_isZero_z <= 0 /\ -1 * s V_uInt64_isZero_i + 8 <= 0)%Z
   | 7 => (-1 * s V_uInt64_isZero_i + 8 <= 0 /\ -1 * s V_uInt64_isZero_z <= 0 /\ 1 * s V_uInt64_isZero_i + -8 <= 0)%Z
   | 8 => (1 * s V_uInt64_isZero_i + -8 <= 0 /\ -1 * s V_uInt64_isZero_z <= 0 /\ -1 * s V_uInt64_isZero_i + 8 <= 0 /\ 1 * s V_uInt64_isZero__tmp + -1 <= 0 /\ -1 * s V_uInt64_isZero__tmp + 1 <= 0)%Z
   | 9 => (-1 * s V_uInt64_isZero__tmp + 1 <= 0 /\ 1 * s V_uInt64_isZero__tmp + -1 <= 0 /\ -1 * s V_uInt64_isZero_i + 8 <= 0 /\ -1 * s V_uInt64_isZero_z <= 0 /\ 1 * s V_uInt64_isZero_i + -8 <= 0)%Z
   | 10 => (-1 * s V_uInt64_isZero_i <= 0 /\ -1 * s V_uInt64_isZero_z <= 0 /\ 1 * s V_uInt64_isZero_i + -7 <= 0)%Z
   | 11 => (1 * s V_uInt64_isZero_i + -7 <= 0 /\ -1 * s V_uInt64_isZero_z <= 0 /\ -1 * s V_uInt64_isZero_i <= 0)%Z
   | 12 => (-1 * s V_uInt64_isZero_i <= 0 /\ -1 * s V_uInt64_isZero_z <= 0 /\ 1 * s V_uInt64_isZero_i + -7 <= 0)%Z
   | 13 => (1 * s V_uInt64_isZero_i + -7 <= 0 /\ -1 * s V_uInt64_isZero_z <= 0 /\ -1 * s V_uInt64_isZero_i <= 0)%Z
   | 14 => (-1 * s V_uInt64_isZero_z <= 0 /\ 1 * s V_uInt64_isZero_i + -8 <= 0 /\ -1 * s V_uInt64_isZero_i + 1 <= 0)%Z
   | 15 => (-1 * s V_uInt64_isZero_i + 1 <= 0 /\ 1 * s V_uInt64_isZero_i + -8 <= 0 /\ -1 * s V_uInt64_isZero_z <= 0)%Z
   | 16 => (-1 * s V_uInt64_isZero_z <= 0 /\ 1 * s V_uInt64_isZero_i + -8 <= 0 /\ -1 * s V_uInt64_isZero_i + 1 <= 0)%Z
   | 17 => (-1 * s V_uInt64_isZero_i + 1 <= 0 /\ 1 * s V_uInt64_isZero_i + -8 <= 0 /\ -1 * s V_uInt64_isZero_z + 1 <= 0)%Z
   | 18 => (-1 * s V_uInt64_isZero_i <= 0 /\ -1 * s V_uInt64_isZero_z <= 0 /\ 1 * s V_uInt64_isZero_i + -7 <= 0)%Z
   | 19 => (1 * s V_uInt64_isZero_i + -7 <= 0 /\ -1 * s V_uInt64_isZero_z <= 0 /\ -1 * s V_uInt64_isZero_i <= 0 /\ 1 * s V_uInt64_isZero__tmp <= 0 /\ -1 * s V_uInt64_isZero__tmp <= 0)%Z
   | 20 => (-1 * s V_uInt64_isZero__tmp <= 0 /\ 1 * s V_uInt64_isZero__tmp <= 0 /\ -1 * s V_uInt64_isZero_i <= 0 /\ -1 * s V_uInt64_isZero_z <= 0 /\ 1 * s V_uInt64_isZero_i + -7 <= 0)%Z
   | 21 => (1 * s V_uInt64_isZero_i + -8 <= 0 /\ 1 * s V_uInt64_isZero__tmp + -1 <= 0 /\ -1 * s V_uInt64_isZero_z <= 0 /\ -1 * s V_uInt64_isZero_i <= 0 /\ -1 * s V_uInt64_isZero__tmp <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_uInt64_isZero (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_uInt64_isZero_z <= z)%Q
   | 3 => (s V_uInt64_isZero_z + max0(8 - s V_uInt64_isZero_i) <= z)%Q
   | 4 => (s V_uInt64_isZero_z + max0(8 - s V_uInt64_isZero_i) <= z)%Q
   | 5 => (s V_uInt64_isZero_z + max0(8 - s V_uInt64_isZero_i) <= z)%Q
   | 6 => (s V_uInt64_isZero_z + max0(8 - s V_uInt64_isZero_i) <= z)%Q
   | 7 => (s V_uInt64_isZero_z + max0(8 - s V_uInt64_isZero_i) <= z)%Q
   | 8 => (s V_uInt64_isZero_z + max0(8 - s V_uInt64_isZero_i) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_uInt64_isZero_i) (7
                                                                    - s V_uInt64_isZero_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_uInt64_isZero_i)]
     (s V_uInt64_isZero_z + max0(8 - s V_uInt64_isZero_i) <= z)%Q
   | 10 => hints
     [(*-1 1e-12*) F_max0_pre_decrement 1 (8 - s V_uInt64_isZero_i) (1)]
     (s V_uInt64_isZero_z + max0(8 - s V_uInt64_isZero_i) <= z)%Q
   | 11 => ((1 # 1) + s V_uInt64_isZero_z + max0(7 - s V_uInt64_isZero_i) <= z)%Q
   | 12 => ((1 # 1) + s V_uInt64_isZero_z + max0(7 - s V_uInt64_isZero_i) <= z)%Q
   | 13 => ((1 # 1) + s V_uInt64_isZero_z + max0(7 - s V_uInt64_isZero_i) <= z)%Q
   | 14 => ((1 # 1) + s V_uInt64_isZero_z + max0(8 - s V_uInt64_isZero_i) <= z)%Q
   | 15 => ((1 # 1) + s V_uInt64_isZero_z + max0(8 - s V_uInt64_isZero_i) <= z)%Q
   | 16 => ((1 # 1) + s V_uInt64_isZero_z + max0(8 - s V_uInt64_isZero_i) <= z)%Q
   | 17 => (s V_uInt64_isZero_z + max0(8 - s V_uInt64_isZero_i) <= z)%Q
   | 18 => ((1 # 1) + s V_uInt64_isZero_z + max0(7 - s V_uInt64_isZero_i) <= z)%Q
   | 19 => ((1 # 1) + s V_uInt64_isZero_z + max0(7 - s V_uInt64_isZero_i) <= z)%Q
   | 20 => hints
     [(*-1.14286 0*) F_max0_ge_0 (7 - s V_uInt64_isZero_i);
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_uInt64_isZero_i)) (F_check_ge (0) (0));
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_uInt64_isZero_i) (0))) (F_max0_ge_0 (s V_uInt64_isZero_i));
      (*0 0.142857*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - 
                                                                    s V_uInt64_isZero_i) (0))) (F_max0_ge_0 (7
                                                                    - s V_uInt64_isZero_i))]
     ((1 # 1) + s V_uInt64_isZero_z + max0(7 - s V_uInt64_isZero_i) <= z)%Q
   | 21 => (s V_uInt64_isZero_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_uInt64_isZero =>
    [mkPA Q (fun n z s => ai_uInt64_isZero n s /\ annot0_uInt64_isZero n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_uInt64_isZero (proc_start P_uInt64_isZero) s1 (proc_end P_uInt64_isZero) s2 ->
    (s2 V_uInt64_isZero_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_uInt64_isZero.
Qed.
