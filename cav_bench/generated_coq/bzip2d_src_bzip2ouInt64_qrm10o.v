Require Import pasta.Pasta.

Inductive proc: Type :=
  P_uInt64_qrm10.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_uInt64_qrm10_z := 1%positive.
Notation V_uInt64_qrm10_i := 2%positive.
Notation V_uInt64_qrm10_rem := 3%positive.
Notation V_uInt64_qrm10_tmp := 4%positive.
Notation V_uInt64_qrm10_n := 5%positive.
Definition Pedges_uInt64_qrm10: list (edge proc) :=
  (EA 1 (AAssign V_uInt64_qrm10_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_uInt64_qrm10_rem (Some (ENum (0)))) 3)::(EA 3 (AAssign V_uInt64_qrm10_i
  (Some (ENum (7)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_uInt64_qrm10_i) s) >= (eval (ENum (0))
  s))%Z)) 9)::(EA 6 (AGuard (fun s => ((eval (EVar V_uInt64_qrm10_i) s) <
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 (AAssign V_uInt64_qrm10_tmp None) 11)::(EA 11 (AAssign
  V_uInt64_qrm10_rem None) 12)::(EA 12 ANone 13)::(EA 13 (AAssign
  V_uInt64_qrm10_i (Some (EAdd (EVar V_uInt64_qrm10_i) (ENum (-1))))) 14)::
  (EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign V_uInt64_qrm10_z
  (Some (EAdd (ENum (1)) (EVar V_uInt64_qrm10_z)))) 17)::(EA 17 AWeaken 6)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_uInt64_qrm10 => Pedges_uInt64_qrm10
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_uInt64_qrm10 => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_uInt64_qrm10 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_uInt64_qrm10_z <= 0 /\ -1 * s V_uInt64_qrm10_z <= 0)%Z
   | 3 => (-1 * s V_uInt64_qrm10_z <= 0 /\ 1 * s V_uInt64_qrm10_z <= 0 /\ 1 * s V_uInt64_qrm10_rem <= 0 /\ -1 * s V_uInt64_qrm10_rem <= 0)%Z
   | 4 => (-1 * s V_uInt64_qrm10_rem <= 0 /\ 1 * s V_uInt64_qrm10_rem <= 0 /\ 1 * s V_uInt64_qrm10_z <= 0 /\ -1 * s V_uInt64_qrm10_z <= 0 /\ 1 * s V_uInt64_qrm10_i + -7 <= 0 /\ -1 * s V_uInt64_qrm10_i + 7 <= 0)%Z
   | 5 => (-1 * s V_uInt64_qrm10_i + 7 <= 0 /\ 1 * s V_uInt64_qrm10_i + -7 <= 0 /\ -1 * s V_uInt64_qrm10_z <= 0 /\ 1 * s V_uInt64_qrm10_z <= 0 /\ 1 * s V_uInt64_qrm10_rem <= 0 /\ -1 * s V_uInt64_qrm10_rem <= 0)%Z
   | 6 => (-1 * s V_uInt64_qrm10_z <= 0 /\ 1 * s V_uInt64_qrm10_i + -7 <= 0 /\ -1 * s V_uInt64_qrm10_i + -1 <= 0)%Z
   | 7 => (-1 * s V_uInt64_qrm10_i + -1 <= 0 /\ -1 * s V_uInt64_qrm10_z <= 0 /\ 1 * s V_uInt64_qrm10_i + 1 <= 0)%Z
   | 8 => (1 * s V_uInt64_qrm10_i + 1 <= 0 /\ -1 * s V_uInt64_qrm10_z <= 0 /\ -1 * s V_uInt64_qrm10_i + -1 <= 0)%Z
   | 9 => (1 * s V_uInt64_qrm10_i + -7 <= 0 /\ -1 * s V_uInt64_qrm10_z <= 0 /\ -1 * s V_uInt64_qrm10_i <= 0)%Z
   | 10 => (-1 * s V_uInt64_qrm10_i <= 0 /\ -1 * s V_uInt64_qrm10_z <= 0 /\ 1 * s V_uInt64_qrm10_i + -7 <= 0)%Z
   | 11 => (1 * s V_uInt64_qrm10_i + -7 <= 0 /\ -1 * s V_uInt64_qrm10_z <= 0 /\ -1 * s V_uInt64_qrm10_i <= 0)%Z
   | 12 => (-1 * s V_uInt64_qrm10_i <= 0 /\ -1 * s V_uInt64_qrm10_z <= 0 /\ 1 * s V_uInt64_qrm10_i + -7 <= 0)%Z
   | 13 => (1 * s V_uInt64_qrm10_i + -7 <= 0 /\ -1 * s V_uInt64_qrm10_z <= 0 /\ -1 * s V_uInt64_qrm10_i <= 0)%Z
   | 14 => (-1 * s V_uInt64_qrm10_z <= 0 /\ 1 * s V_uInt64_qrm10_i + -6 <= 0 /\ -1 * s V_uInt64_qrm10_i + -1 <= 0)%Z
   | 15 => (-1 * s V_uInt64_qrm10_i + -1 <= 0 /\ 1 * s V_uInt64_qrm10_i + -6 <= 0 /\ -1 * s V_uInt64_qrm10_z <= 0)%Z
   | 16 => (-1 * s V_uInt64_qrm10_z <= 0 /\ 1 * s V_uInt64_qrm10_i + -6 <= 0 /\ -1 * s V_uInt64_qrm10_i + -1 <= 0)%Z
   | 17 => (-1 * s V_uInt64_qrm10_i + -1 <= 0 /\ 1 * s V_uInt64_qrm10_i + -6 <= 0 /\ -1 * s V_uInt64_qrm10_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_uInt64_qrm10 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_uInt64_qrm10_z <= z)%Q
   | 3 => ((8 # 1) + s V_uInt64_qrm10_z <= z)%Q
   | 4 => (s V_uInt64_qrm10_z + max0(1 + s V_uInt64_qrm10_i) <= z)%Q
   | 5 => (s V_uInt64_qrm10_z + max0(1 + s V_uInt64_qrm10_i) <= z)%Q
   | 6 => (s V_uInt64_qrm10_z + max0(1 + s V_uInt64_qrm10_i) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_uInt64_qrm10_i) (s V_uInt64_qrm10_i));
      (*-1 0*) F_max0_ge_0 (s V_uInt64_qrm10_i)]
     (s V_uInt64_qrm10_z + max0(1 + s V_uInt64_qrm10_i) <= z)%Q
   | 8 => (s V_uInt64_qrm10_z <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1 + s V_uInt64_qrm10_i) (1)]
     (s V_uInt64_qrm10_z + max0(1 + s V_uInt64_qrm10_i) <= z)%Q
   | 10 => ((1 # 1) + s V_uInt64_qrm10_z + max0(s V_uInt64_qrm10_i) <= z)%Q
   | 11 => ((1 # 1) + s V_uInt64_qrm10_z + max0(s V_uInt64_qrm10_i) <= z)%Q
   | 12 => ((1 # 1) + s V_uInt64_qrm10_z + max0(s V_uInt64_qrm10_i) <= z)%Q
   | 13 => ((1 # 1) + s V_uInt64_qrm10_z + max0(s V_uInt64_qrm10_i) <= z)%Q
   | 14 => ((1 # 1) + s V_uInt64_qrm10_z + max0(1 + s V_uInt64_qrm10_i) <= z)%Q
   | 15 => ((1 # 1) + s V_uInt64_qrm10_z + max0(1 + s V_uInt64_qrm10_i) <= z)%Q
   | 16 => ((1 # 1) + s V_uInt64_qrm10_z + max0(1 + s V_uInt64_qrm10_i) <= z)%Q
   | 17 => (s V_uInt64_qrm10_z + max0(1 + s V_uInt64_qrm10_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_uInt64_qrm10 =>
    [mkPA Q (fun n z s => ai_uInt64_qrm10 n s /\ annot0_uInt64_qrm10 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_uInt64_qrm10 (proc_start P_uInt64_qrm10) s1 (proc_end P_uInt64_qrm10) s2 ->
    (s2 V_uInt64_qrm10_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_uInt64_qrm10.
Qed.
