Require Import pasta.Pasta.

Inductive proc: Type :=
  P_Coefficients_13_26.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_Coefficients_13_26_z := 1%positive.
Notation V_Coefficients_13_26_i := 2%positive.
Notation V_Coefficients_13_26_ltmp := 3%positive.
Notation V_Coefficients_13_26_LARp := 4%positive.
Notation V_Coefficients_13_26_LARpp_j := 5%positive.
Notation V_Coefficients_13_26_LARpp_j_1 := 6%positive.
Definition Pedges_Coefficients_13_26: list (edge proc) :=
  (EA 1 (AAssign V_Coefficients_13_26_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_Coefficients_13_26_i (Some (ENum (1)))) 3)::(EA 3 ANone 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_Coefficients_13_26_i) s) <= (eval (ENum (8))
  s))%Z)) 8)::(EA 5 (AGuard (fun s => ((eval (EVar V_Coefficients_13_26_i)
  s) > (eval (ENum (8)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 8 AWeaken 9)::
  (EA 9 (AAssign V_Coefficients_13_26_ltmp None) 10)::(EA 10 AWeaken 11)::
  (EA 11 ANone 13)::(EA 11 ANone 12)::(EA 12 ANone 14)::(EA 13 ANone 14)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_Coefficients_13_26_i
  (Some (EAdd (EVar V_Coefficients_13_26_i) (ENum (1))))) 16)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_Coefficients_13_26_z
  (Some (EAdd (ENum (1)) (EVar V_Coefficients_13_26_z)))) 19)::
  (EA 19 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_Coefficients_13_26 => Pedges_Coefficients_13_26
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_Coefficients_13_26 => 7
     end)%positive;
  var_global := var_global
}.

Definition ai_Coefficients_13_26 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_Coefficients_13_26_z <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0)%Z
   | 3 => (-1 * s V_Coefficients_13_26_z <= 0 /\ 1 * s V_Coefficients_13_26_z <= 0 /\ 1 * s V_Coefficients_13_26_i + -1 <= 0 /\ -1 * s V_Coefficients_13_26_i + 1 <= 0)%Z
   | 4 => (-1 * s V_Coefficients_13_26_i + 1 <= 0 /\ 1 * s V_Coefficients_13_26_i + -1 <= 0 /\ 1 * s V_Coefficients_13_26_z <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0)%Z
   | 5 => (-1 * s V_Coefficients_13_26_z <= 0 /\ -1 * s V_Coefficients_13_26_i + 1 <= 0 /\ 1 * s V_Coefficients_13_26_i + -9 <= 0)%Z
   | 6 => (1 * s V_Coefficients_13_26_i + -9 <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0 /\ -1 * s V_Coefficients_13_26_i + 9 <= 0)%Z
   | 7 => (-1 * s V_Coefficients_13_26_i + 9 <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0 /\ 1 * s V_Coefficients_13_26_i + -9 <= 0)%Z
   | 8 => (-1 * s V_Coefficients_13_26_i + 1 <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0 /\ 1 * s V_Coefficients_13_26_i + -8 <= 0)%Z
   | 9 => (1 * s V_Coefficients_13_26_i + -8 <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0 /\ -1 * s V_Coefficients_13_26_i + 1 <= 0)%Z
   | 10 => (-1 * s V_Coefficients_13_26_i + 1 <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0 /\ 1 * s V_Coefficients_13_26_i + -8 <= 0)%Z
   | 11 => (1 * s V_Coefficients_13_26_i + -8 <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0 /\ -1 * s V_Coefficients_13_26_i + 1 <= 0)%Z
   | 12 => (-1 * s V_Coefficients_13_26_i + 1 <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0 /\ 1 * s V_Coefficients_13_26_i + -8 <= 0)%Z
   | 13 => (-1 * s V_Coefficients_13_26_i + 1 <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0 /\ 1 * s V_Coefficients_13_26_i + -8 <= 0)%Z
   | 14 => (1 * s V_Coefficients_13_26_i + -8 <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0 /\ -1 * s V_Coefficients_13_26_i + 1 <= 0)%Z
   | 15 => (-1 * s V_Coefficients_13_26_i + 1 <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0 /\ 1 * s V_Coefficients_13_26_i + -8 <= 0)%Z
   | 16 => (-1 * s V_Coefficients_13_26_z <= 0 /\ -1 * s V_Coefficients_13_26_i + 2 <= 0 /\ 1 * s V_Coefficients_13_26_i + -9 <= 0)%Z
   | 17 => (1 * s V_Coefficients_13_26_i + -9 <= 0 /\ -1 * s V_Coefficients_13_26_i + 2 <= 0 /\ -1 * s V_Coefficients_13_26_z <= 0)%Z
   | 18 => (-1 * s V_Coefficients_13_26_z <= 0 /\ -1 * s V_Coefficients_13_26_i + 2 <= 0 /\ 1 * s V_Coefficients_13_26_i + -9 <= 0)%Z
   | 19 => (1 * s V_Coefficients_13_26_i + -9 <= 0 /\ -1 * s V_Coefficients_13_26_i + 2 <= 0 /\ -1 * s V_Coefficients_13_26_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_Coefficients_13_26 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_Coefficients_13_26_z <= z)%Q
   | 3 => (s V_Coefficients_13_26_z + max0(9 - s V_Coefficients_13_26_i) <= z)%Q
   | 4 => (s V_Coefficients_13_26_z + max0(9 - s V_Coefficients_13_26_i) <= z)%Q
   | 5 => (s V_Coefficients_13_26_z + max0(9 - s V_Coefficients_13_26_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (9 - s V_Coefficients_13_26_i) (8
                                                                    - s V_Coefficients_13_26_i));
      (*-1 0*) F_max0_ge_0 (8 - s V_Coefficients_13_26_i)]
     (s V_Coefficients_13_26_z + max0(9 - s V_Coefficients_13_26_i) <= z)%Q
   | 7 => (s V_Coefficients_13_26_z <= z)%Q
   | 8 => hints
     [(*0 1*) F_max0_pre_decrement 1 (9 - s V_Coefficients_13_26_i) (1)]
     (s V_Coefficients_13_26_z + max0(9 - s V_Coefficients_13_26_i) <= z)%Q
   | 9 => ((1 # 1) + s V_Coefficients_13_26_z
           + max0(8 - s V_Coefficients_13_26_i) <= z)%Q
   | 10 => ((1 # 1) + s V_Coefficients_13_26_z
            + max0(8 - s V_Coefficients_13_26_i) <= z)%Q
   | 11 => ((1 # 1) + s V_Coefficients_13_26_z
            + max0(8 - s V_Coefficients_13_26_i) <= z)%Q
   | 12 => ((1 # 1) + s V_Coefficients_13_26_z
            + max0(8 - s V_Coefficients_13_26_i) <= z)%Q
   | 13 => ((1 # 1) + s V_Coefficients_13_26_z
            + max0(8 - s V_Coefficients_13_26_i) <= z)%Q
   | 14 => ((1 # 1) + s V_Coefficients_13_26_z
            + max0(8 - s V_Coefficients_13_26_i) <= z)%Q
   | 15 => ((1 # 1) + s V_Coefficients_13_26_z
            + max0(8 - s V_Coefficients_13_26_i) <= z)%Q
   | 16 => ((1 # 1) + s V_Coefficients_13_26_z
            + max0(9 - s V_Coefficients_13_26_i) <= z)%Q
   | 17 => ((1 # 1) + s V_Coefficients_13_26_z
            + max0(9 - s V_Coefficients_13_26_i) <= z)%Q
   | 18 => ((1 # 1) + s V_Coefficients_13_26_z
            + max0(9 - s V_Coefficients_13_26_i) <= z)%Q
   | 19 => (s V_Coefficients_13_26_z + max0(9 - s V_Coefficients_13_26_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_Coefficients_13_26 =>
    [mkPA Q (fun n z s => ai_Coefficients_13_26 n s /\ annot0_Coefficients_13_26 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_Coefficients_13_26 (proc_start P_Coefficients_13_26) s1 (proc_end P_Coefficients_13_26) s2 ->
    (s2 V_Coefficients_13_26_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_Coefficients_13_26.
Qed.
