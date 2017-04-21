Require Import pasta.Pasta.

Inductive proc: Type :=
  P_BF_PartLength.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_BF_PartLength_z := 1%positive.
Notation V_BF_PartLength_bits := 2%positive.
Notation V_BF_PartLength_i := 3%positive.
Notation V_BF_PartLength_part_dref_off0 := 4%positive.
Notation V_BF_PartLength_part := 5%positive.
Definition Pedges_BF_PartLength: list (edge proc) :=
  (EA 1 (AAssign V_BF_PartLength_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_BF_PartLength_part_dref_off0) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_BF_PartLength_i) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_BF_PartLength_bits
  (Some (ENum (0)))) 6)::(EA 6 (AAssign V_BF_PartLength_i
  (Some (ENum (0)))) 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_BF_PartLength_i) s) <
  (eval (EVar V_BF_PartLength_part_dref_off0) s))%Z)) 12)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_BF_PartLength_i) s) >=
  (eval (EVar V_BF_PartLength_part_dref_off0) s))%Z)) 10)::
  (EA 10 AWeaken 11)::(EA 12 AWeaken 13)::(EA 13 (AAssign
  V_BF_PartLength_bits None) 14)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_BF_PartLength_i (Some (EAdd (EVar V_BF_PartLength_i) (ENum (1))))) 16)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_BF_PartLength_z
  (Some (EAdd (ENum (1)) (EVar V_BF_PartLength_z)))) 19)::(EA 19 AWeaken 9)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_BF_PartLength => Pedges_BF_PartLength
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_BF_PartLength => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_BF_PartLength (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_z <= 0)%Z
   | 3 => (-1 * s V_BF_PartLength_z <= 0 /\ 1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_part_dref_off0 <= 0)%Z
   | 4 => (-1 * s V_BF_PartLength_part_dref_off0 <= 0 /\ 1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_i <= 0)%Z
   | 5 => (-1 * s V_BF_PartLength_i <= 0 /\ -1 * s V_BF_PartLength_z <= 0 /\ 1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_part_dref_off0 <= 0)%Z
   | 6 => (-1 * s V_BF_PartLength_part_dref_off0 <= 0 /\ 1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_i <= 0 /\ 1 * s V_BF_PartLength_bits <= 0 /\ -1 * s V_BF_PartLength_bits <= 0)%Z
   | 7 => (-1 * s V_BF_PartLength_bits <= 0 /\ 1 * s V_BF_PartLength_bits <= 0 /\ -1 * s V_BF_PartLength_z <= 0 /\ 1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_part_dref_off0 <= 0 /\ 1 * s V_BF_PartLength_i <= 0 /\ -1 * s V_BF_PartLength_i <= 0)%Z
   | 8 => (-1 * s V_BF_PartLength_i <= 0 /\ 1 * s V_BF_PartLength_i <= 0 /\ -1 * s V_BF_PartLength_part_dref_off0 <= 0 /\ 1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_z <= 0 /\ 1 * s V_BF_PartLength_bits <= 0 /\ -1 * s V_BF_PartLength_bits <= 0)%Z
   | 9 => (-1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_i <= 0 /\ 1 * s V_BF_PartLength_i+ -1 * s V_BF_PartLength_part_dref_off0 <= 0)%Z
   | 10 => (1 * s V_BF_PartLength_i+ -1 * s V_BF_PartLength_part_dref_off0 <= 0 /\ -1 * s V_BF_PartLength_i <= 0 /\ -1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_i+ 1 * s V_BF_PartLength_part_dref_off0 <= 0)%Z
   | 11 => (-1 * s V_BF_PartLength_i+ 1 * s V_BF_PartLength_part_dref_off0 <= 0 /\ -1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_i <= 0 /\ 1 * s V_BF_PartLength_i+ -1 * s V_BF_PartLength_part_dref_off0 <= 0)%Z
   | 12 => (-1 * s V_BF_PartLength_i <= 0 /\ -1 * s V_BF_PartLength_z <= 0 /\ 1 * s V_BF_PartLength_i+ -1 * s V_BF_PartLength_part_dref_off0 + 1 <= 0)%Z
   | 13 => (1 * s V_BF_PartLength_i+ -1 * s V_BF_PartLength_part_dref_off0 + 1 <= 0 /\ -1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_i <= 0)%Z
   | 14 => (-1 * s V_BF_PartLength_i <= 0 /\ -1 * s V_BF_PartLength_z <= 0 /\ 1 * s V_BF_PartLength_i+ -1 * s V_BF_PartLength_part_dref_off0 + 1 <= 0)%Z
   | 15 => (1 * s V_BF_PartLength_i+ -1 * s V_BF_PartLength_part_dref_off0 + 1 <= 0 /\ -1 * s V_BF_PartLength_z <= 0 /\ -1 * s V_BF_PartLength_i <= 0)%Z
   | 16 => (-1 * s V_BF_PartLength_z <= 0 /\ 1 * s V_BF_PartLength_i+ -1 * s V_BF_PartLength_part_dref_off0 <= 0 /\ -1 * s V_BF_PartLength_i + 1 <= 0)%Z
   | 17 => (-1 * s V_BF_PartLength_i + 1 <= 0 /\ 1 * s V_BF_PartLength_i+ -1 * s V_BF_PartLength_part_dref_off0 <= 0 /\ -1 * s V_BF_PartLength_z <= 0)%Z
   | 18 => (-1 * s V_BF_PartLength_z <= 0 /\ 1 * s V_BF_PartLength_i+ -1 * s V_BF_PartLength_part_dref_off0 <= 0 /\ -1 * s V_BF_PartLength_i + 1 <= 0)%Z
   | 19 => (-1 * s V_BF_PartLength_i + 1 <= 0 /\ 1 * s V_BF_PartLength_i+ -1 * s V_BF_PartLength_part_dref_off0 <= 0 /\ -1 * s V_BF_PartLength_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_BF_PartLength (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 2 => (s V_BF_PartLength_z + max0(s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 3 => (s V_BF_PartLength_z + max0(s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 4 => (s V_BF_PartLength_z + max0(s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 5 => (s V_BF_PartLength_z + max0(s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 6 => (s V_BF_PartLength_z + max0(s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 7 => (s V_BF_PartLength_z
           + max0(-s V_BF_PartLength_i + s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 8 => (s V_BF_PartLength_z
           + max0(-s V_BF_PartLength_i + s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 9 => (s V_BF_PartLength_z
           + max0(-s V_BF_PartLength_i + s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_BF_PartLength_i
                                             + s V_BF_PartLength_part_dref_off0) (-1
                                                                    - s V_BF_PartLength_i
                                                                    + s V_BF_PartLength_part_dref_off0));
      (*-1 0*) F_max0_ge_0 (-1 - s V_BF_PartLength_i
                            + s V_BF_PartLength_part_dref_off0)]
     (s V_BF_PartLength_z
      + max0(-s V_BF_PartLength_i + s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 11 => (s V_BF_PartLength_z <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_BF_PartLength_i
                                       + s V_BF_PartLength_part_dref_off0) (1)]
     (s V_BF_PartLength_z
      + max0(-s V_BF_PartLength_i + s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 13 => ((1 # 1) + s V_BF_PartLength_z
            + max0(-1 - s V_BF_PartLength_i
                   + s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 14 => ((1 # 1) + s V_BF_PartLength_z
            + max0(-1 - s V_BF_PartLength_i
                   + s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 15 => ((1 # 1) + s V_BF_PartLength_z
            + max0(-1 - s V_BF_PartLength_i
                   + s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 16 => ((1 # 1) + s V_BF_PartLength_z
            + max0(-s V_BF_PartLength_i + s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 17 => ((1 # 1) + s V_BF_PartLength_z
            + max0(-s V_BF_PartLength_i + s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 18 => ((1 # 1) + s V_BF_PartLength_z
            + max0(-s V_BF_PartLength_i + s V_BF_PartLength_part_dref_off0) <= z)%Q
   | 19 => (s V_BF_PartLength_z
            + max0(-s V_BF_PartLength_i + s V_BF_PartLength_part_dref_off0) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_BF_PartLength =>
    [mkPA Q (fun n z s => ai_BF_PartLength n s /\ annot0_BF_PartLength n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_BF_PartLength (proc_start P_BF_PartLength) s1 (proc_end P_BF_PartLength) s2 ->
    (s2 V_BF_PartLength_z <= max0(s1 V_BF_PartLength_part_dref_off0))%Q.
Proof.
  prove_bound ipa admissible_ipa P_BF_PartLength.
Qed.
