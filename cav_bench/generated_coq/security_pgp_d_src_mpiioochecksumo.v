Require Import pasta.Pasta.

Inductive proc: Type :=
  P_checksum.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_checksum_z := 1%positive.
Notation V_checksum__tmp := 2%positive.
Notation V_checksum_cs := 3%positive.
Notation V_checksum_buf := 4%positive.
Notation V_checksum_count := 5%positive.
Definition Pedges_checksum: list (edge proc) :=
  (EA 1 (AAssign V_checksum_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_checksum__tmp (Some (EVar V_checksum_count))) 3)::(EA 3 (AAssign
  V_checksum_cs (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 (AAssign
  V_checksum__tmp (Some (EAdd (EVar V_checksum__tmp) (ENum (-1))))) 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard (fun s => ((eval (EVar V_checksum__tmp)
  s) <> (eval (ENum (0)) s))%Z)) 10)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_checksum__tmp) s) = (eval (ENum (0)) s))%Z)) 8)::
  (EA 8 AWeaken 9)::(EA 10 AWeaken 11)::(EA 11 (AAssign V_checksum_cs
  None) 12)::(EA 12 ANone 13)::(EA 13 ANone 14)::(EA 14 (AAssign V_checksum_z
  (Some (EAdd (ENum (1)) (EVar V_checksum_z)))) 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_checksum => Pedges_checksum
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_checksum => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_checksum (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_checksum_z <= 0 /\ -1 * s V_checksum_z <= 0)%Z
   | 3 => (-1 * s V_checksum_z <= 0 /\ 1 * s V_checksum_z <= 0)%Z
   | 4 => (1 * s V_checksum_z <= 0 /\ -1 * s V_checksum_z <= 0 /\ 1 * s V_checksum_cs <= 0 /\ -1 * s V_checksum_cs <= 0)%Z
   | 5 => (-1 * s V_checksum_z <= 0)%Z
   | 6 => (-1 * s V_checksum_z <= 0)%Z
   | 7 => (-1 * s V_checksum_z <= 0)%Z
   | 8 => (-1 * s V_checksum_z <= 0 /\ 1 * s V_checksum__tmp <= 0 /\ -1 * s V_checksum__tmp <= 0)%Z
   | 9 => (-1 * s V_checksum__tmp <= 0 /\ 1 * s V_checksum__tmp <= 0 /\ -1 * s V_checksum_z <= 0)%Z
   | 10 => (-1 * s V_checksum_z <= 0)%Z
   | 11 => (-1 * s V_checksum_z <= 0)%Z
   | 12 => (-1 * s V_checksum_z <= 0)%Z
   | 13 => (-1 * s V_checksum_z <= 0)%Z
   | 14 => (-1 * s V_checksum_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_checksum (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_checksum_count <= z)%Q
   | 2 => (s V_checksum_count + s V_checksum_z <= z)%Q
   | 3 => (s V_checksum__tmp + s V_checksum_z <= z)%Q
   | 4 => (s V_checksum__tmp + s V_checksum_z <= z)%Q
   | 5 => (s V_checksum__tmp + s V_checksum_z <= z)%Q
   | 6 => ((1 # 1) + s V_checksum__tmp + s V_checksum_z <= z)%Q
   | 7 => ((1 # 1) + s V_checksum__tmp + s V_checksum_z <= z)%Q
   | 8 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_checksum__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_checksum__tmp) (0))) (F_max0_ge_0 (s V_checksum__tmp))]
     ((1 # 1) + s V_checksum__tmp + s V_checksum_z <= z)%Q
   | 9 => (s V_checksum_z <= z)%Q
   | 10 => ((1 # 1) + s V_checksum__tmp + s V_checksum_z <= z)%Q
   | 11 => ((1 # 1) + s V_checksum__tmp + s V_checksum_z <= z)%Q
   | 12 => ((1 # 1) + s V_checksum__tmp + s V_checksum_z <= z)%Q
   | 13 => ((1 # 1) + s V_checksum__tmp + s V_checksum_z <= z)%Q
   | 14 => ((1 # 1) + s V_checksum__tmp + s V_checksum_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_checksum =>
    [mkPA Q (fun n z s => ai_checksum n s /\ annot0_checksum n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_checksum (proc_start P_checksum) s1 (proc_end P_checksum) s2 ->
    (s2 V_checksum_z <= s1 V_checksum_count)%Q.
Proof.
  prove_bound ipa admissible_ipa P_checksum.
Qed.