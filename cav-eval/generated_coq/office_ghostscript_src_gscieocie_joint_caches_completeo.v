Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cie_joint_caches_complete.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cie_joint_caches_complete_z := 1%positive.
Notation V_cie_joint_caches_complete_j := 2%positive.
Notation V_cie_joint_caches_complete_pcie := 3%positive.
Notation V_cie_joint_caches_complete_pcier := 4%positive.
Notation V_cie_joint_caches_complete_pjc := 5%positive.
Definition Pedges_cie_joint_caches_complete: list (edge proc) :=
  (EA 1 (AAssign V_cie_joint_caches_complete_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_cie_joint_caches_complete_j (Some (ENum (0)))) 3)::
  (EA 3 ANone 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_cie_joint_caches_complete_j) s) < (eval (ENum (3))
  s))%Z)) 8)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_cie_joint_caches_complete_j) s) >=
  (eval (ENum (3)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 8 AWeaken 9)::
  (EA 9 ANone 10)::(EA 10 (AAssign V_cie_joint_caches_complete_j
  (Some (EAdd (EVar V_cie_joint_caches_complete_j) (ENum (1))))) 11)::
  (EA 11 ANone 12)::(EA 12 ANone 13)::(EA 13 (AAssign
  V_cie_joint_caches_complete_z (Some (EAdd (ENum (1))
  (EVar V_cie_joint_caches_complete_z)))) 14)::(EA 14 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cie_joint_caches_complete => Pedges_cie_joint_caches_complete
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cie_joint_caches_complete => 7
     end)%positive;
  var_global := var_global
}.

Definition ai_cie_joint_caches_complete (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cie_joint_caches_complete_z <= 0 /\ -1 * s V_cie_joint_caches_complete_z <= 0)%Z
   | 3 => (-1 * s V_cie_joint_caches_complete_z <= 0 /\ 1 * s V_cie_joint_caches_complete_z <= 0 /\ 1 * s V_cie_joint_caches_complete_j <= 0 /\ -1 * s V_cie_joint_caches_complete_j <= 0)%Z
   | 4 => (-1 * s V_cie_joint_caches_complete_j <= 0 /\ 1 * s V_cie_joint_caches_complete_j <= 0 /\ 1 * s V_cie_joint_caches_complete_z <= 0 /\ -1 * s V_cie_joint_caches_complete_z <= 0)%Z
   | 5 => (-1 * s V_cie_joint_caches_complete_z <= 0 /\ -1 * s V_cie_joint_caches_complete_j <= 0 /\ 1 * s V_cie_joint_caches_complete_j + -3 <= 0)%Z
   | 6 => (1 * s V_cie_joint_caches_complete_j + -3 <= 0 /\ -1 * s V_cie_joint_caches_complete_z <= 0 /\ -1 * s V_cie_joint_caches_complete_j + 3 <= 0)%Z
   | 7 => (-1 * s V_cie_joint_caches_complete_j + 3 <= 0 /\ -1 * s V_cie_joint_caches_complete_z <= 0 /\ 1 * s V_cie_joint_caches_complete_j + -3 <= 0)%Z
   | 8 => (-1 * s V_cie_joint_caches_complete_j <= 0 /\ -1 * s V_cie_joint_caches_complete_z <= 0 /\ 1 * s V_cie_joint_caches_complete_j + -2 <= 0)%Z
   | 9 => (1 * s V_cie_joint_caches_complete_j + -2 <= 0 /\ -1 * s V_cie_joint_caches_complete_z <= 0 /\ -1 * s V_cie_joint_caches_complete_j <= 0)%Z
   | 10 => (-1 * s V_cie_joint_caches_complete_j <= 0 /\ -1 * s V_cie_joint_caches_complete_z <= 0 /\ 1 * s V_cie_joint_caches_complete_j + -2 <= 0)%Z
   | 11 => (-1 * s V_cie_joint_caches_complete_z <= 0 /\ -1 * s V_cie_joint_caches_complete_j + 1 <= 0 /\ 1 * s V_cie_joint_caches_complete_j + -3 <= 0)%Z
   | 12 => (1 * s V_cie_joint_caches_complete_j + -3 <= 0 /\ -1 * s V_cie_joint_caches_complete_j + 1 <= 0 /\ -1 * s V_cie_joint_caches_complete_z <= 0)%Z
   | 13 => (-1 * s V_cie_joint_caches_complete_z <= 0 /\ -1 * s V_cie_joint_caches_complete_j + 1 <= 0 /\ 1 * s V_cie_joint_caches_complete_j + -3 <= 0)%Z
   | 14 => (1 * s V_cie_joint_caches_complete_j + -3 <= 0 /\ -1 * s V_cie_joint_caches_complete_j + 1 <= 0 /\ -1 * s V_cie_joint_caches_complete_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cie_joint_caches_complete (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((3 # 1) <= z)%Q
   | 2 => ((3 # 1) + s V_cie_joint_caches_complete_z <= z)%Q
   | 3 => (s V_cie_joint_caches_complete_z
           + max0(3 - s V_cie_joint_caches_complete_j) <= z)%Q
   | 4 => (s V_cie_joint_caches_complete_z
           + max0(3 - s V_cie_joint_caches_complete_j) <= z)%Q
   | 5 => (s V_cie_joint_caches_complete_z
           + max0(3 - s V_cie_joint_caches_complete_j) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3
                                             - s V_cie_joint_caches_complete_j) (2
                                                                    - s V_cie_joint_caches_complete_j));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (2
                                                 - s V_cie_joint_caches_complete_j)) (F_check_ge (0) (0))]
     (s V_cie_joint_caches_complete_z
      + max0(3 - s V_cie_joint_caches_complete_j) <= z)%Q
   | 7 => (s V_cie_joint_caches_complete_z <= z)%Q
   | 8 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (3
                                                  - s V_cie_joint_caches_complete_j)) (F_check_ge (3
                                                                    - s V_cie_joint_caches_complete_j) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                              - s V_cie_joint_caches_complete_j) (0))) (F_max0_ge_0 (2
                                                                    - s V_cie_joint_caches_complete_j))]
     (s V_cie_joint_caches_complete_z
      + max0(3 - s V_cie_joint_caches_complete_j) <= z)%Q
   | 9 => ((1 # 1) + s V_cie_joint_caches_complete_z
           + max0(2 - s V_cie_joint_caches_complete_j) <= z)%Q
   | 10 => ((1 # 1) + s V_cie_joint_caches_complete_z
            + max0(2 - s V_cie_joint_caches_complete_j) <= z)%Q
   | 11 => ((1 # 1) + s V_cie_joint_caches_complete_z
            + max0(3 - s V_cie_joint_caches_complete_j) <= z)%Q
   | 12 => ((1 # 1) + s V_cie_joint_caches_complete_z
            + max0(3 - s V_cie_joint_caches_complete_j) <= z)%Q
   | 13 => ((1 # 1) + s V_cie_joint_caches_complete_z
            + max0(3 - s V_cie_joint_caches_complete_j) <= z)%Q
   | 14 => (s V_cie_joint_caches_complete_z
            + max0(3 - s V_cie_joint_caches_complete_j) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cie_joint_caches_complete =>
    [mkPA Q (fun n z s => ai_cie_joint_caches_complete n s /\ annot0_cie_joint_caches_complete n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cie_joint_caches_complete (proc_start P_cie_joint_caches_complete) s1 (proc_end P_cie_joint_caches_complete) s2 ->
    (s2 V_cie_joint_caches_complete_z <= (3 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cie_joint_caches_complete.
Qed.
