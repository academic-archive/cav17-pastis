Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cryptRandInit.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cryptRandInit_z := 1%positive.
Notation V_cryptRandInit_i := 2%positive.
Notation V_cryptRandInit_randSeedOpen := 3%positive.
Notation V_cryptRandInit_cfb := 4%positive.
Definition Pedges_cryptRandInit: list (edge proc) :=
  (EA 1 (AAssign V_cryptRandInit_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_cryptRandInit_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_cryptRandInit_i
  (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_cryptRandInit_i) s) < (eval (ENum (24))
  s))%Z)) 14)::(EA 7 (AGuard (fun s => ((eval (EVar V_cryptRandInit_i) s) >=
  (eval (ENum (24)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 ANone 10)::
  (EA 9 ANone 11)::(EA 10 ANone 11)::(EA 11 (AAssign
  V_cryptRandInit_randSeedOpen (Some (ENum (1)))) 12)::(EA 12 AWeaken 13)::
  (EA 14 AWeaken 15)::(EA 15 ANone 16)::(EA 16 (AAssign V_cryptRandInit_i
  (Some (EAdd (EVar V_cryptRandInit_i) (ENum (1))))) 17)::(EA 17 ANone 18)::
  (EA 18 ANone 19)::(EA 19 (AAssign V_cryptRandInit_z (Some (EAdd (ENum (1))
  (EVar V_cryptRandInit_z)))) 20)::(EA 20 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cryptRandInit => Pedges_cryptRandInit
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cryptRandInit => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_cryptRandInit (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cryptRandInit_z <= 0 /\ -1 * s V_cryptRandInit_z <= 0)%Z
   | 3 => (-1 * s V_cryptRandInit_z <= 0 /\ 1 * s V_cryptRandInit_z <= 0 /\ -1 * s V_cryptRandInit_i <= 0)%Z
   | 4 => (-1 * s V_cryptRandInit_i <= 0 /\ 1 * s V_cryptRandInit_z <= 0 /\ -1 * s V_cryptRandInit_z <= 0)%Z
   | 5 => (-1 * s V_cryptRandInit_z <= 0 /\ 1 * s V_cryptRandInit_z <= 0 /\ 1 * s V_cryptRandInit_i <= 0 /\ -1 * s V_cryptRandInit_i <= 0)%Z
   | 6 => (-1 * s V_cryptRandInit_i <= 0 /\ 1 * s V_cryptRandInit_i <= 0 /\ 1 * s V_cryptRandInit_z <= 0 /\ -1 * s V_cryptRandInit_z <= 0)%Z
   | 7 => (-1 * s V_cryptRandInit_z <= 0 /\ -1 * s V_cryptRandInit_i <= 0 /\ 1 * s V_cryptRandInit_i + -24 <= 0)%Z
   | 8 => (1 * s V_cryptRandInit_i + -24 <= 0 /\ -1 * s V_cryptRandInit_z <= 0 /\ -1 * s V_cryptRandInit_i + 24 <= 0)%Z
   | 9 => (-1 * s V_cryptRandInit_i + 24 <= 0 /\ -1 * s V_cryptRandInit_z <= 0 /\ 1 * s V_cryptRandInit_i + -24 <= 0)%Z
   | 10 => (1 * s V_cryptRandInit_i + -24 <= 0 /\ -1 * s V_cryptRandInit_z <= 0 /\ -1 * s V_cryptRandInit_i + 24 <= 0)%Z
   | 11 => (-1 * s V_cryptRandInit_i + 24 <= 0 /\ -1 * s V_cryptRandInit_z <= 0 /\ 1 * s V_cryptRandInit_i + -24 <= 0)%Z
   | 12 => (1 * s V_cryptRandInit_i + -24 <= 0 /\ -1 * s V_cryptRandInit_z <= 0 /\ -1 * s V_cryptRandInit_i + 24 <= 0 /\ 1 * s V_cryptRandInit_randSeedOpen + -1 <= 0 /\ -1 * s V_cryptRandInit_randSeedOpen + 1 <= 0)%Z
   | 13 => (-1 * s V_cryptRandInit_randSeedOpen + 1 <= 0 /\ 1 * s V_cryptRandInit_randSeedOpen + -1 <= 0 /\ -1 * s V_cryptRandInit_i + 24 <= 0 /\ -1 * s V_cryptRandInit_z <= 0 /\ 1 * s V_cryptRandInit_i + -24 <= 0)%Z
   | 14 => (-1 * s V_cryptRandInit_i <= 0 /\ -1 * s V_cryptRandInit_z <= 0 /\ 1 * s V_cryptRandInit_i + -23 <= 0)%Z
   | 15 => (1 * s V_cryptRandInit_i + -23 <= 0 /\ -1 * s V_cryptRandInit_z <= 0 /\ -1 * s V_cryptRandInit_i <= 0)%Z
   | 16 => (-1 * s V_cryptRandInit_i <= 0 /\ -1 * s V_cryptRandInit_z <= 0 /\ 1 * s V_cryptRandInit_i + -23 <= 0)%Z
   | 17 => (-1 * s V_cryptRandInit_z <= 0 /\ -1 * s V_cryptRandInit_i + 1 <= 0 /\ 1 * s V_cryptRandInit_i + -24 <= 0)%Z
   | 18 => (1 * s V_cryptRandInit_i + -24 <= 0 /\ -1 * s V_cryptRandInit_i + 1 <= 0 /\ -1 * s V_cryptRandInit_z <= 0)%Z
   | 19 => (-1 * s V_cryptRandInit_z <= 0 /\ -1 * s V_cryptRandInit_i + 1 <= 0 /\ 1 * s V_cryptRandInit_i + -24 <= 0)%Z
   | 20 => (1 * s V_cryptRandInit_i + -24 <= 0 /\ -1 * s V_cryptRandInit_i + 1 <= 0 /\ -1 * s V_cryptRandInit_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cryptRandInit (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((24 # 1) <= z)%Q
   | 2 => ((24 # 1) + s V_cryptRandInit_z <= z)%Q
   | 3 => ((24 # 1) + s V_cryptRandInit_z <= z)%Q
   | 4 => ((24 # 1) + s V_cryptRandInit_z <= z)%Q
   | 5 => ((24 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 6 => ((24 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 7 => ((24 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 8 => ((24 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 9 => ((24 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 10 => ((24 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 11 => ((24 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (24 - s V_cryptRandInit_i) (23
                                                                    - s V_cryptRandInit_i));
      (*-1 0*) F_max0_ge_0 (23 - s V_cryptRandInit_i);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (24
                                                              - s V_cryptRandInit_i) (0))) (F_max0_ge_0 (24
                                                                    - s V_cryptRandInit_i))]
     ((24 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 13 => (s V_cryptRandInit_z <= z)%Q
   | 14 => ((24 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 15 => ((24 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 16 => ((24 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 17 => ((25 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 18 => ((25 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 19 => ((25 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | 20 => ((24 # 1) - s V_cryptRandInit_i + s V_cryptRandInit_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cryptRandInit =>
    [mkPA Q (fun n z s => ai_cryptRandInit n s /\ annot0_cryptRandInit n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cryptRandInit (proc_start P_cryptRandInit) s1 (proc_end P_cryptRandInit) s2 ->
    (s2 V_cryptRandInit_z <= (24 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cryptRandInit.
Qed.
