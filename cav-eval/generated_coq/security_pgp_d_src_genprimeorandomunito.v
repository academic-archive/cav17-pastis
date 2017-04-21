Require Import pasta.Pasta.

Inductive proc: Type :=
  P_randomunit.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_randomunit_z := 1%positive.
Notation V_randomunit_i := 2%positive.
Notation V_randomunit_u := 3%positive.
Definition Pedges_randomunit: list (edge proc) :=
  (EA 1 (AAssign V_randomunit_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_randomunit_u (Some (ENum (0)))) 3)::(EA 3 (AAssign V_randomunit_i
  (Some (ENum (2)))) 4)::(EA 4 ANone 5)::(EA 5 (AAssign V_randomunit_u
  None) 6)::(EA 6 ANone 7)::(EA 7 (AAssign V_randomunit_i
  (Some (EAdd (EVar V_randomunit_i) (ENum (-1))))) 8)::(EA 8 AWeaken 9)::
  (EA 9 (AGuard (fun s => ((eval (EAdd (EVar V_randomunit_i) (ENum (-1)))
  s) <> (eval (ENum (0)) s))%Z)) 12)::(EA 9 (AGuard
  (fun s => ((eval (EAdd (EVar V_randomunit_i) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 12 AWeaken 13)::
  (EA 13 ANone 14)::(EA 14 (AAssign V_randomunit_z (Some (EAdd (ENum (1))
  (EVar V_randomunit_z)))) 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_randomunit => Pedges_randomunit
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_randomunit => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_randomunit (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_randomunit_z <= 0 /\ -1 * s V_randomunit_z <= 0)%Z
   | 3 => (-1 * s V_randomunit_z <= 0 /\ 1 * s V_randomunit_z <= 0 /\ 1 * s V_randomunit_u <= 0 /\ -1 * s V_randomunit_u <= 0)%Z
   | 4 => (-1 * s V_randomunit_u <= 0 /\ 1 * s V_randomunit_u <= 0 /\ 1 * s V_randomunit_z <= 0 /\ -1 * s V_randomunit_z <= 0 /\ 1 * s V_randomunit_i + -2 <= 0 /\ -1 * s V_randomunit_i + 2 <= 0)%Z
   | 5 => (-1 * s V_randomunit_i + 2 <= 0 /\ 1 * s V_randomunit_i + -2 <= 0 /\ -1 * s V_randomunit_z <= 0 /\ 1 * s V_randomunit_z <= 0 /\ 1 * s V_randomunit_u <= 0 /\ -1 * s V_randomunit_u <= 0)%Z
   | 6 => (1 * s V_randomunit_z <= 0 /\ -1 * s V_randomunit_z <= 0 /\ 1 * s V_randomunit_i + -2 <= 0 /\ -1 * s V_randomunit_i + 2 <= 0)%Z
   | 7 => (-1 * s V_randomunit_i + 2 <= 0 /\ 1 * s V_randomunit_i + -2 <= 0 /\ -1 * s V_randomunit_z <= 0 /\ 1 * s V_randomunit_z <= 0)%Z
   | 8 => (1 * s V_randomunit_z <= 0 /\ -1 * s V_randomunit_z <= 0 /\ -1 * s V_randomunit_i + 1 <= 0 /\ 1 * s V_randomunit_i + -1 <= 0)%Z
   | 9 => (1 * s V_randomunit_i + -1 <= 0 /\ -1 * s V_randomunit_i + 1 <= 0 /\ -1 * s V_randomunit_z <= 0 /\ 1 * s V_randomunit_z <= 0)%Z
   | 10 => (1 * s V_randomunit_z <= 0 /\ -1 * s V_randomunit_z <= 0 /\ -1 * s V_randomunit_i + 1 <= 0 /\ 1 * s V_randomunit_i + -1 <= 0)%Z
   | 11 => (1 * s V_randomunit_i + -1 <= 0 /\ -1 * s V_randomunit_i + 1 <= 0 /\ -1 * s V_randomunit_z <= 0 /\ 1 * s V_randomunit_z <= 0)%Z
   | 12 => (False)%Z
   | 13 => (False)%Z
   | 14 => (False)%Z
   | _ => False
   end)%positive.

Definition annot0_randomunit (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (0 <= z)%Q
   | 2 => (0 <= z)%Q
   | 3 => (0 <= z)%Q
   | 4 => (0 <= z)%Q
   | 5 => (0 <= z)%Q
   | 6 => (0 <= z)%Q
   | 7 => (0 <= z)%Q
   | 8 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_randomunit_z) (0))) (F_max0_ge_0 (-
                                                                    s V_randomunit_z))]
     (0 <= z)%Q
   | 9 => (s V_randomunit_z + max0(-s V_randomunit_z) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_randomunit_z)) (F_check_ge (0) (0))]
     (s V_randomunit_z + max0(-s V_randomunit_z) <= z)%Q
   | 11 => (s V_randomunit_z <= z)%Q
   | 12 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_randomunit_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_randomunit_z) (0))) (F_max0_ge_0 (s V_randomunit_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_randomunit_z)) (F_check_ge (0) (0))]
     (s V_randomunit_z + max0(-s V_randomunit_z) <= z)%Q
   | 13 => (0 <= z)%Q
   | 14 => (0 <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_randomunit =>
    [mkPA Q (fun n z s => ai_randomunit n s /\ annot0_randomunit n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_randomunit (proc_start P_randomunit) s1 (proc_end P_randomunit) s2 ->
    (s2 V_randomunit_z <= 0)%Q.
Proof.
  prove_bound ipa admissible_ipa P_randomunit.
Qed.
