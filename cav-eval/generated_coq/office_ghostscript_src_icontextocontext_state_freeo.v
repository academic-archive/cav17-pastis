Require Import pasta.Pasta.

Inductive proc: Type :=
  P_context_state_free.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_context_state_free_z := 1%positive.
Notation V_context_state_free_i := 2%positive.
Notation V_context_state_free_mem := 3%positive.
Notation V_context_state_free_pcst := 4%positive.
Definition Pedges_context_state_free: list (edge proc) :=
  (EA 1 (AAssign V_context_state_free_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_context_state_free_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_context_state_free_i
  (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_context_state_free_i) s) < (eval (ENum (4))
  s))%Z)) 10)::(EA 7 (AGuard (fun s => ((eval (EVar V_context_state_free_i)
  s) >= (eval (ENum (4)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 10 AWeaken 11)::
  (EA 11 ANone 12)::(EA 11 ANone 15)::(EA 12 AWeaken 13)::(EA 13 ANone 15)::
  (EA 13 ANone 14)::(EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_context_state_free_i (Some (EAdd (EVar V_context_state_free_i)
  (ENum (1))))) 17)::(EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_context_state_free_z (Some (EAdd (ENum (1))
  (EVar V_context_state_free_z)))) 20)::(EA 20 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_context_state_free => Pedges_context_state_free
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_context_state_free => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_context_state_free (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_context_state_free_z <= 0 /\ -1 * s V_context_state_free_z <= 0)%Z
   | 3 => (-1 * s V_context_state_free_z <= 0 /\ 1 * s V_context_state_free_z <= 0 /\ -1 * s V_context_state_free_i <= 0)%Z
   | 4 => (-1 * s V_context_state_free_i <= 0 /\ 1 * s V_context_state_free_z <= 0 /\ -1 * s V_context_state_free_z <= 0)%Z
   | 5 => (-1 * s V_context_state_free_z <= 0 /\ 1 * s V_context_state_free_z <= 0 /\ 1 * s V_context_state_free_i <= 0 /\ -1 * s V_context_state_free_i <= 0)%Z
   | 6 => (-1 * s V_context_state_free_i <= 0 /\ 1 * s V_context_state_free_i <= 0 /\ 1 * s V_context_state_free_z <= 0 /\ -1 * s V_context_state_free_z <= 0)%Z
   | 7 => (-1 * s V_context_state_free_z <= 0 /\ -1 * s V_context_state_free_i <= 0 /\ 1 * s V_context_state_free_i + -4 <= 0)%Z
   | 8 => (1 * s V_context_state_free_i + -4 <= 0 /\ -1 * s V_context_state_free_z <= 0 /\ -1 * s V_context_state_free_i + 4 <= 0)%Z
   | 9 => (-1 * s V_context_state_free_i + 4 <= 0 /\ -1 * s V_context_state_free_z <= 0 /\ 1 * s V_context_state_free_i + -4 <= 0)%Z
   | 10 => (-1 * s V_context_state_free_i <= 0 /\ -1 * s V_context_state_free_z <= 0 /\ 1 * s V_context_state_free_i + -3 <= 0)%Z
   | 11 => (1 * s V_context_state_free_i + -3 <= 0 /\ -1 * s V_context_state_free_z <= 0 /\ -1 * s V_context_state_free_i <= 0)%Z
   | 12 => (-1 * s V_context_state_free_i <= 0 /\ -1 * s V_context_state_free_z <= 0 /\ 1 * s V_context_state_free_i + -3 <= 0)%Z
   | 13 => (1 * s V_context_state_free_i + -3 <= 0 /\ -1 * s V_context_state_free_z <= 0 /\ -1 * s V_context_state_free_i <= 0)%Z
   | 14 => (-1 * s V_context_state_free_i <= 0 /\ -1 * s V_context_state_free_z <= 0 /\ 1 * s V_context_state_free_i + -3 <= 0)%Z
   | 15 => (1 * s V_context_state_free_i + -3 <= 0 /\ -1 * s V_context_state_free_z <= 0 /\ -1 * s V_context_state_free_i <= 0)%Z
   | 16 => (-1 * s V_context_state_free_i <= 0 /\ -1 * s V_context_state_free_z <= 0 /\ 1 * s V_context_state_free_i + -3 <= 0)%Z
   | 17 => (-1 * s V_context_state_free_z <= 0 /\ -1 * s V_context_state_free_i + 1 <= 0 /\ 1 * s V_context_state_free_i + -4 <= 0)%Z
   | 18 => (1 * s V_context_state_free_i + -4 <= 0 /\ -1 * s V_context_state_free_i + 1 <= 0 /\ -1 * s V_context_state_free_z <= 0)%Z
   | 19 => (-1 * s V_context_state_free_z <= 0 /\ -1 * s V_context_state_free_i + 1 <= 0 /\ 1 * s V_context_state_free_i + -4 <= 0)%Z
   | 20 => (1 * s V_context_state_free_i + -4 <= 0 /\ -1 * s V_context_state_free_i + 1 <= 0 /\ -1 * s V_context_state_free_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_context_state_free (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_context_state_free_z <= z)%Q
   | 3 => ((4 # 1) + s V_context_state_free_z <= z)%Q
   | 4 => ((4 # 1) + s V_context_state_free_z <= z)%Q
   | 5 => (s V_context_state_free_z + max0(4 - s V_context_state_free_i) <= z)%Q
   | 6 => (s V_context_state_free_z + max0(4 - s V_context_state_free_i) <= z)%Q
   | 7 => (s V_context_state_free_z + max0(4 - s V_context_state_free_i) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_context_state_free_i) (3
                                                                    - s V_context_state_free_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_context_state_free_i)]
     (s V_context_state_free_z + max0(4 - s V_context_state_free_i) <= z)%Q
   | 9 => (s V_context_state_free_z <= z)%Q
   | 10 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                   - s V_context_state_free_i)) (F_check_ge (4
                                                                    - s V_context_state_free_i) (0))]
     (s V_context_state_free_z + max0(4 - s V_context_state_free_i) <= z)%Q
   | 11 => ((4 # 1) - s V_context_state_free_i + s V_context_state_free_z <= z)%Q
   | 12 => ((4 # 1) - s V_context_state_free_i + s V_context_state_free_z <= z)%Q
   | 13 => ((4 # 1) - s V_context_state_free_i + s V_context_state_free_z <= z)%Q
   | 14 => ((4 # 1) - s V_context_state_free_i + s V_context_state_free_z <= z)%Q
   | 15 => ((4 # 1) - s V_context_state_free_i + s V_context_state_free_z <= z)%Q
   | 16 => ((4 # 1) - s V_context_state_free_i + s V_context_state_free_z <= z)%Q
   | 17 => ((5 # 1) - s V_context_state_free_i + s V_context_state_free_z <= z)%Q
   | 18 => ((5 # 1) - s V_context_state_free_i + s V_context_state_free_z <= z)%Q
   | 19 => ((5 # 1) - s V_context_state_free_i + s V_context_state_free_z <= z)%Q
   | 20 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_context_state_free_i) (0))) (F_max0_ge_0 (4
                                                                    - s V_context_state_free_i))]
     ((4 # 1) - s V_context_state_free_i + s V_context_state_free_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_context_state_free =>
    [mkPA Q (fun n z s => ai_context_state_free n s /\ annot0_context_state_free n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_context_state_free (proc_start P_context_state_free) s1 (proc_end P_context_state_free) s2 ->
    (s2 V_context_state_free_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_context_state_free.
Qed.
