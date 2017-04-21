Require Import pasta.Pasta.

Inductive proc: Type :=
  P_arg_finit.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_arg_finit_z := 1%positive.
Notation V_arg_finit_pal_dref_off36 := 2%positive.
Notation V_arg_finit_pal := 3%positive.
Definition Pedges_arg_finit: list (edge proc) :=
  (EA 1 (AAssign V_arg_finit_z (Some (ENum (0)))) 2)::(EA 2 ANone 3)::
  (EA 3 AWeaken 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_arg_finit_pal_dref_off36) s) <> (eval (ENum (0))
  s))%Z)) 7)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_arg_finit_pal_dref_off36) s) = (eval (ENum (0))
  s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_arg_finit_pal_dref_off36 (Some (EAdd (EVar V_arg_finit_pal_dref_off36)
  (ENum (-1))))) 9)::(EA 9 AWeaken 10)::(EA 10 ANone 11)::(EA 10 ANone 12)::
  (EA 11 ANone 12)::(EA 12 ANone 13)::(EA 13 ANone 14)::(EA 14 (AAssign
  V_arg_finit_z (Some (EAdd (ENum (1)) (EVar V_arg_finit_z)))) 15)::
  (EA 15 AWeaken 4)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_arg_finit => Pedges_arg_finit
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_arg_finit => 6
     end)%positive;
  var_global := var_global
}.

Definition ai_arg_finit (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_arg_finit_z <= 0 /\ -1 * s V_arg_finit_z <= 0)%Z
   | 3 => (-1 * s V_arg_finit_z <= 0 /\ 1 * s V_arg_finit_z <= 0)%Z
   | 4 => (-1 * s V_arg_finit_z <= 0)%Z
   | 5 => (-1 * s V_arg_finit_z <= 0 /\ 1 * s V_arg_finit_pal_dref_off36 <= 0 /\ -1 * s V_arg_finit_pal_dref_off36 <= 0)%Z
   | 6 => (-1 * s V_arg_finit_pal_dref_off36 <= 0 /\ 1 * s V_arg_finit_pal_dref_off36 <= 0 /\ -1 * s V_arg_finit_z <= 0)%Z
   | 7 => (-1 * s V_arg_finit_z <= 0)%Z
   | 8 => (-1 * s V_arg_finit_z <= 0)%Z
   | 9 => (-1 * s V_arg_finit_z <= 0)%Z
   | 10 => (-1 * s V_arg_finit_z <= 0)%Z
   | 11 => (-1 * s V_arg_finit_z <= 0)%Z
   | 12 => (-1 * s V_arg_finit_z <= 0)%Z
   | 13 => (-1 * s V_arg_finit_z <= 0)%Z
   | 14 => (-1 * s V_arg_finit_z <= 0)%Z
   | 15 => (-1 * s V_arg_finit_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_arg_finit (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_arg_finit_pal_dref_off36 <= z)%Q
   | 2 => (s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | 3 => (s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | 4 => (s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | 5 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_arg_finit_pal_dref_off36)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_arg_finit_pal_dref_off36) (0))) (F_max0_ge_0 (s V_arg_finit_pal_dref_off36))]
     (s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | 6 => (s V_arg_finit_z <= z)%Q
   | 7 => (s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | 8 => (s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | 9 => ((1 # 1) + s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | 10 => ((1 # 1) + s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | 11 => ((1 # 1) + s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | 12 => ((1 # 1) + s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | 13 => ((1 # 1) + s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | 14 => ((1 # 1) + s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | 15 => (s V_arg_finit_pal_dref_off36 + s V_arg_finit_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_arg_finit =>
    [mkPA Q (fun n z s => ai_arg_finit n s /\ annot0_arg_finit n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_arg_finit (proc_start P_arg_finit) s1 (proc_end P_arg_finit) s2 ->
    (s2 V_arg_finit_z <= s1 V_arg_finit_pal_dref_off36)%Q.
Proof.
  prove_bound ipa admissible_ipa P_arg_finit.
Qed.
