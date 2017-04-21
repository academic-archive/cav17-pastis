Require Import pasta.Pasta.

Inductive proc: Type :=
  P_igs_init.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_igs_init_z := 1%positive.
Notation V_igs_init_i := 2%positive.
Definition Pedges_igs_init: list (edge proc) :=
  (EA 1 (AAssign V_igs_init_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_igs_init_i (Some (ENum (25)))) 3)::(EA 3 ANone 4)::(EA 4 ANone 5)::
  (EA 5 (AAssign V_igs_init_i (Some (EAdd (EVar V_igs_init_i)
  (ENum (-1))))) 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EAdd (EVar V_igs_init_i) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 10)::(EA 7 (AGuard
  (fun s => ((eval (EAdd (EVar V_igs_init_i) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 10 AWeaken 11)::
  (EA 11 ANone 12)::(EA 12 (AAssign V_igs_init_z (Some (EAdd (ENum (1))
  (EVar V_igs_init_z)))) 4)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_igs_init => Pedges_igs_init
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_igs_init => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_igs_init (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_igs_init_z <= 0 /\ -1 * s V_igs_init_z <= 0)%Z
   | 3 => (-1 * s V_igs_init_z <= 0 /\ 1 * s V_igs_init_z <= 0 /\ 1 * s V_igs_init_i + -25 <= 0 /\ -1 * s V_igs_init_i + 25 <= 0)%Z
   | 4 => (1 * s V_igs_init_i + -25 <= 0 /\ -1 * s V_igs_init_z <= 0)%Z
   | 5 => (-1 * s V_igs_init_z <= 0 /\ 1 * s V_igs_init_i + -25 <= 0)%Z
   | 6 => (-1 * s V_igs_init_z <= 0 /\ 1 * s V_igs_init_i + -24 <= 0)%Z
   | 7 => (1 * s V_igs_init_i + -24 <= 0 /\ -1 * s V_igs_init_z <= 0)%Z
   | 8 => (-1 * s V_igs_init_z <= 0 /\ 1 * s V_igs_init_i + -1 <= 0 /\ -1 * s V_igs_init_i + 1 <= 0)%Z
   | 9 => (-1 * s V_igs_init_i + 1 <= 0 /\ 1 * s V_igs_init_i + -1 <= 0 /\ -1 * s V_igs_init_z <= 0)%Z
   | 10 => (-1 * s V_igs_init_z <= 0 /\ 1 * s V_igs_init_i + -24 <= 0)%Z
   | 11 => (1 * s V_igs_init_i + -24 <= 0 /\ -1 * s V_igs_init_z <= 0)%Z
   | 12 => (-1 * s V_igs_init_z <= 0 /\ 1 * s V_igs_init_i + -24 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_igs_init (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((23 # 1) <= z)%Q
   | 2 => ((23 # 1) + s V_igs_init_z <= z)%Q
   | 3 => (-(2 # 1) + s V_igs_init_i + s V_igs_init_z <= z)%Q
   | 4 => (-(2 # 1) + s V_igs_init_i + s V_igs_init_z <= z)%Q
   | 5 => (-(2 # 1) + s V_igs_init_i + s V_igs_init_z <= z)%Q
   | 6 => (-(1 # 1) + s V_igs_init_i + s V_igs_init_z <= z)%Q
   | 7 => (-(1 # 1) + s V_igs_init_i + s V_igs_init_z <= z)%Q
   | 8 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_igs_init_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_igs_init_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_igs_init_i))]
     (-(1 # 1) + s V_igs_init_i + s V_igs_init_z <= z)%Q
   | 9 => (s V_igs_init_z <= z)%Q
   | 10 => (-(1 # 1) + s V_igs_init_i + s V_igs_init_z <= z)%Q
   | 11 => (-(1 # 1) + s V_igs_init_i + s V_igs_init_z <= z)%Q
   | 12 => (-(1 # 1) + s V_igs_init_i + s V_igs_init_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_igs_init =>
    [mkPA Q (fun n z s => ai_igs_init n s /\ annot0_igs_init n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_igs_init (proc_start P_igs_init) s1 (proc_end P_igs_init) s2 ->
    (s2 V_igs_init_z <= (23 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_igs_init.
Qed.
