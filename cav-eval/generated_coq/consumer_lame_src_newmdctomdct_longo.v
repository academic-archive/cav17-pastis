Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mdct_long.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mdct_long_z := 1%positive.
Notation V_mdct_long_j := 2%positive.
Notation V_mdct_long_in := 3%positive.
Notation V_mdct_long_out := 4%positive.
Definition Pedges_mdct_long: list (edge proc) :=
  (EA 1 (AAssign V_mdct_long_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_mdct_long_j (Some (ENum (11)))) 3)::(EA 3 ANone 4)::(EA 4 ANone 5)::
  (EA 5 (AAssign V_mdct_long_j (Some (EAdd (EVar V_mdct_long_j)
  (ENum (-1))))) 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EAdd (EVar V_mdct_long_j) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 10)::(EA 7 (AGuard
  (fun s => ((eval (EAdd (EVar V_mdct_long_j) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 10 AWeaken 11)::
  (EA 11 ANone 12)::(EA 12 (AAssign V_mdct_long_z (Some (EAdd (ENum (1))
  (EVar V_mdct_long_z)))) 4)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mdct_long => Pedges_mdct_long
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mdct_long => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_mdct_long (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mdct_long_z <= 0 /\ -1 * s V_mdct_long_z <= 0)%Z
   | 3 => (-1 * s V_mdct_long_z <= 0 /\ 1 * s V_mdct_long_z <= 0 /\ 1 * s V_mdct_long_j + -11 <= 0 /\ -1 * s V_mdct_long_j + 11 <= 0)%Z
   | 4 => (1 * s V_mdct_long_j + -11 <= 0 /\ -1 * s V_mdct_long_z <= 0 /\ -1 * s V_mdct_long_j + 1 <= 0)%Z
   | 5 => (-1 * s V_mdct_long_j + 1 <= 0 /\ -1 * s V_mdct_long_z <= 0 /\ 1 * s V_mdct_long_j + -11 <= 0)%Z
   | 6 => (-1 * s V_mdct_long_z <= 0 /\ -1 * s V_mdct_long_j <= 0 /\ 1 * s V_mdct_long_j + -10 <= 0)%Z
   | 7 => (1 * s V_mdct_long_j + -10 <= 0 /\ -1 * s V_mdct_long_j <= 0 /\ -1 * s V_mdct_long_z <= 0)%Z
   | 8 => (-1 * s V_mdct_long_z <= 0 /\ -1 * s V_mdct_long_j <= 0 /\ 1 * s V_mdct_long_j <= 0)%Z
   | 9 => (1 * s V_mdct_long_j <= 0 /\ -1 * s V_mdct_long_j <= 0 /\ -1 * s V_mdct_long_z <= 0)%Z
   | 10 => (-1 * s V_mdct_long_z <= 0 /\ 1 * s V_mdct_long_j + -10 <= 0 /\ -1 * s V_mdct_long_j + 1 <= 0)%Z
   | 11 => (-1 * s V_mdct_long_j + 1 <= 0 /\ 1 * s V_mdct_long_j + -10 <= 0 /\ -1 * s V_mdct_long_z <= 0)%Z
   | 12 => (-1 * s V_mdct_long_z <= 0 /\ 1 * s V_mdct_long_j + -10 <= 0 /\ -1 * s V_mdct_long_j + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mdct_long (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((10 # 1) <= z)%Q
   | 2 => ((10 # 1) + s V_mdct_long_z <= z)%Q
   | 3 => (-(1 # 1) + s V_mdct_long_j + s V_mdct_long_z <= z)%Q
   | 4 => (-(1 # 1) + s V_mdct_long_j + s V_mdct_long_z <= z)%Q
   | 5 => (-(1 # 1) + s V_mdct_long_j + s V_mdct_long_z <= z)%Q
   | 6 => (s V_mdct_long_j + s V_mdct_long_z <= z)%Q
   | 7 => (s V_mdct_long_j + s V_mdct_long_z <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_mdct_long_j) (-1
                                                               + s V_mdct_long_j));
      (*-1 0*) F_max0_ge_0 (-1 + s V_mdct_long_j);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mdct_long_j) (0))) (F_max0_ge_0 (s V_mdct_long_j))]
     (s V_mdct_long_j + s V_mdct_long_z <= z)%Q
   | 9 => (s V_mdct_long_z <= z)%Q
   | 10 => (s V_mdct_long_j + s V_mdct_long_z <= z)%Q
   | 11 => (s V_mdct_long_j + s V_mdct_long_z <= z)%Q
   | 12 => (s V_mdct_long_j + s V_mdct_long_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mdct_long =>
    [mkPA Q (fun n z s => ai_mdct_long n s /\ annot0_mdct_long n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mdct_long (proc_start P_mdct_long) s1 (proc_end P_mdct_long) s2 ->
    (s2 V_mdct_long_z <= (10 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_mdct_long.
Qed.
