Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mp_neg.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mp_neg_z := 1%positive.
Notation V_mp_neg_global_precision := 2%positive.
Notation V_mp_neg_precision := 3%positive.
Notation V_mp_neg_r := 4%positive.
Definition Pedges_mp_neg: list (edge proc) :=
  (EA 1 (AAssign V_mp_neg_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_mp_neg_precision (Some (EVar V_mp_neg_global_precision))) 3)::
  (EA 3 ANone 4)::(EA 4 ANone 5)::(EA 5 (AAssign V_mp_neg_precision
  (Some (EAdd (EVar V_mp_neg_precision) (ENum (-1))))) 6)::(EA 6 AWeaken 7)::
  (EA 7 (AGuard (fun s => ((eval (EAdd (EVar V_mp_neg_precision) (ENum (-1)))
  s) <> (eval (ENum (0)) s))%Z)) 10)::(EA 7 (AGuard
  (fun s => ((eval (EAdd (EVar V_mp_neg_precision) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 10 AWeaken 11)::
  (EA 11 ANone 12)::(EA 12 (AAssign V_mp_neg_z (Some (EAdd (ENum (1))
  (EVar V_mp_neg_z)))) 4)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mp_neg => Pedges_mp_neg
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mp_neg => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_mp_neg (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mp_neg_z <= 0 /\ -1 * s V_mp_neg_z <= 0)%Z
   | 3 => (-1 * s V_mp_neg_z <= 0 /\ 1 * s V_mp_neg_z <= 0)%Z
   | 4 => (-1 * s V_mp_neg_z <= 0)%Z
   | 5 => (-1 * s V_mp_neg_z <= 0)%Z
   | 6 => (-1 * s V_mp_neg_z <= 0)%Z
   | 7 => (-1 * s V_mp_neg_z <= 0)%Z
   | 8 => (-1 * s V_mp_neg_z <= 0 /\ 1 * s V_mp_neg_precision + -1 <= 0 /\ -1 * s V_mp_neg_precision + 1 <= 0)%Z
   | 9 => (-1 * s V_mp_neg_precision + 1 <= 0 /\ 1 * s V_mp_neg_precision + -1 <= 0 /\ -1 * s V_mp_neg_z <= 0)%Z
   | 10 => (-1 * s V_mp_neg_z <= 0)%Z
   | 11 => (-1 * s V_mp_neg_z <= 0)%Z
   | 12 => (-1 * s V_mp_neg_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mp_neg (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_mp_neg_global_precision <= z)%Q
   | 2 => (s V_mp_neg_global_precision + s V_mp_neg_z <= z)%Q
   | 3 => (s V_mp_neg_precision + s V_mp_neg_z <= z)%Q
   | 4 => (s V_mp_neg_precision + s V_mp_neg_z <= z)%Q
   | 5 => (s V_mp_neg_precision + s V_mp_neg_z <= z)%Q
   | 6 => ((1 # 1) + s V_mp_neg_precision + s V_mp_neg_z <= z)%Q
   | 7 => ((1 # 1) + s V_mp_neg_precision + s V_mp_neg_z <= z)%Q
   | 8 => hints
     [(*-2 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_mp_neg_precision)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_mp_neg_precision) (0))) (F_max0_ge_0 (-1
                                                                    + s V_mp_neg_precision))]
     ((1 # 1) + s V_mp_neg_precision + s V_mp_neg_z <= z)%Q
   | 9 => (s V_mp_neg_z <= z)%Q
   | 10 => ((1 # 1) + s V_mp_neg_precision + s V_mp_neg_z <= z)%Q
   | 11 => ((1 # 1) + s V_mp_neg_precision + s V_mp_neg_z <= z)%Q
   | 12 => ((1 # 1) + s V_mp_neg_precision + s V_mp_neg_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mp_neg =>
    [mkPA Q (fun n z s => ai_mp_neg n s /\ annot0_mp_neg n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mp_neg (proc_start P_mp_neg) s1 (proc_end P_mp_neg) s2 ->
    (s2 V_mp_neg_z <= s1 V_mp_neg_global_precision)%Q.
Proof.
  prove_bound ipa admissible_ipa P_mp_neg.
Qed.
