Require Import pasta.Pasta.

Inductive proc: Type :=
  P_pf_push.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_pf_push_z := 1%positive.
Notation V_pf_push__tmp := 2%positive.
Notation V_pf_push_n := 3%positive.
Notation V_pf_push_op := 4%positive.
Notation V_pf_push_ppts := 5%positive.
Definition Pedges_pf_push: list (edge proc) :=
  (EA 1 (AAssign V_pf_push_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_pf_push__tmp (Some (EVar V_pf_push_n))) 3)::(EA 3 ANone 4)::
  (EA 4 (AAssign V_pf_push__tmp (Some (EAdd (EVar V_pf_push__tmp)
  (ENum (-1))))) 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_pf_push__tmp) s) <> (eval (ENum (0)) s))%Z)) 9)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_pf_push__tmp) s) = (eval (ENum (0))
  s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::(EA 10 ANone 11)::
  (EA 11 ANone 12)::(EA 12 (AAssign V_pf_push_z (Some (EAdd (ENum (1))
  (EVar V_pf_push_z)))) 4)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_pf_push => Pedges_pf_push
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_pf_push => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_pf_push (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_pf_push_z <= 0 /\ -1 * s V_pf_push_z <= 0)%Z
   | 3 => (-1 * s V_pf_push_z <= 0 /\ 1 * s V_pf_push_z <= 0)%Z
   | 4 => (-1 * s V_pf_push_z <= 0)%Z
   | 5 => (-1 * s V_pf_push_z <= 0)%Z
   | 6 => (-1 * s V_pf_push_z <= 0)%Z
   | 7 => (-1 * s V_pf_push_z <= 0 /\ 1 * s V_pf_push__tmp <= 0 /\ -1 * s V_pf_push__tmp <= 0)%Z
   | 8 => (-1 * s V_pf_push__tmp <= 0 /\ 1 * s V_pf_push__tmp <= 0 /\ -1 * s V_pf_push_z <= 0)%Z
   | 9 => (-1 * s V_pf_push_z <= 0)%Z
   | 10 => (-1 * s V_pf_push_z <= 0)%Z
   | 11 => (-1 * s V_pf_push_z <= 0)%Z
   | 12 => (-1 * s V_pf_push_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_pf_push (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_pf_push_n <= z)%Q
   | 2 => (s V_pf_push_n + s V_pf_push_z <= z)%Q
   | 3 => (s V_pf_push__tmp + s V_pf_push_z <= z)%Q
   | 4 => (s V_pf_push__tmp + s V_pf_push_z <= z)%Q
   | 5 => ((1 # 1) + s V_pf_push__tmp + s V_pf_push_z <= z)%Q
   | 6 => ((1 # 1) + s V_pf_push__tmp + s V_pf_push_z <= z)%Q
   | 7 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_pf_push__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_pf_push__tmp) (0))) (F_max0_ge_0 (s V_pf_push__tmp))]
     ((1 # 1) + s V_pf_push__tmp + s V_pf_push_z <= z)%Q
   | 8 => (s V_pf_push_z <= z)%Q
   | 9 => ((1 # 1) + s V_pf_push__tmp + s V_pf_push_z <= z)%Q
   | 10 => ((1 # 1) + s V_pf_push__tmp + s V_pf_push_z <= z)%Q
   | 11 => ((1 # 1) + s V_pf_push__tmp + s V_pf_push_z <= z)%Q
   | 12 => ((1 # 1) + s V_pf_push__tmp + s V_pf_push_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_pf_push =>
    [mkPA Q (fun n z s => ai_pf_push n s /\ annot0_pf_push n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_pf_push (proc_start P_pf_push) s1 (proc_end P_pf_push) s2 ->
    (s2 V_pf_push_z <= s1 V_pf_push_n)%Q.
Proof.
  prove_bound ipa admissible_ipa P_pf_push.
Qed.
