Require Import pasta.Pasta.

Inductive proc: Type :=
  P_refcpy_to_new.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_refcpy_to_new_z := 1%positive.
Notation V_refcpy_to_new__tmp := 2%positive.
Notation V_refcpy_to_new_from := 3%positive.
Notation V_refcpy_to_new_size := 4%positive.
Notation V_refcpy_to_new_to := 5%positive.
Definition Pedges_refcpy_to_new: list (edge proc) :=
  (EA 1 (AAssign V_refcpy_to_new_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_refcpy_to_new__tmp (Some (EVar V_refcpy_to_new_size))) 3)::
  (EA 3 ANone 4)::(EA 4 (AAssign V_refcpy_to_new__tmp
  (Some (EAdd (EVar V_refcpy_to_new__tmp) (ENum (-1))))) 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_refcpy_to_new__tmp) s) <> (eval (ENum (0))
  s))%Z)) 9)::(EA 6 (AGuard (fun s => ((eval (EVar V_refcpy_to_new__tmp) s) =
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 ANone 11)::(EA 11 ANone 12)::(EA 12 (AAssign V_refcpy_to_new_z
  (Some (EAdd (ENum (1)) (EVar V_refcpy_to_new_z)))) 4)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_refcpy_to_new => Pedges_refcpy_to_new
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_refcpy_to_new => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_refcpy_to_new (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_refcpy_to_new_z <= 0 /\ -1 * s V_refcpy_to_new_z <= 0)%Z
   | 3 => (-1 * s V_refcpy_to_new_z <= 0 /\ 1 * s V_refcpy_to_new_z <= 0)%Z
   | 4 => (-1 * s V_refcpy_to_new_z <= 0)%Z
   | 5 => (-1 * s V_refcpy_to_new_z <= 0)%Z
   | 6 => (-1 * s V_refcpy_to_new_z <= 0)%Z
   | 7 => (-1 * s V_refcpy_to_new_z <= 0 /\ 1 * s V_refcpy_to_new__tmp <= 0 /\ -1 * s V_refcpy_to_new__tmp <= 0)%Z
   | 8 => (-1 * s V_refcpy_to_new__tmp <= 0 /\ 1 * s V_refcpy_to_new__tmp <= 0 /\ -1 * s V_refcpy_to_new_z <= 0)%Z
   | 9 => (-1 * s V_refcpy_to_new_z <= 0)%Z
   | 10 => (-1 * s V_refcpy_to_new_z <= 0)%Z
   | 11 => (-1 * s V_refcpy_to_new_z <= 0)%Z
   | 12 => (-1 * s V_refcpy_to_new_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_refcpy_to_new (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_refcpy_to_new_size <= z)%Q
   | 2 => (s V_refcpy_to_new_size + s V_refcpy_to_new_z <= z)%Q
   | 3 => (s V_refcpy_to_new__tmp + s V_refcpy_to_new_z <= z)%Q
   | 4 => (s V_refcpy_to_new__tmp + s V_refcpy_to_new_z <= z)%Q
   | 5 => ((1 # 1) + s V_refcpy_to_new__tmp + s V_refcpy_to_new_z <= z)%Q
   | 6 => ((1 # 1) + s V_refcpy_to_new__tmp + s V_refcpy_to_new_z <= z)%Q
   | 7 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_refcpy_to_new__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_refcpy_to_new__tmp) (0))) (F_max0_ge_0 (s V_refcpy_to_new__tmp))]
     ((1 # 1) + s V_refcpy_to_new__tmp + s V_refcpy_to_new_z <= z)%Q
   | 8 => (s V_refcpy_to_new_z <= z)%Q
   | 9 => ((1 # 1) + s V_refcpy_to_new__tmp + s V_refcpy_to_new_z <= z)%Q
   | 10 => ((1 # 1) + s V_refcpy_to_new__tmp + s V_refcpy_to_new_z <= z)%Q
   | 11 => ((1 # 1) + s V_refcpy_to_new__tmp + s V_refcpy_to_new_z <= z)%Q
   | 12 => ((1 # 1) + s V_refcpy_to_new__tmp + s V_refcpy_to_new_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_refcpy_to_new =>
    [mkPA Q (fun n z s => ai_refcpy_to_new n s /\ annot0_refcpy_to_new n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_refcpy_to_new (proc_start P_refcpy_to_new) s1 (proc_end P_refcpy_to_new) s2 ->
    (s2 V_refcpy_to_new_z <= s1 V_refcpy_to_new_size)%Q.
Proof.
  prove_bound ipa admissible_ipa P_refcpy_to_new.
Qed.
