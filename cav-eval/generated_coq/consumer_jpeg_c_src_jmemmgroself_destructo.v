Require Import pasta.Pasta.

Inductive proc: Type :=
  P_self_destruct.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_self_destruct_z := 1%positive.
Notation V_self_destruct_pool := 2%positive.
Notation V_self_destruct_cinfo := 3%positive.
Definition Pedges_self_destruct: list (edge proc) :=
  (EA 1 (AAssign V_self_destruct_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_self_destruct_pool (Some (ENum (1)))) 3)::(EA 3 ANone 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_self_destruct_pool) s) >= (eval (ENum (0))
  s))%Z)) 8)::(EA 5 (AGuard (fun s => ((eval (EVar V_self_destruct_pool) s) <
  (eval (ENum (0)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 8 AWeaken 9)::
  (EA 9 ANone 10)::(EA 10 (AAssign V_self_destruct_pool
  (Some (EAdd (EVar V_self_destruct_pool) (ENum (-1))))) 11)::
  (EA 11 ANone 12)::(EA 12 ANone 13)::(EA 13 (AAssign V_self_destruct_z
  (Some (EAdd (ENum (1)) (EVar V_self_destruct_z)))) 14)::(EA 14 AWeaken 5)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_self_destruct => Pedges_self_destruct
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_self_destruct => 7
     end)%positive;
  var_global := var_global
}.

Definition ai_self_destruct (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_self_destruct_z <= 0 /\ -1 * s V_self_destruct_z <= 0)%Z
   | 3 => (-1 * s V_self_destruct_z <= 0 /\ 1 * s V_self_destruct_z <= 0 /\ 1 * s V_self_destruct_pool + -1 <= 0 /\ -1 * s V_self_destruct_pool + 1 <= 0)%Z
   | 4 => (-1 * s V_self_destruct_pool + 1 <= 0 /\ 1 * s V_self_destruct_pool + -1 <= 0 /\ 1 * s V_self_destruct_z <= 0 /\ -1 * s V_self_destruct_z <= 0)%Z
   | 5 => (-1 * s V_self_destruct_z <= 0 /\ 1 * s V_self_destruct_pool + -1 <= 0 /\ -1 * s V_self_destruct_pool + -1 <= 0)%Z
   | 6 => (-1 * s V_self_destruct_pool + -1 <= 0 /\ -1 * s V_self_destruct_z <= 0 /\ 1 * s V_self_destruct_pool + 1 <= 0)%Z
   | 7 => (1 * s V_self_destruct_pool + 1 <= 0 /\ -1 * s V_self_destruct_z <= 0 /\ -1 * s V_self_destruct_pool + -1 <= 0)%Z
   | 8 => (1 * s V_self_destruct_pool + -1 <= 0 /\ -1 * s V_self_destruct_z <= 0 /\ -1 * s V_self_destruct_pool <= 0)%Z
   | 9 => (-1 * s V_self_destruct_pool <= 0 /\ -1 * s V_self_destruct_z <= 0 /\ 1 * s V_self_destruct_pool + -1 <= 0)%Z
   | 10 => (1 * s V_self_destruct_pool + -1 <= 0 /\ -1 * s V_self_destruct_z <= 0 /\ -1 * s V_self_destruct_pool <= 0)%Z
   | 11 => (-1 * s V_self_destruct_z <= 0 /\ 1 * s V_self_destruct_pool <= 0 /\ -1 * s V_self_destruct_pool + -1 <= 0)%Z
   | 12 => (-1 * s V_self_destruct_pool + -1 <= 0 /\ 1 * s V_self_destruct_pool <= 0 /\ -1 * s V_self_destruct_z <= 0)%Z
   | 13 => (-1 * s V_self_destruct_z <= 0 /\ 1 * s V_self_destruct_pool <= 0 /\ -1 * s V_self_destruct_pool + -1 <= 0)%Z
   | 14 => (-1 * s V_self_destruct_pool + -1 <= 0 /\ 1 * s V_self_destruct_pool <= 0 /\ -1 * s V_self_destruct_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_self_destruct (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((2 # 1) <= z)%Q
   | 2 => ((2 # 1) + s V_self_destruct_z <= z)%Q
   | 3 => (s V_self_destruct_z + max0(1 + s V_self_destruct_pool) <= z)%Q
   | 4 => (s V_self_destruct_z + max0(1 + s V_self_destruct_pool) <= z)%Q
   | 5 => (s V_self_destruct_z + max0(1 + s V_self_destruct_pool) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_self_destruct_pool) (s V_self_destruct_pool));
      (*-1 0*) F_max0_ge_0 (s V_self_destruct_pool)]
     (s V_self_destruct_z + max0(1 + s V_self_destruct_pool) <= z)%Q
   | 7 => (s V_self_destruct_z <= z)%Q
   | 8 => hints
     [(*0 1*) F_max0_pre_decrement 1 (1 + s V_self_destruct_pool) (1)]
     (s V_self_destruct_z + max0(1 + s V_self_destruct_pool) <= z)%Q
   | 9 => ((1 # 1) + s V_self_destruct_z + max0(s V_self_destruct_pool) <= z)%Q
   | 10 => ((1 # 1) + s V_self_destruct_z + max0(s V_self_destruct_pool) <= z)%Q
   | 11 => ((1 # 1) + s V_self_destruct_z + max0(1 + s V_self_destruct_pool) <= z)%Q
   | 12 => ((1 # 1) + s V_self_destruct_z + max0(1 + s V_self_destruct_pool) <= z)%Q
   | 13 => ((1 # 1) + s V_self_destruct_z + max0(1 + s V_self_destruct_pool) <= z)%Q
   | 14 => (s V_self_destruct_z + max0(1 + s V_self_destruct_pool) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_self_destruct =>
    [mkPA Q (fun n z s => ai_self_destruct n s /\ annot0_self_destruct n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_self_destruct (proc_start P_self_destruct) s1 (proc_end P_self_destruct) s2 ->
    (s2 V_self_destruct_z <= (2 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_self_destruct.
Qed.
