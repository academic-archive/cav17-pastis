Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mp_rotate_left.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mp_rotate_left_z := 1%positive.
Notation V_mp_rotate_left__tmp := 2%positive.
Notation V_mp_rotate_left_global_precision := 3%positive.
Notation V_mp_rotate_left_mcarry := 4%positive.
Notation V_mp_rotate_left_nextcarry := 5%positive.
Notation V_mp_rotate_left_precision := 6%positive.
Notation V_mp_rotate_left_carry := 7%positive.
Notation V_mp_rotate_left_r1 := 8%positive.
Definition Pedges_mp_rotate_left: list (edge proc) :=
  (EA 1 (AAssign V_mp_rotate_left_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_mp_rotate_left__tmp (Some (EVar V_mp_rotate_left_carry))) 3)::
  (EA 3 (AAssign V_mp_rotate_left_mcarry
  (Some (EVar V_mp_rotate_left__tmp))) 4)::(EA 4 (AAssign
  V_mp_rotate_left_precision
  (Some (EVar V_mp_rotate_left_global_precision))) 5)::(EA 5 ANone 6)::
  (EA 6 (AAssign V_mp_rotate_left_precision
  (Some (EAdd (EVar V_mp_rotate_left_precision) (ENum (-1))))) 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_mp_rotate_left_precision) s) <> (eval (ENum (0))
  s))%Z)) 11)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_mp_rotate_left_precision) s) = (eval (ENum (0))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_mp_rotate_left_nextcarry None) 13)::(EA 13 (AAssign
  V_mp_rotate_left_mcarry (Some (EVar V_mp_rotate_left_nextcarry))) 14)::
  (EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign V_mp_rotate_left_z
  (Some (EAdd (ENum (1)) (EVar V_mp_rotate_left_z)))) 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mp_rotate_left => Pedges_mp_rotate_left
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mp_rotate_left => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_mp_rotate_left (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mp_rotate_left_z <= 0 /\ -1 * s V_mp_rotate_left_z <= 0)%Z
   | 3 => (-1 * s V_mp_rotate_left_z <= 0 /\ 1 * s V_mp_rotate_left_z <= 0)%Z
   | 4 => (1 * s V_mp_rotate_left_z <= 0 /\ -1 * s V_mp_rotate_left_z <= 0)%Z
   | 5 => (-1 * s V_mp_rotate_left_z <= 0 /\ 1 * s V_mp_rotate_left_z <= 0)%Z
   | 6 => (-1 * s V_mp_rotate_left_z <= 0)%Z
   | 7 => (-1 * s V_mp_rotate_left_z <= 0)%Z
   | 8 => (-1 * s V_mp_rotate_left_z <= 0)%Z
   | 9 => (-1 * s V_mp_rotate_left_z <= 0 /\ 1 * s V_mp_rotate_left_precision <= 0 /\ -1 * s V_mp_rotate_left_precision <= 0)%Z
   | 10 => (-1 * s V_mp_rotate_left_precision <= 0 /\ 1 * s V_mp_rotate_left_precision <= 0 /\ -1 * s V_mp_rotate_left_z <= 0)%Z
   | 11 => (-1 * s V_mp_rotate_left_z <= 0)%Z
   | 12 => (-1 * s V_mp_rotate_left_z <= 0)%Z
   | 13 => (-1 * s V_mp_rotate_left_z <= 0)%Z
   | 14 => (-1 * s V_mp_rotate_left_z <= 0)%Z
   | 15 => (-1 * s V_mp_rotate_left_z <= 0)%Z
   | 16 => (-1 * s V_mp_rotate_left_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mp_rotate_left (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_mp_rotate_left_global_precision <= z)%Q
   | 2 => (s V_mp_rotate_left_global_precision + s V_mp_rotate_left_z <= z)%Q
   | 3 => (s V_mp_rotate_left_global_precision + s V_mp_rotate_left_z <= z)%Q
   | 4 => (s V_mp_rotate_left_global_precision + s V_mp_rotate_left_z <= z)%Q
   | 5 => (s V_mp_rotate_left_precision + s V_mp_rotate_left_z <= z)%Q
   | 6 => (s V_mp_rotate_left_precision + s V_mp_rotate_left_z <= z)%Q
   | 7 => ((1 # 1) + s V_mp_rotate_left_precision + s V_mp_rotate_left_z <= z)%Q
   | 8 => ((1 # 1) + s V_mp_rotate_left_precision + s V_mp_rotate_left_z <= z)%Q
   | 9 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_mp_rotate_left_precision)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mp_rotate_left_precision) (0))) (F_max0_ge_0 (s V_mp_rotate_left_precision))]
     ((1 # 1) + s V_mp_rotate_left_precision + s V_mp_rotate_left_z <= z)%Q
   | 10 => (s V_mp_rotate_left_z <= z)%Q
   | 11 => ((1 # 1) + s V_mp_rotate_left_precision + s V_mp_rotate_left_z <= z)%Q
   | 12 => ((1 # 1) + s V_mp_rotate_left_precision + s V_mp_rotate_left_z <= z)%Q
   | 13 => ((1 # 1) + s V_mp_rotate_left_precision + s V_mp_rotate_left_z <= z)%Q
   | 14 => ((1 # 1) + s V_mp_rotate_left_precision + s V_mp_rotate_left_z <= z)%Q
   | 15 => ((1 # 1) + s V_mp_rotate_left_precision + s V_mp_rotate_left_z <= z)%Q
   | 16 => ((1 # 1) + s V_mp_rotate_left_precision + s V_mp_rotate_left_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mp_rotate_left =>
    [mkPA Q (fun n z s => ai_mp_rotate_left n s /\ annot0_mp_rotate_left n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mp_rotate_left (proc_start P_mp_rotate_left) s1 (proc_end P_mp_rotate_left) s2 ->
    (s2 V_mp_rotate_left_z <= s1 V_mp_rotate_left_global_precision)%Q.
Proof.
  prove_bound ipa admissible_ipa P_mp_rotate_left.
Qed.
