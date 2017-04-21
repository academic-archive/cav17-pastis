Require Import pasta.Pasta.

Inductive proc: Type :=
  P_Luv32fromXYZ.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_Luv32fromXYZ_z := 1%positive.
Notation V_Luv32fromXYZ__tmp := 2%positive.
Notation V_Luv32fromXYZ_n := 3%positive.
Notation V_Luv32fromXYZ_op := 4%positive.
Notation V_Luv32fromXYZ_sp := 5%positive.
Definition Pedges_Luv32fromXYZ: list (edge proc) :=
  (EA 1 (AAssign V_Luv32fromXYZ_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_Luv32fromXYZ__tmp (Some (EVar V_Luv32fromXYZ_n))) 3)::(EA 3 ANone 4)::
  (EA 4 (AAssign V_Luv32fromXYZ__tmp (Some (EAdd (EVar V_Luv32fromXYZ__tmp)
  (ENum (-1))))) 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_Luv32fromXYZ__tmp) s) > (eval (ENum (0))
  s))%Z)) 9)::(EA 6 (AGuard (fun s => ((eval (EVar V_Luv32fromXYZ__tmp) s) <=
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 ANone 11)::(EA 11 ANone 12)::(EA 12 (AAssign V_Luv32fromXYZ_z
  (Some (EAdd (ENum (1)) (EVar V_Luv32fromXYZ_z)))) 4)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_Luv32fromXYZ => Pedges_Luv32fromXYZ
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_Luv32fromXYZ => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_Luv32fromXYZ (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_Luv32fromXYZ_z <= 0 /\ -1 * s V_Luv32fromXYZ_z <= 0)%Z
   | 3 => (-1 * s V_Luv32fromXYZ_z <= 0 /\ 1 * s V_Luv32fromXYZ_z <= 0)%Z
   | 4 => (-1 * s V_Luv32fromXYZ_z <= 0)%Z
   | 5 => (-1 * s V_Luv32fromXYZ_z <= 0)%Z
   | 6 => (-1 * s V_Luv32fromXYZ_z <= 0)%Z
   | 7 => (-1 * s V_Luv32fromXYZ_z <= 0 /\ 1 * s V_Luv32fromXYZ__tmp <= 0)%Z
   | 8 => (1 * s V_Luv32fromXYZ__tmp <= 0 /\ -1 * s V_Luv32fromXYZ_z <= 0)%Z
   | 9 => (-1 * s V_Luv32fromXYZ_z <= 0 /\ -1 * s V_Luv32fromXYZ__tmp + 1 <= 0)%Z
   | 10 => (-1 * s V_Luv32fromXYZ__tmp + 1 <= 0 /\ -1 * s V_Luv32fromXYZ_z <= 0)%Z
   | 11 => (-1 * s V_Luv32fromXYZ_z <= 0 /\ -1 * s V_Luv32fromXYZ__tmp + 1 <= 0)%Z
   | 12 => (-1 * s V_Luv32fromXYZ__tmp + 1 <= 0 /\ -1 * s V_Luv32fromXYZ_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_Luv32fromXYZ (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_Luv32fromXYZ_n) <= z)%Q
   | 2 => (s V_Luv32fromXYZ_z + max0(-1 + s V_Luv32fromXYZ_n) <= z)%Q
   | 3 => (s V_Luv32fromXYZ_z + max0(-1 + s V_Luv32fromXYZ__tmp) <= z)%Q
   | 4 => (s V_Luv32fromXYZ_z + max0(-1 + s V_Luv32fromXYZ__tmp) <= z)%Q
   | 5 => (s V_Luv32fromXYZ_z + max0(s V_Luv32fromXYZ__tmp) <= z)%Q
   | 6 => (s V_Luv32fromXYZ_z + max0(s V_Luv32fromXYZ__tmp) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_Luv32fromXYZ__tmp) (-1
                                                                    + 
                                                                    s V_Luv32fromXYZ__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_Luv32fromXYZ__tmp)]
     (s V_Luv32fromXYZ_z + max0(s V_Luv32fromXYZ__tmp) <= z)%Q
   | 8 => (s V_Luv32fromXYZ_z <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_Luv32fromXYZ__tmp) (1)]
     (s V_Luv32fromXYZ_z + max0(s V_Luv32fromXYZ__tmp) <= z)%Q
   | 10 => ((1 # 1) + s V_Luv32fromXYZ_z + max0(-1 + s V_Luv32fromXYZ__tmp) <= z)%Q
   | 11 => ((1 # 1) + s V_Luv32fromXYZ_z + max0(-1 + s V_Luv32fromXYZ__tmp) <= z)%Q
   | 12 => ((1 # 1) + s V_Luv32fromXYZ_z + max0(-1 + s V_Luv32fromXYZ__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_Luv32fromXYZ =>
    [mkPA Q (fun n z s => ai_Luv32fromXYZ n s /\ annot0_Luv32fromXYZ n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_Luv32fromXYZ (proc_start P_Luv32fromXYZ) s1 (proc_end P_Luv32fromXYZ) s2 ->
    (s2 V_Luv32fromXYZ_z <= max0(-1 + s1 V_Luv32fromXYZ_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_Luv32fromXYZ.
Qed.
