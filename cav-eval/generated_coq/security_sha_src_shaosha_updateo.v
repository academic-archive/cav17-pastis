Require Import pasta.Pasta.

Inductive proc: Type :=
  P_sha_update.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_sha_update_z := 1%positive.
Notation V_sha_update__tmp := 2%positive.
Notation V_sha_update_buffer := 3%positive.
Notation V_sha_update_count := 4%positive.
Notation V_sha_update_sha_info := 5%positive.
Definition Pedges_sha_update: list (edge proc) :=
  (EA 1 (AAssign V_sha_update_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_sha_update__tmp (Some (EVar V_sha_update_count))) 3)::(EA 3 AWeaken 4)::
  (EA 4 ANone 5)::(EA 4 ANone 6)::(EA 5 ANone 6)::(EA 6 ANone 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard (fun s => ((eval (EVar V_sha_update__tmp)
  s) >= (eval (ENum (64)) s))%Z)) 11)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_sha_update__tmp) s) < (eval (ENum (64))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_sha_update__tmp (Some (ESub (EVar V_sha_update__tmp) (ENum (64))))) 13)::
  (EA 13 ANone 14)::(EA 14 ANone 15)::(EA 15 (AAssign V_sha_update_z
  (Some (EAdd (ENum (1)) (EVar V_sha_update_z)))) 16)::(EA 16 AWeaken 8)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_sha_update => Pedges_sha_update
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_sha_update => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_sha_update (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_sha_update_z <= 0 /\ -1 * s V_sha_update_z <= 0)%Z
   | 3 => (-1 * s V_sha_update_z <= 0 /\ 1 * s V_sha_update_z <= 0)%Z
   | 4 => (1 * s V_sha_update_z <= 0 /\ -1 * s V_sha_update_z <= 0)%Z
   | 5 => (-1 * s V_sha_update_z <= 0 /\ 1 * s V_sha_update_z <= 0)%Z
   | 6 => (1 * s V_sha_update_z <= 0 /\ -1 * s V_sha_update_z <= 0)%Z
   | 7 => (-1 * s V_sha_update_z <= 0 /\ 1 * s V_sha_update_z <= 0)%Z
   | 8 => (-1 * s V_sha_update_z <= 0)%Z
   | 9 => (-1 * s V_sha_update_z <= 0 /\ 1 * s V_sha_update__tmp + -63 <= 0)%Z
   | 10 => (1 * s V_sha_update__tmp + -63 <= 0 /\ -1 * s V_sha_update_z <= 0)%Z
   | 11 => (-1 * s V_sha_update_z <= 0 /\ -1 * s V_sha_update__tmp + 64 <= 0)%Z
   | 12 => (-1 * s V_sha_update__tmp + 64 <= 0 /\ -1 * s V_sha_update_z <= 0)%Z
   | 13 => (-1 * s V_sha_update_z <= 0 /\ -1 * s V_sha_update__tmp <= 0)%Z
   | 14 => (-1 * s V_sha_update__tmp <= 0 /\ -1 * s V_sha_update_z <= 0)%Z
   | 15 => (-1 * s V_sha_update_z <= 0 /\ -1 * s V_sha_update__tmp <= 0)%Z
   | 16 => (-1 * s V_sha_update__tmp <= 0 /\ -1 * s V_sha_update_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_sha_update (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 64) * max0(s V_sha_update_count) <= z)%Q
   | 2 => ((1 # 64) * max0(s V_sha_update_count) + max0(s V_sha_update_z) <= z)%Q
   | 3 => ((1 # 64) * max0(s V_sha_update__tmp) + max0(s V_sha_update_z) <= z)%Q
   | 4 => ((1 # 64) * max0(s V_sha_update__tmp) + max0(s V_sha_update_z) <= z)%Q
   | 5 => ((1 # 64) * max0(s V_sha_update__tmp) + max0(s V_sha_update_z) <= z)%Q
   | 6 => ((1 # 64) * max0(s V_sha_update__tmp) + max0(s V_sha_update_z) <= z)%Q
   | 7 => ((1 # 64) * max0(s V_sha_update__tmp) + max0(s V_sha_update_z) <= z)%Q
   | 8 => ((1 # 64) * max0(s V_sha_update__tmp) + max0(s V_sha_update_z) <= z)%Q
   | 9 => hints
     [(*-0.015625 0*) F_max0_monotonic (F_check_ge (s V_sha_update__tmp) (-64
                                                                    + s V_sha_update__tmp));
      (*-0.015625 0*) F_max0_ge_0 (-64 + s V_sha_update__tmp);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_sha_update_z)) (F_check_ge (s V_sha_update_z) (0))]
     ((1 # 64) * max0(s V_sha_update__tmp) + max0(s V_sha_update_z) <= z)%Q
   | 10 => (s V_sha_update_z <= z)%Q
   | 11 => hints
     [(*-0.015625 0*) F_max0_pre_decrement 1 (s V_sha_update__tmp) (64)]
     ((1 # 64) * max0(s V_sha_update__tmp) + max0(s V_sha_update_z) <= z)%Q
   | 12 => ((1 # 1) + (1 # 64) * max0(-64 + s V_sha_update__tmp)
            + max0(s V_sha_update_z) <= z)%Q
   | 13 => ((1 # 1) + (1 # 64) * max0(s V_sha_update__tmp)
            + max0(s V_sha_update_z) <= z)%Q
   | 14 => ((1 # 1) + (1 # 64) * max0(s V_sha_update__tmp)
            + max0(s V_sha_update_z) <= z)%Q
   | 15 => ((1 # 1) + (1 # 64) * max0(s V_sha_update__tmp)
            + max0(s V_sha_update_z) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_sha_update_z) (0))) (F_max0_ge_0 (s V_sha_update_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_sha_update_z)) (F_check_ge (-1
                                                                    + s V_sha_update_z) (0))]
     ((1 # 1) + max0(-1 + s V_sha_update_z)
      + (1 # 64) * max0(s V_sha_update__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_sha_update =>
    [mkPA Q (fun n z s => ai_sha_update n s /\ annot0_sha_update n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_sha_update (proc_start P_sha_update) s1 (proc_end P_sha_update) s2 ->
    (s2 V_sha_update_z <= (1 # 64) * max0(s1 V_sha_update_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_sha_update.
Qed.
