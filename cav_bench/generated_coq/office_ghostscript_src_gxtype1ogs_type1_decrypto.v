Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gs_type1_decrypt.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gs_type1_decrypt_z := 1%positive.
Notation V_gs_type1_decrypt__tmp := 2%positive.
Notation V_gs_type1_decrypt_ch := 3%positive.
Notation V_gs_type1_decrypt_count := 4%positive.
Notation V_gs_type1_decrypt_pstate_dref := 5%positive.
Notation V_gs_type1_decrypt_state := 6%positive.
Notation V_gs_type1_decrypt_dest := 7%positive.
Notation V_gs_type1_decrypt_len := 8%positive.
Notation V_gs_type1_decrypt_pstate := 9%positive.
Notation V_gs_type1_decrypt_src := 10%positive.
Definition Pedges_gs_type1_decrypt: list (edge proc) :=
  (EA 1 (AAssign V_gs_type1_decrypt_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_gs_type1_decrypt__tmp (Some (EVar V_gs_type1_decrypt_len))) 3)::
  (EA 3 (AAssign V_gs_type1_decrypt_state
  (Some (EVar V_gs_type1_decrypt_pstate_dref))) 4)::(EA 4 (AAssign
  V_gs_type1_decrypt_count (Some (EVar V_gs_type1_decrypt__tmp))) 5)::
  (EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_gs_type1_decrypt_count) s) <> (eval (ENum (0))
  s))%Z)) 12)::(EA 7 (AGuard (fun s => ((eval (EVar V_gs_type1_decrypt_count)
  s) = (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 (AAssign
  V_gs_type1_decrypt_pstate_dref
  (Some (EVar V_gs_type1_decrypt_state))) 10)::(EA 10 AWeaken 11)::
  (EA 12 AWeaken 13)::(EA 13 (AAssign V_gs_type1_decrypt_ch None) 14)::
  (EA 14 (AAssign V_gs_type1_decrypt_state None) 15)::(EA 15 (AAssign
  V_gs_type1_decrypt_count (Some (EAdd (EVar V_gs_type1_decrypt_count)
  (ENum (-1))))) 16)::(EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign
  V_gs_type1_decrypt_z (Some (EAdd (ENum (1))
  (EVar V_gs_type1_decrypt_z)))) 19)::(EA 19 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gs_type1_decrypt => Pedges_gs_type1_decrypt
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gs_type1_decrypt => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_gs_type1_decrypt (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gs_type1_decrypt_z <= 0 /\ -1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 3 => (-1 * s V_gs_type1_decrypt_z <= 0 /\ 1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 4 => (1 * s V_gs_type1_decrypt_z <= 0 /\ -1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 5 => (-1 * s V_gs_type1_decrypt_z <= 0 /\ 1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 6 => (1 * s V_gs_type1_decrypt_z <= 0 /\ -1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 7 => (-1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 8 => (-1 * s V_gs_type1_decrypt_z <= 0 /\ 1 * s V_gs_type1_decrypt_count <= 0 /\ -1 * s V_gs_type1_decrypt_count <= 0)%Z
   | 9 => (-1 * s V_gs_type1_decrypt_count <= 0 /\ 1 * s V_gs_type1_decrypt_count <= 0 /\ -1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 10 => (-1 * s V_gs_type1_decrypt_z <= 0 /\ 1 * s V_gs_type1_decrypt_count <= 0 /\ -1 * s V_gs_type1_decrypt_count <= 0)%Z
   | 11 => (-1 * s V_gs_type1_decrypt_count <= 0 /\ 1 * s V_gs_type1_decrypt_count <= 0 /\ -1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 12 => (-1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 13 => (-1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 14 => (-1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 15 => (-1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 16 => (-1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 17 => (-1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 18 => (-1 * s V_gs_type1_decrypt_z <= 0)%Z
   | 19 => (-1 * s V_gs_type1_decrypt_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gs_type1_decrypt (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_gs_type1_decrypt_len <= z)%Q
   | 2 => (s V_gs_type1_decrypt_len + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 3 => (s V_gs_type1_decrypt__tmp + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 4 => (s V_gs_type1_decrypt__tmp + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 5 => (s V_gs_type1_decrypt_count + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 6 => (s V_gs_type1_decrypt_count + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 7 => (s V_gs_type1_decrypt_count + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 8 => (s V_gs_type1_decrypt_count + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 9 => (s V_gs_type1_decrypt_count + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gs_type1_decrypt_z)) (F_check_ge (s V_gs_type1_decrypt_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_gs_type1_decrypt_count)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_type1_decrypt_count) (0))) (F_max0_ge_0 (s V_gs_type1_decrypt_count))]
     (s V_gs_type1_decrypt_count + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 11 => (s V_gs_type1_decrypt_z <= z)%Q
   | 12 => (s V_gs_type1_decrypt_count + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 13 => (s V_gs_type1_decrypt_count + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 14 => (s V_gs_type1_decrypt_count + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 15 => (s V_gs_type1_decrypt_count + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 16 => ((1 # 1) + s V_gs_type1_decrypt_count
            + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 17 => ((1 # 1) + s V_gs_type1_decrypt_count
            + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 18 => ((1 # 1) + s V_gs_type1_decrypt_count
            + max0(s V_gs_type1_decrypt_z) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gs_type1_decrypt_z) (0))) (F_max0_ge_0 (s V_gs_type1_decrypt_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_gs_type1_decrypt_z)) (F_check_ge (-1
                                                                    + s V_gs_type1_decrypt_z) (0))]
     ((1 # 1) + s V_gs_type1_decrypt_count
      + max0(-1 + s V_gs_type1_decrypt_z) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gs_type1_decrypt =>
    [mkPA Q (fun n z s => ai_gs_type1_decrypt n s /\ annot0_gs_type1_decrypt n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gs_type1_decrypt (proc_start P_gs_type1_decrypt) s1 (proc_end P_gs_type1_decrypt) s2 ->
    (s2 V_gs_type1_decrypt_z <= s1 V_gs_type1_decrypt_len)%Q.
Proof.
  prove_bound ipa admissible_ipa P_gs_type1_decrypt.
Qed.
