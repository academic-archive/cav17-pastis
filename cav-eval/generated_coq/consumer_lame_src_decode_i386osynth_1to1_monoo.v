Require Import pasta.Pasta.

Inductive proc: Type :=
  P_synth_1to1_mono.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_synth_1to1_mono_z := 1%positive.
Notation V_synth_1to1_mono_i := 2%positive.
Notation V_synth_1to1_mono_pnt_dref := 3%positive.
Notation V_synth_1to1_mono_ret := 4%positive.
Notation V_synth_1to1_mono_bandPtr := 5%positive.
Notation V_synth_1to1_mono_pnt := 6%positive.
Notation V_synth_1to1_mono_samples := 7%positive.
Definition Pedges_synth_1to1_mono: list (edge proc) :=
  (EA 1 (AAssign V_synth_1to1_mono_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_synth_1to1_mono_ret None) 3)::(EA 3 (AAssign V_synth_1to1_mono_i
  (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_synth_1to1_mono_i) s) < (eval (ENum (32))
  s))%Z)) 11)::(EA 6 (AGuard (fun s => ((eval (EVar V_synth_1to1_mono_i)
  s) >= (eval (ENum (32)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_synth_1to1_mono_pnt_dref (Some (EAdd (EVar V_synth_1to1_mono_pnt_dref)
  (ENum (64))))) 9)::(EA 9 AWeaken 10)::(EA 11 AWeaken 12)::
  (EA 12 ANone 13)::(EA 13 (AAssign V_synth_1to1_mono_i
  (Some (EAdd (EVar V_synth_1to1_mono_i) (ENum (1))))) 14)::
  (EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign V_synth_1to1_mono_z
  (Some (EAdd (ENum (1)) (EVar V_synth_1to1_mono_z)))) 17)::
  (EA 17 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_synth_1to1_mono => Pedges_synth_1to1_mono
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_synth_1to1_mono => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_synth_1to1_mono (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_synth_1to1_mono_z <= 0 /\ -1 * s V_synth_1to1_mono_z <= 0)%Z
   | 3 => (-1 * s V_synth_1to1_mono_z <= 0 /\ 1 * s V_synth_1to1_mono_z <= 0)%Z
   | 4 => (1 * s V_synth_1to1_mono_z <= 0 /\ -1 * s V_synth_1to1_mono_z <= 0 /\ 1 * s V_synth_1to1_mono_i <= 0 /\ -1 * s V_synth_1to1_mono_i <= 0)%Z
   | 5 => (-1 * s V_synth_1to1_mono_i <= 0 /\ 1 * s V_synth_1to1_mono_i <= 0 /\ -1 * s V_synth_1to1_mono_z <= 0 /\ 1 * s V_synth_1to1_mono_z <= 0)%Z
   | 6 => (-1 * s V_synth_1to1_mono_z <= 0 /\ -1 * s V_synth_1to1_mono_i <= 0 /\ 1 * s V_synth_1to1_mono_i + -32 <= 0)%Z
   | 7 => (1 * s V_synth_1to1_mono_i + -32 <= 0 /\ -1 * s V_synth_1to1_mono_z <= 0 /\ -1 * s V_synth_1to1_mono_i + 32 <= 0)%Z
   | 8 => (-1 * s V_synth_1to1_mono_i + 32 <= 0 /\ -1 * s V_synth_1to1_mono_z <= 0 /\ 1 * s V_synth_1to1_mono_i + -32 <= 0)%Z
   | 9 => (1 * s V_synth_1to1_mono_i + -32 <= 0 /\ -1 * s V_synth_1to1_mono_z <= 0 /\ -1 * s V_synth_1to1_mono_i + 32 <= 0)%Z
   | 10 => (-1 * s V_synth_1to1_mono_i + 32 <= 0 /\ -1 * s V_synth_1to1_mono_z <= 0 /\ 1 * s V_synth_1to1_mono_i + -32 <= 0)%Z
   | 11 => (-1 * s V_synth_1to1_mono_i <= 0 /\ -1 * s V_synth_1to1_mono_z <= 0 /\ 1 * s V_synth_1to1_mono_i + -31 <= 0)%Z
   | 12 => (1 * s V_synth_1to1_mono_i + -31 <= 0 /\ -1 * s V_synth_1to1_mono_z <= 0 /\ -1 * s V_synth_1to1_mono_i <= 0)%Z
   | 13 => (-1 * s V_synth_1to1_mono_i <= 0 /\ -1 * s V_synth_1to1_mono_z <= 0 /\ 1 * s V_synth_1to1_mono_i + -31 <= 0)%Z
   | 14 => (-1 * s V_synth_1to1_mono_z <= 0 /\ -1 * s V_synth_1to1_mono_i + 1 <= 0 /\ 1 * s V_synth_1to1_mono_i + -32 <= 0)%Z
   | 15 => (1 * s V_synth_1to1_mono_i + -32 <= 0 /\ -1 * s V_synth_1to1_mono_i + 1 <= 0 /\ -1 * s V_synth_1to1_mono_z <= 0)%Z
   | 16 => (-1 * s V_synth_1to1_mono_z <= 0 /\ -1 * s V_synth_1to1_mono_i + 1 <= 0 /\ 1 * s V_synth_1to1_mono_i + -32 <= 0)%Z
   | 17 => (1 * s V_synth_1to1_mono_i + -32 <= 0 /\ -1 * s V_synth_1to1_mono_i + 1 <= 0 /\ -1 * s V_synth_1to1_mono_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_synth_1to1_mono (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((32 # 1) <= z)%Q
   | 2 => ((32 # 1) + s V_synth_1to1_mono_z <= z)%Q
   | 3 => ((32 # 1) + s V_synth_1to1_mono_z <= z)%Q
   | 4 => (s V_synth_1to1_mono_z + max0(32 - s V_synth_1to1_mono_i) <= z)%Q
   | 5 => (s V_synth_1to1_mono_z + max0(32 - s V_synth_1to1_mono_i) <= z)%Q
   | 6 => (s V_synth_1to1_mono_z + max0(32 - s V_synth_1to1_mono_i) <= z)%Q
   | 7 => (s V_synth_1to1_mono_z + max0(32 - s V_synth_1to1_mono_i) <= z)%Q
   | 8 => (s V_synth_1to1_mono_z + max0(32 - s V_synth_1to1_mono_i) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (32 - s V_synth_1to1_mono_i) (31
                                                                    - s V_synth_1to1_mono_i));
      (*-1 0*) F_max0_ge_0 (31 - s V_synth_1to1_mono_i)]
     (s V_synth_1to1_mono_z + max0(32 - s V_synth_1to1_mono_i) <= z)%Q
   | 10 => (s V_synth_1to1_mono_z <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (32 - s V_synth_1to1_mono_i) (1)]
     (s V_synth_1to1_mono_z + max0(32 - s V_synth_1to1_mono_i) <= z)%Q
   | 12 => ((1 # 1) + s V_synth_1to1_mono_z
            + max0(31 - s V_synth_1to1_mono_i) <= z)%Q
   | 13 => ((1 # 1) + s V_synth_1to1_mono_z
            + max0(31 - s V_synth_1to1_mono_i) <= z)%Q
   | 14 => ((1 # 1) + s V_synth_1to1_mono_z
            + max0(32 - s V_synth_1to1_mono_i) <= z)%Q
   | 15 => ((1 # 1) + s V_synth_1to1_mono_z
            + max0(32 - s V_synth_1to1_mono_i) <= z)%Q
   | 16 => ((1 # 1) + s V_synth_1to1_mono_z
            + max0(32 - s V_synth_1to1_mono_i) <= z)%Q
   | 17 => (s V_synth_1to1_mono_z + max0(32 - s V_synth_1to1_mono_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_synth_1to1_mono =>
    [mkPA Q (fun n z s => ai_synth_1to1_mono n s /\ annot0_synth_1to1_mono n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_synth_1to1_mono (proc_start P_synth_1to1_mono) s1 (proc_end P_synth_1to1_mono) s2 ->
    (s2 V_synth_1to1_mono_z <= (32 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_synth_1to1_mono.
Qed.
