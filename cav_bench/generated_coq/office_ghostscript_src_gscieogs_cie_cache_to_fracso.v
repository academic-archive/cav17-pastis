Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gs_cie_cache_to_fracs.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gs_cie_cache_to_fracs_z := 1%positive.
Notation V_gs_cie_cache_to_fracs_i := 2%positive.
Notation V_gs_cie_cache_to_fracs_pcache := 3%positive.
Definition Pedges_gs_cie_cache_to_fracs: list (edge proc) :=
  (EA 1 (AAssign V_gs_cie_cache_to_fracs_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_gs_cie_cache_to_fracs_i (Some (ENum (0)))) 3)::
  (EA 3 ANone 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_gs_cie_cache_to_fracs_i) s) < (eval (ENum (512))
  s))%Z)) 8)::(EA 5 (AGuard (fun s => ((eval (EVar V_gs_cie_cache_to_fracs_i)
  s) >= (eval (ENum (512)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 8 AWeaken 9)::
  (EA 9 ANone 10)::(EA 10 (AAssign V_gs_cie_cache_to_fracs_i
  (Some (EAdd (EVar V_gs_cie_cache_to_fracs_i) (ENum (1))))) 11)::
  (EA 11 ANone 12)::(EA 12 ANone 13)::(EA 13 (AAssign
  V_gs_cie_cache_to_fracs_z (Some (EAdd (ENum (1))
  (EVar V_gs_cie_cache_to_fracs_z)))) 14)::(EA 14 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gs_cie_cache_to_fracs => Pedges_gs_cie_cache_to_fracs
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gs_cie_cache_to_fracs => 7
     end)%positive;
  var_global := var_global
}.

Definition ai_gs_cie_cache_to_fracs (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gs_cie_cache_to_fracs_z <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_z <= 0)%Z
   | 3 => (-1 * s V_gs_cie_cache_to_fracs_z <= 0 /\ 1 * s V_gs_cie_cache_to_fracs_z <= 0 /\ 1 * s V_gs_cie_cache_to_fracs_i <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_i <= 0)%Z
   | 4 => (-1 * s V_gs_cie_cache_to_fracs_i <= 0 /\ 1 * s V_gs_cie_cache_to_fracs_i <= 0 /\ 1 * s V_gs_cie_cache_to_fracs_z <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_z <= 0)%Z
   | 5 => (-1 * s V_gs_cie_cache_to_fracs_z <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_i <= 0 /\ 1 * s V_gs_cie_cache_to_fracs_i + -512 <= 0)%Z
   | 6 => (1 * s V_gs_cie_cache_to_fracs_i + -512 <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_z <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_i + 512 <= 0)%Z
   | 7 => (-1 * s V_gs_cie_cache_to_fracs_i + 512 <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_z <= 0 /\ 1 * s V_gs_cie_cache_to_fracs_i + -512 <= 0)%Z
   | 8 => (-1 * s V_gs_cie_cache_to_fracs_i <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_z <= 0 /\ 1 * s V_gs_cie_cache_to_fracs_i + -511 <= 0)%Z
   | 9 => (1 * s V_gs_cie_cache_to_fracs_i + -511 <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_z <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_i <= 0)%Z
   | 10 => (-1 * s V_gs_cie_cache_to_fracs_i <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_z <= 0 /\ 1 * s V_gs_cie_cache_to_fracs_i + -511 <= 0)%Z
   | 11 => (-1 * s V_gs_cie_cache_to_fracs_z <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_i + 1 <= 0 /\ 1 * s V_gs_cie_cache_to_fracs_i + -512 <= 0)%Z
   | 12 => (1 * s V_gs_cie_cache_to_fracs_i + -512 <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_i + 1 <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_z <= 0)%Z
   | 13 => (-1 * s V_gs_cie_cache_to_fracs_z <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_i + 1 <= 0 /\ 1 * s V_gs_cie_cache_to_fracs_i + -512 <= 0)%Z
   | 14 => (1 * s V_gs_cie_cache_to_fracs_i + -512 <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_i + 1 <= 0 /\ -1 * s V_gs_cie_cache_to_fracs_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gs_cie_cache_to_fracs (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((512 # 1) <= z)%Q
   | 2 => ((512 # 1) + s V_gs_cie_cache_to_fracs_z <= z)%Q
   | 3 => (s V_gs_cie_cache_to_fracs_z
           + max0(512 - s V_gs_cie_cache_to_fracs_i) <= z)%Q
   | 4 => (s V_gs_cie_cache_to_fracs_z
           + max0(512 - s V_gs_cie_cache_to_fracs_i) <= z)%Q
   | 5 => (s V_gs_cie_cache_to_fracs_z
           + max0(512 - s V_gs_cie_cache_to_fracs_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (512
                                             - s V_gs_cie_cache_to_fracs_i) (511
                                                                    - s V_gs_cie_cache_to_fracs_i));
      (*-1 0*) F_max0_ge_0 (511 - s V_gs_cie_cache_to_fracs_i)]
     (s V_gs_cie_cache_to_fracs_z + max0(512 - s V_gs_cie_cache_to_fracs_i) <= z)%Q
   | 7 => (s V_gs_cie_cache_to_fracs_z <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (512 - s V_gs_cie_cache_to_fracs_i) (1)]
     (s V_gs_cie_cache_to_fracs_z + max0(512 - s V_gs_cie_cache_to_fracs_i) <= z)%Q
   | 9 => ((1 # 1) + s V_gs_cie_cache_to_fracs_z
           + max0(511 - s V_gs_cie_cache_to_fracs_i) <= z)%Q
   | 10 => ((1 # 1) + s V_gs_cie_cache_to_fracs_z
            + max0(511 - s V_gs_cie_cache_to_fracs_i) <= z)%Q
   | 11 => ((1 # 1) + s V_gs_cie_cache_to_fracs_z
            + max0(512 - s V_gs_cie_cache_to_fracs_i) <= z)%Q
   | 12 => ((1 # 1) + s V_gs_cie_cache_to_fracs_z
            + max0(512 - s V_gs_cie_cache_to_fracs_i) <= z)%Q
   | 13 => ((1 # 1) + s V_gs_cie_cache_to_fracs_z
            + max0(512 - s V_gs_cie_cache_to_fracs_i) <= z)%Q
   | 14 => (s V_gs_cie_cache_to_fracs_z
            + max0(512 - s V_gs_cie_cache_to_fracs_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gs_cie_cache_to_fracs =>
    [mkPA Q (fun n z s => ai_gs_cie_cache_to_fracs n s /\ annot0_gs_cie_cache_to_fracs n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gs_cie_cache_to_fracs (proc_start P_gs_cie_cache_to_fracs) s1 (proc_end P_gs_cie_cache_to_fracs) s2 ->
    (s2 V_gs_cie_cache_to_fracs_z <= (512 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gs_cie_cache_to_fracs.
Qed.
