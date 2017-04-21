Require Import pasta.Pasta.

Inductive proc: Type :=
  P_jpeg_set_defaults.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_jpeg_set_defaults_z := 1%positive.
Notation V_jpeg_set_defaults_i := 2%positive.
Notation V_jpeg_set_defaults_cinfo := 3%positive.
Definition Pedges_jpeg_set_defaults: list (edge proc) :=
  (EA 1 (AAssign V_jpeg_set_defaults_z (Some (ENum (0)))) 2)::
  (EA 2 AWeaken 3)::(EA 3 ANone 5)::(EA 3 ANone 4)::(EA 4 AWeaken 7)::
  (EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 ANone 8)::(EA 7 ANone 9)::
  (EA 8 ANone 9)::(EA 9 (AAssign V_jpeg_set_defaults_i
  (Some (ENum (0)))) 10)::(EA 10 ANone 11)::(EA 11 AWeaken 12)::
  (EA 12 (AGuard (fun s => ((eval (EVar V_jpeg_set_defaults_i) s) <
  (eval (ENum (16)) s))%Z)) 19)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_jpeg_set_defaults_i) s) >= (eval (ENum (16))
  s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 ANone 16)::(EA 14 ANone 15)::
  (EA 15 AWeaken 18)::(EA 16 ANone 17)::(EA 17 AWeaken 18)::
  (EA 19 AWeaken 20)::(EA 20 ANone 21)::(EA 21 (AAssign V_jpeg_set_defaults_i
  (Some (EAdd (EVar V_jpeg_set_defaults_i) (ENum (1))))) 22)::
  (EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 (AAssign V_jpeg_set_defaults_z
  (Some (EAdd (ENum (1)) (EVar V_jpeg_set_defaults_z)))) 25)::
  (EA 25 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_jpeg_set_defaults => Pedges_jpeg_set_defaults
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_jpeg_set_defaults => 18
     end)%positive;
  var_global := var_global
}.

Definition ai_jpeg_set_defaults (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0)%Z
   | 3 => (-1 * s V_jpeg_set_defaults_z <= 0 /\ 1 * s V_jpeg_set_defaults_z <= 0)%Z
   | 4 => (1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0)%Z
   | 5 => (1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0)%Z
   | 6 => (-1 * s V_jpeg_set_defaults_z <= 0 /\ 1 * s V_jpeg_set_defaults_z <= 0)%Z
   | 7 => (1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0)%Z
   | 8 => (-1 * s V_jpeg_set_defaults_z <= 0 /\ 1 * s V_jpeg_set_defaults_z <= 0)%Z
   | 9 => (1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0)%Z
   | 10 => (-1 * s V_jpeg_set_defaults_z <= 0 /\ 1 * s V_jpeg_set_defaults_z <= 0 /\ 1 * s V_jpeg_set_defaults_i <= 0 /\ -1 * s V_jpeg_set_defaults_i <= 0)%Z
   | 11 => (-1 * s V_jpeg_set_defaults_i <= 0 /\ 1 * s V_jpeg_set_defaults_i <= 0 /\ 1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0)%Z
   | 12 => (-1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_i <= 0 /\ 1 * s V_jpeg_set_defaults_i + -16 <= 0)%Z
   | 13 => (1 * s V_jpeg_set_defaults_i + -16 <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_i + 16 <= 0)%Z
   | 14 => (-1 * s V_jpeg_set_defaults_i + 16 <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0 /\ 1 * s V_jpeg_set_defaults_i + -16 <= 0)%Z
   | 15 => (1 * s V_jpeg_set_defaults_i + -16 <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_i + 16 <= 0)%Z
   | 16 => (1 * s V_jpeg_set_defaults_i + -16 <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_i + 16 <= 0)%Z
   | 17 => (-1 * s V_jpeg_set_defaults_i + 16 <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0 /\ 1 * s V_jpeg_set_defaults_i + -16 <= 0)%Z
   | 18 => (1 * s V_jpeg_set_defaults_i + -16 <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_i + 16 <= 0)%Z
   | 19 => (-1 * s V_jpeg_set_defaults_i <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0 /\ 1 * s V_jpeg_set_defaults_i + -15 <= 0)%Z
   | 20 => (1 * s V_jpeg_set_defaults_i + -15 <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_i <= 0)%Z
   | 21 => (-1 * s V_jpeg_set_defaults_i <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0 /\ 1 * s V_jpeg_set_defaults_i + -15 <= 0)%Z
   | 22 => (-1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_i + 1 <= 0 /\ 1 * s V_jpeg_set_defaults_i + -16 <= 0)%Z
   | 23 => (1 * s V_jpeg_set_defaults_i + -16 <= 0 /\ -1 * s V_jpeg_set_defaults_i + 1 <= 0 /\ -1 * s V_jpeg_set_defaults_z <= 0)%Z
   | 24 => (-1 * s V_jpeg_set_defaults_z <= 0 /\ -1 * s V_jpeg_set_defaults_i + 1 <= 0 /\ 1 * s V_jpeg_set_defaults_i + -16 <= 0)%Z
   | 25 => (1 * s V_jpeg_set_defaults_i + -16 <= 0 /\ -1 * s V_jpeg_set_defaults_i + 1 <= 0 /\ -1 * s V_jpeg_set_defaults_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_jpeg_set_defaults (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((16 # 1) <= z)%Q
   | 2 => ((16 # 1) + s V_jpeg_set_defaults_z <= z)%Q
   | 3 => ((16 # 1) + s V_jpeg_set_defaults_z <= z)%Q
   | 4 => ((16 # 1) + s V_jpeg_set_defaults_z <= z)%Q
   | 5 => ((16 # 1) + s V_jpeg_set_defaults_z <= z)%Q
   | 6 => ((16 # 1) + s V_jpeg_set_defaults_z <= z)%Q
   | 7 => ((16 # 1) + s V_jpeg_set_defaults_z <= z)%Q
   | 8 => ((16 # 1) + s V_jpeg_set_defaults_z <= z)%Q
   | 9 => ((16 # 1) + s V_jpeg_set_defaults_z <= z)%Q
   | 10 => (s V_jpeg_set_defaults_z + max0(16 - s V_jpeg_set_defaults_i) <= z)%Q
   | 11 => (s V_jpeg_set_defaults_z + max0(16 - s V_jpeg_set_defaults_i) <= z)%Q
   | 12 => (s V_jpeg_set_defaults_z + max0(16 - s V_jpeg_set_defaults_i) <= z)%Q
   | 13 => (s V_jpeg_set_defaults_z + max0(16 - s V_jpeg_set_defaults_i) <= z)%Q
   | 14 => (s V_jpeg_set_defaults_z + max0(16 - s V_jpeg_set_defaults_i) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (16 - s V_jpeg_set_defaults_i) (15
                                                                    - s V_jpeg_set_defaults_i));
      (*-1 0*) F_max0_ge_0 (15 - s V_jpeg_set_defaults_i)]
     (s V_jpeg_set_defaults_z + max0(16 - s V_jpeg_set_defaults_i) <= z)%Q
   | 16 => (s V_jpeg_set_defaults_z + max0(16 - s V_jpeg_set_defaults_i) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (16 - s V_jpeg_set_defaults_i) (15
                                                                    - s V_jpeg_set_defaults_i));
      (*-1 0*) F_max0_ge_0 (15 - s V_jpeg_set_defaults_i)]
     (s V_jpeg_set_defaults_z + max0(16 - s V_jpeg_set_defaults_i) <= z)%Q
   | 18 => (s V_jpeg_set_defaults_z <= z)%Q
   | 19 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (16
                                                  - s V_jpeg_set_defaults_i)) (F_check_ge (16
                                                                    - s V_jpeg_set_defaults_i) (0))]
     (s V_jpeg_set_defaults_z + max0(16 - s V_jpeg_set_defaults_i) <= z)%Q
   | 20 => ((16 # 1) - s V_jpeg_set_defaults_i + s V_jpeg_set_defaults_z <= z)%Q
   | 21 => ((16 # 1) - s V_jpeg_set_defaults_i + s V_jpeg_set_defaults_z <= z)%Q
   | 22 => ((17 # 1) - s V_jpeg_set_defaults_i + s V_jpeg_set_defaults_z <= z)%Q
   | 23 => ((17 # 1) - s V_jpeg_set_defaults_i + s V_jpeg_set_defaults_z <= z)%Q
   | 24 => ((17 # 1) - s V_jpeg_set_defaults_i + s V_jpeg_set_defaults_z <= z)%Q
   | 25 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                               - s V_jpeg_set_defaults_i) (0))) (F_max0_ge_0 (16
                                                                    - s V_jpeg_set_defaults_i))]
     ((16 # 1) - s V_jpeg_set_defaults_i + s V_jpeg_set_defaults_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_jpeg_set_defaults =>
    [mkPA Q (fun n z s => ai_jpeg_set_defaults n s /\ annot0_jpeg_set_defaults n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_jpeg_set_defaults (proc_start P_jpeg_set_defaults) s1 (proc_end P_jpeg_set_defaults) s2 ->
    (s2 V_jpeg_set_defaults_z <= (16 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_jpeg_set_defaults.
Qed.
