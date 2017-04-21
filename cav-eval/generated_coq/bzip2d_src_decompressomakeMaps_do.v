Require Import pasta.Pasta.

Inductive proc: Type :=
  P_makeMaps_d.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_makeMaps_d_z := 1%positive.
Notation V_makeMaps_d_i := 2%positive.
Notation V_makeMaps_d_s_dref_off3192 := 3%positive.
Notation V_makeMaps_d_s := 4%positive.
Definition Pedges_makeMaps_d: list (edge proc) :=
  (EA 1 (AAssign V_makeMaps_d_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_makeMaps_d_s_dref_off3192 (Some (ENum (0)))) 3)::(EA 3 (AAssign
  V_makeMaps_d_i (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_makeMaps_d_i) s) <
  (eval (ENum (256)) s))%Z)) 9)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_makeMaps_d_i) s) >= (eval (ENum (256))
  s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::(EA 10 ANone 11)::
  (EA 10 ANone 13)::(EA 11 (AAssign V_makeMaps_d_s_dref_off3192
  (Some (EAdd (EVar V_makeMaps_d_s_dref_off3192) (ENum (1))))) 12)::
  (EA 12 ANone 13)::(EA 13 ANone 14)::(EA 14 (AAssign V_makeMaps_d_i
  (Some (EAdd (EVar V_makeMaps_d_i) (ENum (1))))) 15)::(EA 15 ANone 16)::
  (EA 16 ANone 17)::(EA 17 (AAssign V_makeMaps_d_z (Some (EAdd (ENum (1))
  (EVar V_makeMaps_d_z)))) 18)::(EA 18 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_makeMaps_d => Pedges_makeMaps_d
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_makeMaps_d => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_makeMaps_d (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_makeMaps_d_z <= 0 /\ -1 * s V_makeMaps_d_z <= 0)%Z
   | 3 => (-1 * s V_makeMaps_d_z <= 0 /\ 1 * s V_makeMaps_d_z <= 0 /\ 1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ -1 * s V_makeMaps_d_s_dref_off3192 <= 0)%Z
   | 4 => (-1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ 1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ 1 * s V_makeMaps_d_z <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ 1 * s V_makeMaps_d_i <= 0 /\ -1 * s V_makeMaps_d_i <= 0)%Z
   | 5 => (-1 * s V_makeMaps_d_i <= 0 /\ 1 * s V_makeMaps_d_i <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ 1 * s V_makeMaps_d_z <= 0 /\ 1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ -1 * s V_makeMaps_d_s_dref_off3192 <= 0)%Z
   | 6 => (-1 * s V_makeMaps_d_z <= 0 /\ -1 * s V_makeMaps_d_i <= 0 /\ -1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ 1 * s V_makeMaps_d_i + -256 <= 0)%Z
   | 7 => (1 * s V_makeMaps_d_i + -256 <= 0 /\ -1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ -1 * s V_makeMaps_d_i + 256 <= 0)%Z
   | 8 => (-1 * s V_makeMaps_d_i + 256 <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ -1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ 1 * s V_makeMaps_d_i + -256 <= 0)%Z
   | 9 => (-1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ -1 * s V_makeMaps_d_i <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ 1 * s V_makeMaps_d_i + -255 <= 0)%Z
   | 10 => (1 * s V_makeMaps_d_i + -255 <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ -1 * s V_makeMaps_d_i <= 0 /\ -1 * s V_makeMaps_d_s_dref_off3192 <= 0)%Z
   | 11 => (-1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ -1 * s V_makeMaps_d_i <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ 1 * s V_makeMaps_d_i + -255 <= 0)%Z
   | 12 => (1 * s V_makeMaps_d_i + -255 <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ -1 * s V_makeMaps_d_i <= 0 /\ -1 * s V_makeMaps_d_s_dref_off3192 + 1 <= 0)%Z
   | 13 => (-1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ -1 * s V_makeMaps_d_i <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ 1 * s V_makeMaps_d_i + -255 <= 0)%Z
   | 14 => (1 * s V_makeMaps_d_i + -255 <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ -1 * s V_makeMaps_d_i <= 0 /\ -1 * s V_makeMaps_d_s_dref_off3192 <= 0)%Z
   | 15 => (-1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ 1 * s V_makeMaps_d_i + -256 <= 0 /\ -1 * s V_makeMaps_d_i + 1 <= 0)%Z
   | 16 => (-1 * s V_makeMaps_d_i + 1 <= 0 /\ 1 * s V_makeMaps_d_i + -256 <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ -1 * s V_makeMaps_d_s_dref_off3192 <= 0)%Z
   | 17 => (-1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ -1 * s V_makeMaps_d_z <= 0 /\ 1 * s V_makeMaps_d_i + -256 <= 0 /\ -1 * s V_makeMaps_d_i + 1 <= 0)%Z
   | 18 => (-1 * s V_makeMaps_d_i + 1 <= 0 /\ 1 * s V_makeMaps_d_i + -256 <= 0 /\ -1 * s V_makeMaps_d_s_dref_off3192 <= 0 /\ -1 * s V_makeMaps_d_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_makeMaps_d (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((256 # 1) <= z)%Q
   | 2 => ((256 # 1) + s V_makeMaps_d_z <= z)%Q
   | 3 => ((256 # 1) + s V_makeMaps_d_z <= z)%Q
   | 4 => (s V_makeMaps_d_z + max0(256 - s V_makeMaps_d_i) <= z)%Q
   | 5 => (s V_makeMaps_d_z + max0(256 - s V_makeMaps_d_i) <= z)%Q
   | 6 => (s V_makeMaps_d_z + max0(256 - s V_makeMaps_d_i) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (256 - s V_makeMaps_d_i) (255
                                                                    - s V_makeMaps_d_i));
      (*-1 0*) F_max0_ge_0 (255 - s V_makeMaps_d_i)]
     (s V_makeMaps_d_z + max0(256 - s V_makeMaps_d_i) <= z)%Q
   | 8 => (s V_makeMaps_d_z <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (256 - s V_makeMaps_d_i) (1)]
     (s V_makeMaps_d_z + max0(256 - s V_makeMaps_d_i) <= z)%Q
   | 10 => ((1 # 1) + s V_makeMaps_d_z + max0(255 - s V_makeMaps_d_i) <= z)%Q
   | 11 => ((1 # 1) + s V_makeMaps_d_z + max0(255 - s V_makeMaps_d_i) <= z)%Q
   | 12 => ((1 # 1) + s V_makeMaps_d_z + max0(255 - s V_makeMaps_d_i) <= z)%Q
   | 13 => ((1 # 1) + s V_makeMaps_d_z + max0(255 - s V_makeMaps_d_i) <= z)%Q
   | 14 => ((1 # 1) + s V_makeMaps_d_z + max0(255 - s V_makeMaps_d_i) <= z)%Q
   | 15 => ((1 # 1) + s V_makeMaps_d_z + max0(256 - s V_makeMaps_d_i) <= z)%Q
   | 16 => ((1 # 1) + s V_makeMaps_d_z + max0(256 - s V_makeMaps_d_i) <= z)%Q
   | 17 => ((1 # 1) + s V_makeMaps_d_z + max0(256 - s V_makeMaps_d_i) <= z)%Q
   | 18 => (s V_makeMaps_d_z + max0(256 - s V_makeMaps_d_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_makeMaps_d =>
    [mkPA Q (fun n z s => ai_makeMaps_d n s /\ annot0_makeMaps_d n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_makeMaps_d (proc_start P_makeMaps_d) s1 (proc_end P_makeMaps_d) s2 ->
    (s2 V_makeMaps_d_z <= (256 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_makeMaps_d.
Qed.
