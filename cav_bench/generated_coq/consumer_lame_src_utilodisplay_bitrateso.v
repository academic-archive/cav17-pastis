Require Import pasta.Pasta.

Inductive proc: Type :=
  P_display_bitrates.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_display_bitrates_z := 1%positive.
Notation V_display_bitrates_index := 2%positive.
Notation V_display_bitrates_version := 3%positive.
Notation V_display_bitrates_out_fh := 4%positive.
Definition Pedges_display_bitrates: list (edge proc) :=
  (EA 1 (AAssign V_display_bitrates_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_display_bitrates_version (Some (ENum (1)))) 3)::(EA 3 (AAssign
  V_display_bitrates_index (Some (ENum (1)))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_display_bitrates_index) s) < (eval (ENum (15))
  s))%Z)) 22)::(EA 6 (AGuard (fun s => ((eval (EVar V_display_bitrates_index)
  s) >= (eval (ENum (15)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_display_bitrates_version (Some (ENum (0)))) 9)::(EA 9 (AAssign
  V_display_bitrates_index (Some (ENum (1)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_display_bitrates_index) s) < (eval (ENum (15))
  s))%Z)) 15)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_display_bitrates_index) s) >= (eval (ENum (15))
  s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 15 AWeaken 16)::(EA 16 ANone 17)::
  (EA 17 (AAssign V_display_bitrates_index
  (Some (EAdd (EVar V_display_bitrates_index) (ENum (1))))) 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign V_display_bitrates_z
  (Some (EAdd (ENum (1)) (EVar V_display_bitrates_z)))) 21)::
  (EA 21 AWeaken 12)::(EA 22 AWeaken 23)::(EA 23 ANone 24)::(EA 24 (AAssign
  V_display_bitrates_index (Some (EAdd (EVar V_display_bitrates_index)
  (ENum (1))))) 25)::(EA 25 ANone 26)::(EA 26 ANone 27)::(EA 27 (AAssign
  V_display_bitrates_z (Some (EAdd (ENum (1))
  (EVar V_display_bitrates_z)))) 28)::(EA 28 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_display_bitrates => Pedges_display_bitrates
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_display_bitrates => 14
     end)%positive;
  var_global := var_global
}.

Definition ai_display_bitrates (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_z <= 0)%Z
   | 3 => (-1 * s V_display_bitrates_z <= 0 /\ 1 * s V_display_bitrates_z <= 0 /\ 1 * s V_display_bitrates_version + -1 <= 0 /\ -1 * s V_display_bitrates_version + 1 <= 0)%Z
   | 4 => (-1 * s V_display_bitrates_version + 1 <= 0 /\ 1 * s V_display_bitrates_version + -1 <= 0 /\ 1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ 1 * s V_display_bitrates_index + -1 <= 0 /\ -1 * s V_display_bitrates_index + 1 <= 0)%Z
   | 5 => (-1 * s V_display_bitrates_index + 1 <= 0 /\ 1 * s V_display_bitrates_index + -1 <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ 1 * s V_display_bitrates_z <= 0 /\ 1 * s V_display_bitrates_version + -1 <= 0 /\ -1 * s V_display_bitrates_version + 1 <= 0)%Z
   | 6 => (-1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_index + 1 <= 0 /\ -1 * s V_display_bitrates_version + 1 <= 0 /\ 1 * s V_display_bitrates_version + -1 <= 0 /\ 1 * s V_display_bitrates_index + -15 <= 0)%Z
   | 7 => (1 * s V_display_bitrates_index + -15 <= 0 /\ 1 * s V_display_bitrates_version + -1 <= 0 /\ -1 * s V_display_bitrates_version + 1 <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_index + 15 <= 0)%Z
   | 8 => (-1 * s V_display_bitrates_index + 15 <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_version + 1 <= 0 /\ 1 * s V_display_bitrates_version + -1 <= 0 /\ 1 * s V_display_bitrates_index + -15 <= 0)%Z
   | 9 => (1 * s V_display_bitrates_index + -15 <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_index + 15 <= 0 /\ 1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_version <= 0)%Z
   | 10 => (-1 * s V_display_bitrates_version <= 0 /\ 1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ 1 * s V_display_bitrates_index + -1 <= 0 /\ -1 * s V_display_bitrates_index + 1 <= 0)%Z
   | 11 => (-1 * s V_display_bitrates_index + 1 <= 0 /\ 1 * s V_display_bitrates_index + -1 <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ 1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_version <= 0)%Z
   | 12 => (-1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_index + 1 <= 0 /\ -1 * s V_display_bitrates_version <= 0 /\ 1 * s V_display_bitrates_version <= 0 /\ 1 * s V_display_bitrates_index + -15 <= 0)%Z
   | 13 => (1 * s V_display_bitrates_index + -15 <= 0 /\ 1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_index + 15 <= 0)%Z
   | 14 => (-1 * s V_display_bitrates_index + 15 <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_version <= 0 /\ 1 * s V_display_bitrates_version <= 0 /\ 1 * s V_display_bitrates_index + -15 <= 0)%Z
   | 15 => (1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_index + 1 <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ 1 * s V_display_bitrates_index + -14 <= 0)%Z
   | 16 => (1 * s V_display_bitrates_index + -14 <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_index + 1 <= 0 /\ -1 * s V_display_bitrates_version <= 0 /\ 1 * s V_display_bitrates_version <= 0)%Z
   | 17 => (1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_index + 1 <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ 1 * s V_display_bitrates_index + -14 <= 0)%Z
   | 18 => (-1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_version <= 0 /\ 1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_index + 2 <= 0 /\ 1 * s V_display_bitrates_index + -15 <= 0)%Z
   | 19 => (1 * s V_display_bitrates_index + -15 <= 0 /\ -1 * s V_display_bitrates_index + 2 <= 0 /\ 1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_z <= 0)%Z
   | 20 => (-1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_version <= 0 /\ 1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_index + 2 <= 0 /\ 1 * s V_display_bitrates_index + -15 <= 0)%Z
   | 21 => (1 * s V_display_bitrates_index + -15 <= 0 /\ -1 * s V_display_bitrates_index + 2 <= 0 /\ 1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_version <= 0 /\ -1 * s V_display_bitrates_z + 1 <= 0)%Z
   | 22 => (1 * s V_display_bitrates_version + -1 <= 0 /\ -1 * s V_display_bitrates_version + 1 <= 0 /\ -1 * s V_display_bitrates_index + 1 <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ 1 * s V_display_bitrates_index + -14 <= 0)%Z
   | 23 => (1 * s V_display_bitrates_index + -14 <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_index + 1 <= 0 /\ -1 * s V_display_bitrates_version + 1 <= 0 /\ 1 * s V_display_bitrates_version + -1 <= 0)%Z
   | 24 => (1 * s V_display_bitrates_version + -1 <= 0 /\ -1 * s V_display_bitrates_version + 1 <= 0 /\ -1 * s V_display_bitrates_index + 1 <= 0 /\ -1 * s V_display_bitrates_z <= 0 /\ 1 * s V_display_bitrates_index + -14 <= 0)%Z
   | 25 => (-1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_version + 1 <= 0 /\ 1 * s V_display_bitrates_version + -1 <= 0 /\ -1 * s V_display_bitrates_index + 2 <= 0 /\ 1 * s V_display_bitrates_index + -15 <= 0)%Z
   | 26 => (1 * s V_display_bitrates_index + -15 <= 0 /\ -1 * s V_display_bitrates_index + 2 <= 0 /\ 1 * s V_display_bitrates_version + -1 <= 0 /\ -1 * s V_display_bitrates_version + 1 <= 0 /\ -1 * s V_display_bitrates_z <= 0)%Z
   | 27 => (-1 * s V_display_bitrates_z <= 0 /\ -1 * s V_display_bitrates_version + 1 <= 0 /\ 1 * s V_display_bitrates_version + -1 <= 0 /\ -1 * s V_display_bitrates_index + 2 <= 0 /\ 1 * s V_display_bitrates_index + -15 <= 0)%Z
   | 28 => (1 * s V_display_bitrates_index + -15 <= 0 /\ -1 * s V_display_bitrates_index + 2 <= 0 /\ 1 * s V_display_bitrates_version + -1 <= 0 /\ -1 * s V_display_bitrates_version + 1 <= 0 /\ -1 * s V_display_bitrates_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_display_bitrates (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((28 # 1) <= z)%Q
   | 2 => ((28 # 1) + s V_display_bitrates_z <= z)%Q
   | 3 => ((28 # 1) + s V_display_bitrates_z <= z)%Q
   | 4 => ((14 # 1) + s V_display_bitrates_z
           + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 5 => ((14 # 1) + s V_display_bitrates_z
           + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 6 => ((14 # 1) + s V_display_bitrates_z
           + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (15 - s V_display_bitrates_index) (14
                                                                    - s V_display_bitrates_index));
      (*-1 0*) F_max0_ge_0 (14 - s V_display_bitrates_index)]
     ((14 # 1) + s V_display_bitrates_z
      + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 8 => ((14 # 1) + s V_display_bitrates_z <= z)%Q
   | 9 => ((14 # 1) + s V_display_bitrates_z <= z)%Q
   | 10 => (s V_display_bitrates_z + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 11 => (s V_display_bitrates_z + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 12 => (s V_display_bitrates_z + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (15 - s V_display_bitrates_index) (14
                                                                    - s V_display_bitrates_index));
      (*-1 0*) F_max0_ge_0 (14 - s V_display_bitrates_index)]
     (s V_display_bitrates_z + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 14 => (s V_display_bitrates_z <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (15 - s V_display_bitrates_index) (1)]
     (s V_display_bitrates_z + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 16 => ((1 # 1) + s V_display_bitrates_z
            + max0(14 - s V_display_bitrates_index) <= z)%Q
   | 17 => ((1 # 1) + s V_display_bitrates_z
            + max0(14 - s V_display_bitrates_index) <= z)%Q
   | 18 => ((1 # 1) + s V_display_bitrates_z
            + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 19 => ((1 # 1) + s V_display_bitrates_z
            + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 20 => ((1 # 1) + s V_display_bitrates_z
            + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 21 => (s V_display_bitrates_z + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (15 - s V_display_bitrates_index) (1)]
     ((14 # 1) + s V_display_bitrates_z
      + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 23 => ((15 # 1) + s V_display_bitrates_z
            + max0(14 - s V_display_bitrates_index) <= z)%Q
   | 24 => ((15 # 1) + s V_display_bitrates_z
            + max0(14 - s V_display_bitrates_index) <= z)%Q
   | 25 => ((15 # 1) + s V_display_bitrates_z
            + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 26 => ((15 # 1) + s V_display_bitrates_z
            + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 27 => ((15 # 1) + s V_display_bitrates_z
            + max0(15 - s V_display_bitrates_index) <= z)%Q
   | 28 => ((14 # 1) + s V_display_bitrates_z
            + max0(15 - s V_display_bitrates_index) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_display_bitrates =>
    [mkPA Q (fun n z s => ai_display_bitrates n s /\ annot0_display_bitrates n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_display_bitrates (proc_start P_display_bitrates) s1 (proc_end P_display_bitrates) s2 ->
    (s2 V_display_bitrates_z <= (28 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_display_bitrates.
Qed.
