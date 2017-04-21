Require Import pasta.Pasta.

Inductive proc: Type :=
  P_III_overlap_z.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_III_overlap_z_z := 1%positive.
Notation V_III_overlap_z__tmp := 2%positive.
Notation V_III_overlap_z_i := 3%positive.
Notation V_III_overlap_z_overlap := 4%positive.
Notation V_III_overlap_z_sample := 5%positive.
Notation V_III_overlap_z_sb := 6%positive.
Definition Pedges_III_overlap_z: list (edge proc) :=
  (EA 1 (AAssign V_III_overlap_z_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_III_overlap_z_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_III_overlap_z__tmp
  (Some (EVar V_III_overlap_z_sb))) 5)::(EA 5 (AAssign V_III_overlap_z_i
  (Some (ENum (0)))) 6)::(EA 6 ANone 7)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_III_overlap_z_i) s) < (eval (ENum (18))
  s))%Z)) 11)::(EA 8 (AGuard (fun s => ((eval (EVar V_III_overlap_z_i) s) >=
  (eval (ENum (18)) s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 11 AWeaken 12)::
  (EA 12 ANone 13)::(EA 13 (AAssign V_III_overlap_z_i
  (Some (EAdd (EVar V_III_overlap_z_i) (ENum (1))))) 14)::(EA 14 ANone 15)::
  (EA 15 ANone 16)::(EA 16 (AAssign V_III_overlap_z_z (Some (EAdd (ENum (1))
  (EVar V_III_overlap_z_z)))) 17)::(EA 17 AWeaken 8)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_III_overlap_z => Pedges_III_overlap_z
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_III_overlap_z => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_III_overlap_z (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_III_overlap_z_z <= 0 /\ -1 * s V_III_overlap_z_z <= 0)%Z
   | 3 => (-1 * s V_III_overlap_z_z <= 0 /\ 1 * s V_III_overlap_z_z <= 0 /\ -1 * s V_III_overlap_z_i <= 0)%Z
   | 4 => (-1 * s V_III_overlap_z_i <= 0 /\ 1 * s V_III_overlap_z_z <= 0 /\ -1 * s V_III_overlap_z_z <= 0)%Z
   | 5 => (-1 * s V_III_overlap_z_z <= 0 /\ 1 * s V_III_overlap_z_z <= 0 /\ -1 * s V_III_overlap_z_i <= 0)%Z
   | 6 => (1 * s V_III_overlap_z_z <= 0 /\ -1 * s V_III_overlap_z_z <= 0 /\ 1 * s V_III_overlap_z_i <= 0 /\ -1 * s V_III_overlap_z_i <= 0)%Z
   | 7 => (-1 * s V_III_overlap_z_i <= 0 /\ 1 * s V_III_overlap_z_i <= 0 /\ -1 * s V_III_overlap_z_z <= 0 /\ 1 * s V_III_overlap_z_z <= 0)%Z
   | 8 => (-1 * s V_III_overlap_z_z <= 0 /\ -1 * s V_III_overlap_z_i <= 0 /\ 1 * s V_III_overlap_z_i + -18 <= 0)%Z
   | 9 => (1 * s V_III_overlap_z_i + -18 <= 0 /\ -1 * s V_III_overlap_z_z <= 0 /\ -1 * s V_III_overlap_z_i + 18 <= 0)%Z
   | 10 => (-1 * s V_III_overlap_z_i + 18 <= 0 /\ -1 * s V_III_overlap_z_z <= 0 /\ 1 * s V_III_overlap_z_i + -18 <= 0)%Z
   | 11 => (-1 * s V_III_overlap_z_i <= 0 /\ -1 * s V_III_overlap_z_z <= 0 /\ 1 * s V_III_overlap_z_i + -17 <= 0)%Z
   | 12 => (1 * s V_III_overlap_z_i + -17 <= 0 /\ -1 * s V_III_overlap_z_z <= 0 /\ -1 * s V_III_overlap_z_i <= 0)%Z
   | 13 => (-1 * s V_III_overlap_z_i <= 0 /\ -1 * s V_III_overlap_z_z <= 0 /\ 1 * s V_III_overlap_z_i + -17 <= 0)%Z
   | 14 => (-1 * s V_III_overlap_z_z <= 0 /\ -1 * s V_III_overlap_z_i + 1 <= 0 /\ 1 * s V_III_overlap_z_i + -18 <= 0)%Z
   | 15 => (1 * s V_III_overlap_z_i + -18 <= 0 /\ -1 * s V_III_overlap_z_i + 1 <= 0 /\ -1 * s V_III_overlap_z_z <= 0)%Z
   | 16 => (-1 * s V_III_overlap_z_z <= 0 /\ -1 * s V_III_overlap_z_i + 1 <= 0 /\ 1 * s V_III_overlap_z_i + -18 <= 0)%Z
   | 17 => (1 * s V_III_overlap_z_i + -18 <= 0 /\ -1 * s V_III_overlap_z_i + 1 <= 0 /\ -1 * s V_III_overlap_z_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_III_overlap_z (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((18 # 1) <= z)%Q
   | 2 => ((18 # 1) + s V_III_overlap_z_z <= z)%Q
   | 3 => ((18 # 1) + s V_III_overlap_z_z <= z)%Q
   | 4 => ((18 # 1) + s V_III_overlap_z_z <= z)%Q
   | 5 => ((18 # 1) + s V_III_overlap_z_z <= z)%Q
   | 6 => (s V_III_overlap_z_z + max0(18 - s V_III_overlap_z_i) <= z)%Q
   | 7 => (s V_III_overlap_z_z + max0(18 - s V_III_overlap_z_i) <= z)%Q
   | 8 => (s V_III_overlap_z_z + max0(18 - s V_III_overlap_z_i) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (18 - s V_III_overlap_z_i) (17
                                                                    - s V_III_overlap_z_i));
      (*-1 0*) F_max0_ge_0 (17 - s V_III_overlap_z_i)]
     (s V_III_overlap_z_z + max0(18 - s V_III_overlap_z_i) <= z)%Q
   | 10 => (s V_III_overlap_z_z <= z)%Q
   | 11 => hints
     [(*0 1*) F_max0_pre_decrement 1 (18 - s V_III_overlap_z_i) (1)]
     (s V_III_overlap_z_z + max0(18 - s V_III_overlap_z_i) <= z)%Q
   | 12 => ((1 # 1) + s V_III_overlap_z_z + max0(17 - s V_III_overlap_z_i) <= z)%Q
   | 13 => ((1 # 1) + s V_III_overlap_z_z + max0(17 - s V_III_overlap_z_i) <= z)%Q
   | 14 => ((1 # 1) + s V_III_overlap_z_z + max0(18 - s V_III_overlap_z_i) <= z)%Q
   | 15 => ((1 # 1) + s V_III_overlap_z_z + max0(18 - s V_III_overlap_z_i) <= z)%Q
   | 16 => ((1 # 1) + s V_III_overlap_z_z + max0(18 - s V_III_overlap_z_i) <= z)%Q
   | 17 => (s V_III_overlap_z_z + max0(18 - s V_III_overlap_z_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_III_overlap_z =>
    [mkPA Q (fun n z s => ai_III_overlap_z n s /\ annot0_III_overlap_z n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_III_overlap_z (proc_start P_III_overlap_z) s1 (proc_end P_III_overlap_z) s2 ->
    (s2 V_III_overlap_z_z <= (18 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_III_overlap_z.
Qed.
