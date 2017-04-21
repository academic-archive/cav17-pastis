Require Import pasta.Pasta.

Inductive proc: Type :=
  P_PascalToC.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_PascalToC_z := 1%positive.
Notation V_PascalToC_i := 2%positive.
Notation V_PascalToC_j := 3%positive.
Notation V_PascalToC_s_dref_off0 := 4%positive.
Notation V_PascalToC_s := 5%positive.
Definition Pedges_PascalToC: list (edge proc) :=
  (EA 1 (AAssign V_PascalToC_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_PascalToC_i (Some (ENum (0)))) 3)::(EA 3 (AAssign V_PascalToC_j
  (Some (EVar V_PascalToC_s_dref_off0))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard (fun s => ((eval (EVar V_PascalToC_i) s) <
  (eval (EVar V_PascalToC_j) s))%Z)) 9)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_PascalToC_i) s) >= (eval (EVar V_PascalToC_j)
  s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::(EA 10 ANone 11)::
  (EA 11 (AAssign V_PascalToC_i (Some (EAdd (EVar V_PascalToC_i)
  (ENum (1))))) 12)::(EA 12 ANone 13)::(EA 13 ANone 14)::(EA 14 (AAssign
  V_PascalToC_z (Some (EAdd (ENum (1)) (EVar V_PascalToC_z)))) 15)::
  (EA 15 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_PascalToC => Pedges_PascalToC
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_PascalToC => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_PascalToC (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_PascalToC_z <= 0 /\ -1 * s V_PascalToC_z <= 0)%Z
   | 3 => (-1 * s V_PascalToC_z <= 0 /\ 1 * s V_PascalToC_z <= 0 /\ 1 * s V_PascalToC_i <= 0 /\ -1 * s V_PascalToC_i <= 0)%Z
   | 4 => (-1 * s V_PascalToC_i <= 0 /\ 1 * s V_PascalToC_i <= 0 /\ 1 * s V_PascalToC_z <= 0 /\ -1 * s V_PascalToC_z <= 0)%Z
   | 5 => (-1 * s V_PascalToC_z <= 0 /\ 1 * s V_PascalToC_z <= 0 /\ 1 * s V_PascalToC_i <= 0 /\ -1 * s V_PascalToC_i <= 0)%Z
   | 6 => (-1 * s V_PascalToC_i <= 0 /\ -1 * s V_PascalToC_z <= 0)%Z
   | 7 => (-1 * s V_PascalToC_z <= 0 /\ -1 * s V_PascalToC_i <= 0 /\ -1 * s V_PascalToC_i+ 1 * s V_PascalToC_j <= 0)%Z
   | 8 => (-1 * s V_PascalToC_i+ 1 * s V_PascalToC_j <= 0 /\ -1 * s V_PascalToC_i <= 0 /\ -1 * s V_PascalToC_z <= 0)%Z
   | 9 => (-1 * s V_PascalToC_z <= 0 /\ -1 * s V_PascalToC_i <= 0 /\ 1 * s V_PascalToC_i+ -1 * s V_PascalToC_j + 1 <= 0)%Z
   | 10 => (1 * s V_PascalToC_i+ -1 * s V_PascalToC_j + 1 <= 0 /\ -1 * s V_PascalToC_i <= 0 /\ -1 * s V_PascalToC_z <= 0)%Z
   | 11 => (-1 * s V_PascalToC_z <= 0 /\ -1 * s V_PascalToC_i <= 0 /\ 1 * s V_PascalToC_i+ -1 * s V_PascalToC_j + 1 <= 0)%Z
   | 12 => (-1 * s V_PascalToC_z <= 0 /\ -1 * s V_PascalToC_i + 1 <= 0 /\ 1 * s V_PascalToC_i+ -1 * s V_PascalToC_j <= 0)%Z
   | 13 => (1 * s V_PascalToC_i+ -1 * s V_PascalToC_j <= 0 /\ -1 * s V_PascalToC_i + 1 <= 0 /\ -1 * s V_PascalToC_z <= 0)%Z
   | 14 => (-1 * s V_PascalToC_z <= 0 /\ -1 * s V_PascalToC_i + 1 <= 0 /\ 1 * s V_PascalToC_i+ -1 * s V_PascalToC_j <= 0)%Z
   | 15 => (1 * s V_PascalToC_i+ -1 * s V_PascalToC_j <= 0 /\ -1 * s V_PascalToC_i + 1 <= 0 /\ -1 * s V_PascalToC_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_PascalToC (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_PascalToC_s_dref_off0) <= z)%Q
   | 2 => (s V_PascalToC_z + max0(s V_PascalToC_s_dref_off0) <= z)%Q
   | 3 => (s V_PascalToC_z
           + max0(-s V_PascalToC_i + s V_PascalToC_s_dref_off0) <= z)%Q
   | 4 => (s V_PascalToC_z + max0(-s V_PascalToC_i + s V_PascalToC_j) <= z)%Q
   | 5 => (s V_PascalToC_z + max0(-s V_PascalToC_i + s V_PascalToC_j) <= z)%Q
   | 6 => (s V_PascalToC_z + max0(-s V_PascalToC_i + s V_PascalToC_j) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_PascalToC_i
                                             + s V_PascalToC_j) (-1
                                                                 - s V_PascalToC_i
                                                                 + s V_PascalToC_j));
      (*-1 0*) F_max0_ge_0 (-1 - s V_PascalToC_i + s V_PascalToC_j)]
     (s V_PascalToC_z + max0(-s V_PascalToC_i + s V_PascalToC_j) <= z)%Q
   | 8 => (s V_PascalToC_z <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_PascalToC_i + s V_PascalToC_j) (1)]
     (s V_PascalToC_z + max0(-s V_PascalToC_i + s V_PascalToC_j) <= z)%Q
   | 10 => ((1 # 1) + s V_PascalToC_z
            + max0(-1 - s V_PascalToC_i + s V_PascalToC_j) <= z)%Q
   | 11 => ((1 # 1) + s V_PascalToC_z
            + max0(-1 - s V_PascalToC_i + s V_PascalToC_j) <= z)%Q
   | 12 => ((1 # 1) + s V_PascalToC_z
            + max0(-s V_PascalToC_i + s V_PascalToC_j) <= z)%Q
   | 13 => ((1 # 1) + s V_PascalToC_z
            + max0(-s V_PascalToC_i + s V_PascalToC_j) <= z)%Q
   | 14 => ((1 # 1) + s V_PascalToC_z
            + max0(-s V_PascalToC_i + s V_PascalToC_j) <= z)%Q
   | 15 => (s V_PascalToC_z + max0(-s V_PascalToC_i + s V_PascalToC_j) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_PascalToC =>
    [mkPA Q (fun n z s => ai_PascalToC n s /\ annot0_PascalToC n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_PascalToC (proc_start P_PascalToC) s1 (proc_end P_PascalToC) s2 ->
    (s2 V_PascalToC_z <= max0(s1 V_PascalToC_s_dref_off0))%Q.
Proof.
  prove_bound ipa admissible_ipa P_PascalToC.
Qed.
