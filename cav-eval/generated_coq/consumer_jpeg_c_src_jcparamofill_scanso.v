Require Import pasta.Pasta.

Inductive proc: Type :=
  P_fill_scans.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_fill_scans_z := 1%positive.
Notation V_fill_scans__tmp := 2%positive.
Notation V_fill_scans__tmp1 := 3%positive.
Notation V_fill_scans__tmp2 := 4%positive.
Notation V_fill_scans__tmp3 := 5%positive.
Notation V_fill_scans__tmp4 := 6%positive.
Notation V_fill_scans_ci := 7%positive.
Notation V_fill_scans_Ah := 8%positive.
Notation V_fill_scans_Al := 9%positive.
Notation V_fill_scans_Se := 10%positive.
Notation V_fill_scans_Ss := 11%positive.
Notation V_fill_scans_ncomps := 12%positive.
Notation V_fill_scans_scanptr := 13%positive.
Definition Pedges_fill_scans: list (edge proc) :=
  (EA 1 (AAssign V_fill_scans_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_fill_scans__tmp (Some (EVar V_fill_scans_ncomps))) 3)::(EA 3 (AAssign
  V_fill_scans__tmp4 (Some (EVar V_fill_scans_Ss))) 4)::(EA 4 (AAssign
  V_fill_scans__tmp3 (Some (EVar V_fill_scans_Se))) 5)::(EA 5 (AAssign
  V_fill_scans__tmp2 (Some (EVar V_fill_scans_Ah))) 6)::(EA 6 (AAssign
  V_fill_scans__tmp1 (Some (EVar V_fill_scans_Al))) 7)::(EA 7 (AAssign
  V_fill_scans_ci (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_fill_scans_ci) s) <
  (eval (EVar V_fill_scans__tmp) s))%Z)) 13)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_fill_scans_ci) s) >=
  (eval (EVar V_fill_scans__tmp) s))%Z)) 11)::(EA 11 AWeaken 12)::
  (EA 13 AWeaken 14)::(EA 14 ANone 15)::(EA 15 (AAssign V_fill_scans_ci
  (Some (EAdd (EVar V_fill_scans_ci) (ENum (1))))) 16)::(EA 16 ANone 17)::
  (EA 17 ANone 18)::(EA 18 (AAssign V_fill_scans_z (Some (EAdd (ENum (1))
  (EVar V_fill_scans_z)))) 19)::(EA 19 AWeaken 10)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_fill_scans => Pedges_fill_scans
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_fill_scans => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_fill_scans (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_fill_scans_z <= 0 /\ -1 * s V_fill_scans_z <= 0)%Z
   | 3 => (-1 * s V_fill_scans_z <= 0 /\ 1 * s V_fill_scans_z <= 0)%Z
   | 4 => (1 * s V_fill_scans_z <= 0 /\ -1 * s V_fill_scans_z <= 0)%Z
   | 5 => (-1 * s V_fill_scans_z <= 0 /\ 1 * s V_fill_scans_z <= 0)%Z
   | 6 => (1 * s V_fill_scans_z <= 0 /\ -1 * s V_fill_scans_z <= 0)%Z
   | 7 => (-1 * s V_fill_scans_z <= 0 /\ 1 * s V_fill_scans_z <= 0)%Z
   | 8 => (1 * s V_fill_scans_z <= 0 /\ -1 * s V_fill_scans_z <= 0 /\ 1 * s V_fill_scans_ci <= 0 /\ -1 * s V_fill_scans_ci <= 0)%Z
   | 9 => (-1 * s V_fill_scans_ci <= 0 /\ 1 * s V_fill_scans_ci <= 0 /\ -1 * s V_fill_scans_z <= 0 /\ 1 * s V_fill_scans_z <= 0)%Z
   | 10 => (-1 * s V_fill_scans_z <= 0 /\ -1 * s V_fill_scans_ci <= 0)%Z
   | 11 => (-1 * s V_fill_scans_ci <= 0 /\ -1 * s V_fill_scans_z <= 0 /\ 1 * s V_fill_scans__tmp+ -1 * s V_fill_scans_ci <= 0)%Z
   | 12 => (1 * s V_fill_scans__tmp+ -1 * s V_fill_scans_ci <= 0 /\ -1 * s V_fill_scans_z <= 0 /\ -1 * s V_fill_scans_ci <= 0)%Z
   | 13 => (-1 * s V_fill_scans_ci <= 0 /\ -1 * s V_fill_scans_z <= 0 /\ -1 * s V_fill_scans__tmp+ 1 * s V_fill_scans_ci + 1 <= 0)%Z
   | 14 => (-1 * s V_fill_scans__tmp+ 1 * s V_fill_scans_ci + 1 <= 0 /\ -1 * s V_fill_scans_z <= 0 /\ -1 * s V_fill_scans_ci <= 0)%Z
   | 15 => (-1 * s V_fill_scans_ci <= 0 /\ -1 * s V_fill_scans_z <= 0 /\ -1 * s V_fill_scans__tmp+ 1 * s V_fill_scans_ci + 1 <= 0)%Z
   | 16 => (-1 * s V_fill_scans_z <= 0 /\ -1 * s V_fill_scans_ci + 1 <= 0 /\ -1 * s V_fill_scans__tmp+ 1 * s V_fill_scans_ci <= 0)%Z
   | 17 => (-1 * s V_fill_scans__tmp+ 1 * s V_fill_scans_ci <= 0 /\ -1 * s V_fill_scans_ci + 1 <= 0 /\ -1 * s V_fill_scans_z <= 0)%Z
   | 18 => (-1 * s V_fill_scans_z <= 0 /\ -1 * s V_fill_scans_ci + 1 <= 0 /\ -1 * s V_fill_scans__tmp+ 1 * s V_fill_scans_ci <= 0)%Z
   | 19 => (-1 * s V_fill_scans__tmp+ 1 * s V_fill_scans_ci <= 0 /\ -1 * s V_fill_scans_ci + 1 <= 0 /\ -1 * s V_fill_scans_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_fill_scans (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_fill_scans_ncomps) <= z)%Q
   | 2 => (s V_fill_scans_z + max0(s V_fill_scans_ncomps) <= z)%Q
   | 3 => (s V_fill_scans_z + max0(s V_fill_scans__tmp) <= z)%Q
   | 4 => (s V_fill_scans_z + max0(s V_fill_scans__tmp) <= z)%Q
   | 5 => (s V_fill_scans_z + max0(s V_fill_scans__tmp) <= z)%Q
   | 6 => (s V_fill_scans_z + max0(s V_fill_scans__tmp) <= z)%Q
   | 7 => (s V_fill_scans_z + max0(s V_fill_scans__tmp) <= z)%Q
   | 8 => (s V_fill_scans_z + max0(s V_fill_scans__tmp - s V_fill_scans_ci) <= z)%Q
   | 9 => (s V_fill_scans_z + max0(s V_fill_scans__tmp - s V_fill_scans_ci) <= z)%Q
   | 10 => (s V_fill_scans_z + max0(s V_fill_scans__tmp - s V_fill_scans_ci) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_fill_scans__tmp
                                             - s V_fill_scans_ci) (-1
                                                                   + 
                                                                   s V_fill_scans__tmp
                                                                   - 
                                                                   s V_fill_scans_ci));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_fill_scans__tmp
                                                 - s V_fill_scans_ci)) (F_check_ge (0) (0))]
     (s V_fill_scans_z + max0(s V_fill_scans__tmp - s V_fill_scans_ci) <= z)%Q
   | 12 => (s V_fill_scans_z <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_fill_scans__tmp
                                       - s V_fill_scans_ci) (1)]
     (s V_fill_scans_z + max0(s V_fill_scans__tmp - s V_fill_scans_ci) <= z)%Q
   | 14 => ((1 # 1) + s V_fill_scans_z
            + max0(-1 + s V_fill_scans__tmp - s V_fill_scans_ci) <= z)%Q
   | 15 => ((1 # 1) + s V_fill_scans_z
            + max0(-1 + s V_fill_scans__tmp - s V_fill_scans_ci) <= z)%Q
   | 16 => ((1 # 1) + s V_fill_scans_z
            + max0(s V_fill_scans__tmp - s V_fill_scans_ci) <= z)%Q
   | 17 => ((1 # 1) + s V_fill_scans_z
            + max0(s V_fill_scans__tmp - s V_fill_scans_ci) <= z)%Q
   | 18 => ((1 # 1) + s V_fill_scans_z
            + max0(s V_fill_scans__tmp - s V_fill_scans_ci) <= z)%Q
   | 19 => (s V_fill_scans_z + max0(s V_fill_scans__tmp - s V_fill_scans_ci) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_fill_scans =>
    [mkPA Q (fun n z s => ai_fill_scans n s /\ annot0_fill_scans n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_fill_scans (proc_start P_fill_scans) s1 (proc_end P_fill_scans) s2 ->
    (s2 V_fill_scans_z <= max0(s1 V_fill_scans_ncomps))%Q.
Proof.
  prove_bound ipa admissible_ipa P_fill_scans.
Qed.
