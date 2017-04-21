Require Import pasta.Pasta.

Inductive proc: Type :=
  P_fill_dc_scans.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_fill_dc_scans_z := 1%positive.
Notation V_fill_dc_scans__tmp := 2%positive.
Notation V_fill_dc_scans__tmp1 := 3%positive.
Notation V_fill_dc_scans__tmp2 := 4%positive.
Notation V_fill_dc_scans_ci := 5%positive.
Notation V_fill_dc_scans_Ah := 6%positive.
Notation V_fill_dc_scans_Al := 7%positive.
Notation V_fill_dc_scans_ncomps := 8%positive.
Notation V_fill_dc_scans_scanptr := 9%positive.
Definition Pedges_fill_dc_scans: list (edge proc) :=
  (EA 1 (AAssign V_fill_dc_scans_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_fill_dc_scans__tmp (Some (EVar V_fill_dc_scans_ncomps))) 3)::
  (EA 3 (AAssign V_fill_dc_scans__tmp2 (Some (EVar V_fill_dc_scans_Ah))) 4)::
  (EA 4 (AAssign V_fill_dc_scans__tmp1 (Some (EVar V_fill_dc_scans_Al))) 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_fill_dc_scans__tmp) s) <= (eval (ENum (4))
  s))%Z)) 10)::(EA 6 (AGuard (fun s => ((eval (EVar V_fill_dc_scans__tmp)
  s) > (eval (ENum (4)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 18)::(EA 10 AWeaken 11)::(EA 11 (AAssign V_fill_dc_scans_ci
  (Some (ENum (0)))) 12)::(EA 12 ANone 13)::(EA 13 AWeaken 14)::
  (EA 14 (AGuard (fun s => ((eval (EVar V_fill_dc_scans_ci) s) <
  (eval (EVar V_fill_dc_scans__tmp) s))%Z)) 19)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_fill_dc_scans_ci) s) >=
  (eval (EVar V_fill_dc_scans__tmp) s))%Z)) 15)::(EA 15 AWeaken 16)::
  (EA 16 ANone 17)::(EA 17 AWeaken 18)::(EA 19 AWeaken 20)::
  (EA 20 ANone 21)::(EA 21 (AAssign V_fill_dc_scans_ci
  (Some (EAdd (EVar V_fill_dc_scans_ci) (ENum (1))))) 22)::(EA 22 ANone 23)::
  (EA 23 ANone 24)::(EA 24 (AAssign V_fill_dc_scans_z (Some (EAdd (ENum (1))
  (EVar V_fill_dc_scans_z)))) 25)::(EA 25 AWeaken 14)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_fill_dc_scans => Pedges_fill_dc_scans
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_fill_dc_scans => 18
     end)%positive;
  var_global := var_global
}.

Definition ai_fill_dc_scans (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans_z <= 0)%Z
   | 3 => (-1 * s V_fill_dc_scans_z <= 0 /\ 1 * s V_fill_dc_scans_z <= 0)%Z
   | 4 => (1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans_z <= 0)%Z
   | 5 => (-1 * s V_fill_dc_scans_z <= 0 /\ 1 * s V_fill_dc_scans_z <= 0)%Z
   | 6 => (1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans_z <= 0)%Z
   | 7 => (-1 * s V_fill_dc_scans_z <= 0 /\ 1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans__tmp + 5 <= 0)%Z
   | 8 => (-1 * s V_fill_dc_scans__tmp + 5 <= 0 /\ 1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans_z <= 0)%Z
   | 9 => (-1 * s V_fill_dc_scans_z <= 0 /\ 1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans__tmp + 5 <= 0)%Z
   | 10 => (-1 * s V_fill_dc_scans_z <= 0 /\ 1 * s V_fill_dc_scans_z <= 0 /\ 1 * s V_fill_dc_scans__tmp + -4 <= 0)%Z
   | 11 => (1 * s V_fill_dc_scans__tmp + -4 <= 0 /\ 1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans_z <= 0)%Z
   | 12 => (-1 * s V_fill_dc_scans_z <= 0 /\ 1 * s V_fill_dc_scans_z <= 0 /\ 1 * s V_fill_dc_scans__tmp + -4 <= 0 /\ 1 * s V_fill_dc_scans_ci <= 0 /\ -1 * s V_fill_dc_scans_ci <= 0)%Z
   | 13 => (-1 * s V_fill_dc_scans_ci <= 0 /\ 1 * s V_fill_dc_scans_ci <= 0 /\ 1 * s V_fill_dc_scans__tmp + -4 <= 0 /\ 1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans_z <= 0)%Z
   | 14 => (-1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans_ci <= 0 /\ 1 * s V_fill_dc_scans__tmp + -4 <= 0)%Z
   | 15 => (1 * s V_fill_dc_scans__tmp + -4 <= 0 /\ -1 * s V_fill_dc_scans_ci <= 0 /\ -1 * s V_fill_dc_scans_z <= 0 /\ 1 * s V_fill_dc_scans__tmp+ -1 * s V_fill_dc_scans_ci <= 0)%Z
   | 16 => (1 * s V_fill_dc_scans__tmp+ -1 * s V_fill_dc_scans_ci <= 0 /\ -1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans_ci <= 0 /\ 1 * s V_fill_dc_scans__tmp + -4 <= 0)%Z
   | 17 => (1 * s V_fill_dc_scans__tmp + -4 <= 0 /\ -1 * s V_fill_dc_scans_ci <= 0 /\ -1 * s V_fill_dc_scans_z <= 0 /\ 1 * s V_fill_dc_scans__tmp+ -1 * s V_fill_dc_scans_ci <= 0)%Z
   | 18 => (-1 * s V_fill_dc_scans_z <= 0)%Z
   | 19 => (1 * s V_fill_dc_scans__tmp + -4 <= 0 /\ -1 * s V_fill_dc_scans_ci <= 0 /\ -1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans__tmp+ 1 * s V_fill_dc_scans_ci + 1 <= 0)%Z
   | 20 => (-1 * s V_fill_dc_scans__tmp+ 1 * s V_fill_dc_scans_ci + 1 <= 0 /\ -1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans_ci <= 0 /\ 1 * s V_fill_dc_scans__tmp + -4 <= 0)%Z
   | 21 => (1 * s V_fill_dc_scans__tmp + -4 <= 0 /\ -1 * s V_fill_dc_scans_ci <= 0 /\ -1 * s V_fill_dc_scans_z <= 0 /\ -1 * s V_fill_dc_scans__tmp+ 1 * s V_fill_dc_scans_ci + 1 <= 0)%Z
   | 22 => (-1 * s V_fill_dc_scans_z <= 0 /\ 1 * s V_fill_dc_scans__tmp + -4 <= 0 /\ -1 * s V_fill_dc_scans_ci + 1 <= 0 /\ -1 * s V_fill_dc_scans__tmp+ 1 * s V_fill_dc_scans_ci <= 0)%Z
   | 23 => (-1 * s V_fill_dc_scans__tmp+ 1 * s V_fill_dc_scans_ci <= 0 /\ -1 * s V_fill_dc_scans_ci + 1 <= 0 /\ 1 * s V_fill_dc_scans__tmp + -4 <= 0 /\ -1 * s V_fill_dc_scans_z <= 0)%Z
   | 24 => (-1 * s V_fill_dc_scans_z <= 0 /\ 1 * s V_fill_dc_scans__tmp + -4 <= 0 /\ -1 * s V_fill_dc_scans_ci + 1 <= 0 /\ -1 * s V_fill_dc_scans__tmp+ 1 * s V_fill_dc_scans_ci <= 0)%Z
   | 25 => (-1 * s V_fill_dc_scans__tmp+ 1 * s V_fill_dc_scans_ci <= 0 /\ -1 * s V_fill_dc_scans_ci + 1 <= 0 /\ 1 * s V_fill_dc_scans__tmp + -4 <= 0 /\ -1 * s V_fill_dc_scans_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_fill_dc_scans (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_fill_dc_scans_ncomps) <= z)%Q
   | 2 => (s V_fill_dc_scans_z + max0(s V_fill_dc_scans_ncomps) <= z)%Q
   | 3 => (s V_fill_dc_scans_z + max0(s V_fill_dc_scans__tmp) <= z)%Q
   | 4 => (s V_fill_dc_scans_z + max0(s V_fill_dc_scans__tmp) <= z)%Q
   | 5 => (s V_fill_dc_scans_z + max0(s V_fill_dc_scans__tmp) <= z)%Q
   | 6 => (s V_fill_dc_scans_z + max0(s V_fill_dc_scans__tmp) <= z)%Q
   | 7 => (s V_fill_dc_scans_z + max0(s V_fill_dc_scans__tmp) <= z)%Q
   | 8 => (s V_fill_dc_scans_z + max0(s V_fill_dc_scans__tmp) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_dc_scans__tmp)) (F_check_ge (s V_fill_dc_scans__tmp) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_fill_dc_scans__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_fill_dc_scans__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_fill_dc_scans__tmp))]
     (s V_fill_dc_scans_z + max0(s V_fill_dc_scans__tmp) <= z)%Q
   | 10 => (s V_fill_dc_scans_z + max0(s V_fill_dc_scans__tmp) <= z)%Q
   | 11 => (s V_fill_dc_scans_z + max0(s V_fill_dc_scans__tmp) <= z)%Q
   | 12 => (s V_fill_dc_scans_z
            + max0(s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | 13 => (s V_fill_dc_scans_z
            + max0(s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | 14 => (s V_fill_dc_scans_z
            + max0(s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | 15 => (s V_fill_dc_scans_z
            + max0(s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | 16 => (s V_fill_dc_scans_z
            + max0(s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_fill_dc_scans__tmp
                                             - s V_fill_dc_scans_ci) (-1
                                                                    + s V_fill_dc_scans__tmp
                                                                    - s V_fill_dc_scans_ci));
      (*-1 0*) F_max0_ge_0 (-1 + s V_fill_dc_scans__tmp
                            - s V_fill_dc_scans_ci)]
     (s V_fill_dc_scans_z
      + max0(s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | 18 => (s V_fill_dc_scans_z <= z)%Q
   | 19 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_fill_dc_scans__tmp
                                       - s V_fill_dc_scans_ci) (1)]
     (s V_fill_dc_scans_z
      + max0(s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | 20 => ((1 # 1) + s V_fill_dc_scans_z
            + max0(-1 + s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | 21 => ((1 # 1) + s V_fill_dc_scans_z
            + max0(-1 + s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | 22 => ((1 # 1) + s V_fill_dc_scans_z
            + max0(s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | 23 => ((1 # 1) + s V_fill_dc_scans_z
            + max0(s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | 24 => ((1 # 1) + s V_fill_dc_scans_z
            + max0(s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | 25 => (s V_fill_dc_scans_z
            + max0(s V_fill_dc_scans__tmp - s V_fill_dc_scans_ci) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_fill_dc_scans =>
    [mkPA Q (fun n z s => ai_fill_dc_scans n s /\ annot0_fill_dc_scans n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_fill_dc_scans (proc_start P_fill_dc_scans) s1 (proc_end P_fill_dc_scans) s2 ->
    (s2 V_fill_dc_scans_z <= max0(s1 V_fill_dc_scans_ncomps))%Q.
Proof.
  prove_bound ipa admissible_ipa P_fill_dc_scans.
Qed.
