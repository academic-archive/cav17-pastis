Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mp_smul.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mp_smul_z := 1%positive.
Notation V_mp_smul__tmp := 2%positive.
Notation V_mp_smul_carry := 3%positive.
Notation V_mp_smul_i := 4%positive.
Notation V_mp_smul_munit_prec := 5%positive.
Notation V_mp_smul_p := 6%positive.
Notation V_mp_smul_multiplicand := 7%positive.
Notation V_mp_smul_multiplier := 8%positive.
Notation V_mp_smul_prod := 9%positive.
Definition Pedges_mp_smul: list (edge proc) :=
  (EA 1 (AAssign V_mp_smul_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_mp_smul__tmp (Some (EVar V_mp_smul_multiplier))) 3)::(EA 3 (AAssign
  V_mp_smul_carry (Some (ENum (0)))) 4)::(EA 4 (AAssign V_mp_smul_i
  (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_mp_smul_i) s) < (eval (EVar V_mp_smul_munit_prec)
  s))%Z)) 10)::(EA 7 (AGuard (fun s => ((eval (EVar V_mp_smul_i) s) >=
  (eval (EVar V_mp_smul_munit_prec) s))%Z)) 8)::(EA 8 AWeaken 9)::
  (EA 10 AWeaken 11)::(EA 11 (AAssign V_mp_smul_p None) 12)::(EA 12 (AAssign
  V_mp_smul_p None) 13)::(EA 13 (AAssign V_mp_smul_carry None) 14)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_mp_smul_i
  (Some (EAdd (EVar V_mp_smul_i) (ENum (1))))) 16)::(EA 16 ANone 17)::
  (EA 17 ANone 18)::(EA 18 (AAssign V_mp_smul_z (Some (EAdd (ENum (1))
  (EVar V_mp_smul_z)))) 19)::(EA 19 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mp_smul => Pedges_mp_smul
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mp_smul => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_mp_smul (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mp_smul_z <= 0 /\ -1 * s V_mp_smul_z <= 0)%Z
   | 3 => (-1 * s V_mp_smul_z <= 0 /\ 1 * s V_mp_smul_z <= 0)%Z
   | 4 => (1 * s V_mp_smul_z <= 0 /\ -1 * s V_mp_smul_z <= 0 /\ 1 * s V_mp_smul_carry <= 0 /\ -1 * s V_mp_smul_carry <= 0)%Z
   | 5 => (-1 * s V_mp_smul_carry <= 0 /\ 1 * s V_mp_smul_carry <= 0 /\ -1 * s V_mp_smul_z <= 0 /\ 1 * s V_mp_smul_z <= 0 /\ 1 * s V_mp_smul_i <= 0 /\ -1 * s V_mp_smul_i <= 0)%Z
   | 6 => (-1 * s V_mp_smul_i <= 0 /\ 1 * s V_mp_smul_i <= 0 /\ 1 * s V_mp_smul_z <= 0 /\ -1 * s V_mp_smul_z <= 0 /\ 1 * s V_mp_smul_carry <= 0 /\ -1 * s V_mp_smul_carry <= 0)%Z
   | 7 => (-1 * s V_mp_smul_z <= 0 /\ -1 * s V_mp_smul_i <= 0)%Z
   | 8 => (-1 * s V_mp_smul_i <= 0 /\ -1 * s V_mp_smul_z <= 0 /\ -1 * s V_mp_smul_i+ 1 * s V_mp_smul_munit_prec <= 0)%Z
   | 9 => (-1 * s V_mp_smul_i+ 1 * s V_mp_smul_munit_prec <= 0 /\ -1 * s V_mp_smul_z <= 0 /\ -1 * s V_mp_smul_i <= 0)%Z
   | 10 => (-1 * s V_mp_smul_i <= 0 /\ -1 * s V_mp_smul_z <= 0 /\ 1 * s V_mp_smul_i+ -1 * s V_mp_smul_munit_prec + 1 <= 0)%Z
   | 11 => (1 * s V_mp_smul_i+ -1 * s V_mp_smul_munit_prec + 1 <= 0 /\ -1 * s V_mp_smul_z <= 0 /\ -1 * s V_mp_smul_i <= 0)%Z
   | 12 => (-1 * s V_mp_smul_i <= 0 /\ -1 * s V_mp_smul_z <= 0 /\ 1 * s V_mp_smul_i+ -1 * s V_mp_smul_munit_prec + 1 <= 0)%Z
   | 13 => (1 * s V_mp_smul_i+ -1 * s V_mp_smul_munit_prec + 1 <= 0 /\ -1 * s V_mp_smul_z <= 0 /\ -1 * s V_mp_smul_i <= 0)%Z
   | 14 => (-1 * s V_mp_smul_i <= 0 /\ -1 * s V_mp_smul_z <= 0 /\ 1 * s V_mp_smul_i+ -1 * s V_mp_smul_munit_prec + 1 <= 0)%Z
   | 15 => (1 * s V_mp_smul_i+ -1 * s V_mp_smul_munit_prec + 1 <= 0 /\ -1 * s V_mp_smul_z <= 0 /\ -1 * s V_mp_smul_i <= 0)%Z
   | 16 => (-1 * s V_mp_smul_z <= 0 /\ 1 * s V_mp_smul_i+ -1 * s V_mp_smul_munit_prec <= 0 /\ -1 * s V_mp_smul_i + 1 <= 0)%Z
   | 17 => (-1 * s V_mp_smul_i + 1 <= 0 /\ 1 * s V_mp_smul_i+ -1 * s V_mp_smul_munit_prec <= 0 /\ -1 * s V_mp_smul_z <= 0)%Z
   | 18 => (-1 * s V_mp_smul_z <= 0 /\ 1 * s V_mp_smul_i+ -1 * s V_mp_smul_munit_prec <= 0 /\ -1 * s V_mp_smul_i + 1 <= 0)%Z
   | 19 => (-1 * s V_mp_smul_i + 1 <= 0 /\ 1 * s V_mp_smul_i+ -1 * s V_mp_smul_munit_prec <= 0 /\ -1 * s V_mp_smul_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mp_smul (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_mp_smul_munit_prec) <= z)%Q
   | 2 => (s V_mp_smul_z + max0(s V_mp_smul_munit_prec) <= z)%Q
   | 3 => (s V_mp_smul_z + max0(s V_mp_smul_munit_prec) <= z)%Q
   | 4 => (s V_mp_smul_z + max0(s V_mp_smul_munit_prec) <= z)%Q
   | 5 => (s V_mp_smul_z + max0(-s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 6 => (s V_mp_smul_z + max0(-s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 7 => (s V_mp_smul_z + max0(-s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_mp_smul_i
                                             + s V_mp_smul_munit_prec) (-1
                                                                    - s V_mp_smul_i
                                                                    + s V_mp_smul_munit_prec));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_mp_smul_i
                                                 + s V_mp_smul_munit_prec)) (F_check_ge (0) (0))]
     (s V_mp_smul_z + max0(-s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 9 => (s V_mp_smul_z <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_mp_smul_i
                                       + s V_mp_smul_munit_prec) (1)]
     (s V_mp_smul_z + max0(-s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 11 => ((1 # 1) + s V_mp_smul_z
            + max0(-1 - s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 12 => ((1 # 1) + s V_mp_smul_z
            + max0(-1 - s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 13 => ((1 # 1) + s V_mp_smul_z
            + max0(-1 - s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 14 => ((1 # 1) + s V_mp_smul_z
            + max0(-1 - s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 15 => ((1 # 1) + s V_mp_smul_z
            + max0(-1 - s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 16 => ((1 # 1) + s V_mp_smul_z
            + max0(-s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 17 => ((1 # 1) + s V_mp_smul_z
            + max0(-s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 18 => ((1 # 1) + s V_mp_smul_z
            + max0(-s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | 19 => (s V_mp_smul_z + max0(-s V_mp_smul_i + s V_mp_smul_munit_prec) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mp_smul =>
    [mkPA Q (fun n z s => ai_mp_smul n s /\ annot0_mp_smul n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mp_smul (proc_start P_mp_smul) s1 (proc_end P_mp_smul) s2 ->
    (s2 V_mp_smul_z <= max0(s1 V_mp_smul_munit_prec))%Q.
Proof.
  prove_bound ipa admissible_ipa P_mp_smul.
Qed.
