Require Import pasta.Pasta.

Inductive proc: Type :=
  P_encode_mcu_DC_refine.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_encode_mcu_DC_refine_z := 1%positive.
Notation V_encode_mcu_DC_refine_Al := 2%positive.
Notation V_encode_mcu_DC_refine_blkn := 3%positive.
Notation V_encode_mcu_DC_refine_cinfo_dref_off272 := 4%positive.
Notation V_encode_mcu_DC_refine_cinfo_dref_off360 := 5%positive.
Notation V_encode_mcu_DC_refine_cinfo_dref_off416 := 6%positive.
Notation V_encode_mcu_DC_refine_temp := 7%positive.
Notation V_encode_mcu_DC_refine_MCU_data := 8%positive.
Notation V_encode_mcu_DC_refine_cinfo := 9%positive.
Definition Pedges_encode_mcu_DC_refine: list (edge proc) :=
  (EA 1 (AAssign V_encode_mcu_DC_refine_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_encode_mcu_DC_refine_Al
  (Some (EVar V_encode_mcu_DC_refine_cinfo_dref_off416))) 3)::
  (EA 3 AWeaken 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_DC_refine_cinfo_dref_off272) s) <>
  (eval (ENum (0)) s))%Z)) 6)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_DC_refine_cinfo_dref_off272) s) =
  (eval (ENum (0)) s))%Z)) 5)::(EA 5 AWeaken 10)::(EA 6 AWeaken 7)::
  (EA 7 ANone 8)::(EA 7 ANone 9)::(EA 8 ANone 9)::(EA 9 ANone 10)::
  (EA 10 (AAssign V_encode_mcu_DC_refine_blkn (Some (ENum (0)))) 11)::
  (EA 11 ANone 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_DC_refine_blkn) s) <
  (eval (EVar V_encode_mcu_DC_refine_cinfo_dref_off360) s))%Z)) 23)::
  (EA 13 (AGuard (fun s => ((eval (EVar V_encode_mcu_DC_refine_blkn) s) >=
  (eval (EVar V_encode_mcu_DC_refine_cinfo_dref_off360) s))%Z)) 14)::
  (EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_DC_refine_cinfo_dref_off272) s) <>
  (eval (ENum (0)) s))%Z)) 17)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_encode_mcu_DC_refine_cinfo_dref_off272) s) =
  (eval (ENum (0)) s))%Z)) 16)::(EA 16 AWeaken 22)::(EA 17 AWeaken 18)::
  (EA 18 ANone 19)::(EA 18 ANone 20)::(EA 19 ANone 20)::(EA 20 ANone 21)::
  (EA 21 AWeaken 22)::(EA 23 AWeaken 24)::(EA 24 (AAssign
  V_encode_mcu_DC_refine_temp None) 25)::(EA 25 ANone 26)::(EA 26 (AAssign
  V_encode_mcu_DC_refine_blkn (Some (EAdd (EVar V_encode_mcu_DC_refine_blkn)
  (ENum (1))))) 27)::(EA 27 ANone 28)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_encode_mcu_DC_refine_z (Some (EAdd (ENum (1))
  (EVar V_encode_mcu_DC_refine_z)))) 30)::(EA 30 AWeaken 13)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_encode_mcu_DC_refine => Pedges_encode_mcu_DC_refine
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_encode_mcu_DC_refine => 22
     end)%positive;
  var_global := var_global
}.

Definition ai_encode_mcu_DC_refine (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0)%Z
   | 3 => (-1 * s V_encode_mcu_DC_refine_z <= 0 /\ 1 * s V_encode_mcu_DC_refine_z <= 0)%Z
   | 4 => (1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0)%Z
   | 5 => (-1 * s V_encode_mcu_DC_refine_z <= 0 /\ 1 * s V_encode_mcu_DC_refine_z <= 0 /\ 1 * s V_encode_mcu_DC_refine_cinfo_dref_off272 <= 0 /\ -1 * s V_encode_mcu_DC_refine_cinfo_dref_off272 <= 0)%Z
   | 6 => (-1 * s V_encode_mcu_DC_refine_z <= 0 /\ 1 * s V_encode_mcu_DC_refine_z <= 0)%Z
   | 7 => (1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0)%Z
   | 8 => (-1 * s V_encode_mcu_DC_refine_z <= 0 /\ 1 * s V_encode_mcu_DC_refine_z <= 0)%Z
   | 9 => (1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0)%Z
   | 10 => (-1 * s V_encode_mcu_DC_refine_z <= 0 /\ 1 * s V_encode_mcu_DC_refine_z <= 0)%Z
   | 11 => (1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ 1 * s V_encode_mcu_DC_refine_blkn <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn <= 0)%Z
   | 12 => (-1 * s V_encode_mcu_DC_refine_blkn <= 0 /\ 1 * s V_encode_mcu_DC_refine_blkn <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ 1 * s V_encode_mcu_DC_refine_z <= 0)%Z
   | 13 => (-1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn <= 0)%Z
   | 14 => (-1 * s V_encode_mcu_DC_refine_blkn <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn+ 1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0)%Z
   | 15 => (-1 * s V_encode_mcu_DC_refine_blkn+ 1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn <= 0)%Z
   | 16 => (-1 * s V_encode_mcu_DC_refine_blkn <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn+ 1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0 /\ 1 * s V_encode_mcu_DC_refine_cinfo_dref_off272 <= 0 /\ -1 * s V_encode_mcu_DC_refine_cinfo_dref_off272 <= 0)%Z
   | 17 => (-1 * s V_encode_mcu_DC_refine_blkn <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn+ 1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0)%Z
   | 18 => (-1 * s V_encode_mcu_DC_refine_blkn+ 1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn <= 0)%Z
   | 19 => (-1 * s V_encode_mcu_DC_refine_blkn <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn+ 1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0)%Z
   | 20 => (-1 * s V_encode_mcu_DC_refine_blkn+ 1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn <= 0)%Z
   | 21 => (-1 * s V_encode_mcu_DC_refine_blkn <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn+ 1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0)%Z
   | 22 => (-1 * s V_encode_mcu_DC_refine_blkn+ 1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn <= 0)%Z
   | 23 => (-1 * s V_encode_mcu_DC_refine_blkn <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ 1 * s V_encode_mcu_DC_refine_blkn+ -1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 + 1 <= 0)%Z
   | 24 => (1 * s V_encode_mcu_DC_refine_blkn+ -1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 + 1 <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn <= 0)%Z
   | 25 => (-1 * s V_encode_mcu_DC_refine_blkn <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ 1 * s V_encode_mcu_DC_refine_blkn+ -1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 + 1 <= 0)%Z
   | 26 => (1 * s V_encode_mcu_DC_refine_blkn+ -1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 + 1 <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn <= 0)%Z
   | 27 => (-1 * s V_encode_mcu_DC_refine_z <= 0 /\ 1 * s V_encode_mcu_DC_refine_blkn+ -1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn + 1 <= 0)%Z
   | 28 => (-1 * s V_encode_mcu_DC_refine_blkn + 1 <= 0 /\ 1 * s V_encode_mcu_DC_refine_blkn+ -1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0 /\ -1 * s V_encode_mcu_DC_refine_z <= 0)%Z
   | 29 => (-1 * s V_encode_mcu_DC_refine_z <= 0 /\ 1 * s V_encode_mcu_DC_refine_blkn+ -1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0 /\ -1 * s V_encode_mcu_DC_refine_blkn + 1 <= 0)%Z
   | 30 => (-1 * s V_encode_mcu_DC_refine_blkn + 1 <= 0 /\ 1 * s V_encode_mcu_DC_refine_blkn+ -1 * s V_encode_mcu_DC_refine_cinfo_dref_off360 <= 0 /\ -1 * s V_encode_mcu_DC_refine_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_encode_mcu_DC_refine (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 2 => (max0(s V_encode_mcu_DC_refine_cinfo_dref_off360)
           + max0(s V_encode_mcu_DC_refine_z) <= z)%Q
   | 3 => (max0(s V_encode_mcu_DC_refine_cinfo_dref_off360)
           + max0(s V_encode_mcu_DC_refine_z) <= z)%Q
   | 4 => (max0(s V_encode_mcu_DC_refine_cinfo_dref_off360)
           + max0(s V_encode_mcu_DC_refine_z) <= z)%Q
   | 5 => (max0(s V_encode_mcu_DC_refine_cinfo_dref_off360)
           + max0(s V_encode_mcu_DC_refine_z) <= z)%Q
   | 6 => (max0(s V_encode_mcu_DC_refine_cinfo_dref_off360)
           + max0(s V_encode_mcu_DC_refine_z) <= z)%Q
   | 7 => (max0(s V_encode_mcu_DC_refine_cinfo_dref_off360)
           + max0(s V_encode_mcu_DC_refine_z) <= z)%Q
   | 8 => (max0(s V_encode_mcu_DC_refine_cinfo_dref_off360)
           + max0(s V_encode_mcu_DC_refine_z) <= z)%Q
   | 9 => (max0(s V_encode_mcu_DC_refine_cinfo_dref_off360)
           + max0(s V_encode_mcu_DC_refine_z) <= z)%Q
   | 10 => (max0(s V_encode_mcu_DC_refine_cinfo_dref_off360)
            + max0(s V_encode_mcu_DC_refine_z) <= z)%Q
   | 11 => (max0(-s V_encode_mcu_DC_refine_blkn
                 + s V_encode_mcu_DC_refine_cinfo_dref_off360)
            + max0(s V_encode_mcu_DC_refine_z) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_encode_mcu_DC_refine_z)) (F_check_ge (s V_encode_mcu_DC_refine_z) (0))]
     (max0(-s V_encode_mcu_DC_refine_blkn
           + s V_encode_mcu_DC_refine_cinfo_dref_off360)
      + max0(s V_encode_mcu_DC_refine_z) <= z)%Q
   | 13 => (s V_encode_mcu_DC_refine_z
            + max0(-s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 14 => (s V_encode_mcu_DC_refine_z
            + max0(-s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 15 => (s V_encode_mcu_DC_refine_z
            + max0(-s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_encode_mcu_DC_refine_blkn
                                             + s V_encode_mcu_DC_refine_cinfo_dref_off360) (-1
                                                                    - s V_encode_mcu_DC_refine_blkn
                                                                    + s V_encode_mcu_DC_refine_cinfo_dref_off360));
      (*-1 0*) F_max0_ge_0 (-1 - s V_encode_mcu_DC_refine_blkn
                            + s V_encode_mcu_DC_refine_cinfo_dref_off360)]
     (s V_encode_mcu_DC_refine_z
      + max0(-s V_encode_mcu_DC_refine_blkn
             + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 17 => (s V_encode_mcu_DC_refine_z
            + max0(-s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 18 => (s V_encode_mcu_DC_refine_z
            + max0(-s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 19 => (s V_encode_mcu_DC_refine_z
            + max0(-s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 20 => (s V_encode_mcu_DC_refine_z
            + max0(-s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_encode_mcu_DC_refine_blkn
                                             + s V_encode_mcu_DC_refine_cinfo_dref_off360) (-1
                                                                    - s V_encode_mcu_DC_refine_blkn
                                                                    + s V_encode_mcu_DC_refine_cinfo_dref_off360));
      (*-1 0*) F_max0_ge_0 (-1 - s V_encode_mcu_DC_refine_blkn
                            + s V_encode_mcu_DC_refine_cinfo_dref_off360)]
     (s V_encode_mcu_DC_refine_z
      + max0(-s V_encode_mcu_DC_refine_blkn
             + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 22 => (s V_encode_mcu_DC_refine_z <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_encode_mcu_DC_refine_blkn
                                       + s V_encode_mcu_DC_refine_cinfo_dref_off360) (1)]
     (s V_encode_mcu_DC_refine_z
      + max0(-s V_encode_mcu_DC_refine_blkn
             + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 24 => ((1 # 1) + s V_encode_mcu_DC_refine_z
            + max0(-1 - s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 25 => ((1 # 1) + s V_encode_mcu_DC_refine_z
            + max0(-1 - s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 26 => ((1 # 1) + s V_encode_mcu_DC_refine_z
            + max0(-1 - s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 27 => ((1 # 1) + s V_encode_mcu_DC_refine_z
            + max0(-s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 28 => ((1 # 1) + s V_encode_mcu_DC_refine_z
            + max0(-s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 29 => ((1 # 1) + s V_encode_mcu_DC_refine_z
            + max0(-s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | 30 => (s V_encode_mcu_DC_refine_z
            + max0(-s V_encode_mcu_DC_refine_blkn
                   + s V_encode_mcu_DC_refine_cinfo_dref_off360) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_encode_mcu_DC_refine =>
    [mkPA Q (fun n z s => ai_encode_mcu_DC_refine n s /\ annot0_encode_mcu_DC_refine n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_encode_mcu_DC_refine (proc_start P_encode_mcu_DC_refine) s1 (proc_end P_encode_mcu_DC_refine) s2 ->
    (s2 V_encode_mcu_DC_refine_z <= max0(s1 V_encode_mcu_DC_refine_cinfo_dref_off360))%Q.
Proof.
  prove_bound ipa admissible_ipa P_encode_mcu_DC_refine.
Qed.
