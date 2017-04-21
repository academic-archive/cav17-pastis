Require Import pasta.Pasta.

Inductive proc: Type :=
  P_on_pe.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_on_pe_z := 1%positive.
Notation V_on_pe__tmp := 2%positive.
Notation V_on_pe__tmp1 := 3%positive.
Notation V_on_pe_bits := 4%positive.
Notation V_on_pe_ch := 5%positive.
Notation V_on_pe_gfp_dref_off204 := 6%positive.
Notation V_on_pe_gfp := 7%positive.
Notation V_on_pe_gr := 8%positive.
Notation V_on_pe_l3_side := 9%positive.
Notation V_on_pe_mean_bits := 10%positive.
Notation V_on_pe_pe := 11%positive.
Notation V_on_pe_targ_bits := 12%positive.
Definition Pedges_on_pe: list (edge proc) :=
  (EA 1 (AAssign V_on_pe_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_on_pe__tmp1 (Some (EVar V_on_pe_mean_bits))) 3)::(EA 3 (AAssign
  V_on_pe__tmp (Some (EVar V_on_pe_gr))) 4)::(EA 4 (AAssign V_on_pe_ch
  (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_on_pe_ch) s) <
  (eval (EVar V_on_pe_gfp_dref_off204) s))%Z)) 10)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_on_pe_ch) s) >=
  (eval (EVar V_on_pe_gfp_dref_off204) s))%Z)) 8)::(EA 8 AWeaken 9)::
  (EA 10 AWeaken 11)::(EA 11 (AAssign V_on_pe_bits (Some (ENum (0)))) 12)::
  (EA 12 AWeaken 13)::(EA 13 ANone 15)::(EA 13 ANone 14)::
  (EA 14 AWeaken 20)::(EA 15 AWeaken 16)::(EA 16 ANone 17)::
  (EA 16 ANone 18)::(EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 AWeaken 20)::
  (EA 20 ANone 21)::(EA 20 ANone 22)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_on_pe_bits None) 23)::(EA 23 AWeaken 24)::(EA 24 ANone 26)::
  (EA 24 ANone 25)::(EA 25 AWeaken 28)::(EA 26 ANone 27)::
  (EA 27 AWeaken 28)::(EA 28 ANone 29)::(EA 28 ANone 30)::(EA 29 ANone 30)::
  (EA 30 ANone 31)::(EA 31 (AAssign V_on_pe_ch (Some (EAdd (EVar V_on_pe_ch)
  (ENum (1))))) 32)::(EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_on_pe_z (Some (EAdd (ENum (1)) (EVar V_on_pe_z)))) 35)::
  (EA 35 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_on_pe => Pedges_on_pe
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_on_pe => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_on_pe (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_z <= 0)%Z
   | 3 => (-1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_z <= 0)%Z
   | 4 => (1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_z <= 0)%Z
   | 5 => (-1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 6 => (-1 * s V_on_pe_ch <= 0 /\ 1 * s V_on_pe_ch <= 0 /\ 1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_z <= 0)%Z
   | 7 => (-1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 8 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch+ 1 * s V_on_pe_gfp_dref_off204 <= 0)%Z
   | 9 => (-1 * s V_on_pe_ch+ 1 * s V_on_pe_gfp_dref_off204 <= 0 /\ -1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 10 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0)%Z
   | 11 => (1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ -1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 12 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ 1 * s V_on_pe_bits <= 0 /\ -1 * s V_on_pe_bits <= 0)%Z
   | 13 => (-1 * s V_on_pe_bits <= 0 /\ 1 * s V_on_pe_bits <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ -1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 14 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ 1 * s V_on_pe_bits <= 0 /\ -1 * s V_on_pe_bits <= 0)%Z
   | 15 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ 1 * s V_on_pe_bits <= 0 /\ -1 * s V_on_pe_bits <= 0)%Z
   | 16 => (-1 * s V_on_pe_bits <= 0 /\ 1 * s V_on_pe_bits <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ -1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 17 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ 1 * s V_on_pe_bits <= 0 /\ -1 * s V_on_pe_bits <= 0)%Z
   | 18 => (-1 * s V_on_pe_bits <= 0 /\ 1 * s V_on_pe_bits <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ -1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 19 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ 1 * s V_on_pe_bits <= 0 /\ -1 * s V_on_pe_bits <= 0)%Z
   | 20 => (-1 * s V_on_pe_bits <= 0 /\ 1 * s V_on_pe_bits <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ -1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 21 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ 1 * s V_on_pe_bits <= 0 /\ -1 * s V_on_pe_bits <= 0)%Z
   | 22 => (-1 * s V_on_pe_bits <= 0 /\ 1 * s V_on_pe_bits <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ -1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 23 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0)%Z
   | 24 => (1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ -1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 25 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0)%Z
   | 26 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0)%Z
   | 27 => (1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ -1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 28 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0)%Z
   | 29 => (1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ -1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 30 => (-1 * s V_on_pe_ch <= 0 /\ -1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0)%Z
   | 31 => (1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 + 1 <= 0 /\ -1 * s V_on_pe_z <= 0 /\ -1 * s V_on_pe_ch <= 0)%Z
   | 32 => (-1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 <= 0 /\ -1 * s V_on_pe_ch + 1 <= 0)%Z
   | 33 => (-1 * s V_on_pe_ch + 1 <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 <= 0 /\ -1 * s V_on_pe_z <= 0)%Z
   | 34 => (-1 * s V_on_pe_z <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 <= 0 /\ -1 * s V_on_pe_ch + 1 <= 0)%Z
   | 35 => (-1 * s V_on_pe_ch + 1 <= 0 /\ 1 * s V_on_pe_ch+ -1 * s V_on_pe_gfp_dref_off204 <= 0 /\ -1 * s V_on_pe_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_on_pe (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_on_pe_gfp_dref_off204) <= z)%Q
   | 2 => (s V_on_pe_z + max0(s V_on_pe_gfp_dref_off204) <= z)%Q
   | 3 => (s V_on_pe_z + max0(s V_on_pe_gfp_dref_off204) <= z)%Q
   | 4 => (s V_on_pe_z + max0(s V_on_pe_gfp_dref_off204) <= z)%Q
   | 5 => (s V_on_pe_z + max0(-s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 6 => (s V_on_pe_z + max0(-s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 7 => (s V_on_pe_z + max0(-s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_on_pe_ch
                                             + s V_on_pe_gfp_dref_off204) (-1
                                                                    - s V_on_pe_ch
                                                                    + s V_on_pe_gfp_dref_off204));
      (*-1 0*) F_max0_ge_0 (-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204)]
     (s V_on_pe_z + max0(-s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 9 => (s V_on_pe_z <= z)%Q
   | 10 => hints
     [(*0 1*) F_max0_pre_decrement 1 (-s V_on_pe_ch
                                      + s V_on_pe_gfp_dref_off204) (1)]
     (s V_on_pe_z + max0(-s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 11 => ((1 # 1) + s V_on_pe_z
            + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 12 => ((1 # 1) + s V_on_pe_z
            + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 13 => ((1 # 1) + s V_on_pe_z
            + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 14 => ((1 # 1) + s V_on_pe_z
            + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 15 => ((1 # 1) + s V_on_pe_z
            + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 16 => ((1 # 1) + s V_on_pe_z
            + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 17 => ((1 # 1) + s V_on_pe_z
            + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 18 => ((1 # 1) + s V_on_pe_z
            + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 19 => ((1 # 1) + s V_on_pe_z
            + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 20 => ((1 # 1) + s V_on_pe_z
            + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 21 => ((1 # 1) + s V_on_pe_z
            + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 22 => ((1 # 1) + s V_on_pe_z
            + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 23 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 - s V_on_pe_ch
                                                  + s V_on_pe_gfp_dref_off204)) (F_check_ge (-1
                                                                    - s V_on_pe_ch
                                                                    + s V_on_pe_gfp_dref_off204) (0))]
     ((1 # 1) + s V_on_pe_z
      + max0(-1 - s V_on_pe_ch + s V_on_pe_gfp_dref_off204) <= z)%Q
   | 24 => (-s V_on_pe_ch + s V_on_pe_gfp_dref_off204 + s V_on_pe_z <= z)%Q
   | 25 => (-s V_on_pe_ch + s V_on_pe_gfp_dref_off204 + s V_on_pe_z <= z)%Q
   | 26 => (-s V_on_pe_ch + s V_on_pe_gfp_dref_off204 + s V_on_pe_z <= z)%Q
   | 27 => (-s V_on_pe_ch + s V_on_pe_gfp_dref_off204 + s V_on_pe_z <= z)%Q
   | 28 => (-s V_on_pe_ch + s V_on_pe_gfp_dref_off204 + s V_on_pe_z <= z)%Q
   | 29 => (-s V_on_pe_ch + s V_on_pe_gfp_dref_off204 + s V_on_pe_z <= z)%Q
   | 30 => (-s V_on_pe_ch + s V_on_pe_gfp_dref_off204 + s V_on_pe_z <= z)%Q
   | 31 => (-s V_on_pe_ch + s V_on_pe_gfp_dref_off204 + s V_on_pe_z <= z)%Q
   | 32 => ((1 # 1) - s V_on_pe_ch + s V_on_pe_gfp_dref_off204 + s V_on_pe_z <= z)%Q
   | 33 => ((1 # 1) - s V_on_pe_ch + s V_on_pe_gfp_dref_off204 + s V_on_pe_z <= z)%Q
   | 34 => ((1 # 1) - s V_on_pe_ch + s V_on_pe_gfp_dref_off204 + s V_on_pe_z <= z)%Q
   | 35 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_on_pe_ch
                                                               + s V_on_pe_gfp_dref_off204) (0))) (F_max0_ge_0 (-
                                                                    s V_on_pe_ch
                                                                    + s V_on_pe_gfp_dref_off204))]
     (-s V_on_pe_ch + s V_on_pe_gfp_dref_off204 + s V_on_pe_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_on_pe =>
    [mkPA Q (fun n z s => ai_on_pe n s /\ annot0_on_pe n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_on_pe (proc_start P_on_pe) s1 (proc_end P_on_pe) s2 ->
    (s2 V_on_pe_z <= max0(s1 V_on_pe_gfp_dref_off204))%Q.
Proof.
  prove_bound ipa admissible_ipa P_on_pe.
Qed.
