Require Import pasta.Pasta.

Inductive proc: Type :=
  P_hc_sizeof_decoding.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_hc_sizeof_decoding_z := 1%positive.
Notation V_hc_sizeof_decoding__tmp := 2%positive.
Notation V_hc_sizeof_decoding_carry := 3%positive.
Notation V_hc_sizeof_decoding_def_dref_off8 := 4%positive.
Notation V_hc_sizeof_decoding_i := 5%positive.
Notation V_hc_sizeof_decoding_mask := 6%positive.
Notation V_hc_sizeof_decoding_size := 7%positive.
Notation V_hc_sizeof_decoding_def := 8%positive.
Notation V_hc_sizeof_decoding_initial_bits := 9%positive.
Definition Pedges_hc_sizeof_decoding: list (edge proc) :=
  (EA 1 (AAssign V_hc_sizeof_decoding_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_hc_sizeof_decoding_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_hc_sizeof_decoding_def_dref_off8) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_hc_sizeof_decoding__tmp
  (Some (EVar V_hc_sizeof_decoding_initial_bits))) 6)::(EA 6 (AAssign
  V_hc_sizeof_decoding_size None) 7)::(EA 7 (AAssign
  V_hc_sizeof_decoding_carry (Some (ENum (0)))) 8)::(EA 8 (AAssign
  V_hc_sizeof_decoding_mask (Some (ENum (-2)))) 9)::(EA 9 (AAssign
  V_hc_sizeof_decoding_i (Some (EAdd (EVar V_hc_sizeof_decoding__tmp)
  (ENum (1))))) 10)::(EA 10 ANone 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_hc_sizeof_decoding_i) s) <=
  (eval (EVar V_hc_sizeof_decoding_def_dref_off8) s))%Z)) 15)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_hc_sizeof_decoding_i) s) >
  (eval (EVar V_hc_sizeof_decoding_def_dref_off8) s))%Z)) 13)::
  (EA 13 AWeaken 14)::(EA 15 AWeaken 16)::(EA 16 (AAssign
  V_hc_sizeof_decoding_carry None) 17)::(EA 17 (AAssign
  V_hc_sizeof_decoding_size None) 18)::(EA 18 (AAssign
  V_hc_sizeof_decoding_carry None) 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_hc_sizeof_decoding_i (Some (EAdd (EVar V_hc_sizeof_decoding_i)
  (ENum (1))))) 21)::(EA 21 (AAssign V_hc_sizeof_decoding_carry None) 22)::
  (EA 22 (AAssign V_hc_sizeof_decoding_mask None) 23)::(EA 23 ANone 24)::
  (EA 24 ANone 25)::(EA 25 (AAssign V_hc_sizeof_decoding_z
  (Some (EAdd (ENum (1)) (EVar V_hc_sizeof_decoding_z)))) 26)::
  (EA 26 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_hc_sizeof_decoding => Pedges_hc_sizeof_decoding
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_hc_sizeof_decoding => 14
     end)%positive;
  var_global := var_global
}.

Definition ai_hc_sizeof_decoding (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0)%Z
   | 3 => (-1 * s V_hc_sizeof_decoding_z <= 0 /\ 1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_i <= 0)%Z
   | 4 => (-1 * s V_hc_sizeof_decoding_i <= 0 /\ 1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0)%Z
   | 5 => (-1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ 1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_i <= 0)%Z
   | 6 => (-1 * s V_hc_sizeof_decoding_i <= 0 /\ 1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0)%Z
   | 7 => (-1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ 1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_i <= 0)%Z
   | 8 => (-1 * s V_hc_sizeof_decoding_i <= 0 /\ 1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ 1 * s V_hc_sizeof_decoding_carry <= 0 /\ -1 * s V_hc_sizeof_decoding_carry <= 0)%Z
   | 9 => (-1 * s V_hc_sizeof_decoding_carry <= 0 /\ 1 * s V_hc_sizeof_decoding_carry <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ 1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_i <= 0 /\ 1 * s V_hc_sizeof_decoding_mask + 2 <= 0 /\ -1 * s V_hc_sizeof_decoding_mask + -2 <= 0)%Z
   | 10 => (-1 * s V_hc_sizeof_decoding_mask + -2 <= 0 /\ 1 * s V_hc_sizeof_decoding_mask + 2 <= 0 /\ 1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ 1 * s V_hc_sizeof_decoding_carry <= 0 /\ -1 * s V_hc_sizeof_decoding_carry <= 0)%Z
   | 11 => (-1 * s V_hc_sizeof_decoding_carry <= 0 /\ 1 * s V_hc_sizeof_decoding_carry <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ 1 * s V_hc_sizeof_decoding_z <= 0 /\ 1 * s V_hc_sizeof_decoding_mask + 2 <= 0 /\ -1 * s V_hc_sizeof_decoding_mask + -2 <= 0)%Z
   | 12 => (-1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0)%Z
   | 13 => (-1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ 1 * s V_hc_sizeof_decoding_def_dref_off8+ -1 * s V_hc_sizeof_decoding_i + 1 <= 0)%Z
   | 14 => (1 * s V_hc_sizeof_decoding_def_dref_off8+ -1 * s V_hc_sizeof_decoding_i + 1 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0)%Z
   | 15 => (-1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8+ 1 * s V_hc_sizeof_decoding_i <= 0)%Z
   | 16 => (-1 * s V_hc_sizeof_decoding_def_dref_off8+ 1 * s V_hc_sizeof_decoding_i <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0)%Z
   | 17 => (-1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8+ 1 * s V_hc_sizeof_decoding_i <= 0)%Z
   | 18 => (-1 * s V_hc_sizeof_decoding_def_dref_off8+ 1 * s V_hc_sizeof_decoding_i <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0)%Z
   | 19 => (-1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8+ 1 * s V_hc_sizeof_decoding_i <= 0)%Z
   | 20 => (-1 * s V_hc_sizeof_decoding_def_dref_off8+ 1 * s V_hc_sizeof_decoding_i <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0)%Z
   | 21 => (-1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8+ 1 * s V_hc_sizeof_decoding_i + -1 <= 0)%Z
   | 22 => (-1 * s V_hc_sizeof_decoding_def_dref_off8+ 1 * s V_hc_sizeof_decoding_i + -1 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0)%Z
   | 23 => (-1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8+ 1 * s V_hc_sizeof_decoding_i + -1 <= 0)%Z
   | 24 => (-1 * s V_hc_sizeof_decoding_def_dref_off8+ 1 * s V_hc_sizeof_decoding_i + -1 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0)%Z
   | 25 => (-1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ -1 * s V_hc_sizeof_decoding_z <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8+ 1 * s V_hc_sizeof_decoding_i + -1 <= 0)%Z
   | 26 => (-1 * s V_hc_sizeof_decoding_def_dref_off8+ 1 * s V_hc_sizeof_decoding_i + -1 <= 0 /\ -1 * s V_hc_sizeof_decoding_def_dref_off8 <= 0 /\ -1 * s V_hc_sizeof_decoding_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_hc_sizeof_decoding (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_hc_sizeof_decoding_def_dref_off8
                - s V_hc_sizeof_decoding_initial_bits) <= z)%Q
   | 2 => (s V_hc_sizeof_decoding_z
           + max0(s V_hc_sizeof_decoding_def_dref_off8
                  - s V_hc_sizeof_decoding_initial_bits) <= z)%Q
   | 3 => (s V_hc_sizeof_decoding_z
           + max0(s V_hc_sizeof_decoding_def_dref_off8
                  - s V_hc_sizeof_decoding_initial_bits) <= z)%Q
   | 4 => (s V_hc_sizeof_decoding_z
           + max0(s V_hc_sizeof_decoding_def_dref_off8
                  - s V_hc_sizeof_decoding_initial_bits) <= z)%Q
   | 5 => (s V_hc_sizeof_decoding_z
           + max0(s V_hc_sizeof_decoding_def_dref_off8
                  - s V_hc_sizeof_decoding_initial_bits) <= z)%Q
   | 6 => (s V_hc_sizeof_decoding_z
           + max0(-s V_hc_sizeof_decoding__tmp
                  + s V_hc_sizeof_decoding_def_dref_off8) <= z)%Q
   | 7 => (s V_hc_sizeof_decoding_z
           + max0(-s V_hc_sizeof_decoding__tmp
                  + s V_hc_sizeof_decoding_def_dref_off8) <= z)%Q
   | 8 => (s V_hc_sizeof_decoding_z
           + max0(-s V_hc_sizeof_decoding__tmp
                  + s V_hc_sizeof_decoding_def_dref_off8) <= z)%Q
   | 9 => (s V_hc_sizeof_decoding_z
           + max0(-s V_hc_sizeof_decoding__tmp
                  + s V_hc_sizeof_decoding_def_dref_off8) <= z)%Q
   | 10 => (s V_hc_sizeof_decoding_z
            + max0(1 + s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 11 => (s V_hc_sizeof_decoding_z
            + max0(1 + s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 12 => (s V_hc_sizeof_decoding_z
            + max0(1 + s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                             + s V_hc_sizeof_decoding_def_dref_off8
                                             - s V_hc_sizeof_decoding_i) (s V_hc_sizeof_decoding_def_dref_off8
                                                                    - s V_hc_sizeof_decoding_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_hc_sizeof_decoding_def_dref_off8
                                                 - s V_hc_sizeof_decoding_i)) (F_check_ge (0) (0))]
     (s V_hc_sizeof_decoding_z
      + max0(1 + s V_hc_sizeof_decoding_def_dref_off8
             - s V_hc_sizeof_decoding_i) <= z)%Q
   | 14 => (s V_hc_sizeof_decoding_z <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1
                                       + s V_hc_sizeof_decoding_def_dref_off8
                                       - s V_hc_sizeof_decoding_i) (1)]
     (s V_hc_sizeof_decoding_z
      + max0(1 + s V_hc_sizeof_decoding_def_dref_off8
             - s V_hc_sizeof_decoding_i) <= z)%Q
   | 16 => ((1 # 1) + s V_hc_sizeof_decoding_z
            + max0(s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 17 => ((1 # 1) + s V_hc_sizeof_decoding_z
            + max0(s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 18 => ((1 # 1) + s V_hc_sizeof_decoding_z
            + max0(s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 19 => ((1 # 1) + s V_hc_sizeof_decoding_z
            + max0(s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 20 => ((1 # 1) + s V_hc_sizeof_decoding_z
            + max0(s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 21 => ((1 # 1) + s V_hc_sizeof_decoding_z
            + max0(1 + s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 22 => ((1 # 1) + s V_hc_sizeof_decoding_z
            + max0(1 + s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 23 => ((1 # 1) + s V_hc_sizeof_decoding_z
            + max0(1 + s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 24 => ((1 # 1) + s V_hc_sizeof_decoding_z
            + max0(1 + s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 25 => ((1 # 1) + s V_hc_sizeof_decoding_z
            + max0(1 + s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | 26 => (s V_hc_sizeof_decoding_z
            + max0(1 + s V_hc_sizeof_decoding_def_dref_off8
                   - s V_hc_sizeof_decoding_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_hc_sizeof_decoding =>
    [mkPA Q (fun n z s => ai_hc_sizeof_decoding n s /\ annot0_hc_sizeof_decoding n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_hc_sizeof_decoding (proc_start P_hc_sizeof_decoding) s1 (proc_end P_hc_sizeof_decoding) s2 ->
    (s2 V_hc_sizeof_decoding_z <= max0(s1 V_hc_sizeof_decoding_def_dref_off8
                                       - s1 V_hc_sizeof_decoding_initial_bits))%Q.
Proof.
  prove_bound ipa admissible_ipa P_hc_sizeof_decoding.
Qed.
