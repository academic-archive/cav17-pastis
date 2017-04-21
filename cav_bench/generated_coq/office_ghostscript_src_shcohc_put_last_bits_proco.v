Require Import pasta.Pasta.

Inductive proc: Type :=
  P_hc_put_last_bits_proc.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_hc_put_last_bits_proc_z := 1%positive.
Notation V_hc_put_last_bits_proc__tmp := 2%positive.
Notation V_hc_put_last_bits_proc__tmp1 := 3%positive.
Notation V_hc_put_last_bits_proc_c := 4%positive.
Notation V_hc_put_last_bits_proc_ss_dref_off24 := 5%positive.
Notation V_hc_put_last_bits_proc_ss_dref_off28 := 6%positive.
Notation V_hc_put_last_bits_proc_ss_dref_off32 := 7%positive.
Notation V_hc_put_last_bits_proc_bits := 8%positive.
Notation V_hc_put_last_bits_proc_bits_left := 9%positive.
Notation V_hc_put_last_bits_proc_q := 10%positive.
Notation V_hc_put_last_bits_proc_ss := 11%positive.
Definition Pedges_hc_put_last_bits_proc: list (edge proc) :=
  (EA 1 (AAssign V_hc_put_last_bits_proc_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_hc_put_last_bits_proc__tmp1
  (Some (EVar V_hc_put_last_bits_proc_bits))) 3)::(EA 3 (AAssign
  V_hc_put_last_bits_proc__tmp
  (Some (EVar V_hc_put_last_bits_proc_bits_left))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_hc_put_last_bits_proc__tmp) s) < (eval (ENum (32))
  s))%Z)) 12)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_hc_put_last_bits_proc__tmp) s) >=
  (eval (ENum (32)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_hc_put_last_bits_proc_ss_dref_off28
  (Some (EVar V_hc_put_last_bits_proc__tmp1))) 9)::(EA 9 (AAssign
  V_hc_put_last_bits_proc_ss_dref_off32
  (Some (EVar V_hc_put_last_bits_proc__tmp))) 10)::(EA 10 AWeaken 11)::
  (EA 12 AWeaken 13)::(EA 13 (AAssign V_hc_put_last_bits_proc_c None) 14)::
  (EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_hc_put_last_bits_proc_ss_dref_off24) s) <>
  (eval (ENum (0)) s))%Z)) 17)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_hc_put_last_bits_proc_ss_dref_off24) s) =
  (eval (ENum (0)) s))%Z)) 16)::(EA 16 AWeaken 20)::(EA 17 AWeaken 18)::
  (EA 18 (AAssign V_hc_put_last_bits_proc_c None) 19)::(EA 19 ANone 20)::
  (EA 20 (AAssign V_hc_put_last_bits_proc__tmp1 None) 21)::(EA 21 (AAssign
  V_hc_put_last_bits_proc__tmp
  (Some (EAdd (EVar V_hc_put_last_bits_proc__tmp) (ENum (8))))) 22)::
  (EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 (AAssign
  V_hc_put_last_bits_proc_z (Some (EAdd (ENum (1))
  (EVar V_hc_put_last_bits_proc_z)))) 25)::(EA 25 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_hc_put_last_bits_proc => Pedges_hc_put_last_bits_proc
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_hc_put_last_bits_proc => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_hc_put_last_bits_proc (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_hc_put_last_bits_proc_z <= 0 /\ -1 * s V_hc_put_last_bits_proc_z <= 0)%Z
   | 3 => (-1 * s V_hc_put_last_bits_proc_z <= 0 /\ 1 * s V_hc_put_last_bits_proc_z <= 0)%Z
   | 4 => (1 * s V_hc_put_last_bits_proc_z <= 0 /\ -1 * s V_hc_put_last_bits_proc_z <= 0)%Z
   | 5 => (-1 * s V_hc_put_last_bits_proc_z <= 0 /\ 1 * s V_hc_put_last_bits_proc_z <= 0)%Z
   | 6 => (-1 * s V_hc_put_last_bits_proc_z <= 0)%Z
   | 7 => (-1 * s V_hc_put_last_bits_proc_z <= 0 /\ -1 * s V_hc_put_last_bits_proc__tmp + 32 <= 0)%Z
   | 8 => (-1 * s V_hc_put_last_bits_proc__tmp + 32 <= 0 /\ -1 * s V_hc_put_last_bits_proc_z <= 0)%Z
   | 9 => (-1 * s V_hc_put_last_bits_proc_z <= 0 /\ -1 * s V_hc_put_last_bits_proc__tmp + 32 <= 0)%Z
   | 10 => (-1 * s V_hc_put_last_bits_proc__tmp + 32 <= 0 /\ -1 * s V_hc_put_last_bits_proc_z <= 0 /\ -1 * s V_hc_put_last_bits_proc_ss_dref_off32 + 32 <= 0)%Z
   | 11 => (-1 * s V_hc_put_last_bits_proc_ss_dref_off32 + 32 <= 0 /\ -1 * s V_hc_put_last_bits_proc_z <= 0 /\ -1 * s V_hc_put_last_bits_proc__tmp + 32 <= 0)%Z
   | 12 => (-1 * s V_hc_put_last_bits_proc_z <= 0 /\ 1 * s V_hc_put_last_bits_proc__tmp + -31 <= 0)%Z
   | 13 => (1 * s V_hc_put_last_bits_proc__tmp + -31 <= 0 /\ -1 * s V_hc_put_last_bits_proc_z <= 0)%Z
   | 14 => (-1 * s V_hc_put_last_bits_proc_z <= 0 /\ 1 * s V_hc_put_last_bits_proc__tmp + -31 <= 0)%Z
   | 15 => (1 * s V_hc_put_last_bits_proc__tmp + -31 <= 0 /\ -1 * s V_hc_put_last_bits_proc_z <= 0)%Z
   | 16 => (-1 * s V_hc_put_last_bits_proc_z <= 0 /\ 1 * s V_hc_put_last_bits_proc__tmp + -31 <= 0 /\ 1 * s V_hc_put_last_bits_proc_ss_dref_off24 <= 0 /\ -1 * s V_hc_put_last_bits_proc_ss_dref_off24 <= 0)%Z
   | 17 => (-1 * s V_hc_put_last_bits_proc_z <= 0 /\ 1 * s V_hc_put_last_bits_proc__tmp + -31 <= 0)%Z
   | 18 => (1 * s V_hc_put_last_bits_proc__tmp + -31 <= 0 /\ -1 * s V_hc_put_last_bits_proc_z <= 0)%Z
   | 19 => (-1 * s V_hc_put_last_bits_proc_z <= 0 /\ 1 * s V_hc_put_last_bits_proc__tmp + -31 <= 0)%Z
   | 20 => (1 * s V_hc_put_last_bits_proc__tmp + -31 <= 0 /\ -1 * s V_hc_put_last_bits_proc_z <= 0)%Z
   | 21 => (-1 * s V_hc_put_last_bits_proc_z <= 0 /\ 1 * s V_hc_put_last_bits_proc__tmp + -31 <= 0)%Z
   | 22 => (-1 * s V_hc_put_last_bits_proc_z <= 0 /\ 1 * s V_hc_put_last_bits_proc__tmp + -39 <= 0)%Z
   | 23 => (1 * s V_hc_put_last_bits_proc__tmp + -39 <= 0 /\ -1 * s V_hc_put_last_bits_proc_z <= 0)%Z
   | 24 => (-1 * s V_hc_put_last_bits_proc_z <= 0 /\ 1 * s V_hc_put_last_bits_proc__tmp + -39 <= 0)%Z
   | 25 => (1 * s V_hc_put_last_bits_proc__tmp + -39 <= 0 /\ -1 * s V_hc_put_last_bits_proc_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_hc_put_last_bits_proc (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc_bits_left) <= z)%Q
   | 2 => ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc_bits_left)
           + max0(s V_hc_put_last_bits_proc_z) <= z)%Q
   | 3 => ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc_bits_left)
           + max0(s V_hc_put_last_bits_proc_z) <= z)%Q
   | 4 => ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp)
           + max0(s V_hc_put_last_bits_proc_z) <= z)%Q
   | 5 => ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp)
           + max0(s V_hc_put_last_bits_proc_z) <= z)%Q
   | 6 => ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp)
           + max0(s V_hc_put_last_bits_proc_z) <= z)%Q
   | 7 => ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp)
           + max0(s V_hc_put_last_bits_proc_z) <= z)%Q
   | 8 => ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp)
           + max0(s V_hc_put_last_bits_proc_z) <= z)%Q
   | 9 => ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp)
           + max0(s V_hc_put_last_bits_proc_z) <= z)%Q
   | 10 => hints
     [(*-0.125 0*) F_max0_monotonic (F_check_ge (39
                                                 - s V_hc_put_last_bits_proc__tmp) (31
                                                                    - s V_hc_put_last_bits_proc__tmp));
      (*-0.125 0*) F_max0_ge_0 (31 - s V_hc_put_last_bits_proc__tmp);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_hc_put_last_bits_proc_z)) (F_check_ge (s V_hc_put_last_bits_proc_z) (0))]
     ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp)
      + max0(s V_hc_put_last_bits_proc_z) <= z)%Q
   | 11 => (s V_hc_put_last_bits_proc_z <= z)%Q
   | 12 => ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp)
            + max0(s V_hc_put_last_bits_proc_z) <= z)%Q
   | 13 => ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp)
            + max0(s V_hc_put_last_bits_proc_z) <= z)%Q
   | 14 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_hc_put_last_bits_proc_z)) (F_check_ge (s V_hc_put_last_bits_proc_z) (0))]
     ((1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp)
      + max0(s V_hc_put_last_bits_proc_z) <= z)%Q
   | 15 => (s V_hc_put_last_bits_proc_z
            + (1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp) <= z)%Q
   | 16 => hints
     [(*0 0.125*) F_max0_pre_decrement 1 (39 - s V_hc_put_last_bits_proc__tmp) (8)]
     (s V_hc_put_last_bits_proc_z
      + (1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp) <= z)%Q
   | 17 => hints
     [(*-0.125 0*) F_max0_pre_decrement 1 (39
                                           - s V_hc_put_last_bits_proc__tmp) (8)]
     (s V_hc_put_last_bits_proc_z
      + (1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp) <= z)%Q
   | 18 => ((1 # 1) + s V_hc_put_last_bits_proc_z
            + (1 # 8) * max0(31 - s V_hc_put_last_bits_proc__tmp) <= z)%Q
   | 19 => ((1 # 1) + s V_hc_put_last_bits_proc_z
            + (1 # 8) * max0(31 - s V_hc_put_last_bits_proc__tmp) <= z)%Q
   | 20 => ((1 # 1) + s V_hc_put_last_bits_proc_z
            + (1 # 8) * max0(31 - s V_hc_put_last_bits_proc__tmp) <= z)%Q
   | 21 => ((1 # 1) + s V_hc_put_last_bits_proc_z
            + (1 # 8) * max0(31 - s V_hc_put_last_bits_proc__tmp) <= z)%Q
   | 22 => ((1 # 1) + s V_hc_put_last_bits_proc_z
            + (1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp) <= z)%Q
   | 23 => ((1 # 1) + s V_hc_put_last_bits_proc_z
            + (1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp) <= z)%Q
   | 24 => ((1 # 1) + s V_hc_put_last_bits_proc_z
            + (1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_hc_put_last_bits_proc_z) (0))) (F_max0_ge_0 (s V_hc_put_last_bits_proc_z))]
     (s V_hc_put_last_bits_proc_z
      + (1 # 8) * max0(39 - s V_hc_put_last_bits_proc__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_hc_put_last_bits_proc =>
    [mkPA Q (fun n z s => ai_hc_put_last_bits_proc n s /\ annot0_hc_put_last_bits_proc n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_hc_put_last_bits_proc (proc_start P_hc_put_last_bits_proc) s1 (proc_end P_hc_put_last_bits_proc) s2 ->
    (s2 V_hc_put_last_bits_proc_z <= (1 # 8) * max0(39
                                                    - s1 V_hc_put_last_bits_proc_bits_left))%Q.
Proof.
  prove_bound ipa admissible_ipa P_hc_put_last_bits_proc.
Qed.
