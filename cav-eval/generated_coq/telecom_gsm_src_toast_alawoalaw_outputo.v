Require Import pasta.Pasta.

Inductive proc: Type :=
  P_alaw_output.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_alaw_output_z := 1%positive.
Notation V_alaw_output__tmp := 2%positive.
Notation V_alaw_output_i := 3%positive.
Notation V_alaw_output_buf := 4%positive.
Definition Pedges_alaw_output: list (edge proc) :=
  (EA 1 (AAssign V_alaw_output_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_alaw_output_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_alaw_output_i) s) <
  (eval (ENum (160)) s))%Z)) 10)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_alaw_output_i) s) >= (eval (ENum (160))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign V_alaw_output__tmp
  (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 21)::
  (EA 10 AWeaken 11)::(EA 11 ANone 18)::(EA 11 ANone 12)::(EA 12 ANone 13)::
  (EA 13 (AAssign V_alaw_output_i (Some (EAdd (EVar V_alaw_output_i)
  (ENum (1))))) 14)::(EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_alaw_output_z (Some (EAdd (ENum (1)) (EVar V_alaw_output_z)))) 17)::
  (EA 17 AWeaken 5)::(EA 18 (AAssign V_alaw_output__tmp
  (Some (ENum (-1)))) 19)::(EA 19 ANone 20)::(EA 20 AWeaken 21)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_alaw_output => Pedges_alaw_output
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_alaw_output => 21
     end)%positive;
  var_global := var_global
}.

Definition ai_alaw_output (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_alaw_output_z <= 0 /\ -1 * s V_alaw_output_z <= 0)%Z
   | 3 => (-1 * s V_alaw_output_z <= 0 /\ 1 * s V_alaw_output_z <= 0 /\ 1 * s V_alaw_output_i <= 0 /\ -1 * s V_alaw_output_i <= 0)%Z
   | 4 => (-1 * s V_alaw_output_i <= 0 /\ 1 * s V_alaw_output_i <= 0 /\ 1 * s V_alaw_output_z <= 0 /\ -1 * s V_alaw_output_z <= 0)%Z
   | 5 => (-1 * s V_alaw_output_z <= 0 /\ -1 * s V_alaw_output_i <= 0 /\ 1 * s V_alaw_output_i + -160 <= 0)%Z
   | 6 => (1 * s V_alaw_output_i + -160 <= 0 /\ -1 * s V_alaw_output_z <= 0 /\ -1 * s V_alaw_output_i + 160 <= 0)%Z
   | 7 => (-1 * s V_alaw_output_i + 160 <= 0 /\ -1 * s V_alaw_output_z <= 0 /\ 1 * s V_alaw_output_i + -160 <= 0)%Z
   | 8 => (1 * s V_alaw_output_i + -160 <= 0 /\ -1 * s V_alaw_output_z <= 0 /\ -1 * s V_alaw_output_i + 160 <= 0 /\ 1 * s V_alaw_output__tmp <= 0 /\ -1 * s V_alaw_output__tmp <= 0)%Z
   | 9 => (-1 * s V_alaw_output__tmp <= 0 /\ 1 * s V_alaw_output__tmp <= 0 /\ -1 * s V_alaw_output_i + 160 <= 0 /\ -1 * s V_alaw_output_z <= 0 /\ 1 * s V_alaw_output_i + -160 <= 0)%Z
   | 10 => (-1 * s V_alaw_output_i <= 0 /\ -1 * s V_alaw_output_z <= 0 /\ 1 * s V_alaw_output_i + -159 <= 0)%Z
   | 11 => (1 * s V_alaw_output_i + -159 <= 0 /\ -1 * s V_alaw_output_z <= 0 /\ -1 * s V_alaw_output_i <= 0)%Z
   | 12 => (-1 * s V_alaw_output_i <= 0 /\ -1 * s V_alaw_output_z <= 0 /\ 1 * s V_alaw_output_i + -159 <= 0)%Z
   | 13 => (1 * s V_alaw_output_i + -159 <= 0 /\ -1 * s V_alaw_output_z <= 0 /\ -1 * s V_alaw_output_i <= 0)%Z
   | 14 => (-1 * s V_alaw_output_z <= 0 /\ 1 * s V_alaw_output_i + -160 <= 0 /\ -1 * s V_alaw_output_i + 1 <= 0)%Z
   | 15 => (-1 * s V_alaw_output_i + 1 <= 0 /\ 1 * s V_alaw_output_i + -160 <= 0 /\ -1 * s V_alaw_output_z <= 0)%Z
   | 16 => (-1 * s V_alaw_output_z <= 0 /\ 1 * s V_alaw_output_i + -160 <= 0 /\ -1 * s V_alaw_output_i + 1 <= 0)%Z
   | 17 => (-1 * s V_alaw_output_i + 1 <= 0 /\ 1 * s V_alaw_output_i + -160 <= 0 /\ -1 * s V_alaw_output_z + 1 <= 0)%Z
   | 18 => (-1 * s V_alaw_output_i <= 0 /\ -1 * s V_alaw_output_z <= 0 /\ 1 * s V_alaw_output_i + -159 <= 0)%Z
   | 19 => (1 * s V_alaw_output_i + -159 <= 0 /\ -1 * s V_alaw_output_z <= 0 /\ -1 * s V_alaw_output_i <= 0 /\ 1 * s V_alaw_output__tmp + 1 <= 0 /\ -1 * s V_alaw_output__tmp + -1 <= 0)%Z
   | 20 => (-1 * s V_alaw_output__tmp + -1 <= 0 /\ 1 * s V_alaw_output__tmp + 1 <= 0 /\ -1 * s V_alaw_output_i <= 0 /\ -1 * s V_alaw_output_z <= 0 /\ 1 * s V_alaw_output_i + -159 <= 0)%Z
   | 21 => (1 * s V_alaw_output_i + -160 <= 0 /\ 1 * s V_alaw_output__tmp <= 0 /\ -1 * s V_alaw_output_z <= 0 /\ -1 * s V_alaw_output_i <= 0 /\ -1 * s V_alaw_output__tmp + -1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_alaw_output (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((160 # 1) <= z)%Q
   | 2 => ((160 # 1) + s V_alaw_output_z <= z)%Q
   | 3 => ((160 # 1) - (158 # 157) * s V_alaw_output_i + s V_alaw_output_z
           + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 4 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (160
                                                               - s V_alaw_output_i) (0))) (F_max0_ge_0 (160
                                                                    - s V_alaw_output_i))]
     ((160 # 1) - (158 # 157) * s V_alaw_output_i + s V_alaw_output_z
      + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 5 => (-(1 # 157) * s V_alaw_output_i + s V_alaw_output_z
           + max0(160 - s V_alaw_output_i)
           + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 6 => (-(1 # 157) * s V_alaw_output_i + s V_alaw_output_z
           + max0(160 - s V_alaw_output_i)
           + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 7 => (-(1 # 157) * s V_alaw_output_i + s V_alaw_output_z
           + max0(160 - s V_alaw_output_i)
           + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 8 => (-(1 # 157) * s V_alaw_output_i + s V_alaw_output_z
           + max0(160 - s V_alaw_output_i)
           + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (160 - s V_alaw_output_i) (159
                                                                    - s V_alaw_output_i));
      (*-1 0*) F_max0_ge_0 (159 - s V_alaw_output_i);
      (*-0.00628931 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_alaw_output_i)) (F_check_ge (s V_alaw_output_i) (0))]
     (-(1 # 157) * s V_alaw_output_i + s V_alaw_output_z
      + max0(160 - s V_alaw_output_i) + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 10 => hints
     [(*0 1*) F_max0_pre_decrement 1 (160 - s V_alaw_output_i) (1);
      (*0 0.00628931*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (159
                                                                    - s V_alaw_output_i) (0))) (F_max0_ge_0 (159
                                                                    - s V_alaw_output_i))]
     (-(1 # 157) * s V_alaw_output_i + s V_alaw_output_z
      + max0(160 - s V_alaw_output_i) + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 11 => (s V_alaw_output_z + (158 # 157) * max0(159 - s V_alaw_output_i)
            + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 12 => (s V_alaw_output_z + (158 # 157) * max0(159 - s V_alaw_output_i)
            + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 13 => (s V_alaw_output_z + (158 # 157) * max0(159 - s V_alaw_output_i)
            + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 14 => (s V_alaw_output_z + (1 # 157) * max0(-1 + s V_alaw_output_i)
            + (158 # 157) * max0(160 - s V_alaw_output_i) <= z)%Q
   | 15 => (s V_alaw_output_z + (1 # 157) * max0(-1 + s V_alaw_output_i)
            + (158 # 157) * max0(160 - s V_alaw_output_i) <= z)%Q
   | 16 => (s V_alaw_output_z + (1 # 157) * max0(-1 + s V_alaw_output_i)
            + (158 # 157) * max0(160 - s V_alaw_output_i) <= z)%Q
   | 17 => hints
     [(*-0.00628931 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_alaw_output_i) (0))) (F_max0_ge_0 (s V_alaw_output_i));
      (*0 0.00628931*) F_binom_monotonic 1 (F_max0_ge_arg (160
                                                           - s V_alaw_output_i)) (F_check_ge (160
                                                                    - s V_alaw_output_i) (0));
      (*0 0.00628931*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                           + s V_alaw_output_i)) (F_check_ge (-1
                                                                    + s V_alaw_output_i) (0))]
     (-(1 # 1) + s V_alaw_output_z + (1 # 157) * max0(-1 + s V_alaw_output_i)
      + (158 # 157) * max0(160 - s V_alaw_output_i) <= z)%Q
   | 18 => (s V_alaw_output_z + (158 # 157) * max0(159 - s V_alaw_output_i)
            + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 19 => (s V_alaw_output_z + (158 # 157) * max0(159 - s V_alaw_output_i)
            + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 20 => hints
     [(*-1.00629 0*) F_max0_ge_0 (159 - s V_alaw_output_i);
      (*-0.00628931 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_alaw_output_i)) (F_check_ge (0) (0))]
     (s V_alaw_output_z + (158 # 157) * max0(159 - s V_alaw_output_i)
      + (1 # 157) * max0(s V_alaw_output_i) <= z)%Q
   | 21 => (s V_alaw_output_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_alaw_output =>
    [mkPA Q (fun n z s => ai_alaw_output n s /\ annot0_alaw_output n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_alaw_output (proc_start P_alaw_output) s1 (proc_end P_alaw_output) s2 ->
    (s2 V_alaw_output_z <= (160 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_alaw_output.
Qed.
