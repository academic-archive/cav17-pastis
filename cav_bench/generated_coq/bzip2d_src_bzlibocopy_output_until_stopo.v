Require Import pasta.Pasta.

Inductive proc: Type :=
  P_copy_output_until_stop.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_copy_output_until_stop_z := 1%positive.
Notation V_copy_output_until_stop_progress_out := 2%positive.
Notation V_copy_output_until_stop_s_dref_off116 := 3%positive.
Notation V_copy_output_until_stop_s_dref_off120 := 4%positive.
Notation V_copy_output_until_stop_s := 5%positive.
Definition Pedges_copy_output_until_stop: list (edge proc) :=
  (EA 1 (AAssign V_copy_output_until_stop_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_copy_output_until_stop_progress_out
  (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::(EA 5 ANone 21)::
  (EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_copy_output_until_stop_s_dref_off120) s) >=
  (eval (EVar V_copy_output_until_stop_s_dref_off116) s))%Z)) 18)::
  (EA 7 (AGuard
  (fun s => ((eval (EVar V_copy_output_until_stop_s_dref_off120) s) <
  (eval (EVar V_copy_output_until_stop_s_dref_off116) s))%Z)) 8)::
  (EA 8 AWeaken 9)::(EA 9 (AAssign V_copy_output_until_stop_progress_out
  (Some (ENum (1)))) 10)::(EA 10 (AAssign
  V_copy_output_until_stop_s_dref_off120
  (Some (EAdd (EVar V_copy_output_until_stop_s_dref_off120)
  (ENum (1))))) 11)::(EA 11 AWeaken 12)::(EA 12 ANone 13)::(EA 12 ANone 14)::
  (EA 13 ANone 14)::(EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_copy_output_until_stop_z (Some (EAdd (ENum (1))
  (EVar V_copy_output_until_stop_z)))) 17)::(EA 17 AWeaken 5)::
  (EA 18 AWeaken 19)::(EA 19 ANone 20)::(EA 20 AWeaken 23)::
  (EA 21 ANone 22)::(EA 22 AWeaken 23)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_copy_output_until_stop => Pedges_copy_output_until_stop
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_copy_output_until_stop => 23
     end)%positive;
  var_global := var_global
}.

Definition ai_copy_output_until_stop (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_copy_output_until_stop_z <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0)%Z
   | 3 => (-1 * s V_copy_output_until_stop_z <= 0 /\ 1 * s V_copy_output_until_stop_z <= 0 /\ 1 * s V_copy_output_until_stop_progress_out <= 0 /\ -1 * s V_copy_output_until_stop_progress_out <= 0)%Z
   | 4 => (-1 * s V_copy_output_until_stop_progress_out <= 0 /\ 1 * s V_copy_output_until_stop_progress_out <= 0 /\ 1 * s V_copy_output_until_stop_z <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0)%Z
   | 5 => (-1 * s V_copy_output_until_stop_z <= 0 /\ -1 * s V_copy_output_until_stop_progress_out <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0)%Z
   | 6 => (1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_progress_out <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0)%Z
   | 7 => (-1 * s V_copy_output_until_stop_z <= 0 /\ -1 * s V_copy_output_until_stop_progress_out <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0)%Z
   | 8 => (1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_progress_out <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0 /\ -1 * s V_copy_output_until_stop_s_dref_off116+ 1 * s V_copy_output_until_stop_s_dref_off120 + 1 <= 0)%Z
   | 9 => (-1 * s V_copy_output_until_stop_s_dref_off116+ 1 * s V_copy_output_until_stop_s_dref_off120 + 1 <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0 /\ -1 * s V_copy_output_until_stop_progress_out <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0)%Z
   | 10 => (-1 * s V_copy_output_until_stop_z <= 0 /\ -1 * s V_copy_output_until_stop_s_dref_off116+ 1 * s V_copy_output_until_stop_s_dref_off120 + 1 <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_progress_out + 1 <= 0)%Z
   | 11 => (-1 * s V_copy_output_until_stop_progress_out + 1 <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0 /\ -1 * s V_copy_output_until_stop_s_dref_off116+ 1 * s V_copy_output_until_stop_s_dref_off120 <= 0)%Z
   | 12 => (-1 * s V_copy_output_until_stop_s_dref_off116+ 1 * s V_copy_output_until_stop_s_dref_off120 <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_progress_out + 1 <= 0)%Z
   | 13 => (-1 * s V_copy_output_until_stop_progress_out + 1 <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0 /\ -1 * s V_copy_output_until_stop_s_dref_off116+ 1 * s V_copy_output_until_stop_s_dref_off120 <= 0)%Z
   | 14 => (-1 * s V_copy_output_until_stop_s_dref_off116+ 1 * s V_copy_output_until_stop_s_dref_off120 <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_progress_out + 1 <= 0)%Z
   | 15 => (-1 * s V_copy_output_until_stop_progress_out + 1 <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0 /\ -1 * s V_copy_output_until_stop_s_dref_off116+ 1 * s V_copy_output_until_stop_s_dref_off120 <= 0)%Z
   | 16 => (-1 * s V_copy_output_until_stop_s_dref_off116+ 1 * s V_copy_output_until_stop_s_dref_off120 <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_progress_out + 1 <= 0)%Z
   | 17 => (-1 * s V_copy_output_until_stop_progress_out + 1 <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_s_dref_off116+ 1 * s V_copy_output_until_stop_s_dref_off120 <= 0 /\ -1 * s V_copy_output_until_stop_z + 1 <= 0)%Z
   | 18 => (1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_progress_out <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0 /\ 1 * s V_copy_output_until_stop_s_dref_off116+ -1 * s V_copy_output_until_stop_s_dref_off120 <= 0)%Z
   | 19 => (1 * s V_copy_output_until_stop_s_dref_off116+ -1 * s V_copy_output_until_stop_s_dref_off120 <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0 /\ -1 * s V_copy_output_until_stop_progress_out <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0)%Z
   | 20 => (1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_progress_out <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0 /\ 1 * s V_copy_output_until_stop_s_dref_off116+ -1 * s V_copy_output_until_stop_s_dref_off120 <= 0)%Z
   | 21 => (1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_progress_out <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0)%Z
   | 22 => (-1 * s V_copy_output_until_stop_z <= 0 /\ -1 * s V_copy_output_until_stop_progress_out <= 0 /\ 1 * s V_copy_output_until_stop_progress_out + -1 <= 0)%Z
   | 23 => (1 * s V_copy_output_until_stop_progress_out + -1 <= 0 /\ -1 * s V_copy_output_until_stop_progress_out <= 0 /\ -1 * s V_copy_output_until_stop_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_copy_output_until_stop (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_copy_output_until_stop_s_dref_off116
                - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 2 => (s V_copy_output_until_stop_z
           + max0(s V_copy_output_until_stop_s_dref_off116
                  - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 3 => (s V_copy_output_until_stop_z
           + max0(s V_copy_output_until_stop_s_dref_off116
                  - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 4 => (s V_copy_output_until_stop_z
           + max0(s V_copy_output_until_stop_s_dref_off116
                  - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 5 => (s V_copy_output_until_stop_z
           + max0(s V_copy_output_until_stop_s_dref_off116
                  - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 6 => (s V_copy_output_until_stop_z
           + max0(s V_copy_output_until_stop_s_dref_off116
                  - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 7 => (s V_copy_output_until_stop_z
           + max0(s V_copy_output_until_stop_s_dref_off116
                  - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_copy_output_until_stop_s_dref_off116
                                       - s V_copy_output_until_stop_s_dref_off120) (1)]
     (s V_copy_output_until_stop_z
      + max0(s V_copy_output_until_stop_s_dref_off116
             - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 9 => ((1 # 1) + s V_copy_output_until_stop_z
           + max0(-1 + s V_copy_output_until_stop_s_dref_off116
                  - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 10 => ((1 # 1) + s V_copy_output_until_stop_z
            + max0(-1 + s V_copy_output_until_stop_s_dref_off116
                   - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 11 => ((1 # 1) + s V_copy_output_until_stop_z
            + max0(s V_copy_output_until_stop_s_dref_off116
                   - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 12 => ((1 # 1) + s V_copy_output_until_stop_z
            + max0(s V_copy_output_until_stop_s_dref_off116
                   - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 13 => ((1 # 1) + s V_copy_output_until_stop_z
            + max0(s V_copy_output_until_stop_s_dref_off116
                   - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 14 => ((1 # 1) + s V_copy_output_until_stop_z
            + max0(s V_copy_output_until_stop_s_dref_off116
                   - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 15 => ((1 # 1) + s V_copy_output_until_stop_z
            + max0(s V_copy_output_until_stop_s_dref_off116
                   - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 16 => ((1 # 1) + s V_copy_output_until_stop_z
            + max0(s V_copy_output_until_stop_s_dref_off116
                   - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 17 => (s V_copy_output_until_stop_z
            + max0(s V_copy_output_until_stop_s_dref_off116
                   - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 18 => (s V_copy_output_until_stop_z
            + max0(s V_copy_output_until_stop_s_dref_off116
                   - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 19 => (s V_copy_output_until_stop_z
            + max0(s V_copy_output_until_stop_s_dref_off116
                   - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_copy_output_until_stop_s_dref_off116
                                             - s V_copy_output_until_stop_s_dref_off120) (-1
                                                                    + s V_copy_output_until_stop_s_dref_off116
                                                                    - s V_copy_output_until_stop_s_dref_off120));
      (*-1 0*) F_max0_ge_0 (-1 + s V_copy_output_until_stop_s_dref_off116
                            - s V_copy_output_until_stop_s_dref_off120)]
     (s V_copy_output_until_stop_z
      + max0(s V_copy_output_until_stop_s_dref_off116
             - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 21 => (s V_copy_output_until_stop_z
            + max0(s V_copy_output_until_stop_s_dref_off116
                   - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_copy_output_until_stop_s_dref_off116
                                             - s V_copy_output_until_stop_s_dref_off120) (-1
                                                                    + s V_copy_output_until_stop_s_dref_off116
                                                                    - s V_copy_output_until_stop_s_dref_off120));
      (*-1 0*) F_max0_ge_0 (-1 + s V_copy_output_until_stop_s_dref_off116
                            - s V_copy_output_until_stop_s_dref_off120)]
     (s V_copy_output_until_stop_z
      + max0(s V_copy_output_until_stop_s_dref_off116
             - s V_copy_output_until_stop_s_dref_off120) <= z)%Q
   | 23 => (s V_copy_output_until_stop_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_copy_output_until_stop =>
    [mkPA Q (fun n z s => ai_copy_output_until_stop n s /\ annot0_copy_output_until_stop n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_copy_output_until_stop (proc_start P_copy_output_until_stop) s1 (proc_end P_copy_output_until_stop) s2 ->
    (s2 V_copy_output_until_stop_z <= max0(s1 V_copy_output_until_stop_s_dref_off116
                                           - s1 V_copy_output_until_stop_s_dref_off120))%Q.
Proof.
  prove_bound ipa admissible_ipa P_copy_output_until_stop.
Qed.
