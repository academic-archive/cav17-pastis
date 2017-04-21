Require Import pasta.Pasta.

Inductive proc: Type :=
  P_parse_data.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_parse_data_z := 1%positive.
Notation V_parse_data__tmp := 2%positive.
Notation V_parse_data__tmp1 := 3%positive.
Notation V_parse_data_frame_dref_off56 := 4%positive.
Notation V_parse_data_i := 5%positive.
Notation V_parse_data_data := 6%positive.
Notation V_parse_data_frame := 7%positive.
Notation V_parse_data_length := 8%positive.
Definition Pedges_parse_data: list (edge proc) :=
  (EA 1 (AAssign V_parse_data_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_parse_data_i) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 (AGuard (fun s => ((eval (EVar V_parse_data_frame_dref_off56) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_parse_data__tmp1 (Some (EVar V_parse_data_length))) 6)::(EA 6 (AAssign
  V_parse_data_i (Some (ENum (0)))) 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_parse_data_i) s) <
  (eval (EVar V_parse_data_frame_dref_off56) s))%Z)) 14)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_parse_data_i) s) >=
  (eval (EVar V_parse_data_frame_dref_off56) s))%Z)) 10)::
  (EA 10 AWeaken 11)::(EA 11 (AAssign V_parse_data__tmp
  (Some (ENum (0)))) 12)::(EA 12 ANone 13)::(EA 13 AWeaken 25)::
  (EA 14 AWeaken 15)::(EA 15 ANone 22)::(EA 15 ANone 16)::(EA 16 ANone 17)::
  (EA 17 (AAssign V_parse_data_i (Some (EAdd (EVar V_parse_data_i)
  (ENum (1))))) 18)::(EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_parse_data_z (Some (EAdd (ENum (1)) (EVar V_parse_data_z)))) 21)::
  (EA 21 AWeaken 9)::(EA 22 (AAssign V_parse_data__tmp
  (Some (ENum (-1)))) 23)::(EA 23 ANone 24)::(EA 24 AWeaken 25)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_parse_data => Pedges_parse_data
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_parse_data => 25
     end)%positive;
  var_global := var_global
}.

Definition ai_parse_data (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_z <= 0)%Z
   | 3 => (-1 * s V_parse_data_z <= 0 /\ 1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_i <= 0)%Z
   | 4 => (-1 * s V_parse_data_i <= 0 /\ 1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_frame_dref_off56 <= 0)%Z
   | 5 => (-1 * s V_parse_data_frame_dref_off56 <= 0 /\ -1 * s V_parse_data_z <= 0 /\ 1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_i <= 0)%Z
   | 6 => (-1 * s V_parse_data_i <= 0 /\ 1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_frame_dref_off56 <= 0)%Z
   | 7 => (-1 * s V_parse_data_frame_dref_off56 <= 0 /\ -1 * s V_parse_data_z <= 0 /\ 1 * s V_parse_data_z <= 0 /\ 1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_i <= 0)%Z
   | 8 => (-1 * s V_parse_data_i <= 0 /\ 1 * s V_parse_data_i <= 0 /\ 1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_frame_dref_off56 <= 0)%Z
   | 9 => (-1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i <= 0)%Z
   | 10 => (-1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_z <= 0 /\ 1 * s V_parse_data_frame_dref_off56+ -1 * s V_parse_data_i <= 0)%Z
   | 11 => (1 * s V_parse_data_frame_dref_off56+ -1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i <= 0)%Z
   | 12 => (-1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_z <= 0 /\ 1 * s V_parse_data_frame_dref_off56+ -1 * s V_parse_data_i <= 0 /\ 1 * s V_parse_data__tmp <= 0 /\ -1 * s V_parse_data__tmp <= 0)%Z
   | 13 => (-1 * s V_parse_data__tmp <= 0 /\ 1 * s V_parse_data__tmp <= 0 /\ 1 * s V_parse_data_frame_dref_off56+ -1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i <= 0)%Z
   | 14 => (-1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i + 1 <= 0)%Z
   | 15 => (-1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i + 1 <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_i <= 0)%Z
   | 16 => (-1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i + 1 <= 0)%Z
   | 17 => (-1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i + 1 <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_i <= 0)%Z
   | 18 => (-1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_i + 1 <= 0)%Z
   | 19 => (-1 * s V_parse_data_i + 1 <= 0 /\ -1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_z <= 0)%Z
   | 20 => (-1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_i + 1 <= 0)%Z
   | 21 => (-1 * s V_parse_data_i + 1 <= 0 /\ -1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_z + 1 <= 0)%Z
   | 22 => (-1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i + 1 <= 0)%Z
   | 23 => (-1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i + 1 <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_i <= 0 /\ 1 * s V_parse_data__tmp + 1 <= 0 /\ -1 * s V_parse_data__tmp + -1 <= 0)%Z
   | 24 => (-1 * s V_parse_data__tmp + -1 <= 0 /\ 1 * s V_parse_data__tmp + 1 <= 0 /\ -1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i + 1 <= 0)%Z
   | 25 => (-1 * s V_parse_data_frame_dref_off56+ 1 * s V_parse_data_i <= 0 /\ 1 * s V_parse_data__tmp <= 0 /\ -1 * s V_parse_data_z <= 0 /\ -1 * s V_parse_data_i <= 0 /\ -1 * s V_parse_data__tmp + -1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_parse_data (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_parse_data_frame_dref_off56) <= z)%Q
   | 2 => (s V_parse_data_z + max0(s V_parse_data_frame_dref_off56) <= z)%Q
   | 3 => (s V_parse_data_z + max0(s V_parse_data_frame_dref_off56) <= z)%Q
   | 4 => (s V_parse_data_z + max0(s V_parse_data_frame_dref_off56) <= z)%Q
   | 5 => (s V_parse_data_z + max0(s V_parse_data_frame_dref_off56) <= z)%Q
   | 6 => (s V_parse_data_z + max0(s V_parse_data_frame_dref_off56) <= z)%Q
   | 7 => (s V_parse_data_z
           + max0(s V_parse_data_frame_dref_off56 - s V_parse_data_i) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_parse_data_frame_dref_off56
                                                   - s V_parse_data_i)) (F_check_ge (s V_parse_data_frame_dref_off56
                                                                    - s V_parse_data_i) (0))]
     (s V_parse_data_z
      + max0(s V_parse_data_frame_dref_off56 - s V_parse_data_i) <= z)%Q
   | 9 => (s V_parse_data_frame_dref_off56 - s V_parse_data_i
           + s V_parse_data_z <= z)%Q
   | 10 => (s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 11 => (s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 12 => (s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_parse_data_frame_dref_off56
                                             - s V_parse_data_i) (-1
                                                                  + s V_parse_data_frame_dref_off56
                                                                  - s V_parse_data_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_parse_data_frame_dref_off56
                            - s V_parse_data_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_data_frame_dref_off56
                                                               - s V_parse_data_i) (0))) (F_max0_ge_0 (s V_parse_data_frame_dref_off56
                                                                    - s V_parse_data_i))]
     (s V_parse_data_frame_dref_off56 - s V_parse_data_i + s V_parse_data_z <= z)%Q
   | 14 => (s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 15 => (s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 16 => (s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 17 => (s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 18 => ((1 # 1) + s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 19 => ((1 # 1) + s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 20 => ((1 # 1) + s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 21 => (s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 22 => (s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 23 => (s V_parse_data_frame_dref_off56 - s V_parse_data_i
            + s V_parse_data_z <= z)%Q
   | 24 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_parse_data_frame_dref_off56
                                       - s V_parse_data_i) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_parse_data_frame_dref_off56
                            - s V_parse_data_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_parse_data_frame_dref_off56
                                                               - s V_parse_data_i) (0))) (F_max0_ge_0 (s V_parse_data_frame_dref_off56
                                                                    - s V_parse_data_i))]
     (s V_parse_data_frame_dref_off56 - s V_parse_data_i + s V_parse_data_z <= z)%Q
   | 25 => (s V_parse_data_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_parse_data =>
    [mkPA Q (fun n z s => ai_parse_data n s /\ annot0_parse_data n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_parse_data (proc_start P_parse_data) s1 (proc_end P_parse_data) s2 ->
    (s2 V_parse_data_z <= max0(s1 V_parse_data_frame_dref_off56))%Q.
Proof.
  prove_bound ipa admissible_ipa P_parse_data.
Qed.
