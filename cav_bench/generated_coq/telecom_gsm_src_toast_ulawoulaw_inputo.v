Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ulaw_input.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ulaw_input_z := 1%positive.
Notation V_ulaw_input__tmp := 2%positive.
Notation V_ulaw_input_c := 3%positive.
Notation V_ulaw_input_i := 4%positive.
Notation V_ulaw_input_buf := 5%positive.
Definition Pedges_ulaw_input: list (edge proc) :=
  (EA 1 (AAssign V_ulaw_input_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_ulaw_input_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_ulaw_input_i) s) <
  (eval (ENum (160)) s))%Z)) 7)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_ulaw_input_i) s) >= (eval (ENum (160))
  s))%Z)) 6)::(EA 6 AWeaken 13)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_ulaw_input_c None) 9)::(EA 9 ANone 10)::(EA 10 AWeaken 11)::
  (EA 11 (AGuard (fun s => True)) 24)::(EA 11 (AGuard (fun s => True)) 12)::
  (EA 12 AWeaken 13)::(EA 13 (AGuard (fun s => ((eval (EVar V_ulaw_input_c)
  s) = (eval (ENum (-1)) s))%Z)) 15)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_ulaw_input_c) s) <> (eval (ENum (-1))
  s))%Z)) 14)::(EA 14 AWeaken 17)::(EA 15 AWeaken 16)::(EA 16 ANone 20)::
  (EA 16 ANone 17)::(EA 17 (AAssign V_ulaw_input__tmp
  (Some (EVar V_ulaw_input_i))) 18)::(EA 18 ANone 19)::(EA 19 AWeaken 23)::
  (EA 20 (AAssign V_ulaw_input__tmp (Some (ENum (-1)))) 21)::
  (EA 21 ANone 22)::(EA 22 AWeaken 23)::(EA 24 AWeaken 25)::
  (EA 25 ANone 26)::(EA 26 (AAssign V_ulaw_input_i
  (Some (EAdd (EVar V_ulaw_input_i) (ENum (1))))) 27)::(EA 27 ANone 28)::
  (EA 28 ANone 29)::(EA 29 (AAssign V_ulaw_input_z (Some (EAdd (ENum (1))
  (EVar V_ulaw_input_z)))) 30)::(EA 30 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ulaw_input => Pedges_ulaw_input
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ulaw_input => 23
     end)%positive;
  var_global := var_global
}.

Definition ai_ulaw_input (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_z <= 0)%Z
   | 3 => (-1 * s V_ulaw_input_z <= 0 /\ 1 * s V_ulaw_input_z <= 0 /\ 1 * s V_ulaw_input_i <= 0 /\ -1 * s V_ulaw_input_i <= 0)%Z
   | 4 => (-1 * s V_ulaw_input_i <= 0 /\ 1 * s V_ulaw_input_i <= 0 /\ 1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_z <= 0)%Z
   | 5 => (-1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0)%Z
   | 6 => (1 * s V_ulaw_input_i + -160 <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i + 160 <= 0)%Z
   | 7 => (-1 * s V_ulaw_input_i <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ 1 * s V_ulaw_input_i + -159 <= 0)%Z
   | 8 => (1 * s V_ulaw_input_i + -159 <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i <= 0)%Z
   | 9 => (-1 * s V_ulaw_input_i <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ 1 * s V_ulaw_input_i + -159 <= 0)%Z
   | 10 => (1 * s V_ulaw_input_i + -159 <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i <= 0)%Z
   | 11 => (-1 * s V_ulaw_input_i <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ 1 * s V_ulaw_input_i + -159 <= 0)%Z
   | 12 => (1 * s V_ulaw_input_i + -159 <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i <= 0)%Z
   | 13 => (1 * s V_ulaw_input_i + -160 <= 0 /\ -1 * s V_ulaw_input_i <= 0 /\ -1 * s V_ulaw_input_z <= 0)%Z
   | 14 => (-1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0)%Z
   | 15 => (-1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0 /\ 1 * s V_ulaw_input_c + 1 <= 0 /\ -1 * s V_ulaw_input_c + -1 <= 0)%Z
   | 16 => (-1 * s V_ulaw_input_c + -1 <= 0 /\ 1 * s V_ulaw_input_c + 1 <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0 /\ -1 * s V_ulaw_input_i <= 0 /\ -1 * s V_ulaw_input_z <= 0)%Z
   | 17 => (-1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0)%Z
   | 18 => (1 * s V_ulaw_input_i + -160 <= 0 /\ -1 * s V_ulaw_input_i <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ 1 * s V_ulaw_input__tmp + -160 <= 0 /\ -1 * s V_ulaw_input__tmp <= 0)%Z
   | 19 => (-1 * s V_ulaw_input__tmp <= 0 /\ 1 * s V_ulaw_input__tmp + -160 <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0)%Z
   | 20 => (-1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0 /\ 1 * s V_ulaw_input_c + 1 <= 0 /\ -1 * s V_ulaw_input_c + -1 <= 0)%Z
   | 21 => (-1 * s V_ulaw_input_c + -1 <= 0 /\ 1 * s V_ulaw_input_c + 1 <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0 /\ -1 * s V_ulaw_input_i <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ 1 * s V_ulaw_input__tmp + 1 <= 0 /\ -1 * s V_ulaw_input__tmp + -1 <= 0)%Z
   | 22 => (-1 * s V_ulaw_input__tmp + -1 <= 0 /\ 1 * s V_ulaw_input__tmp + 1 <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0 /\ 1 * s V_ulaw_input_c + 1 <= 0 /\ -1 * s V_ulaw_input_c + -1 <= 0)%Z
   | 23 => (1 * s V_ulaw_input__tmp + -160 <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0 /\ -1 * s V_ulaw_input_i <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input__tmp + -1 <= 0)%Z
   | 24 => (1 * s V_ulaw_input_i + -159 <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i <= 0)%Z
   | 25 => (-1 * s V_ulaw_input_i <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ 1 * s V_ulaw_input_i + -159 <= 0)%Z
   | 26 => (1 * s V_ulaw_input_i + -159 <= 0 /\ -1 * s V_ulaw_input_z <= 0 /\ -1 * s V_ulaw_input_i <= 0)%Z
   | 27 => (-1 * s V_ulaw_input_z <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0 /\ -1 * s V_ulaw_input_i + 1 <= 0)%Z
   | 28 => (-1 * s V_ulaw_input_i + 1 <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0 /\ -1 * s V_ulaw_input_z <= 0)%Z
   | 29 => (-1 * s V_ulaw_input_z <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0 /\ -1 * s V_ulaw_input_i + 1 <= 0)%Z
   | 30 => (-1 * s V_ulaw_input_i + 1 <= 0 /\ 1 * s V_ulaw_input_i + -160 <= 0 /\ -1 * s V_ulaw_input_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ulaw_input (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((160 # 1) <= z)%Q
   | 2 => ((160 # 1) + s V_ulaw_input_z <= z)%Q
   | 3 => (s V_ulaw_input_z + max0(160 - s V_ulaw_input_i) <= z)%Q
   | 4 => (s V_ulaw_input_z + max0(160 - s V_ulaw_input_i) <= z)%Q
   | 5 => (s V_ulaw_input_z + max0(160 - s V_ulaw_input_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (160 - s V_ulaw_input_i) (159
                                                                    - s V_ulaw_input_i));
      (*-1 0*) F_max0_ge_0 (159 - s V_ulaw_input_i)]
     (s V_ulaw_input_z + max0(160 - s V_ulaw_input_i) <= z)%Q
   | 7 => (s V_ulaw_input_z + max0(160 - s V_ulaw_input_i) <= z)%Q
   | 8 => (s V_ulaw_input_z + max0(160 - s V_ulaw_input_i) <= z)%Q
   | 9 => (s V_ulaw_input_z + max0(160 - s V_ulaw_input_i) <= z)%Q
   | 10 => hints
     [(*-0.00625 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (159
                                                                    - 
                                                                    s V_ulaw_input_i) (0))) (F_max0_ge_0 (159
                                                                    - s V_ulaw_input_i));
      (*-0.00625 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + 
                                                                    s V_ulaw_input_i) (0))) (F_max0_ge_0 (1
                                                                    + s V_ulaw_input_i))]
     (s V_ulaw_input_z + max0(160 - s V_ulaw_input_i) <= z)%Q
   | 11 => (-(1 # 1) + s V_ulaw_input_z
            + (1 # 158) * max0(1 + s V_ulaw_input_i)
            + (1 # 158) * max0(159 - s V_ulaw_input_i)
            + max0(160 - s V_ulaw_input_i) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (160 - s V_ulaw_input_i) (1);
      (*-1.00625 0*) F_max0_ge_0 (159 - s V_ulaw_input_i);
      (*-0.00625 0*) F_binom_monotonic 1 (F_max0_ge_0 (1 + s V_ulaw_input_i)) (F_check_ge (0) (0))]
     (-(1 # 1) + s V_ulaw_input_z + (1 # 158) * max0(1 + s V_ulaw_input_i)
      + (1 # 158) * max0(159 - s V_ulaw_input_i)
      + max0(160 - s V_ulaw_input_i) <= z)%Q
   | 13 => (s V_ulaw_input_z <= z)%Q
   | 14 => (s V_ulaw_input_z <= z)%Q
   | 15 => (s V_ulaw_input_z <= z)%Q
   | 16 => (s V_ulaw_input_z <= z)%Q
   | 17 => (s V_ulaw_input_z <= z)%Q
   | 18 => (s V_ulaw_input_z <= z)%Q
   | 19 => (s V_ulaw_input_z <= z)%Q
   | 20 => (s V_ulaw_input_z <= z)%Q
   | 21 => (s V_ulaw_input_z <= z)%Q
   | 22 => (s V_ulaw_input_z <= z)%Q
   | 23 => (s V_ulaw_input_z <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (160 - s V_ulaw_input_i) (1);
      (*-0.00625 0*) F_binom_monotonic 1 (F_max0_ge_arg (159
                                                         - s V_ulaw_input_i)) (F_check_ge (159
                                                                    - s V_ulaw_input_i) (0))]
     (-(1 # 1) + s V_ulaw_input_z + (1 # 158) * max0(1 + s V_ulaw_input_i)
      + (1 # 158) * max0(159 - s V_ulaw_input_i)
      + max0(160 - s V_ulaw_input_i) <= z)%Q
   | 25 => ((157 # 158) - (1 # 158) * s V_ulaw_input_i + s V_ulaw_input_z
            + (1 # 158) * max0(1 + s V_ulaw_input_i)
            + max0(159 - s V_ulaw_input_i) <= z)%Q
   | 26 => ((157 # 158) - (1 # 158) * s V_ulaw_input_i + s V_ulaw_input_z
            + (1 # 158) * max0(1 + s V_ulaw_input_i)
            + max0(159 - s V_ulaw_input_i) <= z)%Q
   | 27 => ((1 # 1) - (1 # 158) * s V_ulaw_input_i + s V_ulaw_input_z
            + max0(160 - s V_ulaw_input_i)
            + (1 # 158) * max0(s V_ulaw_input_i) <= z)%Q
   | 28 => ((1 # 1) - (1 # 158) * s V_ulaw_input_i + s V_ulaw_input_z
            + max0(160 - s V_ulaw_input_i)
            + (1 # 158) * max0(s V_ulaw_input_i) <= z)%Q
   | 29 => ((1 # 1) - (1 # 158) * s V_ulaw_input_i + s V_ulaw_input_z
            + max0(160 - s V_ulaw_input_i)
            + (1 # 158) * max0(s V_ulaw_input_i) <= z)%Q
   | 30 => hints
     [(*-0.00625 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_ulaw_input_i)) (F_check_ge (s V_ulaw_input_i) (0))]
     (-(1 # 158) * s V_ulaw_input_i + s V_ulaw_input_z
      + max0(160 - s V_ulaw_input_i) + (1 # 158) * max0(s V_ulaw_input_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ulaw_input =>
    [mkPA Q (fun n z s => ai_ulaw_input n s /\ annot0_ulaw_input n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ulaw_input (proc_start P_ulaw_input) s1 (proc_end P_ulaw_input) s2 ->
    (s2 V_ulaw_input_z <= (160 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ulaw_input.
Qed.
