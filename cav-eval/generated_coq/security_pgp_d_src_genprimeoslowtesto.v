Require Import pasta.Pasta.

Inductive proc: Type :=
  P_slowtest.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_slowtest_z := 1%positive.
Notation V_slowtest__tmp := 2%positive.
Notation V_slowtest_i := 3%positive.
Notation V_slowtest_p := 4%positive.
Definition Pedges_slowtest: list (edge proc) :=
  (EA 1 (AAssign V_slowtest_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_slowtest_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_slowtest_i) s) < (eval (ENum (4))
  s))%Z)) 10)::(EA 5 (AGuard (fun s => ((eval (EVar V_slowtest_i) s) >=
  (eval (ENum (4)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign
  V_slowtest__tmp (Some (ENum (1)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 28)::
  (EA 10 AWeaken 11)::(EA 11 ANone 25)::(EA 11 ANone 12)::
  (EA 12 AWeaken 13)::(EA 13 ANone 22)::(EA 13 ANone 14)::
  (EA 14 AWeaken 15)::(EA 15 ANone 22)::(EA 15 ANone 16)::(EA 16 ANone 17)::
  (EA 17 (AAssign V_slowtest_i (Some (EAdd (EVar V_slowtest_i)
  (ENum (1))))) 18)::(EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_slowtest_z (Some (EAdd (ENum (1)) (EVar V_slowtest_z)))) 21)::
  (EA 21 AWeaken 5)::(EA 22 (AAssign V_slowtest__tmp (Some (ENum (0)))) 23)::
  (EA 23 ANone 24)::(EA 24 AWeaken 28)::(EA 25 (AAssign V_slowtest__tmp
  (Some (ENum (0)))) 26)::(EA 26 ANone 27)::(EA 27 AWeaken 28)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_slowtest => Pedges_slowtest
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_slowtest => 28
     end)%positive;
  var_global := var_global
}.

Definition ai_slowtest (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_slowtest_z <= 0 /\ -1 * s V_slowtest_z <= 0)%Z
   | 3 => (-1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i <= 0 /\ -1 * s V_slowtest_i <= 0)%Z
   | 4 => (-1 * s V_slowtest_i <= 0 /\ 1 * s V_slowtest_i <= 0 /\ 1 * s V_slowtest_z <= 0 /\ -1 * s V_slowtest_z <= 0)%Z
   | 5 => (-1 * s V_slowtest_z <= 0 /\ -1 * s V_slowtest_i <= 0 /\ 1 * s V_slowtest_i + -4 <= 0)%Z
   | 6 => (1 * s V_slowtest_i + -4 <= 0 /\ -1 * s V_slowtest_z <= 0 /\ -1 * s V_slowtest_i + 4 <= 0)%Z
   | 7 => (-1 * s V_slowtest_i + 4 <= 0 /\ -1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i + -4 <= 0)%Z
   | 8 => (1 * s V_slowtest_i + -4 <= 0 /\ -1 * s V_slowtest_z <= 0 /\ -1 * s V_slowtest_i + 4 <= 0 /\ 1 * s V_slowtest__tmp + -1 <= 0 /\ -1 * s V_slowtest__tmp + 1 <= 0)%Z
   | 9 => (-1 * s V_slowtest__tmp + 1 <= 0 /\ 1 * s V_slowtest__tmp + -1 <= 0 /\ -1 * s V_slowtest_i + 4 <= 0 /\ -1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i + -4 <= 0)%Z
   | 10 => (-1 * s V_slowtest_i <= 0 /\ -1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i + -3 <= 0)%Z
   | 11 => (1 * s V_slowtest_i + -3 <= 0 /\ -1 * s V_slowtest_z <= 0 /\ -1 * s V_slowtest_i <= 0)%Z
   | 12 => (-1 * s V_slowtest_i <= 0 /\ -1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i + -3 <= 0)%Z
   | 13 => (1 * s V_slowtest_i + -3 <= 0 /\ -1 * s V_slowtest_z <= 0 /\ -1 * s V_slowtest_i <= 0)%Z
   | 14 => (-1 * s V_slowtest_i <= 0 /\ -1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i + -3 <= 0)%Z
   | 15 => (1 * s V_slowtest_i + -3 <= 0 /\ -1 * s V_slowtest_z <= 0 /\ -1 * s V_slowtest_i <= 0)%Z
   | 16 => (-1 * s V_slowtest_i <= 0 /\ -1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i + -3 <= 0)%Z
   | 17 => (1 * s V_slowtest_i + -3 <= 0 /\ -1 * s V_slowtest_z <= 0 /\ -1 * s V_slowtest_i <= 0)%Z
   | 18 => (-1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i + -4 <= 0 /\ -1 * s V_slowtest_i + 1 <= 0)%Z
   | 19 => (-1 * s V_slowtest_i + 1 <= 0 /\ 1 * s V_slowtest_i + -4 <= 0 /\ -1 * s V_slowtest_z <= 0)%Z
   | 20 => (-1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i + -4 <= 0 /\ -1 * s V_slowtest_i + 1 <= 0)%Z
   | 21 => (-1 * s V_slowtest_i + 1 <= 0 /\ 1 * s V_slowtest_i + -4 <= 0 /\ -1 * s V_slowtest_z + 1 <= 0)%Z
   | 22 => (-1 * s V_slowtest_i <= 0 /\ -1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i + -3 <= 0)%Z
   | 23 => (1 * s V_slowtest_i + -3 <= 0 /\ -1 * s V_slowtest_z <= 0 /\ -1 * s V_slowtest_i <= 0 /\ 1 * s V_slowtest__tmp <= 0 /\ -1 * s V_slowtest__tmp <= 0)%Z
   | 24 => (-1 * s V_slowtest__tmp <= 0 /\ 1 * s V_slowtest__tmp <= 0 /\ -1 * s V_slowtest_i <= 0 /\ -1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i + -3 <= 0)%Z
   | 25 => (-1 * s V_slowtest_i <= 0 /\ -1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i + -3 <= 0)%Z
   | 26 => (1 * s V_slowtest_i + -3 <= 0 /\ -1 * s V_slowtest_z <= 0 /\ -1 * s V_slowtest_i <= 0 /\ 1 * s V_slowtest__tmp <= 0 /\ -1 * s V_slowtest__tmp <= 0)%Z
   | 27 => (-1 * s V_slowtest__tmp <= 0 /\ 1 * s V_slowtest__tmp <= 0 /\ -1 * s V_slowtest_i <= 0 /\ -1 * s V_slowtest_z <= 0 /\ 1 * s V_slowtest_i + -3 <= 0)%Z
   | 28 => (1 * s V_slowtest_i + -4 <= 0 /\ 1 * s V_slowtest__tmp + -1 <= 0 /\ -1 * s V_slowtest_z <= 0 /\ -1 * s V_slowtest_i <= 0 /\ -1 * s V_slowtest__tmp <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_slowtest (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_slowtest_z <= z)%Q
   | 3 => (s V_slowtest_z + max0(4 - s V_slowtest_i) <= z)%Q
   | 4 => (s V_slowtest_z + max0(4 - s V_slowtest_i) <= z)%Q
   | 5 => (s V_slowtest_z + max0(4 - s V_slowtest_i) <= z)%Q
   | 6 => (s V_slowtest_z + max0(4 - s V_slowtest_i) <= z)%Q
   | 7 => (s V_slowtest_z + max0(4 - s V_slowtest_i) <= z)%Q
   | 8 => (s V_slowtest_z + max0(4 - s V_slowtest_i) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_slowtest_i) (3
                                                                  - s V_slowtest_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_slowtest_i)]
     (s V_slowtest_z + max0(4 - s V_slowtest_i) <= z)%Q
   | 10 => hints
     [(*0 1*) F_max0_pre_decrement 1 (4 - s V_slowtest_i) (1);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_slowtest_z) (0))) (F_max0_ge_0 (s V_slowtest_z))]
     (s V_slowtest_z + max0(4 - s V_slowtest_i) <= z)%Q
   | 11 => ((1 # 1) + max0(3 - s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 12 => hints
     [(*0 0.333333*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_slowtest_i) (0))) (F_max0_ge_0 (s V_slowtest_i))]
     ((1 # 1) + max0(3 - s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 13 => ((1 # 1) - (1 # 3) * s V_slowtest_i + max0(3 - s V_slowtest_i)
            + (1 # 3) * max0(s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 14 => ((1 # 1) - (1 # 3) * s V_slowtest_i + max0(3 - s V_slowtest_i)
            + (1 # 3) * max0(s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 15 => ((1 # 1) - (1 # 3) * s V_slowtest_i + max0(3 - s V_slowtest_i)
            + (1 # 3) * max0(s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 16 => ((1 # 1) - (1 # 3) * s V_slowtest_i + max0(3 - s V_slowtest_i)
            + (1 # 3) * max0(s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 17 => ((1 # 1) - (1 # 3) * s V_slowtest_i + max0(3 - s V_slowtest_i)
            + (1 # 3) * max0(s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 18 => ((4 # 3) - (1 # 3) * s V_slowtest_i
            + (1 # 3) * max0(-1 + s V_slowtest_i) + max0(4 - s V_slowtest_i)
            + max0(s V_slowtest_z) <= z)%Q
   | 19 => ((4 # 3) - (1 # 3) * s V_slowtest_i
            + (1 # 3) * max0(-1 + s V_slowtest_i) + max0(4 - s V_slowtest_i)
            + max0(s V_slowtest_z) <= z)%Q
   | 20 => ((4 # 3) - (1 # 3) * s V_slowtest_i
            + (1 # 3) * max0(-1 + s V_slowtest_i) + max0(4 - s V_slowtest_i)
            + max0(s V_slowtest_z) <= z)%Q
   | 21 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_slowtest_z)) (F_check_ge (-1
                                                                    + s V_slowtest_z) (0));
      (*0 0.333333*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_slowtest_i)) (F_check_ge (-1
                                                                    + s V_slowtest_i) (0))]
     ((4 # 3) - (1 # 3) * s V_slowtest_i
      + (1 # 3) * max0(-1 + s V_slowtest_i) + max0(-1 + s V_slowtest_z)
      + max0(4 - s V_slowtest_i) <= z)%Q
   | 22 => ((1 # 1) - (1 # 3) * s V_slowtest_i + max0(3 - s V_slowtest_i)
            + (1 # 3) * max0(s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 23 => ((1 # 1) - (1 # 3) * s V_slowtest_i + max0(3 - s V_slowtest_i)
            + (1 # 3) * max0(s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 24 => hints
     [(*-0.333333 0*) F_max0_pre_decrement 1 (4 - s V_slowtest_i) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_slowtest_z)) (F_check_ge (s V_slowtest_z) (0));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_slowtest_i)) (F_check_ge (0) (0));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                                    - s V_slowtest_i) (0))) (F_max0_ge_0 (4
                                                                    - s V_slowtest_i));
      (*-1.33333 0*) F_binom_monotonic 1 (F_max0_ge_0 (3 - s V_slowtest_i)) (F_check_ge (0) (0))]
     ((1 # 1) - (1 # 3) * s V_slowtest_i + max0(3 - s V_slowtest_i)
      + (1 # 3) * max0(s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 25 => ((1 # 1) + max0(3 - s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 26 => ((1 # 1) + max0(3 - s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_one; (*-1 0*) F_max0_ge_0 (3 - s V_slowtest_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_slowtest_z)) (F_check_ge (s V_slowtest_z) (0))]
     ((1 # 1) + max0(3 - s V_slowtest_i) + max0(s V_slowtest_z) <= z)%Q
   | 28 => (s V_slowtest_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_slowtest =>
    [mkPA Q (fun n z s => ai_slowtest n s /\ annot0_slowtest n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_slowtest (proc_start P_slowtest) s1 (proc_end P_slowtest) s2 ->
    (s2 V_slowtest_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_slowtest.
Qed.
