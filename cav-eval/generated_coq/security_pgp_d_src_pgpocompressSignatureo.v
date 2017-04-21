Require Import pasta.Pasta.

Inductive proc: Type :=
  P_compressSignature.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_compressSignature_z := 1%positive.
Notation V_compressSignature__tmp := 2%positive.
Notation V_compressSignature_i := 3%positive.
Notation V_compressSignature_header := 4%positive.
Definition Pedges_compressSignature: list (edge proc) :=
  (EA 1 (AAssign V_compressSignature_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_compressSignature_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_compressSignature_i
  (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_compressSignature_i) s) < (eval (ENum (10))
  s))%Z)) 30)::(EA 7 (AGuard (fun s => ((eval (EVar V_compressSignature_i)
  s) >= (eval (ENum (10)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 ANone 13)::
  (EA 9 ANone 10)::(EA 10 (AAssign V_compressSignature__tmp
  (Some (EVar V_compressSignature_i))) 11)::(EA 11 ANone 12)::
  (EA 12 AWeaken 35)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::
  (EA 14 ANone 24)::(EA 15 AWeaken 16)::(EA 16 ANone 17)::(EA 16 ANone 24)::
  (EA 17 AWeaken 18)::(EA 18 ANone 22)::(EA 18 ANone 19)::
  (EA 19 AWeaken 20)::(EA 20 ANone 21)::(EA 20 ANone 24)::
  (EA 21 AWeaken 23)::(EA 22 AWeaken 23)::(EA 23 ANone 27)::
  (EA 23 ANone 24)::(EA 24 (AAssign V_compressSignature__tmp
  (Some (ENum (-1)))) 25)::(EA 25 ANone 26)::(EA 26 AWeaken 35)::
  (EA 27 (AAssign V_compressSignature__tmp
  (Some (EAdd (EVar V_compressSignature_i) (ENum (1))))) 28)::
  (EA 28 ANone 29)::(EA 29 AWeaken 35)::(EA 30 AWeaken 31)::
  (EA 31 ANone 36)::(EA 31 ANone 32)::(EA 32 (AAssign
  V_compressSignature__tmp (Some (EVar V_compressSignature_i))) 33)::
  (EA 33 ANone 34)::(EA 34 AWeaken 35)::(EA 36 ANone 37)::(EA 37 (AAssign
  V_compressSignature_i (Some (EAdd (EVar V_compressSignature_i)
  (ENum (1))))) 38)::(EA 38 ANone 39)::(EA 39 ANone 40)::(EA 40 (AAssign
  V_compressSignature_z (Some (EAdd (ENum (1))
  (EVar V_compressSignature_z)))) 41)::(EA 41 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_compressSignature => Pedges_compressSignature
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_compressSignature => 35
     end)%positive;
  var_global := var_global
}.

Definition ai_compressSignature (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_z <= 0)%Z
   | 3 => (-1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i <= 0)%Z
   | 4 => (-1 * s V_compressSignature_i <= 0 /\ 1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_z <= 0)%Z
   | 5 => (-1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i <= 0 /\ -1 * s V_compressSignature_i <= 0)%Z
   | 6 => (-1 * s V_compressSignature_i <= 0 /\ 1 * s V_compressSignature_i <= 0 /\ 1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_z <= 0)%Z
   | 7 => (-1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0)%Z
   | 8 => (1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 9 => (-1 * s V_compressSignature_i + 10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0)%Z
   | 10 => (1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 11 => (-1 * s V_compressSignature_i + 10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0 /\ 1 * s V_compressSignature__tmp + -10 <= 0 /\ -1 * s V_compressSignature__tmp + 10 <= 0)%Z
   | 12 => (-1 * s V_compressSignature__tmp + 10 <= 0 /\ 1 * s V_compressSignature__tmp + -10 <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 13 => (1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 14 => (-1 * s V_compressSignature_i + 10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0)%Z
   | 15 => (1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 16 => (-1 * s V_compressSignature_i + 10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0)%Z
   | 17 => (1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 18 => (-1 * s V_compressSignature_i + 10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0)%Z
   | 19 => (1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 20 => (-1 * s V_compressSignature_i + 10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0)%Z
   | 21 => (1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 22 => (1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 23 => (-1 * s V_compressSignature_i + 10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0)%Z
   | 24 => (1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 25 => (-1 * s V_compressSignature_i + 10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0 /\ 1 * s V_compressSignature__tmp + 1 <= 0 /\ -1 * s V_compressSignature__tmp + -1 <= 0)%Z
   | 26 => (-1 * s V_compressSignature__tmp + -1 <= 0 /\ 1 * s V_compressSignature__tmp + 1 <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 27 => (1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 28 => (-1 * s V_compressSignature_i + 10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0 /\ 1 * s V_compressSignature__tmp + -11 <= 0 /\ -1 * s V_compressSignature__tmp + 11 <= 0)%Z
   | 29 => (-1 * s V_compressSignature__tmp + 11 <= 0 /\ 1 * s V_compressSignature__tmp + -11 <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i + 10 <= 0)%Z
   | 30 => (-1 * s V_compressSignature_i <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -9 <= 0)%Z
   | 31 => (1 * s V_compressSignature_i + -9 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i <= 0)%Z
   | 32 => (-1 * s V_compressSignature_i <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -9 <= 0)%Z
   | 33 => (1 * s V_compressSignature_i + -9 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i <= 0 /\ 1 * s V_compressSignature__tmp + -9 <= 0 /\ -1 * s V_compressSignature__tmp <= 0)%Z
   | 34 => (-1 * s V_compressSignature__tmp <= 0 /\ 1 * s V_compressSignature__tmp + -9 <= 0 /\ -1 * s V_compressSignature_i <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -9 <= 0)%Z
   | 35 => (-1 * s V_compressSignature__tmp + -1 <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0 /\ 1 * s V_compressSignature__tmp + -11 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i <= 0)%Z
   | 36 => (-1 * s V_compressSignature_i <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -9 <= 0)%Z
   | 37 => (1 * s V_compressSignature_i + -9 <= 0 /\ -1 * s V_compressSignature_z <= 0 /\ -1 * s V_compressSignature_i <= 0)%Z
   | 38 => (-1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_i + 1 <= 0)%Z
   | 39 => (-1 * s V_compressSignature_i + 1 <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z <= 0)%Z
   | 40 => (-1 * s V_compressSignature_z <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_i + 1 <= 0)%Z
   | 41 => (-1 * s V_compressSignature_i + 1 <= 0 /\ 1 * s V_compressSignature_i + -10 <= 0 /\ -1 * s V_compressSignature_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_compressSignature (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((5 # 1) * max0(2 + s V_compressSignature_i) <= z)%Q
   | 2 => (s V_compressSignature_z
           + (5 # 1) * max0(2 + s V_compressSignature_i) <= z)%Q
   | 3 => hints
     [(*-5 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_compressSignature_i)) (F_check_ge (0) (0));
      (*-5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_compressSignature_i) (0))) (F_max0_ge_0 (s V_compressSignature_i));
      (*-5 0*) F_binom_monotonic 1 (F_max0_ge_arg (2
                                                   + s V_compressSignature_i)) (F_check_ge (2
                                                                    + s V_compressSignature_i) (0))]
     (s V_compressSignature_z + (5 # 1) * max0(2 + s V_compressSignature_i) <= z)%Q
   | 4 => ((10 # 1) + s V_compressSignature_z <= z)%Q
   | 5 => (s V_compressSignature_z + max0(10 - s V_compressSignature_i) <= z)%Q
   | 6 => (s V_compressSignature_z + max0(10 - s V_compressSignature_i) <= z)%Q
   | 7 => (s V_compressSignature_z + max0(10 - s V_compressSignature_i) <= z)%Q
   | 8 => (s V_compressSignature_z + max0(10 - s V_compressSignature_i) <= z)%Q
   | 9 => (s V_compressSignature_z + max0(10 - s V_compressSignature_i) <= z)%Q
   | 10 => (s V_compressSignature_z + max0(10 - s V_compressSignature_i) <= z)%Q
   | 11 => (s V_compressSignature_z + max0(9 - s V_compressSignature__tmp)
            - max0(9 - s V_compressSignature_i)
            + max0(10 - s V_compressSignature_i) <= z)%Q
   | 12 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (10 - s V_compressSignature_i) (9
                                                                    - s V_compressSignature_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (9
                                                 - s V_compressSignature__tmp)) (F_check_ge (0) (0))]
     (s V_compressSignature_z + max0(9 - s V_compressSignature__tmp)
      - max0(9 - s V_compressSignature_i)
      + max0(10 - s V_compressSignature_i) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (10 - s V_compressSignature_i) (9
                                                                    - s V_compressSignature_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (9 - s V_compressSignature_i)) (F_check_ge (0) (0))]
     (s V_compressSignature_z + max0(10 - s V_compressSignature_i) <= z)%Q
   | 14 => (s V_compressSignature_z <= z)%Q
   | 15 => (s V_compressSignature_z <= z)%Q
   | 16 => (s V_compressSignature_z <= z)%Q
   | 17 => (s V_compressSignature_z <= z)%Q
   | 18 => (s V_compressSignature_z <= z)%Q
   | 19 => (s V_compressSignature_z <= z)%Q
   | 20 => (s V_compressSignature_z <= z)%Q
   | 21 => (s V_compressSignature_z <= z)%Q
   | 22 => (s V_compressSignature_z <= z)%Q
   | 23 => (s V_compressSignature_z <= z)%Q
   | 24 => (s V_compressSignature_z <= z)%Q
   | 25 => (s V_compressSignature_z <= z)%Q
   | 26 => (s V_compressSignature_z <= z)%Q
   | 27 => (s V_compressSignature_z <= z)%Q
   | 28 => (s V_compressSignature_z <= z)%Q
   | 29 => (s V_compressSignature_z <= z)%Q
   | 30 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (10 - s V_compressSignature_i) (1)]
     (s V_compressSignature_z + max0(10 - s V_compressSignature_i) <= z)%Q
   | 31 => ((1 # 1) + s V_compressSignature_z
            + max0(9 - s V_compressSignature_i) <= z)%Q
   | 32 => ((1 # 1) + s V_compressSignature_z
            + max0(9 - s V_compressSignature_i) <= z)%Q
   | 33 => ((1 # 1) + (1 # 2) * s V_compressSignature__tmp
            - (1 # 2) * s V_compressSignature_i + s V_compressSignature_z
            + max0(9 - s V_compressSignature__tmp) <= z)%Q
   | 34 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (11
                                                   - s V_compressSignature_i)) (F_check_ge (0) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (11
                                                                 - s V_compressSignature_i) (0))) (F_max0_ge_0 (11
                                                                    - s V_compressSignature_i));
      (*0.5 1*) F_binom_monotonic 1 (F_max0_ge_0 (9
                                                  - s V_compressSignature__tmp)) (F_check_ge (0) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                     - s V_compressSignature__tmp)) (F_check_ge (9
                                                                    - s V_compressSignature__tmp) (0))]
     ((1 # 1) + (1 # 2) * s V_compressSignature__tmp
      - (1 # 2) * s V_compressSignature_i + s V_compressSignature_z
      + max0(9 - s V_compressSignature__tmp) <= z)%Q
   | 35 => (s V_compressSignature_z <= z)%Q
   | 36 => ((1 # 1) + s V_compressSignature_z
            + max0(9 - s V_compressSignature_i) <= z)%Q
   | 37 => ((1 # 1) + s V_compressSignature_z
            + max0(9 - s V_compressSignature_i) <= z)%Q
   | 38 => ((1 # 1) + s V_compressSignature_z
            + max0(10 - s V_compressSignature_i) <= z)%Q
   | 39 => ((1 # 1) + s V_compressSignature_z
            + max0(10 - s V_compressSignature_i) <= z)%Q
   | 40 => ((1 # 1) + s V_compressSignature_z
            + max0(10 - s V_compressSignature_i) <= z)%Q
   | 41 => (s V_compressSignature_z + max0(10 - s V_compressSignature_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_compressSignature =>
    [mkPA Q (fun n z s => ai_compressSignature n s /\ annot0_compressSignature n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_compressSignature (proc_start P_compressSignature) s1 (proc_end P_compressSignature) s2 ->
    (s2 V_compressSignature_z <= (5 # 1) * max0(2 + s1 V_compressSignature_i))%Q.
Proof.
  prove_bound ipa admissible_ipa P_compressSignature.
Qed.
