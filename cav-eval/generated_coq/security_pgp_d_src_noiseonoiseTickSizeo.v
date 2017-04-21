Require Import pasta.Pasta.

Inductive proc: Type :=
  P_noiseTickSize.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_noiseTickSize_z := 1%positive.
Notation V_noiseTickSize__tmp := 2%positive.
Notation V_noiseTickSize_i := 3%positive.
Notation V_noiseTickSize_j := 4%positive.
Notation V_noiseTickSize_t := 5%positive.
Notation V_noiseTickSize_verbose := 6%positive.
Definition Pedges_noiseTickSize: list (edge proc) :=
  (EA 1 (AAssign V_noiseTickSize_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_noiseTickSize_j (Some (ENum (0)))) 3)::(EA 3 (AAssign V_noiseTickSize_i
  (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 ANone 7)::
  (EA 6 ANone 10)::(EA 7 (AAssign V_noiseTickSize_i
  (Some (EAdd (EVar V_noiseTickSize_i) (ENum (1))))) 8)::(EA 8 (AAssign
  V_noiseTickSize_j (Some (ENum (0)))) 9)::(EA 9 ANone 10)::(EA 10 (AAssign
  V_noiseTickSize_j (Some (EAdd (EVar V_noiseTickSize_j) (ENum (1))))) 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_noiseTickSize_j) s) > (eval (ENum (10000))
  s))%Z)) 31)::(EA 12 (AGuard (fun s => ((eval (EVar V_noiseTickSize_j) s) <=
  (eval (ENum (10000)) s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_noiseTickSize_i) s) < (eval (ENum (15))
  s))%Z)) 27)::(EA 16 (AGuard (fun s => ((eval (EVar V_noiseTickSize_i) s) >=
  (eval (ENum (15)) s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 18 (AAssign
  V_noiseTickSize_t None) 19)::(EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_noiseTickSize_verbose) s) <> (eval (ENum (0))
  s))%Z)) 22)::(EA 20 (AGuard (fun s => ((eval (EVar V_noiseTickSize_verbose)
  s) = (eval (ENum (0)) s))%Z)) 21)::(EA 21 AWeaken 24)::(EA 22 AWeaken 23)::
  (EA 23 ANone 24)::(EA 24 (AAssign V_noiseTickSize__tmp
  (Some (EVar V_noiseTickSize_t))) 25)::(EA 25 ANone 26)::
  (EA 26 AWeaken 35)::(EA 27 AWeaken 28)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_noiseTickSize_z (Some (EAdd (ENum (1)) (EVar V_noiseTickSize_z)))) 30)::
  (EA 30 AWeaken 6)::(EA 31 AWeaken 32)::(EA 32 (AAssign V_noiseTickSize__tmp
  (Some (ENum (2)))) 33)::(EA 33 ANone 34)::(EA 34 AWeaken 35)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_noiseTickSize => Pedges_noiseTickSize
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_noiseTickSize => 35
     end)%positive;
  var_global := var_global
}.

Definition ai_noiseTickSize (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_z <= 0)%Z
   | 3 => (-1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_j <= 0 /\ -1 * s V_noiseTickSize_j <= 0)%Z
   | 4 => (-1 * s V_noiseTickSize_j <= 0 /\ 1 * s V_noiseTickSize_j <= 0 /\ 1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_i <= 0)%Z
   | 5 => (-1 * s V_noiseTickSize_i <= 0 /\ 1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_j <= 0 /\ -1 * s V_noiseTickSize_j <= 0)%Z
   | 6 => (-1 * s V_noiseTickSize_j <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ 1 * s V_noiseTickSize_i + -14 <= 0)%Z
   | 7 => (1 * s V_noiseTickSize_i + -14 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_j <= 0)%Z
   | 8 => (-1 * s V_noiseTickSize_j <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_i + 1 <= 0)%Z
   | 9 => (-1 * s V_noiseTickSize_i + 1 <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_j <= 0 /\ -1 * s V_noiseTickSize_j <= 0)%Z
   | 10 => (1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_j <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0)%Z
   | 11 => (1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ 1 * s V_noiseTickSize_j + -10001 <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0)%Z
   | 12 => (-1 * s V_noiseTickSize_j + 1 <= 0 /\ 1 * s V_noiseTickSize_j + -10001 <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0)%Z
   | 13 => (1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0)%Z
   | 14 => (1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0)%Z
   | 15 => (1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0)%Z
   | 16 => (1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0)%Z
   | 17 => (1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_i + 15 <= 0)%Z
   | 18 => (-1 * s V_noiseTickSize_i + 15 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0)%Z
   | 19 => (1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_i + 15 <= 0)%Z
   | 20 => (-1 * s V_noiseTickSize_i + 15 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0)%Z
   | 21 => (1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_i + 15 <= 0 /\ 1 * s V_noiseTickSize_verbose <= 0 /\ -1 * s V_noiseTickSize_verbose <= 0)%Z
   | 22 => (1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_i + 15 <= 0)%Z
   | 23 => (-1 * s V_noiseTickSize_i + 15 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0)%Z
   | 24 => (1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_i + 15 <= 0)%Z
   | 25 => (-1 * s V_noiseTickSize_i + 15 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0)%Z
   | 26 => (1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_i + 15 <= 0)%Z
   | 27 => (-1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ 1 * s V_noiseTickSize_i + -14 <= 0)%Z
   | 28 => (1 * s V_noiseTickSize_i + -14 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_z <= 0)%Z
   | 29 => (-1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ 1 * s V_noiseTickSize_i + -14 <= 0)%Z
   | 30 => (1 * s V_noiseTickSize_i + -14 <= 0 /\ 1 * s V_noiseTickSize_j + -10000 <= 0 /\ -1 * s V_noiseTickSize_j + 1 <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_z + 1 <= 0)%Z
   | 31 => (1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ 1 * s V_noiseTickSize_j + -10001 <= 0 /\ -1 * s V_noiseTickSize_j + 10001 <= 0)%Z
   | 32 => (-1 * s V_noiseTickSize_j + 10001 <= 0 /\ 1 * s V_noiseTickSize_j + -10001 <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0)%Z
   | 33 => (1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ 1 * s V_noiseTickSize_j + -10001 <= 0 /\ -1 * s V_noiseTickSize_j + 10001 <= 0 /\ 1 * s V_noiseTickSize__tmp + -2 <= 0 /\ -1 * s V_noiseTickSize__tmp + 2 <= 0)%Z
   | 34 => (-1 * s V_noiseTickSize__tmp + 2 <= 0 /\ 1 * s V_noiseTickSize__tmp + -2 <= 0 /\ -1 * s V_noiseTickSize_j + 10001 <= 0 /\ 1 * s V_noiseTickSize_j + -10001 <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0)%Z
   | 35 => (-1 * s V_noiseTickSize_j + 1 <= 0 /\ 1 * s V_noiseTickSize_i + -15 <= 0 /\ -1 * s V_noiseTickSize_z <= 0 /\ -1 * s V_noiseTickSize_i <= 0 /\ 1 * s V_noiseTickSize_j + -10001 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_noiseTickSize (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (-(0 # 1) <= z)%Q
   | 2 => (-(0 # 1) + s V_noiseTickSize_z <= z)%Q
   | 3 => (-(0 # 1) + s V_noiseTickSize_z <= z)%Q
   | 4 => (-(0 # 1) + s V_noiseTickSize_z <= z)%Q
   | 5 => (-(0 # 1) + s V_noiseTickSize_z <= z)%Q
   | 6 => (s V_noiseTickSize_z <= z)%Q
   | 7 => (s V_noiseTickSize_z <= z)%Q
   | 8 => (s V_noiseTickSize_z <= z)%Q
   | 9 => (s V_noiseTickSize_z <= z)%Q
   | 10 => (s V_noiseTickSize_z <= z)%Q
   | 11 => (s V_noiseTickSize_z <= z)%Q
   | 12 => (s V_noiseTickSize_z <= z)%Q
   | 13 => hints
     [(*0 0.0666667*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (15
                                                                    - s V_noiseTickSize_i) (0))) (F_max0_ge_0 (15
                                                                    - s V_noiseTickSize_i))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_noiseTickSize_z)) (F_check_ge (0) (0)))]
     (s V_noiseTickSize_z <= z)%Q
   | 14 => ((1 # 15) * s V_noiseTickSize_i * max0(s V_noiseTickSize_z)
            + s V_noiseTickSize_z
            + (1 # 15) * max0(15 - s V_noiseTickSize_i) * max0(s V_noiseTickSize_z)
            - max0(s V_noiseTickSize_z) <= z)%Q
   | 15 => hints
     [(*-0.0666667 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (15
                                                                    - s V_noiseTickSize_i)) (F_check_ge (15
                                                                    - s V_noiseTickSize_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_noiseTickSize_z)) (F_check_ge (0) (0)))]
     ((1 # 15) * s V_noiseTickSize_i * max0(s V_noiseTickSize_z)
      + s V_noiseTickSize_z
      + (1 # 15) * max0(15 - s V_noiseTickSize_i) * max0(s V_noiseTickSize_z)
      - max0(s V_noiseTickSize_z) <= z)%Q
   | 16 => (s V_noiseTickSize_z <= z)%Q
   | 17 => (s V_noiseTickSize_z <= z)%Q
   | 18 => (s V_noiseTickSize_z <= z)%Q
   | 19 => (s V_noiseTickSize_z <= z)%Q
   | 20 => (s V_noiseTickSize_z <= z)%Q
   | 21 => (s V_noiseTickSize_z <= z)%Q
   | 22 => (s V_noiseTickSize_z <= z)%Q
   | 23 => (s V_noiseTickSize_z <= z)%Q
   | 24 => (s V_noiseTickSize_z <= z)%Q
   | 25 => (s V_noiseTickSize_z <= z)%Q
   | 26 => (s V_noiseTickSize_z <= z)%Q
   | 27 => (s V_noiseTickSize_z <= z)%Q
   | 28 => (s V_noiseTickSize_z <= z)%Q
   | 29 => (s V_noiseTickSize_z <= z)%Q
   | 30 => hints
     [(*-0.00019999 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_noiseTickSize_j)) (F_check_ge (s V_noiseTickSize_j) (0))]
     (-(1 # 1) + s V_noiseTickSize_z <= z)%Q
   | 31 => (s V_noiseTickSize_z <= z)%Q
   | 32 => (s V_noiseTickSize_z <= z)%Q
   | 33 => (s V_noiseTickSize_z <= z)%Q
   | 34 => (s V_noiseTickSize_z <= z)%Q
   | 35 => (s V_noiseTickSize_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_noiseTickSize =>
    [mkPA Q (fun n z s => ai_noiseTickSize n s /\ annot0_noiseTickSize n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_noiseTickSize (proc_start P_noiseTickSize) s1 (proc_end P_noiseTickSize) s2 ->
    (s2 V_noiseTickSize_z <= -(0 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_noiseTickSize.
Qed.
