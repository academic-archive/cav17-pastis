Require Import pasta.Pasta.

Inductive proc: Type :=
  P_trueRandAccum.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_trueRandAccum_z := 1%positive.
Notation V_trueRandAccum__tmp := 2%positive.
Notation V_trueRandAccum_c := 3%positive.
Notation V_trueRandAccum_trueRandBits := 4%positive.
Notation V_trueRandAccum_trueRandPending := 5%positive.
Notation V_trueRandAccum_count := 6%positive.
Definition Pedges_trueRandAccum: list (edge proc) :=
  (EA 1 (AAssign V_trueRandAccum_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_trueRandAccum_trueRandBits) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_trueRandAccum__tmp)
  s) >= (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_trueRandAccum__tmp (Some (EVar V_trueRandAccum_count))) 6)::
  (EA 6 (AAssign V_trueRandAccum__tmp (Some (EAdd (EVar V_trueRandAccum__tmp)
  (EVar V_trueRandAccum_trueRandPending)))) 7)::(EA 7 (AAssign
  V_trueRandAccum_trueRandPending (Some (ENum (0)))) 8)::(EA 8 AWeaken 9)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_trueRandAccum__tmp) s) >
  (eval (ENum (3072)) s))%Z)) 11)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_trueRandAccum__tmp) s) <= (eval (ENum (3072))
  s))%Z)) 10)::(EA 10 AWeaken 15)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_trueRandAccum__tmp (Some (ENum (3072)))) 13)::(EA 13 ANone 14)::
  (EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_trueRandAccum_trueRandBits) s) >=
  (eval (EVar V_trueRandAccum__tmp) s))%Z)) 28)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_trueRandAccum_trueRandBits) s) <
  (eval (EVar V_trueRandAccum__tmp) s))%Z)) 16)::(EA 16 AWeaken 17)::
  (EA 17 ANone 18)::(EA 18 (AAssign V_trueRandAccum_c None) 19)::
  (EA 19 ANone 20)::(EA 20 AWeaken 21)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_trueRandAccum_trueRandBits) s) <
  (eval (EVar V_trueRandAccum__tmp) s))%Z)) 25)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_trueRandAccum_trueRandBits) s) >=
  (eval (EVar V_trueRandAccum__tmp) s))%Z)) 22)::(EA 22 AWeaken 23)::
  (EA 23 ANone 24)::(EA 24 AWeaken 31)::(EA 25 AWeaken 26)::
  (EA 26 ANone 27)::(EA 27 (AAssign V_trueRandAccum_z (Some (EAdd (ENum (1))
  (EVar V_trueRandAccum_z)))) 18)::(EA 28 AWeaken 29)::(EA 29 ANone 30)::
  (EA 30 AWeaken 31)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_trueRandAccum => Pedges_trueRandAccum
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_trueRandAccum => 31
     end)%positive;
  var_global := var_global
}.

Definition ai_trueRandAccum (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_z <= 0)%Z
   | 3 => (-1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0)%Z
   | 4 => (-1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum__tmp <= 0)%Z
   | 5 => (-1 * s V_trueRandAccum__tmp <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0)%Z
   | 6 => (-1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_z <= 0)%Z
   | 7 => (-1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0)%Z
   | 8 => (-1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0)%Z
   | 9 => (-1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0)%Z
   | 10 => (-1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum__tmp + -3072 <= 0)%Z
   | 11 => (-1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum__tmp + 3073 <= 0)%Z
   | 12 => (-1 * s V_trueRandAccum__tmp + 3073 <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0)%Z
   | 13 => (-1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum__tmp + -3072 <= 0 /\ -1 * s V_trueRandAccum__tmp + 3072 <= 0)%Z
   | 14 => (-1 * s V_trueRandAccum__tmp + 3072 <= 0 /\ 1 * s V_trueRandAccum__tmp + -3072 <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0)%Z
   | 15 => (-1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum__tmp + -3072 <= 0)%Z
   | 16 => (1 * s V_trueRandAccum__tmp + -3072 <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ -1 * s V_trueRandAccum__tmp+ 1 * s V_trueRandAccum_trueRandBits + 1 <= 0)%Z
   | 17 => (-1 * s V_trueRandAccum__tmp+ 1 * s V_trueRandAccum_trueRandBits + 1 <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum__tmp + -3072 <= 0)%Z
   | 18 => (-1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum__tmp+ 1 * s V_trueRandAccum_trueRandBits + 1 <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum__tmp + -3072 <= 0)%Z
   | 19 => (1 * s V_trueRandAccum__tmp + -3072 <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ -1 * s V_trueRandAccum__tmp+ 1 * s V_trueRandAccum_trueRandBits + 1 <= 0 /\ -1 * s V_trueRandAccum_z <= 0)%Z
   | 20 => (-1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum__tmp+ 1 * s V_trueRandAccum_trueRandBits + 1 <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum__tmp + -3072 <= 0)%Z
   | 21 => (1 * s V_trueRandAccum__tmp + -3072 <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ -1 * s V_trueRandAccum__tmp+ 1 * s V_trueRandAccum_trueRandBits + 1 <= 0 /\ -1 * s V_trueRandAccum_z <= 0)%Z
   | 22 => (False)%Z
   | 23 => (False)%Z
   | 24 => (False)%Z
   | 25 => (-1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum__tmp+ 1 * s V_trueRandAccum_trueRandBits + 1 <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum__tmp + -3072 <= 0)%Z
   | 26 => (1 * s V_trueRandAccum__tmp + -3072 <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ -1 * s V_trueRandAccum__tmp+ 1 * s V_trueRandAccum_trueRandBits + 1 <= 0 /\ -1 * s V_trueRandAccum_z <= 0)%Z
   | 27 => (-1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum__tmp+ 1 * s V_trueRandAccum_trueRandBits + 1 <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum__tmp + -3072 <= 0)%Z
   | 28 => (1 * s V_trueRandAccum__tmp + -3072 <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum__tmp+ -1 * s V_trueRandAccum_trueRandBits <= 0)%Z
   | 29 => (1 * s V_trueRandAccum__tmp+ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum__tmp + -3072 <= 0)%Z
   | 30 => (1 * s V_trueRandAccum__tmp + -3072 <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum__tmp+ -1 * s V_trueRandAccum_trueRandBits <= 0)%Z
   | 31 => (1 * s V_trueRandAccum__tmp+ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ -1 * s V_trueRandAccum_trueRandBits <= 0 /\ 1 * s V_trueRandAccum_z <= 0 /\ -1 * s V_trueRandAccum_z <= 0 /\ 1 * s V_trueRandAccum_trueRandPending <= 0 /\ -1 * s V_trueRandAccum_trueRandPending <= 0 /\ 1 * s V_trueRandAccum__tmp + -3072 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_trueRandAccum (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (0 <= z)%Q
   | 2 => (s V_trueRandAccum_z <= z)%Q
   | 3 => (s V_trueRandAccum_z <= z)%Q
   | 4 => (s V_trueRandAccum_z <= z)%Q
   | 5 => (s V_trueRandAccum_z <= z)%Q
   | 6 => (s V_trueRandAccum_z <= z)%Q
   | 7 => (s V_trueRandAccum_z <= z)%Q
   | 8 => (s V_trueRandAccum_z <= z)%Q
   | 9 => (s V_trueRandAccum_z <= z)%Q
   | 10 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_trueRandAccum_z) (0))) (F_max0_ge_0 (s V_trueRandAccum_z))]
     (s V_trueRandAccum_z <= z)%Q
   | 11 => (s V_trueRandAccum_z <= z)%Q
   | 12 => (s V_trueRandAccum_z <= z)%Q
   | 13 => (s V_trueRandAccum_z <= z)%Q
   | 14 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_trueRandAccum_z) (0))) (F_max0_ge_0 (s V_trueRandAccum_z))]
     (s V_trueRandAccum_z <= z)%Q
   | 15 => (max0(s V_trueRandAccum_z) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_trueRandAccum_z)) (F_check_ge (0) (0))]
     (max0(s V_trueRandAccum_z) <= z)%Q
   | 17 => (0 <= z)%Q
   | 18 => (0 <= z)%Q
   | 19 => (0 <= z)%Q
   | 20 => (0 <= z)%Q
   | 21 => (0 <= z)%Q
   | 22 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_trueRandAccum_z) (0))) (F_max0_ge_0 (-
                                                                    s V_trueRandAccum_z))]
     (0 <= z)%Q
   | 23 => (s V_trueRandAccum_z + max0(-s V_trueRandAccum_z) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_trueRandAccum_z)) (F_check_ge (0) (0))]
     (s V_trueRandAccum_z + max0(-s V_trueRandAccum_z) <= z)%Q
   | 25 => (0 <= z)%Q
   | 26 => (0 <= z)%Q
   | 27 => (0 <= z)%Q
   | 28 => (max0(s V_trueRandAccum_z) <= z)%Q
   | 29 => (max0(s V_trueRandAccum_z) <= z)%Q
   | 30 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_trueRandAccum_z)) (F_check_ge (s V_trueRandAccum_z) (0))]
     (max0(s V_trueRandAccum_z) <= z)%Q
   | 31 => (s V_trueRandAccum_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_trueRandAccum =>
    [mkPA Q (fun n z s => ai_trueRandAccum n s /\ annot0_trueRandAccum n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_trueRandAccum (proc_start P_trueRandAccum) s1 (proc_end P_trueRandAccum) s2 ->
    (s2 V_trueRandAccum_z <= 0)%Q.
Proof.
  prove_bound ipa admissible_ipa P_trueRandAccum.
Qed.
