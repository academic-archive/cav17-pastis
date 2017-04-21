Require Import pasta.Pasta.

Inductive proc: Type :=
  P_main1.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_main1_z := 1%positive.
Notation V_main1__tmp := 2%positive.
Notation V_main1__tmp1 := 3%positive.
Notation V_main1_argc := 4%positive.
Notation V_main1_argv := 5%positive.
Notation V_main1_print := 6%positive.
Definition Pedges_main1: list (edge proc) :=
  (EA 1 (AAssign V_main1_z (Some (ENum (0)))) 2)::(EA 2 (AAssign V_main1__tmp
  (Some (EVar V_main1_argc))) 3)::(EA 3 (AAssign V_main1__tmp1
  (Some (EVar V_main1_print))) 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_main1__tmp) s) < (eval (ENum (2)) s))%Z)) 27)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_main1__tmp) s) >= (eval (ENum (2))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 ANone 8)::(EA 8 (AAssign V_main1__tmp
  (Some (EAdd (EVar V_main1__tmp) (ENum (-1))))) 9)::(EA 9 AWeaken 10)::
  (EA 10 (AGuard (fun s => ((eval (EAdd (EVar V_main1__tmp) (ENum (-1)))
  s) <> (eval (ENum (0)) s))%Z)) 14)::(EA 10 (AGuard
  (fun s => ((eval (EAdd (EVar V_main1__tmp) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 ANone 13)::
  (EA 13 AWeaken 34)::(EA 14 AWeaken 15)::(EA 15 ANone 25)::
  (EA 15 ANone 16)::(EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_main1__tmp1) s) <> (eval (ENum (0)) s))%Z)) 19)::
  (EA 17 (AGuard (fun s => ((eval (EVar V_main1__tmp1) s) = (eval (ENum (0))
  s))%Z)) 18)::(EA 18 AWeaken 21)::(EA 19 AWeaken 20)::(EA 20 ANone 21)::
  (EA 21 ANone 22)::(EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 (AAssign
  V_main1_z (Some (EAdd (ENum (1)) (EVar V_main1_z)))) 8)::(EA 25 ANone 26)::
  (EA 26 AWeaken 34)::(EA 27 AWeaken 28)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_main1__tmp1) s) <> (eval (ENum (0)) s))%Z)) 30)::
  (EA 28 (AGuard (fun s => ((eval (EVar V_main1__tmp1) s) = (eval (ENum (0))
  s))%Z)) 29)::(EA 29 AWeaken 32)::(EA 30 AWeaken 31)::(EA 31 ANone 32)::
  (EA 32 ANone 33)::(EA 33 AWeaken 34)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_main1 => Pedges_main1
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_main1 => 34
     end)%positive;
  var_global := var_global
}.

Definition ai_main1 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 3 => (-1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0)%Z
   | 4 => (1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 5 => (-1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0)%Z
   | 6 => (1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | 7 => (-1 * s V_main1__tmp + 2 <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0)%Z
   | 8 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | 9 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 1 <= 0)%Z
   | 10 => (-1 * s V_main1__tmp + 1 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 11 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 1 <= 0 /\ 1 * s V_main1__tmp + -1 <= 0)%Z
   | 12 => (1 * s V_main1__tmp + -1 <= 0 /\ -1 * s V_main1__tmp + 1 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 13 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 1 <= 0 /\ 1 * s V_main1__tmp + -1 <= 0)%Z
   | 14 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | 15 => (-1 * s V_main1__tmp + 2 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 16 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | 17 => (-1 * s V_main1__tmp + 2 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 18 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0 /\ 1 * s V_main1__tmp1 <= 0 /\ -1 * s V_main1__tmp1 <= 0)%Z
   | 19 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | 20 => (-1 * s V_main1__tmp + 2 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 21 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | 22 => (-1 * s V_main1__tmp + 2 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 23 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | 24 => (-1 * s V_main1__tmp + 2 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 25 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | 26 => (-1 * s V_main1__tmp + 2 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 27 => (1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1__tmp + -1 <= 0)%Z
   | 28 => (1 * s V_main1__tmp + -1 <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0)%Z
   | 29 => (1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1__tmp + -1 <= 0 /\ 1 * s V_main1__tmp1 <= 0 /\ -1 * s V_main1__tmp1 <= 0)%Z
   | 30 => (1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1__tmp + -1 <= 0)%Z
   | 31 => (1 * s V_main1__tmp + -1 <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0)%Z
   | 32 => (1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1__tmp + -1 <= 0)%Z
   | 33 => (1 * s V_main1__tmp + -1 <= 0 /\ -1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0)%Z
   | 34 => (-1 * s V_main1_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_main1 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_main1_argc) <= z)%Q
   | 2 => (s V_main1_z + max0(-1 + s V_main1_argc) <= z)%Q
   | 3 => (s V_main1_z + max0(-1 + s V_main1__tmp) <= z)%Q
   | 4 => (s V_main1_z + max0(-1 + s V_main1__tmp) <= z)%Q
   | 5 => (s V_main1_z + max0(-1 + s V_main1__tmp) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_main1__tmp)) (F_check_ge (-1
                                                                    + s V_main1__tmp) (0))]
     (s V_main1_z + max0(-1 + s V_main1__tmp) <= z)%Q
   | 7 => (-(1 # 1) + s V_main1__tmp + s V_main1_z <= z)%Q
   | 8 => (-(1 # 1) + s V_main1__tmp + s V_main1_z <= z)%Q
   | 9 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 10 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 11 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 12 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 13 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_main1__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_main1__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_main1__tmp))]
     (s V_main1__tmp + s V_main1_z <= z)%Q
   | 14 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 15 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 16 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 17 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 18 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 19 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 20 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 21 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 22 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 23 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 24 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 25 => (s V_main1__tmp + s V_main1_z <= z)%Q
   | 26 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_main1__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_main1__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_main1__tmp))]
     (s V_main1__tmp + s V_main1_z <= z)%Q
   | 27 => hints
     [(*5e-12 1*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_main1__tmp)) (F_check_ge (0) (0))]
     (s V_main1_z + max0(-1 + s V_main1__tmp) <= z)%Q
   | 28 => (s V_main1_z <= z)%Q
   | 29 => (s V_main1_z <= z)%Q
   | 30 => (s V_main1_z <= z)%Q
   | 31 => (s V_main1_z <= z)%Q
   | 32 => (s V_main1_z <= z)%Q
   | 33 => (s V_main1_z <= z)%Q
   | 34 => (s V_main1_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_main1 =>
    [mkPA Q (fun n z s => ai_main1 n s /\ annot0_main1 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_main1 (proc_start P_main1) s1 (proc_end P_main1) s2 ->
    (s2 V_main1_z <= max0(-1 + s1 V_main1_argc))%Q.
Proof.
  prove_bound ipa admissible_ipa P_main1.
Qed.
