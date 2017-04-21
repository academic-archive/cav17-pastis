Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ireclaim.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ireclaim_z := 1%positive.
Notation V_ireclaim__tmp := 2%positive.
Notation V_ireclaim__tmp1 := 3%positive.
Notation V_ireclaim_global := 4%positive.
Notation V_ireclaim_i := 5%positive.
Notation V_ireclaim_dmem := 6%positive.
Notation V_ireclaim_space := 7%positive.
Definition Pedges_ireclaim: list (edge proc) :=
  (EA 1 (AAssign V_ireclaim_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_ireclaim_i) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 AWeaken 4)::(EA 4 (AAssign V_ireclaim__tmp
  (Some (EVar V_ireclaim_space))) 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_ireclaim__tmp) s) < (eval (ENum (0)) s))%Z)) 9)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_ireclaim__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 ANone 24)::
  (EA 9 AWeaken 10)::(EA 10 (AAssign V_ireclaim_i (Some (ENum (0)))) 11)::
  (EA 11 ANone 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_ireclaim_i) s) < (eval (ENum (4)) s))%Z)) 15)::
  (EA 13 (AGuard (fun s => ((eval (EVar V_ireclaim_i) s) >= (eval (ENum (4))
  s))%Z)) 14)::(EA 14 AWeaken 22)::(EA 15 AWeaken 16)::(EA 16 ANone 34)::
  (EA 16 ANone 17)::(EA 17 AWeaken 18)::(EA 18 ANone 20)::(EA 18 ANone 19)::
  (EA 19 ANone 35)::(EA 20 ANone 21)::(EA 21 AWeaken 22)::(EA 22 ANone 30)::
  (EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 ANone 25)::(EA 25 ANone 26)::
  (EA 26 (AAssign V_ireclaim_global None) 27)::(EA 27 (AAssign
  V_ireclaim__tmp1 (Some (ENum (0)))) 28)::(EA 28 ANone 29)::
  (EA 29 AWeaken 33)::(EA 30 (AAssign V_ireclaim__tmp1
  (Some (ENum (-25)))) 31)::(EA 31 ANone 32)::(EA 32 AWeaken 33)::
  (EA 34 ANone 35)::(EA 35 (AAssign V_ireclaim_i
  (Some (EAdd (EVar V_ireclaim_i) (ENum (1))))) 36)::(EA 36 ANone 37)::
  (EA 37 ANone 38)::(EA 38 (AAssign V_ireclaim_z (Some (EAdd (ENum (1))
  (EVar V_ireclaim_z)))) 39)::(EA 39 AWeaken 13)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ireclaim => Pedges_ireclaim
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ireclaim => 33
     end)%positive;
  var_global := var_global
}.

Definition ai_ireclaim (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_z <= 0)%Z
   | 3 => (-1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0)%Z
   | 4 => (-1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_z <= 0)%Z
   | 5 => (-1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0)%Z
   | 6 => (-1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_z <= 0)%Z
   | 7 => (-1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim__tmp <= 0)%Z
   | 8 => (-1 * s V_ireclaim__tmp <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_z <= 0)%Z
   | 9 => (-1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0)%Z
   | 10 => (1 * s V_ireclaim__tmp + 1 <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_z <= 0)%Z
   | 11 => (-1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0 /\ 1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim_i <= 0)%Z
   | 12 => (-1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0 /\ 1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_z <= 0)%Z
   | 13 => (-1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0 /\ 1 * s V_ireclaim_i + -4 <= 0)%Z
   | 14 => (1 * s V_ireclaim_i + -4 <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i + 4 <= 0)%Z
   | 15 => (1 * s V_ireclaim__tmp + 1 <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim_i + -3 <= 0)%Z
   | 16 => (1 * s V_ireclaim_i + -3 <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0)%Z
   | 17 => (1 * s V_ireclaim__tmp + 1 <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim_i + -3 <= 0)%Z
   | 18 => (1 * s V_ireclaim_i + -3 <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0)%Z
   | 19 => (1 * s V_ireclaim__tmp + 1 <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim_i + -3 <= 0)%Z
   | 20 => (1 * s V_ireclaim__tmp + 1 <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim_i + -3 <= 0)%Z
   | 21 => (1 * s V_ireclaim_i + -3 <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0)%Z
   | 22 => (1 * s V_ireclaim_i + -4 <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim_z <= 0)%Z
   | 23 => (-1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0 /\ 1 * s V_ireclaim_i + -4 <= 0)%Z
   | 24 => (-1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim_z <= 0)%Z
   | 25 => (-1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0)%Z
   | 26 => (-1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim_z <= 0)%Z
   | 27 => (-1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0)%Z
   | 28 => (-1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim__tmp1 <= 0 /\ -1 * s V_ireclaim__tmp1 <= 0)%Z
   | 29 => (-1 * s V_ireclaim__tmp1 <= 0 /\ 1 * s V_ireclaim__tmp1 <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0)%Z
   | 30 => (-1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0 /\ 1 * s V_ireclaim_i + -4 <= 0)%Z
   | 31 => (1 * s V_ireclaim_i + -4 <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim__tmp1 + 25 <= 0 /\ -1 * s V_ireclaim__tmp1 + -25 <= 0)%Z
   | 32 => (-1 * s V_ireclaim__tmp1 + -25 <= 0 /\ 1 * s V_ireclaim__tmp1 + 25 <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0 /\ 1 * s V_ireclaim_i + -4 <= 0)%Z
   | 33 => (1 * s V_ireclaim__tmp1 <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim__tmp1 + -25 <= 0)%Z
   | 34 => (1 * s V_ireclaim__tmp + 1 <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim_i + -3 <= 0)%Z
   | 35 => (1 * s V_ireclaim_i + -3 <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ -1 * s V_ireclaim_i <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0)%Z
   | 36 => (1 * s V_ireclaim__tmp + 1 <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim_i + -4 <= 0 /\ -1 * s V_ireclaim_i + 1 <= 0)%Z
   | 37 => (-1 * s V_ireclaim_i + 1 <= 0 /\ 1 * s V_ireclaim_i + -4 <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0)%Z
   | 38 => (1 * s V_ireclaim__tmp + 1 <= 0 /\ -1 * s V_ireclaim_z <= 0 /\ 1 * s V_ireclaim_i + -4 <= 0 /\ -1 * s V_ireclaim_i + 1 <= 0)%Z
   | 39 => (-1 * s V_ireclaim_i + 1 <= 0 /\ 1 * s V_ireclaim_i + -4 <= 0 /\ 1 * s V_ireclaim__tmp + 1 <= 0 /\ -1 * s V_ireclaim_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ireclaim (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_ireclaim_z <= z)%Q
   | 3 => ((4 # 1) + s V_ireclaim_z <= z)%Q
   | 4 => ((4 # 1) + s V_ireclaim_z <= z)%Q
   | 5 => ((4 # 1) + s V_ireclaim_z <= z)%Q
   | 6 => ((4 # 1) + s V_ireclaim_z <= z)%Q
   | 7 => hints
     [(*-4 0*) F_one;
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_ireclaim_i)) (F_check_ge (s V_ireclaim_i) (0))]
     ((4 # 1) + s V_ireclaim_z <= z)%Q
   | 8 => ((1 # 3) * s V_ireclaim_i + s V_ireclaim_z
           - (1 # 3) * max0(s V_ireclaim_i) <= z)%Q
   | 9 => ((4 # 1) + s V_ireclaim_z <= z)%Q
   | 10 => ((4 # 1) + s V_ireclaim_z <= z)%Q
   | 11 => (s V_ireclaim_z + max0(4 - s V_ireclaim_i) <= z)%Q
   | 12 => (s V_ireclaim_z + max0(4 - s V_ireclaim_i) <= z)%Q
   | 13 => (s V_ireclaim_z + max0(4 - s V_ireclaim_i) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_ireclaim_i) (3
                                                                  - s V_ireclaim_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_ireclaim_i);
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_ireclaim_i)) (F_check_ge (s V_ireclaim_i) (0))]
     (s V_ireclaim_z + max0(4 - s V_ireclaim_i) <= z)%Q
   | 15 => hints
     [(*0 1*) F_max0_pre_decrement 1 (4 - s V_ireclaim_i) (1)]
     (s V_ireclaim_z + max0(4 - s V_ireclaim_i) <= z)%Q
   | 16 => ((1 # 1) + s V_ireclaim_z + max0(3 - s V_ireclaim_i) <= z)%Q
   | 17 => ((1 # 1) + s V_ireclaim_z + max0(3 - s V_ireclaim_i) <= z)%Q
   | 18 => ((1 # 1) + s V_ireclaim_z + max0(3 - s V_ireclaim_i) <= z)%Q
   | 19 => ((1 # 1) + s V_ireclaim_z + max0(3 - s V_ireclaim_i) <= z)%Q
   | 20 => ((1 # 1) + s V_ireclaim_z + max0(3 - s V_ireclaim_i) <= z)%Q
   | 21 => hints
     [(*-1.33333 0*) F_max0_ge_0 (3 - s V_ireclaim_i);
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_ireclaim_i)) (F_check_ge (0) (0));
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                                    - s V_ireclaim_i) (0))) (F_max0_ge_0 (3
                                                                    - s V_ireclaim_i))]
     ((1 # 1) + s V_ireclaim_z + max0(3 - s V_ireclaim_i) <= z)%Q
   | 22 => ((1 # 3) * s V_ireclaim_i + s V_ireclaim_z
            - (1 # 3) * max0(s V_ireclaim_i) <= z)%Q
   | 23 => ((1 # 3) * s V_ireclaim_i + s V_ireclaim_z
            - (1 # 3) * max0(s V_ireclaim_i) <= z)%Q
   | 24 => ((1 # 3) * s V_ireclaim_i + s V_ireclaim_z
            - (1 # 3) * max0(s V_ireclaim_i) <= z)%Q
   | 25 => ((1 # 3) * s V_ireclaim_i + s V_ireclaim_z
            - (1 # 3) * max0(s V_ireclaim_i) <= z)%Q
   | 26 => ((1 # 3) * s V_ireclaim_i + s V_ireclaim_z
            - (1 # 3) * max0(s V_ireclaim_i) <= z)%Q
   | 27 => ((1 # 3) * s V_ireclaim_i + s V_ireclaim_z
            - (1 # 3) * max0(s V_ireclaim_i) <= z)%Q
   | 28 => ((1 # 3) * s V_ireclaim_i + s V_ireclaim_z
            - (1 # 3) * max0(s V_ireclaim_i) <= z)%Q
   | 29 => hints
     [(*0 0.333333*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ireclaim_i) (0))) (F_max0_ge_0 (s V_ireclaim_i))]
     ((1 # 3) * s V_ireclaim_i + s V_ireclaim_z
      - (1 # 3) * max0(s V_ireclaim_i) <= z)%Q
   | 30 => ((1 # 3) * s V_ireclaim_i + s V_ireclaim_z
            - (1 # 3) * max0(s V_ireclaim_i) <= z)%Q
   | 31 => ((1 # 3) * s V_ireclaim_i + s V_ireclaim_z
            - (1 # 3) * max0(s V_ireclaim_i) <= z)%Q
   | 32 => hints
     [(*-0.333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ireclaim_i) (0))) (F_max0_ge_0 (s V_ireclaim_i))]
     ((1 # 3) * s V_ireclaim_i + s V_ireclaim_z
      - (1 # 3) * max0(s V_ireclaim_i) <= z)%Q
   | 33 => (s V_ireclaim_z <= z)%Q
   | 34 => ((1 # 1) + s V_ireclaim_z + max0(3 - s V_ireclaim_i) <= z)%Q
   | 35 => ((1 # 1) + s V_ireclaim_z + max0(3 - s V_ireclaim_i) <= z)%Q
   | 36 => ((1 # 1) + s V_ireclaim_z + max0(4 - s V_ireclaim_i) <= z)%Q
   | 37 => ((1 # 1) + s V_ireclaim_z + max0(4 - s V_ireclaim_i) <= z)%Q
   | 38 => ((1 # 1) + s V_ireclaim_z + max0(4 - s V_ireclaim_i) <= z)%Q
   | 39 => (s V_ireclaim_z + max0(4 - s V_ireclaim_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ireclaim =>
    [mkPA Q (fun n z s => ai_ireclaim n s /\ annot0_ireclaim n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ireclaim (proc_start P_ireclaim) s1 (proc_end P_ireclaim) s2 ->
    (s2 V_ireclaim_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ireclaim.
Qed.
