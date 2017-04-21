Require Import pasta.Pasta.

Inductive proc: Type :=
  P_num_params.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_num_params_z := 1%positive.
Notation V_num_params__tmp := 2%positive.
Notation V_num_params__tmp1 := 3%positive.
Notation V_num_params_mask := 4%positive.
Notation V_num_params_count := 5%positive.
Notation V_num_params_op := 6%positive.
Notation V_num_params_pval := 7%positive.
Definition Pedges_num_params: list (edge proc) :=
  (EA 1 (AAssign V_num_params_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_num_params__tmp (Some (EVar V_num_params_count))) 3)::(EA 3 (AAssign
  V_num_params_mask (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 (AAssign
  V_num_params__tmp (Some (EAdd (EVar V_num_params__tmp) (ENum (-1))))) 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EAdd (EVar V_num_params__tmp) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 17)::(EA 7 (AGuard
  (fun s => ((eval (EAdd (EVar V_num_params__tmp) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_num_params_mask) s) < (eval (ENum (0))
  s))%Z)) 12)::(EA 9 (AGuard (fun s => ((eval (EVar V_num_params_mask) s) >=
  (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 ANone 14)::
  (EA 12 AWeaken 13)::(EA 13 ANone 14)::(EA 14 (AAssign V_num_params__tmp1
  None) 15)::(EA 15 ANone 16)::(EA 16 AWeaken 33)::(EA 17 AWeaken 18)::
  (EA 18 (AAssign V_num_params_mask None) 19)::(EA 19 AWeaken 20)::
  (EA 20 ANone 30)::(EA 20 ANone 26)::(EA 20 ANone 24)::(EA 20 ANone 21)::
  (EA 21 (AAssign V_num_params__tmp1 (Some (ENum (-17)))) 22)::
  (EA 22 ANone 23)::(EA 23 AWeaken 33)::(EA 24 (AAssign V_num_params_mask
  (Some (EAdd (EVar V_num_params_mask) (ENum (1))))) 25)::(EA 25 ANone 27)::
  (EA 26 ANone 27)::(EA 27 ANone 28)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_num_params_z (Some (EAdd (ENum (1)) (EVar V_num_params_z)))) 5)::
  (EA 30 (AAssign V_num_params__tmp1 (Some (ENum (-20)))) 31)::
  (EA 31 ANone 32)::(EA 32 AWeaken 33)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_num_params => Pedges_num_params
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_num_params => 33
     end)%positive;
  var_global := var_global
}.

Definition ai_num_params (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_num_params_z <= 0 /\ -1 * s V_num_params_z <= 0)%Z
   | 3 => (-1 * s V_num_params_z <= 0 /\ 1 * s V_num_params_z <= 0)%Z
   | 4 => (1 * s V_num_params_z <= 0 /\ -1 * s V_num_params_z <= 0 /\ 1 * s V_num_params_mask <= 0 /\ -1 * s V_num_params_mask <= 0)%Z
   | 5 => (-1 * s V_num_params_z <= 0)%Z
   | 6 => (-1 * s V_num_params_z <= 0)%Z
   | 7 => (-1 * s V_num_params_z <= 0)%Z
   | 8 => (-1 * s V_num_params_z <= 0 /\ 1 * s V_num_params__tmp <= 0)%Z
   | 9 => (1 * s V_num_params__tmp <= 0 /\ -1 * s V_num_params_z <= 0)%Z
   | 10 => (-1 * s V_num_params_z <= 0 /\ 1 * s V_num_params__tmp <= 0 /\ -1 * s V_num_params_mask <= 0)%Z
   | 11 => (-1 * s V_num_params_mask <= 0 /\ 1 * s V_num_params__tmp <= 0 /\ -1 * s V_num_params_z <= 0)%Z
   | 12 => (-1 * s V_num_params_z <= 0 /\ 1 * s V_num_params__tmp <= 0 /\ 1 * s V_num_params_mask + 1 <= 0)%Z
   | 13 => (1 * s V_num_params_mask + 1 <= 0 /\ 1 * s V_num_params__tmp <= 0 /\ -1 * s V_num_params_z <= 0)%Z
   | 14 => (-1 * s V_num_params_z <= 0 /\ 1 * s V_num_params__tmp <= 0)%Z
   | 15 => (1 * s V_num_params__tmp <= 0 /\ -1 * s V_num_params_z <= 0)%Z
   | 16 => (-1 * s V_num_params_z <= 0 /\ 1 * s V_num_params__tmp <= 0)%Z
   | 17 => (-1 * s V_num_params_z <= 0 /\ -1 * s V_num_params__tmp + 1 <= 0)%Z
   | 18 => (-1 * s V_num_params__tmp + 1 <= 0 /\ -1 * s V_num_params_z <= 0)%Z
   | 19 => (-1 * s V_num_params_z <= 0 /\ -1 * s V_num_params__tmp + 1 <= 0)%Z
   | 20 => (-1 * s V_num_params__tmp + 1 <= 0 /\ -1 * s V_num_params_z <= 0)%Z
   | 21 => (-1 * s V_num_params_z <= 0 /\ -1 * s V_num_params__tmp + 1 <= 0)%Z
   | 22 => (-1 * s V_num_params__tmp + 1 <= 0 /\ -1 * s V_num_params_z <= 0 /\ 1 * s V_num_params__tmp1 + 17 <= 0 /\ -1 * s V_num_params__tmp1 + -17 <= 0)%Z
   | 23 => (-1 * s V_num_params__tmp1 + -17 <= 0 /\ 1 * s V_num_params__tmp1 + 17 <= 0 /\ -1 * s V_num_params_z <= 0 /\ -1 * s V_num_params__tmp + 1 <= 0)%Z
   | 24 => (-1 * s V_num_params_z <= 0 /\ -1 * s V_num_params__tmp + 1 <= 0)%Z
   | 25 => (-1 * s V_num_params__tmp + 1 <= 0 /\ -1 * s V_num_params_z <= 0)%Z
   | 26 => (-1 * s V_num_params_z <= 0 /\ -1 * s V_num_params__tmp + 1 <= 0)%Z
   | 27 => (-1 * s V_num_params__tmp + 1 <= 0 /\ -1 * s V_num_params_z <= 0)%Z
   | 28 => (-1 * s V_num_params_z <= 0 /\ -1 * s V_num_params__tmp + 1 <= 0)%Z
   | 29 => (-1 * s V_num_params__tmp + 1 <= 0 /\ -1 * s V_num_params_z <= 0)%Z
   | 30 => (-1 * s V_num_params_z <= 0 /\ -1 * s V_num_params__tmp + 1 <= 0)%Z
   | 31 => (-1 * s V_num_params__tmp + 1 <= 0 /\ -1 * s V_num_params_z <= 0 /\ 1 * s V_num_params__tmp1 + 20 <= 0 /\ -1 * s V_num_params__tmp1 + -20 <= 0)%Z
   | 32 => (-1 * s V_num_params__tmp1 + -20 <= 0 /\ 1 * s V_num_params__tmp1 + 20 <= 0 /\ -1 * s V_num_params_z <= 0 /\ -1 * s V_num_params__tmp + 1 <= 0)%Z
   | 33 => (-1 * s V_num_params_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_num_params (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_num_params_count) <= z)%Q
   | 2 => (s V_num_params_z + max0(-1 + s V_num_params_count) <= z)%Q
   | 3 => (s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 4 => (s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 5 => (s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 6 => (s V_num_params_z + max0(s V_num_params__tmp) <= z)%Q
   | 7 => (s V_num_params_z + max0(s V_num_params__tmp) <= z)%Q
   | 8 => (s V_num_params_z + max0(s V_num_params__tmp) <= z)%Q
   | 9 => (s V_num_params_z + max0(s V_num_params__tmp) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_num_params__tmp) (-1
                                                                   + 
                                                                   s V_num_params__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_num_params__tmp)]
     (s V_num_params_z + max0(s V_num_params__tmp) <= z)%Q
   | 11 => (s V_num_params_z <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_num_params__tmp) (-1
                                                                   + 
                                                                   s V_num_params__tmp));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_num_params__tmp)) (F_check_ge (0) (0))]
     (s V_num_params_z + max0(s V_num_params__tmp) <= z)%Q
   | 13 => (s V_num_params_z <= z)%Q
   | 14 => (s V_num_params_z <= z)%Q
   | 15 => (s V_num_params_z <= z)%Q
   | 16 => (s V_num_params_z <= z)%Q
   | 17 => (s V_num_params_z + max0(s V_num_params__tmp) <= z)%Q
   | 18 => (s V_num_params_z + max0(s V_num_params__tmp) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_num_params__tmp) (1)]
     (s V_num_params_z + max0(s V_num_params__tmp) <= z)%Q
   | 20 => ((1 # 1) + s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 21 => ((1 # 1) + s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 22 => (s V_num_params_z + max0(-1 + s V_num_params__tmp)
            + (1 # 3) * max0(20 + s V_num_params__tmp1) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_num_params__tmp);
      (*-0.333333 0*) F_binom_monotonic 1 (F_max0_ge_0 (20
                                                        + s V_num_params__tmp1)) (F_check_ge (0) (0))]
     (s V_num_params_z + max0(-1 + s V_num_params__tmp)
      + (1 # 3) * max0(20 + s V_num_params__tmp1) <= z)%Q
   | 24 => ((1 # 1) + s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 25 => ((1 # 1) + s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 26 => ((1 # 1) + s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 27 => ((1 # 1) + s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 28 => ((1 # 1) + s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 29 => ((1 # 1) + s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 30 => ((1 # 1) + s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 31 => ((1 # 1) + s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_one; (*-1 0*) F_max0_ge_0 (-1 + s V_num_params__tmp)]
     ((1 # 1) + s V_num_params_z + max0(-1 + s V_num_params__tmp) <= z)%Q
   | 33 => (s V_num_params_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_num_params =>
    [mkPA Q (fun n z s => ai_num_params n s /\ annot0_num_params n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_num_params (proc_start P_num_params) s1 (proc_end P_num_params) s2 ->
    (s2 V_num_params_z <= max0(-1 + s1 V_num_params_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_num_params.
Qed.
