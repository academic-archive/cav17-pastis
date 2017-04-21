Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gx_install_CIEABC.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gx_install_CIEABC_z := 1%positive.
Notation V_gx_install_CIEABC_i := 2%positive.
Notation V_gx_install_CIEABC_j := 3%positive.
Notation V_gx_install_CIEABC_pcs := 4%positive.
Notation V_gx_install_CIEABC_pgs := 5%positive.
Definition Pedges_gx_install_CIEABC: list (edge proc) :=
  (EA 1 (AAssign V_gx_install_CIEABC_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_gx_install_CIEABC_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_gx_install_CIEABC_i) s) < (eval (ENum (512))
  s))%Z)) 8)::(EA 5 (AGuard (fun s => ((eval (EVar V_gx_install_CIEABC_i)
  s) >= (eval (ENum (512)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 8 AWeaken 9)::
  (EA 9 (AAssign V_gx_install_CIEABC_j (Some (ENum (0)))) 10)::
  (EA 10 ANone 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_gx_install_CIEABC_j) s) < (eval (ENum (3))
  s))%Z)) 20)::(EA 12 (AGuard (fun s => ((eval (EVar V_gx_install_CIEABC_j)
  s) >= (eval (ENum (3)) s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::
  (EA 15 (AAssign V_gx_install_CIEABC_i
  (Some (EAdd (EVar V_gx_install_CIEABC_i) (ENum (1))))) 16)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_gx_install_CIEABC_z
  (Some (EAdd (ENum (1)) (EVar V_gx_install_CIEABC_z)))) 19)::
  (EA 19 AWeaken 5)::(EA 20 AWeaken 21)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_gx_install_CIEABC_j (Some (EAdd (EVar V_gx_install_CIEABC_j)
  (ENum (1))))) 23)::(EA 23 ANone 24)::(EA 24 ANone 25)::(EA 25 (AAssign
  V_gx_install_CIEABC_z (Some (EAdd (ENum (1))
  (EVar V_gx_install_CIEABC_z)))) 26)::(EA 26 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gx_install_CIEABC => Pedges_gx_install_CIEABC
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gx_install_CIEABC => 7
     end)%positive;
  var_global := var_global
}.

Definition ai_gx_install_CIEABC (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0)%Z
   | 3 => (-1 * s V_gx_install_CIEABC_z <= 0 /\ 1 * s V_gx_install_CIEABC_z <= 0 /\ 1 * s V_gx_install_CIEABC_i <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0)%Z
   | 4 => (-1 * s V_gx_install_CIEABC_i <= 0 /\ 1 * s V_gx_install_CIEABC_i <= 0 /\ 1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0)%Z
   | 5 => (-1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0)%Z
   | 6 => (-1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_i + 512 <= 0)%Z
   | 7 => (-1 * s V_gx_install_CIEABC_i + 512 <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0)%Z
   | 8 => (-1 * s V_gx_install_CIEABC_i <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ 1 * s V_gx_install_CIEABC_i + -511 <= 0)%Z
   | 9 => (1 * s V_gx_install_CIEABC_i + -511 <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0)%Z
   | 10 => (-1 * s V_gx_install_CIEABC_i <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ 1 * s V_gx_install_CIEABC_i + -511 <= 0 /\ 1 * s V_gx_install_CIEABC_j <= 0 /\ -1 * s V_gx_install_CIEABC_j <= 0)%Z
   | 11 => (-1 * s V_gx_install_CIEABC_j <= 0 /\ 1 * s V_gx_install_CIEABC_j <= 0 /\ 1 * s V_gx_install_CIEABC_i + -511 <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0)%Z
   | 12 => (-1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_j <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0 /\ 1 * s V_gx_install_CIEABC_j + -3 <= 0)%Z
   | 13 => (1 * s V_gx_install_CIEABC_j + -3 <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_j + 3 <= 0)%Z
   | 14 => (-1 * s V_gx_install_CIEABC_j + 3 <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0 /\ 1 * s V_gx_install_CIEABC_j + -3 <= 0)%Z
   | 15 => (1 * s V_gx_install_CIEABC_j + -3 <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_j + 3 <= 0)%Z
   | 16 => (-1 * s V_gx_install_CIEABC_j + 3 <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ 1 * s V_gx_install_CIEABC_j + -3 <= 0 /\ -1 * s V_gx_install_CIEABC_i + 1 <= 0)%Z
   | 17 => (-1 * s V_gx_install_CIEABC_i + 1 <= 0 /\ 1 * s V_gx_install_CIEABC_j + -3 <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_j + 3 <= 0)%Z
   | 18 => (-1 * s V_gx_install_CIEABC_j + 3 <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ 1 * s V_gx_install_CIEABC_j + -3 <= 0 /\ -1 * s V_gx_install_CIEABC_i + 1 <= 0)%Z
   | 19 => (-1 * s V_gx_install_CIEABC_i + 1 <= 0 /\ 1 * s V_gx_install_CIEABC_j + -3 <= 0 /\ -1 * s V_gx_install_CIEABC_j + 3 <= 0 /\ -1 * s V_gx_install_CIEABC_z + 1 <= 0)%Z
   | 20 => (-1 * s V_gx_install_CIEABC_i <= 0 /\ -1 * s V_gx_install_CIEABC_j <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ 1 * s V_gx_install_CIEABC_j + -2 <= 0)%Z
   | 21 => (1 * s V_gx_install_CIEABC_j + -2 <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_j <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0)%Z
   | 22 => (-1 * s V_gx_install_CIEABC_i <= 0 /\ -1 * s V_gx_install_CIEABC_j <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0 /\ 1 * s V_gx_install_CIEABC_j + -2 <= 0)%Z
   | 23 => (-1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0 /\ -1 * s V_gx_install_CIEABC_j + 1 <= 0 /\ 1 * s V_gx_install_CIEABC_j + -3 <= 0)%Z
   | 24 => (1 * s V_gx_install_CIEABC_j + -3 <= 0 /\ -1 * s V_gx_install_CIEABC_j + 1 <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0 /\ -1 * s V_gx_install_CIEABC_z <= 0)%Z
   | 25 => (-1 * s V_gx_install_CIEABC_z <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0 /\ -1 * s V_gx_install_CIEABC_j + 1 <= 0 /\ 1 * s V_gx_install_CIEABC_j + -3 <= 0)%Z
   | 26 => (1 * s V_gx_install_CIEABC_j + -3 <= 0 /\ -1 * s V_gx_install_CIEABC_j + 1 <= 0 /\ -1 * s V_gx_install_CIEABC_i <= 0 /\ -1 * s V_gx_install_CIEABC_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gx_install_CIEABC (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((2048 # 1) <= z)%Q
   | 2 => ((2048 # 1) + s V_gx_install_CIEABC_z <= z)%Q
   | 3 => (s V_gx_install_CIEABC_z
           + (4 # 1) * max0(512 - s V_gx_install_CIEABC_i) <= z)%Q
   | 4 => (s V_gx_install_CIEABC_z
           + (4 # 1) * max0(512 - s V_gx_install_CIEABC_i) <= z)%Q
   | 5 => (s V_gx_install_CIEABC_z
           + (4 # 1) * max0(512 - s V_gx_install_CIEABC_i) <= z)%Q
   | 6 => hints
     [(*-4 0*) F_max0_monotonic (F_check_ge (512 - s V_gx_install_CIEABC_i) (511
                                                                    - s V_gx_install_CIEABC_i));
      (*-4 0*) F_max0_ge_0 (511 - s V_gx_install_CIEABC_i)]
     (s V_gx_install_CIEABC_z + (4 # 1) * max0(512 - s V_gx_install_CIEABC_i) <= z)%Q
   | 7 => (s V_gx_install_CIEABC_z <= z)%Q
   | 8 => hints
     [(*-4 0*) F_max0_pre_decrement 1 (512 - s V_gx_install_CIEABC_i) (1)]
     (s V_gx_install_CIEABC_z + (4 # 1) * max0(512 - s V_gx_install_CIEABC_i) <= z)%Q
   | 9 => ((4 # 1) + s V_gx_install_CIEABC_z
           + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 10 => ((1 # 1) + s V_gx_install_CIEABC_z
            + max0(3 - s V_gx_install_CIEABC_j)
            + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 11 => ((1 # 1) + s V_gx_install_CIEABC_z
            + max0(3 - s V_gx_install_CIEABC_j)
            + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 12 => ((1 # 1) + s V_gx_install_CIEABC_z
            + max0(3 - s V_gx_install_CIEABC_j)
            + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 13 => ((1 # 1) + s V_gx_install_CIEABC_z
            + max0(3 - s V_gx_install_CIEABC_j)
            + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 14 => ((1 # 1) + s V_gx_install_CIEABC_z
            + max0(3 - s V_gx_install_CIEABC_j)
            + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 15 => ((1 # 1) + s V_gx_install_CIEABC_z
            + max0(3 - s V_gx_install_CIEABC_j)
            + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 16 => ((1 # 1) + s V_gx_install_CIEABC_z
            + max0(3 - s V_gx_install_CIEABC_j)
            + (4 # 1) * max0(512 - s V_gx_install_CIEABC_i) <= z)%Q
   | 17 => ((1 # 1) + s V_gx_install_CIEABC_z
            + max0(3 - s V_gx_install_CIEABC_j)
            + (4 # 1) * max0(512 - s V_gx_install_CIEABC_i) <= z)%Q
   | 18 => ((1 # 1) + s V_gx_install_CIEABC_z
            + max0(3 - s V_gx_install_CIEABC_j)
            + (4 # 1) * max0(512 - s V_gx_install_CIEABC_i) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3 - s V_gx_install_CIEABC_j)) (F_check_ge (0) (0))]
     (s V_gx_install_CIEABC_z + max0(3 - s V_gx_install_CIEABC_j)
      + (4 # 1) * max0(512 - s V_gx_install_CIEABC_i) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (3
                                                   - s V_gx_install_CIEABC_j)) (F_check_ge (3
                                                                    - s V_gx_install_CIEABC_j) (0))]
     ((1 # 1) + s V_gx_install_CIEABC_z + max0(3 - s V_gx_install_CIEABC_j)
      + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 21 => ((4 # 1) - s V_gx_install_CIEABC_j + s V_gx_install_CIEABC_z
            + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 22 => ((4 # 1) - s V_gx_install_CIEABC_j + s V_gx_install_CIEABC_z
            + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 23 => ((5 # 1) - s V_gx_install_CIEABC_j + s V_gx_install_CIEABC_z
            + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 24 => ((5 # 1) - s V_gx_install_CIEABC_j + s V_gx_install_CIEABC_z
            + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 25 => ((5 # 1) - s V_gx_install_CIEABC_j + s V_gx_install_CIEABC_z
            + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                               - s V_gx_install_CIEABC_j) (0))) (F_max0_ge_0 (3
                                                                    - s V_gx_install_CIEABC_j))]
     ((4 # 1) - s V_gx_install_CIEABC_j + s V_gx_install_CIEABC_z
      + (4 # 1) * max0(511 - s V_gx_install_CIEABC_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gx_install_CIEABC =>
    [mkPA Q (fun n z s => ai_gx_install_CIEABC n s /\ annot0_gx_install_CIEABC n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gx_install_CIEABC (proc_start P_gx_install_CIEABC) s1 (proc_end P_gx_install_CIEABC) s2 ->
    (s2 V_gx_install_CIEABC_z <= (2048 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gx_install_CIEABC.
Qed.
