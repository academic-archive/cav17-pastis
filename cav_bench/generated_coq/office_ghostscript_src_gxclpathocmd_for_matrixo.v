Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cmd_for_matrix.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cmd_for_matrix_z := 1%positive.
Notation V_cmd_for_matrix_b := 2%positive.
Notation V_cmd_for_matrix_i := 3%positive.
Notation V_cmd_for_matrix_cbuf := 4%positive.
Notation V_cmd_for_matrix_pmat := 5%positive.
Definition Pedges_cmd_for_matrix: list (edge proc) :=
  (EA 1 (AAssign V_cmd_for_matrix_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_cmd_for_matrix_b (Some (ENum (0)))) 3)::(EA 3 (AAssign V_cmd_for_matrix_i
  (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_cmd_for_matrix_i) s) < (eval (ENum (4))
  s))%Z)) 25)::(EA 6 (AGuard (fun s => ((eval (EVar V_cmd_for_matrix_i) s) >=
  (eval (ENum (4)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_cmd_for_matrix_i) s) < (eval (ENum (6))
  s))%Z)) 13)::(EA 10 (AGuard (fun s => ((eval (EVar V_cmd_for_matrix_i)
  s) >= (eval (ENum (6)) s))%Z)) 11)::(EA 11 AWeaken 12)::
  (EA 13 AWeaken 14)::(EA 14 (AAssign V_cmd_for_matrix_b None) 15)::
  (EA 15 AWeaken 16)::(EA 16 ANone 17)::(EA 16 ANone 19)::(EA 17 (AAssign
  V_cmd_for_matrix_b (Some (EAdd (EVar V_cmd_for_matrix_b)
  (ENum (1))))) 18)::(EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_cmd_for_matrix_i (Some (EAdd (EVar V_cmd_for_matrix_i)
  (ENum (1))))) 21)::(EA 21 ANone 22)::(EA 22 ANone 23)::(EA 23 (AAssign
  V_cmd_for_matrix_z (Some (EAdd (ENum (1))
  (EVar V_cmd_for_matrix_z)))) 24)::(EA 24 AWeaken 10)::(EA 25 AWeaken 26)::
  (EA 26 (AAssign V_cmd_for_matrix_b None) 27)::(EA 27 AWeaken 28)::
  (EA 28 ANone 32)::(EA 28 ANone 29)::(EA 29 AWeaken 30)::(EA 30 ANone 31)::
  (EA 30 ANone 44)::(EA 31 AWeaken 33)::(EA 32 AWeaken 33)::
  (EA 33 ANone 41)::(EA 33 ANone 34)::(EA 34 AWeaken 35)::(EA 35 ANone 38)::
  (EA 35 ANone 36)::(EA 36 (AAssign V_cmd_for_matrix_b
  (Some (EAdd (EVar V_cmd_for_matrix_b) (ENum (3))))) 37)::(EA 37 ANone 40)::
  (EA 38 (AAssign V_cmd_for_matrix_b (Some (EAdd (EVar V_cmd_for_matrix_b)
  (ENum (2))))) 39)::(EA 39 ANone 40)::(EA 40 ANone 43)::(EA 41 (AAssign
  V_cmd_for_matrix_b (Some (EAdd (EVar V_cmd_for_matrix_b)
  (ENum (1))))) 42)::(EA 42 ANone 43)::(EA 43 ANone 44)::(EA 44 ANone 45)::
  (EA 45 (AAssign V_cmd_for_matrix_i (Some (EAdd (EVar V_cmd_for_matrix_i)
  (ENum (2))))) 46)::(EA 46 ANone 47)::(EA 47 ANone 48)::(EA 48 (AAssign
  V_cmd_for_matrix_z (Some (EAdd (ENum (1))
  (EVar V_cmd_for_matrix_z)))) 49)::(EA 49 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cmd_for_matrix => Pedges_cmd_for_matrix
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cmd_for_matrix => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_cmd_for_matrix (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0)%Z
   | 3 => (-1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_b <= 0 /\ -1 * s V_cmd_for_matrix_b <= 0)%Z
   | 4 => (-1 * s V_cmd_for_matrix_b <= 0 /\ 1 * s V_cmd_for_matrix_b <= 0 /\ 1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_i <= 0)%Z
   | 5 => (-1 * s V_cmd_for_matrix_i <= 0 /\ 1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_b <= 0 /\ -1 * s V_cmd_for_matrix_b <= 0)%Z
   | 6 => (-1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i <= 0 /\ 1 * s V_cmd_for_matrix_i + -5 <= 0)%Z
   | 7 => (1 * s V_cmd_for_matrix_i + -5 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i + 4 <= 0)%Z
   | 8 => (-1 * s V_cmd_for_matrix_i + 4 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -5 <= 0)%Z
   | 9 => (1 * s V_cmd_for_matrix_i + -5 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i + 4 <= 0)%Z
   | 10 => (-1 * s V_cmd_for_matrix_i + 4 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -6 <= 0)%Z
   | 11 => (1 * s V_cmd_for_matrix_i + -6 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i + 6 <= 0)%Z
   | 12 => (-1 * s V_cmd_for_matrix_i + 6 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -6 <= 0)%Z
   | 13 => (-1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i + 4 <= 0 /\ 1 * s V_cmd_for_matrix_i + -5 <= 0)%Z
   | 14 => (1 * s V_cmd_for_matrix_i + -5 <= 0 /\ -1 * s V_cmd_for_matrix_i + 4 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0)%Z
   | 15 => (-1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i + 4 <= 0 /\ 1 * s V_cmd_for_matrix_i + -5 <= 0)%Z
   | 16 => (1 * s V_cmd_for_matrix_i + -5 <= 0 /\ -1 * s V_cmd_for_matrix_i + 4 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0)%Z
   | 17 => (-1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i + 4 <= 0 /\ 1 * s V_cmd_for_matrix_i + -5 <= 0)%Z
   | 18 => (1 * s V_cmd_for_matrix_i + -5 <= 0 /\ -1 * s V_cmd_for_matrix_i + 4 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0)%Z
   | 19 => (-1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i + 4 <= 0 /\ 1 * s V_cmd_for_matrix_i + -5 <= 0)%Z
   | 20 => (1 * s V_cmd_for_matrix_i + -5 <= 0 /\ -1 * s V_cmd_for_matrix_i + 4 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0)%Z
   | 21 => (-1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -6 <= 0 /\ -1 * s V_cmd_for_matrix_i + 5 <= 0)%Z
   | 22 => (-1 * s V_cmd_for_matrix_i + 5 <= 0 /\ 1 * s V_cmd_for_matrix_i + -6 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0)%Z
   | 23 => (-1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -6 <= 0 /\ -1 * s V_cmd_for_matrix_i + 5 <= 0)%Z
   | 24 => (-1 * s V_cmd_for_matrix_i + 5 <= 0 /\ 1 * s V_cmd_for_matrix_i + -6 <= 0 /\ -1 * s V_cmd_for_matrix_z + 1 <= 0)%Z
   | 25 => (-1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -3 <= 0)%Z
   | 26 => (1 * s V_cmd_for_matrix_i + -3 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i <= 0)%Z
   | 27 => (-1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -3 <= 0)%Z
   | 28 => (1 * s V_cmd_for_matrix_i + -3 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i <= 0)%Z
   | 29 => (-1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -3 <= 0)%Z
   | 30 => (1 * s V_cmd_for_matrix_i + -3 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i <= 0)%Z
   | 31 => (-1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -3 <= 0)%Z
   | 32 => (-1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -3 <= 0)%Z
   | 33 => (1 * s V_cmd_for_matrix_i + -3 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i <= 0)%Z
   | 34 => (-1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -3 <= 0)%Z
   | 35 => (1 * s V_cmd_for_matrix_i + -3 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i <= 0)%Z
   | 36 => (-1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -3 <= 0)%Z
   | 37 => (1 * s V_cmd_for_matrix_i + -3 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i <= 0)%Z
   | 38 => (-1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -3 <= 0)%Z
   | 39 => (1 * s V_cmd_for_matrix_i + -3 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i <= 0)%Z
   | 40 => (-1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -3 <= 0)%Z
   | 41 => (-1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -3 <= 0)%Z
   | 42 => (1 * s V_cmd_for_matrix_i + -3 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i <= 0)%Z
   | 43 => (-1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -3 <= 0)%Z
   | 44 => (1 * s V_cmd_for_matrix_i + -3 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i <= 0)%Z
   | 45 => (-1 * s V_cmd_for_matrix_i <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0 /\ 1 * s V_cmd_for_matrix_i + -3 <= 0)%Z
   | 46 => (-1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i + 2 <= 0 /\ 1 * s V_cmd_for_matrix_i + -5 <= 0)%Z
   | 47 => (1 * s V_cmd_for_matrix_i + -5 <= 0 /\ -1 * s V_cmd_for_matrix_i + 2 <= 0 /\ -1 * s V_cmd_for_matrix_z <= 0)%Z
   | 48 => (-1 * s V_cmd_for_matrix_z <= 0 /\ -1 * s V_cmd_for_matrix_i + 2 <= 0 /\ 1 * s V_cmd_for_matrix_i + -5 <= 0)%Z
   | 49 => (1 * s V_cmd_for_matrix_i + -5 <= 0 /\ -1 * s V_cmd_for_matrix_i + 2 <= 0 /\ -1 * s V_cmd_for_matrix_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cmd_for_matrix (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_cmd_for_matrix_z <= z)%Q
   | 3 => ((4 # 1) + s V_cmd_for_matrix_z <= z)%Q
   | 4 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 5 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 6 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 7 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (6
                                                              - s V_cmd_for_matrix_i) (0))) (F_max0_ge_0 (6
                                                                    - s V_cmd_for_matrix_i))]
     ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 8 => (-(2 # 1) + (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z
           + max0(6 - s V_cmd_for_matrix_i) <= z)%Q
   | 9 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (-4 + s V_cmd_for_matrix_i)) (F_check_ge (0) (0));
      (*2.04176e-13 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                    + s V_cmd_for_matrix_i) (0))) (F_max0_ge_0 (-4
                                                                    + s V_cmd_for_matrix_i))]
     (-(2 # 1) + (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z
      + max0(6 - s V_cmd_for_matrix_i) <= z)%Q
   | 10 => (s V_cmd_for_matrix_z + max0(6 - s V_cmd_for_matrix_i) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (6 - s V_cmd_for_matrix_i) (4
                                                                    - s V_cmd_for_matrix_i));
      (*-1 0*) F_max0_ge_0 (4 - s V_cmd_for_matrix_i)]
     (s V_cmd_for_matrix_z + max0(6 - s V_cmd_for_matrix_i) <= z)%Q
   | 12 => (s V_cmd_for_matrix_z <= z)%Q
   | 13 => hints
     [(*0 1*) F_max0_pre_decrement 1 (6 - s V_cmd_for_matrix_i) (1)]
     (s V_cmd_for_matrix_z + max0(6 - s V_cmd_for_matrix_i) <= z)%Q
   | 14 => ((1 # 1) + s V_cmd_for_matrix_z + max0(5 - s V_cmd_for_matrix_i) <= z)%Q
   | 15 => ((1 # 1) + s V_cmd_for_matrix_z + max0(5 - s V_cmd_for_matrix_i) <= z)%Q
   | 16 => ((1 # 1) + s V_cmd_for_matrix_z + max0(5 - s V_cmd_for_matrix_i) <= z)%Q
   | 17 => ((1 # 1) + s V_cmd_for_matrix_z + max0(5 - s V_cmd_for_matrix_i) <= z)%Q
   | 18 => ((1 # 1) + s V_cmd_for_matrix_z + max0(5 - s V_cmd_for_matrix_i) <= z)%Q
   | 19 => ((1 # 1) + s V_cmd_for_matrix_z + max0(5 - s V_cmd_for_matrix_i) <= z)%Q
   | 20 => ((1 # 1) + s V_cmd_for_matrix_z + max0(5 - s V_cmd_for_matrix_i) <= z)%Q
   | 21 => ((1 # 1) + s V_cmd_for_matrix_z + max0(6 - s V_cmd_for_matrix_i) <= z)%Q
   | 22 => ((1 # 1) + s V_cmd_for_matrix_z + max0(6 - s V_cmd_for_matrix_i) <= z)%Q
   | 23 => ((1 # 1) + s V_cmd_for_matrix_z + max0(6 - s V_cmd_for_matrix_i) <= z)%Q
   | 24 => (s V_cmd_for_matrix_z + max0(6 - s V_cmd_for_matrix_i) <= z)%Q
   | 25 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 26 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 27 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 28 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 29 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 30 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 31 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 32 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 33 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 34 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 35 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 36 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 37 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 38 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 39 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 40 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 41 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 42 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 43 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 44 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 45 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 46 => ((5 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 47 => ((5 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 48 => ((5 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | 49 => ((4 # 1) - (1 # 2) * s V_cmd_for_matrix_i + s V_cmd_for_matrix_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cmd_for_matrix =>
    [mkPA Q (fun n z s => ai_cmd_for_matrix n s /\ annot0_cmd_for_matrix n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cmd_for_matrix (proc_start P_cmd_for_matrix) s1 (proc_end P_cmd_for_matrix) s2 ->
    (s2 V_cmd_for_matrix_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cmd_for_matrix.
Qed.
