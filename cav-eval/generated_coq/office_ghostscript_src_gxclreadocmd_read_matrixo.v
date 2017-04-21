Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cmd_read_matrix.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cmd_read_matrix_z := 1%positive.
Notation V_cmd_read_matrix_b := 2%positive.
Notation V_cmd_read_matrix_i := 3%positive.
Notation V_cmd_read_matrix_cbp := 4%positive.
Notation V_cmd_read_matrix_pmat := 5%positive.
Definition Pedges_cmd_read_matrix: list (edge proc) :=
  (EA 1 (AAssign V_cmd_read_matrix_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_cmd_read_matrix_b None) 3)::(EA 3 (AAssign V_cmd_read_matrix_i
  (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_cmd_read_matrix_i) s) < (eval (ENum (4))
  s))%Z)) 24)::(EA 6 (AGuard (fun s => ((eval (EVar V_cmd_read_matrix_i)
  s) >= (eval (ENum (4)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_cmd_read_matrix_i) s) < (eval (ENum (6))
  s))%Z)) 13)::(EA 10 (AGuard (fun s => ((eval (EVar V_cmd_read_matrix_i)
  s) >= (eval (ENum (6)) s))%Z)) 11)::(EA 11 AWeaken 12)::
  (EA 13 AWeaken 14)::(EA 14 ANone 16)::(EA 14 ANone 15)::(EA 15 ANone 17)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_cmd_read_matrix_i
  (Some (EAdd (EVar V_cmd_read_matrix_i) (ENum (1))))) 19)::(EA 19 (AAssign
  V_cmd_read_matrix_b None) 20)::(EA 20 ANone 21)::(EA 21 ANone 22)::
  (EA 22 (AAssign V_cmd_read_matrix_z (Some (EAdd (ENum (1))
  (EVar V_cmd_read_matrix_z)))) 23)::(EA 23 AWeaken 10)::(EA 24 AWeaken 25)::
  (EA 25 ANone 27)::(EA 25 ANone 26)::(EA 26 ANone 33)::(EA 27 AWeaken 28)::
  (EA 28 ANone 32)::(EA 28 ANone 31)::(EA 28 ANone 30)::(EA 28 ANone 29)::
  (EA 29 ANone 32)::(EA 30 ANone 32)::(EA 31 ANone 32)::(EA 32 ANone 33)::
  (EA 33 ANone 34)::(EA 34 (AAssign V_cmd_read_matrix_i
  (Some (EAdd (EVar V_cmd_read_matrix_i) (ENum (2))))) 35)::(EA 35 (AAssign
  V_cmd_read_matrix_b None) 36)::(EA 36 ANone 37)::(EA 37 ANone 38)::
  (EA 38 (AAssign V_cmd_read_matrix_z (Some (EAdd (ENum (1))
  (EVar V_cmd_read_matrix_z)))) 39)::(EA 39 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cmd_read_matrix => Pedges_cmd_read_matrix
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cmd_read_matrix => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_cmd_read_matrix (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0)%Z
   | 3 => (-1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_z <= 0)%Z
   | 4 => (1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i <= 0 /\ -1 * s V_cmd_read_matrix_i <= 0)%Z
   | 5 => (-1 * s V_cmd_read_matrix_i <= 0 /\ 1 * s V_cmd_read_matrix_i <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_z <= 0)%Z
   | 6 => (-1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i <= 0 /\ 1 * s V_cmd_read_matrix_i + -5 <= 0)%Z
   | 7 => (1 * s V_cmd_read_matrix_i + -5 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i + 4 <= 0)%Z
   | 8 => (-1 * s V_cmd_read_matrix_i + 4 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i + -5 <= 0)%Z
   | 9 => (1 * s V_cmd_read_matrix_i + -5 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i + 4 <= 0)%Z
   | 10 => (-1 * s V_cmd_read_matrix_i + 4 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i + -6 <= 0)%Z
   | 11 => (1 * s V_cmd_read_matrix_i + -6 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i + 6 <= 0)%Z
   | 12 => (-1 * s V_cmd_read_matrix_i + 6 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i + -6 <= 0)%Z
   | 13 => (-1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i + 4 <= 0 /\ 1 * s V_cmd_read_matrix_i + -5 <= 0)%Z
   | 14 => (1 * s V_cmd_read_matrix_i + -5 <= 0 /\ -1 * s V_cmd_read_matrix_i + 4 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0)%Z
   | 15 => (-1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i + 4 <= 0 /\ 1 * s V_cmd_read_matrix_i + -5 <= 0)%Z
   | 16 => (-1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i + 4 <= 0 /\ 1 * s V_cmd_read_matrix_i + -5 <= 0)%Z
   | 17 => (1 * s V_cmd_read_matrix_i + -5 <= 0 /\ -1 * s V_cmd_read_matrix_i + 4 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0)%Z
   | 18 => (-1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i + 4 <= 0 /\ 1 * s V_cmd_read_matrix_i + -5 <= 0)%Z
   | 19 => (-1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i + 5 <= 0 /\ 1 * s V_cmd_read_matrix_i + -6 <= 0)%Z
   | 20 => (1 * s V_cmd_read_matrix_i + -6 <= 0 /\ -1 * s V_cmd_read_matrix_i + 5 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0)%Z
   | 21 => (-1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i + 5 <= 0 /\ 1 * s V_cmd_read_matrix_i + -6 <= 0)%Z
   | 22 => (1 * s V_cmd_read_matrix_i + -6 <= 0 /\ -1 * s V_cmd_read_matrix_i + 5 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0)%Z
   | 23 => (-1 * s V_cmd_read_matrix_i + 5 <= 0 /\ 1 * s V_cmd_read_matrix_i + -6 <= 0 /\ -1 * s V_cmd_read_matrix_z + 1 <= 0)%Z
   | 24 => (-1 * s V_cmd_read_matrix_i <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i + -3 <= 0)%Z
   | 25 => (1 * s V_cmd_read_matrix_i + -3 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i <= 0)%Z
   | 26 => (-1 * s V_cmd_read_matrix_i <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i + -3 <= 0)%Z
   | 27 => (-1 * s V_cmd_read_matrix_i <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i + -3 <= 0)%Z
   | 28 => (1 * s V_cmd_read_matrix_i + -3 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i <= 0)%Z
   | 29 => (-1 * s V_cmd_read_matrix_i <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i + -3 <= 0)%Z
   | 30 => (-1 * s V_cmd_read_matrix_i <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i + -3 <= 0)%Z
   | 31 => (-1 * s V_cmd_read_matrix_i <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i + -3 <= 0)%Z
   | 32 => (1 * s V_cmd_read_matrix_i + -3 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i <= 0)%Z
   | 33 => (-1 * s V_cmd_read_matrix_i <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i + -3 <= 0)%Z
   | 34 => (1 * s V_cmd_read_matrix_i + -3 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0 /\ -1 * s V_cmd_read_matrix_i <= 0)%Z
   | 35 => (-1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i + -5 <= 0 /\ -1 * s V_cmd_read_matrix_i + 2 <= 0)%Z
   | 36 => (-1 * s V_cmd_read_matrix_i + 2 <= 0 /\ 1 * s V_cmd_read_matrix_i + -5 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0)%Z
   | 37 => (-1 * s V_cmd_read_matrix_z <= 0 /\ 1 * s V_cmd_read_matrix_i + -5 <= 0 /\ -1 * s V_cmd_read_matrix_i + 2 <= 0)%Z
   | 38 => (-1 * s V_cmd_read_matrix_i + 2 <= 0 /\ 1 * s V_cmd_read_matrix_i + -5 <= 0 /\ -1 * s V_cmd_read_matrix_z <= 0)%Z
   | 39 => (1 * s V_cmd_read_matrix_i + -5 <= 0 /\ -1 * s V_cmd_read_matrix_i + 2 <= 0 /\ -1 * s V_cmd_read_matrix_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cmd_read_matrix (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_cmd_read_matrix_z <= z)%Q
   | 3 => ((4 # 1) + s V_cmd_read_matrix_z <= z)%Q
   | 4 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 5 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 6 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 7 => hints
     [(*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-4
                                                                + s V_cmd_read_matrix_i) (0))) (F_max0_ge_0 (-4
                                                                    + s V_cmd_read_matrix_i))]
     ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 8 => ((6 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z
           + (1 # 2) * max0(-4 + s V_cmd_read_matrix_i) <= z)%Q
   | 9 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_0 (-4 + s V_cmd_read_matrix_i)) (F_check_ge (0) (0))]
     ((6 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z
      + (1 # 2) * max0(-4 + s V_cmd_read_matrix_i) <= z)%Q
   | 10 => ((6 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (6 - s V_cmd_read_matrix_i) (4
                                                                    - s V_cmd_read_matrix_i));
      (*-1 0*) F_max0_ge_0 (4 - s V_cmd_read_matrix_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (6
                                                               - s V_cmd_read_matrix_i) (0))) (F_max0_ge_0 (6
                                                                    - s V_cmd_read_matrix_i))]
     ((6 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 12 => (s V_cmd_read_matrix_z <= z)%Q
   | 13 => ((6 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 14 => ((6 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 15 => ((6 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 16 => ((6 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 17 => ((6 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 18 => ((6 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 19 => ((7 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 20 => ((7 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 21 => ((7 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 22 => ((7 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 23 => ((6 # 1) - s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 24 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 25 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 26 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 27 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 28 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 29 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 30 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 31 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 32 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 33 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 34 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 35 => ((5 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 36 => ((5 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 37 => ((5 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 38 => ((5 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | 39 => ((4 # 1) - (1 # 2) * s V_cmd_read_matrix_i + s V_cmd_read_matrix_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cmd_read_matrix =>
    [mkPA Q (fun n z s => ai_cmd_read_matrix n s /\ annot0_cmd_read_matrix n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cmd_read_matrix (proc_start P_cmd_read_matrix) s1 (proc_end P_cmd_read_matrix) s2 ->
    (s2 V_cmd_read_matrix_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cmd_read_matrix.
Qed.
