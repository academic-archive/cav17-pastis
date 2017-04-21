Require Import pasta.Pasta.

Inductive proc: Type :=
  P_write_matrix.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_write_matrix_z := 1%positive.
Notation V_write_matrix__tmp := 2%positive.
Notation V_write_matrix_i := 3%positive.
Notation V_write_matrix_op := 4%positive.
Notation V_write_matrix_pmat := 5%positive.
Definition Pedges_write_matrix: list (edge proc) :=
  (EA 1 (AAssign V_write_matrix_z (Some (ENum (0)))) 2)::(EA 2 AWeaken 3)::
  (EA 3 ANone 12)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::(EA 5 ANone 10)::
  (EA 5 ANone 6)::(EA 6 ANone 7)::(EA 7 (AAssign V_write_matrix__tmp
  None) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 35)::(EA 10 ANone 11)::
  (EA 11 AWeaken 35)::(EA 12 AWeaken 13)::(EA 13 ANone 32)::
  (EA 13 ANone 14)::(EA 14 (AAssign V_write_matrix_i (Some (ENum (5)))) 15)::
  (EA 15 ANone 16)::(EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_write_matrix_i) s) >= (eval (ENum (0))
  s))%Z)) 22)::(EA 17 (AGuard (fun s => ((eval (EVar V_write_matrix_i) s) <
  (eval (ENum (0)) s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 (AAssign
  V_write_matrix__tmp (Some (ENum (0)))) 20)::(EA 20 ANone 21)::
  (EA 21 AWeaken 35)::(EA 22 AWeaken 23)::(EA 23 ANone 25)::
  (EA 23 ANone 24)::(EA 24 ANone 26)::(EA 25 ANone 26)::(EA 26 ANone 27)::
  (EA 27 (AAssign V_write_matrix_i (Some (EAdd (EVar V_write_matrix_i)
  (ENum (-1))))) 28)::(EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign
  V_write_matrix_z (Some (EAdd (ENum (1)) (EVar V_write_matrix_z)))) 31)::
  (EA 31 AWeaken 17)::(EA 32 (AAssign V_write_matrix__tmp
  (Some (ENum (-15)))) 33)::(EA 33 ANone 34)::(EA 34 AWeaken 35)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_write_matrix => Pedges_write_matrix
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_write_matrix => 35
     end)%positive;
  var_global := var_global
}.

Definition ai_write_matrix (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_z <= 0)%Z
   | 3 => (-1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_z <= 0)%Z
   | 4 => (1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_z <= 0)%Z
   | 5 => (-1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_z <= 0)%Z
   | 6 => (1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_z <= 0)%Z
   | 7 => (-1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_z <= 0)%Z
   | 8 => (1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_z <= 0)%Z
   | 9 => (-1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_z <= 0)%Z
   | 10 => (1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_z <= 0)%Z
   | 11 => (-1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_z <= 0)%Z
   | 12 => (1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_z <= 0)%Z
   | 13 => (-1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_z <= 0)%Z
   | 14 => (1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_z <= 0)%Z
   | 15 => (-1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_i + -5 <= 0 /\ -1 * s V_write_matrix_i + 5 <= 0)%Z
   | 16 => (-1 * s V_write_matrix_i + 5 <= 0 /\ 1 * s V_write_matrix_i + -5 <= 0 /\ 1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_z <= 0)%Z
   | 17 => (-1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_i + -5 <= 0 /\ -1 * s V_write_matrix_i + -1 <= 0)%Z
   | 18 => (-1 * s V_write_matrix_i + -1 <= 0 /\ -1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_i + 1 <= 0)%Z
   | 19 => (1 * s V_write_matrix_i + 1 <= 0 /\ -1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_i + -1 <= 0)%Z
   | 20 => (-1 * s V_write_matrix_i + -1 <= 0 /\ -1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_i + 1 <= 0 /\ 1 * s V_write_matrix__tmp <= 0 /\ -1 * s V_write_matrix__tmp <= 0)%Z
   | 21 => (-1 * s V_write_matrix__tmp <= 0 /\ 1 * s V_write_matrix__tmp <= 0 /\ 1 * s V_write_matrix_i + 1 <= 0 /\ -1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_i + -1 <= 0)%Z
   | 22 => (1 * s V_write_matrix_i + -5 <= 0 /\ -1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_i <= 0)%Z
   | 23 => (-1 * s V_write_matrix_i <= 0 /\ -1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_i + -5 <= 0)%Z
   | 24 => (1 * s V_write_matrix_i + -5 <= 0 /\ -1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_i <= 0)%Z
   | 25 => (1 * s V_write_matrix_i + -5 <= 0 /\ -1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_i <= 0)%Z
   | 26 => (-1 * s V_write_matrix_i <= 0 /\ -1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_i + -5 <= 0)%Z
   | 27 => (1 * s V_write_matrix_i + -5 <= 0 /\ -1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_i <= 0)%Z
   | 28 => (-1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_i + -4 <= 0 /\ -1 * s V_write_matrix_i + -1 <= 0)%Z
   | 29 => (-1 * s V_write_matrix_i + -1 <= 0 /\ 1 * s V_write_matrix_i + -4 <= 0 /\ -1 * s V_write_matrix_z <= 0)%Z
   | 30 => (-1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_i + -4 <= 0 /\ -1 * s V_write_matrix_i + -1 <= 0)%Z
   | 31 => (-1 * s V_write_matrix_i + -1 <= 0 /\ 1 * s V_write_matrix_i + -4 <= 0 /\ -1 * s V_write_matrix_z + 1 <= 0)%Z
   | 32 => (1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_z <= 0)%Z
   | 33 => (-1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix_z <= 0 /\ 1 * s V_write_matrix__tmp + 15 <= 0 /\ -1 * s V_write_matrix__tmp + -15 <= 0)%Z
   | 34 => (-1 * s V_write_matrix__tmp + -15 <= 0 /\ 1 * s V_write_matrix__tmp + 15 <= 0 /\ 1 * s V_write_matrix_z <= 0 /\ -1 * s V_write_matrix_z <= 0)%Z
   | 35 => (-1 * s V_write_matrix_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_write_matrix (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((6 # 1) <= z)%Q
   | 2 => ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 3 => ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 4 => ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 5 => ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 6 => ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 7 => ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 8 => ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 9 => hints
     [(*-6 0*) F_one]
     ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 10 => ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 11 => hints
     [(*-6 0*) F_one]
     ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 12 => ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 13 => ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 14 => ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 15 => (s V_write_matrix_z + max0(1 + s V_write_matrix_i) <= z)%Q
   | 16 => (s V_write_matrix_z + max0(1 + s V_write_matrix_i) <= z)%Q
   | 17 => (s V_write_matrix_z + max0(1 + s V_write_matrix_i) <= z)%Q
   | 18 => (s V_write_matrix_z + max0(1 + s V_write_matrix_i) <= z)%Q
   | 19 => (s V_write_matrix_z + max0(1 + s V_write_matrix_i) <= z)%Q
   | 20 => (s V_write_matrix_z + max0(1 + s V_write_matrix_i) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_write_matrix_i) (s V_write_matrix_i));
      (*-1 0*) F_max0_ge_0 (s V_write_matrix_i)]
     (s V_write_matrix_z + max0(1 + s V_write_matrix_i) <= z)%Q
   | 22 => hints
     [(*0 1*) F_max0_pre_decrement 1 (1 + s V_write_matrix_i) (1)]
     (s V_write_matrix_z + max0(1 + s V_write_matrix_i) <= z)%Q
   | 23 => ((1 # 1) + s V_write_matrix_z + max0(s V_write_matrix_i) <= z)%Q
   | 24 => ((1 # 1) + s V_write_matrix_z + max0(s V_write_matrix_i) <= z)%Q
   | 25 => ((1 # 1) + s V_write_matrix_z + max0(s V_write_matrix_i) <= z)%Q
   | 26 => ((1 # 1) + s V_write_matrix_z + max0(s V_write_matrix_i) <= z)%Q
   | 27 => ((1 # 1) + s V_write_matrix_z + max0(s V_write_matrix_i) <= z)%Q
   | 28 => ((1 # 1) + s V_write_matrix_z + max0(1 + s V_write_matrix_i) <= z)%Q
   | 29 => ((1 # 1) + s V_write_matrix_z + max0(1 + s V_write_matrix_i) <= z)%Q
   | 30 => ((1 # 1) + s V_write_matrix_z + max0(1 + s V_write_matrix_i) <= z)%Q
   | 31 => (s V_write_matrix_z + max0(1 + s V_write_matrix_i) <= z)%Q
   | 32 => ((6 # 1) + s V_write_matrix_z <= z)%Q
   | 33 => (s V_write_matrix_z + (2 # 5) * max0(-s V_write_matrix__tmp) <= z)%Q
   | 34 => hints
     [(*-0.4 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_write_matrix__tmp)) (F_check_ge (0) (0))]
     (s V_write_matrix_z + (2 # 5) * max0(-s V_write_matrix__tmp) <= z)%Q
   | 35 => (s V_write_matrix_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_write_matrix =>
    [mkPA Q (fun n z s => ai_write_matrix n s /\ annot0_write_matrix n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_write_matrix (proc_start P_write_matrix) s1 (proc_end P_write_matrix) s2 ->
    (s2 V_write_matrix_z <= (6 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_write_matrix.
Qed.
