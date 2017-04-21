Require Import pasta.Pasta.

Inductive proc: Type :=
  P_write_tables_only.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_write_tables_only_z := 1%positive.
Notation V_write_tables_only_i := 2%positive.
Notation V_write_tables_only_cinfo := 3%positive.
Definition Pedges_write_tables_only: list (edge proc) :=
  (EA 1 (AAssign V_write_tables_only_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_write_tables_only_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_write_tables_only_i) s) < (eval (ENum (4))
  s))%Z)) 30)::(EA 5 (AGuard (fun s => ((eval (EVar V_write_tables_only_i)
  s) >= (eval (ENum (4)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 ANone 28)::
  (EA 7 ANone 8)::(EA 8 (AAssign V_write_tables_only_i
  (Some (ENum (0)))) 9)::(EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_write_tables_only_i) s) < (eval (ENum (4))
  s))%Z)) 15)::(EA 11 (AGuard (fun s => ((eval (EVar V_write_tables_only_i)
  s) >= (eval (ENum (4)) s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 ANone 14)::
  (EA 14 AWeaken 29)::(EA 15 AWeaken 16)::(EA 16 ANone 18)::
  (EA 16 ANone 17)::(EA 17 AWeaken 20)::(EA 18 ANone 19)::
  (EA 19 AWeaken 20)::(EA 20 ANone 21)::(EA 20 ANone 22)::(EA 21 ANone 22)::
  (EA 22 ANone 23)::(EA 23 (AAssign V_write_tables_only_i
  (Some (EAdd (EVar V_write_tables_only_i) (ENum (1))))) 24)::
  (EA 24 ANone 25)::(EA 25 ANone 26)::(EA 26 (AAssign V_write_tables_only_z
  (Some (EAdd (ENum (1)) (EVar V_write_tables_only_z)))) 27)::
  (EA 27 AWeaken 11)::(EA 28 AWeaken 29)::(EA 30 AWeaken 31)::
  (EA 31 ANone 32)::(EA 31 ANone 33)::(EA 32 ANone 33)::(EA 33 ANone 34)::
  (EA 34 (AAssign V_write_tables_only_i
  (Some (EAdd (EVar V_write_tables_only_i) (ENum (1))))) 35)::
  (EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign V_write_tables_only_z
  (Some (EAdd (ENum (1)) (EVar V_write_tables_only_z)))) 38)::
  (EA 38 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_write_tables_only => Pedges_write_tables_only
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_write_tables_only => 29
     end)%positive;
  var_global := var_global
}.

Definition ai_write_tables_only (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_z <= 0)%Z
   | 3 => (-1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i <= 0 /\ -1 * s V_write_tables_only_i <= 0)%Z
   | 4 => (-1 * s V_write_tables_only_i <= 0 /\ 1 * s V_write_tables_only_i <= 0 /\ 1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_z <= 0)%Z
   | 5 => (-1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i <= 0 /\ 1 * s V_write_tables_only_i + -4 <= 0)%Z
   | 6 => (1 * s V_write_tables_only_i + -4 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i + 4 <= 0)%Z
   | 7 => (-1 * s V_write_tables_only_i + 4 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -4 <= 0)%Z
   | 8 => (1 * s V_write_tables_only_i + -4 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i + 4 <= 0)%Z
   | 9 => (-1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i <= 0 /\ -1 * s V_write_tables_only_i <= 0)%Z
   | 10 => (-1 * s V_write_tables_only_i <= 0 /\ 1 * s V_write_tables_only_i <= 0 /\ -1 * s V_write_tables_only_z <= 0)%Z
   | 11 => (-1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i <= 0 /\ 1 * s V_write_tables_only_i + -4 <= 0)%Z
   | 12 => (1 * s V_write_tables_only_i + -4 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i + 4 <= 0)%Z
   | 13 => (-1 * s V_write_tables_only_i + 4 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -4 <= 0)%Z
   | 14 => (1 * s V_write_tables_only_i + -4 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i + 4 <= 0)%Z
   | 15 => (-1 * s V_write_tables_only_i <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -3 <= 0)%Z
   | 16 => (1 * s V_write_tables_only_i + -3 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i <= 0)%Z
   | 17 => (-1 * s V_write_tables_only_i <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -3 <= 0)%Z
   | 18 => (-1 * s V_write_tables_only_i <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -3 <= 0)%Z
   | 19 => (1 * s V_write_tables_only_i + -3 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i <= 0)%Z
   | 20 => (-1 * s V_write_tables_only_i <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -3 <= 0)%Z
   | 21 => (1 * s V_write_tables_only_i + -3 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i <= 0)%Z
   | 22 => (-1 * s V_write_tables_only_i <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -3 <= 0)%Z
   | 23 => (1 * s V_write_tables_only_i + -3 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i <= 0)%Z
   | 24 => (-1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -4 <= 0 /\ -1 * s V_write_tables_only_i + 1 <= 0)%Z
   | 25 => (-1 * s V_write_tables_only_i + 1 <= 0 /\ 1 * s V_write_tables_only_i + -4 <= 0 /\ -1 * s V_write_tables_only_z <= 0)%Z
   | 26 => (-1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -4 <= 0 /\ -1 * s V_write_tables_only_i + 1 <= 0)%Z
   | 27 => (-1 * s V_write_tables_only_i + 1 <= 0 /\ 1 * s V_write_tables_only_i + -4 <= 0 /\ -1 * s V_write_tables_only_z + 1 <= 0)%Z
   | 28 => (1 * s V_write_tables_only_i + -4 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i + 4 <= 0)%Z
   | 29 => (-1 * s V_write_tables_only_i + 4 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -4 <= 0)%Z
   | 30 => (-1 * s V_write_tables_only_i <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -3 <= 0)%Z
   | 31 => (1 * s V_write_tables_only_i + -3 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i <= 0)%Z
   | 32 => (-1 * s V_write_tables_only_i <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -3 <= 0)%Z
   | 33 => (1 * s V_write_tables_only_i + -3 <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i <= 0)%Z
   | 34 => (-1 * s V_write_tables_only_i <= 0 /\ -1 * s V_write_tables_only_z <= 0 /\ 1 * s V_write_tables_only_i + -3 <= 0)%Z
   | 35 => (-1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i + 1 <= 0 /\ 1 * s V_write_tables_only_i + -4 <= 0)%Z
   | 36 => (1 * s V_write_tables_only_i + -4 <= 0 /\ -1 * s V_write_tables_only_i + 1 <= 0 /\ -1 * s V_write_tables_only_z <= 0)%Z
   | 37 => (-1 * s V_write_tables_only_z <= 0 /\ -1 * s V_write_tables_only_i + 1 <= 0 /\ 1 * s V_write_tables_only_i + -4 <= 0)%Z
   | 38 => (1 * s V_write_tables_only_i + -4 <= 0 /\ -1 * s V_write_tables_only_i + 1 <= 0 /\ -1 * s V_write_tables_only_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_write_tables_only (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_write_tables_only_z <= z)%Q
   | 3 => ((4 # 1) + s V_write_tables_only_z
           + max0(4 - s V_write_tables_only_i) <= z)%Q
   | 4 => ((4 # 1) + s V_write_tables_only_z
           + max0(4 - s V_write_tables_only_i) <= z)%Q
   | 5 => ((4 # 1) + s V_write_tables_only_z
           + max0(4 - s V_write_tables_only_i) <= z)%Q
   | 6 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (4 - s V_write_tables_only_i) (3
                                                                    - s V_write_tables_only_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3 - s V_write_tables_only_i)) (F_check_ge (0) (0))]
     ((4 # 1) + s V_write_tables_only_z + max0(4 - s V_write_tables_only_i) <= z)%Q
   | 7 => ((4 # 1) + s V_write_tables_only_z <= z)%Q
   | 8 => ((4 # 1) + s V_write_tables_only_z <= z)%Q
   | 9 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 10 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 11 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 12 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 13 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_write_tables_only_i) (3
                                                                    - s V_write_tables_only_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_write_tables_only_i);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                              - s V_write_tables_only_i) (0))) (F_max0_ge_0 (4
                                                                    - s V_write_tables_only_i))]
     ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 15 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 16 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 17 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 18 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 19 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 20 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 21 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 22 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 23 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 24 => ((5 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 25 => ((5 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 26 => ((5 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 27 => ((4 # 1) - s V_write_tables_only_i + s V_write_tables_only_z <= z)%Q
   | 28 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_write_tables_only_i) (3
                                                                    - s V_write_tables_only_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_write_tables_only_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_write_tables_only_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_write_tables_only_i) (0))) (F_max0_ge_0 (s V_write_tables_only_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_write_tables_only_i) (0))) (F_max0_ge_0 (4
                                                                    - s V_write_tables_only_i))]
     ((4 # 1) + s V_write_tables_only_z <= z)%Q
   | 29 => (s V_write_tables_only_z <= z)%Q
   | 30 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                   - s V_write_tables_only_i)) (F_check_ge (4
                                                                    - s V_write_tables_only_i) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                               - s V_write_tables_only_i) (0))) (F_max0_ge_0 (3
                                                                    - s V_write_tables_only_i))]
     ((4 # 1) + s V_write_tables_only_z + max0(4 - s V_write_tables_only_i) <= z)%Q
   | 31 => ((5 # 1) + s V_write_tables_only_z
            + max0(3 - s V_write_tables_only_i) <= z)%Q
   | 32 => ((5 # 1) + s V_write_tables_only_z
            + max0(3 - s V_write_tables_only_i) <= z)%Q
   | 33 => ((5 # 1) + s V_write_tables_only_z
            + max0(3 - s V_write_tables_only_i) <= z)%Q
   | 34 => ((5 # 1) + s V_write_tables_only_z
            + max0(3 - s V_write_tables_only_i) <= z)%Q
   | 35 => ((5 # 1) + s V_write_tables_only_z
            + max0(4 - s V_write_tables_only_i) <= z)%Q
   | 36 => ((5 # 1) + s V_write_tables_only_z
            + max0(4 - s V_write_tables_only_i) <= z)%Q
   | 37 => ((5 # 1) + s V_write_tables_only_z
            + max0(4 - s V_write_tables_only_i) <= z)%Q
   | 38 => ((4 # 1) + s V_write_tables_only_z
            + max0(4 - s V_write_tables_only_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_write_tables_only =>
    [mkPA Q (fun n z s => ai_write_tables_only n s /\ annot0_write_tables_only n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_write_tables_only (proc_start P_write_tables_only) s1 (proc_end P_write_tables_only) s2 ->
    (s2 V_write_tables_only_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_write_tables_only.
Qed.
