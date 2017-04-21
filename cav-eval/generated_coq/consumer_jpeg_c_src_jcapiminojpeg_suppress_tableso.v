Require Import pasta.Pasta.

Inductive proc: Type :=
  P_jpeg_suppress_tables.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_jpeg_suppress_tables_z := 1%positive.
Notation V_jpeg_suppress_tables__tmp := 2%positive.
Notation V_jpeg_suppress_tables_i := 3%positive.
Notation V_jpeg_suppress_tables_cinfo := 4%positive.
Notation V_jpeg_suppress_tables_suppress := 5%positive.
Definition Pedges_jpeg_suppress_tables: list (edge proc) :=
  (EA 1 (AAssign V_jpeg_suppress_tables_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_jpeg_suppress_tables__tmp
  (Some (EVar V_jpeg_suppress_tables_suppress))) 3)::(EA 3 (AAssign
  V_jpeg_suppress_tables_i (Some (ENum (0)))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_jpeg_suppress_tables_i) s) < (eval (ENum (4))
  s))%Z)) 27)::(EA 6 (AGuard (fun s => ((eval (EVar V_jpeg_suppress_tables_i)
  s) >= (eval (ENum (4)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_jpeg_suppress_tables_i (Some (ENum (0)))) 9)::(EA 9 ANone 10)::
  (EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_jpeg_suppress_tables_i) s) < (eval (ENum (4))
  s))%Z)) 14)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_jpeg_suppress_tables_i) s) >= (eval (ENum (4))
  s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 14 AWeaken 15)::(EA 15 ANone 17)::
  (EA 15 ANone 16)::(EA 16 AWeaken 19)::(EA 17 ANone 18)::
  (EA 18 AWeaken 19)::(EA 19 ANone 20)::(EA 19 ANone 21)::(EA 20 ANone 21)::
  (EA 21 ANone 22)::(EA 22 (AAssign V_jpeg_suppress_tables_i
  (Some (EAdd (EVar V_jpeg_suppress_tables_i) (ENum (1))))) 23)::
  (EA 23 ANone 24)::(EA 24 ANone 25)::(EA 25 (AAssign
  V_jpeg_suppress_tables_z (Some (EAdd (ENum (1))
  (EVar V_jpeg_suppress_tables_z)))) 26)::(EA 26 AWeaken 11)::
  (EA 27 AWeaken 28)::(EA 28 ANone 29)::(EA 28 ANone 30)::(EA 29 ANone 30)::
  (EA 30 ANone 31)::(EA 31 (AAssign V_jpeg_suppress_tables_i
  (Some (EAdd (EVar V_jpeg_suppress_tables_i) (ENum (1))))) 32)::
  (EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_jpeg_suppress_tables_z (Some (EAdd (ENum (1))
  (EVar V_jpeg_suppress_tables_z)))) 35)::(EA 35 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_jpeg_suppress_tables => Pedges_jpeg_suppress_tables
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_jpeg_suppress_tables => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_jpeg_suppress_tables (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0)%Z
   | 3 => (-1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_z <= 0)%Z
   | 4 => (1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i <= 0 /\ -1 * s V_jpeg_suppress_tables_i <= 0)%Z
   | 5 => (-1 * s V_jpeg_suppress_tables_i <= 0 /\ 1 * s V_jpeg_suppress_tables_i <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_z <= 0)%Z
   | 6 => (-1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_i <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -4 <= 0)%Z
   | 7 => (1 * s V_jpeg_suppress_tables_i + -4 <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_i + 4 <= 0)%Z
   | 8 => (-1 * s V_jpeg_suppress_tables_i + 4 <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -4 <= 0)%Z
   | 9 => (-1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i <= 0 /\ -1 * s V_jpeg_suppress_tables_i <= 0)%Z
   | 10 => (-1 * s V_jpeg_suppress_tables_i <= 0 /\ 1 * s V_jpeg_suppress_tables_i <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0)%Z
   | 11 => (-1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_i <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -4 <= 0)%Z
   | 12 => (1 * s V_jpeg_suppress_tables_i + -4 <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_i + 4 <= 0)%Z
   | 13 => (-1 * s V_jpeg_suppress_tables_i + 4 <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -4 <= 0)%Z
   | 14 => (-1 * s V_jpeg_suppress_tables_i <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -3 <= 0)%Z
   | 15 => (1 * s V_jpeg_suppress_tables_i + -3 <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_i <= 0)%Z
   | 16 => (-1 * s V_jpeg_suppress_tables_i <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -3 <= 0)%Z
   | 17 => (-1 * s V_jpeg_suppress_tables_i <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -3 <= 0)%Z
   | 18 => (1 * s V_jpeg_suppress_tables_i + -3 <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_i <= 0)%Z
   | 19 => (-1 * s V_jpeg_suppress_tables_i <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -3 <= 0)%Z
   | 20 => (1 * s V_jpeg_suppress_tables_i + -3 <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_i <= 0)%Z
   | 21 => (-1 * s V_jpeg_suppress_tables_i <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -3 <= 0)%Z
   | 22 => (1 * s V_jpeg_suppress_tables_i + -3 <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_i <= 0)%Z
   | 23 => (-1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -4 <= 0 /\ -1 * s V_jpeg_suppress_tables_i + 1 <= 0)%Z
   | 24 => (-1 * s V_jpeg_suppress_tables_i + 1 <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -4 <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0)%Z
   | 25 => (-1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -4 <= 0 /\ -1 * s V_jpeg_suppress_tables_i + 1 <= 0)%Z
   | 26 => (-1 * s V_jpeg_suppress_tables_i + 1 <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -4 <= 0 /\ -1 * s V_jpeg_suppress_tables_z + 1 <= 0)%Z
   | 27 => (-1 * s V_jpeg_suppress_tables_i <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -3 <= 0)%Z
   | 28 => (1 * s V_jpeg_suppress_tables_i + -3 <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_i <= 0)%Z
   | 29 => (-1 * s V_jpeg_suppress_tables_i <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -3 <= 0)%Z
   | 30 => (1 * s V_jpeg_suppress_tables_i + -3 <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_i <= 0)%Z
   | 31 => (-1 * s V_jpeg_suppress_tables_i <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -3 <= 0)%Z
   | 32 => (-1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_i + 1 <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -4 <= 0)%Z
   | 33 => (1 * s V_jpeg_suppress_tables_i + -4 <= 0 /\ -1 * s V_jpeg_suppress_tables_i + 1 <= 0 /\ -1 * s V_jpeg_suppress_tables_z <= 0)%Z
   | 34 => (-1 * s V_jpeg_suppress_tables_z <= 0 /\ -1 * s V_jpeg_suppress_tables_i + 1 <= 0 /\ 1 * s V_jpeg_suppress_tables_i + -4 <= 0)%Z
   | 35 => (1 * s V_jpeg_suppress_tables_i + -4 <= 0 /\ -1 * s V_jpeg_suppress_tables_i + 1 <= 0 /\ -1 * s V_jpeg_suppress_tables_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_jpeg_suppress_tables (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_jpeg_suppress_tables_z <= z)%Q
   | 3 => ((8 # 1) + s V_jpeg_suppress_tables_z <= z)%Q
   | 4 => ((4 # 1) + s V_jpeg_suppress_tables_z
           + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 5 => ((4 # 1) + s V_jpeg_suppress_tables_z
           + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 6 => ((4 # 1) + s V_jpeg_suppress_tables_z
           + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_jpeg_suppress_tables_i) (3
                                                                    - s V_jpeg_suppress_tables_i));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                - s V_jpeg_suppress_tables_i)) (F_check_ge (0) (0))]
     ((4 # 1) + s V_jpeg_suppress_tables_z
      + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 8 => ((4 # 1) + s V_jpeg_suppress_tables_z <= z)%Q
   | 9 => (s V_jpeg_suppress_tables_z + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 10 => (s V_jpeg_suppress_tables_z + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 11 => (s V_jpeg_suppress_tables_z + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_jpeg_suppress_tables_i) (3
                                                                    - s V_jpeg_suppress_tables_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_jpeg_suppress_tables_i)]
     (s V_jpeg_suppress_tables_z + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 13 => (s V_jpeg_suppress_tables_z <= z)%Q
   | 14 => (s V_jpeg_suppress_tables_z + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 15 => (s V_jpeg_suppress_tables_z + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                   - s V_jpeg_suppress_tables_i)) (F_check_ge (4
                                                                    - s V_jpeg_suppress_tables_i) (0))]
     (s V_jpeg_suppress_tables_z + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 17 => (s V_jpeg_suppress_tables_z + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 18 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (4
                                                  - s V_jpeg_suppress_tables_i)) (F_check_ge (4
                                                                    - s V_jpeg_suppress_tables_i) (0))]
     (s V_jpeg_suppress_tables_z + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 19 => ((4 # 1) - s V_jpeg_suppress_tables_i + s V_jpeg_suppress_tables_z <= z)%Q
   | 20 => ((4 # 1) - s V_jpeg_suppress_tables_i + s V_jpeg_suppress_tables_z <= z)%Q
   | 21 => ((4 # 1) - s V_jpeg_suppress_tables_i + s V_jpeg_suppress_tables_z <= z)%Q
   | 22 => ((4 # 1) - s V_jpeg_suppress_tables_i + s V_jpeg_suppress_tables_z <= z)%Q
   | 23 => ((5 # 1) - s V_jpeg_suppress_tables_i + s V_jpeg_suppress_tables_z <= z)%Q
   | 24 => ((5 # 1) - s V_jpeg_suppress_tables_i + s V_jpeg_suppress_tables_z <= z)%Q
   | 25 => ((5 # 1) - s V_jpeg_suppress_tables_i + s V_jpeg_suppress_tables_z <= z)%Q
   | 26 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_jpeg_suppress_tables_i) (0))) (F_max0_ge_0 (4
                                                                    - s V_jpeg_suppress_tables_i))]
     ((4 # 1) - s V_jpeg_suppress_tables_i + s V_jpeg_suppress_tables_z <= z)%Q
   | 27 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_jpeg_suppress_tables_i) (1)]
     ((4 # 1) + s V_jpeg_suppress_tables_z
      + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 28 => ((5 # 1) + s V_jpeg_suppress_tables_z
            + max0(3 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 29 => ((5 # 1) + s V_jpeg_suppress_tables_z
            + max0(3 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 30 => ((5 # 1) + s V_jpeg_suppress_tables_z
            + max0(3 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 31 => ((5 # 1) + s V_jpeg_suppress_tables_z
            + max0(3 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 32 => ((5 # 1) + s V_jpeg_suppress_tables_z
            + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 33 => ((5 # 1) + s V_jpeg_suppress_tables_z
            + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 34 => ((5 # 1) + s V_jpeg_suppress_tables_z
            + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | 35 => ((4 # 1) + s V_jpeg_suppress_tables_z
            + max0(4 - s V_jpeg_suppress_tables_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_jpeg_suppress_tables =>
    [mkPA Q (fun n z s => ai_jpeg_suppress_tables n s /\ annot0_jpeg_suppress_tables n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_jpeg_suppress_tables (proc_start P_jpeg_suppress_tables) s1 (proc_end P_jpeg_suppress_tables) s2 ->
    (s2 V_jpeg_suppress_tables_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_jpeg_suppress_tables.
Qed.
