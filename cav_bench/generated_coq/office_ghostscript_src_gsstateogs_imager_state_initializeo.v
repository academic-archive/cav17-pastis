Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gs_imager_state_initialize.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gs_imager_state_initialize_z := 1%positive.
Notation V_gs_imager_state_initialize__tmp := 2%positive.
Notation V_gs_imager_state_initialize_i := 3%positive.
Notation V_gs_imager_state_initialize_mem := 4%positive.
Notation V_gs_imager_state_initialize_pis := 5%positive.
Definition Pedges_gs_imager_state_initialize: list (edge proc) :=
  (EA 1 (AAssign V_gs_imager_state_initialize_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_gs_imager_state_initialize_i (Some (ENum (0)))) 3)::
  (EA 3 ANone 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_gs_imager_state_initialize_i) s) <
  (eval (ENum (2)) s))%Z)) 19)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_gs_imager_state_initialize_i) s) >=
  (eval (ENum (2)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 ANone 15)::(EA 9 ANone 10)::(EA 10 ANone 11)::
  (EA 11 ANone 12)::(EA 12 (AAssign V_gs_imager_state_initialize__tmp
  (Some (ENum (0)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 18)::
  (EA 15 (AAssign V_gs_imager_state_initialize__tmp
  (Some (ENum (-25)))) 16)::(EA 16 ANone 17)::(EA 17 AWeaken 18)::
  (EA 19 AWeaken 20)::(EA 20 ANone 21)::(EA 21 (AAssign
  V_gs_imager_state_initialize_i
  (Some (EAdd (EVar V_gs_imager_state_initialize_i) (ENum (1))))) 22)::
  (EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 (AAssign
  V_gs_imager_state_initialize_z (Some (EAdd (ENum (1))
  (EVar V_gs_imager_state_initialize_z)))) 25)::(EA 25 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gs_imager_state_initialize => Pedges_gs_imager_state_initialize
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gs_imager_state_initialize => 18
     end)%positive;
  var_global := var_global
}.

Definition ai_gs_imager_state_initialize (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0)%Z
   | 3 => (-1 * s V_gs_imager_state_initialize_z <= 0 /\ 1 * s V_gs_imager_state_initialize_z <= 0 /\ 1 * s V_gs_imager_state_initialize_i <= 0 /\ -1 * s V_gs_imager_state_initialize_i <= 0)%Z
   | 4 => (-1 * s V_gs_imager_state_initialize_i <= 0 /\ 1 * s V_gs_imager_state_initialize_i <= 0 /\ 1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0)%Z
   | 5 => (-1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_i <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -2 <= 0)%Z
   | 6 => (1 * s V_gs_imager_state_initialize_i + -2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_i + 2 <= 0)%Z
   | 7 => (-1 * s V_gs_imager_state_initialize_i + 2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -2 <= 0)%Z
   | 8 => (1 * s V_gs_imager_state_initialize_i + -2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_i + 2 <= 0)%Z
   | 9 => (-1 * s V_gs_imager_state_initialize_i + 2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -2 <= 0)%Z
   | 10 => (1 * s V_gs_imager_state_initialize_i + -2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_i + 2 <= 0)%Z
   | 11 => (-1 * s V_gs_imager_state_initialize_i + 2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -2 <= 0)%Z
   | 12 => (1 * s V_gs_imager_state_initialize_i + -2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_i + 2 <= 0)%Z
   | 13 => (-1 * s V_gs_imager_state_initialize_i + 2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -2 <= 0 /\ 1 * s V_gs_imager_state_initialize__tmp <= 0 /\ -1 * s V_gs_imager_state_initialize__tmp <= 0)%Z
   | 14 => (-1 * s V_gs_imager_state_initialize__tmp <= 0 /\ 1 * s V_gs_imager_state_initialize__tmp <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_i + 2 <= 0)%Z
   | 15 => (1 * s V_gs_imager_state_initialize_i + -2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_i + 2 <= 0)%Z
   | 16 => (-1 * s V_gs_imager_state_initialize_i + 2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -2 <= 0 /\ 1 * s V_gs_imager_state_initialize__tmp + 25 <= 0 /\ -1 * s V_gs_imager_state_initialize__tmp + -25 <= 0)%Z
   | 17 => (-1 * s V_gs_imager_state_initialize__tmp + -25 <= 0 /\ 1 * s V_gs_imager_state_initialize__tmp + 25 <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_i + 2 <= 0)%Z
   | 18 => (1 * s V_gs_imager_state_initialize__tmp <= 0 /\ -1 * s V_gs_imager_state_initialize_i + 2 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -2 <= 0 /\ -1 * s V_gs_imager_state_initialize__tmp + -25 <= 0)%Z
   | 19 => (-1 * s V_gs_imager_state_initialize_i <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -1 <= 0)%Z
   | 20 => (1 * s V_gs_imager_state_initialize_i + -1 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_i <= 0)%Z
   | 21 => (-1 * s V_gs_imager_state_initialize_i <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -1 <= 0)%Z
   | 22 => (-1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_i + 1 <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -2 <= 0)%Z
   | 23 => (1 * s V_gs_imager_state_initialize_i + -2 <= 0 /\ -1 * s V_gs_imager_state_initialize_i + 1 <= 0 /\ -1 * s V_gs_imager_state_initialize_z <= 0)%Z
   | 24 => (-1 * s V_gs_imager_state_initialize_z <= 0 /\ -1 * s V_gs_imager_state_initialize_i + 1 <= 0 /\ 1 * s V_gs_imager_state_initialize_i + -2 <= 0)%Z
   | 25 => (1 * s V_gs_imager_state_initialize_i + -2 <= 0 /\ -1 * s V_gs_imager_state_initialize_i + 1 <= 0 /\ -1 * s V_gs_imager_state_initialize_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gs_imager_state_initialize (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((2 # 1) <= z)%Q
   | 2 => ((2 # 1) + s V_gs_imager_state_initialize_z <= z)%Q
   | 3 => ((2 # 1) - s V_gs_imager_state_initialize_i
           + s V_gs_imager_state_initialize_z <= z)%Q
   | 4 => ((2 # 1) - s V_gs_imager_state_initialize_i
           + s V_gs_imager_state_initialize_z <= z)%Q
   | 5 => ((2 # 1) - s V_gs_imager_state_initialize_i
           + s V_gs_imager_state_initialize_z <= z)%Q
   | 6 => ((2 # 1) - s V_gs_imager_state_initialize_i
           + s V_gs_imager_state_initialize_z <= z)%Q
   | 7 => ((2 # 1) - s V_gs_imager_state_initialize_i
           + s V_gs_imager_state_initialize_z <= z)%Q
   | 8 => ((2 # 1) - s V_gs_imager_state_initialize_i
           + s V_gs_imager_state_initialize_z <= z)%Q
   | 9 => ((2 # 1) - s V_gs_imager_state_initialize_i
           + s V_gs_imager_state_initialize_z <= z)%Q
   | 10 => ((2 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | 11 => ((2 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | 12 => ((2 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | 13 => ((2 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (2
                                             - s V_gs_imager_state_initialize_i) (1
                                                                    - s V_gs_imager_state_initialize_i));
      (*-1 0*) F_max0_ge_0 (1 - s V_gs_imager_state_initialize_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                               - s V_gs_imager_state_initialize_i) (0))) (F_max0_ge_0 (2
                                                                    - s V_gs_imager_state_initialize_i))]
     ((2 # 1) - s V_gs_imager_state_initialize_i
      + s V_gs_imager_state_initialize_z <= z)%Q
   | 15 => ((2 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | 16 => ((2 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (2
                                             - s V_gs_imager_state_initialize_i) (1
                                                                    - s V_gs_imager_state_initialize_i));
      (*-1 0*) F_max0_ge_0 (1 - s V_gs_imager_state_initialize_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                               - s V_gs_imager_state_initialize_i) (0))) (F_max0_ge_0 (2
                                                                    - s V_gs_imager_state_initialize_i))]
     ((2 # 1) - s V_gs_imager_state_initialize_i
      + s V_gs_imager_state_initialize_z <= z)%Q
   | 18 => (s V_gs_imager_state_initialize_z <= z)%Q
   | 19 => ((2 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | 20 => ((2 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | 21 => ((2 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | 22 => ((3 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | 23 => ((3 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | 24 => ((3 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | 25 => ((2 # 1) - s V_gs_imager_state_initialize_i
            + s V_gs_imager_state_initialize_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gs_imager_state_initialize =>
    [mkPA Q (fun n z s => ai_gs_imager_state_initialize n s /\ annot0_gs_imager_state_initialize n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gs_imager_state_initialize (proc_start P_gs_imager_state_initialize) s1 (proc_end P_gs_imager_state_initialize) s2 ->
    (s2 V_gs_imager_state_initialize_z <= (2 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gs_imager_state_initialize.
Qed.
