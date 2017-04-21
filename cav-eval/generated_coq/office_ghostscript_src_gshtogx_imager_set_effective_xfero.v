Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gx_imager_set_effective_xfer.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gx_imager_set_effective_xfer_z := 1%positive.
Notation V_gx_imager_set_effective_xfer_i := 2%positive.
Notation V_gx_imager_set_effective_xfer_pis := 3%positive.
Definition Pedges_gx_imager_set_effective_xfer: list (edge proc) :=
  (EA 1 (AAssign V_gx_imager_set_effective_xfer_z (Some (ENum (0)))) 2)::
  (EA 2 AWeaken 3)::(EA 3 ANone 27)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 ANone 22)::(EA 5 ANone 6)::(EA 6 (AAssign
  V_gx_imager_set_effective_xfer_i (Some (ENum (0)))) 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_gx_imager_set_effective_xfer_i) s) <
  (eval (ENum (4)) s))%Z)) 13)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_gx_imager_set_effective_xfer_i) s) >=
  (eval (ENum (4)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 ANone 12)::
  (EA 12 AWeaken 29)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::
  (EA 14 ANone 16)::(EA 15 ANone 16)::(EA 16 ANone 17)::(EA 17 (AAssign
  V_gx_imager_set_effective_xfer_i
  (Some (EAdd (EVar V_gx_imager_set_effective_xfer_i) (ENum (1))))) 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_gx_imager_set_effective_xfer_z (Some (EAdd (ENum (1))
  (EVar V_gx_imager_set_effective_xfer_z)))) 21)::(EA 21 AWeaken 9)::
  (EA 22 AWeaken 23)::(EA 23 ANone 24)::(EA 23 ANone 25)::(EA 24 ANone 25)::
  (EA 25 ANone 26)::(EA 26 AWeaken 29)::(EA 27 ANone 28)::
  (EA 28 AWeaken 29)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gx_imager_set_effective_xfer => Pedges_gx_imager_set_effective_xfer
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gx_imager_set_effective_xfer => 29
     end)%positive;
  var_global := var_global
}.

Definition ai_gx_imager_set_effective_xfer (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 3 => (-1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 4 => (1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 5 => (-1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 6 => (1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 7 => (-1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_i <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_i <= 0)%Z
   | 8 => (-1 * s V_gx_imager_set_effective_xfer_i <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_i <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 9 => (-1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_i <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_i + -4 <= 0)%Z
   | 10 => (1 * s V_gx_imager_set_effective_xfer_i + -4 <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_i + 4 <= 0)%Z
   | 11 => (-1 * s V_gx_imager_set_effective_xfer_i + 4 <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_i + -4 <= 0)%Z
   | 12 => (1 * s V_gx_imager_set_effective_xfer_i + -4 <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_i + 4 <= 0)%Z
   | 13 => (-1 * s V_gx_imager_set_effective_xfer_i <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_i + -3 <= 0)%Z
   | 14 => (1 * s V_gx_imager_set_effective_xfer_i + -3 <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_i <= 0)%Z
   | 15 => (-1 * s V_gx_imager_set_effective_xfer_i <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_i + -3 <= 0)%Z
   | 16 => (1 * s V_gx_imager_set_effective_xfer_i + -3 <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_i <= 0)%Z
   | 17 => (-1 * s V_gx_imager_set_effective_xfer_i <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_i + -3 <= 0)%Z
   | 18 => (-1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_i + 1 <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_i + -4 <= 0)%Z
   | 19 => (1 * s V_gx_imager_set_effective_xfer_i + -4 <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_i + 1 <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 20 => (-1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_i + 1 <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_i + -4 <= 0)%Z
   | 21 => (1 * s V_gx_imager_set_effective_xfer_i + -4 <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_i + 1 <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z + 1 <= 0)%Z
   | 22 => (1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 23 => (-1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 24 => (1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 25 => (-1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 26 => (1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 27 => (1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ -1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 28 => (-1 * s V_gx_imager_set_effective_xfer_z <= 0 /\ 1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | 29 => (-1 * s V_gx_imager_set_effective_xfer_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gx_imager_set_effective_xfer (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + max0(s V_gx_imager_set_effective_xfer_z) <= z)%Q
   | 3 => ((4 # 1) + max0(s V_gx_imager_set_effective_xfer_z) <= z)%Q
   | 4 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gx_imager_set_effective_xfer_z)) (F_check_ge (s V_gx_imager_set_effective_xfer_z) (0))]
     ((4 # 1) + max0(s V_gx_imager_set_effective_xfer_z) <= z)%Q
   | 5 => ((4 # 1) + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 6 => ((4 # 1) + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 7 => ((4 # 1) - s V_gx_imager_set_effective_xfer_i
           + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 8 => ((4 # 1) - s V_gx_imager_set_effective_xfer_i
           + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 9 => ((4 # 1) - s V_gx_imager_set_effective_xfer_i
           + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 10 => ((4 # 1) - s V_gx_imager_set_effective_xfer_i
            + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 11 => ((4 # 1) - s V_gx_imager_set_effective_xfer_i
            + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4
                                             - s V_gx_imager_set_effective_xfer_i) (3
                                                                    - s V_gx_imager_set_effective_xfer_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_gx_imager_set_effective_xfer_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_gx_imager_set_effective_xfer_i) (0))) (F_max0_ge_0 (4
                                                                    - s V_gx_imager_set_effective_xfer_i))]
     ((4 # 1) - s V_gx_imager_set_effective_xfer_i
      + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 13 => ((4 # 1) - s V_gx_imager_set_effective_xfer_i
            + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 14 => ((4 # 1) - s V_gx_imager_set_effective_xfer_i
            + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 15 => ((4 # 1) - s V_gx_imager_set_effective_xfer_i
            + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 16 => ((4 # 1) - s V_gx_imager_set_effective_xfer_i
            + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 17 => ((4 # 1) - s V_gx_imager_set_effective_xfer_i
            + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 18 => ((5 # 1) - s V_gx_imager_set_effective_xfer_i
            + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 19 => ((5 # 1) - s V_gx_imager_set_effective_xfer_i
            + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 20 => ((5 # 1) - s V_gx_imager_set_effective_xfer_i
            + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 21 => ((4 # 1) - s V_gx_imager_set_effective_xfer_i
            + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 22 => ((4 # 1) + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 23 => ((4 # 1) + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 24 => ((4 # 1) + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 25 => ((4 # 1) + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 26 => hints
     [(*-4 0*) F_one]
     ((4 # 1) + s V_gx_imager_set_effective_xfer_z <= z)%Q
   | 27 => ((4 # 1) + max0(s V_gx_imager_set_effective_xfer_z) <= z)%Q
   | 28 => hints
     [(*-4 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gx_imager_set_effective_xfer_z)) (F_check_ge (s V_gx_imager_set_effective_xfer_z) (0))]
     ((4 # 1) + max0(s V_gx_imager_set_effective_xfer_z) <= z)%Q
   | 29 => (s V_gx_imager_set_effective_xfer_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gx_imager_set_effective_xfer =>
    [mkPA Q (fun n z s => ai_gx_imager_set_effective_xfer n s /\ annot0_gx_imager_set_effective_xfer n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gx_imager_set_effective_xfer (proc_start P_gx_imager_set_effective_xfer) s1 (proc_end P_gx_imager_set_effective_xfer) s2 ->
    (s2 V_gx_imager_set_effective_xfer_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gx_imager_set_effective_xfer.
Qed.
