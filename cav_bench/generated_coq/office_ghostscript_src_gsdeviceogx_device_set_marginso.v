Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gx_device_set_margins.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gx_device_set_margins_z := 1%positive.
Notation V_gx_device_set_margins__tmp := 2%positive.
Notation V_gx_device_set_margins_i := 3%positive.
Notation V_gx_device_set_margins_dev := 4%positive.
Notation V_gx_device_set_margins_margins := 5%positive.
Notation V_gx_device_set_margins_move_origin := 6%positive.
Definition Pedges_gx_device_set_margins: list (edge proc) :=
  (EA 1 (AAssign V_gx_device_set_margins_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_gx_device_set_margins__tmp
  (Some (EVar V_gx_device_set_margins_move_origin))) 3)::(EA 3 (AAssign
  V_gx_device_set_margins_i (Some (ENum (0)))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_gx_device_set_margins_i) s) < (eval (ENum (4))
  s))%Z)) 14)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_gx_device_set_margins_i) s) >= (eval (ENum (4))
  s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_gx_device_set_margins__tmp) s) <> (eval (ENum (0))
  s))%Z)) 10)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_gx_device_set_margins__tmp) s) = (eval (ENum (0))
  s))%Z)) 9)::(EA 9 AWeaken 13)::(EA 10 AWeaken 11)::(EA 11 ANone 12)::
  (EA 12 AWeaken 13)::(EA 14 AWeaken 15)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_gx_device_set_margins_i (Some (EAdd (EVar V_gx_device_set_margins_i)
  (ENum (1))))) 17)::(EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_gx_device_set_margins_z (Some (EAdd (ENum (1))
  (EVar V_gx_device_set_margins_z)))) 20)::(EA 20 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gx_device_set_margins => Pedges_gx_device_set_margins
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gx_device_set_margins => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_gx_device_set_margins (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gx_device_set_margins_z <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0)%Z
   | 3 => (-1 * s V_gx_device_set_margins_z <= 0 /\ 1 * s V_gx_device_set_margins_z <= 0)%Z
   | 4 => (1 * s V_gx_device_set_margins_z <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0 /\ 1 * s V_gx_device_set_margins_i <= 0 /\ -1 * s V_gx_device_set_margins_i <= 0)%Z
   | 5 => (-1 * s V_gx_device_set_margins_i <= 0 /\ 1 * s V_gx_device_set_margins_i <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0 /\ 1 * s V_gx_device_set_margins_z <= 0)%Z
   | 6 => (-1 * s V_gx_device_set_margins_z <= 0 /\ -1 * s V_gx_device_set_margins_i <= 0 /\ 1 * s V_gx_device_set_margins_i + -4 <= 0)%Z
   | 7 => (1 * s V_gx_device_set_margins_i + -4 <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0 /\ -1 * s V_gx_device_set_margins_i + 4 <= 0)%Z
   | 8 => (-1 * s V_gx_device_set_margins_i + 4 <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0 /\ 1 * s V_gx_device_set_margins_i + -4 <= 0)%Z
   | 9 => (1 * s V_gx_device_set_margins_i + -4 <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0 /\ -1 * s V_gx_device_set_margins_i + 4 <= 0 /\ 1 * s V_gx_device_set_margins__tmp <= 0 /\ -1 * s V_gx_device_set_margins__tmp <= 0)%Z
   | 10 => (1 * s V_gx_device_set_margins_i + -4 <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0 /\ -1 * s V_gx_device_set_margins_i + 4 <= 0)%Z
   | 11 => (-1 * s V_gx_device_set_margins_i + 4 <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0 /\ 1 * s V_gx_device_set_margins_i + -4 <= 0)%Z
   | 12 => (1 * s V_gx_device_set_margins_i + -4 <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0 /\ -1 * s V_gx_device_set_margins_i + 4 <= 0)%Z
   | 13 => (-1 * s V_gx_device_set_margins_i + 4 <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0 /\ 1 * s V_gx_device_set_margins_i + -4 <= 0)%Z
   | 14 => (-1 * s V_gx_device_set_margins_i <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0 /\ 1 * s V_gx_device_set_margins_i + -3 <= 0)%Z
   | 15 => (1 * s V_gx_device_set_margins_i + -3 <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0 /\ -1 * s V_gx_device_set_margins_i <= 0)%Z
   | 16 => (-1 * s V_gx_device_set_margins_i <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0 /\ 1 * s V_gx_device_set_margins_i + -3 <= 0)%Z
   | 17 => (-1 * s V_gx_device_set_margins_z <= 0 /\ -1 * s V_gx_device_set_margins_i + 1 <= 0 /\ 1 * s V_gx_device_set_margins_i + -4 <= 0)%Z
   | 18 => (1 * s V_gx_device_set_margins_i + -4 <= 0 /\ -1 * s V_gx_device_set_margins_i + 1 <= 0 /\ -1 * s V_gx_device_set_margins_z <= 0)%Z
   | 19 => (-1 * s V_gx_device_set_margins_z <= 0 /\ -1 * s V_gx_device_set_margins_i + 1 <= 0 /\ 1 * s V_gx_device_set_margins_i + -4 <= 0)%Z
   | 20 => (1 * s V_gx_device_set_margins_i + -4 <= 0 /\ -1 * s V_gx_device_set_margins_i + 1 <= 0 /\ -1 * s V_gx_device_set_margins_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gx_device_set_margins (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_gx_device_set_margins_z <= z)%Q
   | 3 => ((4 # 1) + s V_gx_device_set_margins_z <= z)%Q
   | 4 => ((4 # 1) - s V_gx_device_set_margins_i
           + s V_gx_device_set_margins_z <= z)%Q
   | 5 => ((4 # 1) - s V_gx_device_set_margins_i
           + s V_gx_device_set_margins_z <= z)%Q
   | 6 => ((4 # 1) - s V_gx_device_set_margins_i
           + s V_gx_device_set_margins_z <= z)%Q
   | 7 => hints
     [(*0 1*) F_max0_ge_0 (3 - s V_gx_device_set_margins_i)]
     ((4 # 1) - s V_gx_device_set_margins_i + s V_gx_device_set_margins_z <= z)%Q
   | 8 => ((4 # 1) - s V_gx_device_set_margins_i
           + s V_gx_device_set_margins_z
           - max0(3 - s V_gx_device_set_margins_i) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_gx_device_set_margins_i) (3
                                                                    - s V_gx_device_set_margins_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_gx_device_set_margins_i) (0))) (F_max0_ge_0 (4
                                                                    - s V_gx_device_set_margins_i))]
     ((4 # 1) - s V_gx_device_set_margins_i + s V_gx_device_set_margins_z
      - max0(3 - s V_gx_device_set_margins_i) <= z)%Q
   | 10 => ((4 # 1) - s V_gx_device_set_margins_i
            + s V_gx_device_set_margins_z
            - max0(3 - s V_gx_device_set_margins_i) <= z)%Q
   | 11 => ((4 # 1) - s V_gx_device_set_margins_i
            + s V_gx_device_set_margins_z
            - max0(3 - s V_gx_device_set_margins_i) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_gx_device_set_margins_i) (3
                                                                    - s V_gx_device_set_margins_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_gx_device_set_margins_i) (0))) (F_max0_ge_0 (4
                                                                    - s V_gx_device_set_margins_i))]
     ((4 # 1) - s V_gx_device_set_margins_i + s V_gx_device_set_margins_z
      - max0(3 - s V_gx_device_set_margins_i) <= z)%Q
   | 13 => (s V_gx_device_set_margins_z <= z)%Q
   | 14 => ((4 # 1) - s V_gx_device_set_margins_i
            + s V_gx_device_set_margins_z <= z)%Q
   | 15 => ((4 # 1) - s V_gx_device_set_margins_i
            + s V_gx_device_set_margins_z <= z)%Q
   | 16 => ((4 # 1) - s V_gx_device_set_margins_i
            + s V_gx_device_set_margins_z <= z)%Q
   | 17 => ((5 # 1) - s V_gx_device_set_margins_i
            + s V_gx_device_set_margins_z <= z)%Q
   | 18 => ((5 # 1) - s V_gx_device_set_margins_i
            + s V_gx_device_set_margins_z <= z)%Q
   | 19 => ((5 # 1) - s V_gx_device_set_margins_i
            + s V_gx_device_set_margins_z <= z)%Q
   | 20 => ((4 # 1) - s V_gx_device_set_margins_i
            + s V_gx_device_set_margins_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gx_device_set_margins =>
    [mkPA Q (fun n z s => ai_gx_device_set_margins n s /\ annot0_gx_device_set_margins n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gx_device_set_margins (proc_start P_gx_device_set_margins) s1 (proc_end P_gx_device_set_margins) s2 ->
    (s2 V_gx_device_set_margins_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gx_device_set_margins.
Qed.
