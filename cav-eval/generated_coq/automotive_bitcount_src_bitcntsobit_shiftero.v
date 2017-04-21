Require Import pasta.Pasta.

Inductive proc: Type :=
  P_bit_shifter.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_bit_shifter_z := 1%positive.
Notation V_bit_shifter__tmp := 2%positive.
Notation V_bit_shifter_i := 3%positive.
Notation V_bit_shifter_n := 4%positive.
Notation V_bit_shifter_x := 5%positive.
Definition Pedges_bit_shifter: list (edge proc) :=
  (EA 1 (AAssign V_bit_shifter_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_bit_shifter__tmp (Some (EVar V_bit_shifter_x))) 3)::(EA 3 (AAssign
  V_bit_shifter_n (Some (ENum (0)))) 4)::(EA 4 (AAssign V_bit_shifter_i
  (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_bit_shifter__tmp) s) <> (eval (ENum (0))
  s))%Z)) 9)::(EA 7 (AGuard (fun s => ((eval (EVar V_bit_shifter__tmp) s) =
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 14)::(EA 9 AWeaken 10)::
  (EA 10 ANone 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_bit_shifter_i) s) < (eval (ENum (64))
  s))%Z)) 15)::(EA 12 (AGuard (fun s => ((eval (EVar V_bit_shifter_i) s) >=
  (eval (ENum (64)) s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 15 AWeaken 16)::
  (EA 16 (AAssign V_bit_shifter_n None) 17)::(EA 17 ANone 18)::
  (EA 18 (AAssign V_bit_shifter_i (Some (EAdd (EVar V_bit_shifter_i)
  (ENum (1))))) 19)::(EA 19 (AAssign V_bit_shifter__tmp None) 20)::
  (EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 (AAssign V_bit_shifter_z
  (Some (EAdd (ENum (1)) (EVar V_bit_shifter_z)))) 23)::(EA 23 AWeaken 7)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_bit_shifter => Pedges_bit_shifter
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_bit_shifter => 14
     end)%positive;
  var_global := var_global
}.

Definition ai_bit_shifter (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_bit_shifter_z <= 0 /\ -1 * s V_bit_shifter_z <= 0)%Z
   | 3 => (-1 * s V_bit_shifter_z <= 0 /\ 1 * s V_bit_shifter_z <= 0)%Z
   | 4 => (1 * s V_bit_shifter_z <= 0 /\ -1 * s V_bit_shifter_z <= 0 /\ 1 * s V_bit_shifter_n <= 0 /\ -1 * s V_bit_shifter_n <= 0)%Z
   | 5 => (-1 * s V_bit_shifter_n <= 0 /\ 1 * s V_bit_shifter_n <= 0 /\ -1 * s V_bit_shifter_z <= 0 /\ 1 * s V_bit_shifter_z <= 0 /\ 1 * s V_bit_shifter_i <= 0 /\ -1 * s V_bit_shifter_i <= 0)%Z
   | 6 => (-1 * s V_bit_shifter_i <= 0 /\ 1 * s V_bit_shifter_i <= 0 /\ 1 * s V_bit_shifter_z <= 0 /\ -1 * s V_bit_shifter_z <= 0 /\ 1 * s V_bit_shifter_n <= 0 /\ -1 * s V_bit_shifter_n <= 0)%Z
   | 7 => (-1 * s V_bit_shifter_z <= 0 /\ -1 * s V_bit_shifter_i <= 0 /\ 1 * s V_bit_shifter_i + -64 <= 0)%Z
   | 8 => (1 * s V_bit_shifter_i + -64 <= 0 /\ -1 * s V_bit_shifter_i <= 0 /\ -1 * s V_bit_shifter_z <= 0 /\ 1 * s V_bit_shifter__tmp <= 0 /\ -1 * s V_bit_shifter__tmp <= 0)%Z
   | 9 => (1 * s V_bit_shifter_i + -64 <= 0 /\ -1 * s V_bit_shifter_i <= 0 /\ -1 * s V_bit_shifter_z <= 0)%Z
   | 10 => (-1 * s V_bit_shifter_z <= 0 /\ -1 * s V_bit_shifter_i <= 0 /\ 1 * s V_bit_shifter_i + -64 <= 0)%Z
   | 11 => (1 * s V_bit_shifter_i + -64 <= 0 /\ -1 * s V_bit_shifter_i <= 0 /\ -1 * s V_bit_shifter_z <= 0)%Z
   | 12 => (-1 * s V_bit_shifter_z <= 0 /\ -1 * s V_bit_shifter_i <= 0 /\ 1 * s V_bit_shifter_i + -64 <= 0)%Z
   | 13 => (1 * s V_bit_shifter_i + -64 <= 0 /\ -1 * s V_bit_shifter_z <= 0 /\ -1 * s V_bit_shifter_i + 64 <= 0)%Z
   | 14 => (-1 * s V_bit_shifter_i <= 0 /\ -1 * s V_bit_shifter_z <= 0 /\ 1 * s V_bit_shifter_i + -64 <= 0)%Z
   | 15 => (-1 * s V_bit_shifter_i <= 0 /\ -1 * s V_bit_shifter_z <= 0 /\ 1 * s V_bit_shifter_i + -63 <= 0)%Z
   | 16 => (1 * s V_bit_shifter_i + -63 <= 0 /\ -1 * s V_bit_shifter_z <= 0 /\ -1 * s V_bit_shifter_i <= 0)%Z
   | 17 => (-1 * s V_bit_shifter_i <= 0 /\ -1 * s V_bit_shifter_z <= 0 /\ 1 * s V_bit_shifter_i + -63 <= 0)%Z
   | 18 => (1 * s V_bit_shifter_i + -63 <= 0 /\ -1 * s V_bit_shifter_z <= 0 /\ -1 * s V_bit_shifter_i <= 0)%Z
   | 19 => (-1 * s V_bit_shifter_z <= 0 /\ 1 * s V_bit_shifter_i + -64 <= 0 /\ -1 * s V_bit_shifter_i + 1 <= 0)%Z
   | 20 => (-1 * s V_bit_shifter_i + 1 <= 0 /\ 1 * s V_bit_shifter_i + -64 <= 0 /\ -1 * s V_bit_shifter_z <= 0)%Z
   | 21 => (-1 * s V_bit_shifter_z <= 0 /\ 1 * s V_bit_shifter_i + -64 <= 0 /\ -1 * s V_bit_shifter_i + 1 <= 0)%Z
   | 22 => (-1 * s V_bit_shifter_i + 1 <= 0 /\ 1 * s V_bit_shifter_i + -64 <= 0 /\ -1 * s V_bit_shifter_z <= 0)%Z
   | 23 => (1 * s V_bit_shifter_i + -64 <= 0 /\ -1 * s V_bit_shifter_i + 1 <= 0 /\ -1 * s V_bit_shifter_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_bit_shifter (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((64 # 1) <= z)%Q
   | 2 => ((64 # 1) + s V_bit_shifter_z <= z)%Q
   | 3 => ((64 # 1) + s V_bit_shifter_z <= z)%Q
   | 4 => ((64 # 1) + s V_bit_shifter_z <= z)%Q
   | 5 => (s V_bit_shifter_z + max0(64 - s V_bit_shifter_i) <= z)%Q
   | 6 => (s V_bit_shifter_z + max0(64 - s V_bit_shifter_i) <= z)%Q
   | 7 => (s V_bit_shifter_z + max0(64 - s V_bit_shifter_i) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (64 - s V_bit_shifter_i) (63
                                                                    - s V_bit_shifter_i));
      (*-1 0*) F_max0_ge_0 (63 - s V_bit_shifter_i)]
     (s V_bit_shifter_z + max0(64 - s V_bit_shifter_i) <= z)%Q
   | 9 => (s V_bit_shifter_z + max0(64 - s V_bit_shifter_i) <= z)%Q
   | 10 => (s V_bit_shifter_z + max0(64 - s V_bit_shifter_i) <= z)%Q
   | 11 => (s V_bit_shifter_z + max0(64 - s V_bit_shifter_i) <= z)%Q
   | 12 => (s V_bit_shifter_z + max0(64 - s V_bit_shifter_i) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (64 - s V_bit_shifter_i) (63
                                                                    - s V_bit_shifter_i));
      (*-1 0*) F_max0_ge_0 (63 - s V_bit_shifter_i)]
     (s V_bit_shifter_z + max0(64 - s V_bit_shifter_i) <= z)%Q
   | 14 => (s V_bit_shifter_z <= z)%Q
   | 15 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (64 - s V_bit_shifter_i)) (F_check_ge (64
                                                                    - s V_bit_shifter_i) (0))]
     (s V_bit_shifter_z + max0(64 - s V_bit_shifter_i) <= z)%Q
   | 16 => ((64 # 1) - s V_bit_shifter_i + s V_bit_shifter_z <= z)%Q
   | 17 => ((64 # 1) - s V_bit_shifter_i + s V_bit_shifter_z <= z)%Q
   | 18 => ((64 # 1) - s V_bit_shifter_i + s V_bit_shifter_z <= z)%Q
   | 19 => ((65 # 1) - s V_bit_shifter_i + s V_bit_shifter_z <= z)%Q
   | 20 => ((65 # 1) - s V_bit_shifter_i + s V_bit_shifter_z <= z)%Q
   | 21 => ((65 # 1) - s V_bit_shifter_i + s V_bit_shifter_z <= z)%Q
   | 22 => ((65 # 1) - s V_bit_shifter_i + s V_bit_shifter_z <= z)%Q
   | 23 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                              - s V_bit_shifter_i) (0))) (F_max0_ge_0 (64
                                                                    - s V_bit_shifter_i))]
     ((64 # 1) - s V_bit_shifter_i + s V_bit_shifter_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_bit_shifter =>
    [mkPA Q (fun n z s => ai_bit_shifter n s /\ annot0_bit_shifter n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_bit_shifter (proc_start P_bit_shifter) s1 (proc_end P_bit_shifter) s2 ->
    (s2 V_bit_shifter_z <= (64 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_bit_shifter.
Qed.
