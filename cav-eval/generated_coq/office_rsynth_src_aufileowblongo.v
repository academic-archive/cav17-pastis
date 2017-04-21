Require Import pasta.Pasta.

Inductive proc: Type :=
  P_wblong.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_wblong_z := 1%positive.
Notation V_wblong__tmp := 2%positive.
Notation V_wblong__tmp1 := 3%positive.
Notation V_wblong_i := 4%positive.
Notation V_wblong_fd := 5%positive.
Notation V_wblong_x := 6%positive.
Definition Pedges_wblong: list (edge proc) :=
  (EA 1 (AAssign V_wblong_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_wblong__tmp1 (Some (EVar V_wblong_fd))) 3)::(EA 3 (AAssign V_wblong__tmp
  (Some (EVar V_wblong_x))) 4)::(EA 4 (AAssign V_wblong_i
  (Some (ENum (24)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_wblong_i) s) >= (eval (ENum (0)) s))%Z)) 10)::
  (EA 7 (AGuard (fun s => ((eval (EVar V_wblong_i) s) < (eval (ENum (0))
  s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 10 AWeaken 11)::(EA 11 ANone 12)::
  (EA 12 (AAssign V_wblong_i (Some (ESub (EVar V_wblong_i)
  (ENum (8))))) 13)::(EA 13 ANone 14)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_wblong_z (Some (EAdd (ENum (1)) (EVar V_wblong_z)))) 16)::
  (EA 16 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_wblong => Pedges_wblong
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_wblong => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_wblong (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_wblong_z <= 0 /\ -1 * s V_wblong_z <= 0)%Z
   | 3 => (-1 * s V_wblong_z <= 0 /\ 1 * s V_wblong_z <= 0)%Z
   | 4 => (1 * s V_wblong_z <= 0 /\ -1 * s V_wblong_z <= 0)%Z
   | 5 => (-1 * s V_wblong_z <= 0 /\ 1 * s V_wblong_z <= 0 /\ 1 * s V_wblong_i + -24 <= 0 /\ -1 * s V_wblong_i + 24 <= 0)%Z
   | 6 => (-1 * s V_wblong_i + 24 <= 0 /\ 1 * s V_wblong_i + -24 <= 0 /\ 1 * s V_wblong_z <= 0 /\ -1 * s V_wblong_z <= 0)%Z
   | 7 => (-1 * s V_wblong_z <= 0 /\ 1 * s V_wblong_i + -24 <= 0 /\ -1 * s V_wblong_i + -8 <= 0)%Z
   | 8 => (-1 * s V_wblong_i + -8 <= 0 /\ -1 * s V_wblong_z <= 0 /\ 1 * s V_wblong_i + 1 <= 0)%Z
   | 9 => (1 * s V_wblong_i + 1 <= 0 /\ -1 * s V_wblong_z <= 0 /\ -1 * s V_wblong_i + -8 <= 0)%Z
   | 10 => (1 * s V_wblong_i + -24 <= 0 /\ -1 * s V_wblong_z <= 0 /\ -1 * s V_wblong_i <= 0)%Z
   | 11 => (-1 * s V_wblong_i <= 0 /\ -1 * s V_wblong_z <= 0 /\ 1 * s V_wblong_i + -24 <= 0)%Z
   | 12 => (1 * s V_wblong_i + -24 <= 0 /\ -1 * s V_wblong_z <= 0 /\ -1 * s V_wblong_i <= 0)%Z
   | 13 => (-1 * s V_wblong_z <= 0 /\ 1 * s V_wblong_i + -16 <= 0 /\ -1 * s V_wblong_i + -8 <= 0)%Z
   | 14 => (-1 * s V_wblong_i + -8 <= 0 /\ 1 * s V_wblong_i + -16 <= 0 /\ -1 * s V_wblong_z <= 0)%Z
   | 15 => (-1 * s V_wblong_z <= 0 /\ 1 * s V_wblong_i + -16 <= 0 /\ -1 * s V_wblong_i + -8 <= 0)%Z
   | 16 => (-1 * s V_wblong_i + -8 <= 0 /\ 1 * s V_wblong_i + -16 <= 0 /\ -1 * s V_wblong_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_wblong (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_wblong_z <= z)%Q
   | 3 => ((4 # 1) + s V_wblong_z <= z)%Q
   | 4 => ((4 # 1) + s V_wblong_z <= z)%Q
   | 5 => (s V_wblong_z + (1 # 8) * max0(8 + s V_wblong_i) <= z)%Q
   | 6 => (s V_wblong_z + (1 # 8) * max0(8 + s V_wblong_i) <= z)%Q
   | 7 => (s V_wblong_z + (1 # 8) * max0(8 + s V_wblong_i) <= z)%Q
   | 8 => hints
     [(*-0.125 0*) F_max0_monotonic (F_check_ge (8 + s V_wblong_i) (s V_wblong_i));
      (*-0.125 0*) F_max0_ge_0 (s V_wblong_i)]
     (s V_wblong_z + (1 # 8) * max0(8 + s V_wblong_i) <= z)%Q
   | 9 => (s V_wblong_z <= z)%Q
   | 10 => hints
     [(*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_arg (8 + s V_wblong_i)) (F_check_ge (8
                                                                    + s V_wblong_i) (0))]
     (s V_wblong_z + (1 # 8) * max0(8 + s V_wblong_i) <= z)%Q
   | 11 => ((1 # 1) + (1 # 8) * s V_wblong_i + s V_wblong_z <= z)%Q
   | 12 => ((1 # 1) + (1 # 8) * s V_wblong_i + s V_wblong_z <= z)%Q
   | 13 => ((2 # 1) + (1 # 8) * s V_wblong_i + s V_wblong_z <= z)%Q
   | 14 => ((2 # 1) + (1 # 8) * s V_wblong_i + s V_wblong_z <= z)%Q
   | 15 => ((2 # 1) + (1 # 8) * s V_wblong_i + s V_wblong_z <= z)%Q
   | 16 => hints
     [(*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                   + 
                                                                   s V_wblong_i) (0))) (F_max0_ge_0 (8
                                                                    + s V_wblong_i))]
     ((1 # 1) + (1 # 8) * s V_wblong_i + s V_wblong_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_wblong =>
    [mkPA Q (fun n z s => ai_wblong n s /\ annot0_wblong n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_wblong (proc_start P_wblong) s1 (proc_end P_wblong) s2 ->
    (s2 V_wblong_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_wblong.
Qed.
