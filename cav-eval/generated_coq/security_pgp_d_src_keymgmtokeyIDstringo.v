Require Import pasta.Pasta.

Inductive proc: Type :=
  P_keyIDstring.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_keyIDstring_z := 1%positive.
Notation V_keyIDstring_i := 2%positive.
Notation V_keyIDstring_keyID := 3%positive.
Definition Pedges_keyIDstring: list (edge proc) :=
  (EA 1 (AAssign V_keyIDstring_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_keyIDstring_i (Some (ENum (4)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_keyIDstring_i) s) < (eval (ENum (8))
  s))%Z)) 8)::(EA 5 (AGuard (fun s => ((eval (EVar V_keyIDstring_i) s) >=
  (eval (ENum (8)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 8 AWeaken 9)::
  (EA 9 ANone 10)::(EA 10 (AAssign V_keyIDstring_i
  (Some (EAdd (EVar V_keyIDstring_i) (ENum (1))))) 11)::(EA 11 ANone 12)::
  (EA 12 ANone 13)::(EA 13 (AAssign V_keyIDstring_z (Some (EAdd (ENum (1))
  (EVar V_keyIDstring_z)))) 14)::(EA 14 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_keyIDstring => Pedges_keyIDstring
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_keyIDstring => 7
     end)%positive;
  var_global := var_global
}.

Definition ai_keyIDstring (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_keyIDstring_z <= 0 /\ -1 * s V_keyIDstring_z <= 0)%Z
   | 3 => (-1 * s V_keyIDstring_z <= 0 /\ 1 * s V_keyIDstring_z <= 0 /\ 1 * s V_keyIDstring_i + -4 <= 0 /\ -1 * s V_keyIDstring_i + 4 <= 0)%Z
   | 4 => (-1 * s V_keyIDstring_i + 4 <= 0 /\ 1 * s V_keyIDstring_i + -4 <= 0 /\ 1 * s V_keyIDstring_z <= 0 /\ -1 * s V_keyIDstring_z <= 0)%Z
   | 5 => (-1 * s V_keyIDstring_z <= 0 /\ -1 * s V_keyIDstring_i + 4 <= 0 /\ 1 * s V_keyIDstring_i + -8 <= 0)%Z
   | 6 => (1 * s V_keyIDstring_i + -8 <= 0 /\ -1 * s V_keyIDstring_z <= 0 /\ -1 * s V_keyIDstring_i + 8 <= 0)%Z
   | 7 => (-1 * s V_keyIDstring_i + 8 <= 0 /\ -1 * s V_keyIDstring_z <= 0 /\ 1 * s V_keyIDstring_i + -8 <= 0)%Z
   | 8 => (-1 * s V_keyIDstring_i + 4 <= 0 /\ -1 * s V_keyIDstring_z <= 0 /\ 1 * s V_keyIDstring_i + -7 <= 0)%Z
   | 9 => (1 * s V_keyIDstring_i + -7 <= 0 /\ -1 * s V_keyIDstring_z <= 0 /\ -1 * s V_keyIDstring_i + 4 <= 0)%Z
   | 10 => (-1 * s V_keyIDstring_i + 4 <= 0 /\ -1 * s V_keyIDstring_z <= 0 /\ 1 * s V_keyIDstring_i + -7 <= 0)%Z
   | 11 => (-1 * s V_keyIDstring_z <= 0 /\ -1 * s V_keyIDstring_i + 5 <= 0 /\ 1 * s V_keyIDstring_i + -8 <= 0)%Z
   | 12 => (1 * s V_keyIDstring_i + -8 <= 0 /\ -1 * s V_keyIDstring_i + 5 <= 0 /\ -1 * s V_keyIDstring_z <= 0)%Z
   | 13 => (-1 * s V_keyIDstring_z <= 0 /\ -1 * s V_keyIDstring_i + 5 <= 0 /\ 1 * s V_keyIDstring_i + -8 <= 0)%Z
   | 14 => (1 * s V_keyIDstring_i + -8 <= 0 /\ -1 * s V_keyIDstring_i + 5 <= 0 /\ -1 * s V_keyIDstring_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_keyIDstring (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + s V_keyIDstring_z <= z)%Q
   | 3 => (s V_keyIDstring_z + max0(8 - s V_keyIDstring_i) <= z)%Q
   | 4 => (s V_keyIDstring_z + max0(8 - s V_keyIDstring_i) <= z)%Q
   | 5 => (s V_keyIDstring_z + max0(8 - s V_keyIDstring_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_keyIDstring_i) (7
                                                                    - 
                                                                    s V_keyIDstring_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_keyIDstring_i)]
     (s V_keyIDstring_z + max0(8 - s V_keyIDstring_i) <= z)%Q
   | 7 => (s V_keyIDstring_z <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (8 - s V_keyIDstring_i) (1)]
     (s V_keyIDstring_z + max0(8 - s V_keyIDstring_i) <= z)%Q
   | 9 => ((1 # 1) + s V_keyIDstring_z + max0(7 - s V_keyIDstring_i) <= z)%Q
   | 10 => ((1 # 1) + s V_keyIDstring_z + max0(7 - s V_keyIDstring_i) <= z)%Q
   | 11 => ((1 # 1) + s V_keyIDstring_z + max0(8 - s V_keyIDstring_i) <= z)%Q
   | 12 => ((1 # 1) + s V_keyIDstring_z + max0(8 - s V_keyIDstring_i) <= z)%Q
   | 13 => ((1 # 1) + s V_keyIDstring_z + max0(8 - s V_keyIDstring_i) <= z)%Q
   | 14 => (s V_keyIDstring_z + max0(8 - s V_keyIDstring_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_keyIDstring =>
    [mkPA Q (fun n z s => ai_keyIDstring n s /\ annot0_keyIDstring n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_keyIDstring (proc_start P_keyIDstring) s1 (proc_end P_keyIDstring) s2 ->
    (s2 V_keyIDstring_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_keyIDstring.
Qed.
