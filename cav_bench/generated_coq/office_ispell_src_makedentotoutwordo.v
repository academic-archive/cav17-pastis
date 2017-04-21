Require Import pasta.Pasta.

Inductive proc: Type :=
  P_toutword.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_toutword_z := 1%positive.
Notation V_toutword_bit := 2%positive.
Notation V_toutword_has_marker := 3%positive.
Notation V_toutword_cent := 4%positive.
Notation V_toutword_toutfile := 5%positive.
Notation V_toutword_word := 6%positive.
Definition Pedges_toutword: list (edge proc) :=
  (EA 1 (AAssign V_toutword_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_toutword_has_marker (Some (ENum (0)))) 3)::(EA 3 (AAssign V_toutword_bit
  (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_toutword_bit) s) < (eval (ENum (26)) s))%Z)) 9)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_toutword_bit) s) >=
  (eval (ENum (26)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 ANone 11)::(EA 10 ANone 12)::(EA 11 ANone 12)::(EA 12 ANone 13)::
  (EA 13 (AAssign V_toutword_bit (Some (EAdd (EVar V_toutword_bit)
  (ENum (1))))) 14)::(EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_toutword_z (Some (EAdd (ENum (1)) (EVar V_toutword_z)))) 17)::
  (EA 17 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_toutword => Pedges_toutword
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_toutword => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_toutword (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_toutword_z <= 0 /\ -1 * s V_toutword_z <= 0)%Z
   | 3 => (-1 * s V_toutword_z <= 0 /\ 1 * s V_toutword_z <= 0 /\ 1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_has_marker <= 0)%Z
   | 4 => (-1 * s V_toutword_has_marker <= 0 /\ 1 * s V_toutword_has_marker <= 0 /\ 1 * s V_toutword_z <= 0 /\ -1 * s V_toutword_z <= 0 /\ 1 * s V_toutword_bit <= 0 /\ -1 * s V_toutword_bit <= 0)%Z
   | 5 => (-1 * s V_toutword_bit <= 0 /\ 1 * s V_toutword_bit <= 0 /\ -1 * s V_toutword_z <= 0 /\ 1 * s V_toutword_z <= 0 /\ 1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_has_marker <= 0)%Z
   | 6 => (-1 * s V_toutword_z <= 0 /\ -1 * s V_toutword_bit <= 0 /\ -1 * s V_toutword_has_marker <= 0 /\ 1 * s V_toutword_has_marker <= 0 /\ 1 * s V_toutword_bit + -26 <= 0)%Z
   | 7 => (1 * s V_toutword_bit + -26 <= 0 /\ 1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_z <= 0 /\ -1 * s V_toutword_bit + 26 <= 0)%Z
   | 8 => (-1 * s V_toutword_bit + 26 <= 0 /\ -1 * s V_toutword_z <= 0 /\ -1 * s V_toutword_has_marker <= 0 /\ 1 * s V_toutword_has_marker <= 0 /\ 1 * s V_toutword_bit + -26 <= 0)%Z
   | 9 => (1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_bit <= 0 /\ -1 * s V_toutword_z <= 0 /\ 1 * s V_toutword_bit + -25 <= 0)%Z
   | 10 => (1 * s V_toutword_bit + -25 <= 0 /\ -1 * s V_toutword_z <= 0 /\ -1 * s V_toutword_bit <= 0 /\ -1 * s V_toutword_has_marker <= 0 /\ 1 * s V_toutword_has_marker <= 0)%Z
   | 11 => (1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_bit <= 0 /\ -1 * s V_toutword_z <= 0 /\ 1 * s V_toutword_bit + -25 <= 0)%Z
   | 12 => (1 * s V_toutword_bit + -25 <= 0 /\ -1 * s V_toutword_z <= 0 /\ -1 * s V_toutword_bit <= 0 /\ -1 * s V_toutword_has_marker <= 0 /\ 1 * s V_toutword_has_marker <= 0)%Z
   | 13 => (1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_bit <= 0 /\ -1 * s V_toutword_z <= 0 /\ 1 * s V_toutword_bit + -25 <= 0)%Z
   | 14 => (-1 * s V_toutword_z <= 0 /\ -1 * s V_toutword_has_marker <= 0 /\ 1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_bit + 1 <= 0 /\ 1 * s V_toutword_bit + -26 <= 0)%Z
   | 15 => (1 * s V_toutword_bit + -26 <= 0 /\ -1 * s V_toutword_bit + 1 <= 0 /\ 1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_z <= 0)%Z
   | 16 => (-1 * s V_toutword_z <= 0 /\ -1 * s V_toutword_has_marker <= 0 /\ 1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_bit + 1 <= 0 /\ 1 * s V_toutword_bit + -26 <= 0)%Z
   | 17 => (1 * s V_toutword_bit + -26 <= 0 /\ -1 * s V_toutword_bit + 1 <= 0 /\ 1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_has_marker <= 0 /\ -1 * s V_toutword_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_toutword (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((26 # 1) <= z)%Q
   | 2 => ((26 # 1) + s V_toutword_z <= z)%Q
   | 3 => ((26 # 1) + s V_toutword_z <= z)%Q
   | 4 => (s V_toutword_z + max0(26 - s V_toutword_bit) <= z)%Q
   | 5 => (s V_toutword_z + max0(26 - s V_toutword_bit) <= z)%Q
   | 6 => (s V_toutword_z + max0(26 - s V_toutword_bit) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (26 - s V_toutword_bit) (25
                                                                    - 
                                                                    s V_toutword_bit));
      (*-1 0*) F_max0_ge_0 (25 - s V_toutword_bit)]
     (s V_toutword_z + max0(26 - s V_toutword_bit) <= z)%Q
   | 8 => (s V_toutword_z <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (26 - s V_toutword_bit) (1)]
     (s V_toutword_z + max0(26 - s V_toutword_bit) <= z)%Q
   | 10 => ((1 # 1) + s V_toutword_z + max0(25 - s V_toutword_bit) <= z)%Q
   | 11 => ((1 # 1) + s V_toutword_z + max0(25 - s V_toutword_bit) <= z)%Q
   | 12 => ((1 # 1) + s V_toutword_z + max0(25 - s V_toutword_bit) <= z)%Q
   | 13 => ((1 # 1) + s V_toutword_z + max0(25 - s V_toutword_bit) <= z)%Q
   | 14 => ((1 # 1) + s V_toutword_z + max0(26 - s V_toutword_bit) <= z)%Q
   | 15 => ((1 # 1) + s V_toutword_z + max0(26 - s V_toutword_bit) <= z)%Q
   | 16 => ((1 # 1) + s V_toutword_z + max0(26 - s V_toutword_bit) <= z)%Q
   | 17 => (s V_toutword_z + max0(26 - s V_toutword_bit) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_toutword =>
    [mkPA Q (fun n z s => ai_toutword n s /\ annot0_toutword n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_toutword (proc_start P_toutword) s1 (proc_end P_toutword) s2 ->
    (s2 V_toutword_z <= (26 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_toutword.
Qed.
