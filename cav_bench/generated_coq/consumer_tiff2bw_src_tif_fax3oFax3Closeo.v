Require Import pasta.Pasta.

Inductive proc: Type :=
  P_Fax3Close.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_Fax3Close_z := 1%positive.
Notation V_Fax3Close_code := 2%positive.
Notation V_Fax3Close_i := 3%positive.
Notation V_Fax3Close_length := 4%positive.
Notation V_Fax3Close_tif := 5%positive.
Definition Pedges_Fax3Close: list (edge proc) :=
  (EA 1 (AAssign V_Fax3Close_z (Some (ENum (0)))) 2)::(EA 2 AWeaken 3)::
  (EA 3 ANone 5)::(EA 3 ANone 4)::(EA 4 AWeaken 21)::(EA 5 (AAssign
  V_Fax3Close_code (Some (ENum (1)))) 6)::(EA 6 (AAssign V_Fax3Close_length
  (Some (ENum (12)))) 7)::(EA 7 AWeaken 8)::(EA 8 ANone 9)::(EA 8 ANone 12)::
  (EA 9 (AAssign V_Fax3Close_code None) 10)::(EA 10 (AAssign
  V_Fax3Close_length (Some (EAdd (EVar V_Fax3Close_length)
  (ENum (1))))) 11)::(EA 11 ANone 12)::(EA 12 (AAssign V_Fax3Close_i
  (Some (ENum (0)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_Fax3Close_i) s) < (eval (ENum (6))
  s))%Z)) 22)::(EA 15 (AGuard (fun s => ((eval (EVar V_Fax3Close_i) s) >=
  (eval (ENum (6)) s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 18)::
  (EA 17 ANone 19)::(EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 AWeaken 21)::
  (EA 22 AWeaken 23)::(EA 23 ANone 24)::(EA 24 (AAssign V_Fax3Close_i
  (Some (EAdd (EVar V_Fax3Close_i) (ENum (1))))) 25)::(EA 25 ANone 26)::
  (EA 26 ANone 27)::(EA 27 (AAssign V_Fax3Close_z (Some (EAdd (ENum (1))
  (EVar V_Fax3Close_z)))) 28)::(EA 28 AWeaken 15)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_Fax3Close => Pedges_Fax3Close
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_Fax3Close => 21
     end)%positive;
  var_global := var_global
}.

Definition ai_Fax3Close (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_z <= 0)%Z
   | 3 => (-1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_z <= 0)%Z
   | 4 => (1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_z <= 0)%Z
   | 5 => (1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_z <= 0)%Z
   | 6 => (-1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_code + -1 <= 0 /\ -1 * s V_Fax3Close_code + 1 <= 0)%Z
   | 7 => (-1 * s V_Fax3Close_code + 1 <= 0 /\ 1 * s V_Fax3Close_code + -1 <= 0 /\ 1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_length + -12 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0)%Z
   | 8 => (-1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_length + -12 <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_code + -1 <= 0 /\ -1 * s V_Fax3Close_code + 1 <= 0)%Z
   | 9 => (-1 * s V_Fax3Close_code + 1 <= 0 /\ 1 * s V_Fax3Close_code + -1 <= 0 /\ 1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_length + -12 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0)%Z
   | 10 => (-1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_length + -12 <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_z <= 0)%Z
   | 11 => (1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_length + 13 <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0)%Z
   | 12 => (-1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_z <= 0)%Z
   | 13 => (1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_i <= 0 /\ -1 * s V_Fax3Close_i <= 0)%Z
   | 14 => (-1 * s V_Fax3Close_i <= 0 /\ 1 * s V_Fax3Close_i <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_z <= 0)%Z
   | 15 => (-1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_i <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_i + -6 <= 0)%Z
   | 16 => (1 * s V_Fax3Close_i + -6 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_i + 6 <= 0)%Z
   | 17 => (-1 * s V_Fax3Close_i + 6 <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_i + -6 <= 0)%Z
   | 18 => (1 * s V_Fax3Close_i + -6 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_i + 6 <= 0)%Z
   | 19 => (-1 * s V_Fax3Close_i + 6 <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_i + -6 <= 0)%Z
   | 20 => (1 * s V_Fax3Close_i + -6 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_i + 6 <= 0)%Z
   | 21 => (-1 * s V_Fax3Close_z <= 0)%Z
   | 22 => (-1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_i <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_i + -5 <= 0)%Z
   | 23 => (1 * s V_Fax3Close_i + -5 <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ -1 * s V_Fax3Close_i <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0)%Z
   | 24 => (-1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_i <= 0 /\ -1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_i + -5 <= 0)%Z
   | 25 => (-1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0 /\ -1 * s V_Fax3Close_i + 1 <= 0 /\ 1 * s V_Fax3Close_i + -6 <= 0)%Z
   | 26 => (1 * s V_Fax3Close_i + -6 <= 0 /\ -1 * s V_Fax3Close_i + 1 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_z <= 0)%Z
   | 27 => (-1 * s V_Fax3Close_z <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0 /\ -1 * s V_Fax3Close_i + 1 <= 0 /\ 1 * s V_Fax3Close_i + -6 <= 0)%Z
   | 28 => (1 * s V_Fax3Close_i + -6 <= 0 /\ -1 * s V_Fax3Close_i + 1 <= 0 /\ -1 * s V_Fax3Close_length + 12 <= 0 /\ 1 * s V_Fax3Close_length + -13 <= 0 /\ -1 * s V_Fax3Close_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_Fax3Close (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((6 # 1) <= z)%Q
   | 2 => ((6 # 1) + s V_Fax3Close_z <= z)%Q
   | 3 => ((6 # 1) + s V_Fax3Close_z <= z)%Q
   | 4 => hints
     [(*-6 0*) F_one]
     ((6 # 1) + s V_Fax3Close_z <= z)%Q
   | 5 => ((6 # 1) + s V_Fax3Close_z <= z)%Q
   | 6 => ((6 # 1) + s V_Fax3Close_z <= z)%Q
   | 7 => ((6 # 1) + s V_Fax3Close_z <= z)%Q
   | 8 => ((6 # 1) + s V_Fax3Close_z <= z)%Q
   | 9 => ((6 # 1) + s V_Fax3Close_z <= z)%Q
   | 10 => ((6 # 1) + s V_Fax3Close_z <= z)%Q
   | 11 => ((6 # 1) + s V_Fax3Close_z <= z)%Q
   | 12 => ((6 # 1) + s V_Fax3Close_z <= z)%Q
   | 13 => (s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | 14 => (s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | 15 => (s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | 16 => (s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | 17 => (s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | 18 => (s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | 19 => (s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (6 - s V_Fax3Close_i) (5
                                                                   - 
                                                                   s V_Fax3Close_i));
      (*-1 0*) F_max0_ge_0 (5 - s V_Fax3Close_i)]
     (s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | 21 => (s V_Fax3Close_z <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (6 - s V_Fax3Close_i) (1)]
     (s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | 23 => ((1 # 1) + s V_Fax3Close_z + max0(5 - s V_Fax3Close_i) <= z)%Q
   | 24 => ((1 # 1) + s V_Fax3Close_z + max0(5 - s V_Fax3Close_i) <= z)%Q
   | 25 => ((1 # 1) + s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | 26 => ((1 # 1) + s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | 27 => ((1 # 1) + s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | 28 => (s V_Fax3Close_z + max0(6 - s V_Fax3Close_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_Fax3Close =>
    [mkPA Q (fun n z s => ai_Fax3Close n s /\ annot0_Fax3Close n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_Fax3Close (proc_start P_Fax3Close) s1 (proc_end P_Fax3Close) s2 ->
    (s2 V_Fax3Close_z <= (6 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_Fax3Close.
Qed.
