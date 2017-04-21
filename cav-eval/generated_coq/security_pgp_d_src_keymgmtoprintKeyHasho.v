Require Import pasta.Pasta.

Inductive proc: Type :=
  P_printKeyHash.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_printKeyHash_z := 1%positive.
Notation V_printKeyHash__tmp := 2%positive.
Notation V_printKeyHash_i := 3%positive.
Notation V_printKeyHash_hash := 4%positive.
Notation V_printKeyHash_indent := 5%positive.
Definition Pedges_printKeyHash: list (edge proc) :=
  (EA 1 (AAssign V_printKeyHash_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_printKeyHash__tmp (Some (EVar V_printKeyHash_indent))) 3)::(EA 3 (AAssign
  V_printKeyHash_i (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_printKeyHash_i) s) <
  (eval (ENum (8)) s))%Z)) 21)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_printKeyHash_i) s) >= (eval (ENum (8))
  s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign V_printKeyHash_i
  (Some (ENum (8)))) 9)::(EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_printKeyHash_i) s) < (eval (ENum (16))
  s))%Z)) 14)::(EA 11 (AGuard (fun s => ((eval (EVar V_printKeyHash_i) s) >=
  (eval (ENum (16)) s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 14 AWeaken 15)::
  (EA 15 ANone 16)::(EA 16 (AAssign V_printKeyHash_i
  (Some (EAdd (EVar V_printKeyHash_i) (ENum (1))))) 17)::(EA 17 ANone 18)::
  (EA 18 ANone 19)::(EA 19 (AAssign V_printKeyHash_z (Some (EAdd (ENum (1))
  (EVar V_printKeyHash_z)))) 20)::(EA 20 AWeaken 11)::(EA 21 AWeaken 22)::
  (EA 22 ANone 23)::(EA 23 (AAssign V_printKeyHash_i
  (Some (EAdd (EVar V_printKeyHash_i) (ENum (1))))) 24)::(EA 24 ANone 25)::
  (EA 25 ANone 26)::(EA 26 (AAssign V_printKeyHash_z (Some (EAdd (ENum (1))
  (EVar V_printKeyHash_z)))) 27)::(EA 27 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_printKeyHash => Pedges_printKeyHash
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_printKeyHash => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_printKeyHash (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_printKeyHash_z <= 0 /\ -1 * s V_printKeyHash_z <= 0)%Z
   | 3 => (-1 * s V_printKeyHash_z <= 0 /\ 1 * s V_printKeyHash_z <= 0)%Z
   | 4 => (1 * s V_printKeyHash_z <= 0 /\ -1 * s V_printKeyHash_z <= 0 /\ 1 * s V_printKeyHash_i <= 0 /\ -1 * s V_printKeyHash_i <= 0)%Z
   | 5 => (-1 * s V_printKeyHash_i <= 0 /\ 1 * s V_printKeyHash_i <= 0 /\ -1 * s V_printKeyHash_z <= 0 /\ 1 * s V_printKeyHash_z <= 0)%Z
   | 6 => (-1 * s V_printKeyHash_z <= 0 /\ -1 * s V_printKeyHash_i <= 0 /\ 1 * s V_printKeyHash_i + -8 <= 0)%Z
   | 7 => (1 * s V_printKeyHash_i + -8 <= 0 /\ -1 * s V_printKeyHash_z <= 0 /\ -1 * s V_printKeyHash_i + 8 <= 0)%Z
   | 8 => (-1 * s V_printKeyHash_i + 8 <= 0 /\ -1 * s V_printKeyHash_z <= 0 /\ 1 * s V_printKeyHash_i + -8 <= 0)%Z
   | 9 => (-1 * s V_printKeyHash_z <= 0 /\ 1 * s V_printKeyHash_i + -8 <= 0 /\ -1 * s V_printKeyHash_i + 8 <= 0)%Z
   | 10 => (-1 * s V_printKeyHash_i + 8 <= 0 /\ 1 * s V_printKeyHash_i + -8 <= 0 /\ -1 * s V_printKeyHash_z <= 0)%Z
   | 11 => (-1 * s V_printKeyHash_z <= 0 /\ -1 * s V_printKeyHash_i + 8 <= 0 /\ 1 * s V_printKeyHash_i + -16 <= 0)%Z
   | 12 => (1 * s V_printKeyHash_i + -16 <= 0 /\ -1 * s V_printKeyHash_z <= 0 /\ -1 * s V_printKeyHash_i + 16 <= 0)%Z
   | 13 => (-1 * s V_printKeyHash_i + 16 <= 0 /\ -1 * s V_printKeyHash_z <= 0 /\ 1 * s V_printKeyHash_i + -16 <= 0)%Z
   | 14 => (-1 * s V_printKeyHash_i + 8 <= 0 /\ -1 * s V_printKeyHash_z <= 0 /\ 1 * s V_printKeyHash_i + -15 <= 0)%Z
   | 15 => (1 * s V_printKeyHash_i + -15 <= 0 /\ -1 * s V_printKeyHash_z <= 0 /\ -1 * s V_printKeyHash_i + 8 <= 0)%Z
   | 16 => (-1 * s V_printKeyHash_i + 8 <= 0 /\ -1 * s V_printKeyHash_z <= 0 /\ 1 * s V_printKeyHash_i + -15 <= 0)%Z
   | 17 => (-1 * s V_printKeyHash_z <= 0 /\ -1 * s V_printKeyHash_i + 9 <= 0 /\ 1 * s V_printKeyHash_i + -16 <= 0)%Z
   | 18 => (1 * s V_printKeyHash_i + -16 <= 0 /\ -1 * s V_printKeyHash_i + 9 <= 0 /\ -1 * s V_printKeyHash_z <= 0)%Z
   | 19 => (-1 * s V_printKeyHash_z <= 0 /\ -1 * s V_printKeyHash_i + 9 <= 0 /\ 1 * s V_printKeyHash_i + -16 <= 0)%Z
   | 20 => (1 * s V_printKeyHash_i + -16 <= 0 /\ -1 * s V_printKeyHash_i + 9 <= 0 /\ -1 * s V_printKeyHash_z + 1 <= 0)%Z
   | 21 => (-1 * s V_printKeyHash_i <= 0 /\ -1 * s V_printKeyHash_z <= 0 /\ 1 * s V_printKeyHash_i + -7 <= 0)%Z
   | 22 => (1 * s V_printKeyHash_i + -7 <= 0 /\ -1 * s V_printKeyHash_z <= 0 /\ -1 * s V_printKeyHash_i <= 0)%Z
   | 23 => (-1 * s V_printKeyHash_i <= 0 /\ -1 * s V_printKeyHash_z <= 0 /\ 1 * s V_printKeyHash_i + -7 <= 0)%Z
   | 24 => (-1 * s V_printKeyHash_z <= 0 /\ -1 * s V_printKeyHash_i + 1 <= 0 /\ 1 * s V_printKeyHash_i + -8 <= 0)%Z
   | 25 => (1 * s V_printKeyHash_i + -8 <= 0 /\ -1 * s V_printKeyHash_i + 1 <= 0 /\ -1 * s V_printKeyHash_z <= 0)%Z
   | 26 => (-1 * s V_printKeyHash_z <= 0 /\ -1 * s V_printKeyHash_i + 1 <= 0 /\ 1 * s V_printKeyHash_i + -8 <= 0)%Z
   | 27 => (1 * s V_printKeyHash_i + -8 <= 0 /\ -1 * s V_printKeyHash_i + 1 <= 0 /\ -1 * s V_printKeyHash_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_printKeyHash (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((16 # 1) <= z)%Q
   | 2 => ((16 # 1) + s V_printKeyHash_z <= z)%Q
   | 3 => ((16 # 1) + s V_printKeyHash_z <= z)%Q
   | 4 => ((8 # 1) + s V_printKeyHash_z + max0(8 - s V_printKeyHash_i) <= z)%Q
   | 5 => ((8 # 1) + s V_printKeyHash_z + max0(8 - s V_printKeyHash_i) <= z)%Q
   | 6 => ((8 # 1) + s V_printKeyHash_z + max0(8 - s V_printKeyHash_i) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_printKeyHash_i) (7
                                                                    - s V_printKeyHash_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_printKeyHash_i)]
     ((8 # 1) + s V_printKeyHash_z + max0(8 - s V_printKeyHash_i) <= z)%Q
   | 8 => ((8 # 1) + s V_printKeyHash_z <= z)%Q
   | 9 => (s V_printKeyHash_z + max0(16 - s V_printKeyHash_i) <= z)%Q
   | 10 => (s V_printKeyHash_z + max0(16 - s V_printKeyHash_i) <= z)%Q
   | 11 => (s V_printKeyHash_z + max0(16 - s V_printKeyHash_i) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (16 - s V_printKeyHash_i) (15
                                                                    - s V_printKeyHash_i));
      (*-1 0*) F_max0_ge_0 (15 - s V_printKeyHash_i)]
     (s V_printKeyHash_z + max0(16 - s V_printKeyHash_i) <= z)%Q
   | 13 => (s V_printKeyHash_z <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (16 - s V_printKeyHash_i) (1)]
     (s V_printKeyHash_z + max0(16 - s V_printKeyHash_i) <= z)%Q
   | 15 => ((1 # 1) + s V_printKeyHash_z + max0(15 - s V_printKeyHash_i) <= z)%Q
   | 16 => ((1 # 1) + s V_printKeyHash_z + max0(15 - s V_printKeyHash_i) <= z)%Q
   | 17 => ((1 # 1) + s V_printKeyHash_z + max0(16 - s V_printKeyHash_i) <= z)%Q
   | 18 => ((1 # 1) + s V_printKeyHash_z + max0(16 - s V_printKeyHash_i) <= z)%Q
   | 19 => ((1 # 1) + s V_printKeyHash_z + max0(16 - s V_printKeyHash_i) <= z)%Q
   | 20 => (s V_printKeyHash_z + max0(16 - s V_printKeyHash_i) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (8 - s V_printKeyHash_i) (1)]
     ((8 # 1) + s V_printKeyHash_z + max0(8 - s V_printKeyHash_i) <= z)%Q
   | 22 => ((9 # 1) + s V_printKeyHash_z + max0(7 - s V_printKeyHash_i) <= z)%Q
   | 23 => ((9 # 1) + s V_printKeyHash_z + max0(7 - s V_printKeyHash_i) <= z)%Q
   | 24 => ((9 # 1) + s V_printKeyHash_z + max0(8 - s V_printKeyHash_i) <= z)%Q
   | 25 => ((9 # 1) + s V_printKeyHash_z + max0(8 - s V_printKeyHash_i) <= z)%Q
   | 26 => ((9 # 1) + s V_printKeyHash_z + max0(8 - s V_printKeyHash_i) <= z)%Q
   | 27 => ((8 # 1) + s V_printKeyHash_z + max0(8 - s V_printKeyHash_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_printKeyHash =>
    [mkPA Q (fun n z s => ai_printKeyHash n s /\ annot0_printKeyHash n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_printKeyHash (proc_start P_printKeyHash) s1 (proc_end P_printKeyHash) s2 ->
    (s2 V_printKeyHash_z <= (16 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_printKeyHash.
Qed.
