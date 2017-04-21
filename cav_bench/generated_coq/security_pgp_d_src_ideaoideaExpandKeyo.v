Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ideaExpandKey.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ideaExpandKey_z := 1%positive.
Notation V_ideaExpandKey_i := 2%positive.
Notation V_ideaExpandKey_j := 3%positive.
Notation V_ideaExpandKey_EK := 4%positive.
Notation V_ideaExpandKey_userkey := 5%positive.
Definition Pedges_ideaExpandKey: list (edge proc) :=
  (EA 1 (AAssign V_ideaExpandKey_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_ideaExpandKey_j (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_ideaExpandKey_j) s) <
  (eval (ENum (8)) s))%Z)) 22)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_ideaExpandKey_j) s) >= (eval (ENum (8))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign V_ideaExpandKey_i
  (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_ideaExpandKey_j) s) < (eval (ENum (52))
  s))%Z)) 13)::(EA 10 (AGuard (fun s => ((eval (EVar V_ideaExpandKey_j) s) >=
  (eval (ENum (52)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 13 AWeaken 14)::
  (EA 14 (AAssign V_ideaExpandKey_i (Some (EAdd (EVar V_ideaExpandKey_i)
  (ENum (1))))) 15)::(EA 15 (AAssign V_ideaExpandKey_i None) 16)::
  (EA 16 ANone 17)::(EA 17 (AAssign V_ideaExpandKey_j
  (Some (EAdd (EVar V_ideaExpandKey_j) (ENum (1))))) 18)::(EA 18 ANone 19)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_ideaExpandKey_z (Some (EAdd (ENum (1))
  (EVar V_ideaExpandKey_z)))) 21)::(EA 21 AWeaken 10)::(EA 22 AWeaken 23)::
  (EA 23 ANone 24)::(EA 24 (AAssign V_ideaExpandKey_j
  (Some (EAdd (EVar V_ideaExpandKey_j) (ENum (1))))) 25)::(EA 25 ANone 26)::
  (EA 26 ANone 27)::(EA 27 (AAssign V_ideaExpandKey_z (Some (EAdd (ENum (1))
  (EVar V_ideaExpandKey_z)))) 28)::(EA 28 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ideaExpandKey => Pedges_ideaExpandKey
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ideaExpandKey => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_ideaExpandKey (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_z <= 0)%Z
   | 3 => (-1 * s V_ideaExpandKey_z <= 0 /\ 1 * s V_ideaExpandKey_z <= 0 /\ 1 * s V_ideaExpandKey_j <= 0 /\ -1 * s V_ideaExpandKey_j <= 0)%Z
   | 4 => (-1 * s V_ideaExpandKey_j <= 0 /\ 1 * s V_ideaExpandKey_j <= 0 /\ 1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_z <= 0)%Z
   | 5 => (-1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_j <= 0 /\ 1 * s V_ideaExpandKey_j + -8 <= 0)%Z
   | 6 => (1 * s V_ideaExpandKey_j + -8 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_j + 8 <= 0)%Z
   | 7 => (-1 * s V_ideaExpandKey_j + 8 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ 1 * s V_ideaExpandKey_j + -8 <= 0)%Z
   | 8 => (1 * s V_ideaExpandKey_j + -8 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_j + 8 <= 0 /\ 1 * s V_ideaExpandKey_i <= 0 /\ -1 * s V_ideaExpandKey_i <= 0)%Z
   | 9 => (-1 * s V_ideaExpandKey_i <= 0 /\ 1 * s V_ideaExpandKey_i <= 0 /\ -1 * s V_ideaExpandKey_j + 8 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ 1 * s V_ideaExpandKey_j + -8 <= 0)%Z
   | 10 => (-1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_j + 8 <= 0 /\ 1 * s V_ideaExpandKey_j + -52 <= 0)%Z
   | 11 => (1 * s V_ideaExpandKey_j + -52 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_j + 52 <= 0)%Z
   | 12 => (-1 * s V_ideaExpandKey_j + 52 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ 1 * s V_ideaExpandKey_j + -52 <= 0)%Z
   | 13 => (-1 * s V_ideaExpandKey_j + 8 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ 1 * s V_ideaExpandKey_j + -51 <= 0)%Z
   | 14 => (1 * s V_ideaExpandKey_j + -51 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_j + 8 <= 0)%Z
   | 15 => (-1 * s V_ideaExpandKey_j + 8 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ 1 * s V_ideaExpandKey_j + -51 <= 0)%Z
   | 16 => (1 * s V_ideaExpandKey_j + -51 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_j + 8 <= 0)%Z
   | 17 => (-1 * s V_ideaExpandKey_j + 8 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ 1 * s V_ideaExpandKey_j + -51 <= 0)%Z
   | 18 => (-1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_j + 9 <= 0 /\ 1 * s V_ideaExpandKey_j + -52 <= 0)%Z
   | 19 => (1 * s V_ideaExpandKey_j + -52 <= 0 /\ -1 * s V_ideaExpandKey_j + 9 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0)%Z
   | 20 => (-1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_j + 9 <= 0 /\ 1 * s V_ideaExpandKey_j + -52 <= 0)%Z
   | 21 => (1 * s V_ideaExpandKey_j + -52 <= 0 /\ -1 * s V_ideaExpandKey_j + 9 <= 0 /\ -1 * s V_ideaExpandKey_z + 1 <= 0)%Z
   | 22 => (-1 * s V_ideaExpandKey_j <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ 1 * s V_ideaExpandKey_j + -7 <= 0)%Z
   | 23 => (1 * s V_ideaExpandKey_j + -7 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_j <= 0)%Z
   | 24 => (-1 * s V_ideaExpandKey_j <= 0 /\ -1 * s V_ideaExpandKey_z <= 0 /\ 1 * s V_ideaExpandKey_j + -7 <= 0)%Z
   | 25 => (-1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_j + 1 <= 0 /\ 1 * s V_ideaExpandKey_j + -8 <= 0)%Z
   | 26 => (1 * s V_ideaExpandKey_j + -8 <= 0 /\ -1 * s V_ideaExpandKey_j + 1 <= 0 /\ -1 * s V_ideaExpandKey_z <= 0)%Z
   | 27 => (-1 * s V_ideaExpandKey_z <= 0 /\ -1 * s V_ideaExpandKey_j + 1 <= 0 /\ 1 * s V_ideaExpandKey_j + -8 <= 0)%Z
   | 28 => (1 * s V_ideaExpandKey_j + -8 <= 0 /\ -1 * s V_ideaExpandKey_j + 1 <= 0 /\ -1 * s V_ideaExpandKey_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ideaExpandKey (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((52 # 1) <= z)%Q
   | 2 => ((52 # 1) + s V_ideaExpandKey_z <= z)%Q
   | 3 => (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 4 => (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 5 => (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 6 => (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 7 => (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 8 => (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 9 => (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 10 => (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (52 - s V_ideaExpandKey_j) (51
                                                                    - s V_ideaExpandKey_j));
      (*-1 0*) F_max0_ge_0 (51 - s V_ideaExpandKey_j)]
     (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 12 => (s V_ideaExpandKey_z <= z)%Q
   | 13 => hints
     [(*0 1*) F_max0_pre_decrement 1 (52 - s V_ideaExpandKey_j) (1)]
     (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 14 => ((1 # 1) + s V_ideaExpandKey_z + max0(51 - s V_ideaExpandKey_j) <= z)%Q
   | 15 => ((1 # 1) + s V_ideaExpandKey_z + max0(51 - s V_ideaExpandKey_j) <= z)%Q
   | 16 => ((1 # 1) + s V_ideaExpandKey_z + max0(51 - s V_ideaExpandKey_j) <= z)%Q
   | 17 => ((1 # 1) + s V_ideaExpandKey_z + max0(51 - s V_ideaExpandKey_j) <= z)%Q
   | 18 => ((1 # 1) + s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 19 => ((1 # 1) + s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 20 => ((1 # 1) + s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 21 => (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 22 => hints
     [(*3.76378e-10 1*) F_max0_pre_decrement 1 (52 - s V_ideaExpandKey_j) (1)]
     (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 23 => ((1 # 1) + s V_ideaExpandKey_z + max0(51 - s V_ideaExpandKey_j) <= z)%Q
   | 24 => ((1 # 1) + s V_ideaExpandKey_z + max0(51 - s V_ideaExpandKey_j) <= z)%Q
   | 25 => ((1 # 1) + s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 26 => ((1 # 1) + s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 27 => ((1 # 1) + s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | 28 => (s V_ideaExpandKey_z + max0(52 - s V_ideaExpandKey_j) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ideaExpandKey =>
    [mkPA Q (fun n z s => ai_ideaExpandKey n s /\ annot0_ideaExpandKey n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ideaExpandKey (proc_start P_ideaExpandKey) s1 (proc_end P_ideaExpandKey) s2 ->
    (s2 V_ideaExpandKey_z <= (52 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ideaExpandKey.
Qed.
