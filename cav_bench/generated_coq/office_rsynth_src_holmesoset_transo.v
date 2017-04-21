Require Import pasta.Pasta.

Inductive proc: Type :=
  P_set_trans.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_set_trans_z := 1%positive.
Notation V_set_trans__tmp := 2%positive.
Notation V_set_trans__tmp1 := 3%positive.
Notation V_set_trans_i := 4%positive.
Notation V_set_trans_a := 5%positive.
Notation V_set_trans_b := 6%positive.
Notation V_set_trans_e := 7%positive.
Notation V_set_trans_ext := 8%positive.
Notation V_set_trans_t := 9%positive.
Definition Pedges_set_trans: list (edge proc) :=
  (EA 1 (AAssign V_set_trans_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_set_trans__tmp (Some (EVar V_set_trans_ext))) 3)::(EA 3 (AAssign
  V_set_trans__tmp1 (Some (EVar V_set_trans_e))) 4)::(EA 4 (AAssign
  V_set_trans_i (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::
  (EA 7 (AGuard (fun s => ((eval (EVar V_set_trans_i) s) < (eval (ENum (19))
  s))%Z)) 10)::(EA 7 (AGuard (fun s => ((eval (EVar V_set_trans_i) s) >=
  (eval (ENum (19)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 10 AWeaken 11)::
  (EA 11 (AGuard (fun s => ((eval (EVar V_set_trans__tmp) s) <>
  (eval (ENum (0)) s))%Z)) 15)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_set_trans__tmp) s) = (eval (ENum (0))
  s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 ANone 14)::(EA 14 AWeaken 18)::
  (EA 15 AWeaken 16)::(EA 16 ANone 17)::(EA 17 AWeaken 18)::
  (EA 18 ANone 20)::(EA 18 ANone 19)::(EA 19 ANone 21)::(EA 20 ANone 21)::
  (EA 21 ANone 22)::(EA 22 (AAssign V_set_trans_i
  (Some (EAdd (EVar V_set_trans_i) (ENum (1))))) 23)::(EA 23 ANone 24)::
  (EA 24 ANone 25)::(EA 25 (AAssign V_set_trans_z (Some (EAdd (ENum (1))
  (EVar V_set_trans_z)))) 26)::(EA 26 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_set_trans => Pedges_set_trans
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_set_trans => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_set_trans (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_set_trans_z <= 0 /\ -1 * s V_set_trans_z <= 0)%Z
   | 3 => (-1 * s V_set_trans_z <= 0 /\ 1 * s V_set_trans_z <= 0)%Z
   | 4 => (1 * s V_set_trans_z <= 0 /\ -1 * s V_set_trans_z <= 0)%Z
   | 5 => (-1 * s V_set_trans_z <= 0 /\ 1 * s V_set_trans_z <= 0 /\ 1 * s V_set_trans_i <= 0 /\ -1 * s V_set_trans_i <= 0)%Z
   | 6 => (-1 * s V_set_trans_i <= 0 /\ 1 * s V_set_trans_i <= 0 /\ 1 * s V_set_trans_z <= 0 /\ -1 * s V_set_trans_z <= 0)%Z
   | 7 => (-1 * s V_set_trans_z <= 0 /\ -1 * s V_set_trans_i <= 0 /\ 1 * s V_set_trans_i + -19 <= 0)%Z
   | 8 => (1 * s V_set_trans_i + -19 <= 0 /\ -1 * s V_set_trans_z <= 0 /\ -1 * s V_set_trans_i + 19 <= 0)%Z
   | 9 => (-1 * s V_set_trans_i + 19 <= 0 /\ -1 * s V_set_trans_z <= 0 /\ 1 * s V_set_trans_i + -19 <= 0)%Z
   | 10 => (-1 * s V_set_trans_i <= 0 /\ -1 * s V_set_trans_z <= 0 /\ 1 * s V_set_trans_i + -18 <= 0)%Z
   | 11 => (1 * s V_set_trans_i + -18 <= 0 /\ -1 * s V_set_trans_z <= 0 /\ -1 * s V_set_trans_i <= 0)%Z
   | 12 => (-1 * s V_set_trans_i <= 0 /\ -1 * s V_set_trans_z <= 0 /\ 1 * s V_set_trans_i + -18 <= 0 /\ 1 * s V_set_trans__tmp <= 0 /\ -1 * s V_set_trans__tmp <= 0)%Z
   | 13 => (-1 * s V_set_trans__tmp <= 0 /\ 1 * s V_set_trans__tmp <= 0 /\ 1 * s V_set_trans_i + -18 <= 0 /\ -1 * s V_set_trans_z <= 0 /\ -1 * s V_set_trans_i <= 0)%Z
   | 14 => (-1 * s V_set_trans_i <= 0 /\ -1 * s V_set_trans_z <= 0 /\ 1 * s V_set_trans_i + -18 <= 0 /\ 1 * s V_set_trans__tmp <= 0 /\ -1 * s V_set_trans__tmp <= 0)%Z
   | 15 => (-1 * s V_set_trans_i <= 0 /\ -1 * s V_set_trans_z <= 0 /\ 1 * s V_set_trans_i + -18 <= 0)%Z
   | 16 => (1 * s V_set_trans_i + -18 <= 0 /\ -1 * s V_set_trans_z <= 0 /\ -1 * s V_set_trans_i <= 0)%Z
   | 17 => (-1 * s V_set_trans_i <= 0 /\ -1 * s V_set_trans_z <= 0 /\ 1 * s V_set_trans_i + -18 <= 0)%Z
   | 18 => (1 * s V_set_trans_i + -18 <= 0 /\ -1 * s V_set_trans_z <= 0 /\ -1 * s V_set_trans_i <= 0)%Z
   | 19 => (-1 * s V_set_trans_i <= 0 /\ -1 * s V_set_trans_z <= 0 /\ 1 * s V_set_trans_i + -18 <= 0)%Z
   | 20 => (-1 * s V_set_trans_i <= 0 /\ -1 * s V_set_trans_z <= 0 /\ 1 * s V_set_trans_i + -18 <= 0)%Z
   | 21 => (1 * s V_set_trans_i + -18 <= 0 /\ -1 * s V_set_trans_z <= 0 /\ -1 * s V_set_trans_i <= 0)%Z
   | 22 => (-1 * s V_set_trans_i <= 0 /\ -1 * s V_set_trans_z <= 0 /\ 1 * s V_set_trans_i + -18 <= 0)%Z
   | 23 => (-1 * s V_set_trans_z <= 0 /\ -1 * s V_set_trans_i + 1 <= 0 /\ 1 * s V_set_trans_i + -19 <= 0)%Z
   | 24 => (1 * s V_set_trans_i + -19 <= 0 /\ -1 * s V_set_trans_i + 1 <= 0 /\ -1 * s V_set_trans_z <= 0)%Z
   | 25 => (-1 * s V_set_trans_z <= 0 /\ -1 * s V_set_trans_i + 1 <= 0 /\ 1 * s V_set_trans_i + -19 <= 0)%Z
   | 26 => (1 * s V_set_trans_i + -19 <= 0 /\ -1 * s V_set_trans_i + 1 <= 0 /\ -1 * s V_set_trans_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_set_trans (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((19 # 1) <= z)%Q
   | 2 => ((19 # 1) + s V_set_trans_z <= z)%Q
   | 3 => ((19 # 1) + s V_set_trans_z <= z)%Q
   | 4 => ((19 # 1) + s V_set_trans_z <= z)%Q
   | 5 => (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 6 => (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 7 => (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (19 - s V_set_trans_i) (18
                                                                    - 
                                                                    s V_set_trans_i));
      (*-1 0*) F_max0_ge_0 (18 - s V_set_trans_i)]
     (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 9 => (s V_set_trans_z <= z)%Q
   | 10 => (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 11 => (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 12 => (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 13 => (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 14 => hints
     [(*0 1*) F_max0_pre_decrement 1 (19 - s V_set_trans_i) (1)]
     (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 15 => (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 16 => (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (19 - s V_set_trans_i) (1)]
     (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 18 => ((1 # 1) + s V_set_trans_z + max0(18 - s V_set_trans_i) <= z)%Q
   | 19 => ((1 # 1) + s V_set_trans_z + max0(18 - s V_set_trans_i) <= z)%Q
   | 20 => ((1 # 1) + s V_set_trans_z + max0(18 - s V_set_trans_i) <= z)%Q
   | 21 => ((1 # 1) + s V_set_trans_z + max0(18 - s V_set_trans_i) <= z)%Q
   | 22 => ((1 # 1) + s V_set_trans_z + max0(18 - s V_set_trans_i) <= z)%Q
   | 23 => ((1 # 1) + s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 24 => ((1 # 1) + s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 25 => ((1 # 1) + s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | 26 => (s V_set_trans_z + max0(19 - s V_set_trans_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_set_trans =>
    [mkPA Q (fun n z s => ai_set_trans n s /\ annot0_set_trans n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_set_trans (proc_start P_set_trans) s1 (proc_end P_set_trans) s2 ->
    (s2 V_set_trans_z <= (19 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_set_trans.
Qed.
