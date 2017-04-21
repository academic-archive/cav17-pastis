Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gstate_unshare.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gstate_unshare_z := 1%positive.
Notation V_gstate_unshare__tmp := 2%positive.
Notation V_gstate_unshare_i := 3%positive.
Notation V_gstate_unshare_op := 4%positive.
Definition Pedges_gstate_unshare: list (edge proc) :=
  (EA 1 (AAssign V_gstate_unshare_z (Some (ENum (0)))) 2)::(EA 2 AWeaken 3)::
  (EA 3 ANone 7)::(EA 3 ANone 4)::(EA 4 (AAssign V_gstate_unshare__tmp
  (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 25)::
  (EA 7 AWeaken 8)::(EA 8 ANone 22)::(EA 8 ANone 9)::(EA 9 (AAssign
  V_gstate_unshare_i (Some (ENum (25)))) 10)::(EA 10 ANone 11)::
  (EA 11 ANone 12)::(EA 12 (AAssign V_gstate_unshare_i
  (Some (EAdd (EVar V_gstate_unshare_i) (ENum (-1))))) 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EAdd (EVar V_gstate_unshare_i) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 19)::(EA 14 (AGuard
  (fun s => ((eval (EAdd (EVar V_gstate_unshare_i) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 16 (AAssign
  V_gstate_unshare__tmp (Some (ENum (0)))) 17)::(EA 17 ANone 18)::
  (EA 18 AWeaken 25)::(EA 19 AWeaken 20)::(EA 20 ANone 21)::(EA 21 (AAssign
  V_gstate_unshare_z (Some (EAdd (ENum (1))
  (EVar V_gstate_unshare_z)))) 11)::(EA 22 (AAssign V_gstate_unshare__tmp
  (Some (ENum (-25)))) 23)::(EA 23 ANone 24)::(EA 24 AWeaken 25)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gstate_unshare => Pedges_gstate_unshare
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gstate_unshare => 25
     end)%positive;
  var_global := var_global
}.

Definition ai_gstate_unshare (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gstate_unshare_z <= 0 /\ -1 * s V_gstate_unshare_z <= 0)%Z
   | 3 => (-1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare_z <= 0)%Z
   | 4 => (1 * s V_gstate_unshare_z <= 0 /\ -1 * s V_gstate_unshare_z <= 0)%Z
   | 5 => (-1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare__tmp <= 0 /\ -1 * s V_gstate_unshare__tmp <= 0)%Z
   | 6 => (-1 * s V_gstate_unshare__tmp <= 0 /\ 1 * s V_gstate_unshare__tmp <= 0 /\ 1 * s V_gstate_unshare_z <= 0 /\ -1 * s V_gstate_unshare_z <= 0)%Z
   | 7 => (1 * s V_gstate_unshare_z <= 0 /\ -1 * s V_gstate_unshare_z <= 0)%Z
   | 8 => (-1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare_z <= 0)%Z
   | 9 => (1 * s V_gstate_unshare_z <= 0 /\ -1 * s V_gstate_unshare_z <= 0)%Z
   | 10 => (-1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare_i + -25 <= 0 /\ -1 * s V_gstate_unshare_i + 25 <= 0)%Z
   | 11 => (1 * s V_gstate_unshare_i + -25 <= 0 /\ -1 * s V_gstate_unshare_z <= 0)%Z
   | 12 => (-1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare_i + -25 <= 0)%Z
   | 13 => (-1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare_i + -24 <= 0)%Z
   | 14 => (1 * s V_gstate_unshare_i + -24 <= 0 /\ -1 * s V_gstate_unshare_z <= 0)%Z
   | 15 => (-1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare_i + -1 <= 0 /\ -1 * s V_gstate_unshare_i + 1 <= 0)%Z
   | 16 => (-1 * s V_gstate_unshare_i + 1 <= 0 /\ 1 * s V_gstate_unshare_i + -1 <= 0 /\ -1 * s V_gstate_unshare_z <= 0)%Z
   | 17 => (-1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare_i + -1 <= 0 /\ -1 * s V_gstate_unshare_i + 1 <= 0 /\ 1 * s V_gstate_unshare__tmp <= 0 /\ -1 * s V_gstate_unshare__tmp <= 0)%Z
   | 18 => (-1 * s V_gstate_unshare__tmp <= 0 /\ 1 * s V_gstate_unshare__tmp <= 0 /\ -1 * s V_gstate_unshare_i + 1 <= 0 /\ 1 * s V_gstate_unshare_i + -1 <= 0 /\ -1 * s V_gstate_unshare_z <= 0)%Z
   | 19 => (-1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare_i + -24 <= 0)%Z
   | 20 => (1 * s V_gstate_unshare_i + -24 <= 0 /\ -1 * s V_gstate_unshare_z <= 0)%Z
   | 21 => (-1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare_i + -24 <= 0)%Z
   | 22 => (1 * s V_gstate_unshare_z <= 0 /\ -1 * s V_gstate_unshare_z <= 0)%Z
   | 23 => (-1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare_z <= 0 /\ 1 * s V_gstate_unshare__tmp + 25 <= 0 /\ -1 * s V_gstate_unshare__tmp + -25 <= 0)%Z
   | 24 => (-1 * s V_gstate_unshare__tmp + -25 <= 0 /\ 1 * s V_gstate_unshare__tmp + 25 <= 0 /\ 1 * s V_gstate_unshare_z <= 0 /\ -1 * s V_gstate_unshare_z <= 0)%Z
   | 25 => (1 * s V_gstate_unshare__tmp <= 0 /\ -1 * s V_gstate_unshare_z <= 0 /\ -1 * s V_gstate_unshare__tmp + -25 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gstate_unshare (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((23 # 1) <= z)%Q
   | 2 => ((23 # 1) + s V_gstate_unshare_z <= z)%Q
   | 3 => ((23 # 1) + s V_gstate_unshare_z <= z)%Q
   | 4 => ((23 # 1) + s V_gstate_unshare_z <= z)%Q
   | 5 => (s V_gstate_unshare_z
           + (23 # 25) * max0(25 + s V_gstate_unshare__tmp) <= z)%Q
   | 6 => hints
     [(*-0.92 0*) F_binom_monotonic 1 (F_max0_ge_0 (25
                                                    + s V_gstate_unshare__tmp)) (F_check_ge (0) (0))]
     (s V_gstate_unshare_z + (23 # 25) * max0(25 + s V_gstate_unshare__tmp) <= z)%Q
   | 7 => ((23 # 1) + s V_gstate_unshare_z <= z)%Q
   | 8 => ((23 # 1) + s V_gstate_unshare_z <= z)%Q
   | 9 => ((23 # 1) + s V_gstate_unshare_z <= z)%Q
   | 10 => (-(2 # 1) + s V_gstate_unshare_i + s V_gstate_unshare_z <= z)%Q
   | 11 => (-(2 # 1) + s V_gstate_unshare_i + s V_gstate_unshare_z <= z)%Q
   | 12 => (-(2 # 1) + s V_gstate_unshare_i + s V_gstate_unshare_z <= z)%Q
   | 13 => (-(1 # 1) + s V_gstate_unshare_i + s V_gstate_unshare_z <= z)%Q
   | 14 => (-(1 # 1) + s V_gstate_unshare_i + s V_gstate_unshare_z <= z)%Q
   | 15 => (-(1 # 1) + s V_gstate_unshare_i + s V_gstate_unshare_z <= z)%Q
   | 16 => (-(1 # 1) + s V_gstate_unshare_i + s V_gstate_unshare_z <= z)%Q
   | 17 => (-(1 # 1) + s V_gstate_unshare_i + s V_gstate_unshare_z <= z)%Q
   | 18 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_gstate_unshare_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_gstate_unshare_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_gstate_unshare_i))]
     (-(1 # 1) + s V_gstate_unshare_i + s V_gstate_unshare_z <= z)%Q
   | 19 => (-(1 # 1) + s V_gstate_unshare_i + s V_gstate_unshare_z <= z)%Q
   | 20 => (-(1 # 1) + s V_gstate_unshare_i + s V_gstate_unshare_z <= z)%Q
   | 21 => (-(1 # 1) + s V_gstate_unshare_i + s V_gstate_unshare_z <= z)%Q
   | 22 => ((23 # 1) + s V_gstate_unshare_z <= z)%Q
   | 23 => (s V_gstate_unshare_z + (23 # 25) * max0(-s V_gstate_unshare__tmp) <= z)%Q
   | 24 => hints
     [(*-0.92 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_gstate_unshare__tmp)) (F_check_ge (0) (0))]
     (s V_gstate_unshare_z + (23 # 25) * max0(-s V_gstate_unshare__tmp) <= z)%Q
   | 25 => (s V_gstate_unshare_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gstate_unshare =>
    [mkPA Q (fun n z s => ai_gstate_unshare n s /\ annot0_gstate_unshare n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gstate_unshare (proc_start P_gstate_unshare) s1 (proc_end P_gstate_unshare) s2 ->
    (s2 V_gstate_unshare_z <= (23 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gstate_unshare.
Qed.
