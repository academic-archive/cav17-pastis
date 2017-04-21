Require Import pasta.Pasta.

Inductive proc: Type :=
  P_zgstate.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_zgstate_z := 1%positive.
Notation V_zgstate__tmp := 2%positive.
Notation V_zgstate_code := 3%positive.
Notation V_zgstate_i := 4%positive.
Notation V_zgstate_op := 5%positive.
Definition Pedges_zgstate: list (edge proc) :=
  (EA 1 (AAssign V_zgstate_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_zgstate_code None) 3)::(EA 3 AWeaken 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_zgstate_code) s) < (eval (ENum (0)) s))%Z)) 40)::
  (EA 4 (AGuard (fun s => ((eval (EVar V_zgstate_code) s) >= (eval (ENum (0))
  s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 ANone 37)::(EA 6 ANone 7)::
  (EA 7 AWeaken 8)::(EA 8 ANone 34)::(EA 8 ANone 9)::(EA 9 (AAssign
  V_zgstate_i (Some (ENum (25)))) 10)::(EA 10 ANone 11)::(EA 11 ANone 12)::
  (EA 12 (AAssign V_zgstate_i (Some (EAdd (EVar V_zgstate_i)
  (ENum (-1))))) 13)::(EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EAdd (EVar V_zgstate_i) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 31)::(EA 14 (AGuard
  (fun s => ((eval (EAdd (EVar V_zgstate_i) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 16 ANone 17)::
  (EA 17 AWeaken 18)::(EA 18 ANone 28)::(EA 18 ANone 19)::(EA 19 ANone 20)::
  (EA 20 ANone 21)::(EA 21 AWeaken 22)::(EA 22 ANone 24)::(EA 22 ANone 23)::
  (EA 23 ANone 25)::(EA 24 ANone 25)::(EA 25 (AAssign V_zgstate__tmp
  (Some (ENum (0)))) 26)::(EA 26 ANone 27)::(EA 27 AWeaken 44)::
  (EA 28 (AAssign V_zgstate__tmp (Some (ENum (-16)))) 29)::(EA 29 ANone 30)::
  (EA 30 AWeaken 44)::(EA 31 AWeaken 32)::(EA 32 ANone 33)::(EA 33 (AAssign
  V_zgstate_z (Some (EAdd (ENum (1)) (EVar V_zgstate_z)))) 11)::
  (EA 34 (AAssign V_zgstate__tmp (Some (ENum (-25)))) 35)::(EA 35 ANone 36)::
  (EA 36 AWeaken 44)::(EA 37 (AAssign V_zgstate__tmp
  (Some (ENum (-25)))) 38)::(EA 38 ANone 39)::(EA 39 AWeaken 44)::
  (EA 40 AWeaken 41)::(EA 41 (AAssign V_zgstate__tmp
  (Some (EVar V_zgstate_code))) 42)::(EA 42 ANone 43)::(EA 43 AWeaken 44)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_zgstate => Pedges_zgstate
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_zgstate => 44
     end)%positive;
  var_global := var_global
}.

Definition ai_zgstate (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 3 => (-1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_z <= 0)%Z
   | 4 => (1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 5 => (-1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0)%Z
   | 6 => (-1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 7 => (-1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0)%Z
   | 8 => (-1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 9 => (-1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0)%Z
   | 10 => (-1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_i + -25 <= 0 /\ -1 * s V_zgstate_i + 25 <= 0)%Z
   | 11 => (1 * s V_zgstate_i + -25 <= 0 /\ -1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0)%Z
   | 12 => (-1 * s V_zgstate_code <= 0 /\ -1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_i + -25 <= 0)%Z
   | 13 => (-1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_i + -24 <= 0)%Z
   | 14 => (1 * s V_zgstate_i + -24 <= 0 /\ -1 * s V_zgstate_code <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 15 => (-1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_i + 1 <= 0)%Z
   | 16 => (-1 * s V_zgstate_i + 1 <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_code <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 17 => (-1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_i + 1 <= 0)%Z
   | 18 => (-1 * s V_zgstate_i + 1 <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_code <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 19 => (-1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_i + 1 <= 0)%Z
   | 20 => (-1 * s V_zgstate_i + 1 <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_code <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 21 => (-1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_i + 1 <= 0)%Z
   | 22 => (-1 * s V_zgstate_i + 1 <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_code <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 23 => (-1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_i + 1 <= 0)%Z
   | 24 => (-1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_i + 1 <= 0)%Z
   | 25 => (-1 * s V_zgstate_i + 1 <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_code <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 26 => (-1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_i + 1 <= 0 /\ 1 * s V_zgstate__tmp <= 0 /\ -1 * s V_zgstate__tmp <= 0)%Z
   | 27 => (-1 * s V_zgstate__tmp <= 0 /\ 1 * s V_zgstate__tmp <= 0 /\ -1 * s V_zgstate_i + 1 <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_code <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 28 => (-1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_i + 1 <= 0)%Z
   | 29 => (-1 * s V_zgstate_i + 1 <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_code <= 0 /\ -1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate__tmp + 16 <= 0 /\ -1 * s V_zgstate__tmp + -16 <= 0)%Z
   | 30 => (-1 * s V_zgstate__tmp + -16 <= 0 /\ 1 * s V_zgstate__tmp + 16 <= 0 /\ -1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_i + -1 <= 0 /\ -1 * s V_zgstate_i + 1 <= 0)%Z
   | 31 => (-1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_i + -24 <= 0)%Z
   | 32 => (1 * s V_zgstate_i + -24 <= 0 /\ -1 * s V_zgstate_code <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 33 => (-1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_i + -24 <= 0)%Z
   | 34 => (-1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0)%Z
   | 35 => (-1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate__tmp + 25 <= 0 /\ -1 * s V_zgstate__tmp + -25 <= 0)%Z
   | 36 => (-1 * s V_zgstate__tmp + -25 <= 0 /\ 1 * s V_zgstate__tmp + 25 <= 0 /\ -1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0)%Z
   | 37 => (-1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0)%Z
   | 38 => (-1 * s V_zgstate_code <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate__tmp + 25 <= 0 /\ -1 * s V_zgstate__tmp + -25 <= 0)%Z
   | 39 => (-1 * s V_zgstate__tmp + -25 <= 0 /\ 1 * s V_zgstate__tmp + 25 <= 0 /\ -1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_code <= 0)%Z
   | 40 => (-1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_code + 1 <= 0)%Z
   | 41 => (1 * s V_zgstate_code + 1 <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 42 => (-1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_z <= 0 /\ 1 * s V_zgstate_code + 1 <= 0 /\ 1 * s V_zgstate__tmp + 1 <= 0)%Z
   | 43 => (1 * s V_zgstate__tmp + 1 <= 0 /\ 1 * s V_zgstate_code + 1 <= 0 /\ 1 * s V_zgstate_z <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | 44 => (1 * s V_zgstate__tmp <= 0 /\ -1 * s V_zgstate_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_zgstate (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((23 # 1) <= z)%Q
   | 2 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 3 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 4 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 5 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 6 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 7 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 8 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 9 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 10 => (-(2 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 11 => (-(2 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 12 => (-(2 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 13 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 14 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 15 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 16 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 17 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 18 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 19 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 20 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 21 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 22 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 23 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 24 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 25 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 26 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 27 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zgstate_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_zgstate_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zgstate_i))]
     (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 28 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 29 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 30 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_zgstate_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_zgstate_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_zgstate_i))]
     (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 31 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 32 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 33 => (-(1 # 1) + s V_zgstate_i + s V_zgstate_z <= z)%Q
   | 34 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 35 => (s V_zgstate_z + (23 # 9) * max0(-16 - s V_zgstate__tmp) <= z)%Q
   | 36 => hints
     [(*-2.55556 0*) F_binom_monotonic 1 (F_max0_ge_0 (-16 - s V_zgstate__tmp)) (F_check_ge (0) (0))]
     (s V_zgstate_z + (23 # 9) * max0(-16 - s V_zgstate__tmp) <= z)%Q
   | 37 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 38 => (s V_zgstate_z + (23 # 9) * max0(-16 - s V_zgstate__tmp) <= z)%Q
   | 39 => hints
     [(*-2.55556 0*) F_binom_monotonic 1 (F_max0_ge_0 (-16 - s V_zgstate__tmp)) (F_check_ge (0) (0))]
     (s V_zgstate_z + (23 # 9) * max0(-16 - s V_zgstate__tmp) <= z)%Q
   | 40 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 41 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 42 => ((23 # 1) + s V_zgstate_z <= z)%Q
   | 43 => hints
     [(*-23 0*) F_one]
     ((23 # 1) + s V_zgstate_z <= z)%Q
   | 44 => (s V_zgstate_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_zgstate =>
    [mkPA Q (fun n z s => ai_zgstate n s /\ annot0_zgstate n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_zgstate (proc_start P_zgstate) s1 (proc_end P_zgstate) s2 ->
    (s2 V_zgstate_z <= (23 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_zgstate.
Qed.
