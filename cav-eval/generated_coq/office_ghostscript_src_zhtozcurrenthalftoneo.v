Require Import pasta.Pasta.

Inductive proc: Type :=
  P_zcurrenthalftone.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_zcurrenthalftone_z := 1%positive.
Notation V_zcurrenthalftone__tmp := 2%positive.
Notation V_zcurrenthalftone_i := 3%positive.
Notation V_zcurrenthalftone_op := 4%positive.
Definition Pedges_zcurrenthalftone: list (edge proc) :=
  (EA 1 (AAssign V_zcurrenthalftone_z (Some (ENum (0)))) 2)::
  (EA 2 AWeaken 3)::(EA 3 ANone 34)::(EA 3 ANone 25)::(EA 3 ANone 4)::
  (EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 ANone 22)::(EA 6 ANone 7)::
  (EA 7 ANone 8)::(EA 8 ANone 9)::(EA 9 (AAssign V_zcurrenthalftone_i
  (Some (ENum (0)))) 10)::(EA 10 ANone 11)::(EA 11 AWeaken 12)::
  (EA 12 (AGuard (fun s => ((eval (EVar V_zcurrenthalftone_i) s) <
  (eval (ENum (4)) s))%Z)) 15)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_zcurrenthalftone_i) s) >= (eval (ENum (4))
  s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 ANone 40)::(EA 15 AWeaken 16)::
  (EA 16 ANone 17)::(EA 17 (AAssign V_zcurrenthalftone_i
  (Some (EAdd (EVar V_zcurrenthalftone_i) (ENum (1))))) 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign V_zcurrenthalftone_z
  (Some (EAdd (ENum (1)) (EVar V_zcurrenthalftone_z)))) 21)::
  (EA 21 AWeaken 12)::(EA 22 (AAssign V_zcurrenthalftone__tmp
  (Some (ENum (-16)))) 23)::(EA 23 ANone 24)::(EA 24 AWeaken 46)::
  (EA 25 ANone 26)::(EA 26 AWeaken 27)::(EA 27 ANone 31)::(EA 27 ANone 28)::
  (EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 ANone 40)::(EA 31 (AAssign
  V_zcurrenthalftone__tmp (Some (ENum (-16)))) 32)::(EA 32 ANone 33)::
  (EA 33 AWeaken 46)::(EA 34 ANone 35)::(EA 35 AWeaken 36)::
  (EA 36 ANone 43)::(EA 36 ANone 37)::(EA 37 ANone 38)::(EA 38 ANone 39)::
  (EA 39 ANone 40)::(EA 40 (AAssign V_zcurrenthalftone__tmp
  (Some (ENum (0)))) 41)::(EA 41 ANone 42)::(EA 42 AWeaken 46)::
  (EA 43 (AAssign V_zcurrenthalftone__tmp (Some (ENum (-16)))) 44)::
  (EA 44 ANone 45)::(EA 45 AWeaken 46)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_zcurrenthalftone => Pedges_zcurrenthalftone
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_zcurrenthalftone => 46
     end)%positive;
  var_global := var_global
}.

Definition ai_zcurrenthalftone (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0)%Z
   | 3 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 4 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0)%Z
   | 5 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 6 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0)%Z
   | 7 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 8 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0)%Z
   | 9 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 10 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_i <= 0 /\ -1 * s V_zcurrenthalftone_i <= 0)%Z
   | 11 => (-1 * s V_zcurrenthalftone_i <= 0 /\ 1 * s V_zcurrenthalftone_i <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 12 => (-1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_i <= 0 /\ 1 * s V_zcurrenthalftone_i + -4 <= 0)%Z
   | 13 => (1 * s V_zcurrenthalftone_i + -4 <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_i + 4 <= 0)%Z
   | 14 => (-1 * s V_zcurrenthalftone_i + 4 <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_i + -4 <= 0)%Z
   | 15 => (-1 * s V_zcurrenthalftone_i <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_i + -3 <= 0)%Z
   | 16 => (1 * s V_zcurrenthalftone_i + -3 <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_i <= 0)%Z
   | 17 => (-1 * s V_zcurrenthalftone_i <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_i + -3 <= 0)%Z
   | 18 => (-1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_i + 1 <= 0 /\ 1 * s V_zcurrenthalftone_i + -4 <= 0)%Z
   | 19 => (1 * s V_zcurrenthalftone_i + -4 <= 0 /\ -1 * s V_zcurrenthalftone_i + 1 <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0)%Z
   | 20 => (-1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_i + 1 <= 0 /\ 1 * s V_zcurrenthalftone_i + -4 <= 0)%Z
   | 21 => (1 * s V_zcurrenthalftone_i + -4 <= 0 /\ -1 * s V_zcurrenthalftone_i + 1 <= 0 /\ -1 * s V_zcurrenthalftone_z + 1 <= 0)%Z
   | 22 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 23 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone__tmp + 16 <= 0 /\ -1 * s V_zcurrenthalftone__tmp + -16 <= 0)%Z
   | 24 => (-1 * s V_zcurrenthalftone__tmp + -16 <= 0 /\ 1 * s V_zcurrenthalftone__tmp + 16 <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 25 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0)%Z
   | 26 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 27 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0)%Z
   | 28 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 29 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0)%Z
   | 30 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 31 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 32 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone__tmp + 16 <= 0 /\ -1 * s V_zcurrenthalftone__tmp + -16 <= 0)%Z
   | 33 => (-1 * s V_zcurrenthalftone__tmp + -16 <= 0 /\ 1 * s V_zcurrenthalftone__tmp + 16 <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 34 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0)%Z
   | 35 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 36 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0)%Z
   | 37 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 38 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0)%Z
   | 39 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 40 => (-1 * s V_zcurrenthalftone_z <= 0)%Z
   | 41 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone__tmp <= 0 /\ -1 * s V_zcurrenthalftone__tmp <= 0)%Z
   | 42 => (-1 * s V_zcurrenthalftone__tmp <= 0 /\ 1 * s V_zcurrenthalftone__tmp <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0)%Z
   | 43 => (-1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 44 => (1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone__tmp + 16 <= 0 /\ -1 * s V_zcurrenthalftone__tmp + -16 <= 0)%Z
   | 45 => (-1 * s V_zcurrenthalftone__tmp + -16 <= 0 /\ 1 * s V_zcurrenthalftone__tmp + 16 <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ 1 * s V_zcurrenthalftone_z <= 0)%Z
   | 46 => (1 * s V_zcurrenthalftone__tmp <= 0 /\ -1 * s V_zcurrenthalftone_z <= 0 /\ -1 * s V_zcurrenthalftone__tmp + -16 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_zcurrenthalftone (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) <= z)%Q
   | 2 => ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 3 => ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 4 => ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 5 => ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 6 => ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 7 => ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 8 => ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 9 => ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 10 => (max0(4 - s V_zcurrenthalftone_i) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 11 => (max0(4 - s V_zcurrenthalftone_i) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 12 => (max0(4 - s V_zcurrenthalftone_i) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_zcurrenthalftone_i) (3
                                                                    - s V_zcurrenthalftone_i));
      (*-1 0*) F_max0_ge_0 (3 - s V_zcurrenthalftone_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zcurrenthalftone_z)) (F_check_ge (s V_zcurrenthalftone_z) (0))]
     (max0(4 - s V_zcurrenthalftone_i) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 14 => (s V_zcurrenthalftone_z <= z)%Q
   | 15 => hints
     [(*0 1*) F_max0_pre_decrement 1 (4 - s V_zcurrenthalftone_i) (1)]
     (max0(4 - s V_zcurrenthalftone_i) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 16 => ((1 # 1) + max0(3 - s V_zcurrenthalftone_i)
            + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 17 => ((1 # 1) + max0(3 - s V_zcurrenthalftone_i)
            + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 18 => ((1 # 1) + max0(4 - s V_zcurrenthalftone_i)
            + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 19 => ((1 # 1) + max0(4 - s V_zcurrenthalftone_i)
            + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 20 => ((1 # 1) + max0(4 - s V_zcurrenthalftone_i)
            + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zcurrenthalftone_z) (0))) (F_max0_ge_0 (s V_zcurrenthalftone_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_zcurrenthalftone_z)) (F_check_ge (-1
                                                                    + s V_zcurrenthalftone_z) (0))]
     ((1 # 1) + max0(-1 + s V_zcurrenthalftone_z)
      + max0(4 - s V_zcurrenthalftone_i) <= z)%Q
   | 22 => ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 23 => ((1 # 4) * max0(-s V_zcurrenthalftone__tmp)
            + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zcurrenthalftone_z)) (F_check_ge (s V_zcurrenthalftone_z) (0));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_zcurrenthalftone__tmp)) (F_check_ge (0) (0))]
     ((1 # 4) * max0(-s V_zcurrenthalftone__tmp)
      + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 25 => ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 26 => hints
     [(*-4 0*) F_one;
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zcurrenthalftone_z)) (F_check_ge (s V_zcurrenthalftone_z) (0))]
     ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 27 => (s V_zcurrenthalftone_z <= z)%Q
   | 28 => (s V_zcurrenthalftone_z <= z)%Q
   | 29 => (s V_zcurrenthalftone_z <= z)%Q
   | 30 => (s V_zcurrenthalftone_z <= z)%Q
   | 31 => (s V_zcurrenthalftone_z <= z)%Q
   | 32 => (s V_zcurrenthalftone_z <= z)%Q
   | 33 => (s V_zcurrenthalftone_z <= z)%Q
   | 34 => ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 35 => hints
     [(*0 4*) F_one;
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zcurrenthalftone_z)) (F_check_ge (s V_zcurrenthalftone_z) (0))]
     ((4 # 1) + max0(s V_zcurrenthalftone_z) <= z)%Q
   | 36 => (s V_zcurrenthalftone_z <= z)%Q
   | 37 => (s V_zcurrenthalftone_z <= z)%Q
   | 38 => (s V_zcurrenthalftone_z <= z)%Q
   | 39 => (s V_zcurrenthalftone_z <= z)%Q
   | 40 => (s V_zcurrenthalftone_z <= z)%Q
   | 41 => (s V_zcurrenthalftone_z <= z)%Q
   | 42 => (s V_zcurrenthalftone_z <= z)%Q
   | 43 => (s V_zcurrenthalftone_z <= z)%Q
   | 44 => (s V_zcurrenthalftone_z <= z)%Q
   | 45 => (s V_zcurrenthalftone_z <= z)%Q
   | 46 => (s V_zcurrenthalftone_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_zcurrenthalftone =>
    [mkPA Q (fun n z s => ai_zcurrenthalftone n s /\ annot0_zcurrenthalftone n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_zcurrenthalftone (proc_start P_zcurrenthalftone) s1 (proc_end P_zcurrenthalftone) s2 ->
    (s2 V_zcurrenthalftone_z <= (4 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_zcurrenthalftone.
Qed.
