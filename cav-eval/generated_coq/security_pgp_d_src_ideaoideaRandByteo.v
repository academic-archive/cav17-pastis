Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ideaRandByte.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ideaRandByte_z := 1%positive.
Notation V_ideaRandByte_i := 2%positive.
Notation V_ideaRandByte_c := 3%positive.
Definition Pedges_ideaRandByte: list (edge proc) :=
  (EA 1 (AAssign V_ideaRandByte_z (Some (ENum (0)))) 2)::(EA 2 AWeaken 3)::
  (EA 3 ANone 30)::(EA 3 ANone 4)::(EA 4 (AAssign V_ideaRandByte_i
  (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_ideaRandByte_i) s) < (eval (ENum (8))
  s))%Z)) 23)::(EA 7 (AGuard (fun s => ((eval (EVar V_ideaRandByte_i) s) >=
  (eval (ENum (8)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 (AAssign
  V_ideaRandByte_i (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard (fun s => ((eval (EVar V_ideaRandByte_i)
  s) < (eval (ENum (8)) s))%Z)) 16)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_ideaRandByte_i) s) >= (eval (ENum (8))
  s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::(EA 15 AWeaken 31)::
  (EA 16 AWeaken 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_ideaRandByte_i
  (Some (EAdd (EVar V_ideaRandByte_i) (ENum (1))))) 19)::(EA 19 ANone 20)::
  (EA 20 ANone 21)::(EA 21 (AAssign V_ideaRandByte_z (Some (EAdd (ENum (1))
  (EVar V_ideaRandByte_z)))) 22)::(EA 22 AWeaken 12)::(EA 23 AWeaken 24)::
  (EA 24 ANone 25)::(EA 25 (AAssign V_ideaRandByte_i
  (Some (EAdd (EVar V_ideaRandByte_i) (ENum (1))))) 26)::(EA 26 ANone 27)::
  (EA 27 ANone 28)::(EA 28 (AAssign V_ideaRandByte_z (Some (EAdd (ENum (1))
  (EVar V_ideaRandByte_z)))) 29)::(EA 29 AWeaken 7)::(EA 30 AWeaken 31)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ideaRandByte => Pedges_ideaRandByte
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ideaRandByte => 31
     end)%positive;
  var_global := var_global
}.

Definition ai_ideaRandByte (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_z <= 0)%Z
   | 3 => (-1 * s V_ideaRandByte_z <= 0 /\ 1 * s V_ideaRandByte_z <= 0)%Z
   | 4 => (1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_z <= 0)%Z
   | 5 => (-1 * s V_ideaRandByte_z <= 0 /\ 1 * s V_ideaRandByte_z <= 0 /\ 1 * s V_ideaRandByte_i <= 0 /\ -1 * s V_ideaRandByte_i <= 0)%Z
   | 6 => (-1 * s V_ideaRandByte_i <= 0 /\ 1 * s V_ideaRandByte_i <= 0 /\ 1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_z <= 0)%Z
   | 7 => (-1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_i <= 0 /\ 1 * s V_ideaRandByte_i + -8 <= 0)%Z
   | 8 => (1 * s V_ideaRandByte_i + -8 <= 0 /\ -1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_i + 8 <= 0)%Z
   | 9 => (-1 * s V_ideaRandByte_i + 8 <= 0 /\ -1 * s V_ideaRandByte_z <= 0 /\ 1 * s V_ideaRandByte_i + -8 <= 0)%Z
   | 10 => (-1 * s V_ideaRandByte_z <= 0 /\ 1 * s V_ideaRandByte_i <= 0 /\ -1 * s V_ideaRandByte_i <= 0)%Z
   | 11 => (-1 * s V_ideaRandByte_i <= 0 /\ 1 * s V_ideaRandByte_i <= 0 /\ -1 * s V_ideaRandByte_z <= 0)%Z
   | 12 => (-1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_i <= 0 /\ 1 * s V_ideaRandByte_i + -8 <= 0)%Z
   | 13 => (1 * s V_ideaRandByte_i + -8 <= 0 /\ -1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_i + 8 <= 0)%Z
   | 14 => (-1 * s V_ideaRandByte_i + 8 <= 0 /\ -1 * s V_ideaRandByte_z <= 0 /\ 1 * s V_ideaRandByte_i + -8 <= 0)%Z
   | 15 => (1 * s V_ideaRandByte_i + -8 <= 0 /\ -1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_i + 8 <= 0)%Z
   | 16 => (-1 * s V_ideaRandByte_i <= 0 /\ -1 * s V_ideaRandByte_z <= 0 /\ 1 * s V_ideaRandByte_i + -7 <= 0)%Z
   | 17 => (1 * s V_ideaRandByte_i + -7 <= 0 /\ -1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_i <= 0)%Z
   | 18 => (-1 * s V_ideaRandByte_i <= 0 /\ -1 * s V_ideaRandByte_z <= 0 /\ 1 * s V_ideaRandByte_i + -7 <= 0)%Z
   | 19 => (-1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_i + 1 <= 0 /\ 1 * s V_ideaRandByte_i + -8 <= 0)%Z
   | 20 => (1 * s V_ideaRandByte_i + -8 <= 0 /\ -1 * s V_ideaRandByte_i + 1 <= 0 /\ -1 * s V_ideaRandByte_z <= 0)%Z
   | 21 => (-1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_i + 1 <= 0 /\ 1 * s V_ideaRandByte_i + -8 <= 0)%Z
   | 22 => (1 * s V_ideaRandByte_i + -8 <= 0 /\ -1 * s V_ideaRandByte_i + 1 <= 0 /\ -1 * s V_ideaRandByte_z + 1 <= 0)%Z
   | 23 => (-1 * s V_ideaRandByte_i <= 0 /\ -1 * s V_ideaRandByte_z <= 0 /\ 1 * s V_ideaRandByte_i + -7 <= 0)%Z
   | 24 => (1 * s V_ideaRandByte_i + -7 <= 0 /\ -1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_i <= 0)%Z
   | 25 => (-1 * s V_ideaRandByte_i <= 0 /\ -1 * s V_ideaRandByte_z <= 0 /\ 1 * s V_ideaRandByte_i + -7 <= 0)%Z
   | 26 => (-1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_i + 1 <= 0 /\ 1 * s V_ideaRandByte_i + -8 <= 0)%Z
   | 27 => (1 * s V_ideaRandByte_i + -8 <= 0 /\ -1 * s V_ideaRandByte_i + 1 <= 0 /\ -1 * s V_ideaRandByte_z <= 0)%Z
   | 28 => (-1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_i + 1 <= 0 /\ 1 * s V_ideaRandByte_i + -8 <= 0)%Z
   | 29 => (1 * s V_ideaRandByte_i + -8 <= 0 /\ -1 * s V_ideaRandByte_i + 1 <= 0 /\ -1 * s V_ideaRandByte_z + 1 <= 0)%Z
   | 30 => (1 * s V_ideaRandByte_z <= 0 /\ -1 * s V_ideaRandByte_z <= 0)%Z
   | 31 => (-1 * s V_ideaRandByte_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ideaRandByte (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((16 # 1) <= z)%Q
   | 2 => ((16 # 1) + s V_ideaRandByte_z <= z)%Q
   | 3 => ((16 # 1) + s V_ideaRandByte_z <= z)%Q
   | 4 => ((16 # 1) + s V_ideaRandByte_z <= z)%Q
   | 5 => ((16 # 1) - s V_ideaRandByte_i + s V_ideaRandByte_z <= z)%Q
   | 6 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                               - s V_ideaRandByte_i) (0))) (F_max0_ge_0 (8
                                                                    - s V_ideaRandByte_i))]
     ((16 # 1) - s V_ideaRandByte_i + s V_ideaRandByte_z <= z)%Q
   | 7 => ((8 # 1) + s V_ideaRandByte_z + max0(8 - s V_ideaRandByte_i) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_ideaRandByte_i) (7
                                                                    - s V_ideaRandByte_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_ideaRandByte_i)]
     ((8 # 1) + s V_ideaRandByte_z + max0(8 - s V_ideaRandByte_i) <= z)%Q
   | 9 => ((8 # 1) + s V_ideaRandByte_z <= z)%Q
   | 10 => (-(1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
            + max0(8 - s V_ideaRandByte_i)
            + (1 # 2) * max0(s V_ideaRandByte_i) <= z)%Q
   | 11 => (-(1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
            + max0(8 - s V_ideaRandByte_i)
            + (1 # 2) * max0(s V_ideaRandByte_i) <= z)%Q
   | 12 => (-(1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
            + max0(8 - s V_ideaRandByte_i)
            + (1 # 2) * max0(s V_ideaRandByte_i) <= z)%Q
   | 13 => (-(1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
            + max0(8 - s V_ideaRandByte_i)
            + (1 # 2) * max0(s V_ideaRandByte_i) <= z)%Q
   | 14 => (-(1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
            + max0(8 - s V_ideaRandByte_i)
            + (1 # 2) * max0(s V_ideaRandByte_i) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_ideaRandByte_i) (7
                                                                    - s V_ideaRandByte_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_ideaRandByte_i);
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_ideaRandByte_i)) (F_check_ge (s V_ideaRandByte_i) (0))]
     (-(1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
      + max0(8 - s V_ideaRandByte_i) + (1 # 2) * max0(s V_ideaRandByte_i) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (8 - s V_ideaRandByte_i)) (F_check_ge (8
                                                                    - s V_ideaRandByte_i) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                              - s V_ideaRandByte_i) (0))) (F_max0_ge_0 (7
                                                                    - s V_ideaRandByte_i))]
     (-(1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
      + max0(8 - s V_ideaRandByte_i) + (1 # 2) * max0(s V_ideaRandByte_i) <= z)%Q
   | 17 => ((1 # 1) - (1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
            + max0(7 - s V_ideaRandByte_i)
            + (1 # 2) * max0(s V_ideaRandByte_i) <= z)%Q
   | 18 => ((1 # 1) - (1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
            + max0(7 - s V_ideaRandByte_i)
            + (1 # 2) * max0(s V_ideaRandByte_i) <= z)%Q
   | 19 => ((3 # 2) - (1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
            + (1 # 2) * max0(-1 + s V_ideaRandByte_i)
            + max0(8 - s V_ideaRandByte_i) <= z)%Q
   | 20 => ((3 # 2) - (1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
            + (1 # 2) * max0(-1 + s V_ideaRandByte_i)
            + max0(8 - s V_ideaRandByte_i) <= z)%Q
   | 21 => ((3 # 2) - (1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
            + (1 # 2) * max0(-1 + s V_ideaRandByte_i)
            + max0(8 - s V_ideaRandByte_i) <= z)%Q
   | 22 => hints
     [(*0 0.5*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ideaRandByte_i) (0))) (F_max0_ge_0 (s V_ideaRandByte_i));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_ideaRandByte_i)) (F_check_ge (-1
                                                                    + s V_ideaRandByte_i) (0))]
     ((1 # 2) - (1 # 2) * s V_ideaRandByte_i + s V_ideaRandByte_z
      + (1 # 2) * max0(-1 + s V_ideaRandByte_i)
      + max0(8 - s V_ideaRandByte_i) <= z)%Q
   | 23 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (8 - s V_ideaRandByte_i)) (F_check_ge (8
                                                                    - s V_ideaRandByte_i) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                              - s V_ideaRandByte_i) (0))) (F_max0_ge_0 (7
                                                                    - s V_ideaRandByte_i))]
     ((8 # 1) + s V_ideaRandByte_z + max0(8 - s V_ideaRandByte_i) <= z)%Q
   | 24 => ((9 # 1) + s V_ideaRandByte_z + max0(7 - s V_ideaRandByte_i) <= z)%Q
   | 25 => ((9 # 1) + s V_ideaRandByte_z + max0(7 - s V_ideaRandByte_i) <= z)%Q
   | 26 => ((9 # 1) + s V_ideaRandByte_z + max0(8 - s V_ideaRandByte_i) <= z)%Q
   | 27 => ((9 # 1) + s V_ideaRandByte_z + max0(8 - s V_ideaRandByte_i) <= z)%Q
   | 28 => ((9 # 1) + s V_ideaRandByte_z + max0(8 - s V_ideaRandByte_i) <= z)%Q
   | 29 => ((8 # 1) + s V_ideaRandByte_z + max0(8 - s V_ideaRandByte_i) <= z)%Q
   | 30 => hints
     [(*-16 0*) F_one]
     ((16 # 1) + s V_ideaRandByte_z <= z)%Q
   | 31 => (s V_ideaRandByte_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ideaRandByte =>
    [mkPA Q (fun n z s => ai_ideaRandByte n s /\ annot0_ideaRandByte n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ideaRandByte (proc_start P_ideaRandByte) s1 (proc_end P_ideaRandByte) s2 ->
    (s2 V_ideaRandByte_z <= (16 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ideaRandByte.
Qed.
