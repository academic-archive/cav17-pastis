Require Import pasta.Pasta.

Inductive proc: Type :=
  P_id3_render_paddedstring.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_id3_render_paddedstring_z := 1%positive.
Notation V_id3_render_paddedstring__tmp := 2%positive.
Notation V_id3_render_paddedstring_length := 3%positive.
Notation V_id3_render_paddedstring_ptr := 4%positive.
Notation V_id3_render_paddedstring_ucs4 := 5%positive.
Definition Pedges_id3_render_paddedstring: list (edge proc) :=
  (EA 1 (AAssign V_id3_render_paddedstring_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_id3_render_paddedstring__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign
  V_id3_render_paddedstring__tmp
  (Some (EVar V_id3_render_paddedstring_length))) 5)::(EA 5 AWeaken 6)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_id3_render_paddedstring__tmp) s) <=
  (eval (ENum (30)) s))%Z)) 10)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_id3_render_paddedstring__tmp) s) >
  (eval (ENum (30)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 28)::(EA 10 AWeaken 11)::(EA 11 ANone 12)::
  (EA 12 AWeaken 13)::(EA 13 ANone 14)::(EA 13 ANone 23)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::(EA 16 ANone 17)::(EA 16 ANone 22)::(EA 17 (AAssign
  V_id3_render_paddedstring__tmp
  (Some (EAdd (EVar V_id3_render_paddedstring__tmp) (ENum (-1))))) 18)::
  (EA 18 ANone 19)::(EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_id3_render_paddedstring__tmp) s) <>
  (eval (ENum (0)) s))%Z)) 33)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_id3_render_paddedstring__tmp) s) =
  (eval (ENum (0)) s))%Z)) 21)::(EA 21 AWeaken 22)::(EA 22 ANone 23)::
  (EA 23 ANone 24)::(EA 24 (AAssign V_id3_render_paddedstring__tmp
  (Some (EAdd (EVar V_id3_render_paddedstring__tmp) (ENum (-1))))) 25)::
  (EA 25 AWeaken 26)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_id3_render_paddedstring__tmp) s) <>
  (eval (ENum (0)) s))%Z)) 29)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_id3_render_paddedstring__tmp) s) =
  (eval (ENum (0)) s))%Z)) 27)::(EA 27 AWeaken 28)::(EA 29 AWeaken 30)::
  (EA 30 ANone 31)::(EA 31 ANone 32)::(EA 32 (AAssign
  V_id3_render_paddedstring_z (Some (EAdd (ENum (1))
  (EVar V_id3_render_paddedstring_z)))) 24)::(EA 33 AWeaken 34)::
  (EA 34 ANone 35)::(EA 34 ANone 36)::(EA 35 ANone 36)::(EA 36 ANone 37)::
  (EA 37 ANone 38)::(EA 38 (AAssign V_id3_render_paddedstring_z
  (Some (EAdd (ENum (1)) (EVar V_id3_render_paddedstring_z)))) 39)::
  (EA 39 AWeaken 16)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_id3_render_paddedstring => Pedges_id3_render_paddedstring
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_id3_render_paddedstring => 28
     end)%positive;
  var_global := var_global
}.

Definition ai_id3_render_paddedstring (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_id3_render_paddedstring_z <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 3 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring_z <= 0 /\ -1 * s V_id3_render_paddedstring__tmp <= 0)%Z
   | 4 => (-1 * s V_id3_render_paddedstring__tmp <= 0 /\ 1 * s V_id3_render_paddedstring_z <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 5 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 6 => (1 * s V_id3_render_paddedstring_z <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 7 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring_z <= 0 /\ -1 * s V_id3_render_paddedstring__tmp + 31 <= 0)%Z
   | 8 => (-1 * s V_id3_render_paddedstring__tmp + 31 <= 0 /\ 1 * s V_id3_render_paddedstring_z <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 9 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring_z <= 0 /\ -1 * s V_id3_render_paddedstring__tmp + 31 <= 0)%Z
   | 10 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -30 <= 0)%Z
   | 11 => (1 * s V_id3_render_paddedstring__tmp + -30 <= 0 /\ 1 * s V_id3_render_paddedstring_z <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 12 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -30 <= 0)%Z
   | 13 => (1 * s V_id3_render_paddedstring__tmp + -30 <= 0 /\ 1 * s V_id3_render_paddedstring_z <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 14 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -30 <= 0)%Z
   | 15 => (1 * s V_id3_render_paddedstring__tmp + -30 <= 0 /\ 1 * s V_id3_render_paddedstring_z <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 16 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -30 <= 0)%Z
   | 17 => (1 * s V_id3_render_paddedstring__tmp + -30 <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 18 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -29 <= 0)%Z
   | 19 => (1 * s V_id3_render_paddedstring__tmp + -29 <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 20 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -29 <= 0)%Z
   | 21 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp <= 0 /\ -1 * s V_id3_render_paddedstring__tmp <= 0)%Z
   | 22 => (1 * s V_id3_render_paddedstring__tmp + -30 <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 23 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -30 <= 0)%Z
   | 24 => (1 * s V_id3_render_paddedstring__tmp + -30 <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 25 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -29 <= 0)%Z
   | 26 => (1 * s V_id3_render_paddedstring__tmp + -29 <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 27 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp <= 0 /\ -1 * s V_id3_render_paddedstring__tmp <= 0)%Z
   | 28 => (-1 * s V_id3_render_paddedstring__tmp <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 29 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -29 <= 0)%Z
   | 30 => (1 * s V_id3_render_paddedstring__tmp + -29 <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 31 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -29 <= 0)%Z
   | 32 => (1 * s V_id3_render_paddedstring__tmp + -29 <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 33 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -29 <= 0)%Z
   | 34 => (1 * s V_id3_render_paddedstring__tmp + -29 <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 35 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -29 <= 0)%Z
   | 36 => (1 * s V_id3_render_paddedstring__tmp + -29 <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 37 => (-1 * s V_id3_render_paddedstring_z <= 0 /\ 1 * s V_id3_render_paddedstring__tmp + -29 <= 0)%Z
   | 38 => (1 * s V_id3_render_paddedstring__tmp + -29 <= 0 /\ -1 * s V_id3_render_paddedstring_z <= 0)%Z
   | 39 => (1 * s V_id3_render_paddedstring__tmp + -29 <= 0 /\ -1 * s V_id3_render_paddedstring_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_id3_render_paddedstring (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((29 # 1) <= z)%Q
   | 2 => ((29 # 1) <= z)%Q
   | 3 => ((29 # 1) <= z)%Q
   | 4 => ((29 # 1) <= z)%Q
   | 5 => ((29 # 1) <= z)%Q
   | 6 => ((29 # 1) <= z)%Q
   | 7 => hints
     [(*-29 0*) F_one]
     ((29 # 1) <= z)%Q
   | 8 => (0 <= z)%Q
   | 9 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_id3_render_paddedstring_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_id3_render_paddedstring_z) (0))) (F_max0_ge_0 (-
                                                                    s V_id3_render_paddedstring_z))]
     (0 <= z)%Q
   | 10 => ((29 # 1) <= z)%Q
   | 11 => ((29 # 1) <= z)%Q
   | 12 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_id3_render_paddedstring_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_id3_render_paddedstring_z) (0))) (F_max0_ge_0 (-
                                                                    s V_id3_render_paddedstring_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (30
                                                - s V_id3_render_paddedstring__tmp)) (F_check_ge (0) (0))]
     ((29 # 1) <= z)%Q
   | 13 => ((29 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 14 => ((29 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 15 => ((29 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 16 => ((29 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 17 => ((29 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 18 => ((29 # 1) + s V_id3_render_paddedstring_z
            - max0(29 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 19 => hints
     [(*-0.966667 0*) F_binom_monotonic 1 (F_max0_ge_arg (30
                                                          - s V_id3_render_paddedstring__tmp)) (F_check_ge (30
                                                                    - s V_id3_render_paddedstring__tmp) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (29
                                                              - s V_id3_render_paddedstring__tmp) (0))) (F_max0_ge_0 (29
                                                                    - s V_id3_render_paddedstring__tmp))]
     ((29 # 1) + s V_id3_render_paddedstring_z
      - max0(29 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 20 => ((29 # 1) + (1 # 30) * s V_id3_render_paddedstring__tmp
            + s V_id3_render_paddedstring_z
            - (29 # 30) * max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 21 => hints
     [(*-0.0333333 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_id3_render_paddedstring__tmp)) (F_check_ge (0) (0));
      (*-0.0333333 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_id3_render_paddedstring__tmp) (0))) (F_max0_ge_0 (s V_id3_render_paddedstring__tmp));
      (*-0.0333333 0*) F_binom_monotonic 1 (F_max0_ge_0 (30
                                                         - s V_id3_render_paddedstring__tmp)) (F_check_ge (0) (0))]
     ((29 # 1) + (1 # 30) * s V_id3_render_paddedstring__tmp
      + s V_id3_render_paddedstring_z
      - (29 # 30) * max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 22 => ((29 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 23 => ((29 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 24 => ((29 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 25 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (30
                                                  - s V_id3_render_paddedstring__tmp)) (F_check_ge (30
                                                                    - s V_id3_render_paddedstring__tmp) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (29
                                                              - s V_id3_render_paddedstring__tmp) (0))) (F_max0_ge_0 (29
                                                                    - s V_id3_render_paddedstring__tmp))]
     ((29 # 1) + s V_id3_render_paddedstring_z
      - max0(29 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 26 => ((30 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 27 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_id3_render_paddedstring__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_id3_render_paddedstring__tmp) (0))) (F_max0_ge_0 (s V_id3_render_paddedstring__tmp));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (30
                                                               - s V_id3_render_paddedstring__tmp) (0))) (F_max0_ge_0 (30
                                                                    - s V_id3_render_paddedstring__tmp))]
     ((30 # 1) + s V_id3_render_paddedstring_z
      - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 28 => (s V_id3_render_paddedstring_z <= z)%Q
   | 29 => ((30 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 30 => ((30 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 31 => ((30 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 32 => ((30 # 1) + s V_id3_render_paddedstring_z
            - max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 33 => ((29 # 1) + (1 # 30) * s V_id3_render_paddedstring__tmp
            + s V_id3_render_paddedstring_z
            - (29 # 30) * max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 34 => ((29 # 1) + (1 # 30) * s V_id3_render_paddedstring__tmp
            + s V_id3_render_paddedstring_z
            - (29 # 30) * max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 35 => ((29 # 1) + (1 # 30) * s V_id3_render_paddedstring__tmp
            + s V_id3_render_paddedstring_z
            - (29 # 30) * max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 36 => ((29 # 1) + (1 # 30) * s V_id3_render_paddedstring__tmp
            + s V_id3_render_paddedstring_z
            - (29 # 30) * max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 37 => ((29 # 1) + (1 # 30) * s V_id3_render_paddedstring__tmp
            + s V_id3_render_paddedstring_z
            - (29 # 30) * max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 38 => ((29 # 1) + (1 # 30) * s V_id3_render_paddedstring__tmp
            + s V_id3_render_paddedstring_z
            - (29 # 30) * max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | 39 => hints
     [(*-0.0333333 0*) F_binom_monotonic 1 (F_max0_ge_arg (30
                                                           - s V_id3_render_paddedstring__tmp)) (F_check_ge (30
                                                                    - s V_id3_render_paddedstring__tmp) (0))]
     ((28 # 1) + (1 # 30) * s V_id3_render_paddedstring__tmp
      + s V_id3_render_paddedstring_z
      - (29 # 30) * max0(30 - s V_id3_render_paddedstring__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_id3_render_paddedstring =>
    [mkPA Q (fun n z s => ai_id3_render_paddedstring n s /\ annot0_id3_render_paddedstring n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_id3_render_paddedstring (proc_start P_id3_render_paddedstring) s1 (proc_end P_id3_render_paddedstring) s2 ->
    (s2 V_id3_render_paddedstring_z <= (29 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_id3_render_paddedstring.
Qed.
