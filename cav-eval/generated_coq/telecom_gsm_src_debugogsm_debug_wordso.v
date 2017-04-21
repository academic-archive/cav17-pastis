Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gsm_debug_words.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gsm_debug_words_z := 1%positive.
Notation V_gsm_debug_words__tmp := 2%positive.
Notation V_gsm_debug_words__tmp1 := 3%positive.
Notation V_gsm_debug_words_nprinted := 4%positive.
Notation V_gsm_debug_words_from := 5%positive.
Notation V_gsm_debug_words_name := 6%positive.
Notation V_gsm_debug_words_ptr := 7%positive.
Notation V_gsm_debug_words_to := 8%positive.
Definition Pedges_gsm_debug_words: list (edge proc) :=
  (EA 1 (AAssign V_gsm_debug_words_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_gsm_debug_words__tmp (Some (EVar V_gsm_debug_words_from))) 3)::
  (EA 3 (AAssign V_gsm_debug_words__tmp1
  (Some (EVar V_gsm_debug_words_to))) 4)::(EA 4 (AAssign
  V_gsm_debug_words_nprinted (Some (ENum (0)))) 5)::(EA 5 ANone 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_gsm_debug_words__tmp) s) <=
  (eval (EVar V_gsm_debug_words__tmp1) s))%Z)) 10)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_gsm_debug_words__tmp) s) >
  (eval (EVar V_gsm_debug_words__tmp1) s))%Z)) 8)::(EA 8 AWeaken 9)::
  (EA 10 AWeaken 11)::(EA 11 (AAssign V_gsm_debug_words__tmp
  (Some (EAdd (EVar V_gsm_debug_words__tmp) (ENum (1))))) 12)::
  (EA 12 (AAssign V_gsm_debug_words_nprinted
  (Some (EAdd (EVar V_gsm_debug_words_nprinted) (ENum (1))))) 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_gsm_debug_words_nprinted) s) >= (eval (ENum (7))
  s))%Z)) 16)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_gsm_debug_words_nprinted) s) < (eval (ENum (7))
  s))%Z)) 15)::(EA 15 AWeaken 24)::(EA 16 AWeaken 17)::(EA 17 (AAssign
  V_gsm_debug_words_nprinted (Some (ENum (0)))) 18)::(EA 18 AWeaken 19)::
  (EA 19 (AGuard (fun s => ((eval (EVar V_gsm_debug_words__tmp) s) <
  (eval (EVar V_gsm_debug_words__tmp1) s))%Z)) 21)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_gsm_debug_words__tmp) s) >=
  (eval (EVar V_gsm_debug_words__tmp1) s))%Z)) 20)::(EA 20 AWeaken 23)::
  (EA 21 AWeaken 22)::(EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 ANone 25)::
  (EA 25 ANone 26)::(EA 26 (AAssign V_gsm_debug_words_z
  (Some (EAdd (ENum (1)) (EVar V_gsm_debug_words_z)))) 27)::
  (EA 27 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gsm_debug_words => Pedges_gsm_debug_words
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gsm_debug_words => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_gsm_debug_words (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gsm_debug_words_z <= 0 /\ -1 * s V_gsm_debug_words_z <= 0)%Z
   | 3 => (-1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words_z <= 0)%Z
   | 4 => (1 * s V_gsm_debug_words_z <= 0 /\ -1 * s V_gsm_debug_words_z <= 0)%Z
   | 5 => (-1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words_nprinted <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0)%Z
   | 6 => (-1 * s V_gsm_debug_words_nprinted <= 0 /\ 1 * s V_gsm_debug_words_nprinted <= 0 /\ 1 * s V_gsm_debug_words_z <= 0 /\ -1 * s V_gsm_debug_words_z <= 0)%Z
   | 7 => (-1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words_nprinted + -6 <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0)%Z
   | 8 => (-1 * s V_gsm_debug_words_nprinted <= 0 /\ 1 * s V_gsm_debug_words_nprinted + -6 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ -1 * s V_gsm_debug_words__tmp+ 1 * s V_gsm_debug_words__tmp1 + 1 <= 0)%Z
   | 9 => (-1 * s V_gsm_debug_words__tmp+ 1 * s V_gsm_debug_words__tmp1 + 1 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words_nprinted + -6 <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0)%Z
   | 10 => (-1 * s V_gsm_debug_words_nprinted <= 0 /\ 1 * s V_gsm_debug_words_nprinted + -6 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 <= 0)%Z
   | 11 => (1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words_nprinted + -6 <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0)%Z
   | 12 => (-1 * s V_gsm_debug_words_nprinted <= 0 /\ 1 * s V_gsm_debug_words_nprinted + -6 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0)%Z
   | 13 => (1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ -1 * s V_gsm_debug_words_nprinted + 1 <= 0 /\ 1 * s V_gsm_debug_words_nprinted + -7 <= 0)%Z
   | 14 => (1 * s V_gsm_debug_words_nprinted + -7 <= 0 /\ -1 * s V_gsm_debug_words_nprinted + 1 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0)%Z
   | 15 => (1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ -1 * s V_gsm_debug_words_nprinted + 1 <= 0 /\ 1 * s V_gsm_debug_words_nprinted + -6 <= 0)%Z
   | 16 => (1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words_nprinted + -7 <= 0 /\ -1 * s V_gsm_debug_words_nprinted + 7 <= 0)%Z
   | 17 => (-1 * s V_gsm_debug_words_nprinted + 7 <= 0 /\ 1 * s V_gsm_debug_words_nprinted + -7 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0)%Z
   | 18 => (1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words_nprinted <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0)%Z
   | 19 => (-1 * s V_gsm_debug_words_nprinted <= 0 /\ 1 * s V_gsm_debug_words_nprinted <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0)%Z
   | 20 => (1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words_nprinted <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0 /\ -1 * s V_gsm_debug_words__tmp+ 1 * s V_gsm_debug_words__tmp1 <= 0)%Z
   | 21 => (-1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words_nprinted <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0 /\ 1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + 1 <= 0)%Z
   | 22 => (1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + 1 <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0 /\ 1 * s V_gsm_debug_words_nprinted <= 0 /\ -1 * s V_gsm_debug_words_z <= 0)%Z
   | 23 => (1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words_nprinted <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0)%Z
   | 24 => (1 * s V_gsm_debug_words_nprinted + -6 <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0)%Z
   | 25 => (1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0 /\ 1 * s V_gsm_debug_words_nprinted + -6 <= 0)%Z
   | 26 => (1 * s V_gsm_debug_words_nprinted + -6 <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0 /\ -1 * s V_gsm_debug_words_z <= 0 /\ 1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0)%Z
   | 27 => (1 * s V_gsm_debug_words__tmp+ -1 * s V_gsm_debug_words__tmp1 + -1 <= 0 /\ -1 * s V_gsm_debug_words_nprinted <= 0 /\ 1 * s V_gsm_debug_words_nprinted + -6 <= 0 /\ -1 * s V_gsm_debug_words_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gsm_debug_words (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(1 - s V_gsm_debug_words_from + s V_gsm_debug_words_to) <= z)%Q
   | 2 => (s V_gsm_debug_words_z
           + max0(1 - s V_gsm_debug_words_from + s V_gsm_debug_words_to) <= z)%Q
   | 3 => (s V_gsm_debug_words_z
           + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words_to) <= z)%Q
   | 4 => (s V_gsm_debug_words_z
           + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 5 => (s V_gsm_debug_words_z
           + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 6 => (s V_gsm_debug_words_z
           + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 7 => (s V_gsm_debug_words_z
           + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 - s V_gsm_debug_words__tmp
                                             + s V_gsm_debug_words__tmp1) (-
                                                                    s V_gsm_debug_words__tmp
                                                                    + s V_gsm_debug_words__tmp1));
      (*-1 0*) F_max0_ge_0 (-s V_gsm_debug_words__tmp
                            + s V_gsm_debug_words__tmp1)]
     (s V_gsm_debug_words_z
      + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 9 => (s V_gsm_debug_words_z <= z)%Q
   | 10 => hints
     [(*0 1*) F_max0_pre_decrement 1 (1 - s V_gsm_debug_words__tmp
                                      + s V_gsm_debug_words__tmp1) (1)]
     (s V_gsm_debug_words_z
      + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 11 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(-s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 12 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 13 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                  + s V_gsm_debug_words_nprinted)) (F_check_ge (-1
                                                                    + s V_gsm_debug_words_nprinted) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_gsm_debug_words_nprinted) (0))) (F_max0_ge_0 (-1
                                                                    + s V_gsm_debug_words_nprinted))]
     ((1 # 1) + s V_gsm_debug_words_z
      + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 14 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 15 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 16 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 17 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 18 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 19 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 20 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 21 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 22 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 23 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 24 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 25 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 26 => ((1 # 1) + s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | 27 => (s V_gsm_debug_words_z
            + max0(1 - s V_gsm_debug_words__tmp + s V_gsm_debug_words__tmp1) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gsm_debug_words =>
    [mkPA Q (fun n z s => ai_gsm_debug_words n s /\ annot0_gsm_debug_words n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gsm_debug_words (proc_start P_gsm_debug_words) s1 (proc_end P_gsm_debug_words) s2 ->
    (s2 V_gsm_debug_words_z <= max0(1 - s1 V_gsm_debug_words_from
                                    + s1 V_gsm_debug_words_to))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gsm_debug_words.
Qed.
