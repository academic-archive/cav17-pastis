Require Import pasta.Pasta.

Inductive proc: Type :=
  P_Short_term_analysis_filtering.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_Short_term_analysis_filtering_z := 1%positive.
Notation V_Short_term_analysis_filtering__tmp := 2%positive.
Notation V_Short_term_analysis_filtering_di := 3%positive.
Notation V_Short_term_analysis_filtering_i := 4%positive.
Notation V_Short_term_analysis_filtering_ltmp := 5%positive.
Notation V_Short_term_analysis_filtering_rpi := 6%positive.
Notation V_Short_term_analysis_filtering_sav := 7%positive.
Notation V_Short_term_analysis_filtering_ui := 8%positive.
Notation V_Short_term_analysis_filtering_zzz := 9%positive.
Notation V_Short_term_analysis_filtering_S := 10%positive.
Notation V_Short_term_analysis_filtering_k_n := 11%positive.
Notation V_Short_term_analysis_filtering_rp := 12%positive.
Notation V_Short_term_analysis_filtering_s := 13%positive.
Definition Pedges_Short_term_analysis_filtering: list (edge proc) :=
  (EA 1 (AAssign V_Short_term_analysis_filtering_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_Short_term_analysis_filtering__tmp
  (Some (EVar V_Short_term_analysis_filtering_k_n))) 3)::(EA 3 ANone 4)::
  (EA 4 (AAssign V_Short_term_analysis_filtering__tmp
  (Some (EAdd (EVar V_Short_term_analysis_filtering__tmp) (ENum (-1))))) 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_Short_term_analysis_filtering__tmp) s) <>
  (eval (ENum (0)) s))%Z)) 9)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_Short_term_analysis_filtering__tmp) s) =
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 (AAssign V_Short_term_analysis_filtering_sav None) 11)::
  (EA 11 (AAssign V_Short_term_analysis_filtering_di None) 12)::
  (EA 12 (AAssign V_Short_term_analysis_filtering_i (Some (ENum (0)))) 13)::
  (EA 13 ANone 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_Short_term_analysis_filtering_i) s) <
  (eval (ENum (8)) s))%Z)) 21)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_Short_term_analysis_filtering_i) s) >=
  (eval (ENum (8)) s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_Short_term_analysis_filtering_z (Some (EAdd (ENum (1))
  (EVar V_Short_term_analysis_filtering_z)))) 4)::(EA 21 AWeaken 22)::
  (EA 22 (AAssign V_Short_term_analysis_filtering_ui None) 23)::
  (EA 23 (AAssign V_Short_term_analysis_filtering_rpi None) 24)::
  (EA 24 (AAssign V_Short_term_analysis_filtering_zzz None) 25)::
  (EA 25 (AAssign V_Short_term_analysis_filtering_ltmp
  (Some (EAdd (EVar V_Short_term_analysis_filtering_ui)
  (EVar V_Short_term_analysis_filtering_zzz)))) 26)::(EA 26 AWeaken 27)::
  (EA 27 ANone 29)::(EA 27 ANone 28)::(EA 28 ANone 30)::(EA 29 ANone 30)::
  (EA 30 (AAssign V_Short_term_analysis_filtering_sav None) 31)::
  (EA 31 (AAssign V_Short_term_analysis_filtering_zzz None) 32)::
  (EA 32 (AAssign V_Short_term_analysis_filtering_ltmp
  (Some (EAdd (EVar V_Short_term_analysis_filtering_di)
  (EVar V_Short_term_analysis_filtering_zzz)))) 33)::(EA 33 AWeaken 34)::
  (EA 34 ANone 36)::(EA 34 ANone 35)::(EA 35 ANone 37)::(EA 36 ANone 37)::
  (EA 37 (AAssign V_Short_term_analysis_filtering_di None) 38)::
  (EA 38 ANone 39)::(EA 39 (AAssign V_Short_term_analysis_filtering_i
  (Some (EAdd (EVar V_Short_term_analysis_filtering_i) (ENum (1))))) 40)::
  (EA 40 ANone 41)::(EA 41 ANone 42)::(EA 42 (AAssign
  V_Short_term_analysis_filtering_z (Some (EAdd (ENum (1))
  (EVar V_Short_term_analysis_filtering_z)))) 43)::(EA 43 AWeaken 15)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_Short_term_analysis_filtering => Pedges_Short_term_analysis_filtering
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_Short_term_analysis_filtering => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_Short_term_analysis_filtering (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0)%Z
   | 3 => (-1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_z <= 0)%Z
   | 4 => (-1 * s V_Short_term_analysis_filtering_z <= 0)%Z
   | 5 => (-1 * s V_Short_term_analysis_filtering_z <= 0)%Z
   | 6 => (-1 * s V_Short_term_analysis_filtering_z <= 0)%Z
   | 7 => (-1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering__tmp <= 0 /\ -1 * s V_Short_term_analysis_filtering__tmp <= 0)%Z
   | 8 => (-1 * s V_Short_term_analysis_filtering__tmp <= 0 /\ 1 * s V_Short_term_analysis_filtering__tmp <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0)%Z
   | 9 => (-1 * s V_Short_term_analysis_filtering_z <= 0)%Z
   | 10 => (-1 * s V_Short_term_analysis_filtering_z <= 0)%Z
   | 11 => (-1 * s V_Short_term_analysis_filtering_z <= 0)%Z
   | 12 => (-1 * s V_Short_term_analysis_filtering_z <= 0)%Z
   | 13 => (-1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_i <= 0 /\ -1 * s V_Short_term_analysis_filtering_i <= 0)%Z
   | 14 => (-1 * s V_Short_term_analysis_filtering_i <= 0 /\ 1 * s V_Short_term_analysis_filtering_i <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0)%Z
   | 15 => (-1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -8 <= 0)%Z
   | 16 => (1 * s V_Short_term_analysis_filtering_i + -8 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i + 8 <= 0)%Z
   | 17 => (-1 * s V_Short_term_analysis_filtering_i + 8 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -8 <= 0)%Z
   | 18 => (1 * s V_Short_term_analysis_filtering_i + -8 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i + 8 <= 0)%Z
   | 19 => (-1 * s V_Short_term_analysis_filtering_i + 8 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -8 <= 0)%Z
   | 20 => (1 * s V_Short_term_analysis_filtering_i + -8 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i + 8 <= 0)%Z
   | 21 => (-1 * s V_Short_term_analysis_filtering_i <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -7 <= 0)%Z
   | 22 => (1 * s V_Short_term_analysis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i <= 0)%Z
   | 23 => (-1 * s V_Short_term_analysis_filtering_i <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -7 <= 0)%Z
   | 24 => (1 * s V_Short_term_analysis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i <= 0)%Z
   | 25 => (-1 * s V_Short_term_analysis_filtering_i <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -7 <= 0)%Z
   | 26 => (1 * s V_Short_term_analysis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i <= 0)%Z
   | 27 => (-1 * s V_Short_term_analysis_filtering_i <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -7 <= 0)%Z
   | 28 => (1 * s V_Short_term_analysis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i <= 0)%Z
   | 29 => (1 * s V_Short_term_analysis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i <= 0)%Z
   | 30 => (-1 * s V_Short_term_analysis_filtering_i <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -7 <= 0)%Z
   | 31 => (1 * s V_Short_term_analysis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i <= 0)%Z
   | 32 => (-1 * s V_Short_term_analysis_filtering_i <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -7 <= 0)%Z
   | 33 => (1 * s V_Short_term_analysis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i <= 0)%Z
   | 34 => (-1 * s V_Short_term_analysis_filtering_i <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -7 <= 0)%Z
   | 35 => (1 * s V_Short_term_analysis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i <= 0)%Z
   | 36 => (1 * s V_Short_term_analysis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i <= 0)%Z
   | 37 => (-1 * s V_Short_term_analysis_filtering_i <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -7 <= 0)%Z
   | 38 => (1 * s V_Short_term_analysis_filtering_i + -7 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i <= 0)%Z
   | 39 => (-1 * s V_Short_term_analysis_filtering_i <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -7 <= 0)%Z
   | 40 => (-1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i + 1 <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -8 <= 0)%Z
   | 41 => (1 * s V_Short_term_analysis_filtering_i + -8 <= 0 /\ -1 * s V_Short_term_analysis_filtering_i + 1 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z <= 0)%Z
   | 42 => (-1 * s V_Short_term_analysis_filtering_z <= 0 /\ -1 * s V_Short_term_analysis_filtering_i + 1 <= 0 /\ 1 * s V_Short_term_analysis_filtering_i + -8 <= 0)%Z
   | 43 => (1 * s V_Short_term_analysis_filtering_i + -8 <= 0 /\ -1 * s V_Short_term_analysis_filtering_i + 1 <= 0 /\ -1 * s V_Short_term_analysis_filtering_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_Short_term_analysis_filtering (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((9 # 1) * s V_Short_term_analysis_filtering_k_n <= z)%Q
   | 2 => ((9 # 1) * s V_Short_term_analysis_filtering_k_n
           + s V_Short_term_analysis_filtering_z <= z)%Q
   | 3 => ((9 # 1) * s V_Short_term_analysis_filtering__tmp
           + s V_Short_term_analysis_filtering_z <= z)%Q
   | 4 => ((9 # 1) * s V_Short_term_analysis_filtering__tmp
           + s V_Short_term_analysis_filtering_z <= z)%Q
   | 5 => ((9 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
           + s V_Short_term_analysis_filtering_z <= z)%Q
   | 6 => ((9 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
           + s V_Short_term_analysis_filtering_z <= z)%Q
   | 7 => hints
     [(*-9 0*) F_one;
      (*-9 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_Short_term_analysis_filtering__tmp)) (F_check_ge (0) (0));
      (*-9 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_Short_term_analysis_filtering__tmp) (0))) (F_max0_ge_0 (s V_Short_term_analysis_filtering__tmp))]
     ((9 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
      + s V_Short_term_analysis_filtering_z <= z)%Q
   | 8 => (s V_Short_term_analysis_filtering_z <= z)%Q
   | 9 => ((9 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
           + s V_Short_term_analysis_filtering_z <= z)%Q
   | 10 => ((9 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z <= z)%Q
   | 11 => ((9 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z <= z)%Q
   | 12 => ((9 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z <= z)%Q
   | 13 => ((1 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(8 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 14 => ((1 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(8 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 15 => ((1 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(8 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 16 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (8
                                            - s V_Short_term_analysis_filtering_i) (7
                                                                    - s V_Short_term_analysis_filtering_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (7
                                                 - s V_Short_term_analysis_filtering_i)) (F_check_ge (0) (0))]
     ((1 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
      + s V_Short_term_analysis_filtering_z
      + max0(8 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 17 => ((1 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z <= z)%Q
   | 18 => ((1 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z <= z)%Q
   | 19 => ((1 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z <= z)%Q
   | 20 => ((1 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z <= z)%Q
   | 21 => hints
     [(*0 1*) F_max0_pre_decrement 1 (8 - s V_Short_term_analysis_filtering_i) (1)]
     ((1 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
      + s V_Short_term_analysis_filtering_z
      + max0(8 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 22 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 23 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 24 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 25 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 26 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 27 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 28 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 29 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 30 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 31 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 32 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 33 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 34 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 35 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 36 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 37 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 38 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 39 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(7 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 40 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(8 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 41 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(8 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 42 => ((2 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(8 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | 43 => ((1 # 1) + (9 # 1) * s V_Short_term_analysis_filtering__tmp
            + s V_Short_term_analysis_filtering_z
            + max0(8 - s V_Short_term_analysis_filtering_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_Short_term_analysis_filtering =>
    [mkPA Q (fun n z s => ai_Short_term_analysis_filtering n s /\ annot0_Short_term_analysis_filtering n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_Short_term_analysis_filtering (proc_start P_Short_term_analysis_filtering) s1 (proc_end P_Short_term_analysis_filtering) s2 ->
    (s2 V_Short_term_analysis_filtering_z <= (9 # 1) * s1 V_Short_term_analysis_filtering_k_n)%Q.
Proof.
  prove_bound ipa admissible_ipa P_Short_term_analysis_filtering.
Qed.
