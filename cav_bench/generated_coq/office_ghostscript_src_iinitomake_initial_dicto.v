Require Import pasta.Pasta.

Inductive proc: Type :=
  P_make_initial_dict.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_make_initial_dict_z := 1%positive.
Notation V_make_initial_dict_code := 2%positive.
Notation V_make_initial_dict_dsize := 3%positive.
Notation V_make_initial_dict_i := 4%positive.
Notation V_make_initial_dict_space := 5%positive.
Notation V_make_initial_dict_idicts := 6%positive.
Notation V_make_initial_dict_iname := 7%positive.
Definition Pedges_make_initial_dict: list (edge proc) :=
  (EA 1 (AAssign V_make_initial_dict_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_make_initial_dict_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 ANone 7)::(EA 4 ANone 5)::
  (EA 5 ANone 6)::(EA 6 AWeaken 31)::(EA 7 (AAssign V_make_initial_dict_i
  (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_make_initial_dict_i) s) < (eval (ENum (5))
  s))%Z)) 14)::(EA 10 (AGuard (fun s => ((eval (EVar V_make_initial_dict_i)
  s) >= (eval (ENum (5)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 ANone 13)::
  (EA 13 AWeaken 31)::(EA 14 AWeaken 15)::(EA 15 (AAssign
  V_make_initial_dict_dsize None) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 32)::
  (EA 17 ANone 18)::(EA 18 AWeaken 19)::(EA 19 ANone 20)::(EA 19 ANone 26)::
  (EA 20 (AAssign V_make_initial_dict_space None) 21)::(EA 21 (AAssign
  V_make_initial_dict_code None) 22)::(EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_make_initial_dict_code) s) < (eval (ENum (0))
  s))%Z)) 28)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_make_initial_dict_code) s) >= (eval (ENum (0))
  s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 ANone 26)::(EA 26 ANone 27)::
  (EA 27 AWeaken 31)::(EA 28 AWeaken 29)::(EA 29 ANone 30)::
  (EA 30 AWeaken 31)::(EA 32 ANone 33)::(EA 33 (AAssign V_make_initial_dict_i
  (Some (EAdd (EVar V_make_initial_dict_i) (ENum (1))))) 34)::
  (EA 34 ANone 35)::(EA 35 ANone 36)::(EA 36 (AAssign V_make_initial_dict_z
  (Some (EAdd (ENum (1)) (EVar V_make_initial_dict_z)))) 37)::
  (EA 37 AWeaken 10)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_make_initial_dict => Pedges_make_initial_dict
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_make_initial_dict => 31
     end)%positive;
  var_global := var_global
}.

Definition ai_make_initial_dict (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_z <= 0)%Z
   | 3 => (-1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 4 => (-1 * s V_make_initial_dict_i <= 0 /\ 1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_z <= 0)%Z
   | 5 => (-1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 6 => (-1 * s V_make_initial_dict_i <= 0 /\ 1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_z <= 0)%Z
   | 7 => (-1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 8 => (1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 9 => (-1 * s V_make_initial_dict_i <= 0 /\ 1 * s V_make_initial_dict_i <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_z <= 0)%Z
   | 10 => (-1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0 /\ 1 * s V_make_initial_dict_i + -5 <= 0)%Z
   | 11 => (1 * s V_make_initial_dict_i + -5 <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i + 5 <= 0)%Z
   | 12 => (-1 * s V_make_initial_dict_i + 5 <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -5 <= 0)%Z
   | 13 => (1 * s V_make_initial_dict_i + -5 <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i + 5 <= 0)%Z
   | 14 => (-1 * s V_make_initial_dict_i <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -4 <= 0)%Z
   | 15 => (1 * s V_make_initial_dict_i + -4 <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 16 => (-1 * s V_make_initial_dict_i <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -4 <= 0)%Z
   | 17 => (1 * s V_make_initial_dict_i + -4 <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 18 => (-1 * s V_make_initial_dict_i <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -4 <= 0)%Z
   | 19 => (1 * s V_make_initial_dict_i + -4 <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 20 => (-1 * s V_make_initial_dict_i <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -4 <= 0)%Z
   | 21 => (1 * s V_make_initial_dict_i + -4 <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 22 => (-1 * s V_make_initial_dict_i <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -4 <= 0)%Z
   | 23 => (1 * s V_make_initial_dict_i + -4 <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 24 => (-1 * s V_make_initial_dict_i <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -4 <= 0 /\ -1 * s V_make_initial_dict_code <= 0)%Z
   | 25 => (-1 * s V_make_initial_dict_code <= 0 /\ 1 * s V_make_initial_dict_i + -4 <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 26 => (-1 * s V_make_initial_dict_i <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -4 <= 0)%Z
   | 27 => (1 * s V_make_initial_dict_i + -4 <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 28 => (-1 * s V_make_initial_dict_i <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -4 <= 0 /\ 1 * s V_make_initial_dict_code + 1 <= 0)%Z
   | 29 => (1 * s V_make_initial_dict_code + 1 <= 0 /\ 1 * s V_make_initial_dict_i + -4 <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 30 => (-1 * s V_make_initial_dict_i <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -4 <= 0 /\ 1 * s V_make_initial_dict_code + 1 <= 0)%Z
   | 31 => (-1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 32 => (-1 * s V_make_initial_dict_i <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -4 <= 0)%Z
   | 33 => (1 * s V_make_initial_dict_i + -4 <= 0 /\ -1 * s V_make_initial_dict_z <= 0 /\ -1 * s V_make_initial_dict_i <= 0)%Z
   | 34 => (-1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -5 <= 0 /\ -1 * s V_make_initial_dict_i + 1 <= 0)%Z
   | 35 => (-1 * s V_make_initial_dict_i + 1 <= 0 /\ 1 * s V_make_initial_dict_i + -5 <= 0 /\ -1 * s V_make_initial_dict_z <= 0)%Z
   | 36 => (-1 * s V_make_initial_dict_z <= 0 /\ 1 * s V_make_initial_dict_i + -5 <= 0 /\ -1 * s V_make_initial_dict_i + 1 <= 0)%Z
   | 37 => (-1 * s V_make_initial_dict_i + 1 <= 0 /\ 1 * s V_make_initial_dict_i + -5 <= 0 /\ -1 * s V_make_initial_dict_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_make_initial_dict (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((5 # 1) <= z)%Q
   | 2 => ((5 # 1) + s V_make_initial_dict_z <= z)%Q
   | 3 => ((5 # 1) + s V_make_initial_dict_z <= z)%Q
   | 4 => ((5 # 1) + s V_make_initial_dict_z <= z)%Q
   | 5 => ((5 # 1) + s V_make_initial_dict_z <= z)%Q
   | 6 => hints
     [(*-5 0*) F_one]
     ((5 # 1) + s V_make_initial_dict_z <= z)%Q
   | 7 => ((5 # 1) + s V_make_initial_dict_z <= z)%Q
   | 8 => (s V_make_initial_dict_z + max0(5 - s V_make_initial_dict_i) <= z)%Q
   | 9 => (s V_make_initial_dict_z + max0(5 - s V_make_initial_dict_i) <= z)%Q
   | 10 => (s V_make_initial_dict_z + max0(5 - s V_make_initial_dict_i) <= z)%Q
   | 11 => (s V_make_initial_dict_z + max0(5 - s V_make_initial_dict_i) <= z)%Q
   | 12 => (s V_make_initial_dict_z + max0(5 - s V_make_initial_dict_i) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (5 - s V_make_initial_dict_i) (4
                                                                    - s V_make_initial_dict_i));
      (*-1 0*) F_max0_ge_0 (4 - s V_make_initial_dict_i)]
     (s V_make_initial_dict_z + max0(5 - s V_make_initial_dict_i) <= z)%Q
   | 14 => hints
     [(*0 1*) F_max0_pre_decrement 1 (5 - s V_make_initial_dict_i) (1)]
     (s V_make_initial_dict_z + max0(5 - s V_make_initial_dict_i) <= z)%Q
   | 15 => ((1 # 1) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i) <= z)%Q
   | 16 => ((1 # 1) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i) <= z)%Q
   | 17 => ((1 # 1) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i) <= z)%Q
   | 18 => hints
     [(*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_make_initial_dict_i) (0))) (F_max0_ge_0 (s V_make_initial_dict_i));
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (5
                                                                  - s V_make_initial_dict_i) (0))) (F_max0_ge_0 (5
                                                                    - s V_make_initial_dict_i))]
     ((1 # 1) + s V_make_initial_dict_z + max0(4 - s V_make_initial_dict_i) <= z)%Q
   | 19 => (-(1 # 4) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i)
            + (1 # 4) * max0(5 - s V_make_initial_dict_i)
            + (1 # 4) * max0(s V_make_initial_dict_i) <= z)%Q
   | 20 => (-(1 # 4) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i)
            + (1 # 4) * max0(5 - s V_make_initial_dict_i)
            + (1 # 4) * max0(s V_make_initial_dict_i) <= z)%Q
   | 21 => (-(1 # 4) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i)
            + (1 # 4) * max0(5 - s V_make_initial_dict_i)
            + (1 # 4) * max0(s V_make_initial_dict_i) <= z)%Q
   | 22 => (-(1 # 4) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i)
            + (1 # 4) * max0(5 - s V_make_initial_dict_i)
            + (1 # 4) * max0(s V_make_initial_dict_i) <= z)%Q
   | 23 => (-(1 # 4) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i)
            + (1 # 4) * max0(5 - s V_make_initial_dict_i)
            + (1 # 4) * max0(s V_make_initial_dict_i) <= z)%Q
   | 24 => (-(1 # 4) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i)
            + (1 # 4) * max0(5 - s V_make_initial_dict_i)
            + (1 # 4) * max0(s V_make_initial_dict_i) <= z)%Q
   | 25 => (-(1 # 4) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i)
            + (1 # 4) * max0(5 - s V_make_initial_dict_i)
            + (1 # 4) * max0(s V_make_initial_dict_i) <= z)%Q
   | 26 => (-(1 # 4) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i)
            + (1 # 4) * max0(5 - s V_make_initial_dict_i)
            + (1 # 4) * max0(s V_make_initial_dict_i) <= z)%Q
   | 27 => hints
     [(*-0.25 0*) F_max0_pre_decrement 1 (5 - s V_make_initial_dict_i) (1);
      (*-1.25 0*) F_max0_ge_0 (4 - s V_make_initial_dict_i);
      (*-0.25 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_make_initial_dict_i)) (F_check_ge (0) (0))]
     (-(1 # 4) + s V_make_initial_dict_z + max0(4 - s V_make_initial_dict_i)
      + (1 # 4) * max0(5 - s V_make_initial_dict_i)
      + (1 # 4) * max0(s V_make_initial_dict_i) <= z)%Q
   | 28 => hints
     [(*0 0.2*) F_binom_monotonic 1 (F_max0_ge_0 (5 - s V_make_initial_dict_i)) (F_check_ge (0) (0));
      (*0 0.05*) F_binom_monotonic 1 (F_max0_ge_arg (5
                                                     - s V_make_initial_dict_i)) (F_check_ge (5
                                                                    - s V_make_initial_dict_i) (0))]
     (-(1 # 4) + s V_make_initial_dict_z + max0(4 - s V_make_initial_dict_i)
      + (1 # 4) * max0(5 - s V_make_initial_dict_i)
      + (1 # 4) * max0(s V_make_initial_dict_i) <= z)%Q
   | 29 => (-(1 # 20) * s V_make_initial_dict_i + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i)
            + (1 # 4) * max0(s V_make_initial_dict_i) <= z)%Q
   | 30 => hints
     [(*-1 0*) F_max0_ge_0 (4 - s V_make_initial_dict_i);
      (*-0.2 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_make_initial_dict_i)) (F_check_ge (0) (0));
      (*-0.05 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_make_initial_dict_i)) (F_check_ge (s V_make_initial_dict_i) (0))]
     (-(1 # 20) * s V_make_initial_dict_i + s V_make_initial_dict_z
      + max0(4 - s V_make_initial_dict_i)
      + (1 # 4) * max0(s V_make_initial_dict_i) <= z)%Q
   | 31 => (s V_make_initial_dict_z <= z)%Q
   | 32 => ((1 # 1) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i) <= z)%Q
   | 33 => ((1 # 1) + s V_make_initial_dict_z
            + max0(4 - s V_make_initial_dict_i) <= z)%Q
   | 34 => ((1 # 1) + s V_make_initial_dict_z
            + max0(5 - s V_make_initial_dict_i) <= z)%Q
   | 35 => ((1 # 1) + s V_make_initial_dict_z
            + max0(5 - s V_make_initial_dict_i) <= z)%Q
   | 36 => ((1 # 1) + s V_make_initial_dict_z
            + max0(5 - s V_make_initial_dict_i) <= z)%Q
   | 37 => (s V_make_initial_dict_z + max0(5 - s V_make_initial_dict_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_make_initial_dict =>
    [mkPA Q (fun n z s => ai_make_initial_dict n s /\ annot0_make_initial_dict n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_make_initial_dict (proc_start P_make_initial_dict) s1 (proc_end P_make_initial_dict) s2 ->
    (s2 V_make_initial_dict_z <= (5 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_make_initial_dict.
Qed.
