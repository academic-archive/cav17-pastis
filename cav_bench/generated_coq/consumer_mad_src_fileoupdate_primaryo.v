Require Import pasta.Pasta.

Inductive proc: Type :=
  P_update_primary.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_update_primary_z := 1%positive.
Notation V_update_primary__tmp := 2%positive.
Notation V_update_primary_i := 3%positive.
Notation V_update_primary_new_dref_off24 := 4%positive.
Notation V_update_primary_new := 5%positive.
Notation V_update_primary_tag := 6%positive.
Definition Pedges_update_primary: list (edge proc) :=
  (EA 1 (AAssign V_update_primary_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_update_primary_new_dref_off24) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_update_primary_i) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 ANone 7)::(EA 5 ANone 6)::
  (EA 6 ANone 7)::(EA 7 (AAssign V_update_primary_i (Some (ENum (0)))) 8)::
  (EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_update_primary_i) s) <
  (eval (EVar V_update_primary_new_dref_off24) s))%Z)) 15)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_update_primary_i) s) >=
  (eval (EVar V_update_primary_new_dref_off24) s))%Z)) 11)::
  (EA 11 AWeaken 12)::(EA 12 (AAssign V_update_primary__tmp
  (Some (ENum (0)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 26)::
  (EA 15 AWeaken 16)::(EA 16 ANone 23)::(EA 16 ANone 17)::(EA 17 ANone 18)::
  (EA 18 (AAssign V_update_primary_i (Some (EAdd (EVar V_update_primary_i)
  (ENum (1))))) 19)::(EA 19 ANone 20)::(EA 20 ANone 21)::(EA 21 (AAssign
  V_update_primary_z (Some (EAdd (ENum (1))
  (EVar V_update_primary_z)))) 22)::(EA 22 AWeaken 10)::(EA 23 (AAssign
  V_update_primary__tmp (Some (ENum (-1)))) 24)::(EA 24 ANone 25)::
  (EA 25 AWeaken 26)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_update_primary => Pedges_update_primary
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_update_primary => 26
     end)%positive;
  var_global := var_global
}.

Definition ai_update_primary (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_z <= 0)%Z
   | 3 => (-1 * s V_update_primary_z <= 0 /\ 1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_new_dref_off24 <= 0)%Z
   | 4 => (-1 * s V_update_primary_new_dref_off24 <= 0 /\ 1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_i <= 0)%Z
   | 5 => (-1 * s V_update_primary_i <= 0 /\ -1 * s V_update_primary_z <= 0 /\ 1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_new_dref_off24 <= 0)%Z
   | 6 => (-1 * s V_update_primary_new_dref_off24 <= 0 /\ 1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_i <= 0)%Z
   | 7 => (-1 * s V_update_primary_i <= 0 /\ -1 * s V_update_primary_z <= 0 /\ 1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_new_dref_off24 <= 0)%Z
   | 8 => (-1 * s V_update_primary_new_dref_off24 <= 0 /\ 1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_z <= 0 /\ 1 * s V_update_primary_i <= 0 /\ -1 * s V_update_primary_i <= 0)%Z
   | 9 => (-1 * s V_update_primary_i <= 0 /\ 1 * s V_update_primary_i <= 0 /\ -1 * s V_update_primary_z <= 0 /\ 1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_new_dref_off24 <= 0)%Z
   | 10 => (-1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_i <= 0 /\ 1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 <= 0)%Z
   | 11 => (1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 <= 0 /\ -1 * s V_update_primary_i <= 0 /\ -1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_i+ 1 * s V_update_primary_new_dref_off24 <= 0)%Z
   | 12 => (-1 * s V_update_primary_i+ 1 * s V_update_primary_new_dref_off24 <= 0 /\ -1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_i <= 0 /\ 1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 <= 0)%Z
   | 13 => (1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 <= 0 /\ -1 * s V_update_primary_i <= 0 /\ -1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_i+ 1 * s V_update_primary_new_dref_off24 <= 0 /\ 1 * s V_update_primary__tmp <= 0 /\ -1 * s V_update_primary__tmp <= 0)%Z
   | 14 => (-1 * s V_update_primary__tmp <= 0 /\ 1 * s V_update_primary__tmp <= 0 /\ -1 * s V_update_primary_i+ 1 * s V_update_primary_new_dref_off24 <= 0 /\ -1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_i <= 0 /\ 1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 <= 0)%Z
   | 15 => (-1 * s V_update_primary_i <= 0 /\ -1 * s V_update_primary_z <= 0 /\ 1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 + 1 <= 0)%Z
   | 16 => (1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 + 1 <= 0 /\ -1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_i <= 0)%Z
   | 17 => (-1 * s V_update_primary_i <= 0 /\ -1 * s V_update_primary_z <= 0 /\ 1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 + 1 <= 0)%Z
   | 18 => (1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 + 1 <= 0 /\ -1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_i <= 0)%Z
   | 19 => (-1 * s V_update_primary_z <= 0 /\ 1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 <= 0 /\ -1 * s V_update_primary_i + 1 <= 0)%Z
   | 20 => (-1 * s V_update_primary_i + 1 <= 0 /\ 1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 <= 0 /\ -1 * s V_update_primary_z <= 0)%Z
   | 21 => (-1 * s V_update_primary_z <= 0 /\ 1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 <= 0 /\ -1 * s V_update_primary_i + 1 <= 0)%Z
   | 22 => (-1 * s V_update_primary_i + 1 <= 0 /\ 1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 <= 0 /\ -1 * s V_update_primary_z + 1 <= 0)%Z
   | 23 => (-1 * s V_update_primary_i <= 0 /\ -1 * s V_update_primary_z <= 0 /\ 1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 + 1 <= 0)%Z
   | 24 => (1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 + 1 <= 0 /\ -1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_i <= 0 /\ 1 * s V_update_primary__tmp + 1 <= 0 /\ -1 * s V_update_primary__tmp + -1 <= 0)%Z
   | 25 => (-1 * s V_update_primary__tmp + -1 <= 0 /\ 1 * s V_update_primary__tmp + 1 <= 0 /\ -1 * s V_update_primary_i <= 0 /\ -1 * s V_update_primary_z <= 0 /\ 1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 + 1 <= 0)%Z
   | 26 => (1 * s V_update_primary_i+ -1 * s V_update_primary_new_dref_off24 <= 0 /\ 1 * s V_update_primary__tmp <= 0 /\ -1 * s V_update_primary_z <= 0 /\ -1 * s V_update_primary_i <= 0 /\ -1 * s V_update_primary__tmp + -1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_update_primary (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 2 => (s V_update_primary_z + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 3 => (s V_update_primary_z + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 4 => (s V_update_primary_z + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 5 => (s V_update_primary_z + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 6 => (s V_update_primary_z + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 7 => (s V_update_primary_z + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 8 => (-s V_update_primary_i + s V_update_primary_z
           + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 9 => (-s V_update_primary_i + s V_update_primary_z
           + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 10 => (-s V_update_primary_i + s V_update_primary_z
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 11 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_update_primary_i
                                                              + s V_update_primary_new_dref_off24) (0))) (F_max0_ge_0 (-
                                                                    s V_update_primary_i
                                                                    + s V_update_primary_new_dref_off24))]
     (-s V_update_primary_i + s V_update_primary_z
      + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 12 => (-s V_update_primary_new_dref_off24 + s V_update_primary_z
            + max0(-s V_update_primary_i + s V_update_primary_new_dref_off24)
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 13 => (-s V_update_primary_new_dref_off24 + s V_update_primary_z
            + max0(-s V_update_primary_i + s V_update_primary_new_dref_off24)
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_update_primary_i
                                             + s V_update_primary_new_dref_off24) (-1
                                                                    - s V_update_primary_i
                                                                    + s V_update_primary_new_dref_off24));
      (*-1 0*) F_max0_ge_0 (-1 - s V_update_primary_i
                            + s V_update_primary_new_dref_off24);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_update_primary_new_dref_off24)) (F_check_ge (s V_update_primary_new_dref_off24) (0))]
     (-s V_update_primary_new_dref_off24 + s V_update_primary_z
      + max0(-s V_update_primary_i + s V_update_primary_new_dref_off24)
      + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 15 => (-s V_update_primary_i + s V_update_primary_z
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 16 => (-s V_update_primary_i + s V_update_primary_z
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 17 => (-s V_update_primary_i + s V_update_primary_z
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 18 => (-s V_update_primary_i + s V_update_primary_z
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 19 => ((1 # 1) - s V_update_primary_i + s V_update_primary_z
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 20 => ((1 # 1) - s V_update_primary_i + s V_update_primary_z
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 21 => ((1 # 1) - s V_update_primary_i + s V_update_primary_z
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 22 => (-s V_update_primary_i + s V_update_primary_z
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 23 => (-s V_update_primary_i + s V_update_primary_z
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 24 => (-s V_update_primary_i + s V_update_primary_z
            + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (-s V_update_primary_i
                                       + s V_update_primary_new_dref_off24) (1);
      (*-1 0*) F_max0_ge_0 (-1 - s V_update_primary_i
                            + s V_update_primary_new_dref_off24);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_update_primary_new_dref_off24)) (F_check_ge (s V_update_primary_new_dref_off24) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_update_primary_i
                                                               + s V_update_primary_new_dref_off24) (0))) (F_max0_ge_0 (-
                                                                    s V_update_primary_i
                                                                    + s V_update_primary_new_dref_off24))]
     (-s V_update_primary_i + s V_update_primary_z
      + max0(s V_update_primary_new_dref_off24) <= z)%Q
   | 26 => (s V_update_primary_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_update_primary =>
    [mkPA Q (fun n z s => ai_update_primary n s /\ annot0_update_primary n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_update_primary (proc_start P_update_primary) s1 (proc_end P_update_primary) s2 ->
    (s2 V_update_primary_z <= max0(s1 V_update_primary_new_dref_off24))%Q.
Proof.
  prove_bound ipa admissible_ipa P_update_primary.
Qed.
