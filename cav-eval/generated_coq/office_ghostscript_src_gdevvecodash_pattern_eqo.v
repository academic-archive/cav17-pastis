Require Import pasta.Pasta.

Inductive proc: Type :=
  P_dash_pattern_eq.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_dash_pattern_eq_z := 1%positive.
Notation V_dash_pattern_eq__tmp := 2%positive.
Notation V_dash_pattern_eq_i := 3%positive.
Notation V_dash_pattern_eq_set_dref_off8 := 4%positive.
Notation V_dash_pattern_eq_scale := 5%positive.
Notation V_dash_pattern_eq_set := 6%positive.
Notation V_dash_pattern_eq_stored := 7%positive.
Definition Pedges_dash_pattern_eq: list (edge proc) :=
  (EA 1 (AAssign V_dash_pattern_eq_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_dash_pattern_eq_set_dref_off8) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_dash_pattern_eq_i) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_dash_pattern_eq_i
  (Some (ENum (0)))) 6)::(EA 6 ANone 7)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_dash_pattern_eq_i) s) <
  (eval (EVar V_dash_pattern_eq_set_dref_off8) s))%Z)) 13)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_dash_pattern_eq_i) s) >=
  (eval (EVar V_dash_pattern_eq_set_dref_off8) s))%Z)) 9)::
  (EA 9 AWeaken 10)::(EA 10 (AAssign V_dash_pattern_eq__tmp
  (Some (ENum (1)))) 11)::(EA 11 ANone 12)::(EA 12 AWeaken 24)::
  (EA 13 AWeaken 14)::(EA 14 ANone 21)::(EA 14 ANone 15)::(EA 15 ANone 16)::
  (EA 16 (AAssign V_dash_pattern_eq_i (Some (EAdd (EVar V_dash_pattern_eq_i)
  (ENum (1))))) 17)::(EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_dash_pattern_eq_z (Some (EAdd (ENum (1))
  (EVar V_dash_pattern_eq_z)))) 20)::(EA 20 AWeaken 8)::(EA 21 (AAssign
  V_dash_pattern_eq__tmp (Some (ENum (0)))) 22)::(EA 22 ANone 23)::
  (EA 23 AWeaken 24)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_dash_pattern_eq => Pedges_dash_pattern_eq
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_dash_pattern_eq => 24
     end)%positive;
  var_global := var_global
}.

Definition ai_dash_pattern_eq (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0)%Z
   | 3 => (-1 * s V_dash_pattern_eq_z <= 0 /\ 1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0)%Z
   | 4 => (-1 * s V_dash_pattern_eq_set_dref_off8 <= 0 /\ 1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_i <= 0)%Z
   | 5 => (-1 * s V_dash_pattern_eq_i <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ 1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0)%Z
   | 6 => (-1 * s V_dash_pattern_eq_set_dref_off8 <= 0 /\ 1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ 1 * s V_dash_pattern_eq_i <= 0 /\ -1 * s V_dash_pattern_eq_i <= 0)%Z
   | 7 => (-1 * s V_dash_pattern_eq_i <= 0 /\ 1 * s V_dash_pattern_eq_i <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ 1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0)%Z
   | 8 => (-1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_i <= 0 /\ 1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0)%Z
   | 9 => (1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0 /\ -1 * s V_dash_pattern_eq_i <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_i+ 1 * s V_dash_pattern_eq_set_dref_off8 <= 0)%Z
   | 10 => (-1 * s V_dash_pattern_eq_i+ 1 * s V_dash_pattern_eq_set_dref_off8 <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_i <= 0 /\ 1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0)%Z
   | 11 => (1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0 /\ -1 * s V_dash_pattern_eq_i <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_i+ 1 * s V_dash_pattern_eq_set_dref_off8 <= 0 /\ 1 * s V_dash_pattern_eq__tmp + -1 <= 0 /\ -1 * s V_dash_pattern_eq__tmp + 1 <= 0)%Z
   | 12 => (-1 * s V_dash_pattern_eq__tmp + 1 <= 0 /\ 1 * s V_dash_pattern_eq__tmp + -1 <= 0 /\ -1 * s V_dash_pattern_eq_i+ 1 * s V_dash_pattern_eq_set_dref_off8 <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_i <= 0 /\ 1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0)%Z
   | 13 => (-1 * s V_dash_pattern_eq_i <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ 1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 + 1 <= 0)%Z
   | 14 => (1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 + 1 <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_i <= 0)%Z
   | 15 => (-1 * s V_dash_pattern_eq_i <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ 1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 + 1 <= 0)%Z
   | 16 => (1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 + 1 <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_i <= 0)%Z
   | 17 => (-1 * s V_dash_pattern_eq_z <= 0 /\ 1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0 /\ -1 * s V_dash_pattern_eq_i + 1 <= 0)%Z
   | 18 => (-1 * s V_dash_pattern_eq_i + 1 <= 0 /\ 1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0)%Z
   | 19 => (-1 * s V_dash_pattern_eq_z <= 0 /\ 1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0 /\ -1 * s V_dash_pattern_eq_i + 1 <= 0)%Z
   | 20 => (-1 * s V_dash_pattern_eq_i + 1 <= 0 /\ 1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0 /\ -1 * s V_dash_pattern_eq_z + 1 <= 0)%Z
   | 21 => (-1 * s V_dash_pattern_eq_i <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ 1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 + 1 <= 0)%Z
   | 22 => (1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 + 1 <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_i <= 0 /\ 1 * s V_dash_pattern_eq__tmp <= 0 /\ -1 * s V_dash_pattern_eq__tmp <= 0)%Z
   | 23 => (-1 * s V_dash_pattern_eq__tmp <= 0 /\ 1 * s V_dash_pattern_eq__tmp <= 0 /\ -1 * s V_dash_pattern_eq_i <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ 1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 + 1 <= 0)%Z
   | 24 => (1 * s V_dash_pattern_eq_i+ -1 * s V_dash_pattern_eq_set_dref_off8 <= 0 /\ 1 * s V_dash_pattern_eq__tmp + -1 <= 0 /\ -1 * s V_dash_pattern_eq_z <= 0 /\ -1 * s V_dash_pattern_eq_i <= 0 /\ -1 * s V_dash_pattern_eq__tmp <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_dash_pattern_eq (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 2 => (s V_dash_pattern_eq_z + max0(s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 3 => (s V_dash_pattern_eq_z + max0(s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 4 => (s V_dash_pattern_eq_z + max0(s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 5 => (s V_dash_pattern_eq_z + max0(s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 6 => (s V_dash_pattern_eq_z
           + max0(-s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 7 => (s V_dash_pattern_eq_z
           + max0(-s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 8 => (s V_dash_pattern_eq_z
           + max0(-s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 9 => (s V_dash_pattern_eq_z
           + max0(-s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 10 => (s V_dash_pattern_eq_z
            + max0(-s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 11 => (s V_dash_pattern_eq_z
            + max0(-s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_dash_pattern_eq_i
                                             + s V_dash_pattern_eq_set_dref_off8) (-1
                                                                    - s V_dash_pattern_eq_i
                                                                    + s V_dash_pattern_eq_set_dref_off8));
      (*-1 0*) F_max0_ge_0 (-1 - s V_dash_pattern_eq_i
                            + s V_dash_pattern_eq_set_dref_off8)]
     (s V_dash_pattern_eq_z
      + max0(-s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_dash_pattern_eq_i
                                       + s V_dash_pattern_eq_set_dref_off8) (1)]
     (s V_dash_pattern_eq_z
      + max0(-s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 14 => ((1 # 1) + s V_dash_pattern_eq_z
            + max0(-1 - s V_dash_pattern_eq_i
                   + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 15 => ((1 # 1) + s V_dash_pattern_eq_z
            + max0(-1 - s V_dash_pattern_eq_i
                   + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 16 => ((1 # 1) + s V_dash_pattern_eq_z
            + max0(-1 - s V_dash_pattern_eq_i
                   + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 17 => ((1 # 1) + s V_dash_pattern_eq_z
            + max0(-s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 18 => ((1 # 1) + s V_dash_pattern_eq_z
            + max0(-s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 19 => ((1 # 1) + s V_dash_pattern_eq_z
            + max0(-s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 20 => (s V_dash_pattern_eq_z
            + max0(-s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 21 => ((1 # 1) + s V_dash_pattern_eq_z
            + max0(-1 - s V_dash_pattern_eq_i
                   + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 22 => ((1 # 1) + s V_dash_pattern_eq_z
            + max0(-1 - s V_dash_pattern_eq_i
                   + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 - s V_dash_pattern_eq_i
                            + s V_dash_pattern_eq_set_dref_off8)]
     ((1 # 1) + s V_dash_pattern_eq_z
      + max0(-1 - s V_dash_pattern_eq_i + s V_dash_pattern_eq_set_dref_off8) <= z)%Q
   | 24 => (s V_dash_pattern_eq_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_dash_pattern_eq =>
    [mkPA Q (fun n z s => ai_dash_pattern_eq n s /\ annot0_dash_pattern_eq n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_dash_pattern_eq (proc_start P_dash_pattern_eq) s1 (proc_end P_dash_pattern_eq) s2 ->
    (s2 V_dash_pattern_eq_z <= max0(s1 V_dash_pattern_eq_set_dref_off8))%Q.
Proof.
  prove_bound ipa admissible_ipa P_dash_pattern_eq.
Qed.
