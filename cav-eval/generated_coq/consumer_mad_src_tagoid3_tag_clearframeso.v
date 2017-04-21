Require Import pasta.Pasta.

Inductive proc: Type :=
  P_id3_tag_clearframes.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_id3_tag_clearframes_z := 1%positive.
Notation V_id3_tag_clearframes_i := 2%positive.
Notation V_id3_tag_clearframes_tag_dref_off24 := 3%positive.
Notation V_id3_tag_clearframes_tag := 4%positive.
Definition Pedges_id3_tag_clearframes: list (edge proc) :=
  (EA 1 (AAssign V_id3_tag_clearframes_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_id3_tag_clearframes_tag_dref_off24) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_id3_tag_clearframes_i) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_id3_tag_clearframes_i
  (Some (ENum (0)))) 6)::(EA 6 ANone 7)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_id3_tag_clearframes_i) s) <
  (eval (EVar V_id3_tag_clearframes_tag_dref_off24) s))%Z)) 13)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_id3_tag_clearframes_i) s) >=
  (eval (EVar V_id3_tag_clearframes_tag_dref_off24) s))%Z)) 9)::
  (EA 9 AWeaken 10)::(EA 10 (AAssign V_id3_tag_clearframes_tag_dref_off24
  (Some (ENum (0)))) 11)::(EA 11 AWeaken 12)::(EA 13 AWeaken 14)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_id3_tag_clearframes_i
  (Some (EAdd (EVar V_id3_tag_clearframes_i) (ENum (1))))) 16)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_id3_tag_clearframes_z
  (Some (EAdd (ENum (1)) (EVar V_id3_tag_clearframes_z)))) 19)::
  (EA 19 AWeaken 8)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_id3_tag_clearframes => Pedges_id3_tag_clearframes
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_id3_tag_clearframes => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_id3_tag_clearframes (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0)%Z
   | 3 => (-1 * s V_id3_tag_clearframes_z <= 0 /\ 1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0)%Z
   | 4 => (-1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_i <= 0)%Z
   | 5 => (-1 * s V_id3_tag_clearframes_i <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0 /\ 1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0)%Z
   | 6 => (-1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0 /\ 1 * s V_id3_tag_clearframes_i <= 0 /\ -1 * s V_id3_tag_clearframes_i <= 0)%Z
   | 7 => (-1 * s V_id3_tag_clearframes_i <= 0 /\ 1 * s V_id3_tag_clearframes_i <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0 /\ 1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0)%Z
   | 8 => (-1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_i <= 0 /\ 1 * s V_id3_tag_clearframes_i+ -1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0)%Z
   | 9 => (1 * s V_id3_tag_clearframes_i+ -1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_clearframes_i <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_i+ 1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0)%Z
   | 10 => (-1 * s V_id3_tag_clearframes_i+ 1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_i <= 0 /\ 1 * s V_id3_tag_clearframes_i+ -1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0)%Z
   | 11 => (-1 * s V_id3_tag_clearframes_i <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0 /\ 1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0)%Z
   | 12 => (-1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0 /\ 1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_i <= 0)%Z
   | 13 => (-1 * s V_id3_tag_clearframes_i <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0 /\ 1 * s V_id3_tag_clearframes_i+ -1 * s V_id3_tag_clearframes_tag_dref_off24 + 1 <= 0)%Z
   | 14 => (1 * s V_id3_tag_clearframes_i+ -1 * s V_id3_tag_clearframes_tag_dref_off24 + 1 <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_i <= 0)%Z
   | 15 => (-1 * s V_id3_tag_clearframes_i <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0 /\ 1 * s V_id3_tag_clearframes_i+ -1 * s V_id3_tag_clearframes_tag_dref_off24 + 1 <= 0)%Z
   | 16 => (-1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_i + 1 <= 0 /\ 1 * s V_id3_tag_clearframes_i+ -1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0)%Z
   | 17 => (1 * s V_id3_tag_clearframes_i+ -1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_clearframes_i + 1 <= 0 /\ -1 * s V_id3_tag_clearframes_z <= 0)%Z
   | 18 => (-1 * s V_id3_tag_clearframes_z <= 0 /\ -1 * s V_id3_tag_clearframes_i + 1 <= 0 /\ 1 * s V_id3_tag_clearframes_i+ -1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0)%Z
   | 19 => (1 * s V_id3_tag_clearframes_i+ -1 * s V_id3_tag_clearframes_tag_dref_off24 <= 0 /\ -1 * s V_id3_tag_clearframes_i + 1 <= 0 /\ -1 * s V_id3_tag_clearframes_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_id3_tag_clearframes (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_id3_tag_clearframes_tag_dref_off24) <= z)%Q
   | 2 => (max0(s V_id3_tag_clearframes_tag_dref_off24)
           + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 3 => (max0(s V_id3_tag_clearframes_tag_dref_off24)
           + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 4 => (max0(s V_id3_tag_clearframes_tag_dref_off24)
           + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 5 => (max0(s V_id3_tag_clearframes_tag_dref_off24)
           + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 6 => (max0(-s V_id3_tag_clearframes_i
                + s V_id3_tag_clearframes_tag_dref_off24)
           + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 7 => (max0(-s V_id3_tag_clearframes_i
                + s V_id3_tag_clearframes_tag_dref_off24)
           + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 8 => (max0(-s V_id3_tag_clearframes_i
                + s V_id3_tag_clearframes_tag_dref_off24)
           + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_id3_tag_clearframes_i
                                             + s V_id3_tag_clearframes_tag_dref_off24) (-1
                                                                    - s V_id3_tag_clearframes_i
                                                                    + s V_id3_tag_clearframes_tag_dref_off24));
      (*-1 0*) F_max0_ge_0 (-1 - s V_id3_tag_clearframes_i
                            + s V_id3_tag_clearframes_tag_dref_off24)]
     (max0(-s V_id3_tag_clearframes_i
           + s V_id3_tag_clearframes_tag_dref_off24)
      + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 10 => (max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_id3_tag_clearframes_z)) (F_check_ge (s V_id3_tag_clearframes_z) (0))]
     (max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 12 => (s V_id3_tag_clearframes_z <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_id3_tag_clearframes_i
                                       + s V_id3_tag_clearframes_tag_dref_off24) (1)]
     (max0(-s V_id3_tag_clearframes_i
           + s V_id3_tag_clearframes_tag_dref_off24)
      + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 14 => ((1 # 1)
            + max0(-1 - s V_id3_tag_clearframes_i
                   + s V_id3_tag_clearframes_tag_dref_off24)
            + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 15 => ((1 # 1)
            + max0(-1 - s V_id3_tag_clearframes_i
                   + s V_id3_tag_clearframes_tag_dref_off24)
            + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 16 => ((1 # 1)
            + max0(-s V_id3_tag_clearframes_i
                   + s V_id3_tag_clearframes_tag_dref_off24)
            + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 17 => ((1 # 1)
            + max0(-s V_id3_tag_clearframes_i
                   + s V_id3_tag_clearframes_tag_dref_off24)
            + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 18 => ((1 # 1)
            + max0(-s V_id3_tag_clearframes_i
                   + s V_id3_tag_clearframes_tag_dref_off24)
            + max0(s V_id3_tag_clearframes_z) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_id3_tag_clearframes_z) (0))) (F_max0_ge_0 (s V_id3_tag_clearframes_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                  + s V_id3_tag_clearframes_z)) (F_check_ge (-1
                                                                    + s V_id3_tag_clearframes_z) (0))]
     ((1 # 1) + max0(-1 + s V_id3_tag_clearframes_z)
      + max0(-s V_id3_tag_clearframes_i
             + s V_id3_tag_clearframes_tag_dref_off24) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_id3_tag_clearframes =>
    [mkPA Q (fun n z s => ai_id3_tag_clearframes n s /\ annot0_id3_tag_clearframes n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_id3_tag_clearframes (proc_start P_id3_tag_clearframes) s1 (proc_end P_id3_tag_clearframes) s2 ->
    (s2 V_id3_tag_clearframes_z <= max0(s1 V_id3_tag_clearframes_tag_dref_off24))%Q.
Proof.
  prove_bound ipa admissible_ipa P_id3_tag_clearframes.
Qed.
