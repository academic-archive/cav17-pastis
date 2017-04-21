Require Import pasta.Pasta.

Inductive proc: Type :=
  P_BF_LoadHolderFromBitstreamPart.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_BF_LoadHolderFromBitstreamPart_z := 1%positive.
Notation V_BF_LoadHolderFromBitstreamPart_i := 2%positive.
Notation V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 := 3%positive.
Notation V_BF_LoadHolderFromBitstreamPart_theHolder := 4%positive.
Notation V_BF_LoadHolderFromBitstreamPart_thePart := 5%positive.
Definition Pedges_BF_LoadHolderFromBitstreamPart: list (edge proc) :=
  (EA 1 (AAssign V_BF_LoadHolderFromBitstreamPart_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard
  (fun s => ((eval (EVar V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0)
  s) >= (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_BF_LoadHolderFromBitstreamPart_i) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_BF_LoadHolderFromBitstreamPart_i (Some (ENum (0)))) 6)::(EA 6 ANone 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_BF_LoadHolderFromBitstreamPart_i) s) <
  (eval (EVar V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0)
  s))%Z)) 11)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_BF_LoadHolderFromBitstreamPart_i) s) >=
  (eval (EVar V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0)
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 11 AWeaken 12)::(EA 12 ANone 13)::
  (EA 13 (AAssign V_BF_LoadHolderFromBitstreamPart_i
  (Some (EAdd (EVar V_BF_LoadHolderFromBitstreamPart_i) (ENum (1))))) 14)::
  (EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_BF_LoadHolderFromBitstreamPart_z (Some (EAdd (ENum (1))
  (EVar V_BF_LoadHolderFromBitstreamPart_z)))) 17)::(EA 17 AWeaken 8)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_BF_LoadHolderFromBitstreamPart => Pedges_BF_LoadHolderFromBitstreamPart
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_BF_LoadHolderFromBitstreamPart => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_BF_LoadHolderFromBitstreamPart (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0)%Z
   | 3 => (-1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0)%Z
   | 4 => (-1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_i <= 0)%Z
   | 5 => (-1 * s V_BF_LoadHolderFromBitstreamPart_i <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0)%Z
   | 6 => (-1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_i <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_i <= 0)%Z
   | 7 => (-1 * s V_BF_LoadHolderFromBitstreamPart_i <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_i <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0)%Z
   | 8 => (-1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_i <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_i+ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0)%Z
   | 9 => (1 * s V_BF_LoadHolderFromBitstreamPart_i+ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_i <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_i+ 1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0)%Z
   | 10 => (-1 * s V_BF_LoadHolderFromBitstreamPart_i+ 1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_i <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_i+ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0)%Z
   | 11 => (-1 * s V_BF_LoadHolderFromBitstreamPart_i <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_i+ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 + 1 <= 0)%Z
   | 12 => (1 * s V_BF_LoadHolderFromBitstreamPart_i+ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 + 1 <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_i <= 0)%Z
   | 13 => (-1 * s V_BF_LoadHolderFromBitstreamPart_i <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_i+ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 + 1 <= 0)%Z
   | 14 => (-1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_i + 1 <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_i+ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0)%Z
   | 15 => (1 * s V_BF_LoadHolderFromBitstreamPart_i+ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_i + 1 <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0)%Z
   | 16 => (-1 * s V_BF_LoadHolderFromBitstreamPart_z <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_i + 1 <= 0 /\ 1 * s V_BF_LoadHolderFromBitstreamPart_i+ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0)%Z
   | 17 => (1 * s V_BF_LoadHolderFromBitstreamPart_i+ -1 * s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0 <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_i + 1 <= 0 /\ -1 * s V_BF_LoadHolderFromBitstreamPart_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_BF_LoadHolderFromBitstreamPart (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 2 => (s V_BF_LoadHolderFromBitstreamPart_z
           + max0(s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 3 => (s V_BF_LoadHolderFromBitstreamPart_z
           + max0(s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 4 => (s V_BF_LoadHolderFromBitstreamPart_z
           + max0(s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 5 => (s V_BF_LoadHolderFromBitstreamPart_z
           + max0(s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 6 => (s V_BF_LoadHolderFromBitstreamPart_z
           + max0(-s V_BF_LoadHolderFromBitstreamPart_i
                  + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 7 => (s V_BF_LoadHolderFromBitstreamPart_z
           + max0(-s V_BF_LoadHolderFromBitstreamPart_i
                  + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 8 => (s V_BF_LoadHolderFromBitstreamPart_z
           + max0(-s V_BF_LoadHolderFromBitstreamPart_i
                  + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_BF_LoadHolderFromBitstreamPart_i
                                             + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) (-1
                                                                    - s V_BF_LoadHolderFromBitstreamPart_i
                                                                    + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0));
      (*-1 0*) F_max0_ge_0 (-1 - s V_BF_LoadHolderFromBitstreamPart_i
                            + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0)]
     (s V_BF_LoadHolderFromBitstreamPart_z
      + max0(-s V_BF_LoadHolderFromBitstreamPart_i
             + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 10 => (s V_BF_LoadHolderFromBitstreamPart_z <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_BF_LoadHolderFromBitstreamPart_i
                                       + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) (1)]
     (s V_BF_LoadHolderFromBitstreamPart_z
      + max0(-s V_BF_LoadHolderFromBitstreamPart_i
             + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 12 => ((1 # 1) + s V_BF_LoadHolderFromBitstreamPart_z
            + max0(-1 - s V_BF_LoadHolderFromBitstreamPart_i
                   + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 13 => ((1 # 1) + s V_BF_LoadHolderFromBitstreamPart_z
            + max0(-1 - s V_BF_LoadHolderFromBitstreamPart_i
                   + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 14 => ((1 # 1) + s V_BF_LoadHolderFromBitstreamPart_z
            + max0(-s V_BF_LoadHolderFromBitstreamPart_i
                   + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 15 => ((1 # 1) + s V_BF_LoadHolderFromBitstreamPart_z
            + max0(-s V_BF_LoadHolderFromBitstreamPart_i
                   + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 16 => ((1 # 1) + s V_BF_LoadHolderFromBitstreamPart_z
            + max0(-s V_BF_LoadHolderFromBitstreamPart_i
                   + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | 17 => (s V_BF_LoadHolderFromBitstreamPart_z
            + max0(-s V_BF_LoadHolderFromBitstreamPart_i
                   + s V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_BF_LoadHolderFromBitstreamPart =>
    [mkPA Q (fun n z s => ai_BF_LoadHolderFromBitstreamPart n s /\ annot0_BF_LoadHolderFromBitstreamPart n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_BF_LoadHolderFromBitstreamPart (proc_start P_BF_LoadHolderFromBitstreamPart) s1 (proc_end P_BF_LoadHolderFromBitstreamPart) s2 ->
    (s2 V_BF_LoadHolderFromBitstreamPart_z <= max0(s1 V_BF_LoadHolderFromBitstreamPart_thePart_dref_off0))%Q.
Proof.
  prove_bound ipa admissible_ipa P_BF_LoadHolderFromBitstreamPart.
Qed.
