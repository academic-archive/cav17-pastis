Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ReadColorMap.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ReadColorMap_z := 1%positive.
Notation V_ReadColorMap__tmp := 2%positive.
Notation V_ReadColorMap_i := 3%positive.
Notation V_ReadColorMap_cmap := 4%positive.
Notation V_ReadColorMap_cmaplen := 5%positive.
Notation V_ReadColorMap_sinfo := 6%positive.
Definition Pedges_ReadColorMap: list (edge proc) :=
  (EA 1 (AAssign V_ReadColorMap_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_ReadColorMap__tmp (Some (EVar V_ReadColorMap_cmaplen))) 3)::
  (EA 3 (AAssign V_ReadColorMap_i (Some (ENum (0)))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard (fun s => ((eval (EVar V_ReadColorMap_i)
  s) < (eval (EVar V_ReadColorMap__tmp) s))%Z)) 9)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_ReadColorMap_i) s) >=
  (eval (EVar V_ReadColorMap__tmp) s))%Z)) 7)::(EA 7 AWeaken 8)::
  (EA 9 AWeaken 10)::(EA 10 ANone 11)::(EA 11 (AAssign V_ReadColorMap_i
  (Some (EAdd (EVar V_ReadColorMap_i) (ENum (1))))) 12)::(EA 12 ANone 13)::
  (EA 13 ANone 14)::(EA 14 (AAssign V_ReadColorMap_z (Some (EAdd (ENum (1))
  (EVar V_ReadColorMap_z)))) 15)::(EA 15 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ReadColorMap => Pedges_ReadColorMap
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ReadColorMap => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_ReadColorMap (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ReadColorMap_z <= 0 /\ -1 * s V_ReadColorMap_z <= 0)%Z
   | 3 => (-1 * s V_ReadColorMap_z <= 0 /\ 1 * s V_ReadColorMap_z <= 0)%Z
   | 4 => (1 * s V_ReadColorMap_z <= 0 /\ -1 * s V_ReadColorMap_z <= 0 /\ 1 * s V_ReadColorMap_i <= 0 /\ -1 * s V_ReadColorMap_i <= 0)%Z
   | 5 => (-1 * s V_ReadColorMap_i <= 0 /\ 1 * s V_ReadColorMap_i <= 0 /\ -1 * s V_ReadColorMap_z <= 0 /\ 1 * s V_ReadColorMap_z <= 0)%Z
   | 6 => (-1 * s V_ReadColorMap_z <= 0 /\ -1 * s V_ReadColorMap_i <= 0)%Z
   | 7 => (-1 * s V_ReadColorMap_i <= 0 /\ -1 * s V_ReadColorMap_z <= 0 /\ 1 * s V_ReadColorMap__tmp+ -1 * s V_ReadColorMap_i <= 0)%Z
   | 8 => (1 * s V_ReadColorMap__tmp+ -1 * s V_ReadColorMap_i <= 0 /\ -1 * s V_ReadColorMap_z <= 0 /\ -1 * s V_ReadColorMap_i <= 0)%Z
   | 9 => (-1 * s V_ReadColorMap_i <= 0 /\ -1 * s V_ReadColorMap_z <= 0 /\ -1 * s V_ReadColorMap__tmp+ 1 * s V_ReadColorMap_i + 1 <= 0)%Z
   | 10 => (-1 * s V_ReadColorMap__tmp+ 1 * s V_ReadColorMap_i + 1 <= 0 /\ -1 * s V_ReadColorMap_z <= 0 /\ -1 * s V_ReadColorMap_i <= 0)%Z
   | 11 => (-1 * s V_ReadColorMap_i <= 0 /\ -1 * s V_ReadColorMap_z <= 0 /\ -1 * s V_ReadColorMap__tmp+ 1 * s V_ReadColorMap_i + 1 <= 0)%Z
   | 12 => (-1 * s V_ReadColorMap_z <= 0 /\ -1 * s V_ReadColorMap_i + 1 <= 0 /\ -1 * s V_ReadColorMap__tmp+ 1 * s V_ReadColorMap_i <= 0)%Z
   | 13 => (-1 * s V_ReadColorMap__tmp+ 1 * s V_ReadColorMap_i <= 0 /\ -1 * s V_ReadColorMap_i + 1 <= 0 /\ -1 * s V_ReadColorMap_z <= 0)%Z
   | 14 => (-1 * s V_ReadColorMap_z <= 0 /\ -1 * s V_ReadColorMap_i + 1 <= 0 /\ -1 * s V_ReadColorMap__tmp+ 1 * s V_ReadColorMap_i <= 0)%Z
   | 15 => (-1 * s V_ReadColorMap__tmp+ 1 * s V_ReadColorMap_i <= 0 /\ -1 * s V_ReadColorMap_i + 1 <= 0 /\ -1 * s V_ReadColorMap_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ReadColorMap (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_ReadColorMap_cmaplen) <= z)%Q
   | 2 => (s V_ReadColorMap_z + max0(s V_ReadColorMap_cmaplen) <= z)%Q
   | 3 => (s V_ReadColorMap_z + max0(s V_ReadColorMap__tmp) <= z)%Q
   | 4 => (s V_ReadColorMap_z
           + max0(s V_ReadColorMap__tmp - s V_ReadColorMap_i) <= z)%Q
   | 5 => (s V_ReadColorMap_z
           + max0(s V_ReadColorMap__tmp - s V_ReadColorMap_i) <= z)%Q
   | 6 => (s V_ReadColorMap_z
           + max0(s V_ReadColorMap__tmp - s V_ReadColorMap_i) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_ReadColorMap__tmp
                                             - s V_ReadColorMap_i) (-1
                                                                    + 
                                                                    s V_ReadColorMap__tmp
                                                                    - 
                                                                    s V_ReadColorMap_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_ReadColorMap__tmp
                                                 - s V_ReadColorMap_i)) (F_check_ge (0) (0))]
     (s V_ReadColorMap_z + max0(s V_ReadColorMap__tmp - s V_ReadColorMap_i) <= z)%Q
   | 8 => (s V_ReadColorMap_z <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_ReadColorMap__tmp
                                       - s V_ReadColorMap_i) (1)]
     (s V_ReadColorMap_z + max0(s V_ReadColorMap__tmp - s V_ReadColorMap_i) <= z)%Q
   | 10 => ((1 # 1) + s V_ReadColorMap_z
            + max0(-1 + s V_ReadColorMap__tmp - s V_ReadColorMap_i) <= z)%Q
   | 11 => ((1 # 1) + s V_ReadColorMap_z
            + max0(-1 + s V_ReadColorMap__tmp - s V_ReadColorMap_i) <= z)%Q
   | 12 => ((1 # 1) + s V_ReadColorMap_z
            + max0(s V_ReadColorMap__tmp - s V_ReadColorMap_i) <= z)%Q
   | 13 => ((1 # 1) + s V_ReadColorMap_z
            + max0(s V_ReadColorMap__tmp - s V_ReadColorMap_i) <= z)%Q
   | 14 => ((1 # 1) + s V_ReadColorMap_z
            + max0(s V_ReadColorMap__tmp - s V_ReadColorMap_i) <= z)%Q
   | 15 => (s V_ReadColorMap_z
            + max0(s V_ReadColorMap__tmp - s V_ReadColorMap_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ReadColorMap =>
    [mkPA Q (fun n z s => ai_ReadColorMap n s /\ annot0_ReadColorMap n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ReadColorMap (proc_start P_ReadColorMap) s1 (proc_end P_ReadColorMap) s2 ->
    (s2 V_ReadColorMap_z <= max0(s1 V_ReadColorMap_cmaplen))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ReadColorMap.
Qed.
