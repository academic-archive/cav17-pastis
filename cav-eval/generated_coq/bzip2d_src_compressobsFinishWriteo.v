Require Import pasta.Pasta.

Inductive proc: Type :=
  P_bsFinishWrite.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_bsFinishWrite_z := 1%positive.
Notation V_bsFinishWrite_s_dref_off116 := 2%positive.
Notation V_bsFinishWrite_s_dref_off640 := 3%positive.
Notation V_bsFinishWrite_s_dref_off644 := 4%positive.
Notation V_bsFinishWrite_s := 5%positive.
Definition Pedges_bsFinishWrite: list (edge proc) :=
  (EA 1 (AAssign V_bsFinishWrite_z (Some (ENum (0)))) 2)::(EA 2 ANone 3)::
  (EA 3 AWeaken 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_bsFinishWrite_s_dref_off644) s) > (eval (ENum (0))
  s))%Z)) 7)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_bsFinishWrite_s_dref_off644) s) <=
  (eval (ENum (0)) s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 7 AWeaken 8)::
  (EA 8 (AAssign V_bsFinishWrite_s_dref_off116
  (Some (EAdd (EVar V_bsFinishWrite_s_dref_off116) (ENum (1))))) 9)::
  (EA 9 (AAssign V_bsFinishWrite_s_dref_off640 None) 10)::(EA 10 (AAssign
  V_bsFinishWrite_s_dref_off644
  (Some (ESub (EVar V_bsFinishWrite_s_dref_off644) (ENum (8))))) 11)::
  (EA 11 ANone 12)::(EA 12 ANone 13)::(EA 13 (AAssign V_bsFinishWrite_z
  (Some (EAdd (ENum (1)) (EVar V_bsFinishWrite_z)))) 14)::(EA 14 AWeaken 4)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_bsFinishWrite => Pedges_bsFinishWrite
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_bsFinishWrite => 6
     end)%positive;
  var_global := var_global
}.

Definition ai_bsFinishWrite (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_bsFinishWrite_z <= 0 /\ -1 * s V_bsFinishWrite_z <= 0)%Z
   | 3 => (-1 * s V_bsFinishWrite_z <= 0 /\ 1 * s V_bsFinishWrite_z <= 0)%Z
   | 4 => (-1 * s V_bsFinishWrite_z <= 0)%Z
   | 5 => (-1 * s V_bsFinishWrite_z <= 0 /\ 1 * s V_bsFinishWrite_s_dref_off644 <= 0)%Z
   | 6 => (1 * s V_bsFinishWrite_s_dref_off644 <= 0 /\ -1 * s V_bsFinishWrite_z <= 0)%Z
   | 7 => (-1 * s V_bsFinishWrite_z <= 0 /\ -1 * s V_bsFinishWrite_s_dref_off644 + 1 <= 0)%Z
   | 8 => (-1 * s V_bsFinishWrite_s_dref_off644 + 1 <= 0 /\ -1 * s V_bsFinishWrite_z <= 0)%Z
   | 9 => (-1 * s V_bsFinishWrite_z <= 0 /\ -1 * s V_bsFinishWrite_s_dref_off644 + 1 <= 0)%Z
   | 10 => (-1 * s V_bsFinishWrite_s_dref_off644 + 1 <= 0 /\ -1 * s V_bsFinishWrite_z <= 0)%Z
   | 11 => (-1 * s V_bsFinishWrite_z <= 0 /\ -1 * s V_bsFinishWrite_s_dref_off644 + -7 <= 0)%Z
   | 12 => (-1 * s V_bsFinishWrite_s_dref_off644 + -7 <= 0 /\ -1 * s V_bsFinishWrite_z <= 0)%Z
   | 13 => (-1 * s V_bsFinishWrite_z <= 0 /\ -1 * s V_bsFinishWrite_s_dref_off644 + -7 <= 0)%Z
   | 14 => (-1 * s V_bsFinishWrite_s_dref_off644 + -7 <= 0 /\ -1 * s V_bsFinishWrite_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_bsFinishWrite (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 8) * max0(7 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | 2 => (s V_bsFinishWrite_z
           + (1 # 8) * max0(7 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | 3 => (s V_bsFinishWrite_z
           + (1 # 8) * max0(7 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | 4 => (s V_bsFinishWrite_z
           + (1 # 8) * max0(7 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | 5 => hints
     [(*-0.125 0*) F_max0_monotonic (F_check_ge (7
                                                 + s V_bsFinishWrite_s_dref_off644) (-1
                                                                    + s V_bsFinishWrite_s_dref_off644));
      (*-0.125 0*) F_max0_ge_0 (-1 + s V_bsFinishWrite_s_dref_off644)]
     (s V_bsFinishWrite_z
      + (1 # 8) * max0(7 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | 6 => (s V_bsFinishWrite_z <= z)%Q
   | 7 => hints
     [(*-0.125 0*) F_max0_pre_decrement 1 (7
                                           + s V_bsFinishWrite_s_dref_off644) (8)]
     (s V_bsFinishWrite_z
      + (1 # 8) * max0(7 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | 8 => ((1 # 1) + s V_bsFinishWrite_z
           + (1 # 8) * max0(-1 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | 9 => ((1 # 1) + s V_bsFinishWrite_z
           + (1 # 8) * max0(-1 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | 10 => ((1 # 1) + s V_bsFinishWrite_z
            + (1 # 8) * max0(-1 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | 11 => ((1 # 1) + s V_bsFinishWrite_z
            + (1 # 8) * max0(7 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | 12 => ((1 # 1) + s V_bsFinishWrite_z
            + (1 # 8) * max0(7 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | 13 => ((1 # 1) + s V_bsFinishWrite_z
            + (1 # 8) * max0(7 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | 14 => (s V_bsFinishWrite_z
            + (1 # 8) * max0(7 + s V_bsFinishWrite_s_dref_off644) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_bsFinishWrite =>
    [mkPA Q (fun n z s => ai_bsFinishWrite n s /\ annot0_bsFinishWrite n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_bsFinishWrite (proc_start P_bsFinishWrite) s1 (proc_end P_bsFinishWrite) s2 ->
    (s2 V_bsFinishWrite_z <= (1 # 8) * max0(7
                                            + s1 V_bsFinishWrite_s_dref_off644))%Q.
Proof.
  prove_bound ipa admissible_ipa P_bsFinishWrite.
Qed.
