Require Import pasta.Pasta.

Inductive proc: Type :=
  P_bsW.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_bsW_z := 1%positive.
Notation V_bsW__tmp := 2%positive.
Notation V_bsW__tmp1 := 3%positive.
Notation V_bsW_s_dref_off116 := 4%positive.
Notation V_bsW_s_dref_off640 := 5%positive.
Notation V_bsW_s_dref_off644 := 6%positive.
Notation V_bsW_n := 7%positive.
Notation V_bsW_s := 8%positive.
Notation V_bsW_v := 9%positive.
Definition Pedges_bsW: list (edge proc) :=
  (EA 1 (AAssign V_bsW_z (Some (ENum (0)))) 2)::(EA 2 (AAssign V_bsW__tmp
  (Some (EVar V_bsW_n))) 3)::(EA 3 (AAssign V_bsW__tmp1
  (Some (EVar V_bsW_v))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_bsW_s_dref_off644) s) >= (eval (ENum (8))
  s))%Z)) 12)::(EA 6 (AGuard (fun s => ((eval (EVar V_bsW_s_dref_off644) s) <
  (eval (ENum (8)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 8 (AAssign
  V_bsW_s_dref_off640 None) 9)::(EA 9 (AAssign V_bsW_s_dref_off644
  (Some (EAdd (EVar V_bsW_s_dref_off644) (EVar V_bsW__tmp)))) 10)::
  (EA 10 AWeaken 11)::(EA 12 AWeaken 13)::(EA 13 (AAssign V_bsW_s_dref_off116
  (Some (EAdd (EVar V_bsW_s_dref_off116) (ENum (1))))) 14)::(EA 14 (AAssign
  V_bsW_s_dref_off640 None) 15)::(EA 15 (AAssign V_bsW_s_dref_off644
  (Some (ESub (EVar V_bsW_s_dref_off644) (ENum (8))))) 16)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_bsW_z
  (Some (EAdd (ENum (1)) (EVar V_bsW_z)))) 19)::(EA 19 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_bsW => Pedges_bsW
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_bsW => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_bsW (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_bsW_z <= 0 /\ -1 * s V_bsW_z <= 0)%Z
   | 3 => (-1 * s V_bsW_z <= 0 /\ 1 * s V_bsW_z <= 0)%Z
   | 4 => (1 * s V_bsW_z <= 0 /\ -1 * s V_bsW_z <= 0)%Z
   | 5 => (-1 * s V_bsW_z <= 0 /\ 1 * s V_bsW_z <= 0)%Z
   | 6 => (-1 * s V_bsW_z <= 0)%Z
   | 7 => (-1 * s V_bsW_z <= 0 /\ 1 * s V_bsW_s_dref_off644 + -7 <= 0)%Z
   | 8 => (1 * s V_bsW_s_dref_off644 + -7 <= 0 /\ -1 * s V_bsW_z <= 0)%Z
   | 9 => (-1 * s V_bsW_z <= 0 /\ 1 * s V_bsW_s_dref_off644 + -7 <= 0)%Z
   | 10 => (-1 * s V_bsW_z <= 0 /\ -1 * s V_bsW__tmp+ 1 * s V_bsW_s_dref_off644 + -7 <= 0)%Z
   | 11 => (-1 * s V_bsW__tmp+ 1 * s V_bsW_s_dref_off644 + -7 <= 0 /\ -1 * s V_bsW_z <= 0)%Z
   | 12 => (-1 * s V_bsW_z <= 0 /\ -1 * s V_bsW_s_dref_off644 + 8 <= 0)%Z
   | 13 => (-1 * s V_bsW_s_dref_off644 + 8 <= 0 /\ -1 * s V_bsW_z <= 0)%Z
   | 14 => (-1 * s V_bsW_z <= 0 /\ -1 * s V_bsW_s_dref_off644 + 8 <= 0)%Z
   | 15 => (-1 * s V_bsW_s_dref_off644 + 8 <= 0 /\ -1 * s V_bsW_z <= 0)%Z
   | 16 => (-1 * s V_bsW_z <= 0 /\ -1 * s V_bsW_s_dref_off644 <= 0)%Z
   | 17 => (-1 * s V_bsW_s_dref_off644 <= 0 /\ -1 * s V_bsW_z <= 0)%Z
   | 18 => (-1 * s V_bsW_z <= 0 /\ -1 * s V_bsW_s_dref_off644 <= 0)%Z
   | 19 => (-1 * s V_bsW_s_dref_off644 <= 0 /\ -1 * s V_bsW_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_bsW (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 8) * max0(s V_bsW_s_dref_off644) <= z)%Q
   | 2 => ((1 # 8) * max0(s V_bsW_s_dref_off644) + max0(s V_bsW_z) <= z)%Q
   | 3 => ((1 # 8) * max0(s V_bsW_s_dref_off644) + max0(s V_bsW_z) <= z)%Q
   | 4 => ((1 # 8) * max0(s V_bsW_s_dref_off644) + max0(s V_bsW_z) <= z)%Q
   | 5 => ((1 # 8) * max0(s V_bsW_s_dref_off644) + max0(s V_bsW_z) <= z)%Q
   | 6 => ((1 # 8) * max0(s V_bsW_s_dref_off644) + max0(s V_bsW_z) <= z)%Q
   | 7 => hints
     [(*-0.125 0*) F_max0_monotonic (F_check_ge (s V_bsW_s_dref_off644) (-8
                                                                    + s V_bsW_s_dref_off644));
      (*-0.125 0*) F_max0_ge_0 (-8 + s V_bsW_s_dref_off644)]
     ((1 # 8) * max0(s V_bsW_s_dref_off644) + max0(s V_bsW_z) <= z)%Q
   | 8 => (max0(s V_bsW_z) <= z)%Q
   | 9 => (max0(s V_bsW_z) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_bsW_z)) (F_check_ge (s V_bsW_z) (0))]
     (max0(s V_bsW_z) <= z)%Q
   | 11 => (s V_bsW_z <= z)%Q
   | 12 => hints
     [(*-0.125 0*) F_max0_pre_decrement 1 (s V_bsW_s_dref_off644) (8)]
     ((1 # 8) * max0(s V_bsW_s_dref_off644) + max0(s V_bsW_z) <= z)%Q
   | 13 => ((1 # 1) + (1 # 8) * max0(-8 + s V_bsW_s_dref_off644)
            + max0(s V_bsW_z) <= z)%Q
   | 14 => ((1 # 1) + (1 # 8) * max0(-8 + s V_bsW_s_dref_off644)
            + max0(s V_bsW_z) <= z)%Q
   | 15 => ((1 # 1) + (1 # 8) * max0(-8 + s V_bsW_s_dref_off644)
            + max0(s V_bsW_z) <= z)%Q
   | 16 => ((1 # 1) + (1 # 8) * max0(s V_bsW_s_dref_off644) + max0(s V_bsW_z) <= z)%Q
   | 17 => ((1 # 1) + (1 # 8) * max0(s V_bsW_s_dref_off644) + max0(s V_bsW_z) <= z)%Q
   | 18 => ((1 # 1) + (1 # 8) * max0(s V_bsW_s_dref_off644) + max0(s V_bsW_z) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_bsW_z) (0))) (F_max0_ge_0 (s V_bsW_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_bsW_z)) (F_check_ge (-1
                                                                    + s V_bsW_z) (0))]
     ((1 # 1) + max0(-1 + s V_bsW_z) + (1 # 8) * max0(s V_bsW_s_dref_off644) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_bsW =>
    [mkPA Q (fun n z s => ai_bsW n s /\ annot0_bsW n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_bsW (proc_start P_bsW) s1 (proc_end P_bsW) s2 ->
    (s2 V_bsW_z <= (1 # 8) * max0(s1 V_bsW_s_dref_off644))%Q.
Proof.
  prove_bound ipa admissible_ipa P_bsW.
Qed.
