Require Import pasta.Pasta.

Inductive proc: Type :=
  P_III_freqinver.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_III_freqinver_z := 1%positive.
Notation V_III_freqinver__tmp := 2%positive.
Notation V_III_freqinver_i := 3%positive.
Notation V_III_freqinver_tmp1 := 4%positive.
Notation V_III_freqinver_tmp2 := 5%positive.
Notation V_III_freqinver_sample := 6%positive.
Notation V_III_freqinver_sb := 7%positive.
Definition Pedges_III_freqinver: list (edge proc) :=
  (EA 1 (AAssign V_III_freqinver_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_III_freqinver_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_III_freqinver__tmp
  (Some (EVar V_III_freqinver_sb))) 5)::(EA 5 (AAssign V_III_freqinver_tmp1
  None) 6)::(EA 6 (AAssign V_III_freqinver_tmp2 None) 7)::(EA 7 (AAssign
  V_III_freqinver_i (Some (ENum (1)))) 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 (AGuard (fun s => ((eval (EVar V_III_freqinver_i)
  s) < (eval (ENum (13)) s))%Z)) 15)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_III_freqinver_i) s) >= (eval (ENum (13))
  s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 (AAssign V_III_freqinver_tmp1
  None) 13)::(EA 13 AWeaken 14)::(EA 15 AWeaken 16)::(EA 16 (AAssign
  V_III_freqinver_tmp1 None) 17)::(EA 17 (AAssign V_III_freqinver_tmp2
  None) 18)::(EA 18 ANone 19)::(EA 19 (AAssign V_III_freqinver_i
  (Some (EAdd (EVar V_III_freqinver_i) (ENum (4))))) 20)::(EA 20 ANone 21)::
  (EA 21 ANone 22)::(EA 22 (AAssign V_III_freqinver_z (Some (EAdd (ENum (1))
  (EVar V_III_freqinver_z)))) 23)::(EA 23 AWeaken 10)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_III_freqinver => Pedges_III_freqinver
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_III_freqinver => 14
     end)%positive;
  var_global := var_global
}.

Definition ai_III_freqinver (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_z <= 0)%Z
   | 3 => (-1 * s V_III_freqinver_z <= 0 /\ 1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_i <= 0)%Z
   | 4 => (-1 * s V_III_freqinver_i <= 0 /\ 1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_z <= 0)%Z
   | 5 => (-1 * s V_III_freqinver_z <= 0 /\ 1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_i <= 0)%Z
   | 6 => (-1 * s V_III_freqinver_i <= 0 /\ 1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_z <= 0)%Z
   | 7 => (-1 * s V_III_freqinver_z <= 0 /\ 1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_i <= 0)%Z
   | 8 => (1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_z <= 0 /\ 1 * s V_III_freqinver_i + -1 <= 0 /\ -1 * s V_III_freqinver_i + 1 <= 0)%Z
   | 9 => (-1 * s V_III_freqinver_i + 1 <= 0 /\ 1 * s V_III_freqinver_i + -1 <= 0 /\ -1 * s V_III_freqinver_z <= 0 /\ 1 * s V_III_freqinver_z <= 0)%Z
   | 10 => (-1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_i + 1 <= 0 /\ 1 * s V_III_freqinver_i + -16 <= 0)%Z
   | 11 => (1 * s V_III_freqinver_i + -16 <= 0 /\ -1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_i + 13 <= 0)%Z
   | 12 => (-1 * s V_III_freqinver_i + 13 <= 0 /\ -1 * s V_III_freqinver_z <= 0 /\ 1 * s V_III_freqinver_i + -16 <= 0)%Z
   | 13 => (1 * s V_III_freqinver_i + -16 <= 0 /\ -1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_i + 13 <= 0)%Z
   | 14 => (-1 * s V_III_freqinver_i + 13 <= 0 /\ -1 * s V_III_freqinver_z <= 0 /\ 1 * s V_III_freqinver_i + -16 <= 0)%Z
   | 15 => (-1 * s V_III_freqinver_i + 1 <= 0 /\ -1 * s V_III_freqinver_z <= 0 /\ 1 * s V_III_freqinver_i + -12 <= 0)%Z
   | 16 => (1 * s V_III_freqinver_i + -12 <= 0 /\ -1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_i + 1 <= 0)%Z
   | 17 => (-1 * s V_III_freqinver_i + 1 <= 0 /\ -1 * s V_III_freqinver_z <= 0 /\ 1 * s V_III_freqinver_i + -12 <= 0)%Z
   | 18 => (1 * s V_III_freqinver_i + -12 <= 0 /\ -1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_i + 1 <= 0)%Z
   | 19 => (-1 * s V_III_freqinver_i + 1 <= 0 /\ -1 * s V_III_freqinver_z <= 0 /\ 1 * s V_III_freqinver_i + -12 <= 0)%Z
   | 20 => (-1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_i + 5 <= 0 /\ 1 * s V_III_freqinver_i + -16 <= 0)%Z
   | 21 => (1 * s V_III_freqinver_i + -16 <= 0 /\ -1 * s V_III_freqinver_i + 5 <= 0 /\ -1 * s V_III_freqinver_z <= 0)%Z
   | 22 => (-1 * s V_III_freqinver_z <= 0 /\ -1 * s V_III_freqinver_i + 5 <= 0 /\ 1 * s V_III_freqinver_i + -16 <= 0)%Z
   | 23 => (1 * s V_III_freqinver_i + -16 <= 0 /\ -1 * s V_III_freqinver_i + 5 <= 0 /\ -1 * s V_III_freqinver_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_III_freqinver (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((15 # 4) <= z)%Q
   | 2 => ((15 # 4) + s V_III_freqinver_z <= z)%Q
   | 3 => ((15 # 4) + s V_III_freqinver_z <= z)%Q
   | 4 => ((15 # 4) + s V_III_freqinver_z <= z)%Q
   | 5 => ((15 # 4) + s V_III_freqinver_z <= z)%Q
   | 6 => ((15 # 4) + s V_III_freqinver_z <= z)%Q
   | 7 => ((15 # 4) + s V_III_freqinver_z <= z)%Q
   | 8 => ((4 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 9 => ((4 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 10 => ((4 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 11 => ((4 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 12 => ((4 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 13 => hints
     [(*-0.25 0*) F_max0_monotonic (F_check_ge (16 - s V_III_freqinver_i) (12
                                                                    - s V_III_freqinver_i));
      (*-0.25 0*) F_max0_ge_0 (12 - s V_III_freqinver_i);
      (*0 0.25*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (16
                                                                 - s V_III_freqinver_i) (0))) (F_max0_ge_0 (16
                                                                    - s V_III_freqinver_i))]
     ((4 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 14 => (s V_III_freqinver_z <= z)%Q
   | 15 => ((4 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 16 => ((4 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 17 => ((4 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 18 => ((4 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 19 => ((4 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 20 => ((5 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 21 => ((5 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 22 => ((5 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | 23 => ((4 # 1) - (1 # 4) * s V_III_freqinver_i + s V_III_freqinver_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_III_freqinver =>
    [mkPA Q (fun n z s => ai_III_freqinver n s /\ annot0_III_freqinver n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_III_freqinver (proc_start P_III_freqinver) s1 (proc_end P_III_freqinver) s2 ->
    (s2 V_III_freqinver_z <= (15 # 4))%Q.
Proof.
  prove_bound ipa admissible_ipa P_III_freqinver.
Qed.
