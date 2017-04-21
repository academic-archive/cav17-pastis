Require Import pasta.Pasta.

Inductive proc: Type :=
  P_numFilename.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_numFilename_z := 1%positive.
Notation V_numFilename__tmp := 2%positive.
Notation V_numFilename__tmp1 := 3%positive.
Notation V_numFilename_len := 4%positive.
Notation V_numFilename_offset := 5%positive.
Notation V_numFilename_fname := 6%positive.
Notation V_numFilename_num := 7%positive.
Notation V_numFilename_ofnum := 8%positive.
Definition Pedges_numFilename: list (edge proc) :=
  (EA 1 (AAssign V_numFilename_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_numFilename__tmp1 (Some (EVar V_numFilename_num))) 3)::(EA 3 (AAssign
  V_numFilename__tmp (Some (EVar V_numFilename_ofnum))) 4)::(EA 4 (AAssign
  V_numFilename_offset (Some (ENum (1)))) 5)::(EA 5 (AAssign
  V_numFilename_len None) 6)::(EA 6 ANone 7)::(EA 7 (AAssign
  V_numFilename__tmp1 None) 8)::(EA 8 (AAssign V_numFilename__tmp None) 9)::
  (EA 9 (AAssign V_numFilename_offset (Some (EAdd (EVar V_numFilename_offset)
  (ENum (1))))) 10)::(EA 10 ANone 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_numFilename__tmp) s) >= (eval (ENum (1))
  s))%Z)) 14)::(EA 12 (AGuard (fun s => ((eval (EVar V_numFilename__tmp) s) <
  (eval (ENum (1)) s))%Z)) 13)::(EA 13 AWeaken 19)::(EA 14 AWeaken 15)::
  (EA 15 ANone 16)::(EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_numFilename_offset) s) < (eval (ENum (4))
  s))%Z)) 20)::(EA 17 (AGuard (fun s => ((eval (EVar V_numFilename_offset)
  s) >= (eval (ENum (4)) s))%Z)) 18)::(EA 18 AWeaken 19)::
  (EA 20 AWeaken 21)::(EA 21 ANone 22)::(EA 22 (AAssign V_numFilename_z
  (Some (EAdd (ENum (1)) (EVar V_numFilename_z)))) 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_numFilename => Pedges_numFilename
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_numFilename => 19
     end)%positive;
  var_global := var_global
}.

Definition ai_numFilename (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename_z <= 0)%Z
   | 3 => (-1 * s V_numFilename_z <= 0 /\ 1 * s V_numFilename_z <= 0)%Z
   | 4 => (1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename_z <= 0)%Z
   | 5 => (-1 * s V_numFilename_z <= 0 /\ 1 * s V_numFilename_z <= 0 /\ 1 * s V_numFilename_offset + -1 <= 0 /\ -1 * s V_numFilename_offset + 1 <= 0)%Z
   | 6 => (-1 * s V_numFilename_offset + 1 <= 0 /\ 1 * s V_numFilename_offset + -1 <= 0 /\ 1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename_z <= 0)%Z
   | 7 => (-1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename_offset + 1 <= 0 /\ 1 * s V_numFilename_offset + -3 <= 0)%Z
   | 8 => (1 * s V_numFilename_offset + -3 <= 0 /\ -1 * s V_numFilename_offset + 1 <= 0 /\ -1 * s V_numFilename_z <= 0)%Z
   | 9 => (-1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename_offset + 1 <= 0 /\ 1 * s V_numFilename_offset + -3 <= 0)%Z
   | 10 => (-1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename_offset + 2 <= 0 /\ 1 * s V_numFilename_offset + -4 <= 0)%Z
   | 11 => (1 * s V_numFilename_offset + -4 <= 0 /\ -1 * s V_numFilename_offset + 2 <= 0 /\ -1 * s V_numFilename_z <= 0)%Z
   | 12 => (-1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename_offset + 2 <= 0 /\ 1 * s V_numFilename_offset + -4 <= 0)%Z
   | 13 => (1 * s V_numFilename_offset + -4 <= 0 /\ -1 * s V_numFilename_offset + 2 <= 0 /\ -1 * s V_numFilename_z <= 0 /\ 1 * s V_numFilename__tmp <= 0)%Z
   | 14 => (1 * s V_numFilename_offset + -4 <= 0 /\ -1 * s V_numFilename_offset + 2 <= 0 /\ -1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename__tmp + 1 <= 0)%Z
   | 15 => (-1 * s V_numFilename__tmp + 1 <= 0 /\ -1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename_offset + 2 <= 0 /\ 1 * s V_numFilename_offset + -4 <= 0)%Z
   | 16 => (1 * s V_numFilename_offset + -4 <= 0 /\ -1 * s V_numFilename_offset + 2 <= 0 /\ -1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename__tmp + 1 <= 0)%Z
   | 17 => (-1 * s V_numFilename__tmp + 1 <= 0 /\ -1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename_offset + 2 <= 0 /\ 1 * s V_numFilename_offset + -4 <= 0)%Z
   | 18 => (1 * s V_numFilename_offset + -4 <= 0 /\ -1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename__tmp + 1 <= 0 /\ -1 * s V_numFilename_offset + 4 <= 0)%Z
   | 19 => (-1 * s V_numFilename_offset + 2 <= 0 /\ -1 * s V_numFilename_z <= 0 /\ 1 * s V_numFilename_offset + -4 <= 0)%Z
   | 20 => (-1 * s V_numFilename_offset + 2 <= 0 /\ -1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename__tmp + 1 <= 0 /\ 1 * s V_numFilename_offset + -3 <= 0)%Z
   | 21 => (1 * s V_numFilename_offset + -3 <= 0 /\ -1 * s V_numFilename__tmp + 1 <= 0 /\ -1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename_offset + 2 <= 0)%Z
   | 22 => (-1 * s V_numFilename_offset + 2 <= 0 /\ -1 * s V_numFilename_z <= 0 /\ -1 * s V_numFilename__tmp + 1 <= 0 /\ 1 * s V_numFilename_offset + -3 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_numFilename (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((2 # 1) <= z)%Q
   | 2 => ((2 # 1) + s V_numFilename_z <= z)%Q
   | 3 => ((2 # 1) + s V_numFilename_z <= z)%Q
   | 4 => ((2 # 1) + s V_numFilename_z <= z)%Q
   | 5 => (s V_numFilename_z + max0(3 - s V_numFilename_offset) <= z)%Q
   | 6 => (s V_numFilename_z + max0(3 - s V_numFilename_offset) <= z)%Q
   | 7 => (s V_numFilename_z + max0(3 - s V_numFilename_offset) <= z)%Q
   | 8 => (s V_numFilename_z + max0(3 - s V_numFilename_offset) <= z)%Q
   | 9 => (s V_numFilename_z + max0(3 - s V_numFilename_offset) <= z)%Q
   | 10 => (s V_numFilename_z + max0(4 - s V_numFilename_offset) <= z)%Q
   | 11 => (s V_numFilename_z + max0(4 - s V_numFilename_offset) <= z)%Q
   | 12 => (s V_numFilename_z + max0(4 - s V_numFilename_offset) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_numFilename_offset) (3
                                                                    - s V_numFilename_offset));
      (*-1 0*) F_max0_ge_0 (3 - s V_numFilename_offset)]
     (s V_numFilename_z + max0(4 - s V_numFilename_offset) <= z)%Q
   | 14 => (s V_numFilename_z + max0(4 - s V_numFilename_offset) <= z)%Q
   | 15 => (s V_numFilename_z + max0(4 - s V_numFilename_offset) <= z)%Q
   | 16 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (4 - s V_numFilename_offset)) (F_check_ge (4
                                                                    - s V_numFilename_offset) (0))]
     (s V_numFilename_z + max0(4 - s V_numFilename_offset) <= z)%Q
   | 17 => ((4 # 1) - s V_numFilename_offset + s V_numFilename_z <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (4 - s V_numFilename_offset) (3
                                                                    - s V_numFilename_offset));
      (*-1 0*) F_max0_ge_0 (3 - s V_numFilename_offset);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_numFilename_offset) (0))) (F_max0_ge_0 (4
                                                                    - s V_numFilename_offset))]
     ((4 # 1) - s V_numFilename_offset + s V_numFilename_z <= z)%Q
   | 19 => (s V_numFilename_z <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_numFilename_offset) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (4
                                                               - s V_numFilename_offset) (0))) (F_max0_ge_0 (4
                                                                    - s V_numFilename_offset))]
     ((4 # 1) - s V_numFilename_offset + s V_numFilename_z <= z)%Q
   | 21 => ((1 # 1) + s V_numFilename_z + max0(3 - s V_numFilename_offset) <= z)%Q
   | 22 => ((1 # 1) + s V_numFilename_z + max0(3 - s V_numFilename_offset) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_numFilename =>
    [mkPA Q (fun n z s => ai_numFilename n s /\ annot0_numFilename n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_numFilename (proc_start P_numFilename) s1 (proc_end P_numFilename) s2 ->
    (s2 V_numFilename_z <= (2 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_numFilename.
Qed.
