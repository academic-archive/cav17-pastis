Require Import pasta.Pasta.

Inductive proc: Type :=
  P_prepare_new_block.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_prepare_new_block_z := 1%positive.
Notation V_prepare_new_block_i := 2%positive.
Notation V_prepare_new_block_s_dref_off108 := 3%positive.
Notation V_prepare_new_block_s_dref_off116 := 4%positive.
Notation V_prepare_new_block_s_dref_off120 := 5%positive.
Notation V_prepare_new_block_s_dref_off648 := 6%positive.
Notation V_prepare_new_block_s_dref_off660 := 7%positive.
Notation V_prepare_new_block_s := 8%positive.
Definition Pedges_prepare_new_block: list (edge proc) :=
  (EA 1 (AAssign V_prepare_new_block_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_prepare_new_block_s_dref_off108 (Some (ENum (0)))) 3)::(EA 3 (AAssign
  V_prepare_new_block_s_dref_off116 (Some (ENum (0)))) 4)::(EA 4 (AAssign
  V_prepare_new_block_s_dref_off120 (Some (ENum (0)))) 5)::(EA 5 (AAssign
  V_prepare_new_block_s_dref_off648 (Some (ENum (-1)))) 6)::(EA 6 (AAssign
  V_prepare_new_block_i (Some (ENum (0)))) 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_prepare_new_block_i) s) < (eval (ENum (256))
  s))%Z)) 14)::(EA 9 (AGuard (fun s => ((eval (EVar V_prepare_new_block_i)
  s) >= (eval (ENum (256)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 (AAssign
  V_prepare_new_block_s_dref_off660
  (Some (EAdd (EVar V_prepare_new_block_s_dref_off660) (ENum (1))))) 12)::
  (EA 12 AWeaken 13)::(EA 14 AWeaken 15)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_prepare_new_block_i (Some (EAdd (EVar V_prepare_new_block_i)
  (ENum (1))))) 17)::(EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_prepare_new_block_z (Some (EAdd (ENum (1))
  (EVar V_prepare_new_block_z)))) 20)::(EA 20 AWeaken 9)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_prepare_new_block => Pedges_prepare_new_block
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_prepare_new_block => 13
     end)%positive;
  var_global := var_global
}.

Definition ai_prepare_new_block (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_prepare_new_block_z <= 0 /\ -1 * s V_prepare_new_block_z <= 0)%Z
   | 3 => (-1 * s V_prepare_new_block_z <= 0 /\ 1 * s V_prepare_new_block_z <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0)%Z
   | 4 => (-1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_z <= 0 /\ -1 * s V_prepare_new_block_z <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0)%Z
   | 5 => (-1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ -1 * s V_prepare_new_block_z <= 0 /\ 1 * s V_prepare_new_block_z <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0)%Z
   | 6 => (-1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_z <= 0 /\ -1 * s V_prepare_new_block_z <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0)%Z
   | 7 => (-1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ -1 * s V_prepare_new_block_z <= 0 /\ 1 * s V_prepare_new_block_z <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ 1 * s V_prepare_new_block_i <= 0 /\ -1 * s V_prepare_new_block_i <= 0)%Z
   | 8 => (-1 * s V_prepare_new_block_i <= 0 /\ 1 * s V_prepare_new_block_i <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_z <= 0 /\ -1 * s V_prepare_new_block_z <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0)%Z
   | 9 => (-1 * s V_prepare_new_block_z <= 0 /\ -1 * s V_prepare_new_block_i <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ 1 * s V_prepare_new_block_i + -256 <= 0)%Z
   | 10 => (1 * s V_prepare_new_block_i + -256 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ -1 * s V_prepare_new_block_z <= 0 /\ -1 * s V_prepare_new_block_i + 256 <= 0)%Z
   | 11 => (-1 * s V_prepare_new_block_i + 256 <= 0 /\ -1 * s V_prepare_new_block_z <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ 1 * s V_prepare_new_block_i + -256 <= 0)%Z
   | 12 => (1 * s V_prepare_new_block_i + -256 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ -1 * s V_prepare_new_block_z <= 0 /\ -1 * s V_prepare_new_block_i + 256 <= 0)%Z
   | 13 => (-1 * s V_prepare_new_block_i + 256 <= 0 /\ -1 * s V_prepare_new_block_z <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ 1 * s V_prepare_new_block_i + -256 <= 0)%Z
   | 14 => (-1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ -1 * s V_prepare_new_block_i <= 0 /\ -1 * s V_prepare_new_block_z <= 0 /\ 1 * s V_prepare_new_block_i + -255 <= 0)%Z
   | 15 => (1 * s V_prepare_new_block_i + -255 <= 0 /\ -1 * s V_prepare_new_block_z <= 0 /\ -1 * s V_prepare_new_block_i <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0)%Z
   | 16 => (-1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ -1 * s V_prepare_new_block_i <= 0 /\ -1 * s V_prepare_new_block_z <= 0 /\ 1 * s V_prepare_new_block_i + -255 <= 0)%Z
   | 17 => (-1 * s V_prepare_new_block_z <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_i + 1 <= 0 /\ 1 * s V_prepare_new_block_i + -256 <= 0)%Z
   | 18 => (1 * s V_prepare_new_block_i + -256 <= 0 /\ -1 * s V_prepare_new_block_i + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ -1 * s V_prepare_new_block_z <= 0)%Z
   | 19 => (-1 * s V_prepare_new_block_z <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_i + 1 <= 0 /\ 1 * s V_prepare_new_block_i + -256 <= 0)%Z
   | 20 => (1 * s V_prepare_new_block_i + -256 <= 0 /\ -1 * s V_prepare_new_block_i + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off120 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off108 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off116 <= 0 /\ 1 * s V_prepare_new_block_s_dref_off648 + 1 <= 0 /\ -1 * s V_prepare_new_block_s_dref_off648 + -1 <= 0 /\ -1 * s V_prepare_new_block_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_prepare_new_block (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((256 # 1) <= z)%Q
   | 2 => ((256 # 1) + s V_prepare_new_block_z <= z)%Q
   | 3 => ((256 # 1) + s V_prepare_new_block_z <= z)%Q
   | 4 => ((256 # 1) + s V_prepare_new_block_z <= z)%Q
   | 5 => ((256 # 1) + s V_prepare_new_block_z <= z)%Q
   | 6 => ((256 # 1) + s V_prepare_new_block_z <= z)%Q
   | 7 => (s V_prepare_new_block_z + max0(256 - s V_prepare_new_block_i) <= z)%Q
   | 8 => (s V_prepare_new_block_z + max0(256 - s V_prepare_new_block_i) <= z)%Q
   | 9 => (s V_prepare_new_block_z + max0(256 - s V_prepare_new_block_i) <= z)%Q
   | 10 => (s V_prepare_new_block_z + max0(256 - s V_prepare_new_block_i) <= z)%Q
   | 11 => (s V_prepare_new_block_z + max0(256 - s V_prepare_new_block_i) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (256 - s V_prepare_new_block_i) (255
                                                                    - s V_prepare_new_block_i));
      (*-1 0*) F_max0_ge_0 (255 - s V_prepare_new_block_i)]
     (s V_prepare_new_block_z + max0(256 - s V_prepare_new_block_i) <= z)%Q
   | 13 => (s V_prepare_new_block_z <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (256 - s V_prepare_new_block_i) (1)]
     (s V_prepare_new_block_z + max0(256 - s V_prepare_new_block_i) <= z)%Q
   | 15 => ((1 # 1) + s V_prepare_new_block_z
            + max0(255 - s V_prepare_new_block_i) <= z)%Q
   | 16 => ((1 # 1) + s V_prepare_new_block_z
            + max0(255 - s V_prepare_new_block_i) <= z)%Q
   | 17 => ((1 # 1) + s V_prepare_new_block_z
            + max0(256 - s V_prepare_new_block_i) <= z)%Q
   | 18 => ((1 # 1) + s V_prepare_new_block_z
            + max0(256 - s V_prepare_new_block_i) <= z)%Q
   | 19 => ((1 # 1) + s V_prepare_new_block_z
            + max0(256 - s V_prepare_new_block_i) <= z)%Q
   | 20 => (s V_prepare_new_block_z + max0(256 - s V_prepare_new_block_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_prepare_new_block =>
    [mkPA Q (fun n z s => ai_prepare_new_block n s /\ annot0_prepare_new_block n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_prepare_new_block (proc_start P_prepare_new_block) s1 (proc_end P_prepare_new_block) s2 ->
    (s2 V_prepare_new_block_z <= (256 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_prepare_new_block.
Qed.
