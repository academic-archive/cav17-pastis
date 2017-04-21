Require Import pasta.Pasta.

Inductive proc: Type :=
  P_name_sub_reloc_ptrs.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_name_sub_reloc_ptrs_z := 1%positive.
Notation V_name_sub_reloc_ptrs__tmp := 2%positive.
Notation V_name_sub_reloc_ptrs_i := 3%positive.
Notation V_name_sub_reloc_ptrs_gcst := 4%positive.
Notation V_name_sub_reloc_ptrs_size := 5%positive.
Notation V_name_sub_reloc_ptrs_vptr := 6%positive.
Definition Pedges_name_sub_reloc_ptrs: list (edge proc) :=
  (EA 1 (AAssign V_name_sub_reloc_ptrs_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_name_sub_reloc_ptrs_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_name_sub_reloc_ptrs__tmp
  (Some (EVar V_name_sub_reloc_ptrs_size))) 5)::(EA 5 (AAssign
  V_name_sub_reloc_ptrs_i (Some (ENum (0)))) 6)::(EA 6 ANone 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_name_sub_reloc_ptrs_i) s) < (eval (ENum (128))
  s))%Z)) 11)::(EA 8 (AGuard (fun s => ((eval (EVar V_name_sub_reloc_ptrs_i)
  s) >= (eval (ENum (128)) s))%Z)) 9)::(EA 9 AWeaken 10)::
  (EA 11 AWeaken 12)::(EA 12 ANone 13)::(EA 12 ANone 16)::
  (EA 13 AWeaken 14)::(EA 14 ANone 16)::(EA 14 ANone 15)::(EA 15 ANone 16)::
  (EA 16 ANone 17)::(EA 17 (AAssign V_name_sub_reloc_ptrs_i
  (Some (EAdd (EVar V_name_sub_reloc_ptrs_i) (ENum (1))))) 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign V_name_sub_reloc_ptrs_z
  (Some (EAdd (ENum (1)) (EVar V_name_sub_reloc_ptrs_z)))) 21)::
  (EA 21 AWeaken 8)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_name_sub_reloc_ptrs => Pedges_name_sub_reloc_ptrs
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_name_sub_reloc_ptrs => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_name_sub_reloc_ptrs (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_name_sub_reloc_ptrs_z <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0)%Z
   | 3 => (-1 * s V_name_sub_reloc_ptrs_z <= 0 /\ 1 * s V_name_sub_reloc_ptrs_z <= 0 /\ -1 * s V_name_sub_reloc_ptrs_i <= 0)%Z
   | 4 => (-1 * s V_name_sub_reloc_ptrs_i <= 0 /\ 1 * s V_name_sub_reloc_ptrs_z <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0)%Z
   | 5 => (-1 * s V_name_sub_reloc_ptrs_z <= 0 /\ 1 * s V_name_sub_reloc_ptrs_z <= 0 /\ -1 * s V_name_sub_reloc_ptrs_i <= 0)%Z
   | 6 => (1 * s V_name_sub_reloc_ptrs_z <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0 /\ 1 * s V_name_sub_reloc_ptrs_i <= 0 /\ -1 * s V_name_sub_reloc_ptrs_i <= 0)%Z
   | 7 => (-1 * s V_name_sub_reloc_ptrs_i <= 0 /\ 1 * s V_name_sub_reloc_ptrs_i <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0 /\ 1 * s V_name_sub_reloc_ptrs_z <= 0)%Z
   | 8 => (-1 * s V_name_sub_reloc_ptrs_z <= 0 /\ -1 * s V_name_sub_reloc_ptrs_i <= 0 /\ 1 * s V_name_sub_reloc_ptrs_i + -128 <= 0)%Z
   | 9 => (1 * s V_name_sub_reloc_ptrs_i + -128 <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0 /\ -1 * s V_name_sub_reloc_ptrs_i + 128 <= 0)%Z
   | 10 => (-1 * s V_name_sub_reloc_ptrs_i + 128 <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0 /\ 1 * s V_name_sub_reloc_ptrs_i + -128 <= 0)%Z
   | 11 => (-1 * s V_name_sub_reloc_ptrs_i <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0 /\ 1 * s V_name_sub_reloc_ptrs_i + -127 <= 0)%Z
   | 12 => (1 * s V_name_sub_reloc_ptrs_i + -127 <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0 /\ -1 * s V_name_sub_reloc_ptrs_i <= 0)%Z
   | 13 => (-1 * s V_name_sub_reloc_ptrs_i <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0 /\ 1 * s V_name_sub_reloc_ptrs_i + -127 <= 0)%Z
   | 14 => (1 * s V_name_sub_reloc_ptrs_i + -127 <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0 /\ -1 * s V_name_sub_reloc_ptrs_i <= 0)%Z
   | 15 => (-1 * s V_name_sub_reloc_ptrs_i <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0 /\ 1 * s V_name_sub_reloc_ptrs_i + -127 <= 0)%Z
   | 16 => (1 * s V_name_sub_reloc_ptrs_i + -127 <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0 /\ -1 * s V_name_sub_reloc_ptrs_i <= 0)%Z
   | 17 => (-1 * s V_name_sub_reloc_ptrs_i <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0 /\ 1 * s V_name_sub_reloc_ptrs_i + -127 <= 0)%Z
   | 18 => (-1 * s V_name_sub_reloc_ptrs_z <= 0 /\ -1 * s V_name_sub_reloc_ptrs_i + 1 <= 0 /\ 1 * s V_name_sub_reloc_ptrs_i + -128 <= 0)%Z
   | 19 => (1 * s V_name_sub_reloc_ptrs_i + -128 <= 0 /\ -1 * s V_name_sub_reloc_ptrs_i + 1 <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z <= 0)%Z
   | 20 => (-1 * s V_name_sub_reloc_ptrs_z <= 0 /\ -1 * s V_name_sub_reloc_ptrs_i + 1 <= 0 /\ 1 * s V_name_sub_reloc_ptrs_i + -128 <= 0)%Z
   | 21 => (1 * s V_name_sub_reloc_ptrs_i + -128 <= 0 /\ -1 * s V_name_sub_reloc_ptrs_i + 1 <= 0 /\ -1 * s V_name_sub_reloc_ptrs_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_name_sub_reloc_ptrs (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((128 # 1) <= z)%Q
   | 2 => ((128 # 1) + s V_name_sub_reloc_ptrs_z <= z)%Q
   | 3 => ((128 # 1) + s V_name_sub_reloc_ptrs_z <= z)%Q
   | 4 => ((128 # 1) + s V_name_sub_reloc_ptrs_z <= z)%Q
   | 5 => ((128 # 1) + s V_name_sub_reloc_ptrs_z <= z)%Q
   | 6 => (s V_name_sub_reloc_ptrs_z + max0(128 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 7 => (s V_name_sub_reloc_ptrs_z + max0(128 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 8 => (s V_name_sub_reloc_ptrs_z + max0(128 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (128 - s V_name_sub_reloc_ptrs_i) (127
                                                                    - s V_name_sub_reloc_ptrs_i));
      (*-1 0*) F_max0_ge_0 (127 - s V_name_sub_reloc_ptrs_i)]
     (s V_name_sub_reloc_ptrs_z + max0(128 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 10 => (s V_name_sub_reloc_ptrs_z <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (128 - s V_name_sub_reloc_ptrs_i) (1)]
     (s V_name_sub_reloc_ptrs_z + max0(128 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 12 => ((1 # 1) + s V_name_sub_reloc_ptrs_z
            + max0(127 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 13 => ((1 # 1) + s V_name_sub_reloc_ptrs_z
            + max0(127 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 14 => ((1 # 1) + s V_name_sub_reloc_ptrs_z
            + max0(127 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 15 => ((1 # 1) + s V_name_sub_reloc_ptrs_z
            + max0(127 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 16 => ((1 # 1) + s V_name_sub_reloc_ptrs_z
            + max0(127 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 17 => ((1 # 1) + s V_name_sub_reloc_ptrs_z
            + max0(127 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 18 => ((1 # 1) + s V_name_sub_reloc_ptrs_z
            + max0(128 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 19 => ((1 # 1) + s V_name_sub_reloc_ptrs_z
            + max0(128 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 20 => ((1 # 1) + s V_name_sub_reloc_ptrs_z
            + max0(128 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | 21 => (s V_name_sub_reloc_ptrs_z + max0(128 - s V_name_sub_reloc_ptrs_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_name_sub_reloc_ptrs =>
    [mkPA Q (fun n z s => ai_name_sub_reloc_ptrs n s /\ annot0_name_sub_reloc_ptrs n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_name_sub_reloc_ptrs (proc_start P_name_sub_reloc_ptrs) s1 (proc_end P_name_sub_reloc_ptrs) s2 ->
    (s2 V_name_sub_reloc_ptrs_z <= (128 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_name_sub_reloc_ptrs.
Qed.
