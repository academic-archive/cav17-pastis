Require Import pasta.Pasta.

Inductive proc: Type :=
  P_font_dir_reloc_ptrs.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_font_dir_reloc_ptrs_z := 1%positive.
Notation V_font_dir_reloc_ptrs__tmp := 2%positive.
Notation V_font_dir_reloc_ptrs_chi := 3%positive.
Notation V_font_dir_reloc_ptrs_vptr_dref_off48_off40 := 4%positive.
Notation V_font_dir_reloc_ptrs_gcst := 5%positive.
Notation V_font_dir_reloc_ptrs_size := 6%positive.
Notation V_font_dir_reloc_ptrs_vptr := 7%positive.
Definition Pedges_font_dir_reloc_ptrs: list (edge proc) :=
  (EA 1 (AAssign V_font_dir_reloc_ptrs_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_font_dir_reloc_ptrs__tmp
  (Some (EVar V_font_dir_reloc_ptrs_size))) 3)::(EA 3 (AAssign
  V_font_dir_reloc_ptrs_chi
  (Some (EVar V_font_dir_reloc_ptrs_vptr_dref_off48_off40))) 4)::
  (EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_font_dir_reloc_ptrs_chi) s) >= (eval (ENum (0))
  s))%Z)) 9)::(EA 6 (AGuard (fun s => ((eval (EVar V_font_dir_reloc_ptrs_chi)
  s) < (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 ANone 11)::(EA 10 ANone 12)::(EA 11 ANone 12)::(EA 12 ANone 13)::
  (EA 13 (AAssign V_font_dir_reloc_ptrs_chi
  (Some (EAdd (EVar V_font_dir_reloc_ptrs_chi) (ENum (-1))))) 14)::
  (EA 14 ANone 15)::(EA 15 ANone 16)::(EA 16 (AAssign V_font_dir_reloc_ptrs_z
  (Some (EAdd (ENum (1)) (EVar V_font_dir_reloc_ptrs_z)))) 17)::
  (EA 17 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_font_dir_reloc_ptrs => Pedges_font_dir_reloc_ptrs
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_font_dir_reloc_ptrs => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_font_dir_reloc_ptrs (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_font_dir_reloc_ptrs_z <= 0 /\ -1 * s V_font_dir_reloc_ptrs_z <= 0)%Z
   | 3 => (-1 * s V_font_dir_reloc_ptrs_z <= 0 /\ 1 * s V_font_dir_reloc_ptrs_z <= 0)%Z
   | 4 => (1 * s V_font_dir_reloc_ptrs_z <= 0 /\ -1 * s V_font_dir_reloc_ptrs_z <= 0)%Z
   | 5 => (-1 * s V_font_dir_reloc_ptrs_z <= 0 /\ 1 * s V_font_dir_reloc_ptrs_z <= 0)%Z
   | 6 => (-1 * s V_font_dir_reloc_ptrs_z <= 0)%Z
   | 7 => (-1 * s V_font_dir_reloc_ptrs_z <= 0 /\ 1 * s V_font_dir_reloc_ptrs_chi + 1 <= 0)%Z
   | 8 => (1 * s V_font_dir_reloc_ptrs_chi + 1 <= 0 /\ -1 * s V_font_dir_reloc_ptrs_z <= 0)%Z
   | 9 => (-1 * s V_font_dir_reloc_ptrs_z <= 0 /\ -1 * s V_font_dir_reloc_ptrs_chi <= 0)%Z
   | 10 => (-1 * s V_font_dir_reloc_ptrs_chi <= 0 /\ -1 * s V_font_dir_reloc_ptrs_z <= 0)%Z
   | 11 => (-1 * s V_font_dir_reloc_ptrs_z <= 0 /\ -1 * s V_font_dir_reloc_ptrs_chi <= 0)%Z
   | 12 => (-1 * s V_font_dir_reloc_ptrs_chi <= 0 /\ -1 * s V_font_dir_reloc_ptrs_z <= 0)%Z
   | 13 => (-1 * s V_font_dir_reloc_ptrs_z <= 0 /\ -1 * s V_font_dir_reloc_ptrs_chi <= 0)%Z
   | 14 => (-1 * s V_font_dir_reloc_ptrs_z <= 0 /\ -1 * s V_font_dir_reloc_ptrs_chi + -1 <= 0)%Z
   | 15 => (-1 * s V_font_dir_reloc_ptrs_chi + -1 <= 0 /\ -1 * s V_font_dir_reloc_ptrs_z <= 0)%Z
   | 16 => (-1 * s V_font_dir_reloc_ptrs_z <= 0 /\ -1 * s V_font_dir_reloc_ptrs_chi + -1 <= 0)%Z
   | 17 => (-1 * s V_font_dir_reloc_ptrs_chi + -1 <= 0 /\ -1 * s V_font_dir_reloc_ptrs_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_font_dir_reloc_ptrs (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(1 + s V_font_dir_reloc_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 2 => (s V_font_dir_reloc_ptrs_z
           + max0(1 + s V_font_dir_reloc_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 3 => (s V_font_dir_reloc_ptrs_z
           + max0(1 + s V_font_dir_reloc_ptrs_vptr_dref_off48_off40) <= z)%Q
   | 4 => (s V_font_dir_reloc_ptrs_z + max0(1 + s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | 5 => (s V_font_dir_reloc_ptrs_z + max0(1 + s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | 6 => (s V_font_dir_reloc_ptrs_z + max0(1 + s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_font_dir_reloc_ptrs_chi) (s V_font_dir_reloc_ptrs_chi));
      (*-1 0*) F_max0_ge_0 (s V_font_dir_reloc_ptrs_chi)]
     (s V_font_dir_reloc_ptrs_z + max0(1 + s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | 8 => (s V_font_dir_reloc_ptrs_z <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1 + s V_font_dir_reloc_ptrs_chi) (1)]
     (s V_font_dir_reloc_ptrs_z + max0(1 + s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | 10 => ((1 # 1) + s V_font_dir_reloc_ptrs_z
            + max0(s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | 11 => ((1 # 1) + s V_font_dir_reloc_ptrs_z
            + max0(s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | 12 => ((1 # 1) + s V_font_dir_reloc_ptrs_z
            + max0(s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | 13 => ((1 # 1) + s V_font_dir_reloc_ptrs_z
            + max0(s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | 14 => ((1 # 1) + s V_font_dir_reloc_ptrs_z
            + max0(1 + s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | 15 => ((1 # 1) + s V_font_dir_reloc_ptrs_z
            + max0(1 + s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | 16 => ((1 # 1) + s V_font_dir_reloc_ptrs_z
            + max0(1 + s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | 17 => (s V_font_dir_reloc_ptrs_z + max0(1 + s V_font_dir_reloc_ptrs_chi) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_font_dir_reloc_ptrs =>
    [mkPA Q (fun n z s => ai_font_dir_reloc_ptrs n s /\ annot0_font_dir_reloc_ptrs n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_font_dir_reloc_ptrs (proc_start P_font_dir_reloc_ptrs) s1 (proc_end P_font_dir_reloc_ptrs) s2 ->
    (s2 V_font_dir_reloc_ptrs_z <= max0(1
                                        + s1 V_font_dir_reloc_ptrs_vptr_dref_off48_off40))%Q.
Proof.
  prove_bound ipa admissible_ipa P_font_dir_reloc_ptrs.
Qed.
