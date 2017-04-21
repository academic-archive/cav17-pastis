Require Import pasta.Pasta.

Inductive proc: Type :=
  P_expand_pre.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_expand_pre_z := 1%positive.
Notation V_expand_pre__tmp := 2%positive.
Notation V_expand_pre_entcount := 3%positive.
Notation V_expand_pre_explength := 4%positive.
Notation V_expand_pre_numpflags := 5%positive.
Notation V_expand_pre_croot := 6%positive.
Notation V_expand_pre_extra := 7%positive.
Notation V_expand_pre_mask := 8%positive.
Notation V_expand_pre_option := 9%positive.
Notation V_expand_pre_rootword := 10%positive.
Definition Pedges_expand_pre: list (edge proc) :=
  (EA 1 (AAssign V_expand_pre_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_expand_pre__tmp (Some (EVar V_expand_pre_option))) 3)::(EA 3 (AAssign
  V_expand_pre_entcount (Some (EVar V_expand_pre_numpflags))) 4)::
  (EA 4 (AAssign V_expand_pre_explength (Some (ENum (0)))) 5)::
  (EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_expand_pre_entcount) s) > (eval (ENum (0))
  s))%Z)) 10)::(EA 7 (AGuard (fun s => ((eval (EVar V_expand_pre_entcount)
  s) <= (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 10 AWeaken 11)::
  (EA 11 ANone 12)::(EA 11 ANone 14)::(EA 12 (AAssign V_expand_pre_explength
  None) 13)::(EA 13 ANone 14)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_expand_pre_entcount (Some (EAdd (EVar V_expand_pre_entcount)
  (ENum (-1))))) 16)::(EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign
  V_expand_pre_z (Some (EAdd (ENum (1)) (EVar V_expand_pre_z)))) 19)::
  (EA 19 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_expand_pre => Pedges_expand_pre
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_expand_pre => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_expand_pre (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_expand_pre_z <= 0 /\ -1 * s V_expand_pre_z <= 0)%Z
   | 3 => (-1 * s V_expand_pre_z <= 0 /\ 1 * s V_expand_pre_z <= 0)%Z
   | 4 => (1 * s V_expand_pre_z <= 0 /\ -1 * s V_expand_pre_z <= 0)%Z
   | 5 => (-1 * s V_expand_pre_z <= 0 /\ 1 * s V_expand_pre_z <= 0 /\ 1 * s V_expand_pre_explength <= 0 /\ -1 * s V_expand_pre_explength <= 0)%Z
   | 6 => (-1 * s V_expand_pre_explength <= 0 /\ 1 * s V_expand_pre_explength <= 0 /\ 1 * s V_expand_pre_z <= 0 /\ -1 * s V_expand_pre_z <= 0)%Z
   | 7 => (-1 * s V_expand_pre_z <= 0)%Z
   | 8 => (-1 * s V_expand_pre_z <= 0 /\ 1 * s V_expand_pre_entcount <= 0)%Z
   | 9 => (1 * s V_expand_pre_entcount <= 0 /\ -1 * s V_expand_pre_z <= 0)%Z
   | 10 => (-1 * s V_expand_pre_z <= 0 /\ -1 * s V_expand_pre_entcount + 1 <= 0)%Z
   | 11 => (-1 * s V_expand_pre_entcount + 1 <= 0 /\ -1 * s V_expand_pre_z <= 0)%Z
   | 12 => (-1 * s V_expand_pre_z <= 0 /\ -1 * s V_expand_pre_entcount + 1 <= 0)%Z
   | 13 => (-1 * s V_expand_pre_entcount + 1 <= 0 /\ -1 * s V_expand_pre_z <= 0)%Z
   | 14 => (-1 * s V_expand_pre_z <= 0 /\ -1 * s V_expand_pre_entcount + 1 <= 0)%Z
   | 15 => (-1 * s V_expand_pre_entcount + 1 <= 0 /\ -1 * s V_expand_pre_z <= 0)%Z
   | 16 => (-1 * s V_expand_pre_z <= 0 /\ -1 * s V_expand_pre_entcount <= 0)%Z
   | 17 => (-1 * s V_expand_pre_entcount <= 0 /\ -1 * s V_expand_pre_z <= 0)%Z
   | 18 => (-1 * s V_expand_pre_z <= 0 /\ -1 * s V_expand_pre_entcount <= 0)%Z
   | 19 => (-1 * s V_expand_pre_entcount <= 0 /\ -1 * s V_expand_pre_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_expand_pre (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_expand_pre_numpflags) <= z)%Q
   | 2 => (s V_expand_pre_z + max0(s V_expand_pre_numpflags) <= z)%Q
   | 3 => (s V_expand_pre_z + max0(s V_expand_pre_numpflags) <= z)%Q
   | 4 => (s V_expand_pre_z + max0(s V_expand_pre_entcount) <= z)%Q
   | 5 => (s V_expand_pre_z + max0(s V_expand_pre_entcount) <= z)%Q
   | 6 => (s V_expand_pre_z + max0(s V_expand_pre_entcount) <= z)%Q
   | 7 => (s V_expand_pre_z + max0(s V_expand_pre_entcount) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_expand_pre_entcount) (-1
                                                                    + s V_expand_pre_entcount));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_expand_pre_entcount)) (F_check_ge (0) (0))]
     (s V_expand_pre_z + max0(s V_expand_pre_entcount) <= z)%Q
   | 9 => (s V_expand_pre_z <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_expand_pre_entcount) (1)]
     (s V_expand_pre_z + max0(s V_expand_pre_entcount) <= z)%Q
   | 11 => ((1 # 1) + s V_expand_pre_z + max0(-1 + s V_expand_pre_entcount) <= z)%Q
   | 12 => ((1 # 1) + s V_expand_pre_z + max0(-1 + s V_expand_pre_entcount) <= z)%Q
   | 13 => ((1 # 1) + s V_expand_pre_z + max0(-1 + s V_expand_pre_entcount) <= z)%Q
   | 14 => ((1 # 1) + s V_expand_pre_z + max0(-1 + s V_expand_pre_entcount) <= z)%Q
   | 15 => ((1 # 1) + s V_expand_pre_z + max0(-1 + s V_expand_pre_entcount) <= z)%Q
   | 16 => ((1 # 1) + s V_expand_pre_z + max0(s V_expand_pre_entcount) <= z)%Q
   | 17 => ((1 # 1) + s V_expand_pre_z + max0(s V_expand_pre_entcount) <= z)%Q
   | 18 => ((1 # 1) + s V_expand_pre_z + max0(s V_expand_pre_entcount) <= z)%Q
   | 19 => (s V_expand_pre_z + max0(s V_expand_pre_entcount) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_expand_pre =>
    [mkPA Q (fun n z s => ai_expand_pre n s /\ annot0_expand_pre n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_expand_pre (proc_start P_expand_pre) s1 (proc_end P_expand_pre) s2 ->
    (s2 V_expand_pre_z <= max0(s1 V_expand_pre_numpflags))%Q.
Proof.
  prove_bound ipa admissible_ipa P_expand_pre.
Qed.
