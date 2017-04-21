Require Import pasta.Pasta.

Inductive proc: Type :=
  P_expand_suf.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_expand_suf_z := 1%positive.
Notation V_expand_suf__tmp := 2%positive.
Notation V_expand_suf__tmp1 := 3%positive.
Notation V_expand_suf_entcount := 4%positive.
Notation V_expand_suf_explength := 5%positive.
Notation V_expand_suf_numsflags := 6%positive.
Notation V_expand_suf_croot := 7%positive.
Notation V_expand_suf_extra := 8%positive.
Notation V_expand_suf_mask := 9%positive.
Notation V_expand_suf_optflags := 10%positive.
Notation V_expand_suf_option := 11%positive.
Notation V_expand_suf_rootword := 12%positive.
Definition Pedges_expand_suf: list (edge proc) :=
  (EA 1 (AAssign V_expand_suf_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_expand_suf__tmp1 (Some (EVar V_expand_suf_optflags))) 3)::(EA 3 (AAssign
  V_expand_suf__tmp (Some (EVar V_expand_suf_option))) 4)::(EA 4 (AAssign
  V_expand_suf_entcount (Some (EVar V_expand_suf_numsflags))) 5)::
  (EA 5 (AAssign V_expand_suf_explength (Some (ENum (0)))) 6)::
  (EA 6 ANone 7)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_expand_suf_entcount) s) > (eval (ENum (0))
  s))%Z)) 11)::(EA 8 (AGuard (fun s => ((eval (EVar V_expand_suf_entcount)
  s) <= (eval (ENum (0)) s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 11 AWeaken 12)::
  (EA 12 ANone 13)::(EA 12 ANone 20)::(EA 13 AWeaken 14)::(EA 14 ANone 17)::
  (EA 14 ANone 15)::(EA 15 AWeaken 16)::(EA 16 ANone 17)::(EA 16 ANone 19)::
  (EA 17 (AAssign V_expand_suf_explength None) 18)::(EA 18 ANone 19)::
  (EA 19 ANone 20)::(EA 20 ANone 21)::(EA 21 (AAssign V_expand_suf_entcount
  (Some (EAdd (EVar V_expand_suf_entcount) (ENum (-1))))) 22)::
  (EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 (AAssign V_expand_suf_z
  (Some (EAdd (ENum (1)) (EVar V_expand_suf_z)))) 25)::(EA 25 AWeaken 8)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_expand_suf => Pedges_expand_suf
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_expand_suf => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_expand_suf (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_expand_suf_z <= 0 /\ -1 * s V_expand_suf_z <= 0)%Z
   | 3 => (-1 * s V_expand_suf_z <= 0 /\ 1 * s V_expand_suf_z <= 0)%Z
   | 4 => (1 * s V_expand_suf_z <= 0 /\ -1 * s V_expand_suf_z <= 0)%Z
   | 5 => (-1 * s V_expand_suf_z <= 0 /\ 1 * s V_expand_suf_z <= 0)%Z
   | 6 => (1 * s V_expand_suf_z <= 0 /\ -1 * s V_expand_suf_z <= 0 /\ 1 * s V_expand_suf_explength <= 0 /\ -1 * s V_expand_suf_explength <= 0)%Z
   | 7 => (-1 * s V_expand_suf_explength <= 0 /\ 1 * s V_expand_suf_explength <= 0 /\ -1 * s V_expand_suf_z <= 0 /\ 1 * s V_expand_suf_z <= 0)%Z
   | 8 => (-1 * s V_expand_suf_z <= 0)%Z
   | 9 => (-1 * s V_expand_suf_z <= 0 /\ 1 * s V_expand_suf_entcount <= 0)%Z
   | 10 => (1 * s V_expand_suf_entcount <= 0 /\ -1 * s V_expand_suf_z <= 0)%Z
   | 11 => (-1 * s V_expand_suf_z <= 0 /\ -1 * s V_expand_suf_entcount + 1 <= 0)%Z
   | 12 => (-1 * s V_expand_suf_entcount + 1 <= 0 /\ -1 * s V_expand_suf_z <= 0)%Z
   | 13 => (-1 * s V_expand_suf_z <= 0 /\ -1 * s V_expand_suf_entcount + 1 <= 0)%Z
   | 14 => (-1 * s V_expand_suf_entcount + 1 <= 0 /\ -1 * s V_expand_suf_z <= 0)%Z
   | 15 => (-1 * s V_expand_suf_z <= 0 /\ -1 * s V_expand_suf_entcount + 1 <= 0)%Z
   | 16 => (-1 * s V_expand_suf_entcount + 1 <= 0 /\ -1 * s V_expand_suf_z <= 0)%Z
   | 17 => (-1 * s V_expand_suf_z <= 0 /\ -1 * s V_expand_suf_entcount + 1 <= 0)%Z
   | 18 => (-1 * s V_expand_suf_entcount + 1 <= 0 /\ -1 * s V_expand_suf_z <= 0)%Z
   | 19 => (-1 * s V_expand_suf_z <= 0 /\ -1 * s V_expand_suf_entcount + 1 <= 0)%Z
   | 20 => (-1 * s V_expand_suf_entcount + 1 <= 0 /\ -1 * s V_expand_suf_z <= 0)%Z
   | 21 => (-1 * s V_expand_suf_z <= 0 /\ -1 * s V_expand_suf_entcount + 1 <= 0)%Z
   | 22 => (-1 * s V_expand_suf_z <= 0 /\ -1 * s V_expand_suf_entcount <= 0)%Z
   | 23 => (-1 * s V_expand_suf_entcount <= 0 /\ -1 * s V_expand_suf_z <= 0)%Z
   | 24 => (-1 * s V_expand_suf_z <= 0 /\ -1 * s V_expand_suf_entcount <= 0)%Z
   | 25 => (-1 * s V_expand_suf_entcount <= 0 /\ -1 * s V_expand_suf_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_expand_suf (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_expand_suf_numsflags) <= z)%Q
   | 2 => (s V_expand_suf_z + max0(s V_expand_suf_numsflags) <= z)%Q
   | 3 => (s V_expand_suf_z + max0(s V_expand_suf_numsflags) <= z)%Q
   | 4 => (s V_expand_suf_z + max0(s V_expand_suf_numsflags) <= z)%Q
   | 5 => (s V_expand_suf_z + max0(s V_expand_suf_entcount) <= z)%Q
   | 6 => (s V_expand_suf_z + max0(s V_expand_suf_entcount) <= z)%Q
   | 7 => (s V_expand_suf_z + max0(s V_expand_suf_entcount) <= z)%Q
   | 8 => (s V_expand_suf_z + max0(s V_expand_suf_entcount) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_expand_suf_entcount) (-1
                                                                    + s V_expand_suf_entcount));
      (*-1 0*) F_max0_ge_0 (-1 + s V_expand_suf_entcount)]
     (s V_expand_suf_z + max0(s V_expand_suf_entcount) <= z)%Q
   | 10 => (s V_expand_suf_z <= z)%Q
   | 11 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_suf_entcount)) (F_check_ge (s V_expand_suf_entcount) (0))]
     (s V_expand_suf_z + max0(s V_expand_suf_entcount) <= z)%Q
   | 12 => (s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 13 => (s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 14 => (s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 15 => (s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 16 => (s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 17 => (s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 18 => (s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 19 => (s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 20 => (s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 21 => (s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 22 => ((1 # 1) + s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 23 => ((1 # 1) + s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 24 => ((1 # 1) + s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | 25 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_suf_entcount) (0))) (F_max0_ge_0 (s V_expand_suf_entcount))]
     (s V_expand_suf_entcount + s V_expand_suf_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_expand_suf =>
    [mkPA Q (fun n z s => ai_expand_suf n s /\ annot0_expand_suf n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_expand_suf (proc_start P_expand_suf) s1 (proc_end P_expand_suf) s2 ->
    (s2 V_expand_suf_z <= max0(s1 V_expand_suf_numsflags))%Q.
Proof.
  prove_bound ipa admissible_ipa P_expand_suf.
Qed.
