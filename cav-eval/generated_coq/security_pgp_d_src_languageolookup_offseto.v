Require Import pasta.Pasta.

Inductive proc: Type :=
  P_lookup_offset.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_lookup_offset_z := 1%positive.
Notation V_lookup_offset__tmp := 2%positive.
Notation V_lookup_offset__tmp1 := 3%positive.
Notation V_lookup_offset_i := 4%positive.
Notation V_lookup_offset_nmsg := 5%positive.
Notation V_lookup_offset_crc := 6%positive.
Definition Pedges_lookup_offset: list (edge proc) :=
  (EA 1 (AAssign V_lookup_offset_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_lookup_offset__tmp (Some (EVar V_lookup_offset_crc))) 3)::(EA 3 (AAssign
  V_lookup_offset_i (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_lookup_offset_i) s) <
  (eval (EVar V_lookup_offset_nmsg) s))%Z)) 11)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_lookup_offset_i) s) >=
  (eval (EVar V_lookup_offset_nmsg) s))%Z)) 7)::(EA 7 AWeaken 8)::
  (EA 8 (AAssign V_lookup_offset__tmp1 (Some (ENum (-1)))) 9)::
  (EA 9 ANone 10)::(EA 10 AWeaken 22)::(EA 11 AWeaken 12)::(EA 12 ANone 19)::
  (EA 12 ANone 13)::(EA 13 ANone 14)::(EA 14 (AAssign V_lookup_offset_i
  (Some (EAdd (EVar V_lookup_offset_i) (ENum (1))))) 15)::(EA 15 ANone 16)::
  (EA 16 ANone 17)::(EA 17 (AAssign V_lookup_offset_z (Some (EAdd (ENum (1))
  (EVar V_lookup_offset_z)))) 18)::(EA 18 AWeaken 6)::(EA 19 (AAssign
  V_lookup_offset__tmp1 None) 20)::(EA 20 ANone 21)::(EA 21 AWeaken 22)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_lookup_offset => Pedges_lookup_offset
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_lookup_offset => 22
     end)%positive;
  var_global := var_global
}.

Definition ai_lookup_offset (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_lookup_offset_z <= 0 /\ -1 * s V_lookup_offset_z <= 0)%Z
   | 3 => (-1 * s V_lookup_offset_z <= 0 /\ 1 * s V_lookup_offset_z <= 0)%Z
   | 4 => (1 * s V_lookup_offset_z <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ 1 * s V_lookup_offset_i <= 0 /\ -1 * s V_lookup_offset_i <= 0)%Z
   | 5 => (-1 * s V_lookup_offset_i <= 0 /\ 1 * s V_lookup_offset_i <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ 1 * s V_lookup_offset_z <= 0)%Z
   | 6 => (-1 * s V_lookup_offset_z <= 0 /\ -1 * s V_lookup_offset_i <= 0)%Z
   | 7 => (-1 * s V_lookup_offset_i <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ -1 * s V_lookup_offset_i+ 1 * s V_lookup_offset_nmsg <= 0)%Z
   | 8 => (-1 * s V_lookup_offset_i+ 1 * s V_lookup_offset_nmsg <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ -1 * s V_lookup_offset_i <= 0)%Z
   | 9 => (-1 * s V_lookup_offset_i <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ -1 * s V_lookup_offset_i+ 1 * s V_lookup_offset_nmsg <= 0 /\ 1 * s V_lookup_offset__tmp1 + 1 <= 0 /\ -1 * s V_lookup_offset__tmp1 + -1 <= 0)%Z
   | 10 => (-1 * s V_lookup_offset__tmp1 + -1 <= 0 /\ 1 * s V_lookup_offset__tmp1 + 1 <= 0 /\ -1 * s V_lookup_offset_i+ 1 * s V_lookup_offset_nmsg <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ -1 * s V_lookup_offset_i <= 0)%Z
   | 11 => (-1 * s V_lookup_offset_i <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ 1 * s V_lookup_offset_i+ -1 * s V_lookup_offset_nmsg + 1 <= 0)%Z
   | 12 => (1 * s V_lookup_offset_i+ -1 * s V_lookup_offset_nmsg + 1 <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ -1 * s V_lookup_offset_i <= 0)%Z
   | 13 => (-1 * s V_lookup_offset_i <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ 1 * s V_lookup_offset_i+ -1 * s V_lookup_offset_nmsg + 1 <= 0)%Z
   | 14 => (1 * s V_lookup_offset_i+ -1 * s V_lookup_offset_nmsg + 1 <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ -1 * s V_lookup_offset_i <= 0)%Z
   | 15 => (-1 * s V_lookup_offset_z <= 0 /\ 1 * s V_lookup_offset_i+ -1 * s V_lookup_offset_nmsg <= 0 /\ -1 * s V_lookup_offset_i + 1 <= 0)%Z
   | 16 => (-1 * s V_lookup_offset_i + 1 <= 0 /\ 1 * s V_lookup_offset_i+ -1 * s V_lookup_offset_nmsg <= 0 /\ -1 * s V_lookup_offset_z <= 0)%Z
   | 17 => (-1 * s V_lookup_offset_z <= 0 /\ 1 * s V_lookup_offset_i+ -1 * s V_lookup_offset_nmsg <= 0 /\ -1 * s V_lookup_offset_i + 1 <= 0)%Z
   | 18 => (-1 * s V_lookup_offset_i + 1 <= 0 /\ 1 * s V_lookup_offset_i+ -1 * s V_lookup_offset_nmsg <= 0 /\ -1 * s V_lookup_offset_z + 1 <= 0)%Z
   | 19 => (-1 * s V_lookup_offset_i <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ 1 * s V_lookup_offset_i+ -1 * s V_lookup_offset_nmsg + 1 <= 0)%Z
   | 20 => (1 * s V_lookup_offset_i+ -1 * s V_lookup_offset_nmsg + 1 <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ -1 * s V_lookup_offset_i <= 0)%Z
   | 21 => (-1 * s V_lookup_offset_i <= 0 /\ -1 * s V_lookup_offset_z <= 0 /\ 1 * s V_lookup_offset_i+ -1 * s V_lookup_offset_nmsg + 1 <= 0)%Z
   | 22 => (-1 * s V_lookup_offset_z <= 0 /\ -1 * s V_lookup_offset_i <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_lookup_offset (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_lookup_offset_nmsg) <= z)%Q
   | 2 => (s V_lookup_offset_z + max0(s V_lookup_offset_nmsg) <= z)%Q
   | 3 => (s V_lookup_offset_z + max0(s V_lookup_offset_nmsg) <= z)%Q
   | 4 => (s V_lookup_offset_z
           + max0(-s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 5 => (s V_lookup_offset_z
           + max0(-s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 6 => (s V_lookup_offset_z
           + max0(-s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 7 => (s V_lookup_offset_z
           + max0(-s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 8 => (s V_lookup_offset_z
           + max0(-s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 9 => (s V_lookup_offset_z
           + max0(-s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_lookup_offset_i
                                             + s V_lookup_offset_nmsg) (-1
                                                                    - s V_lookup_offset_i
                                                                    + s V_lookup_offset_nmsg));
      (*-1 0*) F_max0_ge_0 (-1 - s V_lookup_offset_i + s V_lookup_offset_nmsg)]
     (s V_lookup_offset_z
      + max0(-s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 11 => hints
     [(*0 1*) F_max0_pre_decrement 1 (-s V_lookup_offset_i
                                      + s V_lookup_offset_nmsg) (1)]
     (s V_lookup_offset_z
      + max0(-s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 12 => ((1 # 1) + s V_lookup_offset_z
            + max0(-1 - s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 13 => ((1 # 1) + s V_lookup_offset_z
            + max0(-1 - s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 14 => ((1 # 1) + s V_lookup_offset_z
            + max0(-1 - s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 15 => ((1 # 1) + s V_lookup_offset_z
            + max0(-s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 16 => ((1 # 1) + s V_lookup_offset_z
            + max0(-s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 17 => ((1 # 1) + s V_lookup_offset_z
            + max0(-s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 18 => (s V_lookup_offset_z
            + max0(-s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 19 => ((1 # 1) + s V_lookup_offset_z
            + max0(-1 - s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 20 => ((1 # 1) + s V_lookup_offset_z
            + max0(-1 - s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 - s V_lookup_offset_i + s V_lookup_offset_nmsg)]
     ((1 # 1) + s V_lookup_offset_z
      + max0(-1 - s V_lookup_offset_i + s V_lookup_offset_nmsg) <= z)%Q
   | 22 => (s V_lookup_offset_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_lookup_offset =>
    [mkPA Q (fun n z s => ai_lookup_offset n s /\ annot0_lookup_offset n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_lookup_offset (proc_start P_lookup_offset) s1 (proc_end P_lookup_offset) s2 ->
    (s2 V_lookup_offset_z <= max0(s1 V_lookup_offset_nmsg))%Q.
Proof.
  prove_bound ipa admissible_ipa P_lookup_offset.
Qed.
