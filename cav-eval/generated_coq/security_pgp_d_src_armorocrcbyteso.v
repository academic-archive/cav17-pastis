Require Import pasta.Pasta.

Inductive proc: Type :=
  P_crcbytes.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_crcbytes_z := 1%positive.
Notation V_crcbytes__tmp := 2%positive.
Notation V_crcbytes__tmp1 := 3%positive.
Notation V_crcbytes_accum := 4%positive.
Notation V_crcbytes_buf := 5%positive.
Notation V_crcbytes_len := 6%positive.
Definition Pedges_crcbytes: list (edge proc) :=
  (EA 1 (AAssign V_crcbytes_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_crcbytes__tmp (Some (EVar V_crcbytes_len))) 3)::(EA 3 (AAssign
  V_crcbytes__tmp1 (Some (EVar V_crcbytes_accum))) 4)::(EA 4 ANone 5)::
  (EA 5 (AAssign V_crcbytes__tmp1 None) 6)::(EA 6 ANone 7)::(EA 7 (AAssign
  V_crcbytes__tmp (Some (EAdd (EVar V_crcbytes__tmp) (ENum (-1))))) 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EAdd (EVar V_crcbytes__tmp) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 12)::(EA 9 (AGuard
  (fun s => ((eval (EAdd (EVar V_crcbytes__tmp) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 12 AWeaken 13)::
  (EA 13 ANone 14)::(EA 14 (AAssign V_crcbytes_z (Some (EAdd (ENum (1))
  (EVar V_crcbytes_z)))) 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_crcbytes => Pedges_crcbytes
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_crcbytes => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_crcbytes (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_crcbytes_z <= 0 /\ -1 * s V_crcbytes_z <= 0)%Z
   | 3 => (-1 * s V_crcbytes_z <= 0 /\ 1 * s V_crcbytes_z <= 0)%Z
   | 4 => (1 * s V_crcbytes_z <= 0 /\ -1 * s V_crcbytes_z <= 0)%Z
   | 5 => (-1 * s V_crcbytes_z <= 0)%Z
   | 6 => (-1 * s V_crcbytes_z <= 0)%Z
   | 7 => (-1 * s V_crcbytes_z <= 0)%Z
   | 8 => (-1 * s V_crcbytes_z <= 0)%Z
   | 9 => (-1 * s V_crcbytes_z <= 0)%Z
   | 10 => (-1 * s V_crcbytes_z <= 0 /\ 1 * s V_crcbytes__tmp + -1 <= 0 /\ -1 * s V_crcbytes__tmp + 1 <= 0)%Z
   | 11 => (-1 * s V_crcbytes__tmp + 1 <= 0 /\ 1 * s V_crcbytes__tmp + -1 <= 0 /\ -1 * s V_crcbytes_z <= 0)%Z
   | 12 => (-1 * s V_crcbytes_z <= 0)%Z
   | 13 => (-1 * s V_crcbytes_z <= 0)%Z
   | 14 => (-1 * s V_crcbytes_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_crcbytes (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_crcbytes_len <= z)%Q
   | 2 => (s V_crcbytes_len + s V_crcbytes_z <= z)%Q
   | 3 => (s V_crcbytes__tmp + s V_crcbytes_z <= z)%Q
   | 4 => (s V_crcbytes__tmp + s V_crcbytes_z <= z)%Q
   | 5 => (s V_crcbytes__tmp + s V_crcbytes_z <= z)%Q
   | 6 => (s V_crcbytes__tmp + s V_crcbytes_z <= z)%Q
   | 7 => (s V_crcbytes__tmp + s V_crcbytes_z <= z)%Q
   | 8 => ((1 # 1) + s V_crcbytes__tmp + s V_crcbytes_z <= z)%Q
   | 9 => ((1 # 1) + s V_crcbytes__tmp + s V_crcbytes_z <= z)%Q
   | 10 => hints
     [(*-2 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_crcbytes__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_crcbytes__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_crcbytes__tmp))]
     ((1 # 1) + s V_crcbytes__tmp + s V_crcbytes_z <= z)%Q
   | 11 => (s V_crcbytes_z <= z)%Q
   | 12 => ((1 # 1) + s V_crcbytes__tmp + s V_crcbytes_z <= z)%Q
   | 13 => ((1 # 1) + s V_crcbytes__tmp + s V_crcbytes_z <= z)%Q
   | 14 => ((1 # 1) + s V_crcbytes__tmp + s V_crcbytes_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_crcbytes =>
    [mkPA Q (fun n z s => ai_crcbytes n s /\ annot0_crcbytes n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_crcbytes (proc_start P_crcbytes) s1 (proc_end P_crcbytes) s2 ->
    (s2 V_crcbytes_z <= s1 V_crcbytes_len)%Q.
Proof.
  prove_bound ipa admissible_ipa P_crcbytes.
Qed.
