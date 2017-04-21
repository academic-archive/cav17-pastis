Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ms_convert.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ms_convert_z := 1%positive.
Notation V_ms_convert_i := 2%positive.
Notation V_ms_convert_xr := 3%positive.
Notation V_ms_convert_xr_org := 4%positive.
Definition Pedges_ms_convert: list (edge proc) :=
  (EA 1 (AAssign V_ms_convert_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_ms_convert_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_ms_convert_i) s) <
  (eval (ENum (576)) s))%Z)) 8)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_ms_convert_i) s) >= (eval (ENum (576))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 8 AWeaken 9)::(EA 9 ANone 10)::
  (EA 10 (AAssign V_ms_convert_i (Some (EAdd (EVar V_ms_convert_i)
  (ENum (1))))) 11)::(EA 11 ANone 12)::(EA 12 ANone 13)::(EA 13 (AAssign
  V_ms_convert_z (Some (EAdd (ENum (1)) (EVar V_ms_convert_z)))) 14)::
  (EA 14 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ms_convert => Pedges_ms_convert
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ms_convert => 7
     end)%positive;
  var_global := var_global
}.

Definition ai_ms_convert (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ms_convert_z <= 0 /\ -1 * s V_ms_convert_z <= 0)%Z
   | 3 => (-1 * s V_ms_convert_z <= 0 /\ 1 * s V_ms_convert_z <= 0 /\ 1 * s V_ms_convert_i <= 0 /\ -1 * s V_ms_convert_i <= 0)%Z
   | 4 => (-1 * s V_ms_convert_i <= 0 /\ 1 * s V_ms_convert_i <= 0 /\ 1 * s V_ms_convert_z <= 0 /\ -1 * s V_ms_convert_z <= 0)%Z
   | 5 => (-1 * s V_ms_convert_z <= 0 /\ -1 * s V_ms_convert_i <= 0 /\ 1 * s V_ms_convert_i + -576 <= 0)%Z
   | 6 => (1 * s V_ms_convert_i + -576 <= 0 /\ -1 * s V_ms_convert_z <= 0 /\ -1 * s V_ms_convert_i + 576 <= 0)%Z
   | 7 => (-1 * s V_ms_convert_i + 576 <= 0 /\ -1 * s V_ms_convert_z <= 0 /\ 1 * s V_ms_convert_i + -576 <= 0)%Z
   | 8 => (-1 * s V_ms_convert_i <= 0 /\ -1 * s V_ms_convert_z <= 0 /\ 1 * s V_ms_convert_i + -575 <= 0)%Z
   | 9 => (1 * s V_ms_convert_i + -575 <= 0 /\ -1 * s V_ms_convert_z <= 0 /\ -1 * s V_ms_convert_i <= 0)%Z
   | 10 => (-1 * s V_ms_convert_i <= 0 /\ -1 * s V_ms_convert_z <= 0 /\ 1 * s V_ms_convert_i + -575 <= 0)%Z
   | 11 => (-1 * s V_ms_convert_z <= 0 /\ -1 * s V_ms_convert_i + 1 <= 0 /\ 1 * s V_ms_convert_i + -576 <= 0)%Z
   | 12 => (1 * s V_ms_convert_i + -576 <= 0 /\ -1 * s V_ms_convert_i + 1 <= 0 /\ -1 * s V_ms_convert_z <= 0)%Z
   | 13 => (-1 * s V_ms_convert_z <= 0 /\ -1 * s V_ms_convert_i + 1 <= 0 /\ 1 * s V_ms_convert_i + -576 <= 0)%Z
   | 14 => (1 * s V_ms_convert_i + -576 <= 0 /\ -1 * s V_ms_convert_i + 1 <= 0 /\ -1 * s V_ms_convert_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ms_convert (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((576 # 1) <= z)%Q
   | 2 => ((576 # 1) + s V_ms_convert_z <= z)%Q
   | 3 => (s V_ms_convert_z + max0(576 - s V_ms_convert_i) <= z)%Q
   | 4 => (s V_ms_convert_z + max0(576 - s V_ms_convert_i) <= z)%Q
   | 5 => (s V_ms_convert_z + max0(576 - s V_ms_convert_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (576 - s V_ms_convert_i) (575
                                                                    - s V_ms_convert_i));
      (*-1 0*) F_max0_ge_0 (575 - s V_ms_convert_i)]
     (s V_ms_convert_z + max0(576 - s V_ms_convert_i) <= z)%Q
   | 7 => (s V_ms_convert_z <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (576 - s V_ms_convert_i) (1)]
     (s V_ms_convert_z + max0(576 - s V_ms_convert_i) <= z)%Q
   | 9 => ((1 # 1) + s V_ms_convert_z + max0(575 - s V_ms_convert_i) <= z)%Q
   | 10 => ((1 # 1) + s V_ms_convert_z + max0(575 - s V_ms_convert_i) <= z)%Q
   | 11 => ((1 # 1) + s V_ms_convert_z + max0(576 - s V_ms_convert_i) <= z)%Q
   | 12 => ((1 # 1) + s V_ms_convert_z + max0(576 - s V_ms_convert_i) <= z)%Q
   | 13 => ((1 # 1) + s V_ms_convert_z + max0(576 - s V_ms_convert_i) <= z)%Q
   | 14 => (s V_ms_convert_z + max0(576 - s V_ms_convert_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ms_convert =>
    [mkPA Q (fun n z s => ai_ms_convert n s /\ annot0_ms_convert n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ms_convert (proc_start P_ms_convert) s1 (proc_end P_ms_convert) s2 ->
    (s2 V_ms_convert_z <= (576 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ms_convert.
Qed.
