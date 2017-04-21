Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mp_addc.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mp_addc_z := 1%positive.
Notation V_mp_addc__tmp := 2%positive.
Notation V_mp_addc_global_precision := 3%positive.
Notation V_mp_addc_precision := 4%positive.
Notation V_mp_addc_x := 5%positive.
Notation V_mp_addc_carry := 6%positive.
Notation V_mp_addc_r1 := 7%positive.
Notation V_mp_addc_r2 := 8%positive.
Definition Pedges_mp_addc: list (edge proc) :=
  (EA 1 (AAssign V_mp_addc_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_mp_addc__tmp (Some (EVar V_mp_addc_carry))) 3)::(EA 3 (AAssign
  V_mp_addc_precision (Some (EVar V_mp_addc_global_precision))) 4)::
  (EA 4 ANone 5)::(EA 5 (AAssign V_mp_addc_precision
  (Some (EAdd (EVar V_mp_addc_precision) (ENum (-1))))) 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard (fun s => ((eval (EVar V_mp_addc_precision)
  s) <> (eval (ENum (0)) s))%Z)) 10)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_mp_addc_precision) s) = (eval (ENum (0))
  s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_mp_addc__tmp) s) <> (eval (ENum (0)) s))%Z)) 16)::
  (EA 11 (AGuard (fun s => ((eval (EVar V_mp_addc__tmp) s) = (eval (ENum (0))
  s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 (AAssign V_mp_addc_x None) 14)::
  (EA 14 (AAssign V_mp_addc__tmp None) 15)::(EA 15 ANone 20)::
  (EA 16 AWeaken 17)::(EA 17 (AAssign V_mp_addc_x None) 18)::(EA 18 (AAssign
  V_mp_addc__tmp None) 19)::(EA 19 ANone 20)::(EA 20 ANone 21)::
  (EA 21 ANone 22)::(EA 22 (AAssign V_mp_addc_z (Some (EAdd (ENum (1))
  (EVar V_mp_addc_z)))) 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mp_addc => Pedges_mp_addc
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mp_addc => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_mp_addc (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mp_addc_z <= 0 /\ -1 * s V_mp_addc_z <= 0)%Z
   | 3 => (-1 * s V_mp_addc_z <= 0 /\ 1 * s V_mp_addc_z <= 0)%Z
   | 4 => (1 * s V_mp_addc_z <= 0 /\ -1 * s V_mp_addc_z <= 0)%Z
   | 5 => (-1 * s V_mp_addc_z <= 0)%Z
   | 6 => (-1 * s V_mp_addc_z <= 0)%Z
   | 7 => (-1 * s V_mp_addc_z <= 0)%Z
   | 8 => (-1 * s V_mp_addc_z <= 0 /\ 1 * s V_mp_addc_precision <= 0 /\ -1 * s V_mp_addc_precision <= 0)%Z
   | 9 => (-1 * s V_mp_addc_precision <= 0 /\ 1 * s V_mp_addc_precision <= 0 /\ -1 * s V_mp_addc_z <= 0)%Z
   | 10 => (-1 * s V_mp_addc_z <= 0)%Z
   | 11 => (-1 * s V_mp_addc_z <= 0)%Z
   | 12 => (-1 * s V_mp_addc_z <= 0 /\ 1 * s V_mp_addc__tmp <= 0 /\ -1 * s V_mp_addc__tmp <= 0)%Z
   | 13 => (-1 * s V_mp_addc__tmp <= 0 /\ 1 * s V_mp_addc__tmp <= 0 /\ -1 * s V_mp_addc_z <= 0)%Z
   | 14 => (-1 * s V_mp_addc_z <= 0 /\ 1 * s V_mp_addc__tmp <= 0 /\ -1 * s V_mp_addc__tmp <= 0)%Z
   | 15 => (-1 * s V_mp_addc_z <= 0)%Z
   | 16 => (-1 * s V_mp_addc_z <= 0)%Z
   | 17 => (-1 * s V_mp_addc_z <= 0)%Z
   | 18 => (-1 * s V_mp_addc_z <= 0)%Z
   | 19 => (-1 * s V_mp_addc_z <= 0)%Z
   | 20 => (-1 * s V_mp_addc_z <= 0)%Z
   | 21 => (-1 * s V_mp_addc_z <= 0)%Z
   | 22 => (-1 * s V_mp_addc_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mp_addc (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_mp_addc_global_precision <= z)%Q
   | 2 => (s V_mp_addc_global_precision + s V_mp_addc_z <= z)%Q
   | 3 => (s V_mp_addc_global_precision + s V_mp_addc_z <= z)%Q
   | 4 => (s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 5 => (s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 6 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 7 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 8 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_mp_addc_precision)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_mp_addc_precision) (0))) (F_max0_ge_0 (s V_mp_addc_precision))]
     ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 9 => (s V_mp_addc_z <= z)%Q
   | 10 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 11 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 12 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 13 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 14 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 15 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 16 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 17 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 18 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 19 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 20 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 21 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | 22 => ((1 # 1) + s V_mp_addc_precision + s V_mp_addc_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mp_addc =>
    [mkPA Q (fun n z s => ai_mp_addc n s /\ annot0_mp_addc n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mp_addc (proc_start P_mp_addc) s1 (proc_end P_mp_addc) s2 ->
    (s2 V_mp_addc_z <= s1 V_mp_addc_global_precision)%Q.
Proof.
  prove_bound ipa admissible_ipa P_mp_addc.
Qed.
