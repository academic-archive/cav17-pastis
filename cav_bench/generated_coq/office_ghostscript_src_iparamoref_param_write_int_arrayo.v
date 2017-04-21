Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ref_param_write_int_array.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ref_param_write_int_array_z := 1%positive.
Notation V_ref_param_write_int_array__tmp := 2%positive.
Notation V_ref_param_write_int_array_code := 3%positive.
Notation V_ref_param_write_int_array_n := 4%positive.
Notation V_ref_param_write_int_array_pvalue_dref_off8 := 5%positive.
Notation V_ref_param_write_int_array_pkey := 6%positive.
Notation V_ref_param_write_int_array_plist := 7%positive.
Notation V_ref_param_write_int_array_pvalue := 8%positive.
Definition Pedges_ref_param_write_int_array: list (edge proc) :=
  (EA 1 (AAssign V_ref_param_write_int_array_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_ref_param_write_int_array_n) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign
  V_ref_param_write_int_array_n
  (Some (EVar V_ref_param_write_int_array_pvalue_dref_off8))) 5)::
  (EA 5 (AAssign V_ref_param_write_int_array_code None) 6)::
  (EA 6 AWeaken 7)::(EA 7 ANone 22)::(EA 7 ANone 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_ref_param_write_int_array_n) s) > (eval (ENum (0))
  s))%Z)) 15)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_ref_param_write_int_array_n) s) <=
  (eval (ENum (0)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_ref_param_write_int_array__tmp None) 13)::(EA 13 ANone 14)::
  (EA 14 AWeaken 25)::(EA 15 AWeaken 16)::(EA 16 ANone 17)::(EA 17 (AAssign
  V_ref_param_write_int_array_n
  (Some (EAdd (EVar V_ref_param_write_int_array_n) (ENum (-1))))) 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_ref_param_write_int_array_z (Some (EAdd (ENum (1))
  (EVar V_ref_param_write_int_array_z)))) 21)::(EA 21 AWeaken 10)::
  (EA 22 (AAssign V_ref_param_write_int_array__tmp
  (Some (EVar V_ref_param_write_int_array_code))) 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 25)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ref_param_write_int_array => Pedges_ref_param_write_int_array
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ref_param_write_int_array => 25
     end)%positive;
  var_global := var_global
}.

Definition ai_ref_param_write_int_array (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ref_param_write_int_array_z <= 0 /\ -1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 3 => (-1 * s V_ref_param_write_int_array_z <= 0 /\ 1 * s V_ref_param_write_int_array_z <= 0 /\ -1 * s V_ref_param_write_int_array_n <= 0)%Z
   | 4 => (-1 * s V_ref_param_write_int_array_n <= 0 /\ 1 * s V_ref_param_write_int_array_z <= 0 /\ -1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 5 => (-1 * s V_ref_param_write_int_array_z <= 0 /\ 1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 6 => (1 * s V_ref_param_write_int_array_z <= 0 /\ -1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 7 => (-1 * s V_ref_param_write_int_array_z <= 0 /\ 1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 8 => (1 * s V_ref_param_write_int_array_z <= 0 /\ -1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 9 => (-1 * s V_ref_param_write_int_array_z <= 0 /\ 1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 10 => (-1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 11 => (-1 * s V_ref_param_write_int_array_z <= 0 /\ 1 * s V_ref_param_write_int_array_n <= 0)%Z
   | 12 => (1 * s V_ref_param_write_int_array_n <= 0 /\ -1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 13 => (-1 * s V_ref_param_write_int_array_z <= 0 /\ 1 * s V_ref_param_write_int_array_n <= 0)%Z
   | 14 => (1 * s V_ref_param_write_int_array_n <= 0 /\ -1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 15 => (-1 * s V_ref_param_write_int_array_z <= 0 /\ -1 * s V_ref_param_write_int_array_n + 1 <= 0)%Z
   | 16 => (-1 * s V_ref_param_write_int_array_n + 1 <= 0 /\ -1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 17 => (-1 * s V_ref_param_write_int_array_z <= 0 /\ -1 * s V_ref_param_write_int_array_n + 1 <= 0)%Z
   | 18 => (-1 * s V_ref_param_write_int_array_z <= 0 /\ -1 * s V_ref_param_write_int_array_n <= 0)%Z
   | 19 => (-1 * s V_ref_param_write_int_array_n <= 0 /\ -1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 20 => (-1 * s V_ref_param_write_int_array_z <= 0 /\ -1 * s V_ref_param_write_int_array_n <= 0)%Z
   | 21 => (-1 * s V_ref_param_write_int_array_n <= 0 /\ -1 * s V_ref_param_write_int_array_z + 1 <= 0)%Z
   | 22 => (1 * s V_ref_param_write_int_array_z <= 0 /\ -1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 23 => (-1 * s V_ref_param_write_int_array_z <= 0 /\ 1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 24 => (1 * s V_ref_param_write_int_array_z <= 0 /\ -1 * s V_ref_param_write_int_array_z <= 0)%Z
   | 25 => (-1 * s V_ref_param_write_int_array_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ref_param_write_int_array (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_ref_param_write_int_array_pvalue_dref_off8) <= z)%Q
   | 2 => (s V_ref_param_write_int_array_z
           + max0(s V_ref_param_write_int_array_pvalue_dref_off8) <= z)%Q
   | 3 => (s V_ref_param_write_int_array_z
           + max0(s V_ref_param_write_int_array_pvalue_dref_off8) <= z)%Q
   | 4 => (s V_ref_param_write_int_array_z
           + max0(s V_ref_param_write_int_array_pvalue_dref_off8) <= z)%Q
   | 5 => (s V_ref_param_write_int_array_z
           + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 6 => (s V_ref_param_write_int_array_z
           + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 7 => (s V_ref_param_write_int_array_z
           + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 8 => (s V_ref_param_write_int_array_z
           + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 9 => (s V_ref_param_write_int_array_z
           + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 10 => (s V_ref_param_write_int_array_z
            + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 11 => (s V_ref_param_write_int_array_z
            + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 12 => (s V_ref_param_write_int_array_z
            + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 13 => (s V_ref_param_write_int_array_z
            + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_ref_param_write_int_array_n) (-1
                                                                    + s V_ref_param_write_int_array_n));
      (*-1 0*) F_max0_ge_0 (-1 + s V_ref_param_write_int_array_n)]
     (s V_ref_param_write_int_array_z + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_ref_param_write_int_array_n) (1)]
     (s V_ref_param_write_int_array_z + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 16 => ((1 # 1) + s V_ref_param_write_int_array_z
            + max0(-1 + s V_ref_param_write_int_array_n) <= z)%Q
   | 17 => ((1 # 1) + s V_ref_param_write_int_array_z
            + max0(-1 + s V_ref_param_write_int_array_n) <= z)%Q
   | 18 => ((1 # 1) + s V_ref_param_write_int_array_z
            + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 19 => ((1 # 1) + s V_ref_param_write_int_array_z
            + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 20 => ((1 # 1) + s V_ref_param_write_int_array_z
            + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 21 => (s V_ref_param_write_int_array_z
            + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 22 => (s V_ref_param_write_int_array_z
            + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 23 => (s V_ref_param_write_int_array_z
            + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_ref_param_write_int_array_n) (-1
                                                                    + s V_ref_param_write_int_array_n));
      (*-1 0*) F_max0_ge_0 (-1 + s V_ref_param_write_int_array_n)]
     (s V_ref_param_write_int_array_z + max0(s V_ref_param_write_int_array_n) <= z)%Q
   | 25 => (s V_ref_param_write_int_array_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ref_param_write_int_array =>
    [mkPA Q (fun n z s => ai_ref_param_write_int_array n s /\ annot0_ref_param_write_int_array n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ref_param_write_int_array (proc_start P_ref_param_write_int_array) s1 (proc_end P_ref_param_write_int_array) s2 ->
    (s2 V_ref_param_write_int_array_z <= max0(s1 V_ref_param_write_int_array_pvalue_dref_off8))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ref_param_write_int_array.
Qed.
