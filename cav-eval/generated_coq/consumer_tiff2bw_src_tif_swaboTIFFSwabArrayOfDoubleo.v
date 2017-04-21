Require Import pasta.Pasta.

Inductive proc: Type :=
  P_TIFFSwabArrayOfDouble.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_TIFFSwabArrayOfDouble_z := 1%positive.
Notation V_TIFFSwabArrayOfDouble__tmp := 2%positive.
Notation V_TIFFSwabArrayOfDouble_t := 3%positive.
Notation V_TIFFSwabArrayOfDouble_dp := 4%positive.
Notation V_TIFFSwabArrayOfDouble_n := 5%positive.
Definition Pedges_TIFFSwabArrayOfDouble: list (edge proc) :=
  (EA 1 (AAssign V_TIFFSwabArrayOfDouble_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_TIFFSwabArrayOfDouble__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign
  V_TIFFSwabArrayOfDouble__tmp (Some (EVar V_TIFFSwabArrayOfDouble_n))) 5)::
  (EA 5 ANone 6)::(EA 6 (AAssign V_TIFFSwabArrayOfDouble__tmp
  (Some (EAdd (EVar V_TIFFSwabArrayOfDouble__tmp) (ENum (-1))))) 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_TIFFSwabArrayOfDouble__tmp) s) > (eval (ENum (0))
  s))%Z)) 11)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_TIFFSwabArrayOfDouble__tmp) s) <= (eval (ENum (0))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_TIFFSwabArrayOfDouble_t None) 13)::(EA 13 ANone 14)::(EA 14 ANone 15)::
  (EA 15 (AAssign V_TIFFSwabArrayOfDouble_z (Some (EAdd (ENum (1))
  (EVar V_TIFFSwabArrayOfDouble_z)))) 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_TIFFSwabArrayOfDouble => Pedges_TIFFSwabArrayOfDouble
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_TIFFSwabArrayOfDouble => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_TIFFSwabArrayOfDouble (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_TIFFSwabArrayOfDouble_z <= 0 /\ -1 * s V_TIFFSwabArrayOfDouble_z <= 0)%Z
   | 3 => (-1 * s V_TIFFSwabArrayOfDouble_z <= 0 /\ 1 * s V_TIFFSwabArrayOfDouble_z <= 0 /\ -1 * s V_TIFFSwabArrayOfDouble__tmp <= 0)%Z
   | 4 => (-1 * s V_TIFFSwabArrayOfDouble__tmp <= 0 /\ 1 * s V_TIFFSwabArrayOfDouble_z <= 0 /\ -1 * s V_TIFFSwabArrayOfDouble_z <= 0)%Z
   | 5 => (-1 * s V_TIFFSwabArrayOfDouble_z <= 0 /\ 1 * s V_TIFFSwabArrayOfDouble_z <= 0)%Z
   | 6 => (-1 * s V_TIFFSwabArrayOfDouble_z <= 0)%Z
   | 7 => (-1 * s V_TIFFSwabArrayOfDouble_z <= 0)%Z
   | 8 => (-1 * s V_TIFFSwabArrayOfDouble_z <= 0)%Z
   | 9 => (-1 * s V_TIFFSwabArrayOfDouble_z <= 0 /\ 1 * s V_TIFFSwabArrayOfDouble__tmp <= 0)%Z
   | 10 => (1 * s V_TIFFSwabArrayOfDouble__tmp <= 0 /\ -1 * s V_TIFFSwabArrayOfDouble_z <= 0)%Z
   | 11 => (-1 * s V_TIFFSwabArrayOfDouble_z <= 0 /\ -1 * s V_TIFFSwabArrayOfDouble__tmp + 1 <= 0)%Z
   | 12 => (-1 * s V_TIFFSwabArrayOfDouble__tmp + 1 <= 0 /\ -1 * s V_TIFFSwabArrayOfDouble_z <= 0)%Z
   | 13 => (-1 * s V_TIFFSwabArrayOfDouble_z <= 0 /\ -1 * s V_TIFFSwabArrayOfDouble__tmp + 1 <= 0)%Z
   | 14 => (-1 * s V_TIFFSwabArrayOfDouble__tmp + 1 <= 0 /\ -1 * s V_TIFFSwabArrayOfDouble_z <= 0)%Z
   | 15 => (-1 * s V_TIFFSwabArrayOfDouble_z <= 0 /\ -1 * s V_TIFFSwabArrayOfDouble__tmp + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_TIFFSwabArrayOfDouble (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_TIFFSwabArrayOfDouble_n) <= z)%Q
   | 2 => (s V_TIFFSwabArrayOfDouble_z
           + max0(-1 + s V_TIFFSwabArrayOfDouble_n) <= z)%Q
   | 3 => (s V_TIFFSwabArrayOfDouble_z
           + max0(-1 + s V_TIFFSwabArrayOfDouble_n) <= z)%Q
   | 4 => (s V_TIFFSwabArrayOfDouble_z
           + max0(-1 + s V_TIFFSwabArrayOfDouble_n) <= z)%Q
   | 5 => (s V_TIFFSwabArrayOfDouble_z
           + max0(-1 + s V_TIFFSwabArrayOfDouble__tmp) <= z)%Q
   | 6 => (s V_TIFFSwabArrayOfDouble_z
           + max0(-1 + s V_TIFFSwabArrayOfDouble__tmp) <= z)%Q
   | 7 => (s V_TIFFSwabArrayOfDouble_z + max0(s V_TIFFSwabArrayOfDouble__tmp) <= z)%Q
   | 8 => (s V_TIFFSwabArrayOfDouble_z + max0(s V_TIFFSwabArrayOfDouble__tmp) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFSwabArrayOfDouble__tmp) (-1
                                                                    + s V_TIFFSwabArrayOfDouble__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFSwabArrayOfDouble__tmp)]
     (s V_TIFFSwabArrayOfDouble_z + max0(s V_TIFFSwabArrayOfDouble__tmp) <= z)%Q
   | 10 => (s V_TIFFSwabArrayOfDouble_z <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_TIFFSwabArrayOfDouble__tmp) (1)]
     (s V_TIFFSwabArrayOfDouble_z + max0(s V_TIFFSwabArrayOfDouble__tmp) <= z)%Q
   | 12 => ((1 # 1) + s V_TIFFSwabArrayOfDouble_z
            + max0(-1 + s V_TIFFSwabArrayOfDouble__tmp) <= z)%Q
   | 13 => ((1 # 1) + s V_TIFFSwabArrayOfDouble_z
            + max0(-1 + s V_TIFFSwabArrayOfDouble__tmp) <= z)%Q
   | 14 => ((1 # 1) + s V_TIFFSwabArrayOfDouble_z
            + max0(-1 + s V_TIFFSwabArrayOfDouble__tmp) <= z)%Q
   | 15 => ((1 # 1) + s V_TIFFSwabArrayOfDouble_z
            + max0(-1 + s V_TIFFSwabArrayOfDouble__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_TIFFSwabArrayOfDouble =>
    [mkPA Q (fun n z s => ai_TIFFSwabArrayOfDouble n s /\ annot0_TIFFSwabArrayOfDouble n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_TIFFSwabArrayOfDouble (proc_start P_TIFFSwabArrayOfDouble) s1 (proc_end P_TIFFSwabArrayOfDouble) s2 ->
    (s2 V_TIFFSwabArrayOfDouble_z <= max0(-1 + s1 V_TIFFSwabArrayOfDouble_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_TIFFSwabArrayOfDouble.
Qed.
