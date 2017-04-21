Require Import pasta.Pasta.

Inductive proc: Type :=
  P_Postprocessing.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_Postprocessing_z := 1%positive.
Notation V_Postprocessing_S_dref_off650 := 2%positive.
Notation V_Postprocessing_k := 3%positive.
Notation V_Postprocessing_ltmp := 4%positive.
Notation V_Postprocessing_msr := 5%positive.
Notation V_Postprocessing_tmp := 6%positive.
Notation V_Postprocessing_S := 7%positive.
Notation V_Postprocessing_s := 8%positive.
Definition Pedges_Postprocessing: list (edge proc) :=
  (EA 1 (AAssign V_Postprocessing_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_Postprocessing_msr (Some (EVar V_Postprocessing_S_dref_off650))) 3)::
  (EA 3 (AAssign V_Postprocessing_k (Some (ENum (160)))) 4)::(EA 4 ANone 5)::
  (EA 5 (AAssign V_Postprocessing_k (Some (EAdd (EVar V_Postprocessing_k)
  (ENum (-1))))) 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_Postprocessing_k) s) <> (eval (ENum (0))
  s))%Z)) 12)::(EA 7 (AGuard (fun s => ((eval (EVar V_Postprocessing_k) s) =
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 (AAssign
  V_Postprocessing_S_dref_off650 (Some (EVar V_Postprocessing_msr))) 10)::
  (EA 10 AWeaken 11)::(EA 12 AWeaken 13)::(EA 13 (AAssign
  V_Postprocessing_tmp None) 14)::(EA 14 (AAssign V_Postprocessing_ltmp
  None) 15)::(EA 15 AWeaken 16)::(EA 16 ANone 18)::(EA 16 ANone 17)::
  (EA 17 ANone 19)::(EA 18 ANone 19)::(EA 19 (AAssign V_Postprocessing_msr
  None) 20)::(EA 20 (AAssign V_Postprocessing_ltmp
  (Some (EAdd (EVar V_Postprocessing_msr)
  (EVar V_Postprocessing_msr)))) 21)::(EA 21 AWeaken 22)::(EA 22 ANone 24)::
  (EA 22 ANone 23)::(EA 23 ANone 25)::(EA 24 ANone 25)::(EA 25 ANone 26)::
  (EA 26 ANone 27)::(EA 27 ANone 28)::(EA 28 (AAssign V_Postprocessing_z
  (Some (EAdd (ENum (1)) (EVar V_Postprocessing_z)))) 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_Postprocessing => Pedges_Postprocessing
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_Postprocessing => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_Postprocessing (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_Postprocessing_z <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 3 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_z <= 0)%Z
   | 4 => (1 * s V_Postprocessing_z <= 0 /\ -1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k + -160 <= 0 /\ -1 * s V_Postprocessing_k + 160 <= 0)%Z
   | 5 => (1 * s V_Postprocessing_k + -160 <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 6 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k + -159 <= 0)%Z
   | 7 => (1 * s V_Postprocessing_k + -159 <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 8 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k <= 0 /\ -1 * s V_Postprocessing_k <= 0)%Z
   | 9 => (-1 * s V_Postprocessing_k <= 0 /\ 1 * s V_Postprocessing_k <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 10 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k <= 0 /\ -1 * s V_Postprocessing_k <= 0)%Z
   | 11 => (-1 * s V_Postprocessing_k <= 0 /\ 1 * s V_Postprocessing_k <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 12 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k + -159 <= 0)%Z
   | 13 => (1 * s V_Postprocessing_k + -159 <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 14 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k + -159 <= 0)%Z
   | 15 => (1 * s V_Postprocessing_k + -159 <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 16 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k + -159 <= 0)%Z
   | 17 => (1 * s V_Postprocessing_k + -159 <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 18 => (1 * s V_Postprocessing_k + -159 <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 19 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k + -159 <= 0)%Z
   | 20 => (1 * s V_Postprocessing_k + -159 <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 21 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k + -159 <= 0)%Z
   | 22 => (1 * s V_Postprocessing_k + -159 <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 23 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k + -159 <= 0)%Z
   | 24 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k + -159 <= 0)%Z
   | 25 => (1 * s V_Postprocessing_k + -159 <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 26 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k + -159 <= 0)%Z
   | 27 => (1 * s V_Postprocessing_k + -159 <= 0 /\ -1 * s V_Postprocessing_z <= 0)%Z
   | 28 => (-1 * s V_Postprocessing_z <= 0 /\ 1 * s V_Postprocessing_k + -159 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_Postprocessing (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((159 # 1) <= z)%Q
   | 2 => ((159 # 1) + s V_Postprocessing_z <= z)%Q
   | 3 => ((159 # 1) + s V_Postprocessing_z <= z)%Q
   | 4 => (-(1 # 1) + s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 5 => (-(1 # 1) + s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 6 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 7 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 8 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 9 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 10 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_Postprocessing_k)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_Postprocessing_k) (0))) (F_max0_ge_0 (s V_Postprocessing_k))]
     (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 11 => (s V_Postprocessing_z <= z)%Q
   | 12 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 13 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 14 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 15 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 16 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 17 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 18 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 19 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 20 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 21 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 22 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 23 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 24 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 25 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 26 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 27 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | 28 => (s V_Postprocessing_k + s V_Postprocessing_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_Postprocessing =>
    [mkPA Q (fun n z s => ai_Postprocessing n s /\ annot0_Postprocessing n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_Postprocessing (proc_start P_Postprocessing) s1 (proc_end P_Postprocessing) s2 ->
    (s2 V_Postprocessing_z <= (159 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_Postprocessing.
Qed.
