Require Import pasta.Pasta.

Inductive proc: Type :=
  P_main1.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_main1_z := 1%positive.
Notation V_main1__tmp := 2%positive.
Notation V_main1__tmp1 := 3%positive.
Notation V_main1_errors := 4%positive.
Notation V_main1_argc := 5%positive.
Notation V_main1_argv := 6%positive.
Notation V_main1_print := 7%positive.
Definition Pedges_main1: list (edge proc) :=
  (EA 1 (AAssign V_main1_z (Some (ENum (0)))) 2)::(EA 2 (AAssign V_main1__tmp
  (Some (EVar V_main1_argc))) 3)::(EA 3 (AAssign V_main1__tmp1
  (Some (EVar V_main1_print))) 4)::(EA 4 (AAssign V_main1_errors
  (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 (AAssign V_main1__tmp
  (Some (EAdd (EVar V_main1__tmp) (ENum (-1))))) 7)::(EA 7 AWeaken 8)::
  (EA 8 (AGuard (fun s => ((eval (EAdd (EVar V_main1__tmp) (ENum (-1))) s) >
  (eval (ENum (0)) s))%Z)) 11)::(EA 8 (AGuard
  (fun s => ((eval (EAdd (EVar V_main1__tmp) (ENum (-1))) s) <=
  (eval (ENum (0)) s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 11 AWeaken 12)::
  (EA 12 (AAssign V_main1_errors None) 13)::(EA 13 AWeaken 14)::
  (EA 14 (AGuard (fun s => ((eval (EVar V_main1__tmp1) s) <> (eval (ENum (0))
  s))%Z)) 16)::(EA 14 (AGuard (fun s => ((eval (EVar V_main1__tmp1) s) =
  (eval (ENum (0)) s))%Z)) 15)::(EA 15 AWeaken 18)::(EA 16 AWeaken 17)::
  (EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_main1_z (Some (EAdd (ENum (1)) (EVar V_main1_z)))) 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_main1 => Pedges_main1
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_main1 => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_main1 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 3 => (-1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0)%Z
   | 4 => (1 * s V_main1_z <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 5 => (-1 * s V_main1_z <= 0 /\ 1 * s V_main1_z <= 0 /\ 1 * s V_main1_errors <= 0 /\ -1 * s V_main1_errors <= 0)%Z
   | 6 => (-1 * s V_main1_z <= 0)%Z
   | 7 => (-1 * s V_main1_z <= 0)%Z
   | 8 => (-1 * s V_main1_z <= 0)%Z
   | 9 => (-1 * s V_main1_z <= 0 /\ 1 * s V_main1__tmp + -1 <= 0)%Z
   | 10 => (1 * s V_main1__tmp + -1 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 11 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | 12 => (-1 * s V_main1__tmp + 2 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 13 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | 14 => (-1 * s V_main1__tmp + 2 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 15 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0 /\ 1 * s V_main1__tmp1 <= 0 /\ -1 * s V_main1__tmp1 <= 0)%Z
   | 16 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | 17 => (-1 * s V_main1__tmp + 2 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 18 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | 19 => (-1 * s V_main1__tmp + 2 <= 0 /\ -1 * s V_main1_z <= 0)%Z
   | 20 => (-1 * s V_main1_z <= 0 /\ -1 * s V_main1__tmp + 2 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_main1 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-2 + s V_main1_argc) <= z)%Q
   | 2 => (s V_main1_z + max0(-2 + s V_main1_argc) <= z)%Q
   | 3 => (s V_main1_z + max0(-2 + s V_main1__tmp) <= z)%Q
   | 4 => (s V_main1_z + max0(-2 + s V_main1__tmp) <= z)%Q
   | 5 => (s V_main1_z + max0(-2 + s V_main1__tmp) <= z)%Q
   | 6 => (s V_main1_z + max0(-2 + s V_main1__tmp) <= z)%Q
   | 7 => (s V_main1_z + max0(-1 + s V_main1__tmp) <= z)%Q
   | 8 => (s V_main1_z + max0(-1 + s V_main1__tmp) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-1 + s V_main1__tmp) (-2
                                                                   + 
                                                                   s V_main1__tmp));
      (*-1 0*) F_max0_ge_0 (-2 + s V_main1__tmp)]
     (s V_main1_z + max0(-1 + s V_main1__tmp) <= z)%Q
   | 10 => (s V_main1_z <= z)%Q
   | 11 => (s V_main1_z + max0(-1 + s V_main1__tmp) <= z)%Q
   | 12 => (s V_main1_z + max0(-1 + s V_main1__tmp) <= z)%Q
   | 13 => hints
     [(*0 1*) F_max0_pre_decrement 1 (-1 + s V_main1__tmp) (1)]
     (s V_main1_z + max0(-1 + s V_main1__tmp) <= z)%Q
   | 14 => ((1 # 1) + s V_main1_z + max0(-2 + s V_main1__tmp) <= z)%Q
   | 15 => ((1 # 1) + s V_main1_z + max0(-2 + s V_main1__tmp) <= z)%Q
   | 16 => ((1 # 1) + s V_main1_z + max0(-2 + s V_main1__tmp) <= z)%Q
   | 17 => ((1 # 1) + s V_main1_z + max0(-2 + s V_main1__tmp) <= z)%Q
   | 18 => ((1 # 1) + s V_main1_z + max0(-2 + s V_main1__tmp) <= z)%Q
   | 19 => ((1 # 1) + s V_main1_z + max0(-2 + s V_main1__tmp) <= z)%Q
   | 20 => ((1 # 1) + s V_main1_z + max0(-2 + s V_main1__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_main1 =>
    [mkPA Q (fun n z s => ai_main1 n s /\ annot0_main1 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_main1 (proc_start P_main1) s1 (proc_end P_main1) s2 ->
    (s2 V_main1_z <= max0(-2 + s1 V_main1_argc))%Q.
Proof.
  prove_bound ipa admissible_ipa P_main1.
Qed.
