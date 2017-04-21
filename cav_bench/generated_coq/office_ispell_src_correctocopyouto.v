Require Import pasta.Pasta.

Inductive proc: Type :=
  P_copyout.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_copyout_z := 1%positive.
Notation V_copyout__tmp := 2%positive.
Notation V_copyout_aflag := 3%positive.
Notation V_copyout_lflag := 4%positive.
Notation V_copyout_cc := 5%positive.
Notation V_copyout_cnt := 6%positive.
Definition Pedges_copyout: list (edge proc) :=
  (EA 1 (AAssign V_copyout_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_copyout__tmp (Some (EVar V_copyout_cnt))) 3)::(EA 3 ANone 4)::
  (EA 4 (AAssign V_copyout__tmp (Some (EAdd (EVar V_copyout__tmp)
  (ENum (-1))))) 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EAdd (EVar V_copyout__tmp) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 8)::(EA 6 (AGuard
  (fun s => ((eval (EAdd (EVar V_copyout__tmp) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 23)::(EA 8 AWeaken 9)::
  (EA 9 ANone 21)::(EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_copyout_aflag) s) <> (eval (ENum (0))
  s))%Z)) 17)::(EA 11 (AGuard (fun s => ((eval (EVar V_copyout_aflag) s) =
  (eval (ENum (0)) s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_copyout_lflag) s) <> (eval (ENum (0))
  s))%Z)) 16)::(EA 13 (AGuard (fun s => ((eval (EVar V_copyout_lflag) s) =
  (eval (ENum (0)) s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 ANone 18)::
  (EA 16 AWeaken 18)::(EA 17 AWeaken 18)::(EA 18 ANone 19)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_copyout_z (Some (EAdd (ENum (1))
  (EVar V_copyout_z)))) 4)::(EA 21 ANone 22)::(EA 22 AWeaken 23)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_copyout => Pedges_copyout
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_copyout => 23
     end)%positive;
  var_global := var_global
}.

Definition ai_copyout (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_copyout_z <= 0 /\ -1 * s V_copyout_z <= 0)%Z
   | 3 => (-1 * s V_copyout_z <= 0 /\ 1 * s V_copyout_z <= 0)%Z
   | 4 => (-1 * s V_copyout_z <= 0)%Z
   | 5 => (-1 * s V_copyout_z <= 0)%Z
   | 6 => (-1 * s V_copyout_z <= 0)%Z
   | 7 => (-1 * s V_copyout_z <= 0 /\ 1 * s V_copyout__tmp <= 0)%Z
   | 8 => (-1 * s V_copyout_z <= 0 /\ -1 * s V_copyout__tmp + 1 <= 0)%Z
   | 9 => (-1 * s V_copyout__tmp + 1 <= 0 /\ -1 * s V_copyout_z <= 0)%Z
   | 10 => (-1 * s V_copyout_z <= 0 /\ -1 * s V_copyout__tmp + 1 <= 0)%Z
   | 11 => (-1 * s V_copyout__tmp + 1 <= 0 /\ -1 * s V_copyout_z <= 0)%Z
   | 12 => (-1 * s V_copyout_z <= 0 /\ -1 * s V_copyout__tmp + 1 <= 0 /\ 1 * s V_copyout_aflag <= 0 /\ -1 * s V_copyout_aflag <= 0)%Z
   | 13 => (-1 * s V_copyout_aflag <= 0 /\ 1 * s V_copyout_aflag <= 0 /\ -1 * s V_copyout__tmp + 1 <= 0 /\ -1 * s V_copyout_z <= 0)%Z
   | 14 => (-1 * s V_copyout_z <= 0 /\ -1 * s V_copyout__tmp + 1 <= 0 /\ 1 * s V_copyout_aflag <= 0 /\ -1 * s V_copyout_aflag <= 0 /\ 1 * s V_copyout_lflag <= 0 /\ -1 * s V_copyout_lflag <= 0)%Z
   | 15 => (-1 * s V_copyout_lflag <= 0 /\ 1 * s V_copyout_lflag <= 0 /\ -1 * s V_copyout_aflag <= 0 /\ 1 * s V_copyout_aflag <= 0 /\ -1 * s V_copyout__tmp + 1 <= 0 /\ -1 * s V_copyout_z <= 0)%Z
   | 16 => (-1 * s V_copyout_z <= 0 /\ -1 * s V_copyout__tmp + 1 <= 0 /\ 1 * s V_copyout_aflag <= 0 /\ -1 * s V_copyout_aflag <= 0)%Z
   | 17 => (-1 * s V_copyout_z <= 0 /\ -1 * s V_copyout__tmp + 1 <= 0)%Z
   | 18 => (-1 * s V_copyout__tmp + 1 <= 0 /\ -1 * s V_copyout_z <= 0)%Z
   | 19 => (-1 * s V_copyout_z <= 0 /\ -1 * s V_copyout__tmp + 1 <= 0)%Z
   | 20 => (-1 * s V_copyout__tmp + 1 <= 0 /\ -1 * s V_copyout_z <= 0)%Z
   | 21 => (-1 * s V_copyout_z <= 0 /\ -1 * s V_copyout__tmp + 1 <= 0)%Z
   | 22 => (-1 * s V_copyout__tmp + 1 <= 0 /\ -1 * s V_copyout_z <= 0)%Z
   | 23 => (-1 * s V_copyout_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_copyout (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_copyout_cnt) <= z)%Q
   | 2 => (s V_copyout_z + max0(-1 + s V_copyout_cnt) <= z)%Q
   | 3 => (s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 4 => (s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 5 => (s V_copyout_z + max0(s V_copyout__tmp) <= z)%Q
   | 6 => (s V_copyout_z + max0(s V_copyout__tmp) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_copyout__tmp) (-1
                                                                + s V_copyout__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_copyout__tmp)]
     (s V_copyout_z + max0(s V_copyout__tmp) <= z)%Q
   | 8 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_copyout__tmp) (1)]
     (s V_copyout_z + max0(s V_copyout__tmp) <= z)%Q
   | 9 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 10 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 11 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 12 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 13 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 14 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 15 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 16 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 17 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 18 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 19 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 20 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 21 => ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_one; (*-1 0*) F_max0_ge_0 (-1 + s V_copyout__tmp)]
     ((1 # 1) + s V_copyout_z + max0(-1 + s V_copyout__tmp) <= z)%Q
   | 23 => (s V_copyout_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_copyout =>
    [mkPA Q (fun n z s => ai_copyout n s /\ annot0_copyout n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_copyout (proc_start P_copyout) s1 (proc_end P_copyout) s2 ->
    (s2 V_copyout_z <= max0(-1 + s1 V_copyout_cnt))%Q.
Proof.
  prove_bound ipa admissible_ipa P_copyout.
Qed.
