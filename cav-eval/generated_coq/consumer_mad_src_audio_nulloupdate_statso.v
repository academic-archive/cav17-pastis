Require Import pasta.Pasta.

Inductive proc: Type :=
  P_update_stats.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_update_stats_z := 1%positive.
Notation V_update_stats__tmp := 2%positive.
Notation V_update_stats_stats_dref_off12 := 3%positive.
Notation V_update_stats_stats_dref_off8 := 4%positive.
Notation V_update_stats_nsamples := 5%positive.
Notation V_update_stats_sample := 6%positive.
Notation V_update_stats_stats := 7%positive.
Definition Pedges_update_stats: list (edge proc) :=
  (EA 1 (AAssign V_update_stats_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_update_stats__tmp (Some (EVar V_update_stats_nsamples))) 3)::
  (EA 3 ANone 4)::(EA 4 (AAssign V_update_stats__tmp
  (Some (EAdd (EVar V_update_stats__tmp) (ENum (-1))))) 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard (fun s => ((eval (EVar V_update_stats__tmp)
  s) <> (eval (ENum (0)) s))%Z)) 9)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_update_stats__tmp) s) = (eval (ENum (0))
  s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::(EA 10 ANone 22)::
  (EA 10 ANone 11)::(EA 11 AWeaken 12)::(EA 12 ANone 13)::(EA 12 ANone 21)::
  (EA 13 (AAssign V_update_stats_stats_dref_off12 None) 14)::
  (EA 14 AWeaken 15)::(EA 15 ANone 16)::(EA 15 ANone 20)::
  (EA 16 AWeaken 17)::(EA 17 ANone 18)::(EA 17 ANone 20)::(EA 18 (AAssign
  V_update_stats_stats_dref_off8 None) 19)::(EA 19 ANone 20)::
  (EA 20 ANone 21)::(EA 21 ANone 30)::(EA 22 (AAssign
  V_update_stats_stats_dref_off12 None) 23)::(EA 23 AWeaken 24)::
  (EA 24 ANone 25)::(EA 24 ANone 29)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::
  (EA 26 ANone 29)::(EA 27 (AAssign V_update_stats_stats_dref_off8
  None) 28)::(EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 ANone 31)::
  (EA 31 ANone 32)::(EA 32 (AAssign V_update_stats_z (Some (EAdd (ENum (1))
  (EVar V_update_stats_z)))) 4)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_update_stats => Pedges_update_stats
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_update_stats => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_update_stats (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_update_stats_z <= 0 /\ -1 * s V_update_stats_z <= 0)%Z
   | 3 => (-1 * s V_update_stats_z <= 0 /\ 1 * s V_update_stats_z <= 0)%Z
   | 4 => (-1 * s V_update_stats_z <= 0)%Z
   | 5 => (-1 * s V_update_stats_z <= 0)%Z
   | 6 => (-1 * s V_update_stats_z <= 0)%Z
   | 7 => (-1 * s V_update_stats_z <= 0 /\ 1 * s V_update_stats__tmp <= 0 /\ -1 * s V_update_stats__tmp <= 0)%Z
   | 8 => (-1 * s V_update_stats__tmp <= 0 /\ 1 * s V_update_stats__tmp <= 0 /\ -1 * s V_update_stats_z <= 0)%Z
   | 9 => (-1 * s V_update_stats_z <= 0)%Z
   | 10 => (-1 * s V_update_stats_z <= 0)%Z
   | 11 => (-1 * s V_update_stats_z <= 0)%Z
   | 12 => (-1 * s V_update_stats_z <= 0)%Z
   | 13 => (-1 * s V_update_stats_z <= 0)%Z
   | 14 => (-1 * s V_update_stats_z <= 0)%Z
   | 15 => (-1 * s V_update_stats_z <= 0)%Z
   | 16 => (-1 * s V_update_stats_z <= 0)%Z
   | 17 => (-1 * s V_update_stats_z <= 0)%Z
   | 18 => (-1 * s V_update_stats_z <= 0)%Z
   | 19 => (-1 * s V_update_stats_z <= 0)%Z
   | 20 => (-1 * s V_update_stats_z <= 0)%Z
   | 21 => (-1 * s V_update_stats_z <= 0)%Z
   | 22 => (-1 * s V_update_stats_z <= 0)%Z
   | 23 => (-1 * s V_update_stats_z <= 0)%Z
   | 24 => (-1 * s V_update_stats_z <= 0)%Z
   | 25 => (-1 * s V_update_stats_z <= 0)%Z
   | 26 => (-1 * s V_update_stats_z <= 0)%Z
   | 27 => (-1 * s V_update_stats_z <= 0)%Z
   | 28 => (-1 * s V_update_stats_z <= 0)%Z
   | 29 => (-1 * s V_update_stats_z <= 0)%Z
   | 30 => (-1 * s V_update_stats_z <= 0)%Z
   | 31 => (-1 * s V_update_stats_z <= 0)%Z
   | 32 => (-1 * s V_update_stats_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_update_stats (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_update_stats_nsamples <= z)%Q
   | 2 => (s V_update_stats_nsamples + s V_update_stats_z <= z)%Q
   | 3 => (s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 4 => (s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 5 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 6 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 7 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_update_stats__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_update_stats__tmp) (0))) (F_max0_ge_0 (s V_update_stats__tmp))]
     ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 8 => (s V_update_stats_z <= z)%Q
   | 9 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 10 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 11 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 12 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 13 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 14 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 15 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 16 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 17 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 18 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 19 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 20 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 21 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 22 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 23 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 24 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 25 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 26 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 27 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 28 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 29 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 30 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 31 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | 32 => ((1 # 1) + s V_update_stats__tmp + s V_update_stats_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_update_stats =>
    [mkPA Q (fun n z s => ai_update_stats n s /\ annot0_update_stats n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_update_stats (proc_start P_update_stats) s1 (proc_end P_update_stats) s2 ->
    (s2 V_update_stats_z <= s1 V_update_stats_nsamples)%Q.
Proof.
  prove_bound ipa admissible_ipa P_update_stats.
Qed.
