Require Import pasta.Pasta.

Inductive proc: Type :=
  P_NRL.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_NRL_z := 1%positive.
Notation V_NRL__tmp := 2%positive.
Notation V_NRL_ch := 3%positive.
Notation V_NRL_old := 4%positive.
Notation V_NRL_n := 5%positive.
Notation V_NRL_phone := 6%positive.
Notation V_NRL_s := 7%positive.
Definition Pedges_NRL: list (edge proc) :=
  (EA 1 (AAssign V_NRL_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_NRL__tmp) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 AWeaken 4)::(EA 4 (AAssign V_NRL__tmp (Some (EVar V_NRL_n))) 5)::
  (EA 5 (AAssign V_NRL_old None) 6)::(EA 6 ANone 7)::(EA 7 (AAssign
  V_NRL__tmp (Some (EAdd (EVar V_NRL__tmp) (ENum (-1))))) 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard (fun s => ((eval (EVar V_NRL__tmp) s) >
  (eval (ENum (0)) s))%Z)) 12)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_NRL__tmp) s) <= (eval (ENum (0)) s))%Z)) 10)::
  (EA 10 AWeaken 11)::(EA 12 AWeaken 13)::(EA 13 (AAssign V_NRL_ch
  None) 14)::(EA 14 AWeaken 15)::(EA 15 ANone 16)::(EA 15 ANone 18)::
  (EA 16 (AAssign V_NRL_ch None) 17)::(EA 17 ANone 18)::(EA 18 ANone 19)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_NRL_z (Some (EAdd (ENum (1))
  (EVar V_NRL_z)))) 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_NRL => Pedges_NRL
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_NRL => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_NRL (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_NRL_z <= 0 /\ -1 * s V_NRL_z <= 0)%Z
   | 3 => (-1 * s V_NRL_z <= 0 /\ 1 * s V_NRL_z <= 0 /\ -1 * s V_NRL__tmp <= 0)%Z
   | 4 => (-1 * s V_NRL__tmp <= 0 /\ 1 * s V_NRL_z <= 0 /\ -1 * s V_NRL_z <= 0)%Z
   | 5 => (-1 * s V_NRL_z <= 0 /\ 1 * s V_NRL_z <= 0)%Z
   | 6 => (1 * s V_NRL_z <= 0 /\ -1 * s V_NRL_z <= 0)%Z
   | 7 => (-1 * s V_NRL_z <= 0)%Z
   | 8 => (-1 * s V_NRL_z <= 0)%Z
   | 9 => (-1 * s V_NRL_z <= 0)%Z
   | 10 => (-1 * s V_NRL_z <= 0 /\ 1 * s V_NRL__tmp <= 0)%Z
   | 11 => (1 * s V_NRL__tmp <= 0 /\ -1 * s V_NRL_z <= 0)%Z
   | 12 => (-1 * s V_NRL_z <= 0 /\ -1 * s V_NRL__tmp + 1 <= 0)%Z
   | 13 => (-1 * s V_NRL__tmp + 1 <= 0 /\ -1 * s V_NRL_z <= 0)%Z
   | 14 => (-1 * s V_NRL_z <= 0 /\ -1 * s V_NRL__tmp + 1 <= 0)%Z
   | 15 => (-1 * s V_NRL__tmp + 1 <= 0 /\ -1 * s V_NRL_z <= 0)%Z
   | 16 => (-1 * s V_NRL_z <= 0 /\ -1 * s V_NRL__tmp + 1 <= 0)%Z
   | 17 => (-1 * s V_NRL__tmp + 1 <= 0 /\ -1 * s V_NRL_z <= 0)%Z
   | 18 => (-1 * s V_NRL_z <= 0 /\ -1 * s V_NRL__tmp + 1 <= 0)%Z
   | 19 => (-1 * s V_NRL__tmp + 1 <= 0 /\ -1 * s V_NRL_z <= 0)%Z
   | 20 => (-1 * s V_NRL_z <= 0 /\ -1 * s V_NRL__tmp + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_NRL (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_NRL_n) <= z)%Q
   | 2 => (s V_NRL_z + max0(-1 + s V_NRL_n) <= z)%Q
   | 3 => (s V_NRL_z + max0(-1 + s V_NRL_n) <= z)%Q
   | 4 => (s V_NRL_z + max0(-1 + s V_NRL_n) <= z)%Q
   | 5 => (s V_NRL_z + max0(-1 + s V_NRL__tmp) <= z)%Q
   | 6 => (s V_NRL_z + max0(-1 + s V_NRL__tmp) <= z)%Q
   | 7 => (s V_NRL_z + max0(-1 + s V_NRL__tmp) <= z)%Q
   | 8 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_NRL_z) (0))) (F_max0_ge_0 (s V_NRL_z))]
     (s V_NRL_z + max0(s V_NRL__tmp) <= z)%Q
   | 9 => (max0(s V_NRL__tmp) + max0(s V_NRL_z) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_NRL__tmp) (-1 + s V_NRL__tmp));
      (*-1 0*) F_max0_ge_0 (-1 + s V_NRL__tmp);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_NRL_z)) (F_check_ge (s V_NRL_z) (0))]
     (max0(s V_NRL__tmp) + max0(s V_NRL_z) <= z)%Q
   | 11 => (s V_NRL_z <= z)%Q
   | 12 => (max0(s V_NRL__tmp) + max0(s V_NRL_z) <= z)%Q
   | 13 => (max0(s V_NRL__tmp) + max0(s V_NRL_z) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_NRL__tmp) (1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_NRL_z)) (F_check_ge (s V_NRL_z) (0))]
     (max0(s V_NRL__tmp) + max0(s V_NRL_z) <= z)%Q
   | 15 => ((1 # 1) + s V_NRL_z + max0(-1 + s V_NRL__tmp) <= z)%Q
   | 16 => ((1 # 1) + s V_NRL_z + max0(-1 + s V_NRL__tmp) <= z)%Q
   | 17 => ((1 # 1) + s V_NRL_z + max0(-1 + s V_NRL__tmp) <= z)%Q
   | 18 => ((1 # 1) + s V_NRL_z + max0(-1 + s V_NRL__tmp) <= z)%Q
   | 19 => ((1 # 1) + s V_NRL_z + max0(-1 + s V_NRL__tmp) <= z)%Q
   | 20 => ((1 # 1) + s V_NRL_z + max0(-1 + s V_NRL__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_NRL =>
    [mkPA Q (fun n z s => ai_NRL n s /\ annot0_NRL n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_NRL (proc_start P_NRL) s1 (proc_end P_NRL) s2 ->
    (s2 V_NRL_z <= max0(-1 + s1 V_NRL_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_NRL.
Qed.
