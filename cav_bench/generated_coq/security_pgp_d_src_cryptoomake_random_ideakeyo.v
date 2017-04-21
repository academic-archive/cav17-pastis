Require Import pasta.Pasta.

Inductive proc: Type :=
  P_make_random_ideakey.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_make_random_ideakey_z := 1%positive.
Notation V_make_random_ideakey__tmp := 2%positive.
Notation V_make_random_ideakey_count := 3%positive.
Notation V_make_random_ideakey_key := 4%positive.
Notation V_make_random_ideakey_skip := 5%positive.
Definition Pedges_make_random_ideakey: list (edge proc) :=
  (EA 1 (AAssign V_make_random_ideakey_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_make_random_ideakey__tmp
  (Some (EVar V_make_random_ideakey_skip))) 3)::(EA 3 AWeaken 4)::
  (EA 4 ANone 5)::(EA 4 ANone 6)::(EA 5 ANone 6)::(EA 6 (AAssign
  V_make_random_ideakey_count (Some (ENum (24)))) 7)::(EA 7 (AAssign
  V_make_random_ideakey_count (Some (EVar V_make_random_ideakey__tmp))) 8)::
  (EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_make_random_ideakey_count) s) < (eval (ENum (24))
  s))%Z)) 13)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_make_random_ideakey_count) s) >= (eval (ENum (24))
  s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::
  (EA 15 (AAssign V_make_random_ideakey_count
  (Some (EAdd (EVar V_make_random_ideakey_count) (ENum (1))))) 16)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign V_make_random_ideakey_z
  (Some (EAdd (ENum (1)) (EVar V_make_random_ideakey_z)))) 19)::
  (EA 19 AWeaken 10)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_make_random_ideakey => Pedges_make_random_ideakey
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_make_random_ideakey => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_make_random_ideakey (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_make_random_ideakey_z <= 0 /\ -1 * s V_make_random_ideakey_z <= 0)%Z
   | 3 => (-1 * s V_make_random_ideakey_z <= 0 /\ 1 * s V_make_random_ideakey_z <= 0)%Z
   | 4 => (1 * s V_make_random_ideakey_z <= 0 /\ -1 * s V_make_random_ideakey_z <= 0)%Z
   | 5 => (-1 * s V_make_random_ideakey_z <= 0 /\ 1 * s V_make_random_ideakey_z <= 0)%Z
   | 6 => (1 * s V_make_random_ideakey_z <= 0 /\ -1 * s V_make_random_ideakey_z <= 0)%Z
   | 7 => (-1 * s V_make_random_ideakey_z <= 0 /\ 1 * s V_make_random_ideakey_z <= 0 /\ 1 * s V_make_random_ideakey_count + -24 <= 0 /\ -1 * s V_make_random_ideakey_count + 24 <= 0)%Z
   | 8 => (1 * s V_make_random_ideakey_z <= 0 /\ -1 * s V_make_random_ideakey_z <= 0)%Z
   | 9 => (-1 * s V_make_random_ideakey_z <= 0 /\ 1 * s V_make_random_ideakey_z <= 0)%Z
   | 10 => (-1 * s V_make_random_ideakey_z <= 0)%Z
   | 11 => (-1 * s V_make_random_ideakey_z <= 0 /\ -1 * s V_make_random_ideakey_count + 24 <= 0)%Z
   | 12 => (-1 * s V_make_random_ideakey_count + 24 <= 0 /\ -1 * s V_make_random_ideakey_z <= 0)%Z
   | 13 => (-1 * s V_make_random_ideakey_z <= 0 /\ 1 * s V_make_random_ideakey_count + -23 <= 0)%Z
   | 14 => (1 * s V_make_random_ideakey_count + -23 <= 0 /\ -1 * s V_make_random_ideakey_z <= 0)%Z
   | 15 => (-1 * s V_make_random_ideakey_z <= 0 /\ 1 * s V_make_random_ideakey_count + -23 <= 0)%Z
   | 16 => (-1 * s V_make_random_ideakey_z <= 0 /\ 1 * s V_make_random_ideakey_count + -24 <= 0)%Z
   | 17 => (1 * s V_make_random_ideakey_count + -24 <= 0 /\ -1 * s V_make_random_ideakey_z <= 0)%Z
   | 18 => (-1 * s V_make_random_ideakey_z <= 0 /\ 1 * s V_make_random_ideakey_count + -24 <= 0)%Z
   | 19 => (1 * s V_make_random_ideakey_count + -24 <= 0 /\ -1 * s V_make_random_ideakey_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_make_random_ideakey (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(24 - s V_make_random_ideakey_skip) <= z)%Q
   | 2 => (s V_make_random_ideakey_z
           + max0(24 - s V_make_random_ideakey_skip) <= z)%Q
   | 3 => (s V_make_random_ideakey_z
           + max0(24 - s V_make_random_ideakey__tmp) <= z)%Q
   | 4 => (s V_make_random_ideakey_z
           + max0(24 - s V_make_random_ideakey__tmp) <= z)%Q
   | 5 => (s V_make_random_ideakey_z
           + max0(24 - s V_make_random_ideakey__tmp) <= z)%Q
   | 6 => (s V_make_random_ideakey_z
           + max0(24 - s V_make_random_ideakey__tmp) <= z)%Q
   | 7 => (s V_make_random_ideakey_z
           + max0(24 - s V_make_random_ideakey__tmp) <= z)%Q
   | 8 => (s V_make_random_ideakey_z
           + max0(24 - s V_make_random_ideakey_count) <= z)%Q
   | 9 => (s V_make_random_ideakey_z
           + max0(24 - s V_make_random_ideakey_count) <= z)%Q
   | 10 => (s V_make_random_ideakey_z
            + max0(24 - s V_make_random_ideakey_count) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (24
                                             - s V_make_random_ideakey_count) (23
                                                                    - s V_make_random_ideakey_count));
      (*-1 0*) F_max0_ge_0 (23 - s V_make_random_ideakey_count)]
     (s V_make_random_ideakey_z + max0(24 - s V_make_random_ideakey_count) <= z)%Q
   | 12 => (s V_make_random_ideakey_z <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (24 - s V_make_random_ideakey_count) (1)]
     (s V_make_random_ideakey_z + max0(24 - s V_make_random_ideakey_count) <= z)%Q
   | 14 => ((1 # 1) + s V_make_random_ideakey_z
            + max0(23 - s V_make_random_ideakey_count) <= z)%Q
   | 15 => ((1 # 1) + s V_make_random_ideakey_z
            + max0(23 - s V_make_random_ideakey_count) <= z)%Q
   | 16 => ((1 # 1) + s V_make_random_ideakey_z
            + max0(24 - s V_make_random_ideakey_count) <= z)%Q
   | 17 => ((1 # 1) + s V_make_random_ideakey_z
            + max0(24 - s V_make_random_ideakey_count) <= z)%Q
   | 18 => ((1 # 1) + s V_make_random_ideakey_z
            + max0(24 - s V_make_random_ideakey_count) <= z)%Q
   | 19 => (s V_make_random_ideakey_z
            + max0(24 - s V_make_random_ideakey_count) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_make_random_ideakey =>
    [mkPA Q (fun n z s => ai_make_random_ideakey n s /\ annot0_make_random_ideakey n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_make_random_ideakey (proc_start P_make_random_ideakey) s1 (proc_end P_make_random_ideakey) s2 ->
    (s2 V_make_random_ideakey_z <= max0(24 - s1 V_make_random_ideakey_skip))%Q.
Proof.
  prove_bound ipa admissible_ipa P_make_random_ideakey.
Qed.
