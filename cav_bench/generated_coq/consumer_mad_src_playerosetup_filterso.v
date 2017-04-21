Require Import pasta.Pasta.

Inductive proc: Type :=
  P_setup_filters.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_setup_filters_z := 1%positive.
Notation V_setup_filters__tmp := 2%positive.
Notation V_setup_filters_sb := 3%positive.
Notation V_setup_filters_player := 4%positive.
Definition Pedges_setup_filters: list (edge proc) :=
  (EA 1 (AAssign V_setup_filters_z (Some (ENum (0)))) 2)::(EA 2 AWeaken 3)::
  (EA 3 ANone 5)::(EA 3 ANone 4)::(EA 4 AWeaken 8)::(EA 5 AWeaken 6)::
  (EA 6 ANone 45)::(EA 6 ANone 7)::(EA 7 AWeaken 8)::(EA 8 ANone 10)::
  (EA 8 ANone 9)::(EA 9 AWeaken 18)::(EA 10 (AAssign V_setup_filters_sb
  (Some (ENum (0)))) 11)::(EA 11 ANone 12)::(EA 12 AWeaken 13)::
  (EA 13 (AGuard (fun s => ((eval (EVar V_setup_filters_sb) s) <
  (eval (ENum (32)) s))%Z)) 38)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_setup_filters_sb) s) >= (eval (ENum (32))
  s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 ANone 35)::(EA 15 ANone 16)::
  (EA 16 ANone 17)::(EA 17 AWeaken 18)::(EA 18 ANone 20)::(EA 18 ANone 19)::
  (EA 19 AWeaken 23)::(EA 20 AWeaken 21)::(EA 21 ANone 32)::
  (EA 21 ANone 22)::(EA 22 AWeaken 23)::(EA 23 ANone 24)::(EA 23 ANone 26)::
  (EA 24 AWeaken 25)::(EA 25 ANone 29)::(EA 25 ANone 26)::(EA 26 (AAssign
  V_setup_filters__tmp (Some (ENum (0)))) 27)::(EA 27 ANone 28)::
  (EA 28 AWeaken 48)::(EA 29 (AAssign V_setup_filters__tmp
  (Some (ENum (-1)))) 30)::(EA 30 ANone 31)::(EA 31 AWeaken 48)::
  (EA 32 (AAssign V_setup_filters__tmp (Some (ENum (-1)))) 33)::
  (EA 33 ANone 34)::(EA 34 AWeaken 48)::(EA 35 (AAssign V_setup_filters__tmp
  (Some (ENum (-1)))) 36)::(EA 36 ANone 37)::(EA 37 AWeaken 48)::
  (EA 38 AWeaken 39)::(EA 39 ANone 40)::(EA 40 (AAssign V_setup_filters_sb
  (Some (EAdd (EVar V_setup_filters_sb) (ENum (1))))) 41)::(EA 41 ANone 42)::
  (EA 42 ANone 43)::(EA 43 (AAssign V_setup_filters_z (Some (EAdd (ENum (1))
  (EVar V_setup_filters_z)))) 44)::(EA 44 AWeaken 13)::(EA 45 (AAssign
  V_setup_filters__tmp (Some (ENum (-1)))) 46)::(EA 46 ANone 47)::
  (EA 47 AWeaken 48)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_setup_filters => Pedges_setup_filters
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_setup_filters => 48
     end)%positive;
  var_global := var_global
}.

Definition ai_setup_filters (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 3 => (-1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters_z <= 0)%Z
   | 4 => (1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 5 => (1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 6 => (-1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters_z <= 0)%Z
   | 7 => (1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 8 => (-1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters_z <= 0)%Z
   | 9 => (1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 10 => (1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 11 => (-1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters_sb <= 0 /\ -1 * s V_setup_filters_sb <= 0)%Z
   | 12 => (-1 * s V_setup_filters_sb <= 0 /\ 1 * s V_setup_filters_sb <= 0 /\ 1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 13 => (-1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_sb <= 0 /\ 1 * s V_setup_filters_sb + -32 <= 0)%Z
   | 14 => (1 * s V_setup_filters_sb + -32 <= 0 /\ -1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_sb + 32 <= 0)%Z
   | 15 => (-1 * s V_setup_filters_sb + 32 <= 0 /\ -1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters_sb + -32 <= 0)%Z
   | 16 => (1 * s V_setup_filters_sb + -32 <= 0 /\ -1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_sb + 32 <= 0)%Z
   | 17 => (-1 * s V_setup_filters_sb + 32 <= 0 /\ -1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters_sb + -32 <= 0)%Z
   | 18 => (-1 * s V_setup_filters_z <= 0)%Z
   | 19 => (-1 * s V_setup_filters_z <= 0)%Z
   | 20 => (-1 * s V_setup_filters_z <= 0)%Z
   | 21 => (-1 * s V_setup_filters_z <= 0)%Z
   | 22 => (-1 * s V_setup_filters_z <= 0)%Z
   | 23 => (-1 * s V_setup_filters_z <= 0)%Z
   | 24 => (-1 * s V_setup_filters_z <= 0)%Z
   | 25 => (-1 * s V_setup_filters_z <= 0)%Z
   | 26 => (-1 * s V_setup_filters_z <= 0)%Z
   | 27 => (-1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters__tmp <= 0 /\ -1 * s V_setup_filters__tmp <= 0)%Z
   | 28 => (-1 * s V_setup_filters__tmp <= 0 /\ 1 * s V_setup_filters__tmp <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 29 => (-1 * s V_setup_filters_z <= 0)%Z
   | 30 => (-1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters__tmp + 1 <= 0 /\ -1 * s V_setup_filters__tmp + -1 <= 0)%Z
   | 31 => (-1 * s V_setup_filters__tmp + -1 <= 0 /\ 1 * s V_setup_filters__tmp + 1 <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 32 => (-1 * s V_setup_filters_z <= 0)%Z
   | 33 => (-1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters__tmp + 1 <= 0 /\ -1 * s V_setup_filters__tmp + -1 <= 0)%Z
   | 34 => (-1 * s V_setup_filters__tmp + -1 <= 0 /\ 1 * s V_setup_filters__tmp + 1 <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 35 => (1 * s V_setup_filters_sb + -32 <= 0 /\ -1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_sb + 32 <= 0)%Z
   | 36 => (-1 * s V_setup_filters_sb + 32 <= 0 /\ -1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters_sb + -32 <= 0 /\ 1 * s V_setup_filters__tmp + 1 <= 0 /\ -1 * s V_setup_filters__tmp + -1 <= 0)%Z
   | 37 => (-1 * s V_setup_filters__tmp + -1 <= 0 /\ 1 * s V_setup_filters__tmp + 1 <= 0 /\ 1 * s V_setup_filters_sb + -32 <= 0 /\ -1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_sb + 32 <= 0)%Z
   | 38 => (-1 * s V_setup_filters_sb <= 0 /\ -1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters_sb + -31 <= 0)%Z
   | 39 => (1 * s V_setup_filters_sb + -31 <= 0 /\ -1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_sb <= 0)%Z
   | 40 => (-1 * s V_setup_filters_sb <= 0 /\ -1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters_sb + -31 <= 0)%Z
   | 41 => (-1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_sb + 1 <= 0 /\ 1 * s V_setup_filters_sb + -32 <= 0)%Z
   | 42 => (1 * s V_setup_filters_sb + -32 <= 0 /\ -1 * s V_setup_filters_sb + 1 <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 43 => (-1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_sb + 1 <= 0 /\ 1 * s V_setup_filters_sb + -32 <= 0)%Z
   | 44 => (1 * s V_setup_filters_sb + -32 <= 0 /\ -1 * s V_setup_filters_sb + 1 <= 0 /\ -1 * s V_setup_filters_z + 1 <= 0)%Z
   | 45 => (1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 46 => (-1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters_z <= 0 /\ 1 * s V_setup_filters__tmp + 1 <= 0 /\ -1 * s V_setup_filters__tmp + -1 <= 0)%Z
   | 47 => (-1 * s V_setup_filters__tmp + -1 <= 0 /\ 1 * s V_setup_filters__tmp + 1 <= 0 /\ 1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters_z <= 0)%Z
   | 48 => (1 * s V_setup_filters__tmp <= 0 /\ -1 * s V_setup_filters_z <= 0 /\ -1 * s V_setup_filters__tmp + -1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_setup_filters (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((32 # 1) <= z)%Q
   | 2 => ((32 # 1) + max0(s V_setup_filters_z) <= z)%Q
   | 3 => ((32 # 1) + max0(s V_setup_filters_z) <= z)%Q
   | 4 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_setup_filters_z)) (F_check_ge (s V_setup_filters_z) (0))]
     ((32 # 1) + max0(s V_setup_filters_z) <= z)%Q
   | 5 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_setup_filters_z)) (F_check_ge (s V_setup_filters_z) (0))]
     ((32 # 1) + max0(s V_setup_filters_z) <= z)%Q
   | 6 => ((32 # 1) + s V_setup_filters_z <= z)%Q
   | 7 => ((32 # 1) + s V_setup_filters_z <= z)%Q
   | 8 => ((32 # 1) + s V_setup_filters_z <= z)%Q
   | 9 => hints
     [(*-32 0*) F_one]
     ((32 # 1) + s V_setup_filters_z <= z)%Q
   | 10 => ((32 # 1) + s V_setup_filters_z <= z)%Q
   | 11 => (s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 12 => (s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 13 => (s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 14 => (s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 15 => (s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 16 => (s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (32 - s V_setup_filters_sb) (31
                                                                    - s V_setup_filters_sb));
      (*-1 0*) F_max0_ge_0 (31 - s V_setup_filters_sb)]
     (s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 18 => (s V_setup_filters_z <= z)%Q
   | 19 => (s V_setup_filters_z <= z)%Q
   | 20 => (s V_setup_filters_z <= z)%Q
   | 21 => (s V_setup_filters_z <= z)%Q
   | 22 => (s V_setup_filters_z <= z)%Q
   | 23 => (s V_setup_filters_z <= z)%Q
   | 24 => (s V_setup_filters_z <= z)%Q
   | 25 => (s V_setup_filters_z <= z)%Q
   | 26 => (s V_setup_filters_z <= z)%Q
   | 27 => (s V_setup_filters_z <= z)%Q
   | 28 => (s V_setup_filters_z <= z)%Q
   | 29 => (s V_setup_filters_z <= z)%Q
   | 30 => (s V_setup_filters_z <= z)%Q
   | 31 => (s V_setup_filters_z <= z)%Q
   | 32 => (s V_setup_filters_z <= z)%Q
   | 33 => (s V_setup_filters_z <= z)%Q
   | 34 => (s V_setup_filters_z <= z)%Q
   | 35 => (s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 36 => (s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 37 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (32 - s V_setup_filters_sb) (31
                                                                    - s V_setup_filters_sb));
      (*-1 0*) F_max0_ge_0 (31 - s V_setup_filters_sb)]
     (s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (32 - s V_setup_filters_sb) (1)]
     (s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 39 => ((1 # 1) + s V_setup_filters_z + max0(31 - s V_setup_filters_sb) <= z)%Q
   | 40 => ((1 # 1) + s V_setup_filters_z + max0(31 - s V_setup_filters_sb) <= z)%Q
   | 41 => ((1 # 1) + s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 42 => ((1 # 1) + s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 43 => ((1 # 1) + s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 44 => (s V_setup_filters_z + max0(32 - s V_setup_filters_sb) <= z)%Q
   | 45 => ((32 # 1) + s V_setup_filters_z <= z)%Q
   | 46 => ((32 # 1) + s V_setup_filters_z <= z)%Q
   | 47 => hints
     [(*-32 0*) F_one]
     ((32 # 1) + s V_setup_filters_z <= z)%Q
   | 48 => (s V_setup_filters_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_setup_filters =>
    [mkPA Q (fun n z s => ai_setup_filters n s /\ annot0_setup_filters n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_setup_filters (proc_start P_setup_filters) s1 (proc_end P_setup_filters) s2 ->
    (s2 V_setup_filters_z <= (32 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_setup_filters.
Qed.