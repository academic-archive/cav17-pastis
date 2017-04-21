Require Import pasta.Pasta.

Inductive proc: Type :=
  P_insert.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_insert_z := 1%positive.
Notation V_insert__tmp := 2%positive.
Notation V_insert_i := 3%positive.
Notation V_insert_maxposslen := 4%positive.
Notation V_insert_pcount := 5%positive.
Notation V_insert_word := 6%positive.
Definition Pedges_insert: list (edge proc) :=
  (EA 1 (AAssign V_insert_z (Some (ENum (0)))) 2)::(EA 2 (AAssign V_insert_i
  (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_insert_i) s) < (eval (EVar V_insert_pcount)
  s))%Z)) 25)::(EA 5 (AGuard (fun s => ((eval (EVar V_insert_i) s) >=
  (eval (EVar V_insert_pcount) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign
  V_insert_pcount (Some (EAdd (EVar V_insert_pcount) (ENum (1))))) 8)::
  (EA 8 (AAssign V_insert_i None) 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_insert_i) s) > (eval (EVar V_insert_maxposslen)
  s))%Z)) 12)::(EA 10 (AGuard (fun s => ((eval (EVar V_insert_i) s) <=
  (eval (EVar V_insert_maxposslen) s))%Z)) 11)::(EA 11 AWeaken 16)::
  (EA 12 AWeaken 13)::(EA 13 (AAssign V_insert_maxposslen
  (Some (EVar V_insert_i))) 14)::(EA 14 ANone 15)::(EA 15 AWeaken 16)::
  (EA 16 (AGuard (fun s => ((eval (EVar V_insert_pcount) s) >=
  (eval (ENum (100)) s))%Z)) 21)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_insert_pcount) s) < (eval (ENum (100))
  s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 18 (AAssign V_insert__tmp
  (Some (ENum (0)))) 19)::(EA 19 ANone 20)::(EA 20 AWeaken 36)::
  (EA 21 AWeaken 22)::(EA 22 (AAssign V_insert__tmp (Some (ENum (-1)))) 23)::
  (EA 23 ANone 24)::(EA 24 AWeaken 36)::(EA 25 AWeaken 26)::
  (EA 26 ANone 33)::(EA 26 ANone 27)::(EA 27 ANone 28)::(EA 28 (AAssign
  V_insert_i (Some (EAdd (EVar V_insert_i) (ENum (1))))) 29)::
  (EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 (AAssign V_insert_z
  (Some (EAdd (ENum (1)) (EVar V_insert_z)))) 32)::(EA 32 AWeaken 5)::
  (EA 33 (AAssign V_insert__tmp (Some (ENum (0)))) 34)::(EA 34 ANone 35)::
  (EA 35 AWeaken 36)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_insert => Pedges_insert
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_insert => 36
     end)%positive;
  var_global := var_global
}.

Definition ai_insert (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_insert_z <= 0 /\ -1 * s V_insert_z <= 0)%Z
   | 3 => (-1 * s V_insert_z <= 0 /\ 1 * s V_insert_z <= 0 /\ 1 * s V_insert_i <= 0 /\ -1 * s V_insert_i <= 0)%Z
   | 4 => (-1 * s V_insert_i <= 0 /\ 1 * s V_insert_i <= 0 /\ 1 * s V_insert_z <= 0 /\ -1 * s V_insert_z <= 0)%Z
   | 5 => (-1 * s V_insert_z <= 0 /\ -1 * s V_insert_i <= 0)%Z
   | 6 => (-1 * s V_insert_i <= 0 /\ -1 * s V_insert_z <= 0 /\ -1 * s V_insert_i+ 1 * s V_insert_pcount <= 0)%Z
   | 7 => (-1 * s V_insert_i+ 1 * s V_insert_pcount <= 0 /\ -1 * s V_insert_z <= 0 /\ -1 * s V_insert_i <= 0)%Z
   | 8 => (-1 * s V_insert_i <= 0 /\ -1 * s V_insert_z <= 0 /\ -1 * s V_insert_i+ 1 * s V_insert_pcount + -1 <= 0)%Z
   | 9 => (-1 * s V_insert_z <= 0)%Z
   | 10 => (-1 * s V_insert_z <= 0)%Z
   | 11 => (-1 * s V_insert_z <= 0 /\ 1 * s V_insert_i+ -1 * s V_insert_maxposslen <= 0)%Z
   | 12 => (-1 * s V_insert_z <= 0 /\ -1 * s V_insert_i+ 1 * s V_insert_maxposslen + 1 <= 0)%Z
   | 13 => (-1 * s V_insert_i+ 1 * s V_insert_maxposslen + 1 <= 0 /\ -1 * s V_insert_z <= 0)%Z
   | 14 => (-1 * s V_insert_z <= 0)%Z
   | 15 => (-1 * s V_insert_z <= 0)%Z
   | 16 => (-1 * s V_insert_z <= 0)%Z
   | 17 => (-1 * s V_insert_z <= 0 /\ 1 * s V_insert_pcount + -99 <= 0)%Z
   | 18 => (1 * s V_insert_pcount + -99 <= 0 /\ -1 * s V_insert_z <= 0)%Z
   | 19 => (-1 * s V_insert_z <= 0 /\ 1 * s V_insert_pcount + -99 <= 0 /\ 1 * s V_insert__tmp <= 0 /\ -1 * s V_insert__tmp <= 0)%Z
   | 20 => (-1 * s V_insert__tmp <= 0 /\ 1 * s V_insert__tmp <= 0 /\ 1 * s V_insert_pcount + -99 <= 0 /\ -1 * s V_insert_z <= 0)%Z
   | 21 => (-1 * s V_insert_z <= 0 /\ -1 * s V_insert_pcount + 100 <= 0)%Z
   | 22 => (-1 * s V_insert_pcount + 100 <= 0 /\ -1 * s V_insert_z <= 0)%Z
   | 23 => (-1 * s V_insert_z <= 0 /\ -1 * s V_insert_pcount + 100 <= 0 /\ 1 * s V_insert__tmp + 1 <= 0 /\ -1 * s V_insert__tmp + -1 <= 0)%Z
   | 24 => (-1 * s V_insert__tmp + -1 <= 0 /\ 1 * s V_insert__tmp + 1 <= 0 /\ -1 * s V_insert_pcount + 100 <= 0 /\ -1 * s V_insert_z <= 0)%Z
   | 25 => (-1 * s V_insert_i <= 0 /\ -1 * s V_insert_z <= 0 /\ 1 * s V_insert_i+ -1 * s V_insert_pcount + 1 <= 0)%Z
   | 26 => (1 * s V_insert_i+ -1 * s V_insert_pcount + 1 <= 0 /\ -1 * s V_insert_z <= 0 /\ -1 * s V_insert_i <= 0)%Z
   | 27 => (-1 * s V_insert_i <= 0 /\ -1 * s V_insert_z <= 0 /\ 1 * s V_insert_i+ -1 * s V_insert_pcount + 1 <= 0)%Z
   | 28 => (1 * s V_insert_i+ -1 * s V_insert_pcount + 1 <= 0 /\ -1 * s V_insert_z <= 0 /\ -1 * s V_insert_i <= 0)%Z
   | 29 => (-1 * s V_insert_z <= 0 /\ 1 * s V_insert_i+ -1 * s V_insert_pcount <= 0 /\ -1 * s V_insert_i + 1 <= 0)%Z
   | 30 => (-1 * s V_insert_i + 1 <= 0 /\ 1 * s V_insert_i+ -1 * s V_insert_pcount <= 0 /\ -1 * s V_insert_z <= 0)%Z
   | 31 => (-1 * s V_insert_z <= 0 /\ 1 * s V_insert_i+ -1 * s V_insert_pcount <= 0 /\ -1 * s V_insert_i + 1 <= 0)%Z
   | 32 => (-1 * s V_insert_i + 1 <= 0 /\ 1 * s V_insert_i+ -1 * s V_insert_pcount <= 0 /\ -1 * s V_insert_z + 1 <= 0)%Z
   | 33 => (-1 * s V_insert_i <= 0 /\ -1 * s V_insert_z <= 0 /\ 1 * s V_insert_i+ -1 * s V_insert_pcount + 1 <= 0)%Z
   | 34 => (1 * s V_insert_i+ -1 * s V_insert_pcount + 1 <= 0 /\ -1 * s V_insert_z <= 0 /\ -1 * s V_insert_i <= 0 /\ 1 * s V_insert__tmp <= 0 /\ -1 * s V_insert__tmp <= 0)%Z
   | 35 => (-1 * s V_insert__tmp <= 0 /\ 1 * s V_insert__tmp <= 0 /\ -1 * s V_insert_i <= 0 /\ -1 * s V_insert_z <= 0 /\ 1 * s V_insert_i+ -1 * s V_insert_pcount + 1 <= 0)%Z
   | 36 => (-1 * s V_insert__tmp + -1 <= 0 /\ -1 * s V_insert_z <= 0 /\ 1 * s V_insert__tmp <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_insert (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_insert_pcount) <= z)%Q
   | 2 => (max0(s V_insert_pcount) + max0(s V_insert_z) <= z)%Q
   | 3 => (max0(-s V_insert_i + s V_insert_pcount) + max0(s V_insert_z) <= z)%Q
   | 4 => (max0(-s V_insert_i + s V_insert_pcount) + max0(s V_insert_z) <= z)%Q
   | 5 => (max0(-s V_insert_i + s V_insert_pcount) + max0(s V_insert_z) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_insert_i
                                             + s V_insert_pcount) (-1
                                                                   - 
                                                                   s V_insert_i
                                                                   + 
                                                                   s V_insert_pcount));
      (*0 1*) F_max0_ge_0 (-1 - s V_insert_i + s V_insert_pcount)]
     (max0(-s V_insert_i + s V_insert_pcount) + max0(s V_insert_z) <= z)%Q
   | 7 => (max0(s V_insert_z) <= z)%Q
   | 8 => (max0(s V_insert_z) <= z)%Q
   | 9 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_insert_z)) (F_check_ge (s V_insert_z) (0))]
     (max0(s V_insert_z) <= z)%Q
   | 10 => (s V_insert_z <= z)%Q
   | 11 => (s V_insert_z <= z)%Q
   | 12 => (s V_insert_z <= z)%Q
   | 13 => (s V_insert_z <= z)%Q
   | 14 => (s V_insert_z <= z)%Q
   | 15 => (s V_insert_z <= z)%Q
   | 16 => (s V_insert_z <= z)%Q
   | 17 => (s V_insert_z <= z)%Q
   | 18 => (s V_insert_z <= z)%Q
   | 19 => (s V_insert_z <= z)%Q
   | 20 => (s V_insert_z <= z)%Q
   | 21 => (s V_insert_z <= z)%Q
   | 22 => (s V_insert_z <= z)%Q
   | 23 => (s V_insert_z <= z)%Q
   | 24 => (s V_insert_z <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_insert_i + s V_insert_pcount) (1)]
     (max0(-s V_insert_i + s V_insert_pcount) + max0(s V_insert_z) <= z)%Q
   | 26 => ((1 # 1) + max0(-1 - s V_insert_i + s V_insert_pcount)
            + max0(s V_insert_z) <= z)%Q
   | 27 => ((1 # 1) + max0(-1 - s V_insert_i + s V_insert_pcount)
            + max0(s V_insert_z) <= z)%Q
   | 28 => ((1 # 1) + max0(-1 - s V_insert_i + s V_insert_pcount)
            + max0(s V_insert_z) <= z)%Q
   | 29 => ((1 # 1) + max0(-s V_insert_i + s V_insert_pcount)
            + max0(s V_insert_z) <= z)%Q
   | 30 => ((1 # 1) + max0(-s V_insert_i + s V_insert_pcount)
            + max0(s V_insert_z) <= z)%Q
   | 31 => ((1 # 1) + max0(-s V_insert_i + s V_insert_pcount)
            + max0(s V_insert_z) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_insert_z) (0))) (F_max0_ge_0 (s V_insert_z));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_insert_z)) (F_check_ge (-1
                                                                    + s V_insert_z) (0))]
     ((1 # 1) + max0(-1 + s V_insert_z)
      + max0(-s V_insert_i + s V_insert_pcount) <= z)%Q
   | 33 => ((1 # 1) + max0(-1 - s V_insert_i + s V_insert_pcount)
            + max0(s V_insert_z) <= z)%Q
   | 34 => ((1 # 1) + max0(-1 - s V_insert_i + s V_insert_pcount)
            + max0(s V_insert_z) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 - s V_insert_i + s V_insert_pcount);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_insert_z)) (F_check_ge (s V_insert_z) (0))]
     ((1 # 1) + max0(-1 - s V_insert_i + s V_insert_pcount)
      + max0(s V_insert_z) <= z)%Q
   | 36 => (s V_insert_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_insert =>
    [mkPA Q (fun n z s => ai_insert n s /\ annot0_insert n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_insert (proc_start P_insert) s1 (proc_end P_insert) s2 ->
    (s2 V_insert_z <= max0(s1 V_insert_pcount))%Q.
Proof.
  prove_bound ipa admissible_ipa P_insert.
Qed.
