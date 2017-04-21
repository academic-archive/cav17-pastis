Require Import pasta.Pasta.

Inductive proc: Type :=
  P_setkrent.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_setkrent_z := 1%positive.
Notation V_setkrent__tmp := 2%positive.
Notation V_setkrent_i := 3%positive.
Notation V_setkrent_nkr := 4%positive.
Notation V_setkrent_keyring := 5%positive.
Definition Pedges_setkrent: list (edge proc) :=
  (EA 1 (AAssign V_setkrent_z (Some (ENum (0)))) 2)::(EA 2 AWeaken 3)::
  (EA 3 (AGuard (fun s => ((eval (EVar V_setkrent_nkr) s) < (eval (ENum (8))
  s))%Z)) 7)::(EA 3 (AGuard (fun s => ((eval (EVar V_setkrent_nkr) s) >=
  (eval (ENum (8)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 ANone 6)::
  (EA 6 AWeaken 32)::(EA 7 AWeaken 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::
  (EA 10 ANone 11)::(EA 10 ANone 12)::(EA 11 ANone 12)::(EA 12 (AAssign
  V_setkrent_i (Some (ENum (0)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_setkrent_i) s) <
  (eval (EVar V_setkrent_nkr) s))%Z)) 21)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_setkrent_i) s) >= (eval (EVar V_setkrent_nkr)
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 (AAssign V_setkrent_nkr
  (Some (EAdd (EVar V_setkrent_nkr) (ENum (1))))) 18)::(EA 18 (AAssign
  V_setkrent__tmp (Some (ENum (0)))) 19)::(EA 19 ANone 20)::
  (EA 20 AWeaken 32)::(EA 21 AWeaken 22)::(EA 22 ANone 29)::
  (EA 22 ANone 23)::(EA 23 ANone 24)::(EA 24 (AAssign V_setkrent_i
  (Some (EAdd (EVar V_setkrent_i) (ENum (1))))) 25)::(EA 25 ANone 26)::
  (EA 26 ANone 27)::(EA 27 (AAssign V_setkrent_z (Some (EAdd (ENum (1))
  (EVar V_setkrent_z)))) 28)::(EA 28 AWeaken 15)::(EA 29 (AAssign
  V_setkrent__tmp (Some (ENum (0)))) 30)::(EA 30 ANone 31)::
  (EA 31 AWeaken 32)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_setkrent => Pedges_setkrent
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_setkrent => 32
     end)%positive;
  var_global := var_global
}.

Definition ai_setkrent (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_z <= 0)%Z
   | 3 => (-1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_z <= 0)%Z
   | 4 => (1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_nkr + 8 <= 0)%Z
   | 5 => (-1 * s V_setkrent_nkr + 8 <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_z <= 0)%Z
   | 6 => (1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_nkr + 8 <= 0)%Z
   | 7 => (1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0)%Z
   | 8 => (1 * s V_setkrent_nkr + -7 <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_z <= 0)%Z
   | 9 => (1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0)%Z
   | 10 => (1 * s V_setkrent_nkr + -7 <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_z <= 0)%Z
   | 11 => (1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0)%Z
   | 12 => (1 * s V_setkrent_nkr + -7 <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_z <= 0)%Z
   | 13 => (1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0 /\ 1 * s V_setkrent_i <= 0 /\ -1 * s V_setkrent_i <= 0)%Z
   | 14 => (-1 * s V_setkrent_i <= 0 /\ 1 * s V_setkrent_i <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_z <= 0)%Z
   | 15 => (-1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_i <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0)%Z
   | 16 => (1 * s V_setkrent_nkr + -7 <= 0 /\ -1 * s V_setkrent_i <= 0 /\ -1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_i+ 1 * s V_setkrent_nkr <= 0)%Z
   | 17 => (-1 * s V_setkrent_i+ 1 * s V_setkrent_nkr <= 0 /\ -1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_i <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0)%Z
   | 18 => (-1 * s V_setkrent_i <= 0 /\ -1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_i+ 1 * s V_setkrent_nkr + -1 <= 0 /\ 1 * s V_setkrent_nkr + -8 <= 0)%Z
   | 19 => (1 * s V_setkrent_nkr + -8 <= 0 /\ -1 * s V_setkrent_i+ 1 * s V_setkrent_nkr + -1 <= 0 /\ -1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_i <= 0 /\ 1 * s V_setkrent__tmp <= 0 /\ -1 * s V_setkrent__tmp <= 0)%Z
   | 20 => (-1 * s V_setkrent__tmp <= 0 /\ 1 * s V_setkrent__tmp <= 0 /\ -1 * s V_setkrent_i <= 0 /\ -1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_i+ 1 * s V_setkrent_nkr + -1 <= 0 /\ 1 * s V_setkrent_nkr + -8 <= 0)%Z
   | 21 => (1 * s V_setkrent_nkr + -7 <= 0 /\ -1 * s V_setkrent_i <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_i+ -1 * s V_setkrent_nkr + 1 <= 0)%Z
   | 22 => (1 * s V_setkrent_i+ -1 * s V_setkrent_nkr + 1 <= 0 /\ -1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_i <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0)%Z
   | 23 => (1 * s V_setkrent_nkr + -7 <= 0 /\ -1 * s V_setkrent_i <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_i+ -1 * s V_setkrent_nkr + 1 <= 0)%Z
   | 24 => (1 * s V_setkrent_i+ -1 * s V_setkrent_nkr + 1 <= 0 /\ -1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_i <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0)%Z
   | 25 => (1 * s V_setkrent_nkr + -7 <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_i+ -1 * s V_setkrent_nkr <= 0 /\ -1 * s V_setkrent_i + 1 <= 0)%Z
   | 26 => (-1 * s V_setkrent_i + 1 <= 0 /\ 1 * s V_setkrent_i+ -1 * s V_setkrent_nkr <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0)%Z
   | 27 => (1 * s V_setkrent_nkr + -7 <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_i+ -1 * s V_setkrent_nkr <= 0 /\ -1 * s V_setkrent_i + 1 <= 0)%Z
   | 28 => (-1 * s V_setkrent_i + 1 <= 0 /\ 1 * s V_setkrent_i+ -1 * s V_setkrent_nkr <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0 /\ -1 * s V_setkrent_z + 1 <= 0)%Z
   | 29 => (1 * s V_setkrent_nkr + -7 <= 0 /\ -1 * s V_setkrent_i <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_i+ -1 * s V_setkrent_nkr + 1 <= 0)%Z
   | 30 => (1 * s V_setkrent_i+ -1 * s V_setkrent_nkr + 1 <= 0 /\ -1 * s V_setkrent_z <= 0 /\ -1 * s V_setkrent_i <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0 /\ 1 * s V_setkrent__tmp <= 0 /\ -1 * s V_setkrent__tmp <= 0)%Z
   | 31 => (-1 * s V_setkrent__tmp <= 0 /\ 1 * s V_setkrent__tmp <= 0 /\ 1 * s V_setkrent_nkr + -7 <= 0 /\ -1 * s V_setkrent_i <= 0 /\ -1 * s V_setkrent_z <= 0 /\ 1 * s V_setkrent_i+ -1 * s V_setkrent_nkr + 1 <= 0)%Z
   | 32 => (-1 * s V_setkrent_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_setkrent (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_setkrent_nkr) <= z)%Q
   | 2 => (s V_setkrent_z + max0(s V_setkrent_nkr) <= z)%Q
   | 3 => (s V_setkrent_z + max0(s V_setkrent_nkr) <= z)%Q
   | 4 => (s V_setkrent_z + max0(s V_setkrent_nkr) <= z)%Q
   | 5 => (s V_setkrent_z + max0(s V_setkrent_nkr) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_ge_0 (s V_setkrent_nkr)]
     (s V_setkrent_z + max0(s V_setkrent_nkr) <= z)%Q
   | 7 => (s V_setkrent_z + max0(s V_setkrent_nkr) <= z)%Q
   | 8 => (s V_setkrent_z + max0(s V_setkrent_nkr) <= z)%Q
   | 9 => (s V_setkrent_z + max0(s V_setkrent_nkr) <= z)%Q
   | 10 => (s V_setkrent_z + max0(s V_setkrent_nkr) <= z)%Q
   | 11 => (s V_setkrent_z + max0(s V_setkrent_nkr) <= z)%Q
   | 12 => (s V_setkrent_z + max0(s V_setkrent_nkr) <= z)%Q
   | 13 => (s V_setkrent_z + max0(-s V_setkrent_i + s V_setkrent_nkr) <= z)%Q
   | 14 => (s V_setkrent_z + max0(-s V_setkrent_i + s V_setkrent_nkr) <= z)%Q
   | 15 => (s V_setkrent_z + max0(-s V_setkrent_i + s V_setkrent_nkr) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_setkrent_i
                                             + s V_setkrent_nkr) (-1
                                                                  - s V_setkrent_i
                                                                  + s V_setkrent_nkr));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_setkrent_i
                                                 + s V_setkrent_nkr)) (F_check_ge (0) (0))]
     (s V_setkrent_z + max0(-s V_setkrent_i + s V_setkrent_nkr) <= z)%Q
   | 17 => (s V_setkrent_z <= z)%Q
   | 18 => (s V_setkrent_z <= z)%Q
   | 19 => (s V_setkrent_z <= z)%Q
   | 20 => (s V_setkrent_z <= z)%Q
   | 21 => hints
     [(*0 1*) F_max0_pre_decrement 1 (-s V_setkrent_i + s V_setkrent_nkr) (1);
      (*0 0.142857*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                    - 
                                                                    s V_setkrent_nkr) (0))) (F_max0_ge_0 (8
                                                                    - s V_setkrent_nkr));
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_setkrent_nkr) (0))) (F_max0_ge_0 (-1
                                                                    + s V_setkrent_nkr))]
     (s V_setkrent_z + max0(-s V_setkrent_i + s V_setkrent_nkr) <= z)%Q
   | 22 => (s V_setkrent_z + max0(-1 - s V_setkrent_i + s V_setkrent_nkr)
            + (1 # 7) * max0(-1 + s V_setkrent_nkr)
            + (1 # 7) * max0(8 - s V_setkrent_nkr) <= z)%Q
   | 23 => (s V_setkrent_z + max0(-1 - s V_setkrent_i + s V_setkrent_nkr)
            + (1 # 7) * max0(-1 + s V_setkrent_nkr)
            + (1 # 7) * max0(8 - s V_setkrent_nkr) <= z)%Q
   | 24 => (s V_setkrent_z + max0(-1 - s V_setkrent_i + s V_setkrent_nkr)
            + (1 # 7) * max0(-1 + s V_setkrent_nkr)
            + (1 # 7) * max0(8 - s V_setkrent_nkr) <= z)%Q
   | 25 => (s V_setkrent_z + (1 # 7) * max0(-1 + s V_setkrent_nkr)
            + (1 # 7) * max0(8 - s V_setkrent_nkr)
            + max0(-s V_setkrent_i + s V_setkrent_nkr) <= z)%Q
   | 26 => (s V_setkrent_z + (1 # 7) * max0(-1 + s V_setkrent_nkr)
            + (1 # 7) * max0(8 - s V_setkrent_nkr)
            + max0(-s V_setkrent_i + s V_setkrent_nkr) <= z)%Q
   | 27 => (s V_setkrent_z + (1 # 7) * max0(-1 + s V_setkrent_nkr)
            + (1 # 7) * max0(8 - s V_setkrent_nkr)
            + max0(-s V_setkrent_i + s V_setkrent_nkr) <= z)%Q
   | 28 => hints
     [(*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                          - s V_setkrent_nkr)) (F_check_ge (8
                                                                    - s V_setkrent_nkr) (0));
      (*0 0.142857*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                         + s V_setkrent_nkr)) (F_check_ge (-1
                                                                    + s V_setkrent_nkr) (0))]
     (-(1 # 1) + s V_setkrent_z + (1 # 7) * max0(-1 + s V_setkrent_nkr)
      + (1 # 7) * max0(8 - s V_setkrent_nkr)
      + max0(-s V_setkrent_i + s V_setkrent_nkr) <= z)%Q
   | 29 => (s V_setkrent_z + max0(-1 - s V_setkrent_i + s V_setkrent_nkr)
            + (1 # 7) * max0(-1 + s V_setkrent_nkr)
            + (1 # 7) * max0(8 - s V_setkrent_nkr) <= z)%Q
   | 30 => (s V_setkrent_z + max0(-1 - s V_setkrent_i + s V_setkrent_nkr)
            + (1 # 7) * max0(-1 + s V_setkrent_nkr)
            + (1 # 7) * max0(8 - s V_setkrent_nkr) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_max0_ge_0 (-1 - s V_setkrent_i + s V_setkrent_nkr);
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_0 (8 - s V_setkrent_nkr)) (F_check_ge (0) (0));
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_setkrent_nkr)) (F_check_ge (0) (0))]
     (s V_setkrent_z + max0(-1 - s V_setkrent_i + s V_setkrent_nkr)
      + (1 # 7) * max0(-1 + s V_setkrent_nkr)
      + (1 # 7) * max0(8 - s V_setkrent_nkr) <= z)%Q
   | 32 => (s V_setkrent_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_setkrent =>
    [mkPA Q (fun n z s => ai_setkrent n s /\ annot0_setkrent n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_setkrent (proc_start P_setkrent) s1 (proc_end P_setkrent) s2 ->
    (s2 V_setkrent_z <= max0(s1 V_setkrent_nkr))%Q.
Proof.
  prove_bound ipa admissible_ipa P_setkrent.
Qed.
