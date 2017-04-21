Require Import pasta.Pasta.

Inductive proc: Type :=
  P_TIFFUnlinkDirectory.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_TIFFUnlinkDirectory_z := 1%positive.
Notation V_TIFFUnlinkDirectory__tmp := 2%positive.
Notation V_TIFFUnlinkDirectory__tmp1 := 3%positive.
Notation V_TIFFUnlinkDirectory_n := 4%positive.
Notation V_TIFFUnlinkDirectory_dirn := 5%positive.
Notation V_TIFFUnlinkDirectory_tif := 6%positive.
Definition Pedges_TIFFUnlinkDirectory: list (edge proc) :=
  (EA 1 (AAssign V_TIFFUnlinkDirectory_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_TIFFUnlinkDirectory__tmp1
  (Some (EVar V_TIFFUnlinkDirectory_dirn))) 3)::(EA 3 AWeaken 4)::
  (EA 4 ANone 47)::(EA 4 ANone 5)::(EA 5 (AAssign V_TIFFUnlinkDirectory_n
  (Some (ESub (EVar V_TIFFUnlinkDirectory__tmp1) (ENum (1))))) 6)::
  (EA 6 ANone 7)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_TIFFUnlinkDirectory_n) s) > (eval (ENum (0))
  s))%Z)) 31)::(EA 8 (AGuard (fun s => ((eval (EVar V_TIFFUnlinkDirectory_n)
  s) <= (eval (ENum (0)) s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 ANone 14)::
  (EA 10 ANone 11)::(EA 11 (AAssign V_TIFFUnlinkDirectory__tmp
  (Some (ENum (0)))) 12)::(EA 12 ANone 13)::(EA 13 AWeaken 50)::
  (EA 14 AWeaken 15)::(EA 15 ANone 17)::(EA 15 ANone 16)::
  (EA 16 AWeaken 19)::(EA 17 ANone 18)::(EA 18 AWeaken 19)::
  (EA 19 ANone 23)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_TIFFUnlinkDirectory__tmp (Some (ENum (0)))) 21)::(EA 21 ANone 22)::
  (EA 22 AWeaken 50)::(EA 23 AWeaken 24)::(EA 24 ANone 25)::
  (EA 24 ANone 28)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::(EA 26 ANone 28)::
  (EA 27 ANone 28)::(EA 28 (AAssign V_TIFFUnlinkDirectory__tmp
  (Some (ENum (1)))) 29)::(EA 29 ANone 30)::(EA 30 AWeaken 50)::
  (EA 31 AWeaken 32)::(EA 32 ANone 44)::(EA 32 ANone 33)::
  (EA 33 AWeaken 34)::(EA 34 ANone 38)::(EA 34 ANone 35)::(EA 35 (AAssign
  V_TIFFUnlinkDirectory__tmp (Some (ENum (0)))) 36)::(EA 36 ANone 37)::
  (EA 37 AWeaken 50)::(EA 38 ANone 39)::(EA 39 (AAssign
  V_TIFFUnlinkDirectory_n (Some (EAdd (EVar V_TIFFUnlinkDirectory_n)
  (ENum (-1))))) 40)::(EA 40 ANone 41)::(EA 41 ANone 42)::(EA 42 (AAssign
  V_TIFFUnlinkDirectory_z (Some (EAdd (ENum (1))
  (EVar V_TIFFUnlinkDirectory_z)))) 43)::(EA 43 AWeaken 8)::(EA 44 (AAssign
  V_TIFFUnlinkDirectory__tmp (Some (ENum (0)))) 45)::(EA 45 ANone 46)::
  (EA 46 AWeaken 50)::(EA 47 (AAssign V_TIFFUnlinkDirectory__tmp
  (Some (ENum (0)))) 48)::(EA 48 ANone 49)::(EA 49 AWeaken 50)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_TIFFUnlinkDirectory => Pedges_TIFFUnlinkDirectory
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_TIFFUnlinkDirectory => 50
     end)%positive;
  var_global := var_global
}.

Definition ai_TIFFUnlinkDirectory (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 3 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 4 => (1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 5 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 6 => (1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 7 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 8 => (-1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 9 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 10 => (1 * s V_TIFFUnlinkDirectory_n <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 11 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 12 => (1 * s V_TIFFUnlinkDirectory_n <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ -1 * s V_TIFFUnlinkDirectory__tmp <= 0)%Z
   | 13 => (-1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ 1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 14 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 15 => (1 * s V_TIFFUnlinkDirectory_n <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 16 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 17 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 18 => (1 * s V_TIFFUnlinkDirectory_n <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 19 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 20 => (1 * s V_TIFFUnlinkDirectory_n <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 21 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0 /\ 1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ -1 * s V_TIFFUnlinkDirectory__tmp <= 0)%Z
   | 22 => (-1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ 1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 23 => (1 * s V_TIFFUnlinkDirectory_n <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 24 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 25 => (1 * s V_TIFFUnlinkDirectory_n <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 26 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 27 => (1 * s V_TIFFUnlinkDirectory_n <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 28 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 29 => (1 * s V_TIFFUnlinkDirectory_n <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory__tmp + -1 <= 0 /\ -1 * s V_TIFFUnlinkDirectory__tmp + 1 <= 0)%Z
   | 30 => (-1 * s V_TIFFUnlinkDirectory__tmp + 1 <= 0 /\ 1 * s V_TIFFUnlinkDirectory__tmp + -1 <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 31 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_n + 1 <= 0)%Z
   | 32 => (-1 * s V_TIFFUnlinkDirectory_n + 1 <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 33 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_n + 1 <= 0)%Z
   | 34 => (-1 * s V_TIFFUnlinkDirectory_n + 1 <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 35 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_n + 1 <= 0)%Z
   | 36 => (-1 * s V_TIFFUnlinkDirectory_n + 1 <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ -1 * s V_TIFFUnlinkDirectory__tmp <= 0)%Z
   | 37 => (-1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ 1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_n + 1 <= 0)%Z
   | 38 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_n + 1 <= 0)%Z
   | 39 => (-1 * s V_TIFFUnlinkDirectory_n + 1 <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 40 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 41 => (-1 * s V_TIFFUnlinkDirectory_n <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 42 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_n <= 0)%Z
   | 43 => (-1 * s V_TIFFUnlinkDirectory_n <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z + 1 <= 0)%Z
   | 44 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_n + 1 <= 0)%Z
   | 45 => (-1 * s V_TIFFUnlinkDirectory_n + 1 <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ -1 * s V_TIFFUnlinkDirectory__tmp <= 0)%Z
   | 46 => (-1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ 1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_n + 1 <= 0)%Z
   | 47 => (-1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 48 => (1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ -1 * s V_TIFFUnlinkDirectory__tmp <= 0)%Z
   | 49 => (-1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ 1 * s V_TIFFUnlinkDirectory__tmp <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0 /\ 1 * s V_TIFFUnlinkDirectory_z <= 0)%Z
   | 50 => (1 * s V_TIFFUnlinkDirectory__tmp + -1 <= 0 /\ -1 * s V_TIFFUnlinkDirectory_z <= 0 /\ -1 * s V_TIFFUnlinkDirectory__tmp <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_TIFFUnlinkDirectory (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_TIFFUnlinkDirectory_dirn) <= z)%Q
   | 2 => (s V_TIFFUnlinkDirectory_z
           + max0(-1 + s V_TIFFUnlinkDirectory_dirn) <= z)%Q
   | 3 => (s V_TIFFUnlinkDirectory_z
           + max0(-1 + s V_TIFFUnlinkDirectory__tmp1) <= z)%Q
   | 4 => (s V_TIFFUnlinkDirectory_z
           + max0(-1 + s V_TIFFUnlinkDirectory__tmp1) <= z)%Q
   | 5 => (s V_TIFFUnlinkDirectory_z
           + max0(-1 + s V_TIFFUnlinkDirectory__tmp1) <= z)%Q
   | 6 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 7 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 8 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 9 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 10 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 11 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 12 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFUnlinkDirectory_n) (-1
                                                                    + s V_TIFFUnlinkDirectory_n));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFUnlinkDirectory_n)]
     (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 14 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 15 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 16 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 17 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 18 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 19 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 20 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 21 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFUnlinkDirectory_n) (-1
                                                                    + s V_TIFFUnlinkDirectory_n));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFUnlinkDirectory_n)]
     (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 23 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 24 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 25 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 26 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 27 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 28 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 29 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 30 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_TIFFUnlinkDirectory_n) (-1
                                                                    + s V_TIFFUnlinkDirectory_n));
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFUnlinkDirectory_n)]
     (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 31 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 32 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_TIFFUnlinkDirectory_n)) (F_check_ge (s V_TIFFUnlinkDirectory_n) (0))]
     (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 34 => (s V_TIFFUnlinkDirectory_n + s V_TIFFUnlinkDirectory_z <= z)%Q
   | 35 => (s V_TIFFUnlinkDirectory_n + s V_TIFFUnlinkDirectory_z <= z)%Q
   | 36 => (s V_TIFFUnlinkDirectory_n + s V_TIFFUnlinkDirectory_z <= z)%Q
   | 37 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_TIFFUnlinkDirectory_n) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFUnlinkDirectory_n);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_TIFFUnlinkDirectory_n) (0))) (F_max0_ge_0 (s V_TIFFUnlinkDirectory_n))]
     (s V_TIFFUnlinkDirectory_n + s V_TIFFUnlinkDirectory_z <= z)%Q
   | 38 => (s V_TIFFUnlinkDirectory_n + s V_TIFFUnlinkDirectory_z <= z)%Q
   | 39 => (s V_TIFFUnlinkDirectory_n + s V_TIFFUnlinkDirectory_z <= z)%Q
   | 40 => ((1 # 1) + s V_TIFFUnlinkDirectory_n + s V_TIFFUnlinkDirectory_z <= z)%Q
   | 41 => ((1 # 1) + s V_TIFFUnlinkDirectory_n + s V_TIFFUnlinkDirectory_z <= z)%Q
   | 42 => ((1 # 1) + s V_TIFFUnlinkDirectory_n + s V_TIFFUnlinkDirectory_z <= z)%Q
   | 43 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_TIFFUnlinkDirectory_n) (0))) (F_max0_ge_0 (s V_TIFFUnlinkDirectory_n))]
     (s V_TIFFUnlinkDirectory_n + s V_TIFFUnlinkDirectory_z <= z)%Q
   | 44 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 45 => (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 46 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_TIFFUnlinkDirectory_n) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFUnlinkDirectory_n)]
     (s V_TIFFUnlinkDirectory_z + max0(s V_TIFFUnlinkDirectory_n) <= z)%Q
   | 47 => (s V_TIFFUnlinkDirectory_z
            + max0(-1 + s V_TIFFUnlinkDirectory__tmp1) <= z)%Q
   | 48 => (s V_TIFFUnlinkDirectory_z
            + max0(-1 + s V_TIFFUnlinkDirectory__tmp1) <= z)%Q
   | 49 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_TIFFUnlinkDirectory__tmp1)]
     (s V_TIFFUnlinkDirectory_z + max0(-1 + s V_TIFFUnlinkDirectory__tmp1) <= z)%Q
   | 50 => (s V_TIFFUnlinkDirectory_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_TIFFUnlinkDirectory =>
    [mkPA Q (fun n z s => ai_TIFFUnlinkDirectory n s /\ annot0_TIFFUnlinkDirectory n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_TIFFUnlinkDirectory (proc_start P_TIFFUnlinkDirectory) s1 (proc_end P_TIFFUnlinkDirectory) s2 ->
    (s2 V_TIFFUnlinkDirectory_z <= max0(-1 + s1 V_TIFFUnlinkDirectory_dirn))%Q.
Proof.
  prove_bound ipa admissible_ipa P_TIFFUnlinkDirectory.
Qed.
