Require Import pasta.Pasta.

Inductive proc: Type :=
  P_TIFFSetDirectory.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_TIFFSetDirectory_z := 1%positive.
Notation V_TIFFSetDirectory__tmp := 2%positive.
Notation V_TIFFSetDirectory__tmp1 := 3%positive.
Notation V_TIFFSetDirectory_n := 4%positive.
Notation V_TIFFSetDirectory_dirn := 5%positive.
Notation V_TIFFSetDirectory_tif := 6%positive.
Definition Pedges_TIFFSetDirectory: list (edge proc) :=
  (EA 1 (AAssign V_TIFFSetDirectory_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_TIFFSetDirectory__tmp1 (Some (EVar V_TIFFSetDirectory_dirn))) 3)::
  (EA 3 (AAssign V_TIFFSetDirectory_n
  (Some (EVar V_TIFFSetDirectory__tmp1))) 4)::(EA 4 ANone 5)::
  (EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_TIFFSetDirectory_n) s) > (eval (ENum (0))
  s))%Z)) 8)::(EA 6 (AGuard (fun s => ((eval (EVar V_TIFFSetDirectory_n)
  s) <= (eval (ENum (0)) s))%Z)) 7)::(EA 7 AWeaken 13)::(EA 8 AWeaken 9)::
  (EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard (fun s => True)) 16)::
  (EA 11 (AGuard (fun s => True)) 12)::(EA 12 AWeaken 13)::(EA 13 (AAssign
  V_TIFFSetDirectory__tmp None) 14)::(EA 14 ANone 15)::(EA 15 AWeaken 21)::
  (EA 16 AWeaken 17)::(EA 17 ANone 22)::(EA 17 ANone 18)::(EA 18 (AAssign
  V_TIFFSetDirectory__tmp (Some (ENum (0)))) 19)::(EA 19 ANone 20)::
  (EA 20 AWeaken 21)::(EA 22 ANone 23)::(EA 23 (AAssign V_TIFFSetDirectory_n
  (Some (EAdd (EVar V_TIFFSetDirectory_n) (ENum (-1))))) 24)::
  (EA 24 ANone 25)::(EA 25 ANone 26)::(EA 26 (AAssign V_TIFFSetDirectory_z
  (Some (EAdd (ENum (1)) (EVar V_TIFFSetDirectory_z)))) 27)::
  (EA 27 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_TIFFSetDirectory => Pedges_TIFFSetDirectory
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_TIFFSetDirectory => 21
     end)%positive;
  var_global := var_global
}.

Definition ai_TIFFSetDirectory (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_TIFFSetDirectory_z <= 0 /\ -1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 3 => (-1 * s V_TIFFSetDirectory_z <= 0 /\ 1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 4 => (1 * s V_TIFFSetDirectory_z <= 0 /\ -1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 5 => (-1 * s V_TIFFSetDirectory_z <= 0 /\ 1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 6 => (-1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 7 => (-1 * s V_TIFFSetDirectory_z <= 0 /\ 1 * s V_TIFFSetDirectory_n <= 0)%Z
   | 8 => (-1 * s V_TIFFSetDirectory_z <= 0 /\ -1 * s V_TIFFSetDirectory_n + 1 <= 0)%Z
   | 9 => (-1 * s V_TIFFSetDirectory_n + 1 <= 0 /\ -1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 10 => (-1 * s V_TIFFSetDirectory_z <= 0 /\ -1 * s V_TIFFSetDirectory_n + 1 <= 0)%Z
   | 11 => (-1 * s V_TIFFSetDirectory_n + 1 <= 0 /\ -1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 12 => (-1 * s V_TIFFSetDirectory_z <= 0 /\ -1 * s V_TIFFSetDirectory_n + 1 <= 0)%Z
   | 13 => (-1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 14 => (-1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 15 => (-1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 16 => (-1 * s V_TIFFSetDirectory_z <= 0 /\ -1 * s V_TIFFSetDirectory_n + 1 <= 0)%Z
   | 17 => (-1 * s V_TIFFSetDirectory_n + 1 <= 0 /\ -1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 18 => (-1 * s V_TIFFSetDirectory_z <= 0 /\ -1 * s V_TIFFSetDirectory_n + 1 <= 0)%Z
   | 19 => (-1 * s V_TIFFSetDirectory_n + 1 <= 0 /\ -1 * s V_TIFFSetDirectory_z <= 0 /\ 1 * s V_TIFFSetDirectory__tmp <= 0 /\ -1 * s V_TIFFSetDirectory__tmp <= 0)%Z
   | 20 => (-1 * s V_TIFFSetDirectory__tmp <= 0 /\ 1 * s V_TIFFSetDirectory__tmp <= 0 /\ -1 * s V_TIFFSetDirectory_z <= 0 /\ -1 * s V_TIFFSetDirectory_n + 1 <= 0)%Z
   | 21 => (-1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 22 => (-1 * s V_TIFFSetDirectory_z <= 0 /\ -1 * s V_TIFFSetDirectory_n + 1 <= 0)%Z
   | 23 => (-1 * s V_TIFFSetDirectory_n + 1 <= 0 /\ -1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 24 => (-1 * s V_TIFFSetDirectory_z <= 0 /\ -1 * s V_TIFFSetDirectory_n <= 0)%Z
   | 25 => (-1 * s V_TIFFSetDirectory_n <= 0 /\ -1 * s V_TIFFSetDirectory_z <= 0)%Z
   | 26 => (-1 * s V_TIFFSetDirectory_z <= 0 /\ -1 * s V_TIFFSetDirectory_n <= 0)%Z
   | 27 => (-1 * s V_TIFFSetDirectory_n <= 0 /\ -1 * s V_TIFFSetDirectory_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_TIFFSetDirectory (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_TIFFSetDirectory_dirn) <= z)%Q
   | 2 => (max0(s V_TIFFSetDirectory_dirn) + max0(s V_TIFFSetDirectory_z) <= z)%Q
   | 3 => (max0(s V_TIFFSetDirectory__tmp1) + max0(s V_TIFFSetDirectory_z) <= z)%Q
   | 4 => (max0(s V_TIFFSetDirectory_n) + max0(s V_TIFFSetDirectory_z) <= z)%Q
   | 5 => (max0(s V_TIFFSetDirectory_n) + max0(s V_TIFFSetDirectory_z) <= z)%Q
   | 6 => (max0(s V_TIFFSetDirectory_n) + max0(s V_TIFFSetDirectory_z) <= z)%Q
   | 7 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_TIFFSetDirectory_n) (-1
                                                                    + 
                                                                    s V_TIFFSetDirectory_n));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_TIFFSetDirectory_z)) (F_check_ge (s V_TIFFSetDirectory_z) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_TIFFSetDirectory_n)) (F_check_ge (0) (0))]
     (max0(s V_TIFFSetDirectory_n) + max0(s V_TIFFSetDirectory_z) <= z)%Q
   | 8 => (max0(s V_TIFFSetDirectory_n) + max0(s V_TIFFSetDirectory_z) <= z)%Q
   | 9 => (max0(s V_TIFFSetDirectory_n) + max0(s V_TIFFSetDirectory_z) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_TIFFSetDirectory_z)) (F_check_ge (s V_TIFFSetDirectory_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_TIFFSetDirectory_n)) (F_check_ge (s V_TIFFSetDirectory_n) (0))]
     (max0(s V_TIFFSetDirectory_n) + max0(s V_TIFFSetDirectory_z) <= z)%Q
   | 11 => (s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | 12 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_TIFFSetDirectory_n) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFSetDirectory_n);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_TIFFSetDirectory_n) (0))) (F_max0_ge_0 (s V_TIFFSetDirectory_n))]
     (s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | 13 => (s V_TIFFSetDirectory_z <= z)%Q
   | 14 => (s V_TIFFSetDirectory_z <= z)%Q
   | 15 => (s V_TIFFSetDirectory_z <= z)%Q
   | 16 => (s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | 17 => (s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | 18 => (s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | 19 => (s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | 20 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_TIFFSetDirectory_n) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_TIFFSetDirectory_n);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_TIFFSetDirectory_n) (0))) (F_max0_ge_0 (s V_TIFFSetDirectory_n))]
     (s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | 21 => (s V_TIFFSetDirectory_z <= z)%Q
   | 22 => (s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | 23 => (s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | 24 => ((1 # 1) + s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | 25 => ((1 # 1) + s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | 26 => ((1 # 1) + s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | 27 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_TIFFSetDirectory_z) (0))) (F_max0_ge_0 (s V_TIFFSetDirectory_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_TIFFSetDirectory_n) (0))) (F_max0_ge_0 (s V_TIFFSetDirectory_n))]
     (s V_TIFFSetDirectory_n + s V_TIFFSetDirectory_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_TIFFSetDirectory =>
    [mkPA Q (fun n z s => ai_TIFFSetDirectory n s /\ annot0_TIFFSetDirectory n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_TIFFSetDirectory (proc_start P_TIFFSetDirectory) s1 (proc_end P_TIFFSetDirectory) s2 ->
    (s2 V_TIFFSetDirectory_z <= max0(s1 V_TIFFSetDirectory_dirn))%Q.
Proof.
  prove_bound ipa admissible_ipa P_TIFFSetDirectory.
Qed.
