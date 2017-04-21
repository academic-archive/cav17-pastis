Require Import pasta.Pasta.

Inductive proc: Type :=
  P_savetemp.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_savetemp_z := 1%positive.
Notation V_savetemp_i := 2%positive.
Notation V_savetemp_verbose := 3%positive.
Notation V_savetemp_name := 4%positive.
Notation V_savetemp_newname := 5%positive.
Definition Pedges_savetemp: list (edge proc) :=
  (EA 1 (AAssign V_savetemp_z (Some (ENum (0)))) 2)::(EA 2 AWeaken 3)::
  (EA 3 ANone 56)::(EA 3 ANone 4)::(EA 4 (AAssign V_savetemp_i
  (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_savetemp_i) s) < (eval (ENum (8)) s))%Z)) 9)::
  (EA 7 (AGuard (fun s => ((eval (EVar V_savetemp_i) s) >= (eval (ENum (8))
  s))%Z)) 8)::(EA 8 AWeaken 21)::(EA 9 AWeaken 10)::(EA 10 ANone 11)::
  (EA 10 ANone 13)::(EA 11 AWeaken 12)::(EA 12 ANone 19)::(EA 12 ANone 13)::
  (EA 13 ANone 14)::(EA 14 (AAssign V_savetemp_i
  (Some (EAdd (EVar V_savetemp_i) (ENum (1))))) 15)::(EA 15 ANone 16)::
  (EA 16 ANone 17)::(EA 17 (AAssign V_savetemp_z (Some (EAdd (ENum (1))
  (EVar V_savetemp_z)))) 18)::(EA 18 AWeaken 7)::(EA 19 ANone 20)::
  (EA 20 AWeaken 21)::(EA 21 (AGuard (fun s => ((eval (EVar V_savetemp_i)
  s) < (eval (ENum (8)) s))%Z)) 23)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_savetemp_i) s) >= (eval (ENum (8)) s))%Z)) 22)::
  (EA 22 AWeaken 29)::(EA 23 AWeaken 24)::(EA 24 ANone 49)::
  (EA 24 ANone 25)::(EA 25 AWeaken 26)::(EA 26 ANone 48)::(EA 26 ANone 27)::
  (EA 27 ANone 28)::(EA 28 AWeaken 29)::(EA 29 ANone 46)::(EA 29 ANone 30)::
  (EA 30 AWeaken 31)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_savetemp_verbose) s) <> (eval (ENum (0))
  s))%Z)) 33)::(EA 31 (AGuard (fun s => ((eval (EVar V_savetemp_verbose) s) =
  (eval (ENum (0)) s))%Z)) 32)::(EA 32 AWeaken 36)::(EA 33 AWeaken 34)::
  (EA 34 ANone 35)::(EA 35 AWeaken 36)::(EA 36 ANone 44)::(EA 36 ANone 37)::
  (EA 37 AWeaken 38)::(EA 38 (AGuard (fun s => ((eval (EVar V_savetemp_i)
  s) < (eval (ENum (8)) s))%Z)) 40)::(EA 38 (AGuard
  (fun s => ((eval (EVar V_savetemp_i) s) >= (eval (ENum (8)) s))%Z)) 39)::
  (EA 39 AWeaken 42)::(EA 40 AWeaken 41)::(EA 41 ANone 42)::
  (EA 42 ANone 43)::(EA 43 AWeaken 58)::(EA 44 ANone 45)::
  (EA 45 AWeaken 58)::(EA 46 ANone 47)::(EA 47 AWeaken 58)::
  (EA 48 AWeaken 50)::(EA 49 AWeaken 50)::(EA 50 (AGuard
  (fun s => ((eval (EVar V_savetemp_verbose) s) <> (eval (ENum (0))
  s))%Z)) 52)::(EA 50 (AGuard (fun s => ((eval (EVar V_savetemp_verbose) s) =
  (eval (ENum (0)) s))%Z)) 51)::(EA 51 AWeaken 54)::(EA 52 AWeaken 53)::
  (EA 53 ANone 54)::(EA 54 ANone 55)::(EA 55 AWeaken 58)::(EA 56 ANone 57)::
  (EA 57 AWeaken 58)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_savetemp => Pedges_savetemp
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_savetemp => 58
     end)%positive;
  var_global := var_global
}.

Definition ai_savetemp (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 3 => (-1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_z <= 0)%Z
   | 4 => (1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 5 => (-1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 6 => (-1 * s V_savetemp_i <= 0 /\ 1 * s V_savetemp_i <= 0 /\ 1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 7 => (-1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0 /\ 1 * s V_savetemp_i + -8 <= 0)%Z
   | 8 => (1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i + 8 <= 0)%Z
   | 9 => (-1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -7 <= 0)%Z
   | 10 => (1 * s V_savetemp_i + -7 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 11 => (-1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -7 <= 0)%Z
   | 12 => (1 * s V_savetemp_i + -7 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 13 => (-1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -7 <= 0)%Z
   | 14 => (1 * s V_savetemp_i + -7 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 15 => (-1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_i + 1 <= 0)%Z
   | 16 => (-1 * s V_savetemp_i + 1 <= 0 /\ 1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 17 => (-1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_i + 1 <= 0)%Z
   | 18 => (-1 * s V_savetemp_i + 1 <= 0 /\ 1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_z + 1 <= 0)%Z
   | 19 => (-1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -7 <= 0)%Z
   | 20 => (1 * s V_savetemp_i + -7 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 21 => (1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 22 => (-1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_i + 8 <= 0)%Z
   | 23 => (-1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0 /\ 1 * s V_savetemp_i + -7 <= 0)%Z
   | 24 => (1 * s V_savetemp_i + -7 <= 0 /\ -1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 25 => (-1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0 /\ 1 * s V_savetemp_i + -7 <= 0)%Z
   | 26 => (1 * s V_savetemp_i + -7 <= 0 /\ -1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 27 => (-1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0 /\ 1 * s V_savetemp_i + -7 <= 0)%Z
   | 28 => (1 * s V_savetemp_i + -7 <= 0 /\ -1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 29 => (1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 30 => (-1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -8 <= 0)%Z
   | 31 => (1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 32 => (-1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -8 <= 0 /\ 1 * s V_savetemp_verbose <= 0 /\ -1 * s V_savetemp_verbose <= 0)%Z
   | 33 => (-1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -8 <= 0)%Z
   | 34 => (1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 35 => (-1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -8 <= 0)%Z
   | 36 => (1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 37 => (-1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -8 <= 0)%Z
   | 38 => (1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 39 => (-1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_i + 8 <= 0)%Z
   | 40 => (-1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -7 <= 0)%Z
   | 41 => (1 * s V_savetemp_i + -7 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 42 => (1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 43 => (-1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0 /\ 1 * s V_savetemp_i + -8 <= 0)%Z
   | 44 => (-1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -8 <= 0)%Z
   | 45 => (1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 46 => (-1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_i + -8 <= 0)%Z
   | 47 => (1 * s V_savetemp_i + -8 <= 0 /\ -1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0)%Z
   | 48 => (-1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0 /\ 1 * s V_savetemp_i + -7 <= 0)%Z
   | 49 => (-1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0 /\ 1 * s V_savetemp_i + -7 <= 0)%Z
   | 50 => (1 * s V_savetemp_i + -7 <= 0 /\ -1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 51 => (-1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0 /\ 1 * s V_savetemp_i + -7 <= 0 /\ 1 * s V_savetemp_verbose <= 0 /\ -1 * s V_savetemp_verbose <= 0)%Z
   | 52 => (-1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0 /\ 1 * s V_savetemp_i + -7 <= 0)%Z
   | 53 => (1 * s V_savetemp_i + -7 <= 0 /\ -1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 54 => (-1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_i <= 0 /\ 1 * s V_savetemp_i + -7 <= 0)%Z
   | 55 => (1 * s V_savetemp_i + -7 <= 0 /\ -1 * s V_savetemp_i <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 56 => (1 * s V_savetemp_z <= 0 /\ -1 * s V_savetemp_z <= 0)%Z
   | 57 => (-1 * s V_savetemp_z <= 0 /\ 1 * s V_savetemp_z <= 0)%Z
   | 58 => (-1 * s V_savetemp_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_savetemp (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_savetemp_z <= z)%Q
   | 3 => ((8 # 1) + s V_savetemp_z <= z)%Q
   | 4 => ((8 # 1) + s V_savetemp_z <= z)%Q
   | 5 => ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 6 => ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 7 => ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_savetemp_i) (7
                                                                  - s V_savetemp_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_savetemp_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                               - s V_savetemp_i) (0))) (F_max0_ge_0 (8
                                                                    - s V_savetemp_i))]
     ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 9 => ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 10 => ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 11 => ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 12 => ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 13 => ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 14 => ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 15 => ((9 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 16 => ((9 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 17 => ((9 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 18 => ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 19 => ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 20 => hints
     [(*-1.14286 0*) F_max0_ge_0 (7 - s V_savetemp_i);
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_savetemp_i)) (F_check_ge (0) (0));
      (*-0.142857 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_savetemp_i) (0))) (F_max0_ge_0 (s V_savetemp_i));
      (*-1.14286 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                    - 
                                                                    s V_savetemp_i) (0))) (F_max0_ge_0 (7
                                                                    - s V_savetemp_i))]
     ((8 # 1) - s V_savetemp_i + s V_savetemp_z <= z)%Q
   | 21 => (s V_savetemp_z <= z)%Q
   | 22 => (s V_savetemp_z <= z)%Q
   | 23 => (s V_savetemp_z <= z)%Q
   | 24 => (s V_savetemp_z <= z)%Q
   | 25 => (s V_savetemp_z <= z)%Q
   | 26 => (s V_savetemp_z <= z)%Q
   | 27 => (s V_savetemp_z <= z)%Q
   | 28 => (s V_savetemp_z <= z)%Q
   | 29 => (s V_savetemp_z <= z)%Q
   | 30 => (s V_savetemp_z <= z)%Q
   | 31 => (s V_savetemp_z <= z)%Q
   | 32 => (s V_savetemp_z <= z)%Q
   | 33 => (s V_savetemp_z <= z)%Q
   | 34 => (s V_savetemp_z <= z)%Q
   | 35 => (s V_savetemp_z <= z)%Q
   | 36 => (s V_savetemp_z <= z)%Q
   | 37 => (s V_savetemp_z <= z)%Q
   | 38 => (s V_savetemp_z <= z)%Q
   | 39 => (s V_savetemp_z <= z)%Q
   | 40 => (s V_savetemp_z <= z)%Q
   | 41 => (s V_savetemp_z <= z)%Q
   | 42 => (s V_savetemp_z <= z)%Q
   | 43 => (s V_savetemp_z <= z)%Q
   | 44 => (s V_savetemp_z <= z)%Q
   | 45 => (s V_savetemp_z <= z)%Q
   | 46 => (s V_savetemp_z <= z)%Q
   | 47 => (s V_savetemp_z <= z)%Q
   | 48 => (s V_savetemp_z <= z)%Q
   | 49 => (s V_savetemp_z <= z)%Q
   | 50 => (s V_savetemp_z <= z)%Q
   | 51 => (s V_savetemp_z <= z)%Q
   | 52 => (s V_savetemp_z <= z)%Q
   | 53 => (s V_savetemp_z <= z)%Q
   | 54 => (s V_savetemp_z <= z)%Q
   | 55 => (s V_savetemp_z <= z)%Q
   | 56 => ((8 # 1) + s V_savetemp_z <= z)%Q
   | 57 => hints
     [(*-8 0*) F_one]
     ((8 # 1) + s V_savetemp_z <= z)%Q
   | 58 => (s V_savetemp_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_savetemp =>
    [mkPA Q (fun n z s => ai_savetemp n s /\ annot0_savetemp n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_savetemp (proc_start P_savetemp) s1 (proc_end P_savetemp) s2 ->
    (s2 V_savetemp_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_savetemp.
Qed.
