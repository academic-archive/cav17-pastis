Require Import pasta.Pasta.

Inductive proc: Type :=
  P_heap_available.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_heap_available_z := 1%positive.
Notation V_heap_available_avail := 2%positive.
Notation V_heap_available_n := 3%positive.
Definition Pedges_heap_available: list (edge proc) :=
  (EA 1 (AAssign V_heap_available_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_heap_available_n) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_heap_available_avail
  (Some (ENum (0)))) 5)::(EA 5 (AAssign V_heap_available_n
  (Some (ENum (0)))) 6)::(EA 6 ANone 7)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_heap_available_n) s) < (eval (ENum (20))
  s))%Z)) 10)::(EA 8 (AGuard (fun s => ((eval (EVar V_heap_available_n) s) >=
  (eval (ENum (20)) s))%Z)) 9)::(EA 9 AWeaken 22)::(EA 10 AWeaken 11)::
  (EA 11 ANone 21)::(EA 11 ANone 12)::(EA 12 ANone 13)::(EA 13 ANone 14)::
  (EA 14 (AAssign V_heap_available_avail None) 15)::(EA 15 ANone 16)::
  (EA 16 (AAssign V_heap_available_n (Some (EAdd (EVar V_heap_available_n)
  (ENum (1))))) 17)::(EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_heap_available_z (Some (EAdd (ENum (1))
  (EVar V_heap_available_z)))) 20)::(EA 20 AWeaken 8)::(EA 21 ANone 22)::
  (EA 22 ANone 23)::(EA 23 AWeaken 24)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_heap_available_n) s) <> (eval (ENum (0))
  s))%Z)) 27)::(EA 24 (AGuard (fun s => ((eval (EVar V_heap_available_n) s) =
  (eval (ENum (0)) s))%Z)) 25)::(EA 25 AWeaken 26)::(EA 27 AWeaken 28)::
  (EA 28 (AAssign V_heap_available_n (Some (EAdd (EVar V_heap_available_n)
  (ENum (-1))))) 29)::(EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 (AAssign
  V_heap_available_z (Some (EAdd (ENum (1))
  (EVar V_heap_available_z)))) 32)::(EA 32 AWeaken 24)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_heap_available => Pedges_heap_available
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_heap_available => 26
     end)%positive;
  var_global := var_global
}.

Definition ai_heap_available (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_z <= 0)%Z
   | 3 => (-1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n <= 0)%Z
   | 4 => (-1 * s V_heap_available_n <= 0 /\ 1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_z <= 0)%Z
   | 5 => (-1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n <= 0 /\ 1 * s V_heap_available_avail <= 0 /\ -1 * s V_heap_available_avail <= 0)%Z
   | 6 => (-1 * s V_heap_available_avail <= 0 /\ 1 * s V_heap_available_avail <= 0 /\ 1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_n <= 0 /\ -1 * s V_heap_available_n <= 0)%Z
   | 7 => (-1 * s V_heap_available_n <= 0 /\ 1 * s V_heap_available_n <= 0 /\ -1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_avail <= 0 /\ -1 * s V_heap_available_avail <= 0)%Z
   | 8 => (-1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n <= 0 /\ 1 * s V_heap_available_n + -20 <= 0)%Z
   | 9 => (1 * s V_heap_available_n + -20 <= 0 /\ -1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n + 20 <= 0)%Z
   | 10 => (-1 * s V_heap_available_n <= 0 /\ -1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_n + -19 <= 0)%Z
   | 11 => (1 * s V_heap_available_n + -19 <= 0 /\ -1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n <= 0)%Z
   | 12 => (-1 * s V_heap_available_n <= 0 /\ -1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_n + -19 <= 0)%Z
   | 13 => (1 * s V_heap_available_n + -19 <= 0 /\ -1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n <= 0)%Z
   | 14 => (-1 * s V_heap_available_n <= 0 /\ -1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_n + -19 <= 0)%Z
   | 15 => (1 * s V_heap_available_n + -19 <= 0 /\ -1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n <= 0)%Z
   | 16 => (-1 * s V_heap_available_n <= 0 /\ -1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_n + -19 <= 0)%Z
   | 17 => (-1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n + 1 <= 0 /\ 1 * s V_heap_available_n + -20 <= 0)%Z
   | 18 => (1 * s V_heap_available_n + -20 <= 0 /\ -1 * s V_heap_available_n + 1 <= 0 /\ -1 * s V_heap_available_z <= 0)%Z
   | 19 => (-1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n + 1 <= 0 /\ 1 * s V_heap_available_n + -20 <= 0)%Z
   | 20 => (1 * s V_heap_available_n + -20 <= 0 /\ -1 * s V_heap_available_n + 1 <= 0 /\ -1 * s V_heap_available_z + 1 <= 0)%Z
   | 21 => (-1 * s V_heap_available_n <= 0 /\ -1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_n + -19 <= 0)%Z
   | 22 => (1 * s V_heap_available_n + -20 <= 0 /\ -1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n <= 0)%Z
   | 23 => (-1 * s V_heap_available_n <= 0 /\ -1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_n + -20 <= 0)%Z
   | 24 => (1 * s V_heap_available_n + -20 <= 0 /\ -1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n <= 0)%Z
   | 25 => (-1 * s V_heap_available_n <= 0 /\ -1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_n <= 0)%Z
   | 26 => (1 * s V_heap_available_n <= 0 /\ -1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n <= 0)%Z
   | 27 => (-1 * s V_heap_available_z <= 0 /\ -1 * s V_heap_available_n + 1 <= 0 /\ 1 * s V_heap_available_n + -20 <= 0)%Z
   | 28 => (1 * s V_heap_available_n + -20 <= 0 /\ -1 * s V_heap_available_n + 1 <= 0 /\ -1 * s V_heap_available_z <= 0)%Z
   | 29 => (-1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_n + -19 <= 0 /\ -1 * s V_heap_available_n <= 0)%Z
   | 30 => (-1 * s V_heap_available_n <= 0 /\ 1 * s V_heap_available_n + -19 <= 0 /\ -1 * s V_heap_available_z <= 0)%Z
   | 31 => (-1 * s V_heap_available_z <= 0 /\ 1 * s V_heap_available_n + -19 <= 0 /\ -1 * s V_heap_available_n <= 0)%Z
   | 32 => (-1 * s V_heap_available_n <= 0 /\ 1 * s V_heap_available_n + -19 <= 0 /\ -1 * s V_heap_available_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_heap_available (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((40 # 1) <= z)%Q
   | 2 => ((40 # 1) + max0(s V_heap_available_z) <= z)%Q
   | 3 => ((40 # 1) + max0(s V_heap_available_z) <= z)%Q
   | 4 => ((40 # 1) + max0(s V_heap_available_z) <= z)%Q
   | 5 => ((40 # 1) + max0(s V_heap_available_z) <= z)%Q
   | 6 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 7 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 8 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 9 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 10 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 11 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 12 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 13 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 14 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 15 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 16 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 17 => ((41 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 18 => ((41 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 19 => ((41 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_heap_available_z) (0))) (F_max0_ge_0 (s V_heap_available_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_heap_available_z)) (F_check_ge (-1
                                                                    + s V_heap_available_z) (0))]
     ((41 # 1) - s V_heap_available_n + max0(-1 + s V_heap_available_z) <= z)%Q
   | 21 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 22 => ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 23 => hints
     [(*-2 0*) F_max0_monotonic (F_check_ge (20 - s V_heap_available_n) (19
                                                                    - s V_heap_available_n));
      (*-2 0*) F_max0_ge_0 (19 - s V_heap_available_n);
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_heap_available_z)) (F_check_ge (s V_heap_available_z) (0));
      (*-2 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (20
                                                               - s V_heap_available_n) (0))) (F_max0_ge_0 (20
                                                                    - s V_heap_available_n))]
     ((40 # 1) - s V_heap_available_n + max0(s V_heap_available_z) <= z)%Q
   | 24 => (s V_heap_available_n + s V_heap_available_z <= z)%Q
   | 25 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_heap_available_n)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_heap_available_n) (0))) (F_max0_ge_0 (s V_heap_available_n))]
     (s V_heap_available_n + s V_heap_available_z <= z)%Q
   | 26 => (s V_heap_available_z <= z)%Q
   | 27 => (s V_heap_available_n + s V_heap_available_z <= z)%Q
   | 28 => (s V_heap_available_n + s V_heap_available_z <= z)%Q
   | 29 => ((1 # 1) + s V_heap_available_n + s V_heap_available_z <= z)%Q
   | 30 => ((1 # 1) + s V_heap_available_n + s V_heap_available_z <= z)%Q
   | 31 => ((1 # 1) + s V_heap_available_n + s V_heap_available_z <= z)%Q
   | 32 => (s V_heap_available_n + s V_heap_available_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_heap_available =>
    [mkPA Q (fun n z s => ai_heap_available n s /\ annot0_heap_available n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_heap_available (proc_start P_heap_available) s1 (proc_end P_heap_available) s2 ->
    (s2 V_heap_available_z <= (40 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_heap_available.
Qed.
