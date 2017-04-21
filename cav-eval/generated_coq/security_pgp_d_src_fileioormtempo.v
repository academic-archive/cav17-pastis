Require Import pasta.Pasta.

Inductive proc: Type :=
  P_rmtemp.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_rmtemp_z := 1%positive.
Notation V_rmtemp_i := 2%positive.
Notation V_rmtemp_verbose := 3%positive.
Notation V_rmtemp_name := 4%positive.
Definition Pedges_rmtemp: list (edge proc) :=
  (EA 1 (AAssign V_rmtemp_z (Some (ENum (0)))) 2)::(EA 2 (AAssign V_rmtemp_i
  (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_rmtemp_i) s) < (eval (ENum (8)) s))%Z)) 7)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_rmtemp_i) s) >= (eval (ENum (8))
  s))%Z)) 6)::(EA 6 AWeaken 19)::(EA 7 AWeaken 8)::(EA 8 ANone 9)::
  (EA 8 ANone 11)::(EA 9 AWeaken 10)::(EA 10 ANone 17)::(EA 10 ANone 11)::
  (EA 11 ANone 12)::(EA 12 (AAssign V_rmtemp_i (Some (EAdd (EVar V_rmtemp_i)
  (ENum (1))))) 13)::(EA 13 ANone 14)::(EA 14 ANone 15)::(EA 15 (AAssign
  V_rmtemp_z (Some (EAdd (ENum (1)) (EVar V_rmtemp_z)))) 16)::
  (EA 16 AWeaken 5)::(EA 17 ANone 18)::(EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_rmtemp_i) s) < (eval (ENum (8)) s))%Z)) 21)::
  (EA 19 (AGuard (fun s => ((eval (EVar V_rmtemp_i) s) >= (eval (ENum (8))
  s))%Z)) 20)::(EA 20 AWeaken 53)::(EA 21 AWeaken 22)::(EA 22 ANone 24)::
  (EA 22 ANone 23)::(EA 23 AWeaken 27)::(EA 24 AWeaken 25)::
  (EA 25 ANone 32)::(EA 25 ANone 26)::(EA 26 AWeaken 27)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_rmtemp_verbose) s) <> (eval (ENum (0))
  s))%Z)) 29)::(EA 27 (AGuard (fun s => ((eval (EVar V_rmtemp_verbose) s) =
  (eval (ENum (0)) s))%Z)) 28)::(EA 28 AWeaken 31)::(EA 29 AWeaken 30)::
  (EA 30 ANone 31)::(EA 31 ANone 51)::(EA 32 AWeaken 33)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_rmtemp_verbose) s) <> (eval (ENum (0))
  s))%Z)) 35)::(EA 33 (AGuard (fun s => ((eval (EVar V_rmtemp_verbose) s) =
  (eval (ENum (0)) s))%Z)) 34)::(EA 34 AWeaken 38)::(EA 35 AWeaken 36)::
  (EA 36 ANone 37)::(EA 37 AWeaken 38)::(EA 38 ANone 40)::(EA 38 ANone 39)::
  (EA 39 AWeaken 42)::(EA 40 ANone 41)::(EA 41 AWeaken 42)::
  (EA 42 ANone 44)::(EA 42 ANone 43)::(EA 43 ANone 50)::(EA 44 AWeaken 45)::
  (EA 45 (AGuard (fun s => ((eval (EVar V_rmtemp_verbose) s) <>
  (eval (ENum (0)) s))%Z)) 47)::(EA 45 (AGuard
  (fun s => ((eval (EVar V_rmtemp_verbose) s) = (eval (ENum (0))
  s))%Z)) 46)::(EA 46 AWeaken 49)::(EA 47 AWeaken 48)::(EA 48 ANone 49)::
  (EA 49 ANone 50)::(EA 50 ANone 51)::(EA 51 ANone 52)::(EA 52 AWeaken 53)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_rmtemp => Pedges_rmtemp
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_rmtemp => 53
     end)%positive;
  var_global := var_global
}.

Definition ai_rmtemp (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 3 => (-1 * s V_rmtemp_z <= 0 /\ 1 * s V_rmtemp_z <= 0 /\ 1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_i <= 0)%Z
   | 4 => (-1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 5 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -8 <= 0)%Z
   | 6 => (1 * s V_rmtemp_i + -8 <= 0 /\ -1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i + 8 <= 0)%Z
   | 7 => (-1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 8 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0)%Z
   | 9 => (-1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 10 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0)%Z
   | 11 => (-1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 12 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0)%Z
   | 13 => (-1 * s V_rmtemp_z <= 0 /\ 1 * s V_rmtemp_i + -8 <= 0 /\ -1 * s V_rmtemp_i + 1 <= 0)%Z
   | 14 => (-1 * s V_rmtemp_i + 1 <= 0 /\ 1 * s V_rmtemp_i + -8 <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 15 => (-1 * s V_rmtemp_z <= 0 /\ 1 * s V_rmtemp_i + -8 <= 0 /\ -1 * s V_rmtemp_i + 1 <= 0)%Z
   | 16 => (-1 * s V_rmtemp_i + 1 <= 0 /\ 1 * s V_rmtemp_i + -8 <= 0 /\ -1 * s V_rmtemp_z + 1 <= 0)%Z
   | 17 => (-1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 18 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0)%Z
   | 19 => (1 * s V_rmtemp_i + -8 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 20 => (-1 * s V_rmtemp_z <= 0 /\ 1 * s V_rmtemp_i + -8 <= 0 /\ -1 * s V_rmtemp_i + 8 <= 0)%Z
   | 21 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 22 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 23 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 24 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 25 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 26 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 27 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 28 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0 /\ 1 * s V_rmtemp_verbose <= 0 /\ -1 * s V_rmtemp_verbose <= 0)%Z
   | 29 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 30 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 31 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 32 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 33 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 34 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0 /\ 1 * s V_rmtemp_verbose <= 0 /\ -1 * s V_rmtemp_verbose <= 0)%Z
   | 35 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 36 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 37 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 38 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 39 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 40 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 41 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 42 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 43 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 44 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 45 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 46 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0 /\ 1 * s V_rmtemp_verbose <= 0 /\ -1 * s V_rmtemp_verbose <= 0)%Z
   | 47 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 48 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 49 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 50 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 51 => (1 * s V_rmtemp_i + -7 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | 52 => (-1 * s V_rmtemp_z <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ 1 * s V_rmtemp_i + -7 <= 0)%Z
   | 53 => (1 * s V_rmtemp_i + -8 <= 0 /\ -1 * s V_rmtemp_i <= 0 /\ -1 * s V_rmtemp_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_rmtemp (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_rmtemp_z <= z)%Q
   | 3 => (s V_rmtemp_z + max0(8 - s V_rmtemp_i) <= z)%Q
   | 4 => (s V_rmtemp_z + max0(8 - s V_rmtemp_i) <= z)%Q
   | 5 => (s V_rmtemp_z + max0(8 - s V_rmtemp_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_rmtemp_i) (7
                                                                - s V_rmtemp_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_rmtemp_i)]
     (s V_rmtemp_z + max0(8 - s V_rmtemp_i) <= z)%Q
   | 7 => hints
     [(*-1.40406e-12 1*) F_max0_pre_decrement 1 (8 - s V_rmtemp_i) (1)]
     (s V_rmtemp_z + max0(8 - s V_rmtemp_i) <= z)%Q
   | 8 => ((1 # 1) + s V_rmtemp_z + max0(7 - s V_rmtemp_i) <= z)%Q
   | 9 => ((1 # 1) + s V_rmtemp_z + max0(7 - s V_rmtemp_i) <= z)%Q
   | 10 => ((1 # 1) + s V_rmtemp_z + max0(7 - s V_rmtemp_i) <= z)%Q
   | 11 => ((1 # 1) + s V_rmtemp_z + max0(7 - s V_rmtemp_i) <= z)%Q
   | 12 => ((1 # 1) + s V_rmtemp_z + max0(7 - s V_rmtemp_i) <= z)%Q
   | 13 => ((1 # 1) + s V_rmtemp_z + max0(8 - s V_rmtemp_i) <= z)%Q
   | 14 => ((1 # 1) + s V_rmtemp_z + max0(8 - s V_rmtemp_i) <= z)%Q
   | 15 => ((1 # 1) + s V_rmtemp_z + max0(8 - s V_rmtemp_i) <= z)%Q
   | 16 => (s V_rmtemp_z + max0(8 - s V_rmtemp_i) <= z)%Q
   | 17 => ((1 # 1) + s V_rmtemp_z + max0(7 - s V_rmtemp_i) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_one; (*-1 0*) F_max0_ge_0 (7 - s V_rmtemp_i)]
     ((1 # 1) + s V_rmtemp_z + max0(7 - s V_rmtemp_i) <= z)%Q
   | 19 => (s V_rmtemp_z <= z)%Q
   | 20 => (s V_rmtemp_z <= z)%Q
   | 21 => (s V_rmtemp_z <= z)%Q
   | 22 => (s V_rmtemp_z <= z)%Q
   | 23 => (s V_rmtemp_z <= z)%Q
   | 24 => (s V_rmtemp_z <= z)%Q
   | 25 => (s V_rmtemp_z <= z)%Q
   | 26 => (s V_rmtemp_z <= z)%Q
   | 27 => (s V_rmtemp_z <= z)%Q
   | 28 => (s V_rmtemp_z <= z)%Q
   | 29 => (s V_rmtemp_z <= z)%Q
   | 30 => (s V_rmtemp_z <= z)%Q
   | 31 => (s V_rmtemp_z <= z)%Q
   | 32 => (s V_rmtemp_z <= z)%Q
   | 33 => (s V_rmtemp_z <= z)%Q
   | 34 => (s V_rmtemp_z <= z)%Q
   | 35 => (s V_rmtemp_z <= z)%Q
   | 36 => (s V_rmtemp_z <= z)%Q
   | 37 => (s V_rmtemp_z <= z)%Q
   | 38 => (s V_rmtemp_z <= z)%Q
   | 39 => (s V_rmtemp_z <= z)%Q
   | 40 => (s V_rmtemp_z <= z)%Q
   | 41 => (s V_rmtemp_z <= z)%Q
   | 42 => (s V_rmtemp_z <= z)%Q
   | 43 => (s V_rmtemp_z <= z)%Q
   | 44 => (s V_rmtemp_z <= z)%Q
   | 45 => (s V_rmtemp_z <= z)%Q
   | 46 => (s V_rmtemp_z <= z)%Q
   | 47 => (s V_rmtemp_z <= z)%Q
   | 48 => (s V_rmtemp_z <= z)%Q
   | 49 => (s V_rmtemp_z <= z)%Q
   | 50 => (s V_rmtemp_z <= z)%Q
   | 51 => (s V_rmtemp_z <= z)%Q
   | 52 => (s V_rmtemp_z <= z)%Q
   | 53 => (s V_rmtemp_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_rmtemp =>
    [mkPA Q (fun n z s => ai_rmtemp n s /\ annot0_rmtemp n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_rmtemp (proc_start P_rmtemp) s1 (proc_end P_rmtemp) s2 ->
    (s2 V_rmtemp_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_rmtemp.
Qed.
