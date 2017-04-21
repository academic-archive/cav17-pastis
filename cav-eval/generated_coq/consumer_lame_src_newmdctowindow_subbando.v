Require Import pasta.Pasta.

Inductive proc: Type :=
  P_window_subband.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_window_subband_z := 1%positive.
Notation V_window_subband_i := 2%positive.
Notation V_window_subband_j := 3%positive.
Notation V_window_subband_d := 4%positive.
Notation V_window_subband_in := 5%positive.
Notation V_window_subband_xk := 6%positive.
Definition Pedges_window_subband: list (edge proc) :=
  (EA 1 (AAssign V_window_subband_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_window_subband_i (Some (ENum (14)))) 3)::(EA 3 ANone 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard (fun s => ((eval (EVar V_window_subband_i)
  s) >= (eval (ENum (0)) s))%Z)) 32)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_window_subband_i) s) < (eval (ENum (0))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign V_window_subband_i
  (Some (ENum (15)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_window_subband_i) s) >= (eval (ENum (0))
  s))%Z)) 13)::(EA 10 (AGuard (fun s => ((eval (EVar V_window_subband_i) s) <
  (eval (ENum (0)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 13 AWeaken 14)::
  (EA 14 (AAssign V_window_subband_j (Some (ENum (14)))) 15)::
  (EA 15 ANone 16)::(EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_window_subband_j) s) >= (eval (ENum (0))
  s))%Z)) 25)::(EA 17 (AGuard (fun s => ((eval (EVar V_window_subband_j) s) <
  (eval (ENum (0)) s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 ANone 20)::
  (EA 20 (AAssign V_window_subband_i (Some (EAdd (EVar V_window_subband_i)
  (ENum (-1))))) 21)::(EA 21 ANone 22)::(EA 22 ANone 23)::(EA 23 (AAssign
  V_window_subband_z (Some (EAdd (ENum (1))
  (EVar V_window_subband_z)))) 24)::(EA 24 AWeaken 10)::(EA 25 AWeaken 26)::
  (EA 26 ANone 27)::(EA 27 (AAssign V_window_subband_j
  (Some (EAdd (EVar V_window_subband_j) (ENum (-1))))) 28)::
  (EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign V_window_subband_z
  (Some (EAdd (ENum (1)) (EVar V_window_subband_z)))) 31)::
  (EA 31 AWeaken 17)::(EA 32 AWeaken 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_window_subband_i (Some (EAdd (EVar V_window_subband_i)
  (ENum (-1))))) 35)::(EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign
  V_window_subband_z (Some (EAdd (ENum (1))
  (EVar V_window_subband_z)))) 38)::(EA 38 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_window_subband => Pedges_window_subband
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_window_subband => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_window_subband (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_window_subband_z <= 0 /\ -1 * s V_window_subband_z <= 0)%Z
   | 3 => (-1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + -14 <= 0 /\ -1 * s V_window_subband_i + 14 <= 0)%Z
   | 4 => (-1 * s V_window_subband_i + 14 <= 0 /\ 1 * s V_window_subband_i + -14 <= 0 /\ 1 * s V_window_subband_z <= 0 /\ -1 * s V_window_subband_z <= 0)%Z
   | 5 => (-1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + -14 <= 0 /\ -1 * s V_window_subband_i + -1 <= 0)%Z
   | 6 => (-1 * s V_window_subband_i + -1 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + 1 <= 0)%Z
   | 7 => (1 * s V_window_subband_i + 1 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ -1 * s V_window_subband_i + -1 <= 0)%Z
   | 8 => (-1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + -15 <= 0 /\ -1 * s V_window_subband_i + 15 <= 0)%Z
   | 9 => (-1 * s V_window_subband_i + 15 <= 0 /\ 1 * s V_window_subband_i + -15 <= 0 /\ -1 * s V_window_subband_z <= 0)%Z
   | 10 => (-1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + -15 <= 0)%Z
   | 11 => (-1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + 1 <= 0)%Z
   | 12 => (1 * s V_window_subband_i + 1 <= 0 /\ -1 * s V_window_subband_z <= 0)%Z
   | 13 => (1 * s V_window_subband_i + -15 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ -1 * s V_window_subband_i <= 0)%Z
   | 14 => (-1 * s V_window_subband_i <= 0 /\ -1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + -15 <= 0)%Z
   | 15 => (1 * s V_window_subband_i + -15 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ -1 * s V_window_subband_i <= 0 /\ 1 * s V_window_subband_j + -14 <= 0 /\ -1 * s V_window_subband_j + 14 <= 0)%Z
   | 16 => (-1 * s V_window_subband_j + 14 <= 0 /\ 1 * s V_window_subband_j + -14 <= 0 /\ -1 * s V_window_subband_i <= 0 /\ -1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + -15 <= 0)%Z
   | 17 => (-1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_j + -14 <= 0 /\ 1 * s V_window_subband_i + -15 <= 0 /\ -1 * s V_window_subband_j + -1 <= 0)%Z
   | 18 => (-1 * s V_window_subband_j + -1 <= 0 /\ 1 * s V_window_subband_i + -15 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_j + 1 <= 0)%Z
   | 19 => (1 * s V_window_subband_j + 1 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + -15 <= 0 /\ -1 * s V_window_subband_j + -1 <= 0)%Z
   | 20 => (-1 * s V_window_subband_j + -1 <= 0 /\ 1 * s V_window_subband_i + -15 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_j + 1 <= 0)%Z
   | 21 => (1 * s V_window_subband_j + 1 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ -1 * s V_window_subband_j + -1 <= 0 /\ 1 * s V_window_subband_i + -14 <= 0)%Z
   | 22 => (1 * s V_window_subband_i + -14 <= 0 /\ -1 * s V_window_subband_j + -1 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_j + 1 <= 0)%Z
   | 23 => (1 * s V_window_subband_j + 1 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ -1 * s V_window_subband_j + -1 <= 0 /\ 1 * s V_window_subband_i + -14 <= 0)%Z
   | 24 => (1 * s V_window_subband_i + -14 <= 0 /\ -1 * s V_window_subband_j + -1 <= 0 /\ 1 * s V_window_subband_j + 1 <= 0 /\ -1 * s V_window_subband_z + 1 <= 0)%Z
   | 25 => (1 * s V_window_subband_i + -15 <= 0 /\ 1 * s V_window_subband_j + -14 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ -1 * s V_window_subband_j <= 0)%Z
   | 26 => (-1 * s V_window_subband_j <= 0 /\ -1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_j + -14 <= 0 /\ 1 * s V_window_subband_i + -15 <= 0)%Z
   | 27 => (1 * s V_window_subband_i + -15 <= 0 /\ 1 * s V_window_subband_j + -14 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ -1 * s V_window_subband_j <= 0)%Z
   | 28 => (-1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + -15 <= 0 /\ 1 * s V_window_subband_j + -13 <= 0 /\ -1 * s V_window_subband_j + -1 <= 0)%Z
   | 29 => (-1 * s V_window_subband_j + -1 <= 0 /\ 1 * s V_window_subband_j + -13 <= 0 /\ 1 * s V_window_subband_i + -15 <= 0 /\ -1 * s V_window_subband_z <= 0)%Z
   | 30 => (-1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + -15 <= 0 /\ 1 * s V_window_subband_j + -13 <= 0 /\ -1 * s V_window_subband_j + -1 <= 0)%Z
   | 31 => (-1 * s V_window_subband_j + -1 <= 0 /\ 1 * s V_window_subband_j + -13 <= 0 /\ 1 * s V_window_subband_i + -15 <= 0 /\ -1 * s V_window_subband_z + 1 <= 0)%Z
   | 32 => (1 * s V_window_subband_i + -14 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ -1 * s V_window_subband_i <= 0)%Z
   | 33 => (-1 * s V_window_subband_i <= 0 /\ -1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + -14 <= 0)%Z
   | 34 => (1 * s V_window_subband_i + -14 <= 0 /\ -1 * s V_window_subband_z <= 0 /\ -1 * s V_window_subband_i <= 0)%Z
   | 35 => (-1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + -13 <= 0 /\ -1 * s V_window_subband_i + -1 <= 0)%Z
   | 36 => (-1 * s V_window_subband_i + -1 <= 0 /\ 1 * s V_window_subband_i + -13 <= 0 /\ -1 * s V_window_subband_z <= 0)%Z
   | 37 => (-1 * s V_window_subband_z <= 0 /\ 1 * s V_window_subband_i + -13 <= 0 /\ -1 * s V_window_subband_i + -1 <= 0)%Z
   | 38 => (-1 * s V_window_subband_i + -1 <= 0 /\ 1 * s V_window_subband_i + -13 <= 0 /\ -1 * s V_window_subband_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_window_subband (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((271 # 1) <= z)%Q
   | 2 => ((271 # 1) + s V_window_subband_z <= z)%Q
   | 3 => ((256 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_i) <= z)%Q
   | 4 => ((256 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_i) <= z)%Q
   | 5 => ((256 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_window_subband_i) (s V_window_subband_i));
      (*-1 0*) F_max0_ge_0 (s V_window_subband_i)]
     ((256 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_i) <= z)%Q
   | 7 => ((256 # 1) + s V_window_subband_z <= z)%Q
   | 8 => (s V_window_subband_z + (16 # 1) * max0(1 + s V_window_subband_i) <= z)%Q
   | 9 => (s V_window_subband_z + (16 # 1) * max0(1 + s V_window_subband_i) <= z)%Q
   | 10 => (s V_window_subband_z + (16 # 1) * max0(1 + s V_window_subband_i) <= z)%Q
   | 11 => hints
     [(*-16 0*) F_max0_monotonic (F_check_ge (1 + s V_window_subband_i) (s V_window_subband_i));
      (*-16 0*) F_max0_ge_0 (s V_window_subband_i)]
     (s V_window_subband_z + (16 # 1) * max0(1 + s V_window_subband_i) <= z)%Q
   | 12 => (s V_window_subband_z <= z)%Q
   | 13 => hints
     [(*-1.40285e-12 16*) F_max0_pre_decrement 1 (1 + s V_window_subband_i) (1)]
     (s V_window_subband_z + (16 # 1) * max0(1 + s V_window_subband_i) <= z)%Q
   | 14 => ((16 # 1) + s V_window_subband_z
            + (16 # 1) * max0(s V_window_subband_i) <= z)%Q
   | 15 => ((1 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_j)
            + (16 # 1) * max0(s V_window_subband_i) <= z)%Q
   | 16 => ((1 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_j)
            + (16 # 1) * max0(s V_window_subband_i) <= z)%Q
   | 17 => ((1 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_j)
            + (16 # 1) * max0(s V_window_subband_i) <= z)%Q
   | 18 => ((1 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_j)
            + (16 # 1) * max0(s V_window_subband_i) <= z)%Q
   | 19 => ((1 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_j)
            + (16 # 1) * max0(s V_window_subband_i) <= z)%Q
   | 20 => ((1 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_j)
            + (16 # 1) * max0(s V_window_subband_i) <= z)%Q
   | 21 => ((1 # 1) + s V_window_subband_z
            + (16 # 1) * max0(1 + s V_window_subband_i)
            + max0(1 + s V_window_subband_j) <= z)%Q
   | 22 => ((1 # 1) + s V_window_subband_z
            + (16 # 1) * max0(1 + s V_window_subband_i)
            + max0(1 + s V_window_subband_j) <= z)%Q
   | 23 => ((1 # 1) + s V_window_subband_z
            + (16 # 1) * max0(1 + s V_window_subband_i)
            + max0(1 + s V_window_subband_j) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1 + s V_window_subband_j)) (F_check_ge (0) (0))]
     (s V_window_subband_z + (16 # 1) * max0(1 + s V_window_subband_i)
      + max0(1 + s V_window_subband_j) <= z)%Q
   | 25 => hints
     [(*0 1*) F_max0_pre_decrement 1 (1 + s V_window_subband_j) (1)]
     ((1 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_j)
      + (16 # 1) * max0(s V_window_subband_i) <= z)%Q
   | 26 => ((2 # 1) + s V_window_subband_z
            + (16 # 1) * max0(s V_window_subband_i)
            + max0(s V_window_subband_j) <= z)%Q
   | 27 => ((2 # 1) + s V_window_subband_z
            + (16 # 1) * max0(s V_window_subband_i)
            + max0(s V_window_subband_j) <= z)%Q
   | 28 => ((2 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_j)
            + (16 # 1) * max0(s V_window_subband_i) <= z)%Q
   | 29 => ((2 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_j)
            + (16 # 1) * max0(s V_window_subband_i) <= z)%Q
   | 30 => ((2 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_j)
            + (16 # 1) * max0(s V_window_subband_i) <= z)%Q
   | 31 => ((1 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_j)
            + (16 # 1) * max0(s V_window_subband_i) <= z)%Q
   | 32 => hints
     [(*0 1*) F_max0_pre_decrement 1 (1 + s V_window_subband_i) (1)]
     ((256 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_i) <= z)%Q
   | 33 => ((257 # 1) + s V_window_subband_z + max0(s V_window_subband_i) <= z)%Q
   | 34 => ((257 # 1) + s V_window_subband_z + max0(s V_window_subband_i) <= z)%Q
   | 35 => ((257 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_i) <= z)%Q
   | 36 => ((257 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_i) <= z)%Q
   | 37 => ((257 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_i) <= z)%Q
   | 38 => ((256 # 1) + s V_window_subband_z + max0(1 + s V_window_subband_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_window_subband =>
    [mkPA Q (fun n z s => ai_window_subband n s /\ annot0_window_subband n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_window_subband (proc_start P_window_subband) s1 (proc_end P_window_subband) s2 ->
    (s2 V_window_subband_z <= (271 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_window_subband.
Qed.
