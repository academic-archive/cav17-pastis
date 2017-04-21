Require Import pasta.Pasta.

Inductive proc: Type :=
  P_mdct_short.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_mdct_short_z := 1%positive.
Notation V_mdct_short_l := 2%positive.
Notation V_mdct_short_m := 3%positive.
Notation V_mdct_short_in := 4%positive.
Notation V_mdct_short_out := 5%positive.
Definition Pedges_mdct_short: list (edge proc) :=
  (EA 1 (AAssign V_mdct_short_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_mdct_short_m (Some (ENum (5)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_mdct_short_m) s) >= (eval (ENum (0))
  s))%Z)) 8)::(EA 5 (AGuard (fun s => ((eval (EVar V_mdct_short_m) s) <
  (eval (ENum (0)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 8 AWeaken 9)::
  (EA 9 (AAssign V_mdct_short_l (Some (ENum (2)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard (fun s => ((eval (EVar V_mdct_short_l)
  s) >= (eval (ENum (0)) s))%Z)) 20)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_mdct_short_l) s) < (eval (ENum (0)) s))%Z)) 13)::
  (EA 13 AWeaken 14)::(EA 14 ANone 15)::(EA 15 (AAssign V_mdct_short_m
  (Some (EAdd (EVar V_mdct_short_m) (ENum (-1))))) 16)::(EA 16 ANone 17)::
  (EA 17 ANone 18)::(EA 18 (AAssign V_mdct_short_z (Some (EAdd (ENum (1))
  (EVar V_mdct_short_z)))) 19)::(EA 19 AWeaken 5)::(EA 20 AWeaken 21)::
  (EA 21 ANone 22)::(EA 22 (AAssign V_mdct_short_l
  (Some (EAdd (EVar V_mdct_short_l) (ENum (-1))))) 23)::(EA 23 ANone 24)::
  (EA 24 ANone 25)::(EA 25 (AAssign V_mdct_short_z (Some (EAdd (ENum (1))
  (EVar V_mdct_short_z)))) 26)::(EA 26 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_mdct_short => Pedges_mdct_short
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_mdct_short => 7
     end)%positive;
  var_global := var_global
}.

Definition ai_mdct_short (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_mdct_short_z <= 0 /\ -1 * s V_mdct_short_z <= 0)%Z
   | 3 => (-1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0 /\ -1 * s V_mdct_short_m + 5 <= 0)%Z
   | 4 => (-1 * s V_mdct_short_m + 5 <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0 /\ 1 * s V_mdct_short_z <= 0 /\ -1 * s V_mdct_short_z <= 0)%Z
   | 5 => (-1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0)%Z
   | 6 => (-1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_m + 1 <= 0)%Z
   | 7 => (1 * s V_mdct_short_m + 1 <= 0 /\ -1 * s V_mdct_short_z <= 0)%Z
   | 8 => (1 * s V_mdct_short_m + -5 <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ -1 * s V_mdct_short_m <= 0)%Z
   | 9 => (-1 * s V_mdct_short_m <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0)%Z
   | 10 => (1 * s V_mdct_short_m + -5 <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ -1 * s V_mdct_short_m <= 0 /\ 1 * s V_mdct_short_l + -2 <= 0 /\ -1 * s V_mdct_short_l + 2 <= 0)%Z
   | 11 => (-1 * s V_mdct_short_l + 2 <= 0 /\ 1 * s V_mdct_short_l + -2 <= 0 /\ -1 * s V_mdct_short_m <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0)%Z
   | 12 => (-1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_l + -2 <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0 /\ -1 * s V_mdct_short_l + -1 <= 0)%Z
   | 13 => (-1 * s V_mdct_short_l + -1 <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_l + 1 <= 0)%Z
   | 14 => (1 * s V_mdct_short_l + 1 <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0 /\ -1 * s V_mdct_short_l + -1 <= 0)%Z
   | 15 => (-1 * s V_mdct_short_l + -1 <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_l + 1 <= 0)%Z
   | 16 => (1 * s V_mdct_short_l + 1 <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ -1 * s V_mdct_short_l + -1 <= 0 /\ 1 * s V_mdct_short_m + -4 <= 0)%Z
   | 17 => (1 * s V_mdct_short_m + -4 <= 0 /\ -1 * s V_mdct_short_l + -1 <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_l + 1 <= 0)%Z
   | 18 => (1 * s V_mdct_short_l + 1 <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ -1 * s V_mdct_short_l + -1 <= 0 /\ 1 * s V_mdct_short_m + -4 <= 0)%Z
   | 19 => (1 * s V_mdct_short_m + -4 <= 0 /\ -1 * s V_mdct_short_l + -1 <= 0 /\ 1 * s V_mdct_short_l + 1 <= 0 /\ -1 * s V_mdct_short_z + 1 <= 0)%Z
   | 20 => (1 * s V_mdct_short_m + -5 <= 0 /\ 1 * s V_mdct_short_l + -2 <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ -1 * s V_mdct_short_l <= 0)%Z
   | 21 => (-1 * s V_mdct_short_l <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_l + -2 <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0)%Z
   | 22 => (1 * s V_mdct_short_m + -5 <= 0 /\ 1 * s V_mdct_short_l + -2 <= 0 /\ -1 * s V_mdct_short_z <= 0 /\ -1 * s V_mdct_short_l <= 0)%Z
   | 23 => (-1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0 /\ 1 * s V_mdct_short_l + -1 <= 0 /\ -1 * s V_mdct_short_l + -1 <= 0)%Z
   | 24 => (-1 * s V_mdct_short_l + -1 <= 0 /\ 1 * s V_mdct_short_l + -1 <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0 /\ -1 * s V_mdct_short_z <= 0)%Z
   | 25 => (-1 * s V_mdct_short_z <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0 /\ 1 * s V_mdct_short_l + -1 <= 0 /\ -1 * s V_mdct_short_l + -1 <= 0)%Z
   | 26 => (-1 * s V_mdct_short_l + -1 <= 0 /\ 1 * s V_mdct_short_l + -1 <= 0 /\ 1 * s V_mdct_short_m + -5 <= 0 /\ -1 * s V_mdct_short_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_mdct_short (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((24 # 1) <= z)%Q
   | 2 => ((24 # 1) + s V_mdct_short_z <= z)%Q
   | 3 => (s V_mdct_short_z + (4 # 1) * max0(1 + s V_mdct_short_m) <= z)%Q
   | 4 => (s V_mdct_short_z + (4 # 1) * max0(1 + s V_mdct_short_m) <= z)%Q
   | 5 => (s V_mdct_short_z + (4 # 1) * max0(1 + s V_mdct_short_m) <= z)%Q
   | 6 => hints
     [(*-4 0*) F_max0_monotonic (F_check_ge (1 + s V_mdct_short_m) (s V_mdct_short_m));
      (*-4 0*) F_max0_ge_0 (s V_mdct_short_m)]
     (s V_mdct_short_z + (4 # 1) * max0(1 + s V_mdct_short_m) <= z)%Q
   | 7 => (s V_mdct_short_z <= z)%Q
   | 8 => (s V_mdct_short_z + (4 # 1) * max0(1 + s V_mdct_short_m) <= z)%Q
   | 9 => (s V_mdct_short_z + (4 # 1) * max0(1 + s V_mdct_short_m) <= z)%Q
   | 10 => (-(3 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
            + (4 # 1) * max0(1 + s V_mdct_short_m) <= z)%Q
   | 11 => hints
     [(*0 4*) F_max0_pre_decrement 1 (1 + s V_mdct_short_m) (1)]
     (-(3 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
      + (4 # 1) * max0(1 + s V_mdct_short_m) <= z)%Q
   | 12 => ((1 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
            + (4 # 1) * max0(s V_mdct_short_m) <= z)%Q
   | 13 => ((1 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
            + (4 # 1) * max0(s V_mdct_short_m) <= z)%Q
   | 14 => ((1 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
            + (4 # 1) * max0(s V_mdct_short_m) <= z)%Q
   | 15 => ((1 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
            + (4 # 1) * max0(s V_mdct_short_m) <= z)%Q
   | 16 => ((1 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
            + (4 # 1) * max0(1 + s V_mdct_short_m) <= z)%Q
   | 17 => ((1 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
            + (4 # 1) * max0(1 + s V_mdct_short_m) <= z)%Q
   | 18 => ((1 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
            + (4 # 1) * max0(1 + s V_mdct_short_m) <= z)%Q
   | 19 => hints
     [(*0 1*) F_max0_ge_0 (1 + s V_mdct_short_l)]
     (s V_mdct_short_z + max0(1 + s V_mdct_short_l)
      + (4 # 1) * max0(1 + s V_mdct_short_m) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1 + s V_mdct_short_l) (1)]
     ((1 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
      + (4 # 1) * max0(s V_mdct_short_m) <= z)%Q
   | 21 => ((2 # 1) + s V_mdct_short_z + max0(s V_mdct_short_l)
            + (4 # 1) * max0(s V_mdct_short_m) <= z)%Q
   | 22 => ((2 # 1) + s V_mdct_short_z + max0(s V_mdct_short_l)
            + (4 # 1) * max0(s V_mdct_short_m) <= z)%Q
   | 23 => ((2 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
            + (4 # 1) * max0(s V_mdct_short_m) <= z)%Q
   | 24 => ((2 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
            + (4 # 1) * max0(s V_mdct_short_m) <= z)%Q
   | 25 => ((2 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
            + (4 # 1) * max0(s V_mdct_short_m) <= z)%Q
   | 26 => ((1 # 1) + s V_mdct_short_z + max0(1 + s V_mdct_short_l)
            + (4 # 1) * max0(s V_mdct_short_m) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_mdct_short =>
    [mkPA Q (fun n z s => ai_mdct_short n s /\ annot0_mdct_short n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_mdct_short (proc_start P_mdct_short) s1 (proc_end P_mdct_short) s2 ->
    (s2 V_mdct_short_z <= (24 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_mdct_short.
Qed.
