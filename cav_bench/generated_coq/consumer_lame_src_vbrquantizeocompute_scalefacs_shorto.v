Require Import pasta.Pasta.

Inductive proc: Type :=
  P_compute_scalefacs_short.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_compute_scalefacs_short_z := 1%positive.
Notation V_compute_scalefacs_short_i := 2%positive.
Notation V_compute_scalefacs_short_ifqstep_inv := 3%positive.
Notation V_compute_scalefacs_short_sfb := 4%positive.
Notation V_compute_scalefacs_short_cod_info := 5%positive.
Notation V_compute_scalefacs_short_scalefac := 6%positive.
Notation V_compute_scalefacs_short_vbrsf := 7%positive.
Definition Pedges_compute_scalefacs_short: list (edge proc) :=
  (EA 1 (AAssign V_compute_scalefacs_short_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_compute_scalefacs_short_ifqstep_inv None) 3)::
  (EA 3 (AAssign V_compute_scalefacs_short_sfb (Some (ENum (0)))) 4)::
  (EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_short_sfb) s) <
  (eval (ENum (12)) s))%Z)) 9)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_short_sfb) s) >=
  (eval (ENum (12)) s))%Z)) 7)::(EA 7 AWeaken 8)::(EA 9 AWeaken 10)::
  (EA 10 (AAssign V_compute_scalefacs_short_i (Some (ENum (0)))) 11)::
  (EA 11 ANone 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_short_i) s) < (eval (ENum (3))
  s))%Z)) 21)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_short_i) s) >= (eval (ENum (3))
  s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_compute_scalefacs_short_sfb
  (Some (EAdd (EVar V_compute_scalefacs_short_sfb) (ENum (1))))) 17)::
  (EA 17 ANone 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_compute_scalefacs_short_z (Some (EAdd (ENum (1))
  (EVar V_compute_scalefacs_short_z)))) 20)::(EA 20 AWeaken 6)::
  (EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_short_sfb) s) < (eval (ENum (6))
  s))%Z)) 26)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_compute_scalefacs_short_sfb) s) >=
  (eval (ENum (6)) s))%Z)) 23)::(EA 23 AWeaken 24)::(EA 24 ANone 25)::
  (EA 25 AWeaken 29)::(EA 26 AWeaken 27)::(EA 27 ANone 28)::
  (EA 28 AWeaken 29)::(EA 29 ANone 30)::(EA 29 ANone 31)::(EA 30 ANone 31)::
  (EA 31 ANone 32)::(EA 32 (AAssign V_compute_scalefacs_short_i
  (Some (EAdd (EVar V_compute_scalefacs_short_i) (ENum (1))))) 33)::
  (EA 33 ANone 34)::(EA 34 ANone 35)::(EA 35 (AAssign
  V_compute_scalefacs_short_z (Some (EAdd (ENum (1))
  (EVar V_compute_scalefacs_short_z)))) 36)::(EA 36 AWeaken 13)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_compute_scalefacs_short => Pedges_compute_scalefacs_short
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_compute_scalefacs_short => 8
     end)%positive;
  var_global := var_global
}.

Definition ai_compute_scalefacs_short (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0)%Z
   | 3 => (-1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_z <= 0)%Z
   | 4 => (1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0)%Z
   | 5 => (-1 * s V_compute_scalefacs_short_sfb <= 0 /\ 1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_z <= 0)%Z
   | 6 => (-1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0)%Z
   | 7 => (-1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_sfb + 12 <= 0)%Z
   | 8 => (-1 * s V_compute_scalefacs_short_sfb + 12 <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0)%Z
   | 9 => (-1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_sfb + -11 <= 0)%Z
   | 10 => (1 * s V_compute_scalefacs_short_sfb + -11 <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0)%Z
   | 11 => (-1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_sfb + -11 <= 0 /\ 1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_i <= 0)%Z
   | 12 => (-1 * s V_compute_scalefacs_short_i <= 0 /\ 1 * s V_compute_scalefacs_short_i <= 0 /\ 1 * s V_compute_scalefacs_short_sfb + -11 <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0)%Z
   | 13 => (-1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0 /\ 1 * s V_compute_scalefacs_short_i + -3 <= 0)%Z
   | 14 => (1 * s V_compute_scalefacs_short_i + -3 <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_i + 3 <= 0)%Z
   | 15 => (-1 * s V_compute_scalefacs_short_i + 3 <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0 /\ 1 * s V_compute_scalefacs_short_i + -3 <= 0)%Z
   | 16 => (1 * s V_compute_scalefacs_short_i + -3 <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_i + 3 <= 0)%Z
   | 17 => (-1 * s V_compute_scalefacs_short_i + 3 <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_i + -3 <= 0 /\ -1 * s V_compute_scalefacs_short_sfb + 1 <= 0)%Z
   | 18 => (-1 * s V_compute_scalefacs_short_sfb + 1 <= 0 /\ 1 * s V_compute_scalefacs_short_i + -3 <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_i + 3 <= 0)%Z
   | 19 => (-1 * s V_compute_scalefacs_short_i + 3 <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_i + -3 <= 0 /\ -1 * s V_compute_scalefacs_short_sfb + 1 <= 0)%Z
   | 20 => (-1 * s V_compute_scalefacs_short_sfb + 1 <= 0 /\ 1 * s V_compute_scalefacs_short_i + -3 <= 0 /\ -1 * s V_compute_scalefacs_short_i + 3 <= 0 /\ -1 * s V_compute_scalefacs_short_z + 1 <= 0)%Z
   | 21 => (-1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_i + -2 <= 0)%Z
   | 22 => (1 * s V_compute_scalefacs_short_i + -2 <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0)%Z
   | 23 => (-1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_i + -2 <= 0 /\ -1 * s V_compute_scalefacs_short_sfb + 6 <= 0)%Z
   | 24 => (-1 * s V_compute_scalefacs_short_sfb + 6 <= 0 /\ 1 * s V_compute_scalefacs_short_i + -2 <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_i <= 0)%Z
   | 25 => (-1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_i + -2 <= 0 /\ -1 * s V_compute_scalefacs_short_sfb + 6 <= 0)%Z
   | 26 => (-1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_i + -2 <= 0 /\ 1 * s V_compute_scalefacs_short_sfb + -5 <= 0)%Z
   | 27 => (1 * s V_compute_scalefacs_short_sfb + -5 <= 0 /\ 1 * s V_compute_scalefacs_short_i + -2 <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0)%Z
   | 28 => (-1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_i + -2 <= 0 /\ 1 * s V_compute_scalefacs_short_sfb + -5 <= 0)%Z
   | 29 => (1 * s V_compute_scalefacs_short_i + -2 <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0)%Z
   | 30 => (-1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_i + -2 <= 0)%Z
   | 31 => (1 * s V_compute_scalefacs_short_i + -2 <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0)%Z
   | 32 => (-1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_i <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0 /\ 1 * s V_compute_scalefacs_short_i + -2 <= 0)%Z
   | 33 => (-1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_i + 1 <= 0 /\ 1 * s V_compute_scalefacs_short_i + -3 <= 0)%Z
   | 34 => (1 * s V_compute_scalefacs_short_i + -3 <= 0 /\ -1 * s V_compute_scalefacs_short_i + 1 <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_z <= 0)%Z
   | 35 => (-1 * s V_compute_scalefacs_short_z <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_i + 1 <= 0 /\ 1 * s V_compute_scalefacs_short_i + -3 <= 0)%Z
   | 36 => (1 * s V_compute_scalefacs_short_i + -3 <= 0 /\ -1 * s V_compute_scalefacs_short_i + 1 <= 0 /\ -1 * s V_compute_scalefacs_short_sfb <= 0 /\ -1 * s V_compute_scalefacs_short_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_compute_scalefacs_short (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((48 # 1) <= z)%Q
   | 2 => ((48 # 1) + s V_compute_scalefacs_short_z <= z)%Q
   | 3 => ((48 # 1) + s V_compute_scalefacs_short_z <= z)%Q
   | 4 => (s V_compute_scalefacs_short_z
           + (4 # 1) * max0(12 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 5 => (s V_compute_scalefacs_short_z
           + (4 # 1) * max0(12 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 6 => (s V_compute_scalefacs_short_z
           + (4 # 1) * max0(12 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 7 => hints
     [(*-4 0*) F_max0_monotonic (F_check_ge (12
                                             - s V_compute_scalefacs_short_sfb) (11
                                                                    - s V_compute_scalefacs_short_sfb));
      (*-4 0*) F_max0_ge_0 (11 - s V_compute_scalefacs_short_sfb)]
     (s V_compute_scalefacs_short_z
      + (4 # 1) * max0(12 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 8 => (s V_compute_scalefacs_short_z <= z)%Q
   | 9 => (s V_compute_scalefacs_short_z
           + (4 # 1) * max0(12 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 10 => (s V_compute_scalefacs_short_z
            + (4 # 1) * max0(12 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 11 => (-(3 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(12 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 12 => hints
     [(*-4 0*) F_max0_pre_decrement 1 (12 - s V_compute_scalefacs_short_sfb) (1)]
     (-(3 # 1) + s V_compute_scalefacs_short_z
      + max0(3 - s V_compute_scalefacs_short_i)
      + (4 # 1) * max0(12 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 13 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 14 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 15 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 16 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 17 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(12 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 18 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(12 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 19 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(12 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_ge_0 (3 - s V_compute_scalefacs_short_i)]
     (s V_compute_scalefacs_short_z + max0(3 - s V_compute_scalefacs_short_i)
      + (4 # 1) * max0(12 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 21 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 22 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 23 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 24 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 25 => hints
     [(*0 1*) F_max0_pre_decrement 1 (3 - s V_compute_scalefacs_short_i) (1)]
     ((1 # 1) + s V_compute_scalefacs_short_z
      + max0(3 - s V_compute_scalefacs_short_i)
      + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 26 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 27 => ((1 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 28 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (3 - s V_compute_scalefacs_short_i) (1)]
     ((1 # 1) + s V_compute_scalefacs_short_z
      + max0(3 - s V_compute_scalefacs_short_i)
      + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 29 => ((2 # 1) + s V_compute_scalefacs_short_z
            + max0(2 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 30 => ((2 # 1) + s V_compute_scalefacs_short_z
            + max0(2 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 31 => ((2 # 1) + s V_compute_scalefacs_short_z
            + max0(2 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 32 => ((2 # 1) + s V_compute_scalefacs_short_z
            + max0(2 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 33 => ((2 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 34 => ((2 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 35 => ((2 # 1) + s V_compute_scalefacs_short_z
            + max0(3 - s V_compute_scalefacs_short_i)
            + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_compute_scalefacs_short_z)) (F_check_ge (s V_compute_scalefacs_short_z) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_compute_scalefacs_short_z) (0))) (F_max0_ge_0 (s V_compute_scalefacs_short_z))]
     ((1 # 1) + s V_compute_scalefacs_short_z
      + max0(3 - s V_compute_scalefacs_short_i)
      + (4 # 1) * max0(11 - s V_compute_scalefacs_short_sfb) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_compute_scalefacs_short =>
    [mkPA Q (fun n z s => ai_compute_scalefacs_short n s /\ annot0_compute_scalefacs_short n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_compute_scalefacs_short (proc_start P_compute_scalefacs_short) s1 (proc_end P_compute_scalefacs_short) s2 ->
    (s2 V_compute_scalefacs_short_z <= (48 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_compute_scalefacs_short.
Qed.
