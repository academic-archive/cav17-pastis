Require Import pasta.Pasta.

Inductive proc: Type :=
  P_date_ymd.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_date_ymd_z := 1%positive.
Notation V_date_ymd_d := 2%positive.
Notation V_date_ymd_day_dref := 3%positive.
Notation V_date_ymd_days := 4%positive.
Notation V_date_ymd_i := 5%positive.
Notation V_date_ymd_m := 6%positive.
Notation V_date_ymd_month_dref := 7%positive.
Notation V_date_ymd_y := 8%positive.
Notation V_date_ymd_year_dref := 9%positive.
Notation V_date_ymd_day := 10%positive.
Notation V_date_ymd_month := 11%positive.
Notation V_date_ymd_tstamp := 12%positive.
Notation V_date_ymd_year := 13%positive.
Definition Pedges_date_ymd: list (edge proc) :=
  (EA 1 (AAssign V_date_ymd_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_date_ymd_days None) 3)::(EA 3 (AAssign V_date_ymd_days
  (Some (ESub (EVar V_date_ymd_days) (ENum (730))))) 4)::(EA 4 (AAssign
  V_date_ymd_y None) 5)::(EA 5 (AAssign V_date_ymd_d None) 6)::(EA 6 (AAssign
  V_date_ymd_year_dref (Some (EAdd (EVar V_date_ymd_y) (ENum (1972))))) 7)::
  (EA 7 (AAssign V_date_ymd_i (Some (ENum (0)))) 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 (AGuard (fun s => ((eval (EVar V_date_ymd_i) s) <
  (eval (ENum (48)) s))%Z)) 12)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_date_ymd_i) s) >= (eval (ENum (48)) s))%Z)) 11)::
  (EA 11 AWeaken 27)::(EA 12 AWeaken 13)::(EA 13 (AAssign V_date_ymd_m
  None) 14)::(EA 14 (AAssign V_date_ymd_d None) 15)::(EA 15 AWeaken 16)::
  (EA 16 (AGuard (fun s => ((eval (EVar V_date_ymd_d) s) < (eval (ENum (0))
  s))%Z)) 24)::(EA 16 (AGuard (fun s => ((eval (EVar V_date_ymd_d) s) >=
  (eval (ENum (0)) s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 18 ANone 19)::
  (EA 19 (AAssign V_date_ymd_i (Some (EAdd (EVar V_date_ymd_i)
  (ENum (1))))) 20)::(EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_date_ymd_z (Some (EAdd (ENum (1)) (EVar V_date_ymd_z)))) 23)::
  (EA 23 AWeaken 10)::(EA 24 AWeaken 25)::(EA 25 (AAssign V_date_ymd_d
  None) 26)::(EA 26 ANone 27)::(EA 27 (AAssign V_date_ymd_month_dref
  (Some (EAdd (EVar V_date_ymd_m) (ENum (1))))) 28)::(EA 28 (AAssign
  V_date_ymd_day_dref (Some (EAdd (EVar V_date_ymd_d) (ENum (1))))) 29)::
  (EA 29 (AAssign V_date_ymd_i None) 30)::(EA 30 AWeaken 31)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_date_ymd => Pedges_date_ymd
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_date_ymd => 31
     end)%positive;
  var_global := var_global
}.

Definition ai_date_ymd (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_z <= 0)%Z
   | 3 => (-1 * s V_date_ymd_z <= 0 /\ 1 * s V_date_ymd_z <= 0)%Z
   | 4 => (1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_z <= 0)%Z
   | 5 => (-1 * s V_date_ymd_z <= 0 /\ 1 * s V_date_ymd_z <= 0)%Z
   | 6 => (1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_z <= 0)%Z
   | 7 => (-1 * s V_date_ymd_z <= 0 /\ 1 * s V_date_ymd_z <= 0)%Z
   | 8 => (1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ 1 * s V_date_ymd_i <= 0 /\ -1 * s V_date_ymd_i <= 0)%Z
   | 9 => (-1 * s V_date_ymd_i <= 0 /\ 1 * s V_date_ymd_i <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ 1 * s V_date_ymd_z <= 0)%Z
   | 10 => (-1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_i <= 0 /\ 1 * s V_date_ymd_i + -48 <= 0)%Z
   | 11 => (1 * s V_date_ymd_i + -48 <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_i + 48 <= 0)%Z
   | 12 => (-1 * s V_date_ymd_i <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ 1 * s V_date_ymd_i + -47 <= 0)%Z
   | 13 => (1 * s V_date_ymd_i + -47 <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_i <= 0)%Z
   | 14 => (-1 * s V_date_ymd_i <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ 1 * s V_date_ymd_i + -47 <= 0)%Z
   | 15 => (1 * s V_date_ymd_i + -47 <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_i <= 0)%Z
   | 16 => (-1 * s V_date_ymd_i <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ 1 * s V_date_ymd_i + -47 <= 0)%Z
   | 17 => (1 * s V_date_ymd_i + -47 <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_i <= 0 /\ -1 * s V_date_ymd_d <= 0)%Z
   | 18 => (-1 * s V_date_ymd_d <= 0 /\ -1 * s V_date_ymd_i <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ 1 * s V_date_ymd_i + -47 <= 0)%Z
   | 19 => (1 * s V_date_ymd_i + -47 <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_i <= 0 /\ -1 * s V_date_ymd_d <= 0)%Z
   | 20 => (-1 * s V_date_ymd_d <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ 1 * s V_date_ymd_i + -48 <= 0 /\ -1 * s V_date_ymd_i + 1 <= 0)%Z
   | 21 => (-1 * s V_date_ymd_i + 1 <= 0 /\ 1 * s V_date_ymd_i + -48 <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_d <= 0)%Z
   | 22 => (-1 * s V_date_ymd_d <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ 1 * s V_date_ymd_i + -48 <= 0 /\ -1 * s V_date_ymd_i + 1 <= 0)%Z
   | 23 => (-1 * s V_date_ymd_i + 1 <= 0 /\ 1 * s V_date_ymd_i + -48 <= 0 /\ -1 * s V_date_ymd_d <= 0 /\ -1 * s V_date_ymd_z + 1 <= 0)%Z
   | 24 => (1 * s V_date_ymd_i + -47 <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_i <= 0 /\ 1 * s V_date_ymd_d + 1 <= 0)%Z
   | 25 => (1 * s V_date_ymd_d + 1 <= 0 /\ -1 * s V_date_ymd_i <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ 1 * s V_date_ymd_i + -47 <= 0)%Z
   | 26 => (1 * s V_date_ymd_i + -47 <= 0 /\ -1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_i <= 0)%Z
   | 27 => (1 * s V_date_ymd_i + -48 <= 0 /\ -1 * s V_date_ymd_i <= 0 /\ -1 * s V_date_ymd_z <= 0)%Z
   | 28 => (-1 * s V_date_ymd_z <= 0 /\ -1 * s V_date_ymd_i <= 0 /\ 1 * s V_date_ymd_i + -48 <= 0)%Z
   | 29 => (1 * s V_date_ymd_i + -48 <= 0 /\ -1 * s V_date_ymd_i <= 0 /\ -1 * s V_date_ymd_z <= 0)%Z
   | 30 => (-1 * s V_date_ymd_z <= 0)%Z
   | 31 => (-1 * s V_date_ymd_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_date_ymd (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((48 # 1) <= z)%Q
   | 2 => ((48 # 1) + s V_date_ymd_z <= z)%Q
   | 3 => ((48 # 1) + s V_date_ymd_z <= z)%Q
   | 4 => ((48 # 1) + s V_date_ymd_z <= z)%Q
   | 5 => ((48 # 1) + s V_date_ymd_z <= z)%Q
   | 6 => ((48 # 1) + s V_date_ymd_z <= z)%Q
   | 7 => ((48 # 1) + s V_date_ymd_z <= z)%Q
   | 8 => ((48 # 1) - s V_date_ymd_i + s V_date_ymd_z <= z)%Q
   | 9 => ((48 # 1) - s V_date_ymd_i + s V_date_ymd_z <= z)%Q
   | 10 => ((48 # 1) - s V_date_ymd_i + s V_date_ymd_z <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (48 - s V_date_ymd_i) (47
                                                                   - 
                                                                   s V_date_ymd_i));
      (*-1 0*) F_max0_ge_0 (47 - s V_date_ymd_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (48
                                                               - s V_date_ymd_i) (0))) (F_max0_ge_0 (48
                                                                    - s V_date_ymd_i))]
     ((48 # 1) - s V_date_ymd_i + s V_date_ymd_z <= z)%Q
   | 12 => ((48 # 1) - s V_date_ymd_i + s V_date_ymd_z <= z)%Q
   | 13 => ((48 # 1) - s V_date_ymd_i + s V_date_ymd_z <= z)%Q
   | 14 => ((48 # 1) - s V_date_ymd_i + s V_date_ymd_z <= z)%Q
   | 15 => hints
     [(*0 0.0212766*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_date_ymd_i) (0))) (F_max0_ge_0 (s V_date_ymd_i));
      (*-1.02128 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (48
                                                                    - 
                                                                    s V_date_ymd_i) (0))) (F_max0_ge_0 (48
                                                                    - s V_date_ymd_i))]
     ((48 # 1) - s V_date_ymd_i + s V_date_ymd_z <= z)%Q
   | 16 => (-(48 # 47) + s V_date_ymd_z
            + (48 # 47) * max0(48 - s V_date_ymd_i)
            + (1 # 47) * max0(s V_date_ymd_i) <= z)%Q
   | 17 => hints
     [(*-1.02128 0*) F_binom_monotonic 1 (F_max0_ge_arg (48 - s V_date_ymd_i)) (F_check_ge (48
                                                                    - s V_date_ymd_i) (0))]
     (-(48 # 47) + s V_date_ymd_z + (48 # 47) * max0(48 - s V_date_ymd_i)
      + (1 # 47) * max0(s V_date_ymd_i) <= z)%Q
   | 18 => ((48 # 1) - (48 # 47) * s V_date_ymd_i + s V_date_ymd_z
            + (1 # 47) * max0(s V_date_ymd_i) <= z)%Q
   | 19 => ((48 # 1) - (48 # 47) * s V_date_ymd_i + s V_date_ymd_z
            + (1 # 47) * max0(s V_date_ymd_i) <= z)%Q
   | 20 => ((2304 # 47) - (48 # 47) * s V_date_ymd_i + s V_date_ymd_z
            + (1 # 47) * max0(-1 + s V_date_ymd_i) <= z)%Q
   | 21 => ((2304 # 47) - (48 # 47) * s V_date_ymd_i + s V_date_ymd_z
            + (1 # 47) * max0(-1 + s V_date_ymd_i) <= z)%Q
   | 22 => ((2304 # 47) - (48 # 47) * s V_date_ymd_i + s V_date_ymd_z
            + (1 # 47) * max0(-1 + s V_date_ymd_i) <= z)%Q
   | 23 => hints
     [(*-0.0212766 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                           + s V_date_ymd_i)) (F_check_ge (-1
                                                                    + s V_date_ymd_i) (0))]
     ((2257 # 47) - (48 # 47) * s V_date_ymd_i + s V_date_ymd_z
      + (1 # 47) * max0(-1 + s V_date_ymd_i) <= z)%Q
   | 24 => hints
     [(*-1.02128 0*) F_max0_pre_decrement 1 (48 - s V_date_ymd_i) (1);
      (*-1.02128 0*) F_max0_ge_0 (47 - s V_date_ymd_i);
      (*-0.0212766 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_date_ymd_i)) (F_check_ge (0) (0))]
     (-(48 # 47) + s V_date_ymd_z + (48 # 47) * max0(48 - s V_date_ymd_i)
      + (1 # 47) * max0(s V_date_ymd_i) <= z)%Q
   | 25 => (s V_date_ymd_z <= z)%Q
   | 26 => (s V_date_ymd_z <= z)%Q
   | 27 => (s V_date_ymd_z <= z)%Q
   | 28 => (s V_date_ymd_z <= z)%Q
   | 29 => (s V_date_ymd_z <= z)%Q
   | 30 => (s V_date_ymd_z <= z)%Q
   | 31 => (s V_date_ymd_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_date_ymd =>
    [mkPA Q (fun n z s => ai_date_ymd n s /\ annot0_date_ymd n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_date_ymd (proc_start P_date_ymd) s1 (proc_end P_date_ymd) s2 ->
    (s2 V_date_ymd_z <= (48 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_date_ymd.
Qed.
