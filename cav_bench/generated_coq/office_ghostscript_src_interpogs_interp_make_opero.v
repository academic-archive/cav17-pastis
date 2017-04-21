Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gs_interp_make_oper.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gs_interp_make_oper_z := 1%positive.
Notation V_gs_interp_make_oper__tmp := 2%positive.
Notation V_gs_interp_make_oper_i := 3%positive.
Notation V_gs_interp_make_oper_opref_dref_off0_off0 := 4%positive.
Notation V_gs_interp_make_oper_opref_dref_off0_off2 := 5%positive.
Notation V_gs_interp_make_oper_idx := 6%positive.
Notation V_gs_interp_make_oper_opref := 7%positive.
Notation V_gs_interp_make_oper_proc := 8%positive.
Definition Pedges_gs_interp_make_oper: list (edge proc) :=
  (EA 1 (AAssign V_gs_interp_make_oper_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_gs_interp_make_oper__tmp
  (Some (EVar V_gs_interp_make_oper_idx))) 3)::(EA 3 (AAssign
  V_gs_interp_make_oper_i (Some (ENum (10)))) 4)::(EA 4 ANone 5)::
  (EA 5 (AAssign V_gs_interp_make_oper_i
  (Some (EAdd (EVar V_gs_interp_make_oper_i) (ENum (-1))))) 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EAdd (EVar V_gs_interp_make_oper_i) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 9)::(EA 7 (AGuard
  (fun s => ((eval (EAdd (EVar V_gs_interp_make_oper_i) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 14)::(EA 9 AWeaken 10)::
  (EA 10 ANone 11)::(EA 11 AWeaken 12)::(EA 12 (AGuard (fun s => True)) 26)::
  (EA 12 (AGuard (fun s => True)) 13)::(EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_gs_interp_make_oper_i) s) >= (eval (ENum (0))
  s))%Z)) 20)::(EA 14 (AGuard (fun s => ((eval (EVar V_gs_interp_make_oper_i)
  s) < (eval (ENum (0)) s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 16 (AAssign
  V_gs_interp_make_oper_opref_dref_off0_off0 (Some (ENum (3968)))) 17)::
  (EA 17 (AAssign V_gs_interp_make_oper_opref_dref_off0_off2
  (Some (EVar V_gs_interp_make_oper__tmp))) 18)::(EA 18 ANone 19)::
  (EA 19 AWeaken 25)::(EA 20 AWeaken 21)::(EA 21 (AAssign
  V_gs_interp_make_oper_opref_dref_off0_off0 None) 22)::(EA 22 (AAssign
  V_gs_interp_make_oper_opref_dref_off0_off2
  (Some (EAdd (EVar V_gs_interp_make_oper_i) (ENum (1))))) 23)::
  (EA 23 ANone 24)::(EA 24 AWeaken 25)::(EA 26 AWeaken 27)::
  (EA 27 ANone 28)::(EA 28 ANone 29)::(EA 29 (AAssign V_gs_interp_make_oper_z
  (Some (EAdd (ENum (1)) (EVar V_gs_interp_make_oper_z)))) 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gs_interp_make_oper => Pedges_gs_interp_make_oper
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gs_interp_make_oper => 25
     end)%positive;
  var_global := var_global
}.

Definition ai_gs_interp_make_oper (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gs_interp_make_oper_z <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0)%Z
   | 3 => (-1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_z <= 0)%Z
   | 4 => (1 * s V_gs_interp_make_oper_z <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_i + -10 <= 0 /\ -1 * s V_gs_interp_make_oper_i + 10 <= 0)%Z
   | 5 => (1 * s V_gs_interp_make_oper_i + -10 <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0 /\ -1 * s V_gs_interp_make_oper_i + 1 <= 0)%Z
   | 6 => (-1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_i <= 0)%Z
   | 7 => (-1 * s V_gs_interp_make_oper_i <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0)%Z
   | 8 => (-1 * s V_gs_interp_make_oper_z <= 0 /\ -1 * s V_gs_interp_make_oper_i <= 0 /\ 1 * s V_gs_interp_make_oper_i <= 0)%Z
   | 9 => (-1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_i + 1 <= 0)%Z
   | 10 => (-1 * s V_gs_interp_make_oper_i + 1 <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0)%Z
   | 11 => (-1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_i + 1 <= 0)%Z
   | 12 => (-1 * s V_gs_interp_make_oper_i + 1 <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0)%Z
   | 13 => (-1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_i + 1 <= 0)%Z
   | 14 => (-1 * s V_gs_interp_make_oper_i <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0)%Z
   | 15 => (False)%Z
   | 16 => (False)%Z
   | 17 => (False)%Z
   | 18 => (False)%Z
   | 19 => (False)%Z
   | 20 => (-1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_i <= 0)%Z
   | 21 => (-1 * s V_gs_interp_make_oper_i <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0)%Z
   | 22 => (-1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_i <= 0)%Z
   | 23 => (-1 * s V_gs_interp_make_oper_i <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_opref_dref_off0_off2 + -10 <= 0 /\ -1 * s V_gs_interp_make_oper_opref_dref_off0_off2 + 1 <= 0)%Z
   | 24 => (-1 * s V_gs_interp_make_oper_opref_dref_off0_off2 + 1 <= 0 /\ 1 * s V_gs_interp_make_oper_opref_dref_off0_off2 + -10 <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_i <= 0)%Z
   | 25 => (-1 * s V_gs_interp_make_oper_i <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_opref_dref_off0_off2 + -10 <= 0 /\ -1 * s V_gs_interp_make_oper_opref_dref_off0_off2 + 1 <= 0)%Z
   | 26 => (-1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_i + 1 <= 0)%Z
   | 27 => (-1 * s V_gs_interp_make_oper_i + 1 <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0)%Z
   | 28 => (-1 * s V_gs_interp_make_oper_z <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_i + 1 <= 0)%Z
   | 29 => (-1 * s V_gs_interp_make_oper_i + 1 <= 0 /\ 1 * s V_gs_interp_make_oper_i + -9 <= 0 /\ -1 * s V_gs_interp_make_oper_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gs_interp_make_oper (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((9 # 1) <= z)%Q
   | 2 => ((9 # 1) + s V_gs_interp_make_oper_z <= z)%Q
   | 3 => ((9 # 1) + s V_gs_interp_make_oper_z <= z)%Q
   | 4 => (-(46 # 79) + (7 # 132) * s V_gs_interp_make_oper_i
           + s V_gs_interp_make_oper_z
           + (169 # 168) * max0(-1 + s V_gs_interp_make_oper_i)
           + (1 # 17) * max0(10 - s V_gs_interp_make_oper_i) <= z)%Q
   | 5 => (-(46 # 79) + (7 # 132) * s V_gs_interp_make_oper_i
           + s V_gs_interp_make_oper_z
           + (169 # 168) * max0(-1 + s V_gs_interp_make_oper_i)
           + (1 # 17) * max0(10 - s V_gs_interp_make_oper_i) <= z)%Q
   | 6 => (-(9 # 17) + (7 # 132) * s V_gs_interp_make_oper_i
           + s V_gs_interp_make_oper_z
           + (1 # 17) * max0(9 - s V_gs_interp_make_oper_i)
           + (169 # 168) * max0(s V_gs_interp_make_oper_i) <= z)%Q
   | 7 => (-(9 # 17) + (7 # 132) * s V_gs_interp_make_oper_i
           + s V_gs_interp_make_oper_z
           + (1 # 17) * max0(9 - s V_gs_interp_make_oper_i)
           + (169 # 168) * max0(s V_gs_interp_make_oper_i) <= z)%Q
   | 8 => hints
     [(*-1.00588 0*) F_max0_ge_0 (s V_gs_interp_make_oper_i);
      (*-0.00588235 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_gs_interp_make_oper_i)) (F_check_ge (0) (0));
      (*-0.00588235 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_gs_interp_make_oper_i) (0))) (F_max0_ge_0 (-
                                                                    s V_gs_interp_make_oper_i));
      (*-0.0588235 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                           - s V_gs_interp_make_oper_i)) (F_check_ge (9
                                                                    - s V_gs_interp_make_oper_i) (0))]
     (-(9 # 17) + (7 # 132) * s V_gs_interp_make_oper_i
      + s V_gs_interp_make_oper_z
      + (1 # 17) * max0(9 - s V_gs_interp_make_oper_i)
      + (169 # 168) * max0(s V_gs_interp_make_oper_i) <= z)%Q
   | 9 => (-(9 # 17) + (7 # 132) * s V_gs_interp_make_oper_i
           + s V_gs_interp_make_oper_z
           + (1 # 17) * max0(9 - s V_gs_interp_make_oper_i)
           + (169 # 168) * max0(s V_gs_interp_make_oper_i) <= z)%Q
   | 10 => (-(9 # 17) + (7 # 132) * s V_gs_interp_make_oper_i
            + s V_gs_interp_make_oper_z
            + (1 # 17) * max0(9 - s V_gs_interp_make_oper_i)
            + (169 # 168) * max0(s V_gs_interp_make_oper_i) <= z)%Q
   | 11 => hints
     [(*0 1.00588*) F_max0_pre_decrement 1 (s V_gs_interp_make_oper_i) (1);
      (*0 0.0588235*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (10
                                                                    - s V_gs_interp_make_oper_i) (0))) (F_max0_ge_0 (10
                                                                    - s V_gs_interp_make_oper_i))]
     (-(9 # 17) + (7 # 132) * s V_gs_interp_make_oper_i
      + s V_gs_interp_make_oper_z
      + (1 # 17) * max0(9 - s V_gs_interp_make_oper_i)
      + (169 # 168) * max0(s V_gs_interp_make_oper_i) <= z)%Q
   | 12 => (-(17 # 152) + (17 # 152) * s V_gs_interp_make_oper_i
            + s V_gs_interp_make_oper_z
            + (169 # 168) * max0(-1 + s V_gs_interp_make_oper_i)
            + (1 # 17) * max0(9 - s V_gs_interp_make_oper_i)
            + (1 # 17) * max0(10 - s V_gs_interp_make_oper_i) <= z)%Q
   | 13 => hints
     [(*-1.11765 0*) F_max0_ge_0 (-1 + s V_gs_interp_make_oper_i);
      (*-0.0588235 0*) F_binom_monotonic 1 (F_max0_ge_0 (10
                                                         - s V_gs_interp_make_oper_i)) (F_check_ge (0) (0));
      (*-0.0588235 0*) F_binom_monotonic 1 (F_max0_ge_0 (9
                                                         - s V_gs_interp_make_oper_i)) (F_check_ge (0) (0));
      (*0 0.111765*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + 
                                                                    s V_gs_interp_make_oper_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_gs_interp_make_oper_i))]
     (-(17 # 152) + (17 # 152) * s V_gs_interp_make_oper_i
      + s V_gs_interp_make_oper_z
      + (169 # 168) * max0(-1 + s V_gs_interp_make_oper_i)
      + (1 # 17) * max0(9 - s V_gs_interp_make_oper_i)
      + (1 # 17) * max0(10 - s V_gs_interp_make_oper_i) <= z)%Q
   | 14 => (s V_gs_interp_make_oper_z <= z)%Q
   | 15 => (s V_gs_interp_make_oper_z <= z)%Q
   | 16 => (s V_gs_interp_make_oper_z <= z)%Q
   | 17 => (s V_gs_interp_make_oper_z <= z)%Q
   | 18 => (s V_gs_interp_make_oper_z <= z)%Q
   | 19 => (s V_gs_interp_make_oper_z <= z)%Q
   | 20 => (s V_gs_interp_make_oper_z <= z)%Q
   | 21 => (s V_gs_interp_make_oper_z <= z)%Q
   | 22 => (s V_gs_interp_make_oper_z <= z)%Q
   | 23 => (s V_gs_interp_make_oper_z <= z)%Q
   | 24 => (s V_gs_interp_make_oper_z <= z)%Q
   | 25 => (s V_gs_interp_make_oper_z <= z)%Q
   | 26 => hints
     [(*-0.0588235 0*) F_binom_monotonic 1 (F_max0_ge_arg (9
                                                           - s V_gs_interp_make_oper_i)) (F_check_ge (9
                                                                    - s V_gs_interp_make_oper_i) (0))]
     (-(17 # 152) + (17 # 152) * s V_gs_interp_make_oper_i
      + s V_gs_interp_make_oper_z
      + (169 # 168) * max0(-1 + s V_gs_interp_make_oper_i)
      + (1 # 17) * max0(9 - s V_gs_interp_make_oper_i)
      + (1 # 17) * max0(10 - s V_gs_interp_make_oper_i) <= z)%Q
   | 27 => ((33 # 79) + (7 # 132) * s V_gs_interp_make_oper_i
            + s V_gs_interp_make_oper_z
            + (169 # 168) * max0(-1 + s V_gs_interp_make_oper_i)
            + (1 # 17) * max0(10 - s V_gs_interp_make_oper_i) <= z)%Q
   | 28 => ((33 # 79) + (7 # 132) * s V_gs_interp_make_oper_i
            + s V_gs_interp_make_oper_z
            + (169 # 168) * max0(-1 + s V_gs_interp_make_oper_i)
            + (1 # 17) * max0(10 - s V_gs_interp_make_oper_i) <= z)%Q
   | 29 => ((33 # 79) + (7 # 132) * s V_gs_interp_make_oper_i
            + s V_gs_interp_make_oper_z
            + (169 # 168) * max0(-1 + s V_gs_interp_make_oper_i)
            + (1 # 17) * max0(10 - s V_gs_interp_make_oper_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gs_interp_make_oper =>
    [mkPA Q (fun n z s => ai_gs_interp_make_oper n s /\ annot0_gs_interp_make_oper n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gs_interp_make_oper (proc_start P_gs_interp_make_oper) s1 (proc_end P_gs_interp_make_oper) s2 ->
    (s2 V_gs_interp_make_oper_z <= (9 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gs_interp_make_oper.
Qed.
