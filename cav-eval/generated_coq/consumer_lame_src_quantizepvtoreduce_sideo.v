Require Import pasta.Pasta.

Inductive proc: Type :=
  P_reduce_side.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_reduce_side_z := 1%positive.
Notation V_reduce_side__tmp := 2%positive.
Notation V_reduce_side_ch := 3%positive.
Notation V_reduce_side_max_bits := 4%positive.
Notation V_reduce_side_numchn := 5%positive.
Notation V_reduce_side_targ_bits_dref_off0 := 6%positive.
Notation V_reduce_side_targ_bits_dref_off4 := 7%positive.
Notation V_reduce_side_mean_bits := 8%positive.
Notation V_reduce_side_ms_ener_ratio := 9%positive.
Notation V_reduce_side_targ_bits := 10%positive.
Definition Pedges_reduce_side: list (edge proc) :=
  (EA 1 (AAssign V_reduce_side_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_reduce_side__tmp (Some (EVar V_reduce_side_mean_bits))) 3)::
  (EA 3 (AAssign V_reduce_side_numchn (Some (ENum (2)))) 4)::
  (EA 4 AWeaken 5)::(EA 5 ANone 7)::(EA 5 ANone 6)::(EA 6 AWeaken 9)::
  (EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_reduce_side_targ_bits_dref_off4) s) >=
  (eval (ENum (125)) s))%Z)) 11)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_reduce_side_targ_bits_dref_off4) s) <
  (eval (ENum (125)) s))%Z)) 10)::(EA 10 AWeaken 20)::(EA 11 AWeaken 12)::
  (EA 12 ANone 16)::(EA 12 ANone 13)::(EA 13 (AAssign
  V_reduce_side_targ_bits_dref_off0
  (Some (EAdd (EVar V_reduce_side_targ_bits_dref_off0)
  (ESub (EVar V_reduce_side_targ_bits_dref_off4) (ENum (125)))))) 14)::
  (EA 14 (AAssign V_reduce_side_targ_bits_dref_off4
  (Some (ENum (125)))) 15)::(EA 15 ANone 19)::(EA 16 (AAssign
  V_reduce_side_targ_bits_dref_off0 None) 17)::(EA 17 (AAssign
  V_reduce_side_targ_bits_dref_off4 None) 18)::(EA 18 ANone 19)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_reduce_side_ch (Some (ENum (0)))) 21)::
  (EA 21 ANone 22)::(EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_reduce_side_ch) s) <
  (eval (EVar V_reduce_side_numchn) s))%Z)) 26)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_reduce_side_ch) s) >=
  (eval (EVar V_reduce_side_numchn) s))%Z)) 24)::(EA 24 AWeaken 25)::
  (EA 26 AWeaken 27)::(EA 27 ANone 34)::(EA 27 ANone 28)::(EA 28 ANone 29)::
  (EA 29 (AAssign V_reduce_side_max_bits None) 30)::(EA 30 AWeaken 31)::
  (EA 31 (AGuard (fun s => True)) 33)::(EA 31 (AGuard (fun s => True)) 32)::
  (EA 32 AWeaken 36)::(EA 33 AWeaken 35)::(EA 34 ANone 35)::
  (EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign V_reduce_side_ch
  (Some (EAdd (EVar V_reduce_side_ch) (ENum (1))))) 38)::(EA 38 ANone 39)::
  (EA 39 ANone 40)::(EA 40 (AAssign V_reduce_side_z (Some (EAdd (ENum (1))
  (EVar V_reduce_side_z)))) 41)::(EA 41 AWeaken 23)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_reduce_side => Pedges_reduce_side
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_reduce_side => 25
     end)%positive;
  var_global := var_global
}.

Definition ai_reduce_side (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_z <= 0)%Z
   | 3 => (-1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_z <= 0)%Z
   | 4 => (1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0)%Z
   | 5 => (-1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_z <= 0)%Z
   | 6 => (1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0)%Z
   | 7 => (1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0)%Z
   | 8 => (-1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_z <= 0)%Z
   | 9 => (1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0)%Z
   | 10 => (-1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_targ_bits_dref_off4 + -124 <= 0)%Z
   | 11 => (-1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_targ_bits_dref_off4 + 125 <= 0)%Z
   | 12 => (-1 * s V_reduce_side_targ_bits_dref_off4 + 125 <= 0 /\ 1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0)%Z
   | 13 => (-1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_targ_bits_dref_off4 + 125 <= 0)%Z
   | 14 => (-1 * s V_reduce_side_targ_bits_dref_off4 + 125 <= 0 /\ 1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0)%Z
   | 15 => (-1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_targ_bits_dref_off4 + -125 <= 0 /\ -1 * s V_reduce_side_targ_bits_dref_off4 + 125 <= 0)%Z
   | 16 => (-1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_targ_bits_dref_off4 + 125 <= 0)%Z
   | 17 => (-1 * s V_reduce_side_targ_bits_dref_off4 + 125 <= 0 /\ 1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0)%Z
   | 18 => (-1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_z <= 0)%Z
   | 19 => (1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0)%Z
   | 20 => (-1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_z <= 0)%Z
   | 21 => (1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_ch <= 0)%Z
   | 22 => (-1 * s V_reduce_side_ch <= 0 /\ 1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_z <= 0)%Z
   | 23 => (-1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn <= 0)%Z
   | 24 => (1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_ch+ 1 * s V_reduce_side_numchn <= 0)%Z
   | 25 => (-1 * s V_reduce_side_ch+ 1 * s V_reduce_side_numchn <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn <= 0)%Z
   | 26 => (1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn + 1 <= 0)%Z
   | 27 => (1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn + 1 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0)%Z
   | 28 => (1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn + 1 <= 0)%Z
   | 29 => (1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn + 1 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0)%Z
   | 30 => (1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn + 1 <= 0)%Z
   | 31 => (1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn + 1 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0)%Z
   | 32 => (1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn + 1 <= 0)%Z
   | 33 => (1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn + 1 <= 0)%Z
   | 34 => (1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn + 1 <= 0)%Z
   | 35 => (1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn + 1 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0)%Z
   | 36 => (1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn + 1 <= 0)%Z
   | 37 => (1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn + 1 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_ch <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0)%Z
   | 38 => (1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn <= 0 /\ -1 * s V_reduce_side_ch + 1 <= 0)%Z
   | 39 => (-1 * s V_reduce_side_ch + 1 <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0)%Z
   | 40 => (1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ -1 * s V_reduce_side_z <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn <= 0 /\ -1 * s V_reduce_side_ch + 1 <= 0)%Z
   | 41 => (-1 * s V_reduce_side_ch + 1 <= 0 /\ 1 * s V_reduce_side_ch+ -1 * s V_reduce_side_numchn <= 0 /\ -1 * s V_reduce_side_numchn + 2 <= 0 /\ 1 * s V_reduce_side_numchn + -2 <= 0 /\ -1 * s V_reduce_side_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_reduce_side (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((2 # 1) <= z)%Q
   | 2 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 3 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 4 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 5 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 6 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 7 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 8 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 9 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 10 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 11 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 12 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 13 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 14 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 15 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 16 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 17 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 18 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 19 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 20 => ((2 # 1) + s V_reduce_side_z <= z)%Q
   | 21 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 22 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 23 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 24 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (2 - s V_reduce_side_ch)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                               - s V_reduce_side_ch) (0))) (F_max0_ge_0 (2
                                                                    - s V_reduce_side_ch))]
     ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 25 => (s V_reduce_side_z <= z)%Q
   | 26 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 27 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 28 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 29 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 30 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 31 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 32 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 33 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 34 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 35 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 36 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 37 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 38 => ((3 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 39 => ((3 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 40 => ((3 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | 41 => ((2 # 1) - s V_reduce_side_ch + s V_reduce_side_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_reduce_side =>
    [mkPA Q (fun n z s => ai_reduce_side n s /\ annot0_reduce_side n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_reduce_side (proc_start P_reduce_side) s1 (proc_end P_reduce_side) s2 ->
    (s2 V_reduce_side_z <= (2 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_reduce_side.
Qed.
