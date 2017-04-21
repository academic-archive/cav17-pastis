Require Import pasta.Pasta.

Inductive proc: Type :=
  P_save_cap.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_save_cap_z := 1%positive.
Notation V_save_cap__tmp := 2%positive.
Notation V_save_cap_hitno := 3%positive.
Notation V_save_cap_numhits := 4%positive.
Notation V_save_cap_preadd := 5%positive.
Notation V_save_cap_prestrip := 6%positive.
Notation V_save_cap_sufadd := 7%positive.
Notation V_save_cap_sufstrip := 8%positive.
Notation V_save_cap_pattern := 9%positive.
Notation V_save_cap_savearea := 10%positive.
Notation V_save_cap_word := 11%positive.
Definition Pedges_save_cap: list (edge proc) :=
  (EA 1 (AAssign V_save_cap_z (Some (ENum (0)))) 2)::(EA 2 AWeaken 3)::
  (EA 3 ANone 38)::(EA 3 ANone 4)::(EA 4 (AAssign V_save_cap_hitno
  (Some (EVar V_save_cap_numhits))) 5)::(EA 5 ANone 6)::(EA 6 (AAssign
  V_save_cap_hitno (Some (EAdd (EVar V_save_cap_hitno) (ENum (-1))))) 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EAdd (EVar V_save_cap_hitno) (ENum (-1))) s) >=
  (eval (ENum (0)) s))%Z)) 10)::(EA 8 (AGuard
  (fun s => ((eval (EAdd (EVar V_save_cap_hitno) (ENum (-1))) s) <
  (eval (ENum (0)) s))%Z)) 9)::(EA 9 AWeaken 15)::(EA 10 AWeaken 11)::
  (EA 11 ANone 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard (fun s => True)) 18)::
  (EA 13 (AGuard (fun s => True)) 14)::(EA 14 AWeaken 15)::(EA 15 (AAssign
  V_save_cap__tmp None) 16)::(EA 16 ANone 17)::(EA 17 AWeaken 41)::
  (EA 18 AWeaken 19)::(EA 19 ANone 24)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_save_cap_preadd (Some (ENum (0)))) 21)::(EA 21 (AAssign
  V_save_cap_prestrip (Some (ENum (0)))) 22)::(EA 22 ANone 23)::
  (EA 23 AWeaken 28)::(EA 24 (AAssign V_save_cap_prestrip None) 25)::
  (EA 25 (AAssign V_save_cap_preadd None) 26)::(EA 26 ANone 27)::
  (EA 27 AWeaken 28)::(EA 28 ANone 32)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_save_cap_sufstrip (Some (ENum (0)))) 30)::(EA 30 (AAssign
  V_save_cap_sufadd (Some (ENum (0)))) 31)::(EA 31 ANone 35)::(EA 32 (AAssign
  V_save_cap_sufstrip None) 33)::(EA 33 (AAssign V_save_cap_sufadd
  None) 34)::(EA 34 ANone 35)::(EA 35 ANone 36)::(EA 36 ANone 37)::
  (EA 37 (AAssign V_save_cap_z (Some (EAdd (ENum (1))
  (EVar V_save_cap_z)))) 6)::(EA 38 (AAssign V_save_cap__tmp
  (Some (ENum (0)))) 39)::(EA 39 ANone 40)::(EA 40 AWeaken 41)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_save_cap => Pedges_save_cap
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_save_cap => 41
     end)%positive;
  var_global := var_global
}.

Definition ai_save_cap (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 3 => (-1 * s V_save_cap_z <= 0 /\ 1 * s V_save_cap_z <= 0)%Z
   | 4 => (1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 5 => (-1 * s V_save_cap_z <= 0 /\ 1 * s V_save_cap_z <= 0)%Z
   | 6 => (-1 * s V_save_cap_z <= 0)%Z
   | 7 => (-1 * s V_save_cap_z <= 0)%Z
   | 8 => (-1 * s V_save_cap_z <= 0)%Z
   | 9 => (-1 * s V_save_cap_z <= 0 /\ 1 * s V_save_cap_hitno <= 0)%Z
   | 10 => (-1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0)%Z
   | 11 => (-1 * s V_save_cap_hitno + 1 <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 12 => (-1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0)%Z
   | 13 => (-1 * s V_save_cap_hitno + 1 <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 14 => (-1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0)%Z
   | 15 => (-1 * s V_save_cap_z <= 0)%Z
   | 16 => (-1 * s V_save_cap_z <= 0)%Z
   | 17 => (-1 * s V_save_cap_z <= 0)%Z
   | 18 => (-1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0)%Z
   | 19 => (-1 * s V_save_cap_hitno + 1 <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 20 => (-1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0)%Z
   | 21 => (-1 * s V_save_cap_hitno + 1 <= 0 /\ -1 * s V_save_cap_z <= 0 /\ 1 * s V_save_cap_preadd <= 0 /\ -1 * s V_save_cap_preadd <= 0)%Z
   | 22 => (-1 * s V_save_cap_preadd <= 0 /\ 1 * s V_save_cap_preadd <= 0 /\ -1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0 /\ 1 * s V_save_cap_prestrip <= 0 /\ -1 * s V_save_cap_prestrip <= 0)%Z
   | 23 => (-1 * s V_save_cap_prestrip <= 0 /\ 1 * s V_save_cap_prestrip <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0 /\ -1 * s V_save_cap_z <= 0 /\ 1 * s V_save_cap_preadd <= 0 /\ -1 * s V_save_cap_preadd <= 0)%Z
   | 24 => (-1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0)%Z
   | 25 => (-1 * s V_save_cap_hitno + 1 <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 26 => (-1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0)%Z
   | 27 => (-1 * s V_save_cap_hitno + 1 <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 28 => (-1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0)%Z
   | 29 => (-1 * s V_save_cap_hitno + 1 <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 30 => (-1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0 /\ 1 * s V_save_cap_sufstrip <= 0 /\ -1 * s V_save_cap_sufstrip <= 0)%Z
   | 31 => (-1 * s V_save_cap_sufstrip <= 0 /\ 1 * s V_save_cap_sufstrip <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0 /\ -1 * s V_save_cap_z <= 0 /\ 1 * s V_save_cap_sufadd <= 0 /\ -1 * s V_save_cap_sufadd <= 0)%Z
   | 32 => (-1 * s V_save_cap_hitno + 1 <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 33 => (-1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0)%Z
   | 34 => (-1 * s V_save_cap_hitno + 1 <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 35 => (-1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0)%Z
   | 36 => (-1 * s V_save_cap_hitno + 1 <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 37 => (-1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_hitno + 1 <= 0)%Z
   | 38 => (1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 39 => (-1 * s V_save_cap_z <= 0 /\ 1 * s V_save_cap_z <= 0 /\ 1 * s V_save_cap__tmp <= 0 /\ -1 * s V_save_cap__tmp <= 0)%Z
   | 40 => (-1 * s V_save_cap__tmp <= 0 /\ 1 * s V_save_cap__tmp <= 0 /\ 1 * s V_save_cap_z <= 0 /\ -1 * s V_save_cap_z <= 0)%Z
   | 41 => (-1 * s V_save_cap_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_save_cap (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_save_cap_numhits) <= z)%Q
   | 2 => (s V_save_cap_z + max0(-1 + s V_save_cap_numhits) <= z)%Q
   | 3 => (s V_save_cap_z + max0(-1 + s V_save_cap_numhits) <= z)%Q
   | 4 => (s V_save_cap_z + max0(-1 + s V_save_cap_numhits) <= z)%Q
   | 5 => (s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 6 => (s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 7 => (s V_save_cap_z + max0(s V_save_cap_hitno) <= z)%Q
   | 8 => (s V_save_cap_z + max0(s V_save_cap_hitno) <= z)%Q
   | 9 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_save_cap_hitno) (-1
                                                                 + s V_save_cap_hitno));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_save_cap_hitno)) (F_check_ge (0) (0))]
     (s V_save_cap_z + max0(s V_save_cap_hitno) <= z)%Q
   | 10 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_save_cap_hitno) (1)]
     (s V_save_cap_z + max0(s V_save_cap_hitno) <= z)%Q
   | 11 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 12 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 13 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_one; (*-1 0*) F_max0_ge_0 (-1 + s V_save_cap_hitno)]
     ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 15 => (s V_save_cap_z <= z)%Q
   | 16 => (s V_save_cap_z <= z)%Q
   | 17 => (s V_save_cap_z <= z)%Q
   | 18 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 19 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 20 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 21 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 22 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 23 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 24 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 25 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 26 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 27 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 28 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 29 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 30 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 31 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 32 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 33 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 34 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 35 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 36 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 37 => ((1 # 1) + s V_save_cap_z + max0(-1 + s V_save_cap_hitno) <= z)%Q
   | 38 => (s V_save_cap_z + max0(-1 + s V_save_cap_numhits) <= z)%Q
   | 39 => (s V_save_cap_z + max0(-1 + s V_save_cap_numhits) <= z)%Q
   | 40 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_save_cap_numhits)) (F_check_ge (0) (0))]
     (s V_save_cap_z + max0(-1 + s V_save_cap_numhits) <= z)%Q
   | 41 => (s V_save_cap_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_save_cap =>
    [mkPA Q (fun n z s => ai_save_cap n s /\ annot0_save_cap n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_save_cap (proc_start P_save_cap) s1 (proc_end P_save_cap) s2 ->
    (s2 V_save_cap_z <= max0(-1 + s1 V_save_cap_numhits))%Q.
Proof.
  prove_bound ipa admissible_ipa P_save_cap.
Qed.
