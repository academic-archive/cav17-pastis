Require Import pasta.Pasta.

Inductive proc: Type :=
  P_Gsm_Coder.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_Gsm_Coder_z := 1%positive.
Notation V_Gsm_Coder_i := 2%positive.
Notation V_Gsm_Coder_k := 3%positive.
Notation V_Gsm_Coder_ltmp := 4%positive.
Notation V_Gsm_Coder_LARc := 5%positive.
Notation V_Gsm_Coder_Mc := 6%positive.
Notation V_Gsm_Coder_Nc := 7%positive.
Notation V_Gsm_Coder_S := 8%positive.
Notation V_Gsm_Coder_bc := 9%positive.
Notation V_Gsm_Coder_s := 10%positive.
Notation V_Gsm_Coder_xMc := 11%positive.
Notation V_Gsm_Coder_xmaxc := 12%positive.
Definition Pedges_Gsm_Coder: list (edge proc) :=
  (EA 1 (AAssign V_Gsm_Coder_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_Gsm_Coder_k (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_Gsm_Coder_k) s) <= (eval (ENum (3))
  s))%Z)) 8)::(EA 5 (AGuard (fun s => ((eval (EVar V_Gsm_Coder_k) s) >
  (eval (ENum (3)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 8 AWeaken 9)::
  (EA 9 (AAssign V_Gsm_Coder_i (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard (fun s => ((eval (EVar V_Gsm_Coder_i)
  s) <= (eval (ENum (39)) s))%Z)) 20)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_Gsm_Coder_i) s) > (eval (ENum (39)) s))%Z)) 13)::
  (EA 13 AWeaken 14)::(EA 14 ANone 15)::(EA 15 (AAssign V_Gsm_Coder_k
  (Some (EAdd (EVar V_Gsm_Coder_k) (ENum (1))))) 16)::(EA 16 ANone 17)::
  (EA 17 ANone 18)::(EA 18 (AAssign V_Gsm_Coder_z (Some (EAdd (ENum (1))
  (EVar V_Gsm_Coder_z)))) 19)::(EA 19 AWeaken 5)::(EA 20 AWeaken 21)::
  (EA 21 (AAssign V_Gsm_Coder_ltmp None) 22)::(EA 22 AWeaken 23)::
  (EA 23 ANone 25)::(EA 23 ANone 24)::(EA 24 ANone 26)::(EA 25 ANone 26)::
  (EA 26 ANone 27)::(EA 27 (AAssign V_Gsm_Coder_i
  (Some (EAdd (EVar V_Gsm_Coder_i) (ENum (1))))) 28)::(EA 28 ANone 29)::
  (EA 29 ANone 30)::(EA 30 (AAssign V_Gsm_Coder_z (Some (EAdd (ENum (1))
  (EVar V_Gsm_Coder_z)))) 31)::(EA 31 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_Gsm_Coder => Pedges_Gsm_Coder
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_Gsm_Coder => 7
     end)%positive;
  var_global := var_global
}.

Definition ai_Gsm_Coder (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_z <= 0)%Z
   | 3 => (-1 * s V_Gsm_Coder_z <= 0 /\ 1 * s V_Gsm_Coder_z <= 0 /\ 1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_k <= 0)%Z
   | 4 => (-1 * s V_Gsm_Coder_k <= 0 /\ 1 * s V_Gsm_Coder_k <= 0 /\ 1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_z <= 0)%Z
   | 5 => (-1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_k <= 0)%Z
   | 6 => (-1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_k + 4 <= 0)%Z
   | 7 => (-1 * s V_Gsm_Coder_k + 4 <= 0 /\ -1 * s V_Gsm_Coder_z <= 0)%Z
   | 8 => (-1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ 1 * s V_Gsm_Coder_k + -3 <= 0)%Z
   | 9 => (1 * s V_Gsm_Coder_k + -3 <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_k <= 0)%Z
   | 10 => (-1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ 1 * s V_Gsm_Coder_k + -3 <= 0 /\ 1 * s V_Gsm_Coder_i <= 0 /\ -1 * s V_Gsm_Coder_i <= 0)%Z
   | 11 => (-1 * s V_Gsm_Coder_i <= 0 /\ 1 * s V_Gsm_Coder_i <= 0 /\ 1 * s V_Gsm_Coder_k + -3 <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_k <= 0)%Z
   | 12 => (-1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_i <= 0 /\ -1 * s V_Gsm_Coder_k <= 0 /\ 1 * s V_Gsm_Coder_i + -40 <= 0)%Z
   | 13 => (1 * s V_Gsm_Coder_i + -40 <= 0 /\ -1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_i + 40 <= 0)%Z
   | 14 => (-1 * s V_Gsm_Coder_i + 40 <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_k <= 0 /\ 1 * s V_Gsm_Coder_i + -40 <= 0)%Z
   | 15 => (1 * s V_Gsm_Coder_i + -40 <= 0 /\ -1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_i + 40 <= 0)%Z
   | 16 => (-1 * s V_Gsm_Coder_i + 40 <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ 1 * s V_Gsm_Coder_i + -40 <= 0 /\ -1 * s V_Gsm_Coder_k + 1 <= 0)%Z
   | 17 => (-1 * s V_Gsm_Coder_k + 1 <= 0 /\ 1 * s V_Gsm_Coder_i + -40 <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_i + 40 <= 0)%Z
   | 18 => (-1 * s V_Gsm_Coder_i + 40 <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ 1 * s V_Gsm_Coder_i + -40 <= 0 /\ -1 * s V_Gsm_Coder_k + 1 <= 0)%Z
   | 19 => (-1 * s V_Gsm_Coder_k + 1 <= 0 /\ 1 * s V_Gsm_Coder_i + -40 <= 0 /\ -1 * s V_Gsm_Coder_i + 40 <= 0 /\ -1 * s V_Gsm_Coder_z + 1 <= 0)%Z
   | 20 => (-1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_i <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ 1 * s V_Gsm_Coder_i + -39 <= 0)%Z
   | 21 => (1 * s V_Gsm_Coder_i + -39 <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_i <= 0 /\ -1 * s V_Gsm_Coder_k <= 0)%Z
   | 22 => (-1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_i <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ 1 * s V_Gsm_Coder_i + -39 <= 0)%Z
   | 23 => (1 * s V_Gsm_Coder_i + -39 <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_i <= 0 /\ -1 * s V_Gsm_Coder_k <= 0)%Z
   | 24 => (-1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_i <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ 1 * s V_Gsm_Coder_i + -39 <= 0)%Z
   | 25 => (-1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_i <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ 1 * s V_Gsm_Coder_i + -39 <= 0)%Z
   | 26 => (1 * s V_Gsm_Coder_i + -39 <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_i <= 0 /\ -1 * s V_Gsm_Coder_k <= 0)%Z
   | 27 => (-1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_i <= 0 /\ -1 * s V_Gsm_Coder_z <= 0 /\ 1 * s V_Gsm_Coder_i + -39 <= 0)%Z
   | 28 => (-1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_i + 1 <= 0 /\ 1 * s V_Gsm_Coder_i + -40 <= 0)%Z
   | 29 => (1 * s V_Gsm_Coder_i + -40 <= 0 /\ -1 * s V_Gsm_Coder_i + 1 <= 0 /\ -1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_z <= 0)%Z
   | 30 => (-1 * s V_Gsm_Coder_z <= 0 /\ -1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_i + 1 <= 0 /\ 1 * s V_Gsm_Coder_i + -40 <= 0)%Z
   | 31 => (1 * s V_Gsm_Coder_i + -40 <= 0 /\ -1 * s V_Gsm_Coder_i + 1 <= 0 /\ -1 * s V_Gsm_Coder_k <= 0 /\ -1 * s V_Gsm_Coder_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_Gsm_Coder (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((164 # 1) <= z)%Q
   | 2 => ((164 # 1) + s V_Gsm_Coder_z <= z)%Q
   | 3 => (s V_Gsm_Coder_z + (41 # 1) * max0(4 - s V_Gsm_Coder_k) <= z)%Q
   | 4 => (s V_Gsm_Coder_z + (41 # 1) * max0(4 - s V_Gsm_Coder_k) <= z)%Q
   | 5 => (s V_Gsm_Coder_z + (41 # 1) * max0(4 - s V_Gsm_Coder_k) <= z)%Q
   | 6 => hints
     [(*-41 0*) F_max0_ge_0 (4 - s V_Gsm_Coder_k)]
     (s V_Gsm_Coder_z + (41 # 1) * max0(4 - s V_Gsm_Coder_k) <= z)%Q
   | 7 => (s V_Gsm_Coder_z <= z)%Q
   | 8 => (s V_Gsm_Coder_z + (41 # 1) * max0(4 - s V_Gsm_Coder_k) <= z)%Q
   | 9 => (s V_Gsm_Coder_z + (41 # 1) * max0(4 - s V_Gsm_Coder_k) <= z)%Q
   | 10 => (-(40 # 1) + s V_Gsm_Coder_z
            + (41 # 1) * max0(4 - s V_Gsm_Coder_k)
            + max0(40 - s V_Gsm_Coder_i) <= z)%Q
   | 11 => hints
     [(*-41 0*) F_max0_pre_decrement 1 (4 - s V_Gsm_Coder_k) (1)]
     (-(40 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(4 - s V_Gsm_Coder_k)
      + max0(40 - s V_Gsm_Coder_i) <= z)%Q
   | 12 => ((1 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
            + max0(40 - s V_Gsm_Coder_i) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_ge_0 (40 - s V_Gsm_Coder_i)]
     ((1 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
      + max0(40 - s V_Gsm_Coder_i) <= z)%Q
   | 14 => ((1 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k) <= z)%Q
   | 15 => ((1 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k) <= z)%Q
   | 16 => ((1 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(4 - s V_Gsm_Coder_k) <= z)%Q
   | 17 => ((1 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(4 - s V_Gsm_Coder_k) <= z)%Q
   | 18 => ((1 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(4 - s V_Gsm_Coder_k) <= z)%Q
   | 19 => (s V_Gsm_Coder_z + (41 # 1) * max0(4 - s V_Gsm_Coder_k) <= z)%Q
   | 20 => hints
     [(*0 1*) F_max0_pre_decrement 1 (40 - s V_Gsm_Coder_i) (1)]
     ((1 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
      + max0(40 - s V_Gsm_Coder_i) <= z)%Q
   | 21 => ((2 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
            + max0(39 - s V_Gsm_Coder_i) <= z)%Q
   | 22 => ((2 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
            + max0(39 - s V_Gsm_Coder_i) <= z)%Q
   | 23 => ((2 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
            + max0(39 - s V_Gsm_Coder_i) <= z)%Q
   | 24 => ((2 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
            + max0(39 - s V_Gsm_Coder_i) <= z)%Q
   | 25 => ((2 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
            + max0(39 - s V_Gsm_Coder_i) <= z)%Q
   | 26 => ((2 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
            + max0(39 - s V_Gsm_Coder_i) <= z)%Q
   | 27 => ((2 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
            + max0(39 - s V_Gsm_Coder_i) <= z)%Q
   | 28 => ((2 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
            + max0(40 - s V_Gsm_Coder_i) <= z)%Q
   | 29 => ((2 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
            + max0(40 - s V_Gsm_Coder_i) <= z)%Q
   | 30 => ((2 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
            + max0(40 - s V_Gsm_Coder_i) <= z)%Q
   | 31 => ((1 # 1) + s V_Gsm_Coder_z + (41 # 1) * max0(3 - s V_Gsm_Coder_k)
            + max0(40 - s V_Gsm_Coder_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_Gsm_Coder =>
    [mkPA Q (fun n z s => ai_Gsm_Coder n s /\ annot0_Gsm_Coder n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_Gsm_Coder (proc_start P_Gsm_Coder) s1 (proc_end P_Gsm_Coder) s2 ->
    (s2 V_Gsm_Coder_z <= (164 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_Gsm_Coder.
Qed.
