Require Import pasta.Pasta.

Inductive proc: Type :=
  P_make_odither_array.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_make_odither_array_z := 1%positive.
Notation V_make_odither_array__tmp := 2%positive.
Notation V_make_odither_array_den := 3%positive.
Notation V_make_odither_array_j := 4%positive.
Notation V_make_odither_array_k := 5%positive.
Notation V_make_odither_array_num := 6%positive.
Notation V_make_odither_array_cinfo := 7%positive.
Notation V_make_odither_array_ncolors := 8%positive.
Definition Pedges_make_odither_array: list (edge proc) :=
  (EA 1 (AAssign V_make_odither_array_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_make_odither_array__tmp (Some (EVar V_make_odither_array_ncolors))) 3)::
  (EA 3 (AAssign V_make_odither_array_den (Some (EMul (ENum (512))
  (ESub (EVar V_make_odither_array__tmp) (ENum (1)))))) 4)::(EA 4 (AAssign
  V_make_odither_array_j (Some (ENum (0)))) 5)::(EA 5 ANone 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_make_odither_array_j) s) < (eval (ENum (16))
  s))%Z)) 10)::(EA 7 (AGuard (fun s => ((eval (EVar V_make_odither_array_j)
  s) >= (eval (ENum (16)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 10 AWeaken 11)::
  (EA 11 (AAssign V_make_odither_array_k (Some (ENum (0)))) 12)::
  (EA 12 ANone 13)::(EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_make_odither_array_k) s) < (eval (ENum (16))
  s))%Z)) 22)::(EA 14 (AGuard (fun s => ((eval (EVar V_make_odither_array_k)
  s) >= (eval (ENum (16)) s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 16 ANone 17)::
  (EA 17 (AAssign V_make_odither_array_j
  (Some (EAdd (EVar V_make_odither_array_j) (ENum (1))))) 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign V_make_odither_array_z
  (Some (EAdd (ENum (1)) (EVar V_make_odither_array_z)))) 21)::
  (EA 21 AWeaken 7)::(EA 22 AWeaken 23)::(EA 23 (AAssign
  V_make_odither_array_num None) 24)::(EA 24 AWeaken 25)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_make_odither_array_num) s) < (eval (ENum (0))
  s))%Z)) 28)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_make_odither_array_num) s) >= (eval (ENum (0))
  s))%Z)) 26)::(EA 26 AWeaken 27)::(EA 27 ANone 30)::(EA 28 AWeaken 29)::
  (EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 (AAssign V_make_odither_array_k
  (Some (EAdd (EVar V_make_odither_array_k) (ENum (1))))) 32)::
  (EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 (AAssign V_make_odither_array_z
  (Some (EAdd (ENum (1)) (EVar V_make_odither_array_z)))) 35)::
  (EA 35 AWeaken 14)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_make_odither_array => Pedges_make_odither_array
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_make_odither_array => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_make_odither_array (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_z <= 0)%Z
   | 3 => (-1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_z <= 0)%Z
   | 4 => (1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_z <= 0)%Z
   | 5 => (-1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_j <= 0)%Z
   | 6 => (-1 * s V_make_odither_array_j <= 0 /\ 1 * s V_make_odither_array_j <= 0 /\ 1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_z <= 0)%Z
   | 7 => (-1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_j <= 0)%Z
   | 8 => (-1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_j + 16 <= 0)%Z
   | 9 => (-1 * s V_make_odither_array_j + 16 <= 0 /\ -1 * s V_make_odither_array_z <= 0)%Z
   | 10 => (-1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_j + -15 <= 0)%Z
   | 11 => (1 * s V_make_odither_array_j + -15 <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_j <= 0)%Z
   | 12 => (-1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_j + -15 <= 0 /\ 1 * s V_make_odither_array_k <= 0 /\ -1 * s V_make_odither_array_k <= 0)%Z
   | 13 => (-1 * s V_make_odither_array_k <= 0 /\ 1 * s V_make_odither_array_k <= 0 /\ 1 * s V_make_odither_array_j + -15 <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_j <= 0)%Z
   | 14 => (-1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_k <= 0 /\ -1 * s V_make_odither_array_j <= 0 /\ 1 * s V_make_odither_array_k + -16 <= 0)%Z
   | 15 => (1 * s V_make_odither_array_k + -16 <= 0 /\ -1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_k + 16 <= 0)%Z
   | 16 => (-1 * s V_make_odither_array_k + 16 <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_j <= 0 /\ 1 * s V_make_odither_array_k + -16 <= 0)%Z
   | 17 => (1 * s V_make_odither_array_k + -16 <= 0 /\ -1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_k + 16 <= 0)%Z
   | 18 => (-1 * s V_make_odither_array_k + 16 <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_k + -16 <= 0 /\ -1 * s V_make_odither_array_j + 1 <= 0)%Z
   | 19 => (-1 * s V_make_odither_array_j + 1 <= 0 /\ 1 * s V_make_odither_array_k + -16 <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_k + 16 <= 0)%Z
   | 20 => (-1 * s V_make_odither_array_k + 16 <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_k + -16 <= 0 /\ -1 * s V_make_odither_array_j + 1 <= 0)%Z
   | 21 => (-1 * s V_make_odither_array_j + 1 <= 0 /\ 1 * s V_make_odither_array_k + -16 <= 0 /\ -1 * s V_make_odither_array_k + 16 <= 0 /\ -1 * s V_make_odither_array_z + 1 <= 0)%Z
   | 22 => (-1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_k <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_k + -15 <= 0)%Z
   | 23 => (1 * s V_make_odither_array_k + -15 <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_k <= 0 /\ -1 * s V_make_odither_array_j <= 0)%Z
   | 24 => (-1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_k <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_k + -15 <= 0)%Z
   | 25 => (1 * s V_make_odither_array_k + -15 <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_k <= 0 /\ -1 * s V_make_odither_array_j <= 0)%Z
   | 26 => (-1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_k <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_k + -15 <= 0 /\ -1 * s V_make_odither_array_num <= 0)%Z
   | 27 => (-1 * s V_make_odither_array_num <= 0 /\ 1 * s V_make_odither_array_k + -15 <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_k <= 0 /\ -1 * s V_make_odither_array_j <= 0)%Z
   | 28 => (-1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_k <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_k + -15 <= 0 /\ 1 * s V_make_odither_array_num + 1 <= 0)%Z
   | 29 => (1 * s V_make_odither_array_num + 1 <= 0 /\ 1 * s V_make_odither_array_k + -15 <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_k <= 0 /\ -1 * s V_make_odither_array_j <= 0)%Z
   | 30 => (-1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_k <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_k + -15 <= 0)%Z
   | 31 => (1 * s V_make_odither_array_k + -15 <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_k <= 0 /\ -1 * s V_make_odither_array_j <= 0)%Z
   | 32 => (-1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_k + -16 <= 0 /\ -1 * s V_make_odither_array_k + 1 <= 0)%Z
   | 33 => (-1 * s V_make_odither_array_k + 1 <= 0 /\ 1 * s V_make_odither_array_k + -16 <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ -1 * s V_make_odither_array_j <= 0)%Z
   | 34 => (-1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_z <= 0 /\ 1 * s V_make_odither_array_k + -16 <= 0 /\ -1 * s V_make_odither_array_k + 1 <= 0)%Z
   | 35 => (-1 * s V_make_odither_array_k + 1 <= 0 /\ 1 * s V_make_odither_array_k + -16 <= 0 /\ -1 * s V_make_odither_array_j <= 0 /\ -1 * s V_make_odither_array_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_make_odither_array (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((272 # 1) <= z)%Q
   | 2 => ((272 # 1) + s V_make_odither_array_z <= z)%Q
   | 3 => ((272 # 1) + s V_make_odither_array_z <= z)%Q
   | 4 => ((272 # 1) + s V_make_odither_array_z <= z)%Q
   | 5 => (s V_make_odither_array_z
           + (17 # 1) * max0(16 - s V_make_odither_array_j) <= z)%Q
   | 6 => (s V_make_odither_array_z
           + (17 # 1) * max0(16 - s V_make_odither_array_j) <= z)%Q
   | 7 => (s V_make_odither_array_z
           + (17 # 1) * max0(16 - s V_make_odither_array_j) <= z)%Q
   | 8 => hints
     [(*-17 0*) F_max0_monotonic (F_check_ge (16 - s V_make_odither_array_j) (15
                                                                    - s V_make_odither_array_j));
      (*-17 0*) F_max0_ge_0 (15 - s V_make_odither_array_j)]
     (s V_make_odither_array_z
      + (17 # 1) * max0(16 - s V_make_odither_array_j) <= z)%Q
   | 9 => (s V_make_odither_array_z <= z)%Q
   | 10 => (s V_make_odither_array_z
            + (17 # 1) * max0(16 - s V_make_odither_array_j) <= z)%Q
   | 11 => (s V_make_odither_array_z
            + (17 # 1) * max0(16 - s V_make_odither_array_j) <= z)%Q
   | 12 => (-(16 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(16 - s V_make_odither_array_j)
            + max0(16 - s V_make_odither_array_k) <= z)%Q
   | 13 => hints
     [(*-17 0*) F_max0_pre_decrement 1 (16 - s V_make_odither_array_j) (1)]
     (-(16 # 1) + s V_make_odither_array_z
      + (17 # 1) * max0(16 - s V_make_odither_array_j)
      + max0(16 - s V_make_odither_array_k) <= z)%Q
   | 14 => ((1 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(16 - s V_make_odither_array_k) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_ge_0 (16 - s V_make_odither_array_k)]
     ((1 # 1) + s V_make_odither_array_z
      + (17 # 1) * max0(15 - s V_make_odither_array_j)
      + max0(16 - s V_make_odither_array_k) <= z)%Q
   | 16 => ((1 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j) <= z)%Q
   | 17 => ((1 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j) <= z)%Q
   | 18 => ((1 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(16 - s V_make_odither_array_j) <= z)%Q
   | 19 => ((1 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(16 - s V_make_odither_array_j) <= z)%Q
   | 20 => ((1 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(16 - s V_make_odither_array_j) <= z)%Q
   | 21 => (s V_make_odither_array_z
            + (17 # 1) * max0(16 - s V_make_odither_array_j) <= z)%Q
   | 22 => hints
     [(*0 1*) F_max0_pre_decrement 1 (16 - s V_make_odither_array_k) (1)]
     ((1 # 1) + s V_make_odither_array_z
      + (17 # 1) * max0(15 - s V_make_odither_array_j)
      + max0(16 - s V_make_odither_array_k) <= z)%Q
   | 23 => ((2 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(15 - s V_make_odither_array_k) <= z)%Q
   | 24 => ((2 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(15 - s V_make_odither_array_k) <= z)%Q
   | 25 => ((2 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(15 - s V_make_odither_array_k) <= z)%Q
   | 26 => ((2 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(15 - s V_make_odither_array_k) <= z)%Q
   | 27 => ((2 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(15 - s V_make_odither_array_k) <= z)%Q
   | 28 => ((2 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(15 - s V_make_odither_array_k) <= z)%Q
   | 29 => ((2 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(15 - s V_make_odither_array_k) <= z)%Q
   | 30 => ((2 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(15 - s V_make_odither_array_k) <= z)%Q
   | 31 => ((2 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(15 - s V_make_odither_array_k) <= z)%Q
   | 32 => ((2 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(16 - s V_make_odither_array_k) <= z)%Q
   | 33 => ((2 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(16 - s V_make_odither_array_k) <= z)%Q
   | 34 => ((2 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(16 - s V_make_odither_array_k) <= z)%Q
   | 35 => ((1 # 1) + s V_make_odither_array_z
            + (17 # 1) * max0(15 - s V_make_odither_array_j)
            + max0(16 - s V_make_odither_array_k) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_make_odither_array =>
    [mkPA Q (fun n z s => ai_make_odither_array n s /\ annot0_make_odither_array n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_make_odither_array (proc_start P_make_odither_array) s1 (proc_end P_make_odither_array) s2 ->
    (s2 V_make_odither_array_z <= (272 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_make_odither_array.
Qed.
