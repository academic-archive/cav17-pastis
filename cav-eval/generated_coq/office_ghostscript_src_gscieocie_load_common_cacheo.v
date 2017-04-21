Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cie_load_common_cache.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cie_load_common_cache_z := 1%positive.
Notation V_cie_load_common_cache__tmp := 2%positive.
Notation V_cie_load_common_cache_i := 3%positive.
Notation V_cie_load_common_cache_j := 4%positive.
Notation V_cie_load_common_cache_cname := 5%positive.
Notation V_cie_load_common_cache_pcie := 6%positive.
Notation V_cie_load_common_cache_pgs := 7%positive.
Definition Pedges_cie_load_common_cache: list (edge proc) :=
  (EA 1 (AAssign V_cie_load_common_cache_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_cie_load_common_cache_i (Some (ENum (0)))) 3)::
  (EA 3 ANone 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_cie_load_common_cache_i) s) < (eval (ENum (512))
  s))%Z)) 20)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_cie_load_common_cache_i) s) >= (eval (ENum (512))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 ANone 16)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 ANone 13)::(EA 9 ANone 10)::(EA 10 (AAssign
  V_cie_load_common_cache__tmp (Some (ENum (0)))) 11)::(EA 11 ANone 12)::
  (EA 12 AWeaken 19)::(EA 13 (AAssign V_cie_load_common_cache__tmp
  (Some (ENum (-25)))) 14)::(EA 14 ANone 15)::(EA 15 AWeaken 19)::
  (EA 16 (AAssign V_cie_load_common_cache__tmp (Some (ENum (0)))) 17)::
  (EA 17 ANone 18)::(EA 18 AWeaken 19)::(EA 20 AWeaken 21)::(EA 21 (AAssign
  V_cie_load_common_cache_j (Some (ENum (0)))) 22)::(EA 22 ANone 23)::
  (EA 23 AWeaken 24)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_cie_load_common_cache_j) s) < (eval (ENum (3))
  s))%Z)) 32)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_cie_load_common_cache_j) s) >= (eval (ENum (3))
  s))%Z)) 25)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::(EA 27 (AAssign
  V_cie_load_common_cache_i (Some (EAdd (EVar V_cie_load_common_cache_i)
  (ENum (1))))) 28)::(EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign
  V_cie_load_common_cache_z (Some (EAdd (ENum (1))
  (EVar V_cie_load_common_cache_z)))) 31)::(EA 31 AWeaken 5)::
  (EA 32 AWeaken 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_cie_load_common_cache_j (Some (EAdd (EVar V_cie_load_common_cache_j)
  (ENum (1))))) 35)::(EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign
  V_cie_load_common_cache_z (Some (EAdd (ENum (1))
  (EVar V_cie_load_common_cache_z)))) 38)::(EA 38 AWeaken 24)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cie_load_common_cache => Pedges_cie_load_common_cache
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cie_load_common_cache => 19
     end)%positive;
  var_global := var_global
}.

Definition ai_cie_load_common_cache (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0)%Z
   | 3 => (-1 * s V_cie_load_common_cache_z <= 0 /\ 1 * s V_cie_load_common_cache_z <= 0 /\ 1 * s V_cie_load_common_cache_i <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0)%Z
   | 4 => (-1 * s V_cie_load_common_cache_i <= 0 /\ 1 * s V_cie_load_common_cache_i <= 0 /\ 1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0)%Z
   | 5 => (-1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0)%Z
   | 6 => (-1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i + 512 <= 0)%Z
   | 7 => (-1 * s V_cie_load_common_cache_i + 512 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0)%Z
   | 8 => (-1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i + 512 <= 0)%Z
   | 9 => (-1 * s V_cie_load_common_cache_i + 512 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0)%Z
   | 10 => (-1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i + 512 <= 0)%Z
   | 11 => (-1 * s V_cie_load_common_cache_i + 512 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ 1 * s V_cie_load_common_cache__tmp <= 0 /\ -1 * s V_cie_load_common_cache__tmp <= 0)%Z
   | 12 => (-1 * s V_cie_load_common_cache__tmp <= 0 /\ 1 * s V_cie_load_common_cache__tmp <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i + 512 <= 0)%Z
   | 13 => (-1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i + 512 <= 0)%Z
   | 14 => (-1 * s V_cie_load_common_cache_i + 512 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ 1 * s V_cie_load_common_cache__tmp + 25 <= 0 /\ -1 * s V_cie_load_common_cache__tmp + -25 <= 0)%Z
   | 15 => (-1 * s V_cie_load_common_cache__tmp + -25 <= 0 /\ 1 * s V_cie_load_common_cache__tmp + 25 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i + 512 <= 0)%Z
   | 16 => (-1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i + 512 <= 0)%Z
   | 17 => (-1 * s V_cie_load_common_cache_i + 512 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ 1 * s V_cie_load_common_cache__tmp <= 0 /\ -1 * s V_cie_load_common_cache__tmp <= 0)%Z
   | 18 => (-1 * s V_cie_load_common_cache__tmp <= 0 /\ 1 * s V_cie_load_common_cache__tmp <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i + 512 <= 0)%Z
   | 19 => (-1 * s V_cie_load_common_cache__tmp + -25 <= 0 /\ -1 * s V_cie_load_common_cache_i + 512 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ 1 * s V_cie_load_common_cache__tmp <= 0)%Z
   | 20 => (-1 * s V_cie_load_common_cache_i <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ 1 * s V_cie_load_common_cache_i + -511 <= 0)%Z
   | 21 => (1 * s V_cie_load_common_cache_i + -511 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0)%Z
   | 22 => (-1 * s V_cie_load_common_cache_i <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ 1 * s V_cie_load_common_cache_i + -511 <= 0 /\ 1 * s V_cie_load_common_cache_j <= 0 /\ -1 * s V_cie_load_common_cache_j <= 0)%Z
   | 23 => (-1 * s V_cie_load_common_cache_j <= 0 /\ 1 * s V_cie_load_common_cache_j <= 0 /\ 1 * s V_cie_load_common_cache_i + -511 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0)%Z
   | 24 => (-1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_j <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0 /\ 1 * s V_cie_load_common_cache_j + -3 <= 0)%Z
   | 25 => (1 * s V_cie_load_common_cache_j + -3 <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_j + 3 <= 0)%Z
   | 26 => (-1 * s V_cie_load_common_cache_j + 3 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0 /\ 1 * s V_cie_load_common_cache_j + -3 <= 0)%Z
   | 27 => (1 * s V_cie_load_common_cache_j + -3 <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_j + 3 <= 0)%Z
   | 28 => (-1 * s V_cie_load_common_cache_j + 3 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ 1 * s V_cie_load_common_cache_j + -3 <= 0 /\ -1 * s V_cie_load_common_cache_i + 1 <= 0)%Z
   | 29 => (-1 * s V_cie_load_common_cache_i + 1 <= 0 /\ 1 * s V_cie_load_common_cache_j + -3 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_j + 3 <= 0)%Z
   | 30 => (-1 * s V_cie_load_common_cache_j + 3 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ 1 * s V_cie_load_common_cache_j + -3 <= 0 /\ -1 * s V_cie_load_common_cache_i + 1 <= 0)%Z
   | 31 => (-1 * s V_cie_load_common_cache_i + 1 <= 0 /\ 1 * s V_cie_load_common_cache_j + -3 <= 0 /\ -1 * s V_cie_load_common_cache_j + 3 <= 0 /\ -1 * s V_cie_load_common_cache_z + 1 <= 0)%Z
   | 32 => (-1 * s V_cie_load_common_cache_i <= 0 /\ -1 * s V_cie_load_common_cache_j <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ 1 * s V_cie_load_common_cache_j + -2 <= 0)%Z
   | 33 => (1 * s V_cie_load_common_cache_j + -2 <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_j <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0)%Z
   | 34 => (-1 * s V_cie_load_common_cache_i <= 0 /\ -1 * s V_cie_load_common_cache_j <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0 /\ 1 * s V_cie_load_common_cache_j + -2 <= 0)%Z
   | 35 => (-1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0 /\ -1 * s V_cie_load_common_cache_j + 1 <= 0 /\ 1 * s V_cie_load_common_cache_j + -3 <= 0)%Z
   | 36 => (1 * s V_cie_load_common_cache_j + -3 <= 0 /\ -1 * s V_cie_load_common_cache_j + 1 <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0 /\ -1 * s V_cie_load_common_cache_z <= 0)%Z
   | 37 => (-1 * s V_cie_load_common_cache_z <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0 /\ -1 * s V_cie_load_common_cache_j + 1 <= 0 /\ 1 * s V_cie_load_common_cache_j + -3 <= 0)%Z
   | 38 => (1 * s V_cie_load_common_cache_j + -3 <= 0 /\ -1 * s V_cie_load_common_cache_j + 1 <= 0 /\ -1 * s V_cie_load_common_cache_i <= 0 /\ -1 * s V_cie_load_common_cache_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cie_load_common_cache (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((2048 # 1) <= z)%Q
   | 2 => ((2048 # 1) + s V_cie_load_common_cache_z <= z)%Q
   | 3 => (s V_cie_load_common_cache_z
           + (4 # 1) * max0(512 - s V_cie_load_common_cache_i) <= z)%Q
   | 4 => (s V_cie_load_common_cache_z
           + (4 # 1) * max0(512 - s V_cie_load_common_cache_i) <= z)%Q
   | 5 => (s V_cie_load_common_cache_z
           + (4 # 1) * max0(512 - s V_cie_load_common_cache_i) <= z)%Q
   | 6 => hints
     [(*0 4*) F_max0_ge_0 (512 - s V_cie_load_common_cache_i)]
     (s V_cie_load_common_cache_z
      + (4 # 1) * max0(512 - s V_cie_load_common_cache_i) <= z)%Q
   | 7 => (s V_cie_load_common_cache_z <= z)%Q
   | 8 => (s V_cie_load_common_cache_z <= z)%Q
   | 9 => (s V_cie_load_common_cache_z <= z)%Q
   | 10 => (s V_cie_load_common_cache_z <= z)%Q
   | 11 => (s V_cie_load_common_cache_z <= z)%Q
   | 12 => (s V_cie_load_common_cache_z <= z)%Q
   | 13 => (s V_cie_load_common_cache_z <= z)%Q
   | 14 => (s V_cie_load_common_cache_z <= z)%Q
   | 15 => (s V_cie_load_common_cache_z <= z)%Q
   | 16 => (s V_cie_load_common_cache_z <= z)%Q
   | 17 => (s V_cie_load_common_cache_z <= z)%Q
   | 18 => (s V_cie_load_common_cache_z <= z)%Q
   | 19 => (s V_cie_load_common_cache_z <= z)%Q
   | 20 => (s V_cie_load_common_cache_z
            + (4 # 1) * max0(512 - s V_cie_load_common_cache_i) <= z)%Q
   | 21 => (s V_cie_load_common_cache_z
            + (4 # 1) * max0(512 - s V_cie_load_common_cache_i) <= z)%Q
   | 22 => (-(3 # 1) + s V_cie_load_common_cache_z
            + max0(3 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(512 - s V_cie_load_common_cache_i) <= z)%Q
   | 23 => hints
     [(*0 4*) F_max0_pre_decrement 1 (512 - s V_cie_load_common_cache_i) (1)]
     (-(3 # 1) + s V_cie_load_common_cache_z
      + max0(3 - s V_cie_load_common_cache_j)
      + (4 # 1) * max0(512 - s V_cie_load_common_cache_i) <= z)%Q
   | 24 => ((1 # 1) + s V_cie_load_common_cache_z
            + max0(3 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(511 - s V_cie_load_common_cache_i) <= z)%Q
   | 25 => ((1 # 1) + s V_cie_load_common_cache_z
            + max0(3 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(511 - s V_cie_load_common_cache_i) <= z)%Q
   | 26 => ((1 # 1) + s V_cie_load_common_cache_z
            + max0(3 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(511 - s V_cie_load_common_cache_i) <= z)%Q
   | 27 => ((1 # 1) + s V_cie_load_common_cache_z
            + max0(3 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(511 - s V_cie_load_common_cache_i) <= z)%Q
   | 28 => ((1 # 1) + s V_cie_load_common_cache_z
            + max0(3 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(512 - s V_cie_load_common_cache_i) <= z)%Q
   | 29 => ((1 # 1) + s V_cie_load_common_cache_z
            + max0(3 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(512 - s V_cie_load_common_cache_i) <= z)%Q
   | 30 => ((1 # 1) + s V_cie_load_common_cache_z
            + max0(3 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(512 - s V_cie_load_common_cache_i) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3
                                                 - s V_cie_load_common_cache_j)) (F_check_ge (0) (0))]
     (s V_cie_load_common_cache_z + max0(3 - s V_cie_load_common_cache_j)
      + (4 # 1) * max0(512 - s V_cie_load_common_cache_i) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (3 - s V_cie_load_common_cache_j) (1)]
     ((1 # 1) + s V_cie_load_common_cache_z
      + max0(3 - s V_cie_load_common_cache_j)
      + (4 # 1) * max0(511 - s V_cie_load_common_cache_i) <= z)%Q
   | 33 => ((2 # 1) + s V_cie_load_common_cache_z
            + max0(2 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(511 - s V_cie_load_common_cache_i) <= z)%Q
   | 34 => ((2 # 1) + s V_cie_load_common_cache_z
            + max0(2 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(511 - s V_cie_load_common_cache_i) <= z)%Q
   | 35 => ((2 # 1) + s V_cie_load_common_cache_z
            + max0(3 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(511 - s V_cie_load_common_cache_i) <= z)%Q
   | 36 => ((2 # 1) + s V_cie_load_common_cache_z
            + max0(3 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(511 - s V_cie_load_common_cache_i) <= z)%Q
   | 37 => ((2 # 1) + s V_cie_load_common_cache_z
            + max0(3 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(511 - s V_cie_load_common_cache_i) <= z)%Q
   | 38 => ((1 # 1) + s V_cie_load_common_cache_z
            + max0(3 - s V_cie_load_common_cache_j)
            + (4 # 1) * max0(511 - s V_cie_load_common_cache_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cie_load_common_cache =>
    [mkPA Q (fun n z s => ai_cie_load_common_cache n s /\ annot0_cie_load_common_cache n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cie_load_common_cache (proc_start P_cie_load_common_cache) s1 (proc_end P_cie_load_common_cache) s2 ->
    (s2 V_cie_load_common_cache_z <= (2048 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cie_load_common_cache.
Qed.
