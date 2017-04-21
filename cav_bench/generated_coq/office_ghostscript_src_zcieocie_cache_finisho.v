Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cie_cache_finish.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cie_cache_finish_z := 1%positive.
Notation V_cie_cache_finish__tmp := 2%positive.
Notation V_cie_cache_finish_code := 3%positive.
Notation V_cie_cache_finish_i := 4%positive.
Notation V_cie_cache_finish_op := 5%positive.
Definition Pedges_cie_cache_finish: list (edge proc) :=
  (EA 1 (AAssign V_cie_cache_finish_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_cie_cache_finish_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 ANone 36)::(EA 4 ANone 5)::
  (EA 5 (AAssign V_cie_cache_finish_code None) 6)::(EA 6 ANone 7)::
  (EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_cie_cache_finish_code) s) < (eval (ENum (0))
  s))%Z)) 11)::(EA 9 (AGuard (fun s => ((eval (EVar V_cie_cache_finish_code)
  s) >= (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 18)::
  (EA 11 AWeaken 12)::(EA 12 (AAssign V_cie_cache_finish_i
  (Some (ENum (0)))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_cie_cache_finish_i) s) <
  (eval (ENum (512)) s))%Z)) 21)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_cie_cache_finish_i) s) >= (eval (ENum (512))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 18)::(EA 18 (AAssign
  V_cie_cache_finish__tmp (Some (ENum (14)))) 19)::(EA 19 ANone 20)::
  (EA 20 AWeaken 39)::(EA 21 AWeaken 22)::(EA 22 (AAssign
  V_cie_cache_finish_code None) 23)::(EA 23 AWeaken 24)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_cie_cache_finish_code) s) < (eval (ENum (0))
  s))%Z)) 32)::(EA 24 (AGuard (fun s => ((eval (EVar V_cie_cache_finish_code)
  s) >= (eval (ENum (0)) s))%Z)) 25)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::
  (EA 27 (AAssign V_cie_cache_finish_i
  (Some (EAdd (EVar V_cie_cache_finish_i) (ENum (1))))) 28)::
  (EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign V_cie_cache_finish_z
  (Some (EAdd (ENum (1)) (EVar V_cie_cache_finish_z)))) 31)::
  (EA 31 AWeaken 15)::(EA 32 AWeaken 33)::(EA 33 (AAssign
  V_cie_cache_finish__tmp (Some (EVar V_cie_cache_finish_code))) 34)::
  (EA 34 ANone 35)::(EA 35 AWeaken 39)::(EA 36 (AAssign
  V_cie_cache_finish__tmp (Some (ENum (-104)))) 37)::(EA 37 ANone 38)::
  (EA 38 AWeaken 39)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cie_cache_finish => Pedges_cie_cache_finish
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cie_cache_finish => 39
     end)%positive;
  var_global := var_global
}.

Definition ai_cie_cache_finish (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_z <= 0)%Z
   | 3 => (-1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 4 => (-1 * s V_cie_cache_finish_i <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_z <= 0)%Z
   | 5 => (-1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 6 => (-1 * s V_cie_cache_finish_i <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_z <= 0)%Z
   | 7 => (-1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 8 => (-1 * s V_cie_cache_finish_i <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_z <= 0)%Z
   | 9 => (-1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 10 => (-1 * s V_cie_cache_finish_i <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_code <= 0)%Z
   | 11 => (-1 * s V_cie_cache_finish_i <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_code + 1 <= 0)%Z
   | 12 => (1 * s V_cie_cache_finish_code + 1 <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 13 => (1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_code + 1 <= 0 /\ 1 * s V_cie_cache_finish_i <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 14 => (-1 * s V_cie_cache_finish_i <= 0 /\ 1 * s V_cie_cache_finish_i <= 0 /\ 1 * s V_cie_cache_finish_code + 1 <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_z <= 0)%Z
   | 15 => (-1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0 /\ 1 * s V_cie_cache_finish_i + -512 <= 0)%Z
   | 16 => (1 * s V_cie_cache_finish_i + -512 <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i + 512 <= 0)%Z
   | 17 => (-1 * s V_cie_cache_finish_i + 512 <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_i + -512 <= 0)%Z
   | 18 => (-1 * s V_cie_cache_finish_i <= 0 /\ -1 * s V_cie_cache_finish_z <= 0)%Z
   | 19 => (-1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0 /\ 1 * s V_cie_cache_finish__tmp + -14 <= 0 /\ -1 * s V_cie_cache_finish__tmp + 14 <= 0)%Z
   | 20 => (-1 * s V_cie_cache_finish__tmp + 14 <= 0 /\ 1 * s V_cie_cache_finish__tmp + -14 <= 0 /\ -1 * s V_cie_cache_finish_i <= 0 /\ -1 * s V_cie_cache_finish_z <= 0)%Z
   | 21 => (-1 * s V_cie_cache_finish_i <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_i + -511 <= 0)%Z
   | 22 => (1 * s V_cie_cache_finish_i + -511 <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 23 => (-1 * s V_cie_cache_finish_i <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_i + -511 <= 0)%Z
   | 24 => (1 * s V_cie_cache_finish_i + -511 <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 25 => (-1 * s V_cie_cache_finish_i <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_i + -511 <= 0 /\ -1 * s V_cie_cache_finish_code <= 0)%Z
   | 26 => (-1 * s V_cie_cache_finish_code <= 0 /\ 1 * s V_cie_cache_finish_i + -511 <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 27 => (-1 * s V_cie_cache_finish_i <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_i + -511 <= 0 /\ -1 * s V_cie_cache_finish_code <= 0)%Z
   | 28 => (-1 * s V_cie_cache_finish_code <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i + 1 <= 0 /\ 1 * s V_cie_cache_finish_i + -512 <= 0)%Z
   | 29 => (1 * s V_cie_cache_finish_i + -512 <= 0 /\ -1 * s V_cie_cache_finish_i + 1 <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_code <= 0)%Z
   | 30 => (-1 * s V_cie_cache_finish_code <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i + 1 <= 0 /\ 1 * s V_cie_cache_finish_i + -512 <= 0)%Z
   | 31 => (1 * s V_cie_cache_finish_i + -512 <= 0 /\ -1 * s V_cie_cache_finish_i + 1 <= 0 /\ -1 * s V_cie_cache_finish_code <= 0 /\ -1 * s V_cie_cache_finish_z + 1 <= 0)%Z
   | 32 => (-1 * s V_cie_cache_finish_i <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_i + -511 <= 0 /\ 1 * s V_cie_cache_finish_code + 1 <= 0)%Z
   | 33 => (1 * s V_cie_cache_finish_code + 1 <= 0 /\ 1 * s V_cie_cache_finish_i + -511 <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 34 => (-1 * s V_cie_cache_finish_i <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_i + -511 <= 0 /\ 1 * s V_cie_cache_finish_code + 1 <= 0 /\ 1 * s V_cie_cache_finish__tmp + 1 <= 0)%Z
   | 35 => (1 * s V_cie_cache_finish__tmp + 1 <= 0 /\ 1 * s V_cie_cache_finish_code + 1 <= 0 /\ 1 * s V_cie_cache_finish_i + -511 <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 36 => (-1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 37 => (-1 * s V_cie_cache_finish_i <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish__tmp + 104 <= 0 /\ -1 * s V_cie_cache_finish__tmp + -104 <= 0)%Z
   | 38 => (-1 * s V_cie_cache_finish__tmp + -104 <= 0 /\ 1 * s V_cie_cache_finish__tmp + 104 <= 0 /\ -1 * s V_cie_cache_finish_z <= 0 /\ 1 * s V_cie_cache_finish_z <= 0 /\ -1 * s V_cie_cache_finish_i <= 0)%Z
   | 39 => (1 * s V_cie_cache_finish__tmp + -14 <= 0 /\ -1 * s V_cie_cache_finish_i <= 0 /\ -1 * s V_cie_cache_finish_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cie_cache_finish (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((512 # 1) <= z)%Q
   | 2 => ((512 # 1) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 3 => ((512 # 1) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 4 => ((512 # 1) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 5 => ((512 # 1) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 6 => ((512 # 1) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 7 => ((512 # 1) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 8 => ((512 # 1) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 9 => ((512 # 1) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 10 => hints
     [(*0 512*) F_one]
     ((512 # 1) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 11 => ((512 # 1) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 12 => ((512 # 1) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 13 => (max0(512 - s V_cie_cache_finish_i) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cie_cache_finish_z)) (F_check_ge (s V_cie_cache_finish_z) (0))]
     (max0(512 - s V_cie_cache_finish_i) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 15 => (s V_cie_cache_finish_z + max0(512 - s V_cie_cache_finish_i) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (512 - s V_cie_cache_finish_i) (511
                                                                    - s V_cie_cache_finish_i));
      (*-1 0*) F_max0_ge_0 (511 - s V_cie_cache_finish_i);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_cie_cache_finish_z) (0))) (F_max0_ge_0 (s V_cie_cache_finish_z))]
     (s V_cie_cache_finish_z + max0(512 - s V_cie_cache_finish_i) <= z)%Q
   | 17 => (max0(s V_cie_cache_finish_z) <= z)%Q
   | 18 => (max0(s V_cie_cache_finish_z) <= z)%Q
   | 19 => (max0(s V_cie_cache_finish_z) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cie_cache_finish_z)) (F_check_ge (s V_cie_cache_finish_z) (0))]
     (max0(s V_cie_cache_finish_z) <= z)%Q
   | 21 => (s V_cie_cache_finish_z + max0(512 - s V_cie_cache_finish_i) <= z)%Q
   | 22 => (s V_cie_cache_finish_z + max0(512 - s V_cie_cache_finish_i) <= z)%Q
   | 23 => (s V_cie_cache_finish_z + max0(512 - s V_cie_cache_finish_i) <= z)%Q
   | 24 => (s V_cie_cache_finish_z + max0(512 - s V_cie_cache_finish_i) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (512 - s V_cie_cache_finish_i) (1)]
     (s V_cie_cache_finish_z + max0(512 - s V_cie_cache_finish_i) <= z)%Q
   | 26 => ((1 # 1) + s V_cie_cache_finish_z
            + max0(511 - s V_cie_cache_finish_i) <= z)%Q
   | 27 => ((1 # 1) + s V_cie_cache_finish_z
            + max0(511 - s V_cie_cache_finish_i) <= z)%Q
   | 28 => ((1 # 1) + s V_cie_cache_finish_z
            + max0(512 - s V_cie_cache_finish_i) <= z)%Q
   | 29 => ((1 # 1) + s V_cie_cache_finish_z
            + max0(512 - s V_cie_cache_finish_i) <= z)%Q
   | 30 => ((1 # 1) + s V_cie_cache_finish_z
            + max0(512 - s V_cie_cache_finish_i) <= z)%Q
   | 31 => (s V_cie_cache_finish_z + max0(512 - s V_cie_cache_finish_i) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (512 - s V_cie_cache_finish_i)) (F_check_ge (0) (0))]
     (s V_cie_cache_finish_z + max0(512 - s V_cie_cache_finish_i) <= z)%Q
   | 33 => (s V_cie_cache_finish_z <= z)%Q
   | 34 => (s V_cie_cache_finish_z <= z)%Q
   | 35 => (s V_cie_cache_finish_z <= z)%Q
   | 36 => ((512 # 1) + max0(s V_cie_cache_finish_z) <= z)%Q
   | 37 => ((512 # 103) * max0(-1 - s V_cie_cache_finish__tmp)
            + max0(s V_cie_cache_finish_z) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cie_cache_finish_z)) (F_check_ge (s V_cie_cache_finish_z) (0));
      (*-4.97087 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                       - s V_cie_cache_finish__tmp)) (F_check_ge (0) (0))]
     ((512 # 103) * max0(-1 - s V_cie_cache_finish__tmp)
      + max0(s V_cie_cache_finish_z) <= z)%Q
   | 39 => (s V_cie_cache_finish_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cie_cache_finish =>
    [mkPA Q (fun n z s => ai_cie_cache_finish n s /\ annot0_cie_cache_finish n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cie_cache_finish (proc_start P_cie_cache_finish) s1 (proc_end P_cie_cache_finish) s2 ->
    (s2 V_cie_cache_finish_z <= (512 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cie_cache_finish.
Qed.
