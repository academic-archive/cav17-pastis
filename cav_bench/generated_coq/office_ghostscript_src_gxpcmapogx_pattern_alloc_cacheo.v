Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gx_pattern_alloc_cache.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gx_pattern_alloc_cache_z := 1%positive.
Notation V_gx_pattern_alloc_cache__tmp := 2%positive.
Notation V_gx_pattern_alloc_cache__tmp1 := 3%positive.
Notation V_gx_pattern_alloc_cache_i := 4%positive.
Notation V_gx_pattern_alloc_cache_max_bits := 5%positive.
Notation V_gx_pattern_alloc_cache_mem := 6%positive.
Notation V_gx_pattern_alloc_cache_num_tiles := 7%positive.
Definition Pedges_gx_pattern_alloc_cache: list (edge proc) :=
  (EA 1 (AAssign V_gx_pattern_alloc_cache_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_gx_pattern_alloc_cache_i) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_gx_pattern_alloc_cache__tmp) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_gx_pattern_alloc_cache__tmp
  (Some (EVar V_gx_pattern_alloc_cache_num_tiles))) 6)::(EA 6 (AAssign
  V_gx_pattern_alloc_cache__tmp1
  (Some (EVar V_gx_pattern_alloc_cache_max_bits))) 7)::(EA 7 AWeaken 8)::
  (EA 8 ANone 25)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 ANone 25)::
  (EA 10 ANone 11)::(EA 11 (AAssign V_gx_pattern_alloc_cache_i
  (Some (ENum (0)))) 12)::(EA 12 ANone 13)::(EA 13 AWeaken 14)::
  (EA 14 (AGuard (fun s => ((eval (EVar V_gx_pattern_alloc_cache_i) s) <
  (eval (EVar V_gx_pattern_alloc_cache__tmp) s))%Z)) 18)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_gx_pattern_alloc_cache_i) s) >=
  (eval (EVar V_gx_pattern_alloc_cache__tmp) s))%Z)) 15)::
  (EA 15 AWeaken 16)::(EA 16 ANone 17)::(EA 17 AWeaken 27)::
  (EA 18 AWeaken 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_gx_pattern_alloc_cache_i (Some (EAdd (EVar V_gx_pattern_alloc_cache_i)
  (ENum (1))))) 21)::(EA 21 ANone 22)::(EA 22 ANone 23)::(EA 23 (AAssign
  V_gx_pattern_alloc_cache_z (Some (EAdd (ENum (1))
  (EVar V_gx_pattern_alloc_cache_z)))) 24)::(EA 24 AWeaken 14)::
  (EA 25 ANone 26)::(EA 26 AWeaken 27)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gx_pattern_alloc_cache => Pedges_gx_pattern_alloc_cache
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gx_pattern_alloc_cache => 27
     end)%positive;
  var_global := var_global
}.

Definition ai_gx_pattern_alloc_cache (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0)%Z
   | 3 => (-1 * s V_gx_pattern_alloc_cache_z <= 0 /\ 1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 4 => (-1 * s V_gx_pattern_alloc_cache_i <= 0 /\ 1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache__tmp <= 0)%Z
   | 5 => (-1 * s V_gx_pattern_alloc_cache__tmp <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0 /\ 1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 6 => (-1 * s V_gx_pattern_alloc_cache_i <= 0 /\ 1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0)%Z
   | 7 => (-1 * s V_gx_pattern_alloc_cache_z <= 0 /\ 1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 8 => (-1 * s V_gx_pattern_alloc_cache_i <= 0 /\ 1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0)%Z
   | 9 => (-1 * s V_gx_pattern_alloc_cache_z <= 0 /\ 1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 10 => (-1 * s V_gx_pattern_alloc_cache_i <= 0 /\ 1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0)%Z
   | 11 => (-1 * s V_gx_pattern_alloc_cache_z <= 0 /\ 1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 12 => (1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0 /\ 1 * s V_gx_pattern_alloc_cache_i <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 13 => (-1 * s V_gx_pattern_alloc_cache_i <= 0 /\ 1 * s V_gx_pattern_alloc_cache_i <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0 /\ 1 * s V_gx_pattern_alloc_cache_z <= 0)%Z
   | 14 => (-1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 15 => (-1 * s V_gx_pattern_alloc_cache_i <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0 /\ 1 * s V_gx_pattern_alloc_cache__tmp+ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 16 => (1 * s V_gx_pattern_alloc_cache__tmp+ -1 * s V_gx_pattern_alloc_cache_i <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 17 => (-1 * s V_gx_pattern_alloc_cache_i <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0 /\ 1 * s V_gx_pattern_alloc_cache__tmp+ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 18 => (-1 * s V_gx_pattern_alloc_cache_i <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache__tmp+ 1 * s V_gx_pattern_alloc_cache_i + 1 <= 0)%Z
   | 19 => (-1 * s V_gx_pattern_alloc_cache__tmp+ 1 * s V_gx_pattern_alloc_cache_i + 1 <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 20 => (-1 * s V_gx_pattern_alloc_cache_i <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache__tmp+ 1 * s V_gx_pattern_alloc_cache_i + 1 <= 0)%Z
   | 21 => (-1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i + 1 <= 0 /\ -1 * s V_gx_pattern_alloc_cache__tmp+ 1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 22 => (-1 * s V_gx_pattern_alloc_cache__tmp+ 1 * s V_gx_pattern_alloc_cache_i <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i + 1 <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0)%Z
   | 23 => (-1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i + 1 <= 0 /\ -1 * s V_gx_pattern_alloc_cache__tmp+ 1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 24 => (-1 * s V_gx_pattern_alloc_cache__tmp+ 1 * s V_gx_pattern_alloc_cache_i <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i + 1 <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z + 1 <= 0)%Z
   | 25 => (-1 * s V_gx_pattern_alloc_cache_z <= 0 /\ 1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | 26 => (-1 * s V_gx_pattern_alloc_cache_i <= 0 /\ 1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_z <= 0)%Z
   | 27 => (-1 * s V_gx_pattern_alloc_cache_z <= 0 /\ -1 * s V_gx_pattern_alloc_cache_i <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gx_pattern_alloc_cache (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_gx_pattern_alloc_cache_num_tiles) <= z)%Q
   | 2 => (s V_gx_pattern_alloc_cache_z
           + max0(s V_gx_pattern_alloc_cache_num_tiles) <= z)%Q
   | 3 => (s V_gx_pattern_alloc_cache_z
           + max0(s V_gx_pattern_alloc_cache_num_tiles) <= z)%Q
   | 4 => (s V_gx_pattern_alloc_cache_z
           + max0(s V_gx_pattern_alloc_cache_num_tiles) <= z)%Q
   | 5 => (s V_gx_pattern_alloc_cache_z
           + max0(s V_gx_pattern_alloc_cache_num_tiles) <= z)%Q
   | 6 => (s V_gx_pattern_alloc_cache_z
           + max0(s V_gx_pattern_alloc_cache__tmp) <= z)%Q
   | 7 => (s V_gx_pattern_alloc_cache_z
           + max0(s V_gx_pattern_alloc_cache__tmp) <= z)%Q
   | 8 => (s V_gx_pattern_alloc_cache_z
           + max0(s V_gx_pattern_alloc_cache__tmp) <= z)%Q
   | 9 => (s V_gx_pattern_alloc_cache_z
           + max0(s V_gx_pattern_alloc_cache__tmp) <= z)%Q
   | 10 => (s V_gx_pattern_alloc_cache_z
            + max0(s V_gx_pattern_alloc_cache__tmp) <= z)%Q
   | 11 => (s V_gx_pattern_alloc_cache_z
            + max0(s V_gx_pattern_alloc_cache__tmp) <= z)%Q
   | 12 => (s V_gx_pattern_alloc_cache_z
            + max0(s V_gx_pattern_alloc_cache__tmp
                   - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 13 => (s V_gx_pattern_alloc_cache_z
            + max0(s V_gx_pattern_alloc_cache__tmp
                   - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 14 => (s V_gx_pattern_alloc_cache_z
            + max0(s V_gx_pattern_alloc_cache__tmp
                   - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 15 => (s V_gx_pattern_alloc_cache_z
            + max0(s V_gx_pattern_alloc_cache__tmp
                   - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 16 => (s V_gx_pattern_alloc_cache_z
            + max0(s V_gx_pattern_alloc_cache__tmp
                   - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_gx_pattern_alloc_cache__tmp
                                             - s V_gx_pattern_alloc_cache_i) (-1
                                                                    + s V_gx_pattern_alloc_cache__tmp
                                                                    - s V_gx_pattern_alloc_cache_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_gx_pattern_alloc_cache__tmp
                            - s V_gx_pattern_alloc_cache_i)]
     (s V_gx_pattern_alloc_cache_z
      + max0(s V_gx_pattern_alloc_cache__tmp - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 18 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_gx_pattern_alloc_cache__tmp
                                      - s V_gx_pattern_alloc_cache_i) (1)]
     (s V_gx_pattern_alloc_cache_z
      + max0(s V_gx_pattern_alloc_cache__tmp - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 19 => ((1 # 1) + s V_gx_pattern_alloc_cache_z
            + max0(-1 + s V_gx_pattern_alloc_cache__tmp
                   - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 20 => ((1 # 1) + s V_gx_pattern_alloc_cache_z
            + max0(-1 + s V_gx_pattern_alloc_cache__tmp
                   - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 21 => ((1 # 1) + s V_gx_pattern_alloc_cache_z
            + max0(s V_gx_pattern_alloc_cache__tmp
                   - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 22 => ((1 # 1) + s V_gx_pattern_alloc_cache_z
            + max0(s V_gx_pattern_alloc_cache__tmp
                   - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 23 => ((1 # 1) + s V_gx_pattern_alloc_cache_z
            + max0(s V_gx_pattern_alloc_cache__tmp
                   - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 24 => (s V_gx_pattern_alloc_cache_z
            + max0(s V_gx_pattern_alloc_cache__tmp
                   - s V_gx_pattern_alloc_cache_i) <= z)%Q
   | 25 => (s V_gx_pattern_alloc_cache_z
            + max0(s V_gx_pattern_alloc_cache__tmp) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_ge_0 (s V_gx_pattern_alloc_cache__tmp)]
     (s V_gx_pattern_alloc_cache_z + max0(s V_gx_pattern_alloc_cache__tmp) <= z)%Q
   | 27 => (s V_gx_pattern_alloc_cache_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gx_pattern_alloc_cache =>
    [mkPA Q (fun n z s => ai_gx_pattern_alloc_cache n s /\ annot0_gx_pattern_alloc_cache n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gx_pattern_alloc_cache (proc_start P_gx_pattern_alloc_cache) s1 (proc_end P_gx_pattern_alloc_cache) s2 ->
    (s2 V_gx_pattern_alloc_cache_z <= max0(s1 V_gx_pattern_alloc_cache_num_tiles))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gx_pattern_alloc_cache.
Qed.
