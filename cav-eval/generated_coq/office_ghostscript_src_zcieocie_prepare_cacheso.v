Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cie_prepare_caches.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cie_prepare_caches_z := 1%positive.
Notation V_cie_prepare_caches__tmp := 2%positive.
Notation V_cie_prepare_caches__tmp1 := 3%positive.
Notation V_cie_prepare_caches_code := 4%positive.
Notation V_cie_prepare_caches_i := 5%positive.
Notation V_cie_prepare_caches_cname := 6%positive.
Notation V_cie_prepare_caches_container := 7%positive.
Notation V_cie_prepare_caches_count := 8%positive.
Notation V_cie_prepare_caches_domains := 9%positive.
Notation V_cie_prepare_caches_pgs := 10%positive.
Notation V_cie_prepare_caches_ppc := 11%positive.
Notation V_cie_prepare_caches_procs := 12%positive.
Definition Pedges_cie_prepare_caches: list (edge proc) :=
  (EA 1 (AAssign V_cie_prepare_caches_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_cie_prepare_caches__tmp (Some (EVar V_cie_prepare_caches_count))) 3)::
  (EA 3 (AAssign V_cie_prepare_caches_code (Some (ENum (0)))) 4)::
  (EA 4 (AAssign V_cie_prepare_caches_i (Some (ENum (0)))) 5)::
  (EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_cie_prepare_caches_i) s) <
  (eval (EVar V_cie_prepare_caches__tmp) s))%Z)) 12)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_cie_prepare_caches_i) s) >=
  (eval (EVar V_cie_prepare_caches__tmp) s))%Z)) 8)::(EA 8 AWeaken 9)::
  (EA 9 (AAssign V_cie_prepare_caches__tmp1
  (Some (EVar V_cie_prepare_caches_code))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 25)::(EA 12 AWeaken 13)::(EA 13 (AAssign
  V_cie_prepare_caches_code None) 14)::(EA 14 AWeaken 15)::(EA 15 ANone 22)::
  (EA 15 ANone 16)::(EA 16 ANone 17)::(EA 17 (AAssign V_cie_prepare_caches_i
  (Some (EAdd (EVar V_cie_prepare_caches_i) (ENum (1))))) 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign V_cie_prepare_caches_z
  (Some (EAdd (ENum (1)) (EVar V_cie_prepare_caches_z)))) 21)::
  (EA 21 AWeaken 7)::(EA 22 (AAssign V_cie_prepare_caches__tmp1
  (Some (EVar V_cie_prepare_caches_code))) 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 25)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cie_prepare_caches => Pedges_cie_prepare_caches
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cie_prepare_caches => 25
     end)%positive;
  var_global := var_global
}.

Definition ai_cie_prepare_caches (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0)%Z
   | 3 => (-1 * s V_cie_prepare_caches_z <= 0 /\ 1 * s V_cie_prepare_caches_z <= 0)%Z
   | 4 => (1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ 1 * s V_cie_prepare_caches_code <= 0 /\ -1 * s V_cie_prepare_caches_code <= 0)%Z
   | 5 => (-1 * s V_cie_prepare_caches_code <= 0 /\ 1 * s V_cie_prepare_caches_code <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ 1 * s V_cie_prepare_caches_z <= 0 /\ 1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_i <= 0)%Z
   | 6 => (-1 * s V_cie_prepare_caches_i <= 0 /\ 1 * s V_cie_prepare_caches_i <= 0 /\ 1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ 1 * s V_cie_prepare_caches_code <= 0 /\ -1 * s V_cie_prepare_caches_code <= 0)%Z
   | 7 => (-1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches_i <= 0)%Z
   | 8 => (-1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ 1 * s V_cie_prepare_caches__tmp+ -1 * s V_cie_prepare_caches_i <= 0)%Z
   | 9 => (1 * s V_cie_prepare_caches__tmp+ -1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches_i <= 0)%Z
   | 10 => (-1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ 1 * s V_cie_prepare_caches__tmp+ -1 * s V_cie_prepare_caches_i <= 0)%Z
   | 11 => (1 * s V_cie_prepare_caches__tmp+ -1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches_i <= 0)%Z
   | 12 => (-1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i + 1 <= 0)%Z
   | 13 => (-1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i + 1 <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches_i <= 0)%Z
   | 14 => (-1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i + 1 <= 0)%Z
   | 15 => (-1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i + 1 <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches_i <= 0)%Z
   | 16 => (-1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i + 1 <= 0)%Z
   | 17 => (-1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i + 1 <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches_i <= 0)%Z
   | 18 => (-1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_i + 1 <= 0)%Z
   | 19 => (-1 * s V_cie_prepare_caches_i + 1 <= 0 /\ -1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0)%Z
   | 20 => (-1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_i + 1 <= 0)%Z
   | 21 => (-1 * s V_cie_prepare_caches_i + 1 <= 0 /\ -1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_z + 1 <= 0)%Z
   | 22 => (-1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i + 1 <= 0)%Z
   | 23 => (-1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i + 1 <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches_i <= 0)%Z
   | 24 => (-1 * s V_cie_prepare_caches_i <= 0 /\ -1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches__tmp+ 1 * s V_cie_prepare_caches_i + 1 <= 0)%Z
   | 25 => (-1 * s V_cie_prepare_caches_z <= 0 /\ -1 * s V_cie_prepare_caches_i <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cie_prepare_caches (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_cie_prepare_caches_count) <= z)%Q
   | 2 => (max0(s V_cie_prepare_caches_count)
           + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 3 => (max0(s V_cie_prepare_caches__tmp) + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 4 => (max0(s V_cie_prepare_caches__tmp) + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 5 => (max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i)
           + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 6 => (max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i)
           + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 7 => (max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i)
           + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 8 => (max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i)
           + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 9 => (max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i)
           + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 10 => (max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i)
            + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_cie_prepare_caches__tmp
                                             - s V_cie_prepare_caches_i) (-1
                                                                    + s V_cie_prepare_caches__tmp
                                                                    - s V_cie_prepare_caches_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_cie_prepare_caches__tmp
                            - s V_cie_prepare_caches_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cie_prepare_caches_z)) (F_check_ge (s V_cie_prepare_caches_z) (0))]
     (max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i)
      + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 12 => (max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i)
            + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 13 => (max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i)
            + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 14 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_cie_prepare_caches__tmp
                                      - s V_cie_prepare_caches_i) (1);
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cie_prepare_caches_z)) (F_check_ge (s V_cie_prepare_caches_z) (0))]
     (max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i)
      + max0(s V_cie_prepare_caches_z) <= z)%Q
   | 15 => ((1 # 1) + s V_cie_prepare_caches_z
            + max0(-1 + s V_cie_prepare_caches__tmp
                   - s V_cie_prepare_caches_i) <= z)%Q
   | 16 => ((1 # 1) + s V_cie_prepare_caches_z
            + max0(-1 + s V_cie_prepare_caches__tmp
                   - s V_cie_prepare_caches_i) <= z)%Q
   | 17 => ((1 # 1) + s V_cie_prepare_caches_z
            + max0(-1 + s V_cie_prepare_caches__tmp
                   - s V_cie_prepare_caches_i) <= z)%Q
   | 18 => ((1 # 1) + s V_cie_prepare_caches_z
            + max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i) <= z)%Q
   | 19 => ((1 # 1) + s V_cie_prepare_caches_z
            + max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i) <= z)%Q
   | 20 => ((1 # 1) + s V_cie_prepare_caches_z
            + max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_cie_prepare_caches_z) (0))) (F_max0_ge_0 (s V_cie_prepare_caches_z))]
     (s V_cie_prepare_caches_z
      + max0(s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i) <= z)%Q
   | 22 => ((1 # 1) + s V_cie_prepare_caches_z
            + max0(-1 + s V_cie_prepare_caches__tmp
                   - s V_cie_prepare_caches_i) <= z)%Q
   | 23 => ((1 # 1) + s V_cie_prepare_caches_z
            + max0(-1 + s V_cie_prepare_caches__tmp
                   - s V_cie_prepare_caches_i) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 + s V_cie_prepare_caches__tmp
                            - s V_cie_prepare_caches_i)]
     ((1 # 1) + s V_cie_prepare_caches_z
      + max0(-1 + s V_cie_prepare_caches__tmp - s V_cie_prepare_caches_i) <= z)%Q
   | 25 => (s V_cie_prepare_caches_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cie_prepare_caches =>
    [mkPA Q (fun n z s => ai_cie_prepare_caches n s /\ annot0_cie_prepare_caches n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cie_prepare_caches (proc_start P_cie_prepare_caches) s1 (proc_end P_cie_prepare_caches) s2 ->
    (s2 V_cie_prepare_caches_z <= max0(s1 V_cie_prepare_caches_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cie_prepare_caches.
Qed.
