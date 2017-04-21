Require Import pasta.Pasta.

Inductive proc: Type :=
  P_hc_sizes_from_bytes.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_hc_sizes_from_bytes_z := 1%positive.
Notation V_hc_sizes_from_bytes__tmp := 2%positive.
Notation V_hc_sizes_from_bytes_def_dref_off24 := 3%positive.
Notation V_hc_sizes_from_bytes_def_dref_off8 := 4%positive.
Notation V_hc_sizes_from_bytes_i := 5%positive.
Notation V_hc_sizes_from_bytes_l := 6%positive.
Notation V_hc_sizes_from_bytes_n := 7%positive.
Notation V_hc_sizes_from_bytes_num_counts := 8%positive.
Notation V_hc_sizes_from_bytes_num_values := 9%positive.
Notation V_hc_sizes_from_bytes_dbytes := 10%positive.
Notation V_hc_sizes_from_bytes_def := 11%positive.
Notation V_hc_sizes_from_bytes_num_bytes := 12%positive.
Definition Pedges_hc_sizes_from_bytes: list (edge proc) :=
  (EA 1 (AAssign V_hc_sizes_from_bytes_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_hc_sizes_from_bytes_num_counts) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_hc_sizes_from_bytes_l) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_hc_sizes_from_bytes__tmp
  (Some (EVar V_hc_sizes_from_bytes_num_bytes))) 6)::(EA 6 (AAssign
  V_hc_sizes_from_bytes_num_counts (Some (ENum (0)))) 7)::(EA 7 (AAssign
  V_hc_sizes_from_bytes_num_values (Some (ENum (0)))) 8)::(EA 8 (AAssign
  V_hc_sizes_from_bytes_i (Some (ENum (0)))) 9)::(EA 9 ANone 10)::
  (EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_hc_sizes_from_bytes_i) s) <
  (eval (EVar V_hc_sizes_from_bytes__tmp) s))%Z)) 17)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_hc_sizes_from_bytes_i) s) >=
  (eval (EVar V_hc_sizes_from_bytes__tmp) s))%Z)) 12)::(EA 12 AWeaken 13)::
  (EA 13 (AAssign V_hc_sizes_from_bytes_def_dref_off8
  (Some (EVar V_hc_sizes_from_bytes_num_counts))) 14)::(EA 14 (AAssign
  V_hc_sizes_from_bytes_def_dref_off24
  (Some (EVar V_hc_sizes_from_bytes_num_values))) 15)::(EA 15 AWeaken 16)::
  (EA 17 AWeaken 18)::(EA 18 (AAssign V_hc_sizes_from_bytes_n None) 19)::
  (EA 19 (AAssign V_hc_sizes_from_bytes_l None) 20)::(EA 20 AWeaken 21)::
  (EA 21 (AGuard (fun s => ((eval (EVar V_hc_sizes_from_bytes_l) s) >
  (eval (EVar V_hc_sizes_from_bytes_num_counts) s))%Z)) 23)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_hc_sizes_from_bytes_l) s) <=
  (eval (EVar V_hc_sizes_from_bytes_num_counts) s))%Z)) 22)::
  (EA 22 AWeaken 26)::(EA 23 AWeaken 24)::(EA 24 (AAssign
  V_hc_sizes_from_bytes_num_counts
  (Some (EVar V_hc_sizes_from_bytes_l))) 25)::(EA 25 ANone 26)::
  (EA 26 (AAssign V_hc_sizes_from_bytes_num_values
  (Some (EAdd (EVar V_hc_sizes_from_bytes_num_values)
  (EVar V_hc_sizes_from_bytes_n)))) 27)::(EA 27 ANone 28)::(EA 28 (AAssign
  V_hc_sizes_from_bytes_i (Some (EAdd (EVar V_hc_sizes_from_bytes_i)
  (ENum (1))))) 29)::(EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 (AAssign
  V_hc_sizes_from_bytes_z (Some (EAdd (ENum (1))
  (EVar V_hc_sizes_from_bytes_z)))) 32)::(EA 32 AWeaken 11)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_hc_sizes_from_bytes => Pedges_hc_sizes_from_bytes
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_hc_sizes_from_bytes => 16
     end)%positive;
  var_global := var_global
}.

Definition ai_hc_sizes_from_bytes (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0)%Z
   | 3 => (-1 * s V_hc_sizes_from_bytes_z <= 0 /\ 1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_num_counts <= 0)%Z
   | 4 => (-1 * s V_hc_sizes_from_bytes_num_counts <= 0 /\ 1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_l <= 0)%Z
   | 5 => (-1 * s V_hc_sizes_from_bytes_l <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ 1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_num_counts <= 0)%Z
   | 6 => (-1 * s V_hc_sizes_from_bytes_num_counts <= 0 /\ 1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_l <= 0)%Z
   | 7 => (-1 * s V_hc_sizes_from_bytes_l <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ 1 * s V_hc_sizes_from_bytes_z <= 0 /\ 1 * s V_hc_sizes_from_bytes_num_counts <= 0 /\ -1 * s V_hc_sizes_from_bytes_num_counts <= 0)%Z
   | 8 => (-1 * s V_hc_sizes_from_bytes_num_counts <= 0 /\ 1 * s V_hc_sizes_from_bytes_num_counts <= 0 /\ 1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_l <= 0 /\ 1 * s V_hc_sizes_from_bytes_num_values <= 0 /\ -1 * s V_hc_sizes_from_bytes_num_values <= 0)%Z
   | 9 => (-1 * s V_hc_sizes_from_bytes_num_values <= 0 /\ 1 * s V_hc_sizes_from_bytes_num_values <= 0 /\ -1 * s V_hc_sizes_from_bytes_l <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ 1 * s V_hc_sizes_from_bytes_z <= 0 /\ 1 * s V_hc_sizes_from_bytes_num_counts <= 0 /\ -1 * s V_hc_sizes_from_bytes_num_counts <= 0 /\ 1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 10 => (-1 * s V_hc_sizes_from_bytes_i <= 0 /\ 1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_num_counts <= 0 /\ 1 * s V_hc_sizes_from_bytes_num_counts <= 0 /\ 1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_l <= 0 /\ 1 * s V_hc_sizes_from_bytes_num_values <= 0 /\ -1 * s V_hc_sizes_from_bytes_num_values <= 0)%Z
   | 11 => (-1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 12 => (-1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ 1 * s V_hc_sizes_from_bytes__tmp+ -1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 13 => (1 * s V_hc_sizes_from_bytes__tmp+ -1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 14 => (-1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ 1 * s V_hc_sizes_from_bytes__tmp+ -1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 15 => (1 * s V_hc_sizes_from_bytes__tmp+ -1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 16 => (-1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ 1 * s V_hc_sizes_from_bytes__tmp+ -1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 17 => (-1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i + 1 <= 0)%Z
   | 18 => (-1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i + 1 <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 19 => (-1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i + 1 <= 0)%Z
   | 20 => (-1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i + 1 <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 21 => (-1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i + 1 <= 0)%Z
   | 22 => (-1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i + 1 <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_i <= 0 /\ 1 * s V_hc_sizes_from_bytes_l+ -1 * s V_hc_sizes_from_bytes_num_counts <= 0)%Z
   | 23 => (-1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i + 1 <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_l+ 1 * s V_hc_sizes_from_bytes_num_counts + 1 <= 0)%Z
   | 24 => (-1 * s V_hc_sizes_from_bytes_l+ 1 * s V_hc_sizes_from_bytes_num_counts + 1 <= 0 /\ -1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i + 1 <= 0)%Z
   | 25 => (-1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i + 1 <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 26 => (-1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i + 1 <= 0)%Z
   | 27 => (-1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i + 1 <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 28 => (-1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i + 1 <= 0)%Z
   | 29 => (-1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_i + 1 <= 0 /\ -1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 30 => (-1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_i + 1 <= 0 /\ -1 * s V_hc_sizes_from_bytes_z <= 0)%Z
   | 31 => (-1 * s V_hc_sizes_from_bytes_z <= 0 /\ -1 * s V_hc_sizes_from_bytes_i + 1 <= 0 /\ -1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i <= 0)%Z
   | 32 => (-1 * s V_hc_sizes_from_bytes__tmp+ 1 * s V_hc_sizes_from_bytes_i <= 0 /\ -1 * s V_hc_sizes_from_bytes_i + 1 <= 0 /\ -1 * s V_hc_sizes_from_bytes_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_hc_sizes_from_bytes (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_hc_sizes_from_bytes_num_bytes) <= z)%Q
   | 2 => (max0(s V_hc_sizes_from_bytes_num_bytes)
           + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 3 => (max0(s V_hc_sizes_from_bytes_num_bytes)
           + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 4 => (max0(s V_hc_sizes_from_bytes_num_bytes)
           + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 5 => (max0(s V_hc_sizes_from_bytes_num_bytes)
           + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 6 => (max0(s V_hc_sizes_from_bytes__tmp)
           + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 7 => (max0(s V_hc_sizes_from_bytes__tmp)
           + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 8 => (max0(s V_hc_sizes_from_bytes__tmp)
           + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 9 => (max0(s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i)
           + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 10 => (max0(s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i)
            + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 11 => (max0(s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i)
            + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 12 => (max0(s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i)
            + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 13 => (max0(s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i)
            + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 14 => (max0(s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i)
            + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_hc_sizes_from_bytes__tmp
                                             - s V_hc_sizes_from_bytes_i) (-1
                                                                    + s V_hc_sizes_from_bytes__tmp
                                                                    - s V_hc_sizes_from_bytes_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_hc_sizes_from_bytes__tmp
                            - s V_hc_sizes_from_bytes_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_hc_sizes_from_bytes_z)) (F_check_ge (s V_hc_sizes_from_bytes_z) (0))]
     (max0(s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i)
      + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 16 => (s V_hc_sizes_from_bytes_z <= z)%Q
   | 17 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_hc_sizes_from_bytes__tmp
                                      - s V_hc_sizes_from_bytes_i) (1)]
     (max0(s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i)
      + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 18 => ((1 # 1)
            + max0(-1 + s V_hc_sizes_from_bytes__tmp
                   - s V_hc_sizes_from_bytes_i)
            + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 19 => ((1 # 1)
            + max0(-1 + s V_hc_sizes_from_bytes__tmp
                   - s V_hc_sizes_from_bytes_i)
            + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 20 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_hc_sizes_from_bytes_z)) (F_check_ge (s V_hc_sizes_from_bytes_z) (0))]
     ((1 # 1)
      + max0(-1 + s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i)
      + max0(s V_hc_sizes_from_bytes_z) <= z)%Q
   | 21 => ((1 # 1) + s V_hc_sizes_from_bytes_z
            + max0(-1 + s V_hc_sizes_from_bytes__tmp
                   - s V_hc_sizes_from_bytes_i) <= z)%Q
   | 22 => ((1 # 1) + s V_hc_sizes_from_bytes_z
            + max0(-1 + s V_hc_sizes_from_bytes__tmp
                   - s V_hc_sizes_from_bytes_i) <= z)%Q
   | 23 => ((1 # 1) + s V_hc_sizes_from_bytes_z
            + max0(-1 + s V_hc_sizes_from_bytes__tmp
                   - s V_hc_sizes_from_bytes_i) <= z)%Q
   | 24 => ((1 # 1) + s V_hc_sizes_from_bytes_z
            + max0(-1 + s V_hc_sizes_from_bytes__tmp
                   - s V_hc_sizes_from_bytes_i) <= z)%Q
   | 25 => ((1 # 1) + s V_hc_sizes_from_bytes_z
            + max0(-1 + s V_hc_sizes_from_bytes__tmp
                   - s V_hc_sizes_from_bytes_i) <= z)%Q
   | 26 => ((1 # 1) + s V_hc_sizes_from_bytes_z
            + max0(-1 + s V_hc_sizes_from_bytes__tmp
                   - s V_hc_sizes_from_bytes_i) <= z)%Q
   | 27 => ((1 # 1) + s V_hc_sizes_from_bytes_z
            + max0(-1 + s V_hc_sizes_from_bytes__tmp
                   - s V_hc_sizes_from_bytes_i) <= z)%Q
   | 28 => ((1 # 1) + s V_hc_sizes_from_bytes_z
            + max0(-1 + s V_hc_sizes_from_bytes__tmp
                   - s V_hc_sizes_from_bytes_i) <= z)%Q
   | 29 => ((1 # 1) + s V_hc_sizes_from_bytes_z
            + max0(s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i) <= z)%Q
   | 30 => ((1 # 1) + s V_hc_sizes_from_bytes_z
            + max0(s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i) <= z)%Q
   | 31 => ((1 # 1) + s V_hc_sizes_from_bytes_z
            + max0(s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_hc_sizes_from_bytes_z) (0))) (F_max0_ge_0 (s V_hc_sizes_from_bytes_z))]
     (s V_hc_sizes_from_bytes_z
      + max0(s V_hc_sizes_from_bytes__tmp - s V_hc_sizes_from_bytes_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_hc_sizes_from_bytes =>
    [mkPA Q (fun n z s => ai_hc_sizes_from_bytes n s /\ annot0_hc_sizes_from_bytes n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_hc_sizes_from_bytes (proc_start P_hc_sizes_from_bytes) s1 (proc_end P_hc_sizes_from_bytes) s2 ->
    (s2 V_hc_sizes_from_bytes_z <= max0(s1 V_hc_sizes_from_bytes_num_bytes))%Q.
Proof.
  prove_bound ipa admissible_ipa P_hc_sizes_from_bytes.
Qed.
