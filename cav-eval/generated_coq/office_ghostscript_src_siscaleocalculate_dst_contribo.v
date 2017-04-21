Require Import pasta.Pasta.

Inductive proc: Type :=
  P_calculate_dst_contrib.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_calculate_dst_contrib_z := 1%positive.
Notation V_calculate_dst_contrib__tmp := 2%positive.
Notation V_calculate_dst_contrib_first_index_mod := 3%positive.
Notation V_calculate_dst_contrib_i := 4%positive.
Notation V_calculate_dst_contrib_last_index := 5%positive.
Notation V_calculate_dst_contrib_row_size := 6%positive.
Notation V_calculate_dst_contrib_ss := 7%positive.
Notation V_calculate_dst_contrib_y := 8%positive.
Definition Pedges_calculate_dst_contrib: list (edge proc) :=
  (EA 1 (AAssign V_calculate_dst_contrib_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_calculate_dst_contrib__tmp
  (Some (EVar V_calculate_dst_contrib_y))) 3)::(EA 3 (AAssign
  V_calculate_dst_contrib_row_size None) 4)::(EA 4 (AAssign
  V_calculate_dst_contrib_last_index None) 5)::(EA 5 (AAssign
  V_calculate_dst_contrib_first_index_mod None) 6)::(EA 6 (AAssign
  V_calculate_dst_contrib_last_index None) 7)::(EA 7 AWeaken 8)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_calculate_dst_contrib_last_index)
  s) < (eval (EVar V_calculate_dst_contrib_first_index_mod) s))%Z)) 10)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_calculate_dst_contrib_last_index)
  s) >= (eval (EVar V_calculate_dst_contrib_first_index_mod) s))%Z)) 9)::
  (EA 9 AWeaken 18)::(EA 10 AWeaken 11)::(EA 11 (AAssign
  V_calculate_dst_contrib_i (Some (ENum (0)))) 12)::(EA 12 ANone 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_calculate_dst_contrib_i) s) < (eval (ENum (8))
  s))%Z)) 19)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_calculate_dst_contrib_i) s) >= (eval (ENum (8))
  s))%Z)) 15)::(EA 15 AWeaken 16)::(EA 16 ANone 17)::(EA 17 AWeaken 18)::
  (EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_calculate_dst_contrib_i) s) <=
  (eval (EVar V_calculate_dst_contrib_last_index) s))%Z)) 28)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_calculate_dst_contrib_i) s) >
  (eval (EVar V_calculate_dst_contrib_last_index) s))%Z)) 21)::
  (EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_calculate_dst_contrib_i) s) >=
  (eval (EVar V_calculate_dst_contrib_first_index_mod) s))%Z)) 25)::
  (EA 22 (AGuard (fun s => ((eval (EVar V_calculate_dst_contrib_i) s) <
  (eval (EVar V_calculate_dst_contrib_first_index_mod) s))%Z)) 23)::
  (EA 23 AWeaken 24)::(EA 24 ANone 27)::(EA 25 AWeaken 26)::
  (EA 26 ANone 27)::(EA 27 ANone 30)::(EA 28 AWeaken 29)::(EA 29 ANone 30)::
  (EA 30 ANone 31)::(EA 31 (AAssign V_calculate_dst_contrib_i
  (Some (EAdd (EVar V_calculate_dst_contrib_i) (ENum (1))))) 32)::
  (EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_calculate_dst_contrib_z (Some (EAdd (ENum (1))
  (EVar V_calculate_dst_contrib_z)))) 35)::(EA 35 AWeaken 14)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_calculate_dst_contrib => Pedges_calculate_dst_contrib
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_calculate_dst_contrib => 18
     end)%positive;
  var_global := var_global
}.

Definition ai_calculate_dst_contrib (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0)%Z
   | 3 => (-1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_z <= 0)%Z
   | 4 => (1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0)%Z
   | 5 => (-1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_z <= 0)%Z
   | 6 => (1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0)%Z
   | 7 => (-1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_z <= 0)%Z
   | 8 => (1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0)%Z
   | 9 => (-1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_first_index_mod+ -1 * s V_calculate_dst_contrib_last_index <= 0)%Z
   | 10 => (-1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0)%Z
   | 11 => (-1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ 1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0)%Z
   | 12 => (-1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ 1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0)%Z
   | 13 => (-1 * s V_calculate_dst_contrib_i <= 0 /\ 1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ 1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0)%Z
   | 14 => (-1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ 1 * s V_calculate_dst_contrib_i + -8 <= 0)%Z
   | 15 => (1 * s V_calculate_dst_contrib_i + -8 <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_i + 8 <= 0)%Z
   | 16 => (-1 * s V_calculate_dst_contrib_i + 8 <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ 1 * s V_calculate_dst_contrib_i + -8 <= 0)%Z
   | 17 => (1 * s V_calculate_dst_contrib_i + -8 <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_i + 8 <= 0)%Z
   | 18 => (-1 * s V_calculate_dst_contrib_z <= 0)%Z
   | 19 => (-1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_i + -7 <= 0)%Z
   | 20 => (1 * s V_calculate_dst_contrib_i + -7 <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0)%Z
   | 21 => (-1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_i + -7 <= 0 /\ -1 * s V_calculate_dst_contrib_i+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0)%Z
   | 22 => (-1 * s V_calculate_dst_contrib_i+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ 1 * s V_calculate_dst_contrib_i + -7 <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0)%Z
   | 23 => (-1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_i + -7 <= 0 /\ -1 * s V_calculate_dst_contrib_i+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_i + 1 <= 0)%Z
   | 24 => (-1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_i + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_i+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ 1 * s V_calculate_dst_contrib_i + -7 <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0)%Z
   | 25 => (-1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_i + -7 <= 0 /\ -1 * s V_calculate_dst_contrib_i+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ 1 * s V_calculate_dst_contrib_first_index_mod+ -1 * s V_calculate_dst_contrib_i <= 0)%Z
   | 26 => (1 * s V_calculate_dst_contrib_first_index_mod+ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_i+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ 1 * s V_calculate_dst_contrib_i + -7 <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0)%Z
   | 27 => (-1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_i + -7 <= 0 /\ -1 * s V_calculate_dst_contrib_i+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0)%Z
   | 28 => (-1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_i + -7 <= 0 /\ 1 * s V_calculate_dst_contrib_i+ -1 * s V_calculate_dst_contrib_last_index <= 0)%Z
   | 29 => (1 * s V_calculate_dst_contrib_i+ -1 * s V_calculate_dst_contrib_last_index <= 0 /\ 1 * s V_calculate_dst_contrib_i + -7 <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0)%Z
   | 30 => (-1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_i + -7 <= 0)%Z
   | 31 => (1 * s V_calculate_dst_contrib_i + -7 <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_i <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0)%Z
   | 32 => (-1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_i + -8 <= 0 /\ -1 * s V_calculate_dst_contrib_i + 1 <= 0)%Z
   | 33 => (-1 * s V_calculate_dst_contrib_i + 1 <= 0 /\ 1 * s V_calculate_dst_contrib_i + -8 <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0)%Z
   | 34 => (-1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_z <= 0 /\ 1 * s V_calculate_dst_contrib_i + -8 <= 0 /\ -1 * s V_calculate_dst_contrib_i + 1 <= 0)%Z
   | 35 => (-1 * s V_calculate_dst_contrib_i + 1 <= 0 /\ 1 * s V_calculate_dst_contrib_i + -8 <= 0 /\ -1 * s V_calculate_dst_contrib_first_index_mod+ 1 * s V_calculate_dst_contrib_last_index + 1 <= 0 /\ -1 * s V_calculate_dst_contrib_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_calculate_dst_contrib (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((8 # 1) <= z)%Q
   | 2 => ((8 # 1) + s V_calculate_dst_contrib_z <= z)%Q
   | 3 => ((8 # 1) + s V_calculate_dst_contrib_z <= z)%Q
   | 4 => ((8 # 1) + s V_calculate_dst_contrib_z <= z)%Q
   | 5 => ((8 # 1) + s V_calculate_dst_contrib_z <= z)%Q
   | 6 => ((8 # 1) + s V_calculate_dst_contrib_z <= z)%Q
   | 7 => ((8 # 1) + s V_calculate_dst_contrib_z <= z)%Q
   | 8 => ((8 # 1) + s V_calculate_dst_contrib_z <= z)%Q
   | 9 => hints
     [(*-8 0*) F_one]
     ((8 # 1) + s V_calculate_dst_contrib_z <= z)%Q
   | 10 => ((8 # 1) + s V_calculate_dst_contrib_z <= z)%Q
   | 11 => ((8 # 1) + s V_calculate_dst_contrib_z <= z)%Q
   | 12 => (s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 13 => (s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 14 => (s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 15 => (s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 16 => (s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (8 - s V_calculate_dst_contrib_i) (7
                                                                    - s V_calculate_dst_contrib_i));
      (*-1 0*) F_max0_ge_0 (7 - s V_calculate_dst_contrib_i)]
     (s V_calculate_dst_contrib_z + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 18 => (s V_calculate_dst_contrib_z <= z)%Q
   | 19 => (s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 20 => (s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 21 => (s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 22 => (s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (8 - s V_calculate_dst_contrib_i) (1)]
     (s V_calculate_dst_contrib_z + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 24 => ((1 # 1) + s V_calculate_dst_contrib_z
            + max0(7 - s V_calculate_dst_contrib_i) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (8 - s V_calculate_dst_contrib_i) (1)]
     (s V_calculate_dst_contrib_z + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 26 => ((1 # 1) + s V_calculate_dst_contrib_z
            + max0(7 - s V_calculate_dst_contrib_i) <= z)%Q
   | 27 => ((1 # 1) + s V_calculate_dst_contrib_z
            + max0(7 - s V_calculate_dst_contrib_i) <= z)%Q
   | 28 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (8 - s V_calculate_dst_contrib_i) (1)]
     (s V_calculate_dst_contrib_z + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 29 => ((1 # 1) + s V_calculate_dst_contrib_z
            + max0(7 - s V_calculate_dst_contrib_i) <= z)%Q
   | 30 => ((1 # 1) + s V_calculate_dst_contrib_z
            + max0(7 - s V_calculate_dst_contrib_i) <= z)%Q
   | 31 => ((1 # 1) + s V_calculate_dst_contrib_z
            + max0(7 - s V_calculate_dst_contrib_i) <= z)%Q
   | 32 => ((1 # 1) + s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 33 => ((1 # 1) + s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 34 => ((1 # 1) + s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | 35 => (s V_calculate_dst_contrib_z
            + max0(8 - s V_calculate_dst_contrib_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_calculate_dst_contrib =>
    [mkPA Q (fun n z s => ai_calculate_dst_contrib n s /\ annot0_calculate_dst_contrib n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_calculate_dst_contrib (proc_start P_calculate_dst_contrib) s1 (proc_end P_calculate_dst_contrib) s2 ->
    (s2 V_calculate_dst_contrib_z <= (8 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_calculate_dst_contrib.
Qed.
