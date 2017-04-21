Require Import pasta.Pasta.

Inductive proc: Type :=
  P_prepare_range_limit_table.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_prepare_range_limit_table_z := 1%positive.
Notation V_prepare_range_limit_table_i := 2%positive.
Notation V_prepare_range_limit_table_cinfo := 3%positive.
Definition Pedges_prepare_range_limit_table: list (edge proc) :=
  (EA 1 (AAssign V_prepare_range_limit_table_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_prepare_range_limit_table_i (Some (ENum (0)))) 3)::
  (EA 3 ANone 4)::(EA 4 AWeaken 5)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_prepare_range_limit_table_i) s) <=
  (eval (ENum (255)) s))%Z)) 20)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_prepare_range_limit_table_i) s) >
  (eval (ENum (255)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign
  V_prepare_range_limit_table_i (Some (ENum (128)))) 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_prepare_range_limit_table_i) s) <
  (eval (ENum (512)) s))%Z)) 13)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_prepare_range_limit_table_i) s) >=
  (eval (ENum (512)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 13 AWeaken 14)::
  (EA 14 ANone 15)::(EA 15 (AAssign V_prepare_range_limit_table_i
  (Some (EAdd (EVar V_prepare_range_limit_table_i) (ENum (1))))) 16)::
  (EA 16 ANone 17)::(EA 17 ANone 18)::(EA 18 (AAssign
  V_prepare_range_limit_table_z (Some (EAdd (ENum (1))
  (EVar V_prepare_range_limit_table_z)))) 19)::(EA 19 AWeaken 10)::
  (EA 20 AWeaken 21)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_prepare_range_limit_table_i
  (Some (EAdd (EVar V_prepare_range_limit_table_i) (ENum (1))))) 23)::
  (EA 23 ANone 24)::(EA 24 ANone 25)::(EA 25 (AAssign
  V_prepare_range_limit_table_z (Some (EAdd (ENum (1))
  (EVar V_prepare_range_limit_table_z)))) 26)::(EA 26 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_prepare_range_limit_table => Pedges_prepare_range_limit_table
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_prepare_range_limit_table => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_prepare_range_limit_table (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_prepare_range_limit_table_z <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0)%Z
   | 3 => (-1 * s V_prepare_range_limit_table_z <= 0 /\ 1 * s V_prepare_range_limit_table_z <= 0 /\ 1 * s V_prepare_range_limit_table_i <= 0 /\ -1 * s V_prepare_range_limit_table_i <= 0)%Z
   | 4 => (-1 * s V_prepare_range_limit_table_i <= 0 /\ 1 * s V_prepare_range_limit_table_i <= 0 /\ 1 * s V_prepare_range_limit_table_z <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0)%Z
   | 5 => (-1 * s V_prepare_range_limit_table_z <= 0 /\ -1 * s V_prepare_range_limit_table_i <= 0 /\ 1 * s V_prepare_range_limit_table_i + -256 <= 0)%Z
   | 6 => (1 * s V_prepare_range_limit_table_i + -256 <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0 /\ -1 * s V_prepare_range_limit_table_i + 256 <= 0)%Z
   | 7 => (-1 * s V_prepare_range_limit_table_i + 256 <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0 /\ 1 * s V_prepare_range_limit_table_i + -256 <= 0)%Z
   | 8 => (-1 * s V_prepare_range_limit_table_z <= 0 /\ 1 * s V_prepare_range_limit_table_i + -128 <= 0 /\ -1 * s V_prepare_range_limit_table_i + 128 <= 0)%Z
   | 9 => (-1 * s V_prepare_range_limit_table_i + 128 <= 0 /\ 1 * s V_prepare_range_limit_table_i + -128 <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0)%Z
   | 10 => (-1 * s V_prepare_range_limit_table_z <= 0 /\ -1 * s V_prepare_range_limit_table_i + 128 <= 0 /\ 1 * s V_prepare_range_limit_table_i + -512 <= 0)%Z
   | 11 => (1 * s V_prepare_range_limit_table_i + -512 <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0 /\ -1 * s V_prepare_range_limit_table_i + 512 <= 0)%Z
   | 12 => (-1 * s V_prepare_range_limit_table_i + 512 <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0 /\ 1 * s V_prepare_range_limit_table_i + -512 <= 0)%Z
   | 13 => (-1 * s V_prepare_range_limit_table_i + 128 <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0 /\ 1 * s V_prepare_range_limit_table_i + -511 <= 0)%Z
   | 14 => (1 * s V_prepare_range_limit_table_i + -511 <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0 /\ -1 * s V_prepare_range_limit_table_i + 128 <= 0)%Z
   | 15 => (-1 * s V_prepare_range_limit_table_i + 128 <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0 /\ 1 * s V_prepare_range_limit_table_i + -511 <= 0)%Z
   | 16 => (-1 * s V_prepare_range_limit_table_z <= 0 /\ -1 * s V_prepare_range_limit_table_i + 129 <= 0 /\ 1 * s V_prepare_range_limit_table_i + -512 <= 0)%Z
   | 17 => (1 * s V_prepare_range_limit_table_i + -512 <= 0 /\ -1 * s V_prepare_range_limit_table_i + 129 <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0)%Z
   | 18 => (-1 * s V_prepare_range_limit_table_z <= 0 /\ -1 * s V_prepare_range_limit_table_i + 129 <= 0 /\ 1 * s V_prepare_range_limit_table_i + -512 <= 0)%Z
   | 19 => (1 * s V_prepare_range_limit_table_i + -512 <= 0 /\ -1 * s V_prepare_range_limit_table_i + 129 <= 0 /\ -1 * s V_prepare_range_limit_table_z + 1 <= 0)%Z
   | 20 => (-1 * s V_prepare_range_limit_table_i <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0 /\ 1 * s V_prepare_range_limit_table_i + -255 <= 0)%Z
   | 21 => (1 * s V_prepare_range_limit_table_i + -255 <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0 /\ -1 * s V_prepare_range_limit_table_i <= 0)%Z
   | 22 => (-1 * s V_prepare_range_limit_table_i <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0 /\ 1 * s V_prepare_range_limit_table_i + -255 <= 0)%Z
   | 23 => (-1 * s V_prepare_range_limit_table_z <= 0 /\ -1 * s V_prepare_range_limit_table_i + 1 <= 0 /\ 1 * s V_prepare_range_limit_table_i + -256 <= 0)%Z
   | 24 => (1 * s V_prepare_range_limit_table_i + -256 <= 0 /\ -1 * s V_prepare_range_limit_table_i + 1 <= 0 /\ -1 * s V_prepare_range_limit_table_z <= 0)%Z
   | 25 => (-1 * s V_prepare_range_limit_table_z <= 0 /\ -1 * s V_prepare_range_limit_table_i + 1 <= 0 /\ 1 * s V_prepare_range_limit_table_i + -256 <= 0)%Z
   | 26 => (1 * s V_prepare_range_limit_table_i + -256 <= 0 /\ -1 * s V_prepare_range_limit_table_i + 1 <= 0 /\ -1 * s V_prepare_range_limit_table_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_prepare_range_limit_table (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((640 # 1) <= z)%Q
   | 2 => ((640 # 1) + s V_prepare_range_limit_table_z <= z)%Q
   | 3 => ((384 # 1) + s V_prepare_range_limit_table_z
           + max0(256 - s V_prepare_range_limit_table_i) <= z)%Q
   | 4 => ((384 # 1) + s V_prepare_range_limit_table_z
           + max0(256 - s V_prepare_range_limit_table_i) <= z)%Q
   | 5 => ((384 # 1) + s V_prepare_range_limit_table_z
           + max0(256 - s V_prepare_range_limit_table_i) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (256
                                             - s V_prepare_range_limit_table_i) (255
                                                                    - s V_prepare_range_limit_table_i));
      (*-1 0*) F_max0_ge_0 (255 - s V_prepare_range_limit_table_i)]
     ((384 # 1) + s V_prepare_range_limit_table_z
      + max0(256 - s V_prepare_range_limit_table_i) <= z)%Q
   | 7 => ((384 # 1) + s V_prepare_range_limit_table_z <= z)%Q
   | 8 => (s V_prepare_range_limit_table_z
           + max0(512 - s V_prepare_range_limit_table_i) <= z)%Q
   | 9 => (s V_prepare_range_limit_table_z
           + max0(512 - s V_prepare_range_limit_table_i) <= z)%Q
   | 10 => (s V_prepare_range_limit_table_z
            + max0(512 - s V_prepare_range_limit_table_i) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (512
                                             - s V_prepare_range_limit_table_i) (511
                                                                    - s V_prepare_range_limit_table_i));
      (*-1 0*) F_max0_ge_0 (511 - s V_prepare_range_limit_table_i)]
     (s V_prepare_range_limit_table_z
      + max0(512 - s V_prepare_range_limit_table_i) <= z)%Q
   | 12 => (s V_prepare_range_limit_table_z <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (512 - s V_prepare_range_limit_table_i) (1)]
     (s V_prepare_range_limit_table_z
      + max0(512 - s V_prepare_range_limit_table_i) <= z)%Q
   | 14 => ((1 # 1) + s V_prepare_range_limit_table_z
            + max0(511 - s V_prepare_range_limit_table_i) <= z)%Q
   | 15 => ((1 # 1) + s V_prepare_range_limit_table_z
            + max0(511 - s V_prepare_range_limit_table_i) <= z)%Q
   | 16 => ((1 # 1) + s V_prepare_range_limit_table_z
            + max0(512 - s V_prepare_range_limit_table_i) <= z)%Q
   | 17 => ((1 # 1) + s V_prepare_range_limit_table_z
            + max0(512 - s V_prepare_range_limit_table_i) <= z)%Q
   | 18 => ((1 # 1) + s V_prepare_range_limit_table_z
            + max0(512 - s V_prepare_range_limit_table_i) <= z)%Q
   | 19 => (s V_prepare_range_limit_table_z
            + max0(512 - s V_prepare_range_limit_table_i) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (256 - s V_prepare_range_limit_table_i) (1)]
     ((384 # 1) + s V_prepare_range_limit_table_z
      + max0(256 - s V_prepare_range_limit_table_i) <= z)%Q
   | 21 => ((385 # 1) + s V_prepare_range_limit_table_z
            + max0(255 - s V_prepare_range_limit_table_i) <= z)%Q
   | 22 => ((385 # 1) + s V_prepare_range_limit_table_z
            + max0(255 - s V_prepare_range_limit_table_i) <= z)%Q
   | 23 => ((385 # 1) + s V_prepare_range_limit_table_z
            + max0(256 - s V_prepare_range_limit_table_i) <= z)%Q
   | 24 => ((385 # 1) + s V_prepare_range_limit_table_z
            + max0(256 - s V_prepare_range_limit_table_i) <= z)%Q
   | 25 => ((385 # 1) + s V_prepare_range_limit_table_z
            + max0(256 - s V_prepare_range_limit_table_i) <= z)%Q
   | 26 => ((384 # 1) + s V_prepare_range_limit_table_z
            + max0(256 - s V_prepare_range_limit_table_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_prepare_range_limit_table =>
    [mkPA Q (fun n z s => ai_prepare_range_limit_table n s /\ annot0_prepare_range_limit_table n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_prepare_range_limit_table (proc_start P_prepare_range_limit_table) s1 (proc_end P_prepare_range_limit_table) s2 ->
    (s2 V_prepare_range_limit_table_z <= (640 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_prepare_range_limit_table.
Qed.
