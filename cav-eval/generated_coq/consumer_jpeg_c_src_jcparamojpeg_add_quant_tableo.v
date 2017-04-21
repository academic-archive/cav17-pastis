Require Import pasta.Pasta.

Inductive proc: Type :=
  P_jpeg_add_quant_table.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_jpeg_add_quant_table_z := 1%positive.
Notation V_jpeg_add_quant_table__tmp := 2%positive.
Notation V_jpeg_add_quant_table__tmp1 := 3%positive.
Notation V_jpeg_add_quant_table__tmp2 := 4%positive.
Notation V_jpeg_add_quant_table_i := 5%positive.
Notation V_jpeg_add_quant_table_temp := 6%positive.
Notation V_jpeg_add_quant_table_basic_table := 7%positive.
Notation V_jpeg_add_quant_table_cinfo := 8%positive.
Notation V_jpeg_add_quant_table_force_baseline := 9%positive.
Notation V_jpeg_add_quant_table_scale_factor := 10%positive.
Notation V_jpeg_add_quant_table_which_tbl := 11%positive.
Definition Pedges_jpeg_add_quant_table: list (edge proc) :=
  (EA 1 (AAssign V_jpeg_add_quant_table_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_jpeg_add_quant_table__tmp2
  (Some (EVar V_jpeg_add_quant_table_which_tbl))) 3)::(EA 3 (AAssign
  V_jpeg_add_quant_table__tmp1
  (Some (EVar V_jpeg_add_quant_table_scale_factor))) 4)::(EA 4 (AAssign
  V_jpeg_add_quant_table__tmp
  (Some (EVar V_jpeg_add_quant_table_force_baseline))) 5)::(EA 5 AWeaken 6)::
  (EA 6 ANone 8)::(EA 6 ANone 7)::(EA 7 AWeaken 10)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 ANone 11)::(EA 10 ANone 12)::(EA 11 ANone 12)::
  (EA 12 (AAssign V_jpeg_add_quant_table_i (Some (ENum (0)))) 13)::
  (EA 13 ANone 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_jpeg_add_quant_table_i) s) < (eval (ENum (64))
  s))%Z)) 18)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_jpeg_add_quant_table_i) s) >= (eval (ENum (64))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 18 AWeaken 19)::(EA 19 (AAssign
  V_jpeg_add_quant_table_temp None) 20)::(EA 20 AWeaken 21)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_jpeg_add_quant_table_temp) s) <= (eval (ENum (0))
  s))%Z)) 23)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_jpeg_add_quant_table_temp) s) > (eval (ENum (0))
  s))%Z)) 22)::(EA 22 AWeaken 27)::(EA 23 AWeaken 24)::(EA 24 (AAssign
  V_jpeg_add_quant_table_temp (Some (ENum (1)))) 25)::(EA 25 ANone 26)::
  (EA 26 AWeaken 27)::(EA 27 ANone 29)::(EA 27 ANone 28)::
  (EA 28 AWeaken 32)::(EA 29 (AAssign V_jpeg_add_quant_table_temp None) 30)::
  (EA 30 ANone 31)::(EA 31 AWeaken 32)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_jpeg_add_quant_table__tmp) s) <> (eval (ENum (0))
  s))%Z)) 34)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_jpeg_add_quant_table__tmp) s) = (eval (ENum (0))
  s))%Z)) 33)::(EA 33 AWeaken 40)::(EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_jpeg_add_quant_table_temp) s) > (eval (ENum (255))
  s))%Z)) 37)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_jpeg_add_quant_table_temp) s) <=
  (eval (ENum (255)) s))%Z)) 36)::(EA 36 AWeaken 40)::(EA 37 AWeaken 38)::
  (EA 38 (AAssign V_jpeg_add_quant_table_temp (Some (ENum (255)))) 39)::
  (EA 39 ANone 40)::(EA 40 ANone 41)::(EA 41 (AAssign
  V_jpeg_add_quant_table_i (Some (EAdd (EVar V_jpeg_add_quant_table_i)
  (ENum (1))))) 42)::(EA 42 ANone 43)::(EA 43 ANone 44)::(EA 44 (AAssign
  V_jpeg_add_quant_table_z (Some (EAdd (ENum (1))
  (EVar V_jpeg_add_quant_table_z)))) 45)::(EA 45 AWeaken 15)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_jpeg_add_quant_table => Pedges_jpeg_add_quant_table
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_jpeg_add_quant_table => 17
     end)%positive;
  var_global := var_global
}.

Definition ai_jpeg_add_quant_table (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 3 => (-1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 4 => (1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 5 => (-1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 6 => (1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 7 => (-1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 8 => (-1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 9 => (1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 10 => (-1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 11 => (1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 12 => (-1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 13 => (1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0)%Z
   | 14 => (-1 * s V_jpeg_add_quant_table_i <= 0 /\ 1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 15 => (-1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -64 <= 0)%Z
   | 16 => (1 * s V_jpeg_add_quant_table_i + -64 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i + 64 <= 0)%Z
   | 17 => (-1 * s V_jpeg_add_quant_table_i + 64 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -64 <= 0)%Z
   | 18 => (-1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0)%Z
   | 19 => (1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0)%Z
   | 20 => (-1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0)%Z
   | 21 => (1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0)%Z
   | 22 => (-1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_temp + 1 <= 0)%Z
   | 23 => (-1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ 1 * s V_jpeg_add_quant_table_temp <= 0)%Z
   | 24 => (1 * s V_jpeg_add_quant_table_temp <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0)%Z
   | 25 => (-1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ 1 * s V_jpeg_add_quant_table_temp + -1 <= 0 /\ -1 * s V_jpeg_add_quant_table_temp + 1 <= 0)%Z
   | 26 => (-1 * s V_jpeg_add_quant_table_temp + 1 <= 0 /\ 1 * s V_jpeg_add_quant_table_temp + -1 <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0)%Z
   | 27 => (-1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_temp + 1 <= 0)%Z
   | 28 => (-1 * s V_jpeg_add_quant_table_temp + 1 <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0)%Z
   | 29 => (-1 * s V_jpeg_add_quant_table_temp + 1 <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0)%Z
   | 30 => (-1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0)%Z
   | 31 => (1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0)%Z
   | 32 => (-1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0)%Z
   | 33 => (1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0 /\ 1 * s V_jpeg_add_quant_table__tmp <= 0 /\ -1 * s V_jpeg_add_quant_table__tmp <= 0)%Z
   | 34 => (1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0)%Z
   | 35 => (-1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0)%Z
   | 36 => (1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0 /\ 1 * s V_jpeg_add_quant_table_temp + -255 <= 0)%Z
   | 37 => (1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_temp + 256 <= 0)%Z
   | 38 => (-1 * s V_jpeg_add_quant_table_temp + 256 <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0)%Z
   | 39 => (1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0 /\ 1 * s V_jpeg_add_quant_table_temp + -255 <= 0 /\ -1 * s V_jpeg_add_quant_table_temp + 255 <= 0)%Z
   | 40 => (-1 * s V_jpeg_add_quant_table_i <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -63 <= 0)%Z
   | 41 => (1 * s V_jpeg_add_quant_table_i + -63 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0 /\ -1 * s V_jpeg_add_quant_table_i <= 0)%Z
   | 42 => (-1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -64 <= 0 /\ -1 * s V_jpeg_add_quant_table_i + 1 <= 0)%Z
   | 43 => (-1 * s V_jpeg_add_quant_table_i + 1 <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -64 <= 0 /\ -1 * s V_jpeg_add_quant_table_z <= 0)%Z
   | 44 => (-1 * s V_jpeg_add_quant_table_z <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -64 <= 0 /\ -1 * s V_jpeg_add_quant_table_i + 1 <= 0)%Z
   | 45 => (-1 * s V_jpeg_add_quant_table_i + 1 <= 0 /\ 1 * s V_jpeg_add_quant_table_i + -64 <= 0 /\ -1 * s V_jpeg_add_quant_table_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_jpeg_add_quant_table (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((64 # 1) <= z)%Q
   | 2 => ((64 # 1) + s V_jpeg_add_quant_table_z <= z)%Q
   | 3 => ((64 # 1) + s V_jpeg_add_quant_table_z <= z)%Q
   | 4 => ((64 # 1) + s V_jpeg_add_quant_table_z <= z)%Q
   | 5 => ((64 # 1) + s V_jpeg_add_quant_table_z <= z)%Q
   | 6 => ((64 # 1) + s V_jpeg_add_quant_table_z <= z)%Q
   | 7 => ((64 # 1) + s V_jpeg_add_quant_table_z <= z)%Q
   | 8 => ((64 # 1) + s V_jpeg_add_quant_table_z <= z)%Q
   | 9 => ((64 # 1) + s V_jpeg_add_quant_table_z <= z)%Q
   | 10 => ((64 # 1) + s V_jpeg_add_quant_table_z <= z)%Q
   | 11 => ((64 # 1) + s V_jpeg_add_quant_table_z <= z)%Q
   | 12 => ((64 # 1) + s V_jpeg_add_quant_table_z <= z)%Q
   | 13 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 14 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 15 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (64 - s V_jpeg_add_quant_table_i) (63
                                                                    - s V_jpeg_add_quant_table_i));
      (*-1 0*) F_max0_ge_0 (63 - s V_jpeg_add_quant_table_i)]
     (s V_jpeg_add_quant_table_z + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 17 => (s V_jpeg_add_quant_table_z <= z)%Q
   | 18 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 19 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 20 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 21 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 22 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 23 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 24 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 25 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 26 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 27 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 28 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (64 - s V_jpeg_add_quant_table_i) (1)]
     (s V_jpeg_add_quant_table_z + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 29 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 30 => (s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (64 - s V_jpeg_add_quant_table_i) (1)]
     (s V_jpeg_add_quant_table_z + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 32 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(63 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 33 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(63 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 34 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(63 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 35 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(63 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 36 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(63 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 37 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(63 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 38 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(63 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 39 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(63 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 40 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(63 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 41 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(63 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 42 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 43 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 44 => ((1 # 1) + s V_jpeg_add_quant_table_z
            + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | 45 => hints
     [(*-0.015625 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_jpeg_add_quant_table_i)) (F_check_ge (s V_jpeg_add_quant_table_i) (0));
      (*0 0.015625*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_jpeg_add_quant_table_i) (0))) (F_max0_ge_0 (s V_jpeg_add_quant_table_i))]
     (s V_jpeg_add_quant_table_z + max0(64 - s V_jpeg_add_quant_table_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_jpeg_add_quant_table =>
    [mkPA Q (fun n z s => ai_jpeg_add_quant_table n s /\ annot0_jpeg_add_quant_table n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_jpeg_add_quant_table (proc_start P_jpeg_add_quant_table) s1 (proc_end P_jpeg_add_quant_table) s2 ->
    (s2 V_jpeg_add_quant_table_z <= (64 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_jpeg_add_quant_table.
Qed.
