Require Import pasta.Pasta.

Inductive proc: Type :=
  P_emit_dqt.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_emit_dqt_z := 1%positive.
Notation V_emit_dqt__tmp := 2%positive.
Notation V_emit_dqt_i := 3%positive.
Notation V_emit_dqt_prec := 4%positive.
Notation V_emit_dqt_qval := 5%positive.
Notation V_emit_dqt_cinfo := 6%positive.
Notation V_emit_dqt_index := 7%positive.
Definition Pedges_emit_dqt: list (edge proc) :=
  (EA 1 (AAssign V_emit_dqt_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_emit_dqt__tmp (Some (EVar V_emit_dqt_index))) 3)::(EA 3 AWeaken 4)::
  (EA 4 ANone 5)::(EA 4 ANone 6)::(EA 5 ANone 6)::(EA 6 (AAssign
  V_emit_dqt_prec (Some (ENum (0)))) 7)::(EA 7 (AAssign V_emit_dqt_i
  (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_emit_dqt_i) s) < (eval (ENum (64)) s))%Z)) 35)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_emit_dqt_i) s) >= (eval (ENum (64))
  s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 ANone 33)::(EA 12 ANone 13)::
  (EA 13 (AAssign V_emit_dqt_i (Some (ENum (0)))) 14)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::(EA 16 (AGuard (fun s => ((eval (EVar V_emit_dqt_i)
  s) < (eval (ENum (64)) s))%Z)) 20)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_emit_dqt_i) s) >= (eval (ENum (64)) s))%Z)) 17)::
  (EA 17 AWeaken 18)::(EA 18 ANone 19)::(EA 19 AWeaken 34)::
  (EA 20 AWeaken 21)::(EA 21 (AAssign V_emit_dqt_qval None) 22)::
  (EA 22 AWeaken 23)::(EA 23 (AGuard (fun s => ((eval (EVar V_emit_dqt_prec)
  s) <> (eval (ENum (0)) s))%Z)) 25)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_emit_dqt_prec) s) = (eval (ENum (0)) s))%Z)) 24)::
  (EA 24 AWeaken 27)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::
  (EA 27 ANone 28)::(EA 28 (AAssign V_emit_dqt_i
  (Some (EAdd (EVar V_emit_dqt_i) (ENum (1))))) 29)::(EA 29 ANone 30)::
  (EA 30 ANone 31)::(EA 31 (AAssign V_emit_dqt_z (Some (EAdd (ENum (1))
  (EVar V_emit_dqt_z)))) 32)::(EA 32 AWeaken 16)::(EA 33 AWeaken 34)::
  (EA 35 AWeaken 36)::(EA 36 ANone 37)::(EA 36 ANone 39)::(EA 37 (AAssign
  V_emit_dqt_prec (Some (ENum (1)))) 38)::(EA 38 ANone 39)::
  (EA 39 ANone 40)::(EA 40 (AAssign V_emit_dqt_i
  (Some (EAdd (EVar V_emit_dqt_i) (ENum (1))))) 41)::(EA 41 ANone 42)::
  (EA 42 ANone 43)::(EA 43 (AAssign V_emit_dqt_z (Some (EAdd (ENum (1))
  (EVar V_emit_dqt_z)))) 44)::(EA 44 AWeaken 10)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_emit_dqt => Pedges_emit_dqt
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_emit_dqt => 34
     end)%positive;
  var_global := var_global
}.

Definition ai_emit_dqt (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_z <= 0)%Z
   | 3 => (-1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_z <= 0)%Z
   | 4 => (1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_z <= 0)%Z
   | 5 => (-1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_z <= 0)%Z
   | 6 => (1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_z <= 0)%Z
   | 7 => (-1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_prec <= 0)%Z
   | 8 => (-1 * s V_emit_dqt_prec <= 0 /\ 1 * s V_emit_dqt_prec <= 0 /\ 1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_i <= 0)%Z
   | 9 => (-1 * s V_emit_dqt_i <= 0 /\ 1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_prec <= 0)%Z
   | 10 => (-1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0)%Z
   | 11 => (1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i + 64 <= 0)%Z
   | 12 => (-1 * s V_emit_dqt_i + 64 <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0)%Z
   | 13 => (1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i + 64 <= 0)%Z
   | 14 => (-1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ 1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_i <= 0)%Z
   | 15 => (-1 * s V_emit_dqt_i <= 0 /\ 1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_z <= 0)%Z
   | 16 => (-1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0)%Z
   | 17 => (1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i + 64 <= 0)%Z
   | 18 => (-1 * s V_emit_dqt_i + 64 <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0)%Z
   | 19 => (1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i + 64 <= 0)%Z
   | 20 => (-1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i + -63 <= 0)%Z
   | 21 => (1 * s V_emit_dqt_i + -63 <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_prec <= 0)%Z
   | 22 => (-1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i + -63 <= 0)%Z
   | 23 => (1 * s V_emit_dqt_i + -63 <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_prec <= 0)%Z
   | 24 => (-1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i + -63 <= 0 /\ 1 * s V_emit_dqt_prec <= 0)%Z
   | 25 => (-1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i + -63 <= 0 /\ -1 * s V_emit_dqt_prec + 1 <= 0)%Z
   | 26 => (-1 * s V_emit_dqt_prec + 1 <= 0 /\ 1 * s V_emit_dqt_i + -63 <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i <= 0)%Z
   | 27 => (-1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i + -63 <= 0)%Z
   | 28 => (1 * s V_emit_dqt_i + -63 <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_prec <= 0)%Z
   | 29 => (-1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_i + 1 <= 0)%Z
   | 30 => (-1 * s V_emit_dqt_i + 1 <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_prec <= 0)%Z
   | 31 => (-1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_i + 1 <= 0)%Z
   | 32 => (-1 * s V_emit_dqt_i + 1 <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_z + 1 <= 0)%Z
   | 33 => (1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i + 64 <= 0)%Z
   | 34 => (-1 * s V_emit_dqt_i + 64 <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0)%Z
   | 35 => (-1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i + -63 <= 0)%Z
   | 36 => (1 * s V_emit_dqt_i + -63 <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_prec <= 0)%Z
   | 37 => (-1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i + -63 <= 0)%Z
   | 38 => (1 * s V_emit_dqt_i + -63 <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ 1 * s V_emit_dqt_prec + -1 <= 0 /\ -1 * s V_emit_dqt_prec + 1 <= 0)%Z
   | 39 => (-1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i + -63 <= 0)%Z
   | 40 => (1 * s V_emit_dqt_i + -63 <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_i <= 0 /\ -1 * s V_emit_dqt_prec <= 0)%Z
   | 41 => (-1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_i + 1 <= 0)%Z
   | 42 => (-1 * s V_emit_dqt_i + 1 <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ -1 * s V_emit_dqt_prec <= 0)%Z
   | 43 => (-1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_z <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_i + 1 <= 0)%Z
   | 44 => (-1 * s V_emit_dqt_i + 1 <= 0 /\ 1 * s V_emit_dqt_i + -64 <= 0 /\ -1 * s V_emit_dqt_prec <= 0 /\ -1 * s V_emit_dqt_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_emit_dqt (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((128 # 1) <= z)%Q
   | 2 => ((128 # 1) + s V_emit_dqt_z <= z)%Q
   | 3 => ((128 # 1) + s V_emit_dqt_z <= z)%Q
   | 4 => ((128 # 1) + s V_emit_dqt_z <= z)%Q
   | 5 => ((128 # 1) + s V_emit_dqt_z <= z)%Q
   | 6 => ((128 # 1) + s V_emit_dqt_z <= z)%Q
   | 7 => ((128 # 1) + s V_emit_dqt_z <= z)%Q
   | 8 => ((128 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 9 => ((128 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 10 => ((128 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (64 - s V_emit_dqt_i) (63
                                                                   - 
                                                                   s V_emit_dqt_i));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                              - s V_emit_dqt_i) (0))) (F_max0_ge_0 (64
                                                                    - s V_emit_dqt_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (63 - s V_emit_dqt_i)) (F_check_ge (0) (0))]
     ((128 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 12 => ((64 # 1) + s V_emit_dqt_z <= z)%Q
   | 13 => ((64 # 1) + s V_emit_dqt_z <= z)%Q
   | 14 => (s V_emit_dqt_z + max0(64 - s V_emit_dqt_i) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_emit_dqt_z) (0))) (F_max0_ge_0 (s V_emit_dqt_z))]
     (s V_emit_dqt_z + max0(64 - s V_emit_dqt_i) <= z)%Q
   | 16 => (max0(64 - s V_emit_dqt_i) + max0(s V_emit_dqt_z) <= z)%Q
   | 17 => (max0(64 - s V_emit_dqt_i) + max0(s V_emit_dqt_z) <= z)%Q
   | 18 => (max0(64 - s V_emit_dqt_i) + max0(s V_emit_dqt_z) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (64 - s V_emit_dqt_i) (63
                                                                   - 
                                                                   s V_emit_dqt_i));
      (*-1 0*) F_max0_ge_0 (63 - s V_emit_dqt_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_emit_dqt_z)) (F_check_ge (s V_emit_dqt_z) (0))]
     (max0(64 - s V_emit_dqt_i) + max0(s V_emit_dqt_z) <= z)%Q
   | 20 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_emit_dqt_z)) (F_check_ge (s V_emit_dqt_z) (0))]
     (max0(64 - s V_emit_dqt_i) + max0(s V_emit_dqt_z) <= z)%Q
   | 21 => (s V_emit_dqt_z + max0(64 - s V_emit_dqt_i) <= z)%Q
   | 22 => (s V_emit_dqt_z + max0(64 - s V_emit_dqt_i) <= z)%Q
   | 23 => (s V_emit_dqt_z + max0(64 - s V_emit_dqt_i) <= z)%Q
   | 24 => hints
     [(*0 1*) F_max0_pre_decrement 1 (64 - s V_emit_dqt_i) (1)]
     (s V_emit_dqt_z + max0(64 - s V_emit_dqt_i) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (64 - s V_emit_dqt_i) (1)]
     (s V_emit_dqt_z + max0(64 - s V_emit_dqt_i) <= z)%Q
   | 26 => ((1 # 1) + s V_emit_dqt_z + max0(63 - s V_emit_dqt_i) <= z)%Q
   | 27 => ((1 # 1) + s V_emit_dqt_z + max0(63 - s V_emit_dqt_i) <= z)%Q
   | 28 => ((1 # 1) + s V_emit_dqt_z + max0(63 - s V_emit_dqt_i) <= z)%Q
   | 29 => ((1 # 1) + s V_emit_dqt_z + max0(64 - s V_emit_dqt_i) <= z)%Q
   | 30 => ((1 # 1) + s V_emit_dqt_z + max0(64 - s V_emit_dqt_i) <= z)%Q
   | 31 => ((1 # 1) + s V_emit_dqt_z + max0(64 - s V_emit_dqt_i) <= z)%Q
   | 32 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_emit_dqt_z) (0))) (F_max0_ge_0 (s V_emit_dqt_z))]
     (s V_emit_dqt_z + max0(64 - s V_emit_dqt_i) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (64 - s V_emit_dqt_i) (63
                                                                   - 
                                                                   s V_emit_dqt_i));
      (*-1 0*) F_max0_ge_0 (63 - s V_emit_dqt_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_emit_dqt_i)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_emit_dqt_i) (0))) (F_max0_ge_0 (s V_emit_dqt_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (64
                                                               - s V_emit_dqt_i) (0))) (F_max0_ge_0 (64
                                                                    - s V_emit_dqt_i))]
     ((64 # 1) + s V_emit_dqt_z <= z)%Q
   | 34 => (s V_emit_dqt_z <= z)%Q
   | 35 => ((128 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 36 => ((128 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 37 => ((128 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 38 => ((128 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 39 => ((128 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 40 => ((128 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 41 => ((129 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 42 => ((129 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 43 => ((129 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | 44 => ((128 # 1) - s V_emit_dqt_i + s V_emit_dqt_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_emit_dqt =>
    [mkPA Q (fun n z s => ai_emit_dqt n s /\ annot0_emit_dqt n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_emit_dqt (proc_start P_emit_dqt) s1 (proc_end P_emit_dqt) s2 ->
    (s2 V_emit_dqt_z <= (128 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_emit_dqt.
Qed.
