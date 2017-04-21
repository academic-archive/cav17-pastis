Require Import pasta.Pasta.

Inductive proc: Type :=
  P_pclxl_setdash.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_pclxl_setdash_z := 1%positive.
Notation V_pclxl_setdash__tmp := 2%positive.
Notation V_pclxl_setdash__tmp1 := 3%positive.
Notation V_pclxl_setdash_i := 4%positive.
Notation V_pclxl_setdash_count := 5%positive.
Notation V_pclxl_setdash_offset := 6%positive.
Notation V_pclxl_setdash_pattern := 7%positive.
Notation V_pclxl_setdash_vdev := 8%positive.
Definition Pedges_pclxl_setdash: list (edge proc) :=
  (EA 1 (AAssign V_pclxl_setdash_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_pclxl_setdash_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_pclxl_setdash__tmp)
  s) >= (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_pclxl_setdash__tmp (Some (EVar V_pclxl_setdash_count))) 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_pclxl_setdash__tmp) s) = (eval (ENum (0))
  s))%Z)) 31)::(EA 7 (AGuard (fun s => ((eval (EVar V_pclxl_setdash__tmp)
  s) <> (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_pclxl_setdash__tmp) s) > (eval (ENum (255))
  s))%Z)) 27)::(EA 9 (AGuard (fun s => ((eval (EVar V_pclxl_setdash__tmp)
  s) <= (eval (ENum (255)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 (AAssign
  V_pclxl_setdash_i (Some (ENum (0)))) 12)::(EA 12 ANone 13)::
  (EA 13 AWeaken 14)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_pclxl_setdash_i) s) <
  (eval (EVar V_pclxl_setdash__tmp) s))%Z)) 20)::(EA 14 (AGuard
  (fun s => ((eval (EVar V_pclxl_setdash_i) s) >=
  (eval (EVar V_pclxl_setdash__tmp) s))%Z)) 15)::(EA 15 AWeaken 16)::
  (EA 16 ANone 17)::(EA 16 ANone 18)::(EA 17 ANone 18)::(EA 18 ANone 19)::
  (EA 19 ANone 33)::(EA 20 AWeaken 21)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_pclxl_setdash_i (Some (EAdd (EVar V_pclxl_setdash_i) (ENum (1))))) 23)::
  (EA 23 ANone 24)::(EA 24 ANone 25)::(EA 25 (AAssign V_pclxl_setdash_z
  (Some (EAdd (ENum (1)) (EVar V_pclxl_setdash_z)))) 26)::
  (EA 26 AWeaken 14)::(EA 27 AWeaken 28)::(EA 28 (AAssign
  V_pclxl_setdash__tmp1 (Some (ENum (-13)))) 29)::(EA 29 ANone 30)::
  (EA 30 AWeaken 36)::(EA 31 AWeaken 32)::(EA 32 ANone 33)::(EA 33 (AAssign
  V_pclxl_setdash__tmp1 (Some (ENum (0)))) 34)::(EA 34 ANone 35)::
  (EA 35 AWeaken 36)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_pclxl_setdash => Pedges_pclxl_setdash
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_pclxl_setdash => 36
     end)%positive;
  var_global := var_global
}.

Definition ai_pclxl_setdash (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_z <= 0)%Z
   | 3 => (-1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0)%Z
   | 4 => (-1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash__tmp <= 0)%Z
   | 5 => (-1 * s V_pclxl_setdash__tmp <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0)%Z
   | 6 => (-1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_z <= 0)%Z
   | 7 => (-1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0)%Z
   | 8 => (-1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_z <= 0)%Z
   | 9 => (-1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0)%Z
   | 10 => (-1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0)%Z
   | 11 => (1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0)%Z
   | 12 => (1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ 1 * s V_pclxl_setdash_i <= 0 /\ -1 * s V_pclxl_setdash_i <= 0)%Z
   | 13 => (-1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash_z <= 0)%Z
   | 14 => (-1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0)%Z
   | 15 => (1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash__tmp+ -1 * s V_pclxl_setdash_i <= 0)%Z
   | 16 => (1 * s V_pclxl_setdash__tmp+ -1 * s V_pclxl_setdash_i <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0)%Z
   | 17 => (1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash__tmp+ -1 * s V_pclxl_setdash_i <= 0)%Z
   | 18 => (1 * s V_pclxl_setdash__tmp+ -1 * s V_pclxl_setdash_i <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0)%Z
   | 19 => (1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash__tmp+ -1 * s V_pclxl_setdash_i <= 0)%Z
   | 20 => (1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash__tmp+ 1 * s V_pclxl_setdash_i + 1 <= 0)%Z
   | 21 => (-1 * s V_pclxl_setdash__tmp+ 1 * s V_pclxl_setdash_i + 1 <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0)%Z
   | 22 => (1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash__tmp+ 1 * s V_pclxl_setdash_i + 1 <= 0)%Z
   | 23 => (-1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_i + 1 <= 0 /\ -1 * s V_pclxl_setdash__tmp+ 1 * s V_pclxl_setdash_i <= 0)%Z
   | 24 => (-1 * s V_pclxl_setdash__tmp+ 1 * s V_pclxl_setdash_i <= 0 /\ -1 * s V_pclxl_setdash_i + 1 <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_z <= 0)%Z
   | 25 => (-1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_i + 1 <= 0 /\ -1 * s V_pclxl_setdash__tmp+ 1 * s V_pclxl_setdash_i <= 0)%Z
   | 26 => (-1 * s V_pclxl_setdash__tmp+ 1 * s V_pclxl_setdash_i <= 0 /\ -1 * s V_pclxl_setdash_i + 1 <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_z + 1 <= 0)%Z
   | 27 => (-1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash__tmp + 256 <= 0)%Z
   | 28 => (-1 * s V_pclxl_setdash__tmp + 256 <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0)%Z
   | 29 => (-1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash__tmp + 256 <= 0 /\ 1 * s V_pclxl_setdash__tmp1 + 13 <= 0 /\ -1 * s V_pclxl_setdash__tmp1 + -13 <= 0)%Z
   | 30 => (-1 * s V_pclxl_setdash__tmp1 + -13 <= 0 /\ 1 * s V_pclxl_setdash__tmp1 + 13 <= 0 /\ -1 * s V_pclxl_setdash__tmp + 256 <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0)%Z
   | 31 => (-1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash__tmp <= 0 /\ -1 * s V_pclxl_setdash__tmp <= 0)%Z
   | 32 => (-1 * s V_pclxl_setdash__tmp <= 0 /\ 1 * s V_pclxl_setdash__tmp <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ 1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0)%Z
   | 33 => (1 * s V_pclxl_setdash__tmp+ -1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ -1 * s V_pclxl_setdash_z <= 0)%Z
   | 34 => (-1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ 1 * s V_pclxl_setdash__tmp+ -1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash__tmp1 <= 0 /\ -1 * s V_pclxl_setdash__tmp1 <= 0)%Z
   | 35 => (-1 * s V_pclxl_setdash__tmp1 <= 0 /\ 1 * s V_pclxl_setdash__tmp1 <= 0 /\ 1 * s V_pclxl_setdash__tmp+ -1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash__tmp + -255 <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ -1 * s V_pclxl_setdash_z <= 0)%Z
   | 36 => (-1 * s V_pclxl_setdash__tmp1 + -13 <= 0 /\ -1 * s V_pclxl_setdash_z <= 0 /\ -1 * s V_pclxl_setdash_i <= 0 /\ 1 * s V_pclxl_setdash__tmp1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_pclxl_setdash (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_pclxl_setdash_count) <= z)%Q
   | 2 => (max0(s V_pclxl_setdash_count) + max0(s V_pclxl_setdash_z) <= z)%Q
   | 3 => (max0(s V_pclxl_setdash_count) + max0(s V_pclxl_setdash_z) <= z)%Q
   | 4 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_pclxl_setdash_z)) (F_check_ge (s V_pclxl_setdash_z) (0))]
     (max0(s V_pclxl_setdash_count) + max0(s V_pclxl_setdash_z) <= z)%Q
   | 5 => (s V_pclxl_setdash_z + max0(s V_pclxl_setdash_count) <= z)%Q
   | 6 => (s V_pclxl_setdash_z + max0(s V_pclxl_setdash__tmp) <= z)%Q
   | 7 => (s V_pclxl_setdash_z + max0(s V_pclxl_setdash__tmp) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_pclxl_setdash_z) (0))) (F_max0_ge_0 (s V_pclxl_setdash_z))]
     (s V_pclxl_setdash_z + max0(s V_pclxl_setdash__tmp) <= z)%Q
   | 9 => (max0(s V_pclxl_setdash__tmp) + max0(s V_pclxl_setdash_z) <= z)%Q
   | 10 => (max0(s V_pclxl_setdash__tmp) + max0(s V_pclxl_setdash_z) <= z)%Q
   | 11 => (max0(s V_pclxl_setdash__tmp) + max0(s V_pclxl_setdash_z) <= z)%Q
   | 12 => (max0(s V_pclxl_setdash__tmp - s V_pclxl_setdash_i)
            + max0(s V_pclxl_setdash_z) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_pclxl_setdash_z)) (F_check_ge (s V_pclxl_setdash_z) (0))]
     (max0(s V_pclxl_setdash__tmp - s V_pclxl_setdash_i)
      + max0(s V_pclxl_setdash_z) <= z)%Q
   | 14 => (s V_pclxl_setdash_z
            + max0(s V_pclxl_setdash__tmp - s V_pclxl_setdash_i) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_pclxl_setdash__tmp
                                             - s V_pclxl_setdash_i) (-1
                                                                    + 
                                                                    s V_pclxl_setdash__tmp
                                                                    - 
                                                                    s V_pclxl_setdash_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_pclxl_setdash__tmp
                                                 - s V_pclxl_setdash_i)) (F_check_ge (0) (0))]
     (s V_pclxl_setdash_z
      + max0(s V_pclxl_setdash__tmp - s V_pclxl_setdash_i) <= z)%Q
   | 16 => (s V_pclxl_setdash_z <= z)%Q
   | 17 => (s V_pclxl_setdash_z <= z)%Q
   | 18 => (s V_pclxl_setdash_z <= z)%Q
   | 19 => (s V_pclxl_setdash_z <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_pclxl_setdash__tmp
                                       - s V_pclxl_setdash_i) (1)]
     (s V_pclxl_setdash_z
      + max0(s V_pclxl_setdash__tmp - s V_pclxl_setdash_i) <= z)%Q
   | 21 => ((1 # 1) + s V_pclxl_setdash_z
            + max0(-1 + s V_pclxl_setdash__tmp - s V_pclxl_setdash_i) <= z)%Q
   | 22 => ((1 # 1) + s V_pclxl_setdash_z
            + max0(-1 + s V_pclxl_setdash__tmp - s V_pclxl_setdash_i) <= z)%Q
   | 23 => ((1 # 1) + s V_pclxl_setdash_z
            + max0(s V_pclxl_setdash__tmp - s V_pclxl_setdash_i) <= z)%Q
   | 24 => ((1 # 1) + s V_pclxl_setdash_z
            + max0(s V_pclxl_setdash__tmp - s V_pclxl_setdash_i) <= z)%Q
   | 25 => ((1 # 1) + s V_pclxl_setdash_z
            + max0(s V_pclxl_setdash__tmp - s V_pclxl_setdash_i) <= z)%Q
   | 26 => (s V_pclxl_setdash_z
            + max0(s V_pclxl_setdash__tmp - s V_pclxl_setdash_i) <= z)%Q
   | 27 => (max0(s V_pclxl_setdash__tmp) + max0(s V_pclxl_setdash_z) <= z)%Q
   | 28 => (max0(s V_pclxl_setdash__tmp) + max0(s V_pclxl_setdash_z) <= z)%Q
   | 29 => (-(1 # 1) + max0(s V_pclxl_setdash__tmp)
            + (1 # 13) * max0(-s V_pclxl_setdash__tmp1)
            + max0(s V_pclxl_setdash_z) <= z)%Q
   | 30 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_pclxl_setdash_z)) (F_check_ge (s V_pclxl_setdash_z) (0));
      (*-0.0769231 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_pclxl_setdash__tmp1)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_pclxl_setdash__tmp)) (F_check_ge (s V_pclxl_setdash__tmp) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_pclxl_setdash__tmp)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_pclxl_setdash__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_pclxl_setdash__tmp))]
     (-(1 # 1) + max0(s V_pclxl_setdash__tmp)
      + (1 # 13) * max0(-s V_pclxl_setdash__tmp1) + max0(s V_pclxl_setdash_z) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_pclxl_setdash__tmp)) (F_check_ge (0) (0))]
     (s V_pclxl_setdash_z + max0(s V_pclxl_setdash__tmp) <= z)%Q
   | 32 => (s V_pclxl_setdash_z <= z)%Q
   | 33 => (s V_pclxl_setdash_z <= z)%Q
   | 34 => (s V_pclxl_setdash_z <= z)%Q
   | 35 => (s V_pclxl_setdash_z <= z)%Q
   | 36 => (s V_pclxl_setdash_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_pclxl_setdash =>
    [mkPA Q (fun n z s => ai_pclxl_setdash n s /\ annot0_pclxl_setdash n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_pclxl_setdash (proc_start P_pclxl_setdash) s1 (proc_end P_pclxl_setdash) s2 ->
    (s2 V_pclxl_setdash_z <= max0(s1 V_pclxl_setdash_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_pclxl_setdash.
Qed.
