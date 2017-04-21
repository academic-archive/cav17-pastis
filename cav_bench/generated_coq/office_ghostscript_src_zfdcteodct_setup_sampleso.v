Require Import pasta.Pasta.

Inductive proc: Type :=
  P_dct_setup_samples.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_dct_setup_samples_z := 1%positive.
Notation V_dct_setup_samples__tmp := 2%positive.
Notation V_dct_setup_samples__tmp1 := 3%positive.
Notation V_dct_setup_samples__tmp2 := 4%positive.
Notation V_dct_setup_samples_code := 5%positive.
Notation V_dct_setup_samples_i := 6%positive.
Notation V_dct_setup_samples_is_vert := 7%positive.
Notation V_dct_setup_samples_jcdp := 8%positive.
Notation V_dct_setup_samples_kstr := 9%positive.
Notation V_dct_setup_samples_num_colors := 10%positive.
Notation V_dct_setup_samples_op := 11%positive.
Definition Pedges_dct_setup_samples: list (edge proc) :=
  (EA 1 (AAssign V_dct_setup_samples_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_dct_setup_samples__tmp (Some (EVar V_dct_setup_samples_num_colors))) 3)::
  (EA 3 (AAssign V_dct_setup_samples__tmp2
  (Some (EVar V_dct_setup_samples_is_vert))) 4)::(EA 4 AWeaken 5)::
  (EA 5 ANone 6)::(EA 5 ANone 8)::(EA 6 AWeaken 7)::(EA 7 ANone 9)::
  (EA 7 ANone 8)::(EA 8 ANone 15)::(EA 9 AWeaken 10)::(EA 10 ANone 45)::
  (EA 10 ANone 11)::(EA 11 (AAssign V_dct_setup_samples_code None) 12)::
  (EA 12 AWeaken 13)::(EA 13 ANone 42)::(EA 13 ANone 14)::(EA 14 ANone 15)::
  (EA 15 (AAssign V_dct_setup_samples_i (Some (ENum (0)))) 16)::
  (EA 16 ANone 17)::(EA 17 AWeaken 18)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_dct_setup_samples_i) s) <
  (eval (EVar V_dct_setup_samples__tmp) s))%Z)) 23)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_dct_setup_samples_i) s) >=
  (eval (EVar V_dct_setup_samples__tmp) s))%Z)) 19)::(EA 19 AWeaken 20)::
  (EA 20 (AAssign V_dct_setup_samples__tmp1 (Some (ENum (0)))) 21)::
  (EA 21 ANone 22)::(EA 22 AWeaken 48)::(EA 23 AWeaken 24)::
  (EA 24 ANone 39)::(EA 24 ANone 25)::(EA 25 AWeaken 26)::(EA 26 ANone 39)::
  (EA 26 ANone 27)::(EA 27 AWeaken 28)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_dct_setup_samples__tmp2) s) <> (eval (ENum (0))
  s))%Z)) 31)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_dct_setup_samples__tmp2) s) = (eval (ENum (0))
  s))%Z)) 29)::(EA 29 AWeaken 30)::(EA 30 ANone 33)::(EA 31 AWeaken 32)::
  (EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 (AAssign V_dct_setup_samples_i
  (Some (EAdd (EVar V_dct_setup_samples_i) (ENum (1))))) 35)::
  (EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign V_dct_setup_samples_z
  (Some (EAdd (ENum (1)) (EVar V_dct_setup_samples_z)))) 38)::
  (EA 38 AWeaken 18)::(EA 39 (AAssign V_dct_setup_samples__tmp1
  (Some (ENum (-15)))) 40)::(EA 40 ANone 41)::(EA 41 AWeaken 48)::
  (EA 42 (AAssign V_dct_setup_samples__tmp1
  (Some (EVar V_dct_setup_samples_code))) 43)::(EA 43 ANone 44)::
  (EA 44 AWeaken 48)::(EA 45 (AAssign V_dct_setup_samples__tmp1
  (Some (ENum (-15)))) 46)::(EA 46 ANone 47)::(EA 47 AWeaken 48)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_dct_setup_samples => Pedges_dct_setup_samples
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_dct_setup_samples => 48
     end)%positive;
  var_global := var_global
}.

Definition ai_dct_setup_samples (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 3 => (-1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples_z <= 0)%Z
   | 4 => (1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 5 => (-1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples_z <= 0)%Z
   | 6 => (1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 7 => (-1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples_z <= 0)%Z
   | 8 => (1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 9 => (1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 10 => (-1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples_z <= 0)%Z
   | 11 => (1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 12 => (-1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples_z <= 0)%Z
   | 13 => (1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 14 => (-1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples_z <= 0)%Z
   | 15 => (1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 16 => (-1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_i <= 0)%Z
   | 17 => (-1 * s V_dct_setup_samples_i <= 0 /\ 1 * s V_dct_setup_samples_i <= 0 /\ 1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 18 => (-1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_i <= 0)%Z
   | 19 => (-1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples__tmp+ -1 * s V_dct_setup_samples_i <= 0)%Z
   | 20 => (1 * s V_dct_setup_samples__tmp+ -1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_i <= 0)%Z
   | 21 => (-1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples__tmp+ -1 * s V_dct_setup_samples_i <= 0 /\ 1 * s V_dct_setup_samples__tmp1 <= 0 /\ -1 * s V_dct_setup_samples__tmp1 <= 0)%Z
   | 22 => (-1 * s V_dct_setup_samples__tmp1 <= 0 /\ 1 * s V_dct_setup_samples__tmp1 <= 0 /\ 1 * s V_dct_setup_samples__tmp+ -1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_i <= 0)%Z
   | 23 => (-1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0)%Z
   | 24 => (-1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_i <= 0)%Z
   | 25 => (-1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0)%Z
   | 26 => (-1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_i <= 0)%Z
   | 27 => (-1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0)%Z
   | 28 => (-1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_i <= 0)%Z
   | 29 => (-1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0 /\ 1 * s V_dct_setup_samples__tmp2 <= 0 /\ -1 * s V_dct_setup_samples__tmp2 <= 0)%Z
   | 30 => (-1 * s V_dct_setup_samples__tmp2 <= 0 /\ 1 * s V_dct_setup_samples__tmp2 <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_i <= 0)%Z
   | 31 => (-1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0)%Z
   | 32 => (-1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_i <= 0)%Z
   | 33 => (-1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0)%Z
   | 34 => (-1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_i <= 0)%Z
   | 35 => (-1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_i + 1 <= 0)%Z
   | 36 => (-1 * s V_dct_setup_samples_i + 1 <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 37 => (-1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_i + 1 <= 0)%Z
   | 38 => (-1 * s V_dct_setup_samples_i + 1 <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z + 1 <= 0)%Z
   | 39 => (-1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0)%Z
   | 40 => (-1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_i <= 0 /\ 1 * s V_dct_setup_samples__tmp1 + 15 <= 0 /\ -1 * s V_dct_setup_samples__tmp1 + -15 <= 0)%Z
   | 41 => (-1 * s V_dct_setup_samples__tmp1 + -15 <= 0 /\ 1 * s V_dct_setup_samples__tmp1 + 15 <= 0 /\ -1 * s V_dct_setup_samples_i <= 0 /\ -1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples__tmp+ 1 * s V_dct_setup_samples_i + 1 <= 0)%Z
   | 42 => (-1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples_z <= 0)%Z
   | 43 => (1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 44 => (-1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples_z <= 0)%Z
   | 45 => (1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 46 => (-1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples_z <= 0 /\ 1 * s V_dct_setup_samples__tmp1 + 15 <= 0 /\ -1 * s V_dct_setup_samples__tmp1 + -15 <= 0)%Z
   | 47 => (-1 * s V_dct_setup_samples__tmp1 + -15 <= 0 /\ 1 * s V_dct_setup_samples__tmp1 + 15 <= 0 /\ 1 * s V_dct_setup_samples_z <= 0 /\ -1 * s V_dct_setup_samples_z <= 0)%Z
   | 48 => (-1 * s V_dct_setup_samples_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_dct_setup_samples (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_dct_setup_samples_num_colors) <= z)%Q
   | 2 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples_num_colors) <= z)%Q
   | 3 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 4 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 5 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 6 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 7 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 8 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 9 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 10 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 11 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 12 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 13 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 14 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 15 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 16 => (s V_dct_setup_samples_z
            + max0(s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_dct_setup_samples_z) (0))) (F_max0_ge_0 (s V_dct_setup_samples_z))]
     (s V_dct_setup_samples_z
      + max0(s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 18 => (max0(s V_dct_setup_samples__tmp - s V_dct_setup_samples_i)
            + max0(s V_dct_setup_samples_z) <= z)%Q
   | 19 => (max0(s V_dct_setup_samples__tmp - s V_dct_setup_samples_i)
            + max0(s V_dct_setup_samples_z) <= z)%Q
   | 20 => (max0(s V_dct_setup_samples__tmp - s V_dct_setup_samples_i)
            + max0(s V_dct_setup_samples_z) <= z)%Q
   | 21 => (max0(s V_dct_setup_samples__tmp - s V_dct_setup_samples_i)
            + max0(s V_dct_setup_samples_z) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_dct_setup_samples__tmp
                                             - s V_dct_setup_samples_i) (-1
                                                                    + s V_dct_setup_samples__tmp
                                                                    - s V_dct_setup_samples_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_dct_setup_samples__tmp
                            - s V_dct_setup_samples_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_dct_setup_samples_z)) (F_check_ge (s V_dct_setup_samples_z) (0))]
     (max0(s V_dct_setup_samples__tmp - s V_dct_setup_samples_i)
      + max0(s V_dct_setup_samples_z) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_dct_setup_samples__tmp
                                       - s V_dct_setup_samples_i) (1);
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_dct_setup_samples_z)) (F_check_ge (s V_dct_setup_samples_z) (0))]
     (max0(s V_dct_setup_samples__tmp - s V_dct_setup_samples_i)
      + max0(s V_dct_setup_samples_z) <= z)%Q
   | 24 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 25 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 26 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 27 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 28 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 29 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 30 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 31 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 32 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 33 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 34 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 35 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 36 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 37 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_dct_setup_samples_z) (0))) (F_max0_ge_0 (s V_dct_setup_samples_z))]
     (s V_dct_setup_samples_z
      + max0(s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 39 => ((1 # 1) + s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i) <= z)%Q
   | 40 => (s V_dct_setup_samples_z
            + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i)
            + (1 # 15) * max0(-s V_dct_setup_samples__tmp1) <= z)%Q
   | 41 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_dct_setup_samples__tmp
                            - s V_dct_setup_samples_i);
      (*-0.0666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_dct_setup_samples__tmp1)) (F_check_ge (0) (0))]
     (s V_dct_setup_samples_z
      + max0(-1 + s V_dct_setup_samples__tmp - s V_dct_setup_samples_i)
      + (1 # 15) * max0(-s V_dct_setup_samples__tmp1) <= z)%Q
   | 42 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 43 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 44 => hints
     [(*-1 0*) F_max0_ge_0 (s V_dct_setup_samples__tmp)]
     (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 45 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 46 => (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_max0_ge_0 (s V_dct_setup_samples__tmp)]
     (s V_dct_setup_samples_z + max0(s V_dct_setup_samples__tmp) <= z)%Q
   | 48 => (s V_dct_setup_samples_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_dct_setup_samples =>
    [mkPA Q (fun n z s => ai_dct_setup_samples n s /\ annot0_dct_setup_samples n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_dct_setup_samples (proc_start P_dct_setup_samples) s1 (proc_end P_dct_setup_samples) s2 ->
    (s2 V_dct_setup_samples_z <= max0(s1 V_dct_setup_samples_num_colors))%Q.
Proof.
  prove_bound ipa admissible_ipa P_dct_setup_samples.
Qed.
