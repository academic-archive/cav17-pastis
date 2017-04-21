Require Import pasta.Pasta.

Inductive proc: Type :=
  P_getline.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_getline_z := 1%positive.
Notation V_getline__tmp := 2%positive.
Notation V_getline__tmp1 := 3%positive.
Notation V_getline_c := 4%positive.
Notation V_getline_state := 5%positive.
Notation V_getline_buf := 6%positive.
Notation V_getline_f := 7%positive.
Notation V_getline_n := 8%positive.
Definition Pedges_getline: list (edge proc) :=
  (EA 1 (AAssign V_getline_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_getline__tmp1 (Some (EVar V_getline_n))) 3)::(EA 3 (AAssign
  V_getline_state (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 (AAssign
  V_getline_c None) 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_getline_c) s) = (eval (ENum (10)) s))%Z)) 39)::
  (EA 7 (AGuard (fun s => ((eval (EVar V_getline_c) s) <> (eval (ENum (10))
  s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_getline_state) s) <> (eval (ENum (0))
  s))%Z)) 35)::(EA 9 (AGuard (fun s => ((eval (EVar V_getline_state) s) =
  (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_getline_c) s) = (eval (ENum (-1)) s))%Z)) 31)::
  (EA 11 (AGuard (fun s => ((eval (EVar V_getline_c) s) <> (eval (ENum (-1))
  s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_getline_c) s) = (eval (ENum (13)) s))%Z)) 25)::
  (EA 13 (AGuard (fun s => ((eval (EVar V_getline_c) s) <> (eval (ENum (13))
  s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 (AAssign V_getline__tmp1
  (Some (EAdd (EVar V_getline__tmp1) (ENum (-1))))) 16)::(EA 16 AWeaken 17)::
  (EA 17 (AGuard (fun s => ((eval (EAdd (EVar V_getline__tmp1) (ENum (-1)))
  s) > (eval (ENum (0)) s))%Z)) 22)::(EA 17 (AGuard
  (fun s => ((eval (EAdd (EVar V_getline__tmp1) (ENum (-1))) s) <=
  (eval (ENum (0)) s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 (AAssign
  V_getline__tmp (Some (ENum (0)))) 20)::(EA 20 ANone 21)::
  (EA 21 AWeaken 43)::(EA 22 AWeaken 23)::(EA 23 ANone 24)::
  (EA 24 ANone 28)::(EA 25 AWeaken 26)::(EA 26 (AAssign V_getline_state
  (Some (ENum (1)))) 27)::(EA 27 ANone 28)::(EA 28 ANone 29)::
  (EA 29 ANone 30)::(EA 30 (AAssign V_getline_z (Some (EAdd (ENum (1))
  (EVar V_getline_z)))) 5)::(EA 31 AWeaken 32)::(EA 32 (AAssign
  V_getline__tmp None) 33)::(EA 33 ANone 34)::(EA 34 AWeaken 43)::
  (EA 35 AWeaken 36)::(EA 36 (AAssign V_getline__tmp (Some (ENum (1)))) 37)::
  (EA 37 ANone 38)::(EA 38 AWeaken 43)::(EA 39 AWeaken 40)::(EA 40 (AAssign
  V_getline__tmp (Some (ENum (1)))) 41)::(EA 41 ANone 42)::
  (EA 42 AWeaken 43)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_getline => Pedges_getline
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_getline => 43
     end)%positive;
  var_global := var_global
}.

Definition ai_getline (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_getline_z <= 0 /\ -1 * s V_getline_z <= 0)%Z
   | 3 => (-1 * s V_getline_z <= 0 /\ 1 * s V_getline_z <= 0)%Z
   | 4 => (1 * s V_getline_z <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 5 => (-1 * s V_getline_z <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 6 => (-1 * s V_getline_state <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_z <= 0)%Z
   | 7 => (-1 * s V_getline_z <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 8 => (-1 * s V_getline_state <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_z <= 0)%Z
   | 9 => (-1 * s V_getline_z <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 10 => (-1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state <= 0)%Z
   | 11 => (1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 12 => (-1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state <= 0)%Z
   | 13 => (1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 14 => (-1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state <= 0)%Z
   | 15 => (1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 16 => (-1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state <= 0)%Z
   | 17 => (1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 18 => (-1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state <= 0 /\ 1 * s V_getline__tmp1 + -1 <= 0)%Z
   | 19 => (1 * s V_getline__tmp1 + -1 <= 0 /\ 1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 20 => (-1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state <= 0 /\ 1 * s V_getline__tmp1 + -1 <= 0 /\ 1 * s V_getline__tmp <= 0 /\ -1 * s V_getline__tmp <= 0)%Z
   | 21 => (-1 * s V_getline__tmp <= 0 /\ 1 * s V_getline__tmp <= 0 /\ 1 * s V_getline__tmp1 + -1 <= 0 /\ 1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 22 => (-1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state <= 0 /\ -1 * s V_getline__tmp1 + 2 <= 0)%Z
   | 23 => (-1 * s V_getline__tmp1 + 2 <= 0 /\ 1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 24 => (-1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state <= 0 /\ -1 * s V_getline__tmp1 + 2 <= 0)%Z
   | 25 => (-1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state <= 0 /\ 1 * s V_getline_c + -13 <= 0 /\ -1 * s V_getline_c + 13 <= 0)%Z
   | 26 => (-1 * s V_getline_c + 13 <= 0 /\ 1 * s V_getline_c + -13 <= 0 /\ 1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 27 => (-1 * s V_getline_z <= 0 /\ 1 * s V_getline_c + -13 <= 0 /\ -1 * s V_getline_c + 13 <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_state + 1 <= 0)%Z
   | 28 => (-1 * s V_getline_state <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_z <= 0)%Z
   | 29 => (-1 * s V_getline_z <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 30 => (-1 * s V_getline_state <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_z <= 0)%Z
   | 31 => (-1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state <= 0 /\ 1 * s V_getline_c + 1 <= 0 /\ -1 * s V_getline_c + -1 <= 0)%Z
   | 32 => (-1 * s V_getline_c + -1 <= 0 /\ 1 * s V_getline_c + 1 <= 0 /\ 1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 33 => (-1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state <= 0 /\ 1 * s V_getline_c + 1 <= 0 /\ -1 * s V_getline_c + -1 <= 0)%Z
   | 34 => (-1 * s V_getline_c + -1 <= 0 /\ 1 * s V_getline_c + 1 <= 0 /\ 1 * s V_getline_state <= 0 /\ -1 * s V_getline_z <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 35 => (-1 * s V_getline_z <= 0 /\ -1 * s V_getline_state + 1 <= 0 /\ 1 * s V_getline_state + -1 <= 0)%Z
   | 36 => (1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_state + 1 <= 0 /\ -1 * s V_getline_z <= 0)%Z
   | 37 => (-1 * s V_getline_z <= 0 /\ -1 * s V_getline_state + 1 <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ 1 * s V_getline__tmp + -1 <= 0 /\ -1 * s V_getline__tmp + 1 <= 0)%Z
   | 38 => (-1 * s V_getline__tmp + 1 <= 0 /\ 1 * s V_getline__tmp + -1 <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_state + 1 <= 0 /\ -1 * s V_getline_z <= 0)%Z
   | 39 => (-1 * s V_getline_state <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_c + -10 <= 0 /\ -1 * s V_getline_c + 10 <= 0)%Z
   | 40 => (-1 * s V_getline_c + 10 <= 0 /\ 1 * s V_getline_c + -10 <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 41 => (-1 * s V_getline_state <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_c + -10 <= 0 /\ -1 * s V_getline_c + 10 <= 0 /\ 1 * s V_getline__tmp + -1 <= 0 /\ -1 * s V_getline__tmp + 1 <= 0)%Z
   | 42 => (-1 * s V_getline__tmp + 1 <= 0 /\ 1 * s V_getline__tmp + -1 <= 0 /\ -1 * s V_getline_c + 10 <= 0 /\ 1 * s V_getline_c + -10 <= 0 /\ -1 * s V_getline_z <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_state <= 0)%Z
   | 43 => (-1 * s V_getline_state <= 0 /\ 1 * s V_getline_state + -1 <= 0 /\ -1 * s V_getline_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_getline (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((1 # 1) + max0(-2 + s V_getline_n) <= z)%Q
   | 2 => ((1 # 1) + s V_getline_z + max0(-2 + s V_getline_n) <= z)%Q
   | 3 => ((1 # 1) + s V_getline_z + max0(-2 + s V_getline__tmp1) <= z)%Q
   | 4 => ((1 # 1) - s V_getline_state + s V_getline_z
           + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 5 => ((1 # 1) - s V_getline_state + s V_getline_z
           + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 6 => ((1 # 1) - s V_getline_state + s V_getline_z
           + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 7 => ((1 # 1) - s V_getline_state + s V_getline_z
           + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 8 => ((1 # 1) - s V_getline_state + s V_getline_z
           + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 9 => ((1 # 1) - s V_getline_state + s V_getline_z
           + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 10 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 11 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 12 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 13 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 14 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 15 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 16 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-1 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 17 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-1 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 18 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_getline_state) (0))) (F_max0_ge_0 (-
                                                                    s V_getline_state))]
     ((1 # 1) - s V_getline_state + s V_getline_z
      + max0(-1 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 19 => ((1 # 1) + s V_getline_z + max0(-1 + s V_getline__tmp1)
            + max0(-1 + s V_getline_state) + max0(-s V_getline_state) <= z)%Q
   | 20 => ((1 # 1) + s V_getline_z + max0(-1 + s V_getline__tmp1)
            + max0(-1 + s V_getline_state) + max0(-s V_getline_state) <= z)%Q
   | 21 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_monotonic (F_check_ge (-1 + s V_getline__tmp1) (-2
                                                                    + s V_getline__tmp1));
      (*-1 0*) F_max0_ge_0 (-2 + s V_getline__tmp1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_getline_state)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_getline_state)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_getline_z + max0(-1 + s V_getline__tmp1)
      + max0(-1 + s V_getline_state) + max0(-s V_getline_state) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-1 + s V_getline__tmp1) (1)]
     ((1 # 1) - s V_getline_state + s V_getline_z
      + max0(-1 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 23 => ((2 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 24 => ((2 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_getline_state)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_getline_state) (0))) (F_max0_ge_0 (-
                                                                    s V_getline_state));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_getline_state)) (F_check_ge (0) (0))]
     ((1 # 1) - s V_getline_state + s V_getline_z
      + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 26 => ((1 # 1) + s V_getline_z + max0(-2 + s V_getline__tmp1) <= z)%Q
   | 27 => ((2 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 28 => ((2 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 29 => ((2 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 30 => ((2 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 31 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 32 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 33 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 34 => hints
     [(*-1 0*) F_max0_ge_0 (-2 + s V_getline__tmp1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1 - s V_getline_state)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               - s V_getline_state) (0))) (F_max0_ge_0 (1
                                                                    - s V_getline_state));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_getline_state)) (F_check_ge (0) (0))]
     ((1 # 1) - s V_getline_state + s V_getline_z
      + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 35 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 36 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 37 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_max0_ge_0 (-2 + s V_getline__tmp1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_getline_state)) (F_check_ge (-1
                                                                    + s V_getline_state) (0))]
     ((1 # 1) - s V_getline_state + s V_getline_z
      + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 39 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 40 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 41 => ((1 # 1) - s V_getline_state + s V_getline_z
            + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 42 => hints
     [(*-1 0*) F_max0_ge_0 (-2 + s V_getline__tmp1);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (1 - s V_getline_state)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                               - s V_getline_state) (0))) (F_max0_ge_0 (1
                                                                    - s V_getline_state));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 + s V_getline_state)) (F_check_ge (0) (0))]
     ((1 # 1) - s V_getline_state + s V_getline_z
      + max0(-2 + s V_getline__tmp1) + max0(-1 + s V_getline_state) <= z)%Q
   | 43 => (s V_getline_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_getline =>
    [mkPA Q (fun n z s => ai_getline n s /\ annot0_getline n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_getline (proc_start P_getline) s1 (proc_end P_getline) s2 ->
    (s2 V_getline_z <= (1 # 1) + max0(-2 + s1 V_getline_n))%Q.
Proof.
  prove_bound ipa admissible_ipa P_getline.
Qed.
