Require Import pasta.Pasta.

Inductive proc: Type :=
  P_median_cut.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_median_cut_z := 1%positive.
Notation V_median_cut__tmp := 2%positive.
Notation V_median_cut__tmp1 := 3%positive.
Notation V_median_cut_c0 := 4%positive.
Notation V_median_cut_c1 := 5%positive.
Notation V_median_cut_c2 := 6%positive.
Notation V_median_cut_cmax := 7%positive.
Notation V_median_cut_lb := 8%positive.
Notation V_median_cut_n := 9%positive.
Notation V_median_cut_boxlist := 10%positive.
Notation V_median_cut_cinfo := 11%positive.
Notation V_median_cut_desired_colors := 12%positive.
Notation V_median_cut_numboxes := 13%positive.
Definition Pedges_median_cut: list (edge proc) :=
  (EA 1 (AAssign V_median_cut_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_median_cut__tmp (Some (EVar V_median_cut_numboxes))) 3)::(EA 3 (AAssign
  V_median_cut__tmp1 (Some (EVar V_median_cut_desired_colors))) 4)::
  (EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_median_cut__tmp) s) <
  (eval (EVar V_median_cut__tmp1) s))%Z)) 8)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_median_cut__tmp) s) >=
  (eval (EVar V_median_cut__tmp1) s))%Z)) 7)::(EA 7 AWeaken 50)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EMul (EVar V_median_cut__tmp) (ENum (2))) s) <=
  (eval (EVar V_median_cut__tmp1) s))%Z)) 13)::(EA 9 (AGuard
  (fun s => ((eval (EMul (EVar V_median_cut__tmp) (ENum (2))) s) >
  (eval (EVar V_median_cut__tmp1) s))%Z)) 10)::(EA 10 AWeaken 11)::
  (EA 11 ANone 12)::(EA 12 AWeaken 16)::(EA 13 AWeaken 14)::
  (EA 14 ANone 15)::(EA 15 AWeaken 16)::(EA 16 ANone 48)::(EA 16 ANone 17)::
  (EA 17 (AAssign V_median_cut_c0 None) 18)::(EA 18 (AAssign V_median_cut_c1
  None) 19)::(EA 19 (AAssign V_median_cut_c2 None) 20)::(EA 20 (AAssign
  V_median_cut_cmax (Some (EVar V_median_cut_c1))) 21)::(EA 21 (AAssign
  V_median_cut_n (Some (ENum (1)))) 22)::(EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_median_cut_c0) s) > (eval (EVar V_median_cut_cmax)
  s))%Z)) 25)::(EA 23 (AGuard (fun s => ((eval (EVar V_median_cut_c0) s) <=
  (eval (EVar V_median_cut_cmax) s))%Z)) 24)::(EA 24 AWeaken 30)::
  (EA 25 AWeaken 26)::(EA 26 (AAssign V_median_cut_cmax
  (Some (EVar V_median_cut_c0))) 27)::(EA 27 (AAssign V_median_cut_n
  (Some (ENum (0)))) 28)::(EA 28 ANone 29)::(EA 29 AWeaken 30)::
  (EA 30 (AGuard (fun s => ((eval (EVar V_median_cut_c2) s) >
  (eval (EVar V_median_cut_cmax) s))%Z)) 32)::(EA 30 (AGuard
  (fun s => ((eval (EVar V_median_cut_c2) s) <=
  (eval (EVar V_median_cut_cmax) s))%Z)) 31)::(EA 31 AWeaken 36)::
  (EA 32 AWeaken 33)::(EA 33 (AAssign V_median_cut_n (Some (ENum (2)))) 34)::
  (EA 34 ANone 35)::(EA 35 AWeaken 36)::(EA 36 ANone 43)::(EA 36 ANone 41)::
  (EA 36 ANone 39)::(EA 36 ANone 37)::(EA 37 (AAssign V_median_cut_lb
  None) 38)::(EA 38 ANone 43)::(EA 39 (AAssign V_median_cut_lb None) 40)::
  (EA 40 ANone 43)::(EA 41 (AAssign V_median_cut_lb None) 42)::
  (EA 42 ANone 43)::(EA 43 (AAssign V_median_cut__tmp
  (Some (EAdd (EVar V_median_cut__tmp) (ENum (1))))) 44)::(EA 44 ANone 45)::
  (EA 45 ANone 46)::(EA 46 (AAssign V_median_cut_z (Some (EAdd (ENum (1))
  (EVar V_median_cut_z)))) 47)::(EA 47 AWeaken 6)::(EA 48 ANone 49)::
  (EA 49 AWeaken 50)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_median_cut => Pedges_median_cut
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_median_cut => 50
     end)%positive;
  var_global := var_global
}.

Definition ai_median_cut (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_median_cut_z <= 0 /\ -1 * s V_median_cut_z <= 0)%Z
   | 3 => (-1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_z <= 0)%Z
   | 4 => (1 * s V_median_cut_z <= 0 /\ -1 * s V_median_cut_z <= 0)%Z
   | 5 => (-1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_z <= 0)%Z
   | 6 => (-1 * s V_median_cut_z <= 0)%Z
   | 7 => (-1 * s V_median_cut_z <= 0 /\ -1 * s V_median_cut__tmp+ 1 * s V_median_cut__tmp1 <= 0)%Z
   | 8 => (-1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0)%Z
   | 9 => (1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0)%Z
   | 10 => (-1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -2 * s V_median_cut__tmp+ 1 * s V_median_cut__tmp1 + 1 <= 0)%Z
   | 11 => (-2 * s V_median_cut__tmp+ 1 * s V_median_cut__tmp1 + 1 <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0)%Z
   | 12 => (-1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -2 * s V_median_cut__tmp+ 1 * s V_median_cut__tmp1 + 1 <= 0)%Z
   | 13 => (-1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ 2 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 <= 0)%Z
   | 14 => (2 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0)%Z
   | 15 => (-1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ 2 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 <= 0)%Z
   | 16 => (1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0)%Z
   | 17 => (-1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0)%Z
   | 18 => (1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0)%Z
   | 19 => (-1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0)%Z
   | 20 => (1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0)%Z
   | 21 => (-1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0)%Z
   | 22 => (1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_n + -1 <= 0 /\ -1 * s V_median_cut_n + 1 <= 0)%Z
   | 23 => (-1 * s V_median_cut_n + 1 <= 0 /\ 1 * s V_median_cut_n + -1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0)%Z
   | 24 => (1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_n + -1 <= 0 /\ -1 * s V_median_cut_n + 1 <= 0 /\ 1 * s V_median_cut_c0+ -1 * s V_median_cut_cmax <= 0)%Z
   | 25 => (1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_n + -1 <= 0 /\ -1 * s V_median_cut_n + 1 <= 0 /\ -1 * s V_median_cut_c0+ 1 * s V_median_cut_cmax + 1 <= 0)%Z
   | 26 => (-1 * s V_median_cut_c0+ 1 * s V_median_cut_cmax + 1 <= 0 /\ -1 * s V_median_cut_n + 1 <= 0 /\ 1 * s V_median_cut_n + -1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0)%Z
   | 27 => (1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_n + -1 <= 0 /\ -1 * s V_median_cut_n + 1 <= 0)%Z
   | 28 => (-1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ 1 * s V_median_cut_n <= 0 /\ -1 * s V_median_cut_n <= 0)%Z
   | 29 => (-1 * s V_median_cut_n <= 0 /\ 1 * s V_median_cut_n <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0)%Z
   | 30 => (1 * s V_median_cut_n + -1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_n <= 0)%Z
   | 31 => (-1 * s V_median_cut_n <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_n + -1 <= 0 /\ 1 * s V_median_cut_c2+ -1 * s V_median_cut_cmax <= 0)%Z
   | 32 => (-1 * s V_median_cut_n <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_n + -1 <= 0 /\ -1 * s V_median_cut_c2+ 1 * s V_median_cut_cmax + 1 <= 0)%Z
   | 33 => (-1 * s V_median_cut_c2+ 1 * s V_median_cut_cmax + 1 <= 0 /\ 1 * s V_median_cut_n + -1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_n <= 0)%Z
   | 34 => (1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ -1 * s V_median_cut_c2+ 1 * s V_median_cut_cmax + 1 <= 0 /\ 1 * s V_median_cut_n + -2 <= 0 /\ -1 * s V_median_cut_n + 2 <= 0)%Z
   | 35 => (-1 * s V_median_cut_n + 2 <= 0 /\ 1 * s V_median_cut_n + -2 <= 0 /\ -1 * s V_median_cut_c2+ 1 * s V_median_cut_cmax + 1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0)%Z
   | 36 => (-1 * s V_median_cut_n <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_n + -2 <= 0)%Z
   | 37 => (1 * s V_median_cut_n + -2 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_n <= 0)%Z
   | 38 => (-1 * s V_median_cut_n <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_n + -2 <= 0)%Z
   | 39 => (1 * s V_median_cut_n + -2 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_n <= 0)%Z
   | 40 => (-1 * s V_median_cut_n <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_n + -2 <= 0)%Z
   | 41 => (1 * s V_median_cut_n + -2 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_n <= 0)%Z
   | 42 => (-1 * s V_median_cut_n <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_n + -2 <= 0)%Z
   | 43 => (1 * s V_median_cut_n + -2 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_n <= 0)%Z
   | 44 => (-1 * s V_median_cut_n <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_n + -2 <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 <= 0)%Z
   | 45 => (1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 <= 0 /\ 1 * s V_median_cut_n + -2 <= 0 /\ -1 * s V_median_cut_z <= 0 /\ -1 * s V_median_cut_n <= 0)%Z
   | 46 => (-1 * s V_median_cut_n <= 0 /\ -1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut_n + -2 <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 <= 0)%Z
   | 47 => (1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 <= 0 /\ 1 * s V_median_cut_n + -2 <= 0 /\ -1 * s V_median_cut_n <= 0 /\ -1 * s V_median_cut_z + 1 <= 0)%Z
   | 48 => (-1 * s V_median_cut_z <= 0 /\ 1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0)%Z
   | 49 => (1 * s V_median_cut__tmp+ -1 * s V_median_cut__tmp1 + 1 <= 0 /\ -1 * s V_median_cut_z <= 0)%Z
   | 50 => (-1 * s V_median_cut_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_median_cut (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_median_cut_desired_colors - s V_median_cut_numboxes) <= z)%Q
   | 2 => (s V_median_cut_z
           + max0(s V_median_cut_desired_colors - s V_median_cut_numboxes) <= z)%Q
   | 3 => (s V_median_cut_z
           + max0(-s V_median_cut__tmp + s V_median_cut_desired_colors) <= z)%Q
   | 4 => (s V_median_cut_z
           + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 5 => (s V_median_cut_z
           + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 6 => (s V_median_cut_z
           + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_median_cut__tmp
                                             + s V_median_cut__tmp1) (-1
                                                                    - s V_median_cut__tmp
                                                                    + s V_median_cut__tmp1));
      (*-1 0*) F_max0_ge_0 (-1 - s V_median_cut__tmp + s V_median_cut__tmp1)]
     (s V_median_cut_z + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 8 => (s V_median_cut_z
           + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 9 => (s V_median_cut_z
           + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 10 => (s V_median_cut_z
            + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 11 => (s V_median_cut_z
            + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_median_cut__tmp
                                       + s V_median_cut__tmp1) (1)]
     (s V_median_cut_z + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 13 => (s V_median_cut_z
            + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 14 => (s V_median_cut_z
            + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 15 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (-s V_median_cut__tmp
                                       + s V_median_cut__tmp1) (1)]
     (s V_median_cut_z + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 16 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 17 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 18 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 19 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 20 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 21 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 22 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 23 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 24 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (2
                                                                 - s V_median_cut_n) (0))) (F_max0_ge_0 (2
                                                                    - s V_median_cut_n))]
     ((1 # 1) + s V_median_cut_z
      + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 25 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 26 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 27 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 28 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 29 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 30 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 31 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 32 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (2 - s V_median_cut_n)) (F_check_ge (2
                                                                    - s V_median_cut_n) (0))]
     ((1 # 2) * s V_median_cut_n + s V_median_cut_z
      + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
      + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 33 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 34 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 35 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 36 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 37 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 38 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 39 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 40 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 41 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 42 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 43 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1)
            + (1 # 2) * max0(2 - s V_median_cut_n) <= z)%Q
   | 44 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + (1 # 2) * max0(2 - s V_median_cut_n)
            + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 45 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + (1 # 2) * max0(2 - s V_median_cut_n)
            + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 46 => ((1 # 2) * s V_median_cut_n + s V_median_cut_z
            + (1 # 2) * max0(2 - s V_median_cut_n)
            + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 47 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (2 - s V_median_cut_n)) (F_check_ge (2
                                                                    - s V_median_cut_n) (0))]
     (-(1 # 1) + (1 # 2) * s V_median_cut_n + s V_median_cut_z
      + (1 # 2) * max0(2 - s V_median_cut_n)
      + max0(-s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 48 => ((1 # 1) + s V_median_cut_z
            + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 49 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 - s V_median_cut__tmp + s V_median_cut__tmp1)]
     ((1 # 1) + s V_median_cut_z
      + max0(-1 - s V_median_cut__tmp + s V_median_cut__tmp1) <= z)%Q
   | 50 => (s V_median_cut_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_median_cut =>
    [mkPA Q (fun n z s => ai_median_cut n s /\ annot0_median_cut n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_median_cut (proc_start P_median_cut) s1 (proc_end P_median_cut) s2 ->
    (s2 V_median_cut_z <= max0(s1 V_median_cut_desired_colors
                               - s1 V_median_cut_numboxes))%Q.
Proof.
  prove_bound ipa admissible_ipa P_median_cut.
Qed.
