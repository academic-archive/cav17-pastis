Require Import pasta.Pasta.

Inductive proc: Type :=
  P_expand_right_edge.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_expand_right_edge_z := 1%positive.
Notation V_expand_right_edge__tmp := 2%positive.
Notation V_expand_right_edge__tmp1 := 3%positive.
Notation V_expand_right_edge__tmp2 := 4%positive.
Notation V_expand_right_edge_count := 5%positive.
Notation V_expand_right_edge_numcols := 6%positive.
Notation V_expand_right_edge_pixval := 7%positive.
Notation V_expand_right_edge_row := 8%positive.
Notation V_expand_right_edge_image_data := 9%positive.
Notation V_expand_right_edge_input_cols := 10%positive.
Notation V_expand_right_edge_num_rows := 11%positive.
Notation V_expand_right_edge_output_cols := 12%positive.
Definition Pedges_expand_right_edge: list (edge proc) :=
  (EA 1 (AAssign V_expand_right_edge_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_expand_right_edge__tmp (Some (EVar V_expand_right_edge_num_rows))) 3)::
  (EA 3 (AAssign V_expand_right_edge__tmp1
  (Some (EVar V_expand_right_edge_input_cols))) 4)::(EA 4 (AAssign
  V_expand_right_edge__tmp2
  (Some (EVar V_expand_right_edge_output_cols))) 5)::(EA 5 (AAssign
  V_expand_right_edge_numcols (Some (ESub (EVar V_expand_right_edge__tmp2)
  (EVar V_expand_right_edge__tmp1)))) 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_expand_right_edge_numcols) s) > (eval (ENum (0))
  s))%Z)) 9)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_expand_right_edge_numcols) s) <= (eval (ENum (0))
  s))%Z)) 8)::(EA 8 AWeaken 17)::(EA 9 AWeaken 10)::(EA 10 (AAssign
  V_expand_right_edge_row (Some (ENum (0)))) 11)::(EA 11 ANone 12)::
  (EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_expand_right_edge_row) s) <
  (eval (EVar V_expand_right_edge__tmp) s))%Z)) 18)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_expand_right_edge_row) s) >=
  (eval (EVar V_expand_right_edge__tmp) s))%Z)) 14)::(EA 14 AWeaken 15)::
  (EA 15 ANone 16)::(EA 16 AWeaken 17)::(EA 18 AWeaken 19)::(EA 19 (AAssign
  V_expand_right_edge_pixval None) 20)::(EA 20 (AAssign
  V_expand_right_edge_count (Some (EVar V_expand_right_edge_numcols))) 21)::
  (EA 21 ANone 22)::(EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_expand_right_edge_count) s) > (eval (ENum (0))
  s))%Z)) 31)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_expand_right_edge_count) s) <= (eval (ENum (0))
  s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 ANone 26)::(EA 26 (AAssign
  V_expand_right_edge_row (Some (EAdd (EVar V_expand_right_edge_row)
  (ENum (1))))) 27)::(EA 27 ANone 28)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_expand_right_edge_z (Some (EAdd (ENum (1))
  (EVar V_expand_right_edge_z)))) 30)::(EA 30 AWeaken 13)::
  (EA 31 AWeaken 32)::(EA 32 ANone 33)::(EA 33 (AAssign
  V_expand_right_edge_count (Some (EAdd (EVar V_expand_right_edge_count)
  (ENum (-1))))) 34)::(EA 34 ANone 35)::(EA 35 ANone 36)::(EA 36 (AAssign
  V_expand_right_edge_z (Some (EAdd (ENum (1))
  (EVar V_expand_right_edge_z)))) 37)::(EA 37 AWeaken 23)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_expand_right_edge => Pedges_expand_right_edge
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_expand_right_edge => 17
     end)%positive;
  var_global := var_global
}.

Definition ai_expand_right_edge (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_z <= 0)%Z
   | 3 => (-1 * s V_expand_right_edge_z <= 0 /\ 1 * s V_expand_right_edge_z <= 0)%Z
   | 4 => (1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_z <= 0)%Z
   | 5 => (-1 * s V_expand_right_edge_z <= 0 /\ 1 * s V_expand_right_edge_z <= 0)%Z
   | 6 => (1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_z <= 0)%Z
   | 7 => (-1 * s V_expand_right_edge_z <= 0 /\ 1 * s V_expand_right_edge_z <= 0)%Z
   | 8 => (1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ 1 * s V_expand_right_edge_numcols <= 0)%Z
   | 9 => (1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0)%Z
   | 10 => (-1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ 1 * s V_expand_right_edge_z <= 0)%Z
   | 11 => (1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ 1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_row <= 0)%Z
   | 12 => (-1 * s V_expand_right_edge_row <= 0 /\ 1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ 1 * s V_expand_right_edge_z <= 0)%Z
   | 13 => (-1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0)%Z
   | 14 => (-1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ 1 * s V_expand_right_edge__tmp+ -1 * s V_expand_right_edge_row <= 0)%Z
   | 15 => (1 * s V_expand_right_edge__tmp+ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0)%Z
   | 16 => (-1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ 1 * s V_expand_right_edge__tmp+ -1 * s V_expand_right_edge_row <= 0)%Z
   | 17 => (-1 * s V_expand_right_edge_z <= 0)%Z
   | 18 => (-1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0)%Z
   | 19 => (-1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0)%Z
   | 20 => (-1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0)%Z
   | 21 => (-1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_count + 1 <= 0)%Z
   | 22 => (-1 * s V_expand_right_edge_count + 1 <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0)%Z
   | 23 => (-1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_count <= 0)%Z
   | 24 => (-1 * s V_expand_right_edge_count <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ 1 * s V_expand_right_edge_count <= 0)%Z
   | 25 => (1 * s V_expand_right_edge_count <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_count <= 0)%Z
   | 26 => (-1 * s V_expand_right_edge_count <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ 1 * s V_expand_right_edge_count <= 0)%Z
   | 27 => (1 * s V_expand_right_edge_count <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_count <= 0 /\ -1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row <= 0)%Z
   | 28 => (-1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_count <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ 1 * s V_expand_right_edge_count <= 0)%Z
   | 29 => (1 * s V_expand_right_edge_count <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_count <= 0 /\ -1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row <= 0)%Z
   | 30 => (-1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_count <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ 1 * s V_expand_right_edge_count <= 0 /\ -1 * s V_expand_right_edge_z + 1 <= 0)%Z
   | 31 => (-1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_count + 1 <= 0)%Z
   | 32 => (-1 * s V_expand_right_edge_count + 1 <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_row <= 0)%Z
   | 33 => (-1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge_count + 1 <= 0)%Z
   | 34 => (-1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_count <= 0)%Z
   | 35 => (-1 * s V_expand_right_edge_count <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_z <= 0)%Z
   | 36 => (-1 * s V_expand_right_edge_z <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_count <= 0)%Z
   | 37 => (-1 * s V_expand_right_edge_count <= 0 /\ -1 * s V_expand_right_edge_row <= 0 /\ -1 * s V_expand_right_edge_numcols + 1 <= 0 /\ -1 * s V_expand_right_edge__tmp+ 1 * s V_expand_right_edge_row + 1 <= 0 /\ -1 * s V_expand_right_edge_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_expand_right_edge (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 - s V_expand_right_edge_input_cols
                + s V_expand_right_edge_output_cols) * max0(s V_expand_right_edge_num_rows)
           + (2 # 1) * max0(s V_expand_right_edge_num_rows) <= z)%Q
   | 2 => (s V_expand_right_edge_z
           + max0(-1 - s V_expand_right_edge_input_cols
                  + s V_expand_right_edge_output_cols) * max0(s V_expand_right_edge_num_rows)
           + (2 # 1) * max0(s V_expand_right_edge_num_rows) <= z)%Q
   | 3 => (s V_expand_right_edge_z
           + max0(-1 - s V_expand_right_edge_input_cols
                  + s V_expand_right_edge_output_cols) * max0(s V_expand_right_edge__tmp)
           + (2 # 1) * max0(s V_expand_right_edge__tmp) <= z)%Q
   | 4 => (s V_expand_right_edge_z
           + max0(-1 - s V_expand_right_edge__tmp1
                  + s V_expand_right_edge_output_cols) * max0(s V_expand_right_edge__tmp)
           + (2 # 1) * max0(s V_expand_right_edge__tmp) <= z)%Q
   | 5 => (s V_expand_right_edge_z
           + max0(-1 - s V_expand_right_edge__tmp1
                  + s V_expand_right_edge__tmp2) * max0(s V_expand_right_edge__tmp)
           + (2 # 1) * max0(s V_expand_right_edge__tmp) <= z)%Q
   | 6 => (s V_expand_right_edge_z
           + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
           + (2 # 1) * max0(s V_expand_right_edge__tmp) <= z)%Q
   | 7 => (s V_expand_right_edge_z
           + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
           + (2 # 1) * max0(s V_expand_right_edge__tmp) <= z)%Q
   | 8 => hints
     [(*-2 0*) F_max0_ge_0 (s V_expand_right_edge__tmp);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                            + s V_expand_right_edge_numcols)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)))]
     (s V_expand_right_edge_z
      + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
      + (2 # 1) * max0(s V_expand_right_edge__tmp) <= z)%Q
   | 9 => (s V_expand_right_edge_z
           + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
           + (2 # 1) * max0(s V_expand_right_edge__tmp) <= z)%Q
   | 10 => (s V_expand_right_edge_z
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            + (2 # 1) * max0(s V_expand_right_edge__tmp) <= z)%Q
   | 11 => (-(1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            + (1 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge__tmp)
            + s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            + s V_expand_right_edge_z
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + max0(s V_expand_right_edge__tmp)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row)
            + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * max0(s V_expand_right_edge_row) <= z)%Q
   | 12 => hints
     [(*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-s V_expand_right_edge_z) (0))) (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_z));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_0 (-s V_expand_right_edge_z)) (F_check_ge (0) (0));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_expand_right_edge_z)) (F_check_ge (s V_expand_right_edge_z) (0));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_expand_right_edge_z) (0))) (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_expand_right_edge_z)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_z) (0))) (F_max0_ge_0 (s V_expand_right_edge_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0)))]
     (-(1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
      + (1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                    - s V_expand_right_edge_row)
      - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
      + (2 # 5) * s V_expand_right_edge_row
      + (1 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge__tmp)
      + s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
      + s V_expand_right_edge_z
      + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
      - (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
      + max0(s V_expand_right_edge__tmp)
      - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
      + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row)
      + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
      - (2 # 5) * max0(s V_expand_right_edge_row) <= z)%Q
   | 13 => (-(1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            + (1 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge__tmp)
            + s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + max0(s V_expand_right_edge__tmp)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row)
            + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * max0(s V_expand_right_edge_row)
            + max0(s V_expand_right_edge_z) <= z)%Q
   | 14 => (-(1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            + (1 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge__tmp)
            + s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + max0(s V_expand_right_edge__tmp)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row)
            + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * max0(s V_expand_right_edge_row)
            + max0(s V_expand_right_edge_z) <= z)%Q
   | 15 => (-(1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            + (1 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge__tmp)
            + s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + max0(s V_expand_right_edge__tmp)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row)
            + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * max0(s V_expand_right_edge_row)
            + max0(s V_expand_right_edge_z) <= z)%Q
   | 16 => hints
     [(*-2 0*) F_max0_monotonic (F_check_ge (s V_expand_right_edge__tmp
                                             - s V_expand_right_edge_row) (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row));
      (*-2 0*) F_max0_ge_0 (-1 + s V_expand_right_edge__tmp
                            - s V_expand_right_edge_row);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                              + s V_expand_right_edge_numcols)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                            + s V_expand_right_edge_numcols)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_expand_right_edge__tmp
                                                                    + s V_expand_right_edge_row) (0))) (F_max0_ge_0 (-
                                                                    s V_expand_right_edge__tmp
                                                                    + s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_expand_right_edge__tmp
                                                                + s V_expand_right_edge_row)) (F_check_ge (-
                                                                    s V_expand_right_edge__tmp
                                                                    + s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_expand_right_edge__tmp
                                                              + s V_expand_right_edge_row)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                              - s V_expand_right_edge_row)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_numcols)) (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_numcols)) (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_z)) (F_check_ge (s V_expand_right_edge_z) (0));
      (*-0.4 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge_row))]
     (-(1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
      + (1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                    - s V_expand_right_edge_row)
      - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
      + (2 # 5) * s V_expand_right_edge_row
      + (1 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge__tmp)
      + s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
      + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
      - (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
      + max0(s V_expand_right_edge__tmp)
      - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
      + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row)
      + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
      - (2 # 5) * max0(s V_expand_right_edge_row)
      + max0(s V_expand_right_edge_z) <= z)%Q
   | 17 => (s V_expand_right_edge_z <= z)%Q
   | 18 => hints
     [(*0 0.4*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                             + s V_expand_right_edge__tmp)) (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*0 0.2*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                               + s V_expand_right_edge__tmp
                                                               - s V_expand_right_edge_row)) (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*0 0.4*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp
                                                               - s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_row)) (F_check_ge (0) (0)))]
     (-(1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
      + (1 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                    - s V_expand_right_edge_row)
      - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
      + (2 # 5) * s V_expand_right_edge_row
      + (1 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge__tmp)
      + s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
      + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
      - (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
      + max0(s V_expand_right_edge__tmp)
      - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
      + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row)
      + max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
      - (2 # 5) * max0(s V_expand_right_edge_row)
      + max0(s V_expand_right_edge_z) <= z)%Q
   | 19 => ((6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                        - s V_expand_right_edge_row)
            + s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
            - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (4 # 5) * max0(s V_expand_right_edge__tmp)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + max0(s V_expand_right_edge_z) <= z)%Q
   | 20 => ((6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                        - s V_expand_right_edge_row)
            + s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
            - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (4 # 5) * max0(s V_expand_right_edge__tmp)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + max0(s V_expand_right_edge_z) <= z)%Q
   | 21 => ((1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                         + s V_expand_right_edge_count)
            - (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_numcols)
            + (6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
            + (3 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
            + (1 # 5) * s V_expand_right_edge_count
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                           + s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge_numcols)
            - (1 # 10) * s V_expand_right_edge_count * max0(-s V_expand_right_edge_count)
            - (1 # 5) * s V_expand_right_edge_numcols
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                             + s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge_count)
            + (1 # 10) * s V_expand_right_edge_numcols * max0(-s V_expand_right_edge_count)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
            + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
            - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_count)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (4 # 5) * max0(s V_expand_right_edge__tmp)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            + (1 # 2) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            - (1 # 2) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
            + max0(s V_expand_right_edge_z) <= z)%Q
   | 22 => hints
     [(*-1.3 0*) F_max0_pre_decrement 1 (s V_expand_right_edge__tmp
                                         - s V_expand_right_edge_row) (1);
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)) (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*-0.05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp)) (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_z) (0))) (F_max0_ge_0 (s V_expand_right_edge_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_z) (0))) (F_max0_ge_0 (s V_expand_right_edge_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_z)) (F_check_ge (s V_expand_right_edge_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_z)) (F_check_ge (s V_expand_right_edge_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)))]
     ((1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                   + s V_expand_right_edge_count)
      - (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                     + s V_expand_right_edge_numcols)
      + (6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                    - s V_expand_right_edge_row)
      + (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
      + (3 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
      + (1 # 5) * s V_expand_right_edge_count
      + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                      + s V_expand_right_edge__tmp)
      - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                     + s V_expand_right_edge__tmp
                                                     - s V_expand_right_edge_row)
      + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                      + s V_expand_right_edge_numcols)
      - (1 # 10) * s V_expand_right_edge_count * max0(-s V_expand_right_edge_count)
      - (1 # 5) * s V_expand_right_edge_numcols
      - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                        + s V_expand_right_edge__tmp)
      + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                       + s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
      - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                        + s V_expand_right_edge_count)
      + (1 # 10) * s V_expand_right_edge_numcols * max0(-s V_expand_right_edge_count)
      - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
      + (2 # 5) * s V_expand_right_edge_row
      - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
      + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
      - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
      - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                + s V_expand_right_edge_count)
      + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                + s V_expand_right_edge_numcols)
      - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                     - s V_expand_right_edge_row)
      + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
      - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                       - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
      + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                 + s V_expand_right_edge_numcols)
      + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
      - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
      - (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
      + (4 # 5) * max0(s V_expand_right_edge__tmp)
      - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
      - (2 # 5) * max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
      + (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_count)
      - (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
      + (1 # 2) * max0(s V_expand_right_edge_count)
      - (1 # 10) * max0(s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
      - (1 # 2) * max0(s V_expand_right_edge_numcols)
      + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
      + max0(s V_expand_right_edge_z) <= z)%Q
   | 23 => ((13 # 10)
            + (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_count)
            - (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_numcols)
            + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
            + (3 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
            + (1 # 5) * s V_expand_right_edge_count
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                           + s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge_numcols)
            - (1 # 10) * s V_expand_right_edge_count * max0(-s V_expand_right_edge_count)
            - (1 # 5) * s V_expand_right_edge_numcols
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                             + s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge_count)
            + (1 # 10) * s V_expand_right_edge_numcols * max0(-s V_expand_right_edge_count)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
            + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
            - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_count)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
            + (13 # 10) * max0(-1 + s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (4 # 5) * max0(s V_expand_right_edge__tmp)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            - (1 # 20) * max0(s V_expand_right_edge__tmp)^2
            - (13 # 10) * max0(s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            - (2 # 5) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            - (1 # 2) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_z)
            + (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            + (1 # 2) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            - (1 # 2) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
            + (1 # 2) * max0(s V_expand_right_edge_z) <= z)%Q
   | 24 => hints
     [(*0 0.05*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp));
      (*-0.4 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_expand_right_edge__tmp
                                                     - s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0));
      (*-0.8 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge__tmp
                                                                    + s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.4 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge__tmp
                                                                    + s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_numcols)) (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.4 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge__tmp
                                                                    + s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_z) (0))) (F_max0_ge_0 (s V_expand_right_edge_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*0 0.7*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp))]
     ((13 # 10)
      + (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                     + s V_expand_right_edge_count)
      - (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                     + s V_expand_right_edge_numcols)
      + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
      + (6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                    - s V_expand_right_edge_row)
      + (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
      + (3 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
      + (1 # 5) * s V_expand_right_edge_count
      + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                      + s V_expand_right_edge__tmp)
      - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                     + s V_expand_right_edge__tmp
                                                     - s V_expand_right_edge_row)
      + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                      + s V_expand_right_edge_numcols)
      - (1 # 10) * s V_expand_right_edge_count * max0(-s V_expand_right_edge_count)
      - (1 # 5) * s V_expand_right_edge_numcols
      - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                        + s V_expand_right_edge__tmp)
      + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                       + s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
      - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                        + s V_expand_right_edge_count)
      + (1 # 10) * s V_expand_right_edge_numcols * max0(-s V_expand_right_edge_count)
      - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
      + (2 # 5) * s V_expand_right_edge_row
      - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
      + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
      - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
      - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
      - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                 + s V_expand_right_edge__tmp)
      - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                 + s V_expand_right_edge__tmp
                                                 - s V_expand_right_edge_row)
      + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
      + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp
                                                 - s V_expand_right_edge_row)
      - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                + s V_expand_right_edge_count)
      + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                + s V_expand_right_edge_numcols)
      - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                     - s V_expand_right_edge_row)
      + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
      + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
      + (13 # 10) * max0(-1 + s V_expand_right_edge__tmp
                         - s V_expand_right_edge_row)
      - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                       - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
      + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                 + s V_expand_right_edge_numcols)
      + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
      - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
      - (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
      + (4 # 5) * max0(s V_expand_right_edge__tmp)
      - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
      - (1 # 20) * max0(s V_expand_right_edge__tmp)^2
      - (13 # 10) * max0(s V_expand_right_edge__tmp
                         - s V_expand_right_edge_row)
      - (2 # 5) * max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
      - (1 # 2) * max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_z)
      + (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_count)
      - (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
      + (1 # 2) * max0(s V_expand_right_edge_count)
      - (1 # 10) * max0(s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
      - (1 # 2) * max0(s V_expand_right_edge_numcols)
      + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
      + (1 # 2) * max0(s V_expand_right_edge_z) <= z)%Q
   | 25 => ((13 # 10) - (21 # 20) * s V_expand_right_edge__tmp
            - (4 # 5) * s V_expand_right_edge__tmp * s V_expand_right_edge_row
            - (1 # 5) * s V_expand_right_edge__tmp * max0(-1
                                                          + s V_expand_right_edge__tmp)
            + (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_count)
            + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
            + (7 # 20) * s V_expand_right_edge__tmp^2
            + (1 # 5) * s V_expand_right_edge_count
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                           + s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge_numcols)
            - (1 # 10) * s V_expand_right_edge_count * max0(-s V_expand_right_edge_count)
            - (1 # 5) * s V_expand_right_edge_numcols
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                             + s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge_count)
            + (1 # 10) * s V_expand_right_edge_numcols * max0(-s V_expand_right_edge_count)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (4 # 5) * s V_expand_right_edge_row
            + (1 # 5) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge__tmp)
            - (1 # 10) * s V_expand_right_edge_row * max0(-1
                                                          + s V_expand_right_edge_numcols)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
            + (11 # 10) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
            + (2 # 5) * s V_expand_right_edge_row^2
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_count)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
            + (13 # 10) * max0(-1 + s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
            + (29 # 20) * max0(s V_expand_right_edge__tmp)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            + (1 # 2) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
            + (1 # 2) * max0(s V_expand_right_edge_z) <= z)%Q
   | 26 => ((13 # 10) - (21 # 20) * s V_expand_right_edge__tmp
            - (4 # 5) * s V_expand_right_edge__tmp * s V_expand_right_edge_row
            - (1 # 5) * s V_expand_right_edge__tmp * max0(-1
                                                          + s V_expand_right_edge__tmp)
            + (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_count)
            + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
            + (7 # 20) * s V_expand_right_edge__tmp^2
            + (1 # 5) * s V_expand_right_edge_count
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                           + s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge_numcols)
            - (1 # 10) * s V_expand_right_edge_count * max0(-s V_expand_right_edge_count)
            - (1 # 5) * s V_expand_right_edge_numcols
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                             + s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge_count)
            + (1 # 10) * s V_expand_right_edge_numcols * max0(-s V_expand_right_edge_count)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (4 # 5) * s V_expand_right_edge_row
            + (1 # 5) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge__tmp)
            - (1 # 10) * s V_expand_right_edge_row * max0(-1
                                                          + s V_expand_right_edge_numcols)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
            + (11 # 10) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
            + (2 # 5) * s V_expand_right_edge_row^2
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_count)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
            + (13 # 10) * max0(-1 + s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
            + (29 # 20) * max0(s V_expand_right_edge__tmp)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            + (1 # 2) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
            + (1 # 2) * max0(s V_expand_right_edge_z) <= z)%Q
   | 27 => ((9 # 10) - (1 # 4) * s V_expand_right_edge__tmp
            - (4 # 5) * s V_expand_right_edge__tmp * s V_expand_right_edge_row
            - (1 # 5) * s V_expand_right_edge__tmp * max0(-1
                                                          + s V_expand_right_edge__tmp)
            + (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_count)
            + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
            + (7 # 20) * s V_expand_right_edge__tmp^2
            + (1 # 5) * s V_expand_right_edge_count
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge__tmp)
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge_numcols)
            - (1 # 2) * s V_expand_right_edge_count * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge_count * max0(-s V_expand_right_edge_count)
            - (1 # 5) * s V_expand_right_edge_numcols
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge__tmp)
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge_count)
            - s V_expand_right_edge_numcols * max0(-1
                                                   + s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_numcols * max0(s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)
            + (1 # 10) * s V_expand_right_edge_numcols * max0(-s V_expand_right_edge_count)
            + (1 # 5) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge__tmp)
            - (1 # 10) * s V_expand_right_edge_row * max0(-1
                                                          + s V_expand_right_edge_numcols)
            - (2 # 5) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge_row)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
            + (11 # 10) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
            + (2 # 5) * s V_expand_right_edge_row^2
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_count)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + 
                                                                    s V_expand_right_edge_row)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
            + (2 # 5) * max0(-1 + s V_expand_right_edge_row)
            + (29 # 20) * max0(s V_expand_right_edge__tmp)
            - (1 # 5) * max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            + (13 # 10) * max0(s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            + (1 # 2) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            + max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            - (11 # 10) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
            + max0(s V_expand_right_edge_z) <= z)%Q
   | 28 => ((9 # 10) - (1 # 4) * s V_expand_right_edge__tmp
            - (4 # 5) * s V_expand_right_edge__tmp * s V_expand_right_edge_row
            - (1 # 5) * s V_expand_right_edge__tmp * max0(-1
                                                          + s V_expand_right_edge__tmp)
            + (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_count)
            + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
            + (7 # 20) * s V_expand_right_edge__tmp^2
            + (1 # 5) * s V_expand_right_edge_count
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge__tmp)
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge_numcols)
            - (1 # 2) * s V_expand_right_edge_count * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge_count * max0(-s V_expand_right_edge_count)
            - (1 # 5) * s V_expand_right_edge_numcols
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge__tmp)
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge_count)
            - s V_expand_right_edge_numcols * max0(-1
                                                   + s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_numcols * max0(s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)
            + (1 # 10) * s V_expand_right_edge_numcols * max0(-s V_expand_right_edge_count)
            + (1 # 5) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge__tmp)
            - (1 # 10) * s V_expand_right_edge_row * max0(-1
                                                          + s V_expand_right_edge_numcols)
            - (2 # 5) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge_row)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
            + (11 # 10) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
            + (2 # 5) * s V_expand_right_edge_row^2
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_count)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + 
                                                                    s V_expand_right_edge_row)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
            + (2 # 5) * max0(-1 + s V_expand_right_edge_row)
            + (29 # 20) * max0(s V_expand_right_edge__tmp)
            - (1 # 5) * max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            + (13 # 10) * max0(s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            + (1 # 2) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            + max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            - (11 # 10) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
            + max0(s V_expand_right_edge_z) <= z)%Q
   | 29 => ((9 # 10) - (1 # 4) * s V_expand_right_edge__tmp
            - (4 # 5) * s V_expand_right_edge__tmp * s V_expand_right_edge_row
            - (1 # 5) * s V_expand_right_edge__tmp * max0(-1
                                                          + s V_expand_right_edge__tmp)
            + (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_count)
            + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
            + (7 # 20) * s V_expand_right_edge__tmp^2
            + (1 # 5) * s V_expand_right_edge_count
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge__tmp)
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge_numcols)
            - (1 # 2) * s V_expand_right_edge_count * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge_count * max0(-s V_expand_right_edge_count)
            - (1 # 5) * s V_expand_right_edge_numcols
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge__tmp)
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge_count)
            - s V_expand_right_edge_numcols * max0(-1
                                                   + s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_numcols * max0(s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)
            + (1 # 10) * s V_expand_right_edge_numcols * max0(-s V_expand_right_edge_count)
            + (1 # 5) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge__tmp)
            - (1 # 10) * s V_expand_right_edge_row * max0(-1
                                                          + s V_expand_right_edge_numcols)
            - (2 # 5) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge_row)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
            + (11 # 10) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
            + (2 # 5) * s V_expand_right_edge_row^2
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_count)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + 
                                                                    s V_expand_right_edge_row)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
            + (2 # 5) * max0(-1 + s V_expand_right_edge_row)
            + (29 # 20) * max0(s V_expand_right_edge__tmp)
            - (1 # 5) * max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            + (13 # 10) * max0(s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            + (1 # 2) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            + max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
            - (11 # 10) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
            + max0(s V_expand_right_edge_z) <= z)%Q
   | 30 => hints
     [(*0 1.2*) F_max0_monotonic (F_check_ge (s V_expand_right_edge_count) (-1
                                                                    + s V_expand_right_edge_count));
      (*-1.2 0*) F_max0_ge_0 (-1 + s V_expand_right_edge_count);
      (*-0.05 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                      + s V_expand_right_edge__tmp)) (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0));
      (*-0.1 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                 + s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols));
      (*-0.4 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row));
      (*-0.1 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_expand_right_edge_numcols)) (F_check_ge (s V_expand_right_edge_numcols) (0));
      (*-0.05 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.6 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge__tmp)) (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge_numcols)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge_numcols)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge_row) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge_row) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge_row) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                             + s V_expand_right_edge_row)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*0 0.2*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                               + s V_expand_right_edge_row)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*0 0.4*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                               + s V_expand_right_edge_row)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge_z) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge_z)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge_z)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp)) (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp)) (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge__tmp
                                                                    + s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*0 0.1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp)) (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*0 0.1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp)) (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp)) (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*-0.4 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge__tmp
                                                                    + s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*-0.4 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_expand_right_edge_count) (0))) (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_count))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_expand_right_edge_count) (0))) (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_count))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_expand_right_edge_count) (0))) (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_count))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_expand_right_edge_count)) (F_check_ge (-
                                                                    s V_expand_right_edge_count) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-s V_expand_right_edge_count)) (F_check_ge (-
                                                                    s V_expand_right_edge_count) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_expand_right_edge_count)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_expand_right_edge_count)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_count) (0))) (F_max0_ge_0 (s V_expand_right_edge_count))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_count) (0))) (F_max0_ge_0 (s V_expand_right_edge_count))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_count)) (F_check_ge (s V_expand_right_edge_count) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.9 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_numcols)) (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge__tmp
                                                                    + s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.9 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_z) (0))) (F_max0_ge_0 (s V_expand_right_edge_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_z) (0))) (F_max0_ge_0 (s V_expand_right_edge_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_z)) (F_check_ge (s V_expand_right_edge_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_z)) (F_check_ge (s V_expand_right_edge_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.2 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_count) (0))) (F_max0_ge_0 (s V_expand_right_edge_count));
      (*-0.2 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_expand_right_edge_count)) (F_check_ge (0) (0));
      (*-0.8 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                     + s V_expand_right_edge_row)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_row) (0))]
     ((9 # 10) - (1 # 4) * s V_expand_right_edge__tmp
      - (4 # 5) * s V_expand_right_edge__tmp * s V_expand_right_edge_row
      - (1 # 5) * s V_expand_right_edge__tmp * max0(-1
                                                    + s V_expand_right_edge__tmp)
      + (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                     + s V_expand_right_edge_count)
      + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
      + (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
      + (7 # 20) * s V_expand_right_edge__tmp^2
      + (1 # 5) * s V_expand_right_edge_count
      + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                      + s V_expand_right_edge__tmp)
      + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                      + s V_expand_right_edge_numcols)
      - (1 # 2) * s V_expand_right_edge_count * max0(s V_expand_right_edge__tmp
                                                     - s V_expand_right_edge_row)
      - (1 # 10) * s V_expand_right_edge_count * max0(-s V_expand_right_edge_count)
      - (1 # 5) * s V_expand_right_edge_numcols
      - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                        + s V_expand_right_edge__tmp)
      - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                        + s V_expand_right_edge_count)
      - s V_expand_right_edge_numcols * max0(-1 + s V_expand_right_edge_row)
      + (1 # 2) * s V_expand_right_edge_numcols * max0(s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
      + (1 # 10) * s V_expand_right_edge_numcols * max0(-s V_expand_right_edge_count)
      + (1 # 5) * s V_expand_right_edge_row * max0(-1
                                                   + s V_expand_right_edge__tmp)
      - (1 # 10) * s V_expand_right_edge_row * max0(-1
                                                    + s V_expand_right_edge_numcols)
      - (2 # 5) * s V_expand_right_edge_row * max0(-1
                                                   + s V_expand_right_edge_row)
      - (1 # 2) * s V_expand_right_edge_row * max0(-1
                                                   + s V_expand_right_edge_z)
      - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
      + (11 # 10) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
      + (2 # 5) * s V_expand_right_edge_row^2
      - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                 + s V_expand_right_edge__tmp)
      + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
      - (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp
                                                 - s V_expand_right_edge_row)
      + (3 # 10) * max0(-1 + s V_expand_right_edge__tmp)
      - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                + s V_expand_right_edge_count)
      + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                + s V_expand_right_edge_numcols)
      + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                               + s V_expand_right_edge_row)
      + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                               + s V_expand_right_edge_z)
      + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                 + s V_expand_right_edge_numcols)
      + (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)
      + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
      - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
      + (2 # 5) * max0(-1 + s V_expand_right_edge_row)
      + max0(-1 + s V_expand_right_edge_z)
      + (19 # 20) * max0(s V_expand_right_edge__tmp)
      - (1 # 5) * max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
      - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
      + (9 # 5) * max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row)
      + (1 # 2) * max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_numcols)
      + (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_count)
      - (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
      + max0(s V_expand_right_edge_count)
      - (1 # 10) * max0(s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
      - (11 # 10) * max0(s V_expand_right_edge_numcols)
      + (1 # 10) * max0(s V_expand_right_edge_numcols)^2 <= z)%Q
   | 31 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)) (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.9 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge_count) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_count))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                               + s V_expand_right_edge_count)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_count) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.9 -4.51988e-12*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_count) (0))) (F_max0_ge_0 (s V_expand_right_edge_count))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_count) (0))) (F_max0_ge_0 (s V_expand_right_edge_count))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_count) (0))) (F_max0_ge_0 (s V_expand_right_edge_count))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.9 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_count)) (F_check_ge (s V_expand_right_edge_count) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_count)) (F_check_ge (s V_expand_right_edge_count) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*0 0.1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_z) (0))) (F_max0_ge_0 (s V_expand_right_edge_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_z)) (F_check_ge (s V_expand_right_edge_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0)))]
     ((13 # 10)
      + (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                     + s V_expand_right_edge_count)
      - (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                     + s V_expand_right_edge_numcols)
      + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
      + (6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                    - s V_expand_right_edge_row)
      + (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
      + (3 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
      + (1 # 5) * s V_expand_right_edge_count
      + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                      + s V_expand_right_edge__tmp)
      - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                     + s V_expand_right_edge__tmp
                                                     - s V_expand_right_edge_row)
      + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                      + s V_expand_right_edge_numcols)
      - (1 # 10) * s V_expand_right_edge_count * max0(-s V_expand_right_edge_count)
      - (1 # 5) * s V_expand_right_edge_numcols
      - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                        + s V_expand_right_edge__tmp)
      + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                       + s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
      - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                        + s V_expand_right_edge_count)
      + (1 # 10) * s V_expand_right_edge_numcols * max0(-s V_expand_right_edge_count)
      - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
      + (2 # 5) * s V_expand_right_edge_row
      - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
      + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
      - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
      - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
      - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                 + s V_expand_right_edge__tmp)
      - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                 + s V_expand_right_edge__tmp
                                                 - s V_expand_right_edge_row)
      + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
      + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp
                                                 - s V_expand_right_edge_row)
      - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                + s V_expand_right_edge_count)
      + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                + s V_expand_right_edge_numcols)
      - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                     - s V_expand_right_edge_row)
      + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
      + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
      + (13 # 10) * max0(-1 + s V_expand_right_edge__tmp
                         - s V_expand_right_edge_row)
      - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                       - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
      + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                 + s V_expand_right_edge_numcols)
      + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
      - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
      - (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
      + (4 # 5) * max0(s V_expand_right_edge__tmp)
      - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
      - (1 # 20) * max0(s V_expand_right_edge__tmp)^2
      - (13 # 10) * max0(s V_expand_right_edge__tmp
                         - s V_expand_right_edge_row)
      - (2 # 5) * max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
      - (1 # 2) * max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_z)
      + (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_count)
      - (1 # 10) * max0(-s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
      + (1 # 2) * max0(s V_expand_right_edge_count)
      - (1 # 10) * max0(s V_expand_right_edge_count) * max0(s V_expand_right_edge_numcols)
      - (1 # 2) * max0(s V_expand_right_edge_numcols)
      + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
      + (1 # 2) * max0(s V_expand_right_edge_z) <= z)%Q
   | 32 => ((13 # 10)
            - (1 # 2) * s V_expand_right_edge__tmp * max0(-1
                                                          + s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            - (2 # 5) * s V_expand_right_edge__tmp * max0(-1
                                                          + s V_expand_right_edge_count)
            - (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_numcols)
            + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            + (3 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
            + (1 # 5) * s V_expand_right_edge_count
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                           + s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge_numcols)
            - (1 # 10) * s V_expand_right_edge_count * max0(s V_expand_right_edge_numcols)
            - s V_expand_right_edge_count * max0(s V_expand_right_edge_row)
            - (1 # 5) * s V_expand_right_edge_numcols
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                             + s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge_count)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            + (1 # 2) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge__tmp
                                                         - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge_count)
            + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge_z)
            - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_count)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
            + (13 # 10) * max0(-1 + s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row)^2
            + (1 # 2) * max0(-1 + s V_expand_right_edge_count)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + (9 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(s V_expand_right_edge__tmp)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (17 # 10) * max0(s V_expand_right_edge__tmp)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            - (1 # 20) * max0(s V_expand_right_edge__tmp)^2
            - (13 # 10) * max0(s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            - (2 # 5) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            - (1 # 2) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
            + (1 # 2) * max0(s V_expand_right_edge_z)
            - (1 # 2) * max0(s V_expand_right_edge_z)^2 <= z)%Q
   | 33 => ((13 # 10)
            - (1 # 2) * s V_expand_right_edge__tmp * max0(-1
                                                          + s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            - (2 # 5) * s V_expand_right_edge__tmp * max0(-1
                                                          + s V_expand_right_edge_count)
            - (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_numcols)
            + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            + (3 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
            + (1 # 5) * s V_expand_right_edge_count
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                           + s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge_numcols)
            - (1 # 10) * s V_expand_right_edge_count * max0(s V_expand_right_edge_numcols)
            - s V_expand_right_edge_count * max0(s V_expand_right_edge_row)
            - (1 # 5) * s V_expand_right_edge_numcols
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                             + s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge_count)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            + (1 # 2) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge__tmp
                                                         - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge_count)
            + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge_z)
            - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_count)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
            + (13 # 10) * max0(-1 + s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row)^2
            + (1 # 2) * max0(-1 + s V_expand_right_edge_count)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            + (9 # 10) * max0(-1 + s V_expand_right_edge_count) * max0(s V_expand_right_edge__tmp)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (17 # 10) * max0(s V_expand_right_edge__tmp)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            - (1 # 20) * max0(s V_expand_right_edge__tmp)^2
            - (13 # 10) * max0(s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            - (2 # 5) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            - (1 # 2) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
            + (1 # 2) * max0(s V_expand_right_edge_z)
            - (1 # 2) * max0(s V_expand_right_edge_z)^2 <= z)%Q
   | 34 => ((3 # 2)
            - (1 # 2) * s V_expand_right_edge__tmp * max0(-1
                                                          + s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_numcols)
            + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            - (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
            + (3 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
            + (1 # 5) * s V_expand_right_edge_count
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                           + s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge_numcols)
            - (1 # 10) * s V_expand_right_edge_count * max0(s V_expand_right_edge_numcols)
            - s V_expand_right_edge_count * max0(s V_expand_right_edge_row)
            - (1 # 5) * s V_expand_right_edge_numcols
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                             + s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge_numcols * max0(s V_expand_right_edge_count)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            + (1 # 2) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge__tmp
                                                         - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
            + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge_z)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_count)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
            + (4 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row)^2
            + (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (17 # 10) * max0(s V_expand_right_edge__tmp)
            + (9 # 10) * max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_count)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            - (1 # 20) * max0(s V_expand_right_edge__tmp)^2
            - (13 # 10) * max0(s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            - (2 # 5) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(s V_expand_right_edge_count)
            - (3 # 5) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
            - max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(s V_expand_right_edge_z)
            - (1 # 2) * max0(s V_expand_right_edge_z)^2 <= z)%Q
   | 35 => ((3 # 2)
            - (1 # 2) * s V_expand_right_edge__tmp * max0(-1
                                                          + s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_numcols)
            + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            - (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
            + (3 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
            + (1 # 5) * s V_expand_right_edge_count
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                           + s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge_numcols)
            - (1 # 10) * s V_expand_right_edge_count * max0(s V_expand_right_edge_numcols)
            - s V_expand_right_edge_count * max0(s V_expand_right_edge_row)
            - (1 # 5) * s V_expand_right_edge_numcols
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                             + s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge_numcols * max0(s V_expand_right_edge_count)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            + (1 # 2) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge__tmp
                                                         - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
            + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge_z)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_count)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
            + (4 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row)^2
            + (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (17 # 10) * max0(s V_expand_right_edge__tmp)
            + (9 # 10) * max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_count)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            - (1 # 20) * max0(s V_expand_right_edge__tmp)^2
            - (13 # 10) * max0(s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            - (2 # 5) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(s V_expand_right_edge_count)
            - (3 # 5) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
            - max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(s V_expand_right_edge_z)
            - (1 # 2) * max0(s V_expand_right_edge_z)^2 <= z)%Q
   | 36 => ((3 # 2)
            - (1 # 2) * s V_expand_right_edge__tmp * max0(-1
                                                          + s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                           + s V_expand_right_edge_numcols)
            + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
            + (6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                          - s V_expand_right_edge_row)
            - (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
            + (3 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
            + (1 # 5) * s V_expand_right_edge_count
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                           + s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                            + s V_expand_right_edge_numcols)
            - (1 # 10) * s V_expand_right_edge_count * max0(s V_expand_right_edge_numcols)
            - s V_expand_right_edge_count * max0(s V_expand_right_edge_row)
            - (1 # 5) * s V_expand_right_edge_numcols
            - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                              + s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                             + s V_expand_right_edge__tmp
                                                             - s V_expand_right_edge_row)
            - (1 # 10) * s V_expand_right_edge_numcols * max0(s V_expand_right_edge_count)
            - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
            + (2 # 5) * s V_expand_right_edge_row
            + (1 # 2) * s V_expand_right_edge_row * max0(-1
                                                         + s V_expand_right_edge__tmp
                                                         - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
            + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
            - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
            - (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_z)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp)
            - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                       + s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge_z)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp)
            + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                    + s V_expand_right_edge_numcols)
            - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
            - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_count)
            + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_z)
            + (4 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row)
            - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row)^2
            + (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)
            + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
            + (1 # 10) * max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge_count)
            - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
            - (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row)
            + (1 # 5) * max0(-s V_expand_right_edge__tmp
                             + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (17 # 10) * max0(s V_expand_right_edge__tmp)
            + (9 # 10) * max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_count)
            - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
            - (1 # 20) * max0(s V_expand_right_edge__tmp)^2
            - (13 # 10) * max0(s V_expand_right_edge__tmp
                               - s V_expand_right_edge_row)
            - (2 # 5) * max0(s V_expand_right_edge__tmp
                             - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(s V_expand_right_edge_count)
            - (3 # 5) * max0(s V_expand_right_edge_numcols)
            + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
            - max0(s V_expand_right_edge_row)
            + (1 # 2) * max0(s V_expand_right_edge_z)
            - (1 # 2) * max0(s V_expand_right_edge_z)^2 <= z)%Q
   | 37 => hints
     [(*-0.3 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                 + s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                     + s V_expand_right_edge__tmp
                                                     - s V_expand_right_edge_row)) (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0));
      (*-0.3 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                 + s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols));
      (*-0.3 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_expand_right_edge__tmp)) (F_check_ge (s V_expand_right_edge__tmp) (0));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp
                                                                 - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row));
      (*-0.3 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_expand_right_edge_numcols)) (F_check_ge (s V_expand_right_edge_numcols) (0));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge__tmp)) (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge__tmp)) (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge__tmp)) (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge__tmp)) (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)) (F_check_ge (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge_numcols)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge_numcols)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge_numcols)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_expand_right_edge_numcols)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_expand_right_edge_z) (0))) (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                               + s V_expand_right_edge_z)) (F_check_ge (-1
                                                                    + s V_expand_right_edge_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp)) (F_check_ge (0) (0)));
      (*-0.9 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp)) (F_check_ge (s V_expand_right_edge__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp
                                                                - s V_expand_right_edge_row)) (F_check_ge (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_z)) (F_check_ge (0) (0)));
      (*0 0.1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_count) (0))) (F_max0_ge_0 (s V_expand_right_edge_count))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_count)) (F_check_ge (s V_expand_right_edge_count) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_count)) (F_check_ge (s V_expand_right_edge_count) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.3 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (s V_expand_right_edge_numcols))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_numcols)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_numcols)) (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge__tmp
                                                                    - s V_expand_right_edge_row)) (F_check_ge (0) (0)));
      (*-0.1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge_numcols)) (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_expand_right_edge_count)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_row) (0))) (F_max0_ge_0 (s V_expand_right_edge_row));
      (*-0.6 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_expand_right_edge_numcols) (0))) (F_max0_ge_0 (s V_expand_right_edge_numcols));
      (*-0.4 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_expand_right_edge__tmp)) (F_check_ge (s V_expand_right_edge__tmp) (0))]
     ((3 # 2)
      - (1 # 2) * s V_expand_right_edge__tmp * max0(-1
                                                    + s V_expand_right_edge__tmp
                                                    - s V_expand_right_edge_row)
      - (1 # 10) * s V_expand_right_edge__tmp * max0(-1
                                                     + s V_expand_right_edge_numcols)
      + (1 # 20) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp)
      + (6 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge__tmp
                                                    - s V_expand_right_edge_row)
      - (2 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_count)
      + (3 # 5) * s V_expand_right_edge__tmp * max0(s V_expand_right_edge_numcols)
      + (1 # 5) * s V_expand_right_edge_count
      + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                      + s V_expand_right_edge__tmp)
      - (1 # 2) * s V_expand_right_edge_count * max0(-1
                                                     + s V_expand_right_edge__tmp
                                                     - s V_expand_right_edge_row)
      + (1 # 10) * s V_expand_right_edge_count * max0(-1
                                                      + s V_expand_right_edge_numcols)
      - (1 # 10) * s V_expand_right_edge_count * max0(s V_expand_right_edge_numcols)
      - s V_expand_right_edge_count * max0(s V_expand_right_edge_row)
      - (1 # 5) * s V_expand_right_edge_numcols
      - (1 # 10) * s V_expand_right_edge_numcols * max0(-1
                                                        + s V_expand_right_edge__tmp)
      + (1 # 2) * s V_expand_right_edge_numcols * max0(-1
                                                       + s V_expand_right_edge__tmp
                                                       - s V_expand_right_edge_row)
      - (1 # 10) * s V_expand_right_edge_numcols * max0(s V_expand_right_edge_count)
      - s V_expand_right_edge_numcols * max0(s V_expand_right_edge_row)
      + (2 # 5) * s V_expand_right_edge_row
      + (1 # 2) * s V_expand_right_edge_row * max0(-1
                                                   + s V_expand_right_edge__tmp
                                                   - s V_expand_right_edge_row)
      - (1 # 2) * s V_expand_right_edge_row * max0(-1
                                                   + s V_expand_right_edge_z)
      + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_count)
      + (1 # 2) * s V_expand_right_edge_row * max0(s V_expand_right_edge_numcols)
      - (2 # 5) * s V_expand_right_edge_row * max0(s V_expand_right_edge_row)
      - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                 + s V_expand_right_edge__tmp)
      - (1 # 2) * s V_expand_right_edge_z * max0(-1
                                                 + s V_expand_right_edge__tmp
                                                 - s V_expand_right_edge_row)
      + (1 # 2) * s V_expand_right_edge_z * max0(-1 + s V_expand_right_edge_z)
      + (1 # 2) * s V_expand_right_edge_z * max0(s V_expand_right_edge__tmp)
      + (3 # 5) * max0(-1 + s V_expand_right_edge__tmp)
      + (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                                + s V_expand_right_edge_numcols)
      + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp) * max0(-1
                                                               + s V_expand_right_edge_z)
      - max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge__tmp
                                                     - s V_expand_right_edge_row)
      - (1 # 10) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_count)
      + (2 # 5) * max0(-1 + s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_row)
      + (13 # 10) * max0(-1 + s V_expand_right_edge__tmp
                         - s V_expand_right_edge_row)
      - (1 # 5) * max0(-1 + s V_expand_right_edge__tmp
                       - s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
      + (1 # 2) * max0(-1 + s V_expand_right_edge__tmp
                       - s V_expand_right_edge_row)^2
      + (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)
      + max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge__tmp)
      + (1 # 10) * max0(-1 + s V_expand_right_edge_numcols) * max0(s V_expand_right_edge_count)
      - (1 # 10) * max0(-1 + s V_expand_right_edge_numcols)^2
      - (1 # 2) * max0(-1 + s V_expand_right_edge_z)^2
      - (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge__tmp
                                                           - s V_expand_right_edge_row)
      + (1 # 5) * max0(-s V_expand_right_edge__tmp
                       + s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
      + (6 # 5) * max0(s V_expand_right_edge__tmp)
      + (9 # 10) * max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_count)
      - max0(s V_expand_right_edge__tmp) * max0(s V_expand_right_edge_numcols)
      - (1 # 20) * max0(s V_expand_right_edge__tmp)^2
      - (13 # 10) * max0(s V_expand_right_edge__tmp
                         - s V_expand_right_edge_row)
      - (2 # 5) * max0(s V_expand_right_edge__tmp - s V_expand_right_edge_row) * max0(s V_expand_right_edge_row)
      + (1 # 2) * max0(s V_expand_right_edge_count)
      - (3 # 5) * max0(s V_expand_right_edge_numcols)
      + (1 # 10) * max0(s V_expand_right_edge_numcols)^2
      - max0(s V_expand_right_edge_row) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_expand_right_edge =>
    [mkPA Q (fun n z s => ai_expand_right_edge n s /\ annot0_expand_right_edge n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_expand_right_edge (proc_start P_expand_right_edge) s1 (proc_end P_expand_right_edge) s2 ->
    (s2 V_expand_right_edge_z <= max0(-1 - s1 V_expand_right_edge_input_cols
                                      + s1 V_expand_right_edge_output_cols) * max0(s1 V_expand_right_edge_num_rows)
                                 + (2 # 1) * max0(s1 V_expand_right_edge_num_rows))%Q.
Proof.
  prove_bound ipa admissible_ipa P_expand_right_edge.
Qed.
