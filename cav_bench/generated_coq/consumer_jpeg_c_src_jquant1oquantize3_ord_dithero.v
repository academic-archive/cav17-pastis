Require Import pasta.Pasta.

Inductive proc: Type :=
  P_quantize3_ord_dither.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_quantize3_ord_dither_z := 1%positive.
Notation V_quantize3_ord_dither__tmp := 2%positive.
Notation V_quantize3_ord_dither_cinfo_dref_off128 := 3%positive.
Notation V_quantize3_ord_dither_col := 4%positive.
Notation V_quantize3_ord_dither_col_index := 5%positive.
Notation V_quantize3_ord_dither_pixcode := 6%positive.
Notation V_quantize3_ord_dither_row := 7%positive.
Notation V_quantize3_ord_dither_row_index := 8%positive.
Notation V_quantize3_ord_dither_width := 9%positive.
Notation V_quantize3_ord_dither_cinfo := 10%positive.
Notation V_quantize3_ord_dither_input_buf := 11%positive.
Notation V_quantize3_ord_dither_num_rows := 12%positive.
Notation V_quantize3_ord_dither_output_buf := 13%positive.
Definition Pedges_quantize3_ord_dither: list (edge proc) :=
  (EA 1 (AAssign V_quantize3_ord_dither_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_quantize3_ord_dither_col) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign
  V_quantize3_ord_dither__tmp
  (Some (EVar V_quantize3_ord_dither_num_rows))) 5)::(EA 5 (AAssign
  V_quantize3_ord_dither_width
  (Some (EVar V_quantize3_ord_dither_cinfo_dref_off128))) 6)::(EA 6 (AAssign
  V_quantize3_ord_dither_row (Some (ENum (0)))) 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_quantize3_ord_dither_row) s) <
  (eval (EVar V_quantize3_ord_dither__tmp) s))%Z)) 12)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_quantize3_ord_dither_row) s) >=
  (eval (EVar V_quantize3_ord_dither__tmp) s))%Z)) 10)::(EA 10 AWeaken 11)::
  (EA 12 AWeaken 13)::(EA 13 (AAssign V_quantize3_ord_dither_row_index
  None) 14)::(EA 14 (AAssign V_quantize3_ord_dither_col_index
  (Some (ENum (0)))) 15)::(EA 15 (AAssign V_quantize3_ord_dither_col
  (Some (EVar V_quantize3_ord_dither_width))) 16)::(EA 16 ANone 17)::
  (EA 17 AWeaken 18)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_quantize3_ord_dither_col) s) > (eval (ENum (0))
  s))%Z)) 27)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_quantize3_ord_dither_col) s) <= (eval (ENum (0))
  s))%Z)) 19)::(EA 19 AWeaken 20)::(EA 20 (AAssign
  V_quantize3_ord_dither_row_index None) 21)::(EA 21 ANone 22)::
  (EA 22 (AAssign V_quantize3_ord_dither_row
  (Some (EAdd (EVar V_quantize3_ord_dither_row) (ENum (1))))) 23)::
  (EA 23 ANone 24)::(EA 24 ANone 25)::(EA 25 (AAssign
  V_quantize3_ord_dither_z (Some (EAdd (ENum (1))
  (EVar V_quantize3_ord_dither_z)))) 26)::(EA 26 AWeaken 9)::
  (EA 27 AWeaken 28)::(EA 28 (AAssign V_quantize3_ord_dither_pixcode
  None) 29)::(EA 29 (AAssign V_quantize3_ord_dither_pixcode None) 30)::
  (EA 30 (AAssign V_quantize3_ord_dither_pixcode None) 31)::(EA 31 (AAssign
  V_quantize3_ord_dither_col_index None) 32)::(EA 32 ANone 33)::
  (EA 33 (AAssign V_quantize3_ord_dither_col
  (Some (EAdd (EVar V_quantize3_ord_dither_col) (ENum (-1))))) 34)::
  (EA 34 ANone 35)::(EA 35 ANone 36)::(EA 36 (AAssign
  V_quantize3_ord_dither_z (Some (EAdd (ENum (1))
  (EVar V_quantize3_ord_dither_z)))) 37)::(EA 37 AWeaken 18)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_quantize3_ord_dither => Pedges_quantize3_ord_dither
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_quantize3_ord_dither => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_quantize3_ord_dither (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0)%Z
   | 3 => (-1 * s V_quantize3_ord_dither_z <= 0 /\ 1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_col <= 0)%Z
   | 4 => (-1 * s V_quantize3_ord_dither_col <= 0 /\ 1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0)%Z
   | 5 => (-1 * s V_quantize3_ord_dither_z <= 0 /\ 1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_col <= 0)%Z
   | 6 => (-1 * s V_quantize3_ord_dither_col <= 0 /\ 1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0)%Z
   | 7 => (-1 * s V_quantize3_ord_dither_z <= 0 /\ 1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_col <= 0 /\ 1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0)%Z
   | 8 => (-1 * s V_quantize3_ord_dither_row <= 0 /\ 1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_col <= 0 /\ 1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0)%Z
   | 9 => (-1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0)%Z
   | 10 => (-1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ 1 * s V_quantize3_ord_dither__tmp+ -1 * s V_quantize3_ord_dither_row <= 0)%Z
   | 11 => (1 * s V_quantize3_ord_dither__tmp+ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0)%Z
   | 12 => (-1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0)%Z
   | 13 => (-1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0)%Z
   | 14 => (-1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0)%Z
   | 15 => (-1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ 1 * s V_quantize3_ord_dither_col_index <= 0 /\ -1 * s V_quantize3_ord_dither_col_index <= 0)%Z
   | 16 => (-1 * s V_quantize3_ord_dither_col_index <= 0 /\ 1 * s V_quantize3_ord_dither_col_index <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0)%Z
   | 17 => (-1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ 1 * s V_quantize3_ord_dither_col_index <= 0 /\ -1 * s V_quantize3_ord_dither_col_index <= 0)%Z
   | 18 => (-1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0)%Z
   | 19 => (-1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ 1 * s V_quantize3_ord_dither_col <= 0)%Z
   | 20 => (1 * s V_quantize3_ord_dither_col <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0)%Z
   | 21 => (-1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ 1 * s V_quantize3_ord_dither_col <= 0)%Z
   | 22 => (1 * s V_quantize3_ord_dither_col <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0)%Z
   | 23 => (-1 * s V_quantize3_ord_dither_z <= 0 /\ 1 * s V_quantize3_ord_dither_col <= 0 /\ -1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row <= 0)%Z
   | 24 => (-1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ 1 * s V_quantize3_ord_dither_col <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0)%Z
   | 25 => (-1 * s V_quantize3_ord_dither_z <= 0 /\ 1 * s V_quantize3_ord_dither_col <= 0 /\ -1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row <= 0)%Z
   | 26 => (-1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ 1 * s V_quantize3_ord_dither_col <= 0 /\ -1 * s V_quantize3_ord_dither_z + 1 <= 0)%Z
   | 27 => (-1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_col + 1 <= 0)%Z
   | 28 => (-1 * s V_quantize3_ord_dither_col + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0)%Z
   | 29 => (-1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_col + 1 <= 0)%Z
   | 30 => (-1 * s V_quantize3_ord_dither_col + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0)%Z
   | 31 => (-1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_col + 1 <= 0)%Z
   | 32 => (-1 * s V_quantize3_ord_dither_col + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0)%Z
   | 33 => (-1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_col + 1 <= 0)%Z
   | 34 => (-1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_col <= 0)%Z
   | 35 => (-1 * s V_quantize3_ord_dither_col <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z <= 0)%Z
   | 36 => (-1 * s V_quantize3_ord_dither_z <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_col <= 0)%Z
   | 37 => (-1 * s V_quantize3_ord_dither_col <= 0 /\ -1 * s V_quantize3_ord_dither__tmp+ 1 * s V_quantize3_ord_dither_row + 1 <= 0 /\ -1 * s V_quantize3_ord_dither_row <= 0 /\ -1 * s V_quantize3_ord_dither_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_quantize3_ord_dither (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_quantize3_ord_dither_cinfo_dref_off128) * max0(s V_quantize3_ord_dither_num_rows)
           + max0(s V_quantize3_ord_dither_num_rows) <= z)%Q
   | 2 => (s V_quantize3_ord_dither_z
           + max0(s V_quantize3_ord_dither_cinfo_dref_off128) * max0(s V_quantize3_ord_dither_num_rows)
           + max0(s V_quantize3_ord_dither_num_rows) <= z)%Q
   | 3 => (s V_quantize3_ord_dither_z
           + max0(s V_quantize3_ord_dither_cinfo_dref_off128) * max0(s V_quantize3_ord_dither_num_rows)
           + max0(s V_quantize3_ord_dither_num_rows) <= z)%Q
   | 4 => (s V_quantize3_ord_dither_z
           + max0(s V_quantize3_ord_dither_cinfo_dref_off128) * max0(s V_quantize3_ord_dither_num_rows)
           + max0(s V_quantize3_ord_dither_num_rows) <= z)%Q
   | 5 => (s V_quantize3_ord_dither_z + max0(s V_quantize3_ord_dither__tmp)
           + max0(s V_quantize3_ord_dither__tmp) * max0(s V_quantize3_ord_dither_cinfo_dref_off128) <= z)%Q
   | 6 => (s V_quantize3_ord_dither_z + max0(s V_quantize3_ord_dither__tmp)
           + max0(s V_quantize3_ord_dither__tmp) * max0(s V_quantize3_ord_dither_width) <= z)%Q
   | 7 => (s V_quantize3_ord_dither_z
           + max0(s V_quantize3_ord_dither__tmp
                  - s V_quantize3_ord_dither_row)
           + max0(s V_quantize3_ord_dither__tmp
                  - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width) <= z)%Q
   | 8 => (s V_quantize3_ord_dither_z
           + max0(s V_quantize3_ord_dither__tmp
                  - s V_quantize3_ord_dither_row)
           + max0(s V_quantize3_ord_dither__tmp
                  - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width) <= z)%Q
   | 9 => (s V_quantize3_ord_dither_z
           + max0(s V_quantize3_ord_dither__tmp
                  - s V_quantize3_ord_dither_row)
           + max0(s V_quantize3_ord_dither__tmp
                  - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_quantize3_ord_dither__tmp
                                             - s V_quantize3_ord_dither_row) (-1
                                                                    + s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row));
      (*-1 0*) F_max0_ge_0 (-1 + s V_quantize3_ord_dither__tmp
                            - s V_quantize3_ord_dither_row);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither__tmp
                                                            - s V_quantize3_ord_dither_row)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither_width)) (F_check_ge (0) (0)))]
     (s V_quantize3_ord_dither_z
      + max0(s V_quantize3_ord_dither__tmp - s V_quantize3_ord_dither_row)
      + max0(s V_quantize3_ord_dither__tmp - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width) <= z)%Q
   | 11 => (s V_quantize3_ord_dither_z <= z)%Q
   | 12 => (s V_quantize3_ord_dither_z
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width) <= z)%Q
   | 13 => (s V_quantize3_ord_dither_z
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width) <= z)%Q
   | 14 => (s V_quantize3_ord_dither_z
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width) <= z)%Q
   | 15 => (s V_quantize3_ord_dither_z
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width) <= z)%Q
   | 16 => (s V_quantize3_ord_dither_z
            - max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col)
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col) <= z)%Q
   | 17 => (s V_quantize3_ord_dither_z
            - max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col)
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col) <= z)%Q
   | 18 => (s V_quantize3_ord_dither_z
            - max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col)
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col) <= z)%Q
   | 19 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_quantize3_ord_dither__tmp
                                      - s V_quantize3_ord_dither_row) (1);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quantize3_ord_dither__tmp
                                                              - s V_quantize3_ord_dither_row)) (F_check_ge (s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither_col)) (F_check_ge (0) (0)))]
     (s V_quantize3_ord_dither_z
      - max0(-1 + s V_quantize3_ord_dither__tmp
             - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col)
      + max0(-1 + s V_quantize3_ord_dither__tmp
             - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
      + max0(s V_quantize3_ord_dither__tmp - s V_quantize3_ord_dither_row)
      + max0(s V_quantize3_ord_dither__tmp - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col) <= z)%Q
   | 20 => ((1 # 1)
            + s V_quantize3_ord_dither__tmp * max0(s V_quantize3_ord_dither_col)
            - s V_quantize3_ord_dither_row * max0(s V_quantize3_ord_dither_col)
            + s V_quantize3_ord_dither_z
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            - max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col)
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width) <= z)%Q
   | 21 => ((1 # 1)
            + s V_quantize3_ord_dither__tmp * max0(s V_quantize3_ord_dither_col)
            - s V_quantize3_ord_dither_row * max0(s V_quantize3_ord_dither_col)
            + s V_quantize3_ord_dither_z
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            - max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col)
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width) <= z)%Q
   | 22 => ((1 # 1)
            + s V_quantize3_ord_dither__tmp * max0(s V_quantize3_ord_dither_col)
            - s V_quantize3_ord_dither_row * max0(s V_quantize3_ord_dither_col)
            + s V_quantize3_ord_dither_z
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            - max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col)
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width) <= z)%Q
   | 23 => ((1 # 1)
            + s V_quantize3_ord_dither__tmp * max0(s V_quantize3_ord_dither_col)
            - s V_quantize3_ord_dither_row * max0(s V_quantize3_ord_dither_col)
            + s V_quantize3_ord_dither_z
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            - max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + max0(s V_quantize3_ord_dither_col) <= z)%Q
   | 24 => ((1 # 1)
            + s V_quantize3_ord_dither__tmp * max0(s V_quantize3_ord_dither_col)
            - s V_quantize3_ord_dither_row * max0(s V_quantize3_ord_dither_col)
            + s V_quantize3_ord_dither_z
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            - max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + max0(s V_quantize3_ord_dither_col) <= z)%Q
   | 25 => ((1 # 1)
            + s V_quantize3_ord_dither__tmp * max0(s V_quantize3_ord_dither_col)
            - s V_quantize3_ord_dither_row * max0(s V_quantize3_ord_dither_col)
            + s V_quantize3_ord_dither_z
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row)
            - max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + max0(s V_quantize3_ord_dither_col) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row) (0))) (F_max0_ge_0 (s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither_col)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither_col)) (F_check_ge (0) (0))]
     (s V_quantize3_ord_dither__tmp * max0(s V_quantize3_ord_dither_col)
      - s V_quantize3_ord_dither_row * max0(s V_quantize3_ord_dither_col)
      + s V_quantize3_ord_dither_z
      + max0(s V_quantize3_ord_dither__tmp - s V_quantize3_ord_dither_row)
      - max0(s V_quantize3_ord_dither__tmp - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col)
      + max0(s V_quantize3_ord_dither__tmp - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
      + max0(s V_quantize3_ord_dither_col) <= z)%Q
   | 27 => hints
     [(*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                             + s V_quantize3_ord_dither__tmp)) (F_check_ge (-1
                                                                    + s V_quantize3_ord_dither__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither_col)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither_col)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quantize3_ord_dither_col) (0))) (F_max0_ge_0 (s V_quantize3_ord_dither_col))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quantize3_ord_dither__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quantize3_ord_dither_col)) (F_check_ge (s V_quantize3_ord_dither_col) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quantize3_ord_dither_col)) (F_check_ge (s V_quantize3_ord_dither_col) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quantize3_ord_dither_row) (0))) (F_max0_ge_0 (s V_quantize3_ord_dither_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither_col)) (F_check_ge (0) (0)))]
     (s V_quantize3_ord_dither_z
      - max0(-1 + s V_quantize3_ord_dither__tmp
             - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col)
      + max0(-1 + s V_quantize3_ord_dither__tmp
             - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
      + max0(s V_quantize3_ord_dither__tmp - s V_quantize3_ord_dither_row)
      + max0(s V_quantize3_ord_dither__tmp - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_col) <= z)%Q
   | 28 => (-s V_quantize3_ord_dither_col * max0(-1
                                                 + s V_quantize3_ord_dither__tmp)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither__tmp
                                                  - s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_z
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) <= z)%Q
   | 29 => (-s V_quantize3_ord_dither_col * max0(-1
                                                 + s V_quantize3_ord_dither__tmp)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither__tmp
                                                  - s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_z
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) <= z)%Q
   | 30 => (-s V_quantize3_ord_dither_col * max0(-1
                                                 + s V_quantize3_ord_dither__tmp)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither__tmp
                                                  - s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_z
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) <= z)%Q
   | 31 => (-s V_quantize3_ord_dither_col * max0(-1
                                                 + s V_quantize3_ord_dither__tmp)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither__tmp
                                                  - s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_z
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) <= z)%Q
   | 32 => (-s V_quantize3_ord_dither_col * max0(-1
                                                 + s V_quantize3_ord_dither__tmp)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither__tmp
                                                  - s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_z
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) <= z)%Q
   | 33 => (-s V_quantize3_ord_dither_col * max0(-1
                                                 + s V_quantize3_ord_dither__tmp)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither__tmp
                                                  - s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_z
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + max0(s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) <= z)%Q
   | 34 => (-s V_quantize3_ord_dither_col * max0(-1
                                                 + s V_quantize3_ord_dither__tmp)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither__tmp
                                                  - s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_z
            - max0(-1 + s V_quantize3_ord_dither__tmp)
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + (2 # 1) * max0(s V_quantize3_ord_dither__tmp
                             - s V_quantize3_ord_dither_row)
            + max0(s V_quantize3_ord_dither_row) <= z)%Q
   | 35 => (-s V_quantize3_ord_dither_col * max0(-1
                                                 + s V_quantize3_ord_dither__tmp)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither__tmp
                                                  - s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_z
            - max0(-1 + s V_quantize3_ord_dither__tmp)
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + (2 # 1) * max0(s V_quantize3_ord_dither__tmp
                             - s V_quantize3_ord_dither_row)
            + max0(s V_quantize3_ord_dither_row) <= z)%Q
   | 36 => (-s V_quantize3_ord_dither_col * max0(-1
                                                 + s V_quantize3_ord_dither__tmp)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither__tmp
                                                  - s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither_row)
            + s V_quantize3_ord_dither_z
            - max0(-1 + s V_quantize3_ord_dither__tmp)
            + max0(-1 + s V_quantize3_ord_dither__tmp
                   - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
            + (2 # 1) * max0(s V_quantize3_ord_dither__tmp
                             - s V_quantize3_ord_dither_row)
            + max0(s V_quantize3_ord_dither_row) <= z)%Q
   | 37 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_quantize3_ord_dither__tmp
                                       - s V_quantize3_ord_dither_row) (1);
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                     + s V_quantize3_ord_dither__tmp)) (F_check_ge (-1
                                                                    + s V_quantize3_ord_dither__tmp) (0));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_quantize3_ord_dither__tmp) (0))) (F_max0_ge_0 (s V_quantize3_ord_dither__tmp));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_quantize3_ord_dither__tmp
                                                     - s V_quantize3_ord_dither_row)) (F_check_ge (s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row) (0));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quantize3_ord_dither__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quantize3_ord_dither__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quantize3_ord_dither__tmp)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quantize3_ord_dither__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quantize3_ord_dither__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_quantize3_ord_dither__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quantize3_ord_dither__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither_col)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_quantize3_ord_dither__tmp
                                                                - s V_quantize3_ord_dither_row)) (F_check_ge (-1
                                                                    + s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_quantize3_ord_dither__tmp
                                                                - s V_quantize3_ord_dither_row)) (F_check_ge (-1
                                                                    + s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                              + s V_quantize3_ord_dither__tmp
                                                              - s V_quantize3_ord_dither_row)) (F_check_ge (-1
                                                                    + s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither_col)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quantize3_ord_dither__tmp)) (F_check_ge (s V_quantize3_ord_dither__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quantize3_ord_dither__tmp)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quantize3_ord_dither__tmp)) (F_check_ge (s V_quantize3_ord_dither__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither__tmp)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row) (0))) (F_max0_ge_0 (s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row) (0))) (F_max0_ge_0 (s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quantize3_ord_dither_col) (0))) (F_max0_ge_0 (s V_quantize3_ord_dither_col))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_quantize3_ord_dither_col) (0))) (F_max0_ge_0 (s V_quantize3_ord_dither_col))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither_row)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quantize3_ord_dither_col)) (F_check_ge (s V_quantize3_ord_dither_col) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_quantize3_ord_dither__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_quantize3_ord_dither_row)) (F_check_ge (s V_quantize3_ord_dither_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_quantize3_ord_dither_col)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_quantize3_ord_dither_row)) (F_check_ge (s V_quantize3_ord_dither_row) (0));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                 + s V_quantize3_ord_dither__tmp
                                                                 - s V_quantize3_ord_dither_row) (0))) (F_max0_ge_0 (-1
                                                                    + s V_quantize3_ord_dither__tmp
                                                                    - s V_quantize3_ord_dither_row))]
     (-(1 # 1)
      - s V_quantize3_ord_dither_col * max0(-1
                                            + s V_quantize3_ord_dither__tmp)
      + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither__tmp
                                            - s V_quantize3_ord_dither_row)
      + s V_quantize3_ord_dither_col * max0(s V_quantize3_ord_dither_row)
      + s V_quantize3_ord_dither_z - max0(-1 + s V_quantize3_ord_dither__tmp)
      + max0(-1 + s V_quantize3_ord_dither__tmp
             - s V_quantize3_ord_dither_row) * max0(s V_quantize3_ord_dither_width)
      + (2 # 1) * max0(s V_quantize3_ord_dither__tmp
                       - s V_quantize3_ord_dither_row)
      + max0(s V_quantize3_ord_dither_row) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_quantize3_ord_dither =>
    [mkPA Q (fun n z s => ai_quantize3_ord_dither n s /\ annot0_quantize3_ord_dither n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_quantize3_ord_dither (proc_start P_quantize3_ord_dither) s1 (proc_end P_quantize3_ord_dither) s2 ->
    (s2 V_quantize3_ord_dither_z <= max0(s1 V_quantize3_ord_dither_cinfo_dref_off128) * max0(s1 V_quantize3_ord_dither_num_rows)
                                    + max0(s1 V_quantize3_ord_dither_num_rows))%Q.
Proof.
  prove_bound ipa admissible_ipa P_quantize3_ord_dither.
Qed.
