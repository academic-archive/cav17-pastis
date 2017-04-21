Require Import pasta.Pasta.

Inductive proc: Type :=
  P_color_quantize3.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_color_quantize3_z := 1%positive.
Notation V_color_quantize3__tmp := 2%positive.
Notation V_color_quantize3_cinfo_dref_off128 := 3%positive.
Notation V_color_quantize3_col := 4%positive.
Notation V_color_quantize3_pixcode := 5%positive.
Notation V_color_quantize3_row := 6%positive.
Notation V_color_quantize3_width := 7%positive.
Notation V_color_quantize3_cinfo := 8%positive.
Notation V_color_quantize3_input_buf := 9%positive.
Notation V_color_quantize3_num_rows := 10%positive.
Notation V_color_quantize3_output_buf := 11%positive.
Definition Pedges_color_quantize3: list (edge proc) :=
  (EA 1 (AAssign V_color_quantize3_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_color_quantize3_col) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_color_quantize3__tmp
  (Some (EVar V_color_quantize3_num_rows))) 5)::(EA 5 (AAssign
  V_color_quantize3_width
  (Some (EVar V_color_quantize3_cinfo_dref_off128))) 6)::(EA 6 (AAssign
  V_color_quantize3_row (Some (ENum (0)))) 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_color_quantize3_row) s) <
  (eval (EVar V_color_quantize3__tmp) s))%Z)) 12)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_color_quantize3_row) s) >=
  (eval (EVar V_color_quantize3__tmp) s))%Z)) 10)::(EA 10 AWeaken 11)::
  (EA 12 AWeaken 13)::(EA 13 (AAssign V_color_quantize3_col
  (Some (EVar V_color_quantize3_width))) 14)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_color_quantize3_col) s) > (eval (ENum (0))
  s))%Z)) 24)::(EA 16 (AGuard (fun s => ((eval (EVar V_color_quantize3_col)
  s) <= (eval (ENum (0)) s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 18 ANone 19)::
  (EA 19 (AAssign V_color_quantize3_row
  (Some (EAdd (EVar V_color_quantize3_row) (ENum (1))))) 20)::
  (EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 (AAssign V_color_quantize3_z
  (Some (EAdd (ENum (1)) (EVar V_color_quantize3_z)))) 23)::
  (EA 23 AWeaken 9)::(EA 24 AWeaken 25)::(EA 25 (AAssign
  V_color_quantize3_pixcode None) 26)::(EA 26 (AAssign
  V_color_quantize3_pixcode None) 27)::(EA 27 (AAssign
  V_color_quantize3_pixcode None) 28)::(EA 28 ANone 29)::(EA 29 (AAssign
  V_color_quantize3_col (Some (EAdd (EVar V_color_quantize3_col)
  (ENum (-1))))) 30)::(EA 30 ANone 31)::(EA 31 ANone 32)::(EA 32 (AAssign
  V_color_quantize3_z (Some (EAdd (ENum (1))
  (EVar V_color_quantize3_z)))) 33)::(EA 33 AWeaken 16)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_color_quantize3 => Pedges_color_quantize3
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_color_quantize3 => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_color_quantize3 (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_z <= 0)%Z
   | 3 => (-1 * s V_color_quantize3_z <= 0 /\ 1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_col <= 0)%Z
   | 4 => (-1 * s V_color_quantize3_col <= 0 /\ 1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_z <= 0)%Z
   | 5 => (-1 * s V_color_quantize3_z <= 0 /\ 1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_col <= 0)%Z
   | 6 => (-1 * s V_color_quantize3_col <= 0 /\ 1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_z <= 0)%Z
   | 7 => (-1 * s V_color_quantize3_z <= 0 /\ 1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_col <= 0 /\ 1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3_row <= 0)%Z
   | 8 => (-1 * s V_color_quantize3_row <= 0 /\ 1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3_col <= 0 /\ 1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_z <= 0)%Z
   | 9 => (-1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_row <= 0)%Z
   | 10 => (-1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ 1 * s V_color_quantize3__tmp+ -1 * s V_color_quantize3_row <= 0)%Z
   | 11 => (1 * s V_color_quantize3__tmp+ -1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_row <= 0)%Z
   | 12 => (-1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0)%Z
   | 13 => (-1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_row <= 0)%Z
   | 14 => (-1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0)%Z
   | 15 => (-1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_row <= 0)%Z
   | 16 => (-1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_row <= 0)%Z
   | 17 => (-1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ 1 * s V_color_quantize3_col <= 0)%Z
   | 18 => (1 * s V_color_quantize3_col <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_row <= 0)%Z
   | 19 => (-1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ 1 * s V_color_quantize3_col <= 0)%Z
   | 20 => (1 * s V_color_quantize3_col <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row <= 0)%Z
   | 21 => (-1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ 1 * s V_color_quantize3_col <= 0)%Z
   | 22 => (1 * s V_color_quantize3_col <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row <= 0)%Z
   | 23 => (-1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3_row + 1 <= 0 /\ 1 * s V_color_quantize3_col <= 0 /\ -1 * s V_color_quantize3_z + 1 <= 0)%Z
   | 24 => (-1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_col + 1 <= 0)%Z
   | 25 => (-1 * s V_color_quantize3_col + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_row <= 0)%Z
   | 26 => (-1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_col + 1 <= 0)%Z
   | 27 => (-1 * s V_color_quantize3_col + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_row <= 0)%Z
   | 28 => (-1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_col + 1 <= 0)%Z
   | 29 => (-1 * s V_color_quantize3_col + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_row <= 0)%Z
   | 30 => (-1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_col <= 0)%Z
   | 31 => (-1 * s V_color_quantize3_col <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_row <= 0)%Z
   | 32 => (-1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_z <= 0 /\ -1 * s V_color_quantize3_col <= 0)%Z
   | 33 => (-1 * s V_color_quantize3_col <= 0 /\ -1 * s V_color_quantize3__tmp+ 1 * s V_color_quantize3_row + 1 <= 0 /\ -1 * s V_color_quantize3_row <= 0 /\ -1 * s V_color_quantize3_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_color_quantize3 (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_color_quantize3_cinfo_dref_off128) * max0(s V_color_quantize3_num_rows)
           + max0(s V_color_quantize3_num_rows) <= z)%Q
   | 2 => (s V_color_quantize3_z
           + max0(s V_color_quantize3_cinfo_dref_off128) * max0(s V_color_quantize3_num_rows)
           + max0(s V_color_quantize3_num_rows) <= z)%Q
   | 3 => (s V_color_quantize3_z
           + max0(s V_color_quantize3_cinfo_dref_off128) * max0(s V_color_quantize3_num_rows)
           + max0(s V_color_quantize3_num_rows) <= z)%Q
   | 4 => (s V_color_quantize3_z
           + max0(s V_color_quantize3_cinfo_dref_off128) * max0(s V_color_quantize3_num_rows)
           + max0(s V_color_quantize3_num_rows) <= z)%Q
   | 5 => (s V_color_quantize3_z + max0(s V_color_quantize3__tmp)
           + max0(s V_color_quantize3__tmp) * max0(s V_color_quantize3_cinfo_dref_off128) <= z)%Q
   | 6 => (s V_color_quantize3_z + max0(s V_color_quantize3__tmp)
           + max0(s V_color_quantize3__tmp) * max0(s V_color_quantize3_width) <= z)%Q
   | 7 => (s V_color_quantize3_z
           + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
           + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 8 => (s V_color_quantize3_z
           + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
           + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 9 => (s V_color_quantize3_z
           + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
           + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_color_quantize3__tmp
                                             - s V_color_quantize3_row) (-1
                                                                    + s V_color_quantize3__tmp
                                                                    - s V_color_quantize3_row));
      (*-1 0*) F_max0_ge_0 (-1 + s V_color_quantize3__tmp
                            - s V_color_quantize3_row);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3__tmp
                                                            - s V_color_quantize3_row)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3_width)) (F_check_ge (0) (0)))]
     (s V_color_quantize3_z
      + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
      + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 11 => (s V_color_quantize3_z <= z)%Q
   | 12 => (s V_color_quantize3_z
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 13 => (s V_color_quantize3_z
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 14 => (s V_color_quantize3__tmp * max0(s V_color_quantize3_col)
            - s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
            - s V_color_quantize3_row * max0(s V_color_quantize3_col)
            + s V_color_quantize3_row * max0(s V_color_quantize3_width)
            + s V_color_quantize3_z
            - max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_col)
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 15 => (s V_color_quantize3__tmp * max0(s V_color_quantize3_col)
            - s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
            - s V_color_quantize3_row * max0(s V_color_quantize3_col)
            + s V_color_quantize3_row * max0(s V_color_quantize3_width)
            + s V_color_quantize3_z
            - max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_col)
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 16 => (s V_color_quantize3__tmp * max0(s V_color_quantize3_col)
            - s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
            - s V_color_quantize3_row * max0(s V_color_quantize3_col)
            + s V_color_quantize3_row * max0(s V_color_quantize3_width)
            + s V_color_quantize3_z
            - max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_col)
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 17 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_color_quantize3__tmp
                                      - s V_color_quantize3_row) (1);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_color_quantize3__tmp
                                                              - s V_color_quantize3_row)) (F_check_ge (s V_color_quantize3__tmp
                                                                    - s V_color_quantize3_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3_width)) (F_check_ge (0) (0)))]
     (s V_color_quantize3__tmp * max0(s V_color_quantize3_col)
      - s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
      - s V_color_quantize3_row * max0(s V_color_quantize3_col)
      + s V_color_quantize3_row * max0(s V_color_quantize3_width)
      + s V_color_quantize3_z
      - max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_col)
      + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
      + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
      + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 18 => ((1 # 1)
            + s V_color_quantize3__tmp * max0(s V_color_quantize3_col)
            - s V_color_quantize3_row * max0(s V_color_quantize3_col)
            + s V_color_quantize3_z
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row)
            - max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_col)
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 19 => ((1 # 1)
            + s V_color_quantize3__tmp * max0(s V_color_quantize3_col)
            - s V_color_quantize3_row * max0(s V_color_quantize3_col)
            + s V_color_quantize3_z
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row)
            - max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_col)
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 20 => ((1 # 1)
            + s V_color_quantize3__tmp * max0(s V_color_quantize3_col)
            - s V_color_quantize3_row * max0(s V_color_quantize3_col)
            + s V_color_quantize3_z
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            - max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_col)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3_col) <= z)%Q
   | 21 => ((1 # 1)
            + s V_color_quantize3__tmp * max0(s V_color_quantize3_col)
            - s V_color_quantize3_row * max0(s V_color_quantize3_col)
            + s V_color_quantize3_z
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            - max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_col)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3_col) <= z)%Q
   | 22 => ((1 # 1)
            + s V_color_quantize3__tmp * max0(s V_color_quantize3_col)
            - s V_color_quantize3_row * max0(s V_color_quantize3_col)
            + s V_color_quantize3_z
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            - max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_col)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3_col) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_color_quantize3__tmp
                                                                    - s V_color_quantize3_row) (0))) (F_max0_ge_0 (s V_color_quantize3__tmp
                                                                    - s V_color_quantize3_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3_col)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3_col)) (F_check_ge (0) (0))]
     (s V_color_quantize3__tmp * max0(s V_color_quantize3_col)
      - s V_color_quantize3_row * max0(s V_color_quantize3_col)
      + s V_color_quantize3_z
      + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
      - max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_col)
      + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
      + max0(s V_color_quantize3_col) <= z)%Q
   | 24 => hints
     [(*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                             + s V_color_quantize3__tmp)) (F_check_ge (-1
                                                                    + s V_color_quantize3__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3_col)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_color_quantize3__tmp
                                                                    - s V_color_quantize3_row) (0))) (F_max0_ge_0 (-1
                                                                    + s V_color_quantize3__tmp
                                                                    - s V_color_quantize3_row))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3_col)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_color_quantize3__tmp) (0))) (F_max0_ge_0 (s V_color_quantize3__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3_col)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_color_quantize3_col) (0))) (F_max0_ge_0 (s V_color_quantize3_col))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_color_quantize3__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_color_quantize3_col)) (F_check_ge (s V_color_quantize3_col) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3__tmp)) (F_check_ge (0) (0)))]
     (s V_color_quantize3__tmp * max0(s V_color_quantize3_col)
      - s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
      - s V_color_quantize3_row * max0(s V_color_quantize3_col)
      + s V_color_quantize3_row * max0(s V_color_quantize3_width)
      + s V_color_quantize3_z
      - max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_col)
      + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
      + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
      + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 25 => (-s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
            - s V_color_quantize3_col * max0(-1 + s V_color_quantize3__tmp)
            + s V_color_quantize3_col * max0(s V_color_quantize3__tmp)
            + s V_color_quantize3_row * max0(s V_color_quantize3_width)
            + s V_color_quantize3_z
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 26 => (-s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
            - s V_color_quantize3_col * max0(-1 + s V_color_quantize3__tmp)
            + s V_color_quantize3_col * max0(s V_color_quantize3__tmp)
            + s V_color_quantize3_row * max0(s V_color_quantize3_width)
            + s V_color_quantize3_z
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 27 => (-s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
            - s V_color_quantize3_col * max0(-1 + s V_color_quantize3__tmp)
            + s V_color_quantize3_col * max0(s V_color_quantize3__tmp)
            + s V_color_quantize3_row * max0(s V_color_quantize3_width)
            + s V_color_quantize3_z
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 28 => (-s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
            - s V_color_quantize3_col * max0(-1 + s V_color_quantize3__tmp)
            + s V_color_quantize3_col * max0(s V_color_quantize3__tmp)
            + s V_color_quantize3_row * max0(s V_color_quantize3_width)
            + s V_color_quantize3_z
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 29 => (-s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
            - s V_color_quantize3_col * max0(-1 + s V_color_quantize3__tmp)
            + s V_color_quantize3_col * max0(s V_color_quantize3__tmp)
            + s V_color_quantize3_row * max0(s V_color_quantize3_width)
            + s V_color_quantize3_z
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 30 => (-s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
            - s V_color_quantize3_col * max0(-1 + s V_color_quantize3__tmp)
            + s V_color_quantize3_col * max0(s V_color_quantize3__tmp)
            + s V_color_quantize3_row * max0(s V_color_quantize3_width)
            + s V_color_quantize3_z - max0(-1 + s V_color_quantize3__tmp)
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3__tmp)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 31 => (-s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
            - s V_color_quantize3_col * max0(-1 + s V_color_quantize3__tmp)
            + s V_color_quantize3_col * max0(s V_color_quantize3__tmp)
            + s V_color_quantize3_row * max0(s V_color_quantize3_width)
            + s V_color_quantize3_z - max0(-1 + s V_color_quantize3__tmp)
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3__tmp)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 32 => (-s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
            - s V_color_quantize3_col * max0(-1 + s V_color_quantize3__tmp)
            + s V_color_quantize3_col * max0(s V_color_quantize3__tmp)
            + s V_color_quantize3_row * max0(s V_color_quantize3_width)
            + s V_color_quantize3_z - max0(-1 + s V_color_quantize3__tmp)
            + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
            + max0(s V_color_quantize3__tmp)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
            + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | 33 => hints
     [(*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                     + s V_color_quantize3__tmp)) (F_check_ge (-1
                                                                    + s V_color_quantize3__tmp) (0));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_color_quantize3__tmp) (0))) (F_max0_ge_0 (s V_color_quantize3__tmp));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_color_quantize3__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_color_quantize3__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_color_quantize3__tmp)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_color_quantize3__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_color_quantize3__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_color_quantize3__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_color_quantize3__tmp))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3_col)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                              + s V_color_quantize3__tmp
                                                              - s V_color_quantize3_row)) (F_check_ge (-1
                                                                    + s V_color_quantize3__tmp
                                                                    - s V_color_quantize3_row) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3_col)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_color_quantize3__tmp)) (F_check_ge (s V_color_quantize3__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_color_quantize3__tmp)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_color_quantize3__tmp)) (F_check_ge (s V_color_quantize3__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_color_quantize3__tmp)) (F_check_ge (s V_color_quantize3__tmp) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3_col)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_color_quantize3_col) (0))) (F_max0_ge_0 (s V_color_quantize3_col))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_color_quantize3__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_color_quantize3_col)) (F_check_ge (s V_color_quantize3_col) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_color_quantize3__tmp)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_color_quantize3__tmp)) (F_check_ge (s V_color_quantize3__tmp) (0))]
     (-(1 # 1) - s V_color_quantize3__tmp * max0(s V_color_quantize3_width)
      - s V_color_quantize3_col * max0(-1 + s V_color_quantize3__tmp)
      + s V_color_quantize3_col * max0(s V_color_quantize3__tmp)
      + s V_color_quantize3_row * max0(s V_color_quantize3_width)
      + s V_color_quantize3_z - max0(-1 + s V_color_quantize3__tmp)
      + max0(-1 + s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width)
      + max0(s V_color_quantize3__tmp)
      + max0(s V_color_quantize3__tmp - s V_color_quantize3_row)
      + max0(s V_color_quantize3__tmp - s V_color_quantize3_row) * max0(s V_color_quantize3_width) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_color_quantize3 =>
    [mkPA Q (fun n z s => ai_color_quantize3 n s /\ annot0_color_quantize3 n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_color_quantize3 (proc_start P_color_quantize3) s1 (proc_end P_color_quantize3) s2 ->
    (s2 V_color_quantize3_z <= max0(s1 V_color_quantize3_cinfo_dref_off128) * max0(s1 V_color_quantize3_num_rows)
                               + max0(s1 V_color_quantize3_num_rows))%Q.
Proof.
  prove_bound ipa admissible_ipa P_color_quantize3.
Qed.
