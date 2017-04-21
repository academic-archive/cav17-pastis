Require Import pasta.Pasta.

Inductive proc: Type :=
  P_h2v2_downsample.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_h2v2_downsample_z := 1%positive.
Notation V_h2v2_downsample_bias := 2%positive.
Notation V_h2v2_downsample_compptr_dref_off12 := 3%positive.
Notation V_h2v2_downsample_compptr_dref_off28 := 4%positive.
Notation V_h2v2_downsample_inrow := 5%positive.
Notation V_h2v2_downsample_outcol := 6%positive.
Notation V_h2v2_downsample_output_cols := 7%positive.
Notation V_h2v2_downsample_outrow := 8%positive.
Notation V_h2v2_downsample_cinfo := 9%positive.
Notation V_h2v2_downsample_compptr := 10%positive.
Notation V_h2v2_downsample_input_data := 11%positive.
Notation V_h2v2_downsample_output_data := 12%positive.
Definition Pedges_h2v2_downsample: list (edge proc) :=
  (EA 1 (AAssign V_h2v2_downsample_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_h2v2_downsample_output_cols) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_h2v2_downsample_outcol) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_h2v2_downsample_output_cols
  (Some (EMul (EVar V_h2v2_downsample_compptr_dref_off28) (ENum (8))))) 6)::
  (EA 6 (AAssign V_h2v2_downsample_inrow (Some (ENum (0)))) 7)::
  (EA 7 (AAssign V_h2v2_downsample_outrow (Some (ENum (0)))) 8)::
  (EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_h2v2_downsample_outrow) s) <
  (eval (EVar V_h2v2_downsample_compptr_dref_off12) s))%Z)) 13)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_h2v2_downsample_outrow) s) >=
  (eval (EVar V_h2v2_downsample_compptr_dref_off12) s))%Z)) 11)::
  (EA 11 AWeaken 12)::(EA 13 AWeaken 14)::(EA 14 (AAssign
  V_h2v2_downsample_bias (Some (ENum (1)))) 15)::(EA 15 (AAssign
  V_h2v2_downsample_outcol (Some (ENum (0)))) 16)::(EA 16 ANone 17)::
  (EA 17 AWeaken 18)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_h2v2_downsample_outcol) s) <
  (eval (EVar V_h2v2_downsample_output_cols) s))%Z)) 27)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_h2v2_downsample_outcol) s) >=
  (eval (EVar V_h2v2_downsample_output_cols) s))%Z)) 19)::
  (EA 19 AWeaken 20)::(EA 20 (AAssign V_h2v2_downsample_inrow
  (Some (EAdd (EVar V_h2v2_downsample_inrow) (ENum (2))))) 21)::
  (EA 21 ANone 22)::(EA 22 (AAssign V_h2v2_downsample_outrow
  (Some (EAdd (EVar V_h2v2_downsample_outrow) (ENum (1))))) 23)::
  (EA 23 ANone 24)::(EA 24 ANone 25)::(EA 25 (AAssign V_h2v2_downsample_z
  (Some (EAdd (ENum (1)) (EVar V_h2v2_downsample_z)))) 26)::
  (EA 26 AWeaken 10)::(EA 27 AWeaken 28)::(EA 28 (AAssign
  V_h2v2_downsample_bias None) 29)::(EA 29 ANone 30)::(EA 30 (AAssign
  V_h2v2_downsample_outcol (Some (EAdd (EVar V_h2v2_downsample_outcol)
  (ENum (1))))) 31)::(EA 31 ANone 32)::(EA 32 ANone 33)::(EA 33 (AAssign
  V_h2v2_downsample_z (Some (EAdd (ENum (1))
  (EVar V_h2v2_downsample_z)))) 34)::(EA 34 AWeaken 18)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_h2v2_downsample => Pedges_h2v2_downsample
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_h2v2_downsample => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_h2v2_downsample (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_z <= 0)%Z
   | 3 => (-1 * s V_h2v2_downsample_z <= 0 /\ 1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_output_cols <= 0)%Z
   | 4 => (-1 * s V_h2v2_downsample_output_cols <= 0 /\ 1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0)%Z
   | 5 => (-1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ 1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_output_cols <= 0)%Z
   | 6 => (1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0)%Z
   | 7 => (-1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ 1 * s V_h2v2_downsample_z <= 0 /\ 1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0)%Z
   | 8 => (-1 * s V_h2v2_downsample_inrow <= 0 /\ 1 * s V_h2v2_downsample_inrow <= 0 /\ 1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0 /\ 1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0)%Z
   | 9 => (-1 * s V_h2v2_downsample_outrow <= 0 /\ 1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ 1 * s V_h2v2_downsample_z <= 0 /\ 1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0)%Z
   | 10 => (-1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0)%Z
   | 11 => (-1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ 1 * s V_h2v2_downsample_compptr_dref_off12+ -1 * s V_h2v2_downsample_outrow <= 0)%Z
   | 12 => (1 * s V_h2v2_downsample_compptr_dref_off12+ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0)%Z
   | 13 => (-1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0)%Z
   | 14 => (-1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0)%Z
   | 15 => (-1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ 1 * s V_h2v2_downsample_bias + -1 <= 0 /\ -1 * s V_h2v2_downsample_bias + 1 <= 0)%Z
   | 16 => (-1 * s V_h2v2_downsample_bias + 1 <= 0 /\ 1 * s V_h2v2_downsample_bias + -1 <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ 1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0)%Z
   | 17 => (-1 * s V_h2v2_downsample_outcol <= 0 /\ 1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ 1 * s V_h2v2_downsample_bias + -1 <= 0 /\ -1 * s V_h2v2_downsample_bias + 1 <= 0)%Z
   | 18 => (-1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0)%Z
   | 19 => (-1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol+ 1 * s V_h2v2_downsample_output_cols <= 0)%Z
   | 20 => (-1 * s V_h2v2_downsample_outcol+ 1 * s V_h2v2_downsample_output_cols <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0)%Z
   | 21 => (-1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol+ 1 * s V_h2v2_downsample_output_cols <= 0 /\ -1 * s V_h2v2_downsample_inrow + 2 <= 0)%Z
   | 22 => (-1 * s V_h2v2_downsample_inrow + 2 <= 0 /\ -1 * s V_h2v2_downsample_outcol+ 1 * s V_h2v2_downsample_output_cols <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0)%Z
   | 23 => (-1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol+ 1 * s V_h2v2_downsample_output_cols <= 0 /\ -1 * s V_h2v2_downsample_inrow + 2 <= 0 /\ -1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow <= 0)%Z
   | 24 => (-1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_inrow + 2 <= 0 /\ -1 * s V_h2v2_downsample_outcol+ 1 * s V_h2v2_downsample_output_cols <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0)%Z
   | 25 => (-1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol+ 1 * s V_h2v2_downsample_output_cols <= 0 /\ -1 * s V_h2v2_downsample_inrow + 2 <= 0 /\ -1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow <= 0)%Z
   | 26 => (-1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_inrow + 2 <= 0 /\ -1 * s V_h2v2_downsample_outcol+ 1 * s V_h2v2_downsample_output_cols <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_z + 1 <= 0)%Z
   | 27 => (-1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ 1 * s V_h2v2_downsample_outcol+ -1 * s V_h2v2_downsample_output_cols + 1 <= 0)%Z
   | 28 => (1 * s V_h2v2_downsample_outcol+ -1 * s V_h2v2_downsample_output_cols + 1 <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0)%Z
   | 29 => (-1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ 1 * s V_h2v2_downsample_outcol+ -1 * s V_h2v2_downsample_output_cols + 1 <= 0)%Z
   | 30 => (1 * s V_h2v2_downsample_outcol+ -1 * s V_h2v2_downsample_output_cols + 1 <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outcol <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0)%Z
   | 31 => (-1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ 1 * s V_h2v2_downsample_outcol+ -1 * s V_h2v2_downsample_output_cols <= 0 /\ -1 * s V_h2v2_downsample_outcol + 1 <= 0)%Z
   | 32 => (-1 * s V_h2v2_downsample_outcol + 1 <= 0 /\ 1 * s V_h2v2_downsample_outcol+ -1 * s V_h2v2_downsample_output_cols <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0)%Z
   | 33 => (-1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_z <= 0 /\ 1 * s V_h2v2_downsample_outcol+ -1 * s V_h2v2_downsample_output_cols <= 0 /\ -1 * s V_h2v2_downsample_outcol + 1 <= 0)%Z
   | 34 => (-1 * s V_h2v2_downsample_outcol + 1 <= 0 /\ 1 * s V_h2v2_downsample_outcol+ -1 * s V_h2v2_downsample_output_cols <= 0 /\ -1 * s V_h2v2_downsample_outrow <= 0 /\ -1 * s V_h2v2_downsample_inrow <= 0 /\ -1 * s V_h2v2_downsample_compptr_dref_off12+ 1 * s V_h2v2_downsample_outrow + 1 <= 0 /\ -1 * s V_h2v2_downsample_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_h2v2_downsample (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_h2v2_downsample_compptr_dref_off12)
           + max0(s V_h2v2_downsample_compptr_dref_off12) * max0(8 * s V_h2v2_downsample_compptr_dref_off28) <= z)%Q
   | 2 => (s V_h2v2_downsample_z
           + max0(s V_h2v2_downsample_compptr_dref_off12)
           + max0(s V_h2v2_downsample_compptr_dref_off12) * max0(8 * s V_h2v2_downsample_compptr_dref_off28) <= z)%Q
   | 3 => (s V_h2v2_downsample_z
           + max0(s V_h2v2_downsample_compptr_dref_off12)
           + max0(s V_h2v2_downsample_compptr_dref_off12) * max0(8 * s V_h2v2_downsample_compptr_dref_off28) <= z)%Q
   | 4 => (s V_h2v2_downsample_z
           + max0(s V_h2v2_downsample_compptr_dref_off12)
           + max0(s V_h2v2_downsample_compptr_dref_off12) * max0(8 * s V_h2v2_downsample_compptr_dref_off28) <= z)%Q
   | 5 => (s V_h2v2_downsample_z
           + max0(s V_h2v2_downsample_compptr_dref_off12)
           + max0(s V_h2v2_downsample_compptr_dref_off12) * max0(8 * s V_h2v2_downsample_compptr_dref_off28) <= z)%Q
   | 6 => (s V_h2v2_downsample_z
           + max0(s V_h2v2_downsample_compptr_dref_off12)
           + max0(s V_h2v2_downsample_compptr_dref_off12) * max0(s V_h2v2_downsample_output_cols) <= z)%Q
   | 7 => (s V_h2v2_downsample_z
           + max0(s V_h2v2_downsample_compptr_dref_off12)
           + max0(s V_h2v2_downsample_compptr_dref_off12) * max0(s V_h2v2_downsample_output_cols) <= z)%Q
   | 8 => (s V_h2v2_downsample_z
           + max0(s V_h2v2_downsample_compptr_dref_off12
                  - s V_h2v2_downsample_outrow)
           + max0(s V_h2v2_downsample_compptr_dref_off12
                  - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols) <= z)%Q
   | 9 => (s V_h2v2_downsample_z
           + max0(s V_h2v2_downsample_compptr_dref_off12
                  - s V_h2v2_downsample_outrow)
           + max0(s V_h2v2_downsample_compptr_dref_off12
                  - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols) <= z)%Q
   | 10 => (s V_h2v2_downsample_z
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_h2v2_downsample_compptr_dref_off12
                                             - s V_h2v2_downsample_outrow) (-1
                                                                    + s V_h2v2_downsample_compptr_dref_off12
                                                                    - s V_h2v2_downsample_outrow));
      (*-1 0*) F_max0_ge_0 (-1 + s V_h2v2_downsample_compptr_dref_off12
                            - s V_h2v2_downsample_outrow);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v2_downsample_compptr_dref_off12
                                                            - s V_h2v2_downsample_outrow)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v2_downsample_output_cols)) (F_check_ge (0) (0)))]
     (s V_h2v2_downsample_z
      + max0(s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow)
      + max0(s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols) <= z)%Q
   | 12 => (s V_h2v2_downsample_z <= z)%Q
   | 13 => (s V_h2v2_downsample_z
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols) <= z)%Q
   | 14 => (s V_h2v2_downsample_z
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols) <= z)%Q
   | 15 => (s V_h2v2_downsample_z
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols) <= z)%Q
   | 16 => (s V_h2v2_downsample_z
            - max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                        + s V_h2v2_downsample_output_cols)
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                        + s V_h2v2_downsample_output_cols) <= z)%Q
   | 17 => (s V_h2v2_downsample_z
            - max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                        + s V_h2v2_downsample_output_cols)
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                        + s V_h2v2_downsample_output_cols) <= z)%Q
   | 18 => (s V_h2v2_downsample_z
            - max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                        + s V_h2v2_downsample_output_cols)
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                        + s V_h2v2_downsample_output_cols) <= z)%Q
   | 19 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_h2v2_downsample_compptr_dref_off12
                                      - s V_h2v2_downsample_outrow) (1);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_h2v2_downsample_compptr_dref_off12
                                                              - s V_h2v2_downsample_outrow)) (F_check_ge (s V_h2v2_downsample_compptr_dref_off12
                                                                    - s V_h2v2_downsample_outrow) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_h2v2_downsample_outcol
                                                                    + s V_h2v2_downsample_output_cols)) (F_check_ge (0) (0)))]
     (s V_h2v2_downsample_z
      - max0(-1 + s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                  + s V_h2v2_downsample_output_cols)
      + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
      + max0(s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow)
      + max0(s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                  + s V_h2v2_downsample_output_cols) <= z)%Q
   | 20 => ((1 # 1)
            + s V_h2v2_downsample_compptr_dref_off12 * max0(-s V_h2v2_downsample_outcol
                                                            + s V_h2v2_downsample_output_cols)
            - s V_h2v2_downsample_outrow * max0(-s V_h2v2_downsample_outcol
                                                + s V_h2v2_downsample_output_cols)
            + s V_h2v2_downsample_z
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            - max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                        + s V_h2v2_downsample_output_cols)
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols) <= z)%Q
   | 21 => ((1 # 1)
            + s V_h2v2_downsample_compptr_dref_off12 * max0(-s V_h2v2_downsample_outcol
                                                            + s V_h2v2_downsample_output_cols)
            - s V_h2v2_downsample_outrow * max0(-s V_h2v2_downsample_outcol
                                                + s V_h2v2_downsample_output_cols)
            + s V_h2v2_downsample_z
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            - max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                        + s V_h2v2_downsample_output_cols)
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols) <= z)%Q
   | 22 => ((1 # 1)
            + s V_h2v2_downsample_compptr_dref_off12 * max0(-s V_h2v2_downsample_outcol
                                                            + s V_h2v2_downsample_output_cols)
            - s V_h2v2_downsample_outrow * max0(-s V_h2v2_downsample_outcol
                                                + s V_h2v2_downsample_output_cols)
            + s V_h2v2_downsample_z
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            - max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                        + s V_h2v2_downsample_output_cols)
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols) <= z)%Q
   | 23 => ((1 # 1)
            + s V_h2v2_downsample_compptr_dref_off12 * max0(-s V_h2v2_downsample_outcol
                                                            + s V_h2v2_downsample_output_cols)
            - s V_h2v2_downsample_outrow * max0(-s V_h2v2_downsample_outcol
                                                + s V_h2v2_downsample_output_cols)
            + s V_h2v2_downsample_z
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            - max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                        + s V_h2v2_downsample_output_cols)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
            + max0(-s V_h2v2_downsample_outcol
                   + s V_h2v2_downsample_output_cols) <= z)%Q
   | 24 => ((1 # 1)
            + s V_h2v2_downsample_compptr_dref_off12 * max0(-s V_h2v2_downsample_outcol
                                                            + s V_h2v2_downsample_output_cols)
            - s V_h2v2_downsample_outrow * max0(-s V_h2v2_downsample_outcol
                                                + s V_h2v2_downsample_output_cols)
            + s V_h2v2_downsample_z
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            - max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                        + s V_h2v2_downsample_output_cols)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
            + max0(-s V_h2v2_downsample_outcol
                   + s V_h2v2_downsample_output_cols) <= z)%Q
   | 25 => ((1 # 1)
            + s V_h2v2_downsample_compptr_dref_off12 * max0(-s V_h2v2_downsample_outcol
                                                            + s V_h2v2_downsample_output_cols)
            - s V_h2v2_downsample_outrow * max0(-s V_h2v2_downsample_outcol
                                                + s V_h2v2_downsample_output_cols)
            + s V_h2v2_downsample_z
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            - max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                        + s V_h2v2_downsample_output_cols)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
            + max0(-s V_h2v2_downsample_outcol
                   + s V_h2v2_downsample_output_cols) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_h2v2_downsample_compptr_dref_off12
                                                                    - s V_h2v2_downsample_outrow) (0))) (F_max0_ge_0 (s V_h2v2_downsample_compptr_dref_off12
                                                                    - s V_h2v2_downsample_outrow))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_h2v2_downsample_outcol
                                                                    + s V_h2v2_downsample_output_cols)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_h2v2_downsample_outcol
                                                 + s V_h2v2_downsample_output_cols)) (F_check_ge (0) (0))]
     (s V_h2v2_downsample_compptr_dref_off12 * max0(-s V_h2v2_downsample_outcol
                                                    + s V_h2v2_downsample_output_cols)
      - s V_h2v2_downsample_outrow * max0(-s V_h2v2_downsample_outcol
                                          + s V_h2v2_downsample_output_cols)
      + s V_h2v2_downsample_z
      + max0(s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow)
      - max0(s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                  + s V_h2v2_downsample_output_cols)
      + max0(s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
      + max0(-s V_h2v2_downsample_outcol + s V_h2v2_downsample_output_cols) <= z)%Q
   | 27 => hints
     [(*0 1*) F_max0_pre_decrement 1 (-s V_h2v2_downsample_outcol
                                      + s V_h2v2_downsample_output_cols) (1);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_h2v2_downsample_compptr_dref_off12
                                                                    - s V_h2v2_downsample_outrow) (0))) (F_max0_ge_0 (-1
                                                                    + s V_h2v2_downsample_compptr_dref_off12
                                                                    - s V_h2v2_downsample_outrow))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_h2v2_downsample_outcol
                                                                    + s V_h2v2_downsample_output_cols)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_h2v2_downsample_compptr_dref_off12
                                                             - s V_h2v2_downsample_outrow)) (F_check_ge (s V_h2v2_downsample_compptr_dref_off12
                                                                    - s V_h2v2_downsample_outrow) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_h2v2_downsample_outcol
                                                                    + s V_h2v2_downsample_output_cols)) (F_check_ge (0) (0)))]
     (s V_h2v2_downsample_z
      - max0(-1 + s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                  + s V_h2v2_downsample_output_cols)
      + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
      + max0(s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow)
      + max0(s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow) * max0(-s V_h2v2_downsample_outcol
                                                  + s V_h2v2_downsample_output_cols) <= z)%Q
   | 28 => ((1 # 1) + s V_h2v2_downsample_z
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
            + max0(-1 - s V_h2v2_downsample_outcol
                   + s V_h2v2_downsample_output_cols)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) <= z)%Q
   | 29 => ((1 # 1) + s V_h2v2_downsample_z
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
            + max0(-1 - s V_h2v2_downsample_outcol
                   + s V_h2v2_downsample_output_cols)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) <= z)%Q
   | 30 => ((1 # 1) + s V_h2v2_downsample_z
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
            + max0(-1 - s V_h2v2_downsample_outcol
                   + s V_h2v2_downsample_output_cols)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) <= z)%Q
   | 31 => ((1 # 1) + s V_h2v2_downsample_z
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            + max0(-s V_h2v2_downsample_outcol
                   + s V_h2v2_downsample_output_cols) <= z)%Q
   | 32 => ((1 # 1) + s V_h2v2_downsample_z
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            + max0(-s V_h2v2_downsample_outcol
                   + s V_h2v2_downsample_output_cols) <= z)%Q
   | 33 => ((1 # 1) + s V_h2v2_downsample_z
            + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
            + max0(s V_h2v2_downsample_compptr_dref_off12
                   - s V_h2v2_downsample_outrow)
            + max0(-s V_h2v2_downsample_outcol
                   + s V_h2v2_downsample_output_cols) <= z)%Q
   | 34 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                              + s V_h2v2_downsample_compptr_dref_off12
                                                              - s V_h2v2_downsample_outrow)) (F_check_ge (-1
                                                                    + s V_h2v2_downsample_compptr_dref_off12
                                                                    - s V_h2v2_downsample_outrow) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_h2v2_downsample_outcol
                                                                    + s V_h2v2_downsample_output_cols)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_h2v2_downsample_compptr_dref_off12
                                                                    - s V_h2v2_downsample_outrow) (0))) (F_max0_ge_0 (s V_h2v2_downsample_compptr_dref_off12
                                                                    - s V_h2v2_downsample_outrow))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_h2v2_downsample_outcol
                                                                    + s V_h2v2_downsample_output_cols)) (F_check_ge (0) (0)))]
     (s V_h2v2_downsample_z
      + max0(-1 + s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow) * max0(s V_h2v2_downsample_output_cols)
      + max0(s V_h2v2_downsample_compptr_dref_off12
             - s V_h2v2_downsample_outrow)
      + max0(-s V_h2v2_downsample_outcol + s V_h2v2_downsample_output_cols) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_h2v2_downsample =>
    [mkPA Q (fun n z s => ai_h2v2_downsample n s /\ annot0_h2v2_downsample n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_h2v2_downsample (proc_start P_h2v2_downsample) s1 (proc_end P_h2v2_downsample) s2 ->
    (s2 V_h2v2_downsample_z <= max0(s1 V_h2v2_downsample_compptr_dref_off12)
                               + max0(s1 V_h2v2_downsample_compptr_dref_off12) * max0(8 * s1 V_h2v2_downsample_compptr_dref_off28))%Q.
Proof.
  prove_bound ipa admissible_ipa P_h2v2_downsample.
Qed.
