Require Import pasta.Pasta.

Inductive proc: Type :=
  P_fullsize_smooth_downsample.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_fullsize_smooth_downsample_z := 1%positive.
Notation V_fullsize_smooth_downsample_cinfo_dref_off264 := 2%positive.
Notation V_fullsize_smooth_downsample_colctr := 3%positive.
Notation V_fullsize_smooth_downsample_colsum := 4%positive.
Notation V_fullsize_smooth_downsample_compptr_dref_off12 := 5%positive.
Notation V_fullsize_smooth_downsample_compptr_dref_off28 := 6%positive.
Notation V_fullsize_smooth_downsample_lastcolsum := 7%positive.
Notation V_fullsize_smooth_downsample_memberscale := 8%positive.
Notation V_fullsize_smooth_downsample_membersum := 9%positive.
Notation V_fullsize_smooth_downsample_neighscale := 10%positive.
Notation V_fullsize_smooth_downsample_neighsum := 11%positive.
Notation V_fullsize_smooth_downsample_nextcolsum := 12%positive.
Notation V_fullsize_smooth_downsample_output_cols := 13%positive.
Notation V_fullsize_smooth_downsample_outrow := 14%positive.
Notation V_fullsize_smooth_downsample_cinfo := 15%positive.
Notation V_fullsize_smooth_downsample_compptr := 16%positive.
Notation V_fullsize_smooth_downsample_input_data := 17%positive.
Notation V_fullsize_smooth_downsample_output_data := 18%positive.
Definition Pedges_fullsize_smooth_downsample: list (edge proc) :=
  (EA 1 (AAssign V_fullsize_smooth_downsample_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_fullsize_smooth_downsample_colctr)
  s) >= (eval (ENum (0)) s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign
  V_fullsize_smooth_downsample_output_cols
  (Some (EMul (EVar V_fullsize_smooth_downsample_compptr_dref_off28)
  (ENum (8))))) 5)::(EA 5 (AAssign V_fullsize_smooth_downsample_memberscale
  None) 6)::(EA 6 (AAssign V_fullsize_smooth_downsample_neighscale
  (Some (EMul (EVar V_fullsize_smooth_downsample_cinfo_dref_off264)
  (ENum (64))))) 7)::(EA 7 (AAssign V_fullsize_smooth_downsample_outrow
  (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_fullsize_smooth_downsample_outrow) s) <
  (eval (EVar V_fullsize_smooth_downsample_compptr_dref_off12) s))%Z)) 13)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_fullsize_smooth_downsample_outrow)
  s) >= (eval (EVar V_fullsize_smooth_downsample_compptr_dref_off12)
  s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 13 AWeaken 14)::(EA 14 (AAssign
  V_fullsize_smooth_downsample_colsum None) 15)::(EA 15 (AAssign
  V_fullsize_smooth_downsample_membersum None) 16)::(EA 16 (AAssign
  V_fullsize_smooth_downsample_nextcolsum None) 17)::(EA 17 (AAssign
  V_fullsize_smooth_downsample_neighsum
  (Some (EAdd (EAdd (EVar V_fullsize_smooth_downsample_colsum)
  (ESub (EVar V_fullsize_smooth_downsample_colsum)
  (EVar V_fullsize_smooth_downsample_membersum)))
  (EVar V_fullsize_smooth_downsample_nextcolsum)))) 18)::(EA 18 (AAssign
  V_fullsize_smooth_downsample_membersum
  (Some (EAdd (EMul (EVar V_fullsize_smooth_downsample_membersum)
  (EVar V_fullsize_smooth_downsample_memberscale))
  (EMul (EVar V_fullsize_smooth_downsample_neighsum)
  (EVar V_fullsize_smooth_downsample_neighscale))))) 19)::(EA 19 (AAssign
  V_fullsize_smooth_downsample_lastcolsum
  (Some (EVar V_fullsize_smooth_downsample_colsum))) 20)::(EA 20 (AAssign
  V_fullsize_smooth_downsample_colsum
  (Some (EVar V_fullsize_smooth_downsample_nextcolsum))) 21)::(EA 21 (AAssign
  V_fullsize_smooth_downsample_colctr
  (Some (ESub (EVar V_fullsize_smooth_downsample_output_cols)
  (ENum (2))))) 22)::(EA 22 ANone 23)::(EA 23 AWeaken 24)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_fullsize_smooth_downsample_colctr) s) >
  (eval (ENum (0)) s))%Z)) 35)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_fullsize_smooth_downsample_colctr) s) <=
  (eval (ENum (0)) s))%Z)) 25)::(EA 25 AWeaken 26)::(EA 26 (AAssign
  V_fullsize_smooth_downsample_membersum None) 27)::(EA 27 (AAssign
  V_fullsize_smooth_downsample_neighsum
  (Some (EAdd (EAdd (EVar V_fullsize_smooth_downsample_lastcolsum)
  (ESub (EVar V_fullsize_smooth_downsample_colsum)
  (EVar V_fullsize_smooth_downsample_membersum)))
  (EVar V_fullsize_smooth_downsample_colsum)))) 28)::(EA 28 (AAssign
  V_fullsize_smooth_downsample_membersum
  (Some (EAdd (EMul (EVar V_fullsize_smooth_downsample_membersum)
  (EVar V_fullsize_smooth_downsample_memberscale))
  (EMul (EVar V_fullsize_smooth_downsample_neighsum)
  (EVar V_fullsize_smooth_downsample_neighscale))))) 29)::(EA 29 ANone 30)::
  (EA 30 (AAssign V_fullsize_smooth_downsample_outrow
  (Some (EAdd (EVar V_fullsize_smooth_downsample_outrow) (ENum (1))))) 31)::
  (EA 31 ANone 32)::(EA 32 ANone 33)::(EA 33 (AAssign
  V_fullsize_smooth_downsample_z (Some (EAdd (ENum (1))
  (EVar V_fullsize_smooth_downsample_z)))) 34)::(EA 34 AWeaken 10)::
  (EA 35 AWeaken 36)::(EA 36 (AAssign V_fullsize_smooth_downsample_membersum
  None) 37)::(EA 37 (AAssign V_fullsize_smooth_downsample_nextcolsum
  None) 38)::(EA 38 (AAssign V_fullsize_smooth_downsample_neighsum
  (Some (EAdd (EAdd (EVar V_fullsize_smooth_downsample_lastcolsum)
  (ESub (EVar V_fullsize_smooth_downsample_colsum)
  (EVar V_fullsize_smooth_downsample_membersum)))
  (EVar V_fullsize_smooth_downsample_nextcolsum)))) 39)::(EA 39 (AAssign
  V_fullsize_smooth_downsample_membersum
  (Some (EAdd (EMul (EVar V_fullsize_smooth_downsample_membersum)
  (EVar V_fullsize_smooth_downsample_memberscale))
  (EMul (EVar V_fullsize_smooth_downsample_neighsum)
  (EVar V_fullsize_smooth_downsample_neighscale))))) 40)::(EA 40 (AAssign
  V_fullsize_smooth_downsample_lastcolsum
  (Some (EVar V_fullsize_smooth_downsample_colsum))) 41)::(EA 41 (AAssign
  V_fullsize_smooth_downsample_colsum
  (Some (EVar V_fullsize_smooth_downsample_nextcolsum))) 42)::
  (EA 42 ANone 43)::(EA 43 (AAssign V_fullsize_smooth_downsample_colctr
  (Some (EAdd (EVar V_fullsize_smooth_downsample_colctr) (ENum (-1))))) 44)::
  (EA 44 ANone 45)::(EA 45 ANone 46)::(EA 46 (AAssign
  V_fullsize_smooth_downsample_z (Some (EAdd (ENum (1))
  (EVar V_fullsize_smooth_downsample_z)))) 47)::(EA 47 AWeaken 24)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_fullsize_smooth_downsample => Pedges_fullsize_smooth_downsample
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_fullsize_smooth_downsample => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_fullsize_smooth_downsample (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0)%Z
   | 3 => (-1 * s V_fullsize_smooth_downsample_z <= 0 /\ 1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_colctr <= 0)%Z
   | 4 => (-1 * s V_fullsize_smooth_downsample_colctr <= 0 /\ 1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0)%Z
   | 5 => (-1 * s V_fullsize_smooth_downsample_z <= 0 /\ 1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_colctr <= 0)%Z
   | 6 => (-1 * s V_fullsize_smooth_downsample_colctr <= 0 /\ 1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0)%Z
   | 7 => (-1 * s V_fullsize_smooth_downsample_z <= 0 /\ 1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_colctr <= 0)%Z
   | 8 => (-1 * s V_fullsize_smooth_downsample_colctr <= 0 /\ 1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ 1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 9 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ 1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ 1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_colctr <= 0)%Z
   | 10 => (-1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 11 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ 1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 12 => (1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ -1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 13 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0)%Z
   | 14 => (-1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 15 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0)%Z
   | 16 => (-1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 17 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0)%Z
   | 18 => (-1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 19 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0)%Z
   | 20 => (-1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 21 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0)%Z
   | 22 => (-1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 23 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0)%Z
   | 24 => (-1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 25 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ 1 * s V_fullsize_smooth_downsample_colctr <= 0)%Z
   | 26 => (1 * s V_fullsize_smooth_downsample_colctr <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 27 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ 1 * s V_fullsize_smooth_downsample_colctr <= 0)%Z
   | 28 => (1 * s V_fullsize_smooth_downsample_colctr <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 29 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ 1 * s V_fullsize_smooth_downsample_colctr <= 0)%Z
   | 30 => (1 * s V_fullsize_smooth_downsample_colctr <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 31 => (-1 * s V_fullsize_smooth_downsample_z <= 0 /\ 1 * s V_fullsize_smooth_downsample_colctr <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0)%Z
   | 32 => (-1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ 1 * s V_fullsize_smooth_downsample_colctr <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0)%Z
   | 33 => (-1 * s V_fullsize_smooth_downsample_z <= 0 /\ 1 * s V_fullsize_smooth_downsample_colctr <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0)%Z
   | 34 => (-1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ 1 * s V_fullsize_smooth_downsample_colctr <= 0 /\ -1 * s V_fullsize_smooth_downsample_z + 1 <= 0)%Z
   | 35 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_colctr + 1 <= 0)%Z
   | 36 => (-1 * s V_fullsize_smooth_downsample_colctr + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 37 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_colctr + 1 <= 0)%Z
   | 38 => (-1 * s V_fullsize_smooth_downsample_colctr + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 39 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_colctr + 1 <= 0)%Z
   | 40 => (-1 * s V_fullsize_smooth_downsample_colctr + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 41 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_colctr + 1 <= 0)%Z
   | 42 => (-1 * s V_fullsize_smooth_downsample_colctr + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0)%Z
   | 43 => (-1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_colctr + 1 <= 0)%Z
   | 44 => (-1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_colctr <= 0)%Z
   | 45 => (-1 * s V_fullsize_smooth_downsample_colctr <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z <= 0)%Z
   | 46 => (-1 * s V_fullsize_smooth_downsample_z <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_colctr <= 0)%Z
   | 47 => (-1 * s V_fullsize_smooth_downsample_colctr <= 0 /\ -1 * s V_fullsize_smooth_downsample_outrow <= 0 /\ -1 * s V_fullsize_smooth_downsample_compptr_dref_off12+ 1 * s V_fullsize_smooth_downsample_outrow + 1 <= 0 /\ -1 * s V_fullsize_smooth_downsample_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_fullsize_smooth_downsample (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-2 + 8 * s V_fullsize_smooth_downsample_compptr_dref_off28) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
           + max0(s V_fullsize_smooth_downsample_compptr_dref_off12) <= z)%Q
   | 2 => (s V_fullsize_smooth_downsample_z
           + max0(-2 + 8 * s V_fullsize_smooth_downsample_compptr_dref_off28) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
           + max0(s V_fullsize_smooth_downsample_compptr_dref_off12) <= z)%Q
   | 3 => (s V_fullsize_smooth_downsample_z
           + max0(-2 + 8 * s V_fullsize_smooth_downsample_compptr_dref_off28) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
           + max0(s V_fullsize_smooth_downsample_compptr_dref_off12) <= z)%Q
   | 4 => (s V_fullsize_smooth_downsample_z
           + max0(-2 + 8 * s V_fullsize_smooth_downsample_compptr_dref_off28) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
           + max0(s V_fullsize_smooth_downsample_compptr_dref_off12) <= z)%Q
   | 5 => (s V_fullsize_smooth_downsample_z
           + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
           + max0(s V_fullsize_smooth_downsample_compptr_dref_off12) <= z)%Q
   | 6 => (s V_fullsize_smooth_downsample_z
           + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
           + max0(s V_fullsize_smooth_downsample_compptr_dref_off12) <= z)%Q
   | 7 => (s V_fullsize_smooth_downsample_z
           + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
           + max0(s V_fullsize_smooth_downsample_compptr_dref_off12) <= z)%Q
   | 8 => (s V_fullsize_smooth_downsample_z
           + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)
           + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                  - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 9 => (s V_fullsize_smooth_downsample_z
           + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)
           + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                  - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 10 => (s V_fullsize_smooth_downsample_z
            + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_fullsize_smooth_downsample_compptr_dref_off12
                                             - s V_fullsize_smooth_downsample_outrow) (-1
                                                                    + s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow));
      (*-1 0*) F_max0_ge_0 (-1
                            + s V_fullsize_smooth_downsample_compptr_dref_off12
                            - s V_fullsize_smooth_downsample_outrow);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                            + s V_fullsize_smooth_downsample_output_cols)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)) (F_check_ge (0) (0)))]
     (s V_fullsize_smooth_downsample_z
      + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - 
                                                                    s V_fullsize_smooth_downsample_outrow)
      + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
             - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 12 => (s V_fullsize_smooth_downsample_z <= z)%Q
   | 13 => (s V_fullsize_smooth_downsample_z
            + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 14 => (s V_fullsize_smooth_downsample_z
            + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 15 => (s V_fullsize_smooth_downsample_z
            + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 16 => (s V_fullsize_smooth_downsample_z
            + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 17 => (s V_fullsize_smooth_downsample_z
            + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 18 => (s V_fullsize_smooth_downsample_z
            + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 19 => (s V_fullsize_smooth_downsample_z
            + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 20 => (s V_fullsize_smooth_downsample_z
            + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 21 => (s V_fullsize_smooth_downsample_z
            + max0(-2 + s V_fullsize_smooth_downsample_output_cols) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 22 => (s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + 
                                                                    s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(s V_fullsize_smooth_downsample_colctr)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_outrow * max0(s V_fullsize_smooth_downsample_colctr)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_colctr) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                 - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 23 => (s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + 
                                                                    s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(s V_fullsize_smooth_downsample_colctr)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_outrow * max0(s V_fullsize_smooth_downsample_colctr)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_colctr) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                 - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 24 => (s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + 
                                                                    s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(s V_fullsize_smooth_downsample_colctr)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_outrow * max0(s V_fullsize_smooth_downsample_colctr)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_colctr) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                 - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 25 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_fullsize_smooth_downsample_compptr_dref_off12
                                      - s V_fullsize_smooth_downsample_outrow) (1);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                              - s V_fullsize_smooth_downsample_outrow)) (F_check_ge (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_colctr)) (F_check_ge (0) (0)))]
     (s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                               + s V_fullsize_smooth_downsample_output_cols)
      - s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(s V_fullsize_smooth_downsample_colctr)
      - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                     + s V_fullsize_smooth_downsample_output_cols)
      + s V_fullsize_smooth_downsample_outrow * max0(s V_fullsize_smooth_downsample_colctr)
      + s V_fullsize_smooth_downsample_z
      - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
      + max0(s V_fullsize_smooth_downsample_colctr)
      + max0(s V_fullsize_smooth_downsample_colctr) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
      + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
             - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 26 => ((1 # 1)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_colctr) <= z)%Q
   | 27 => ((1 # 1)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_colctr) <= z)%Q
   | 28 => ((1 # 1)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_colctr) <= z)%Q
   | 29 => ((1 # 1)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_colctr) <= z)%Q
   | 30 => ((1 # 1)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_colctr) <= z)%Q
   | 31 => ((1 # 1)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            + max0(s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 32 => ((1 # 1)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            + max0(s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 33 => ((1 # 1)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            + max0(s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 34 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow) (0))) (F_max0_ge_0 (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_colctr)) (F_check_ge (0) (0))]
     (s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                               + s V_fullsize_smooth_downsample_output_cols)
      - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                     + s V_fullsize_smooth_downsample_output_cols)
      + s V_fullsize_smooth_downsample_z
      + max0(s V_fullsize_smooth_downsample_colctr)
      + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
             - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_fullsize_smooth_downsample_colctr) (1);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fullsize_smooth_downsample_colctr) (0))) (F_max0_ge_0 (s V_fullsize_smooth_downsample_colctr))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_compptr_dref_off12)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_fullsize_smooth_downsample_colctr)) (F_check_ge (s V_fullsize_smooth_downsample_colctr) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_fullsize_smooth_downsample_colctr)) (F_check_ge (s V_fullsize_smooth_downsample_colctr) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_outrow)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_fullsize_smooth_downsample_compptr_dref_off12)) (F_check_ge (s V_fullsize_smooth_downsample_compptr_dref_off12) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_colctr)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fullsize_smooth_downsample_outrow) (0))) (F_max0_ge_0 (s V_fullsize_smooth_downsample_outrow))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_colctr)) (F_check_ge (0) (0)))]
     (s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                               + s V_fullsize_smooth_downsample_output_cols)
      - s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(s V_fullsize_smooth_downsample_colctr)
      - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                     + s V_fullsize_smooth_downsample_output_cols)
      + s V_fullsize_smooth_downsample_outrow * max0(s V_fullsize_smooth_downsample_colctr)
      + s V_fullsize_smooth_downsample_z
      - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
      + max0(s V_fullsize_smooth_downsample_colctr)
      + max0(s V_fullsize_smooth_downsample_colctr) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
      + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
             - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 36 => ((1 # 1)
            - s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 37 => ((1 # 1)
            - s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 38 => ((1 # 1)
            - s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 39 => ((1 # 1)
            - s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 40 => ((1 # 1)
            - s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 41 => ((1 # 1)
            - s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 42 => ((1 # 1)
            - s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 43 => ((1 # 1)
            - s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(-1 + s V_fullsize_smooth_downsample_colctr)
            + max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                   - s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 44 => ((1 # 1)
            - s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(s V_fullsize_smooth_downsample_colctr)
            - max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + (2 # 1) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                             - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 45 => ((1 # 1)
            - s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(s V_fullsize_smooth_downsample_colctr)
            - max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + (2 # 1) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                             - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 46 => ((1 # 1)
            - s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                           - s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_outrow)
            + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                    + s V_fullsize_smooth_downsample_output_cols)
            - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                           + s V_fullsize_smooth_downsample_output_cols)
            + s V_fullsize_smooth_downsample_z
            - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
            + max0(s V_fullsize_smooth_downsample_colctr)
            - max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
            + (2 # 1) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                             - s V_fullsize_smooth_downsample_outrow)
            + max0(s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_fullsize_smooth_downsample_compptr_dref_off12
                                       - s V_fullsize_smooth_downsample_outrow) (1);
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                     - s V_fullsize_smooth_downsample_outrow)) (F_check_ge (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow) (0));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                - s V_fullsize_smooth_downsample_outrow)) (F_check_ge (-1
                                                                    + s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                - s V_fullsize_smooth_downsample_outrow)) (F_check_ge (-1
                                                                    + s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fullsize_smooth_downsample_colctr) (0))) (F_max0_ge_0 (s V_fullsize_smooth_downsample_colctr))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fullsize_smooth_downsample_colctr) (0))) (F_max0_ge_0 (s V_fullsize_smooth_downsample_colctr))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_outrow)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_fullsize_smooth_downsample_colctr)) (F_check_ge (s V_fullsize_smooth_downsample_colctr) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_compptr_dref_off12)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fullsize_smooth_downsample_compptr_dref_off12) (0))) (F_max0_ge_0 (s V_fullsize_smooth_downsample_compptr_dref_off12))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_colctr)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow) (0))) (F_max0_ge_0 (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow) (0))) (F_max0_ge_0 (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_fullsize_smooth_downsample_outrow)) (F_check_ge (s V_fullsize_smooth_downsample_outrow) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fullsize_smooth_downsample_colctr)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_fullsize_smooth_downsample_outrow)) (F_check_ge (s V_fullsize_smooth_downsample_outrow) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fullsize_smooth_downsample_compptr_dref_off12) (0))) (F_max0_ge_0 (s V_fullsize_smooth_downsample_compptr_dref_off12));
      (*-0.5 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (-1
                                                                 + s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                 - s V_fullsize_smooth_downsample_outrow) (0))) (F_max0_ge_0 (-1
                                                                    + s V_fullsize_smooth_downsample_compptr_dref_off12
                                                                    - s V_fullsize_smooth_downsample_outrow))]
     (-s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
      + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                                                     - s V_fullsize_smooth_downsample_outrow)
      + s V_fullsize_smooth_downsample_colctr * max0(s V_fullsize_smooth_downsample_outrow)
      + s V_fullsize_smooth_downsample_compptr_dref_off12 * max0(-2
                                                                 + s V_fullsize_smooth_downsample_output_cols)
      - s V_fullsize_smooth_downsample_outrow * max0(-2
                                                     + s V_fullsize_smooth_downsample_output_cols)
      + s V_fullsize_smooth_downsample_z
      - max0(-2 + s V_fullsize_smooth_downsample_output_cols)
      + max0(s V_fullsize_smooth_downsample_colctr)
      - max0(s V_fullsize_smooth_downsample_compptr_dref_off12)
      + (2 # 1) * max0(s V_fullsize_smooth_downsample_compptr_dref_off12
                       - s V_fullsize_smooth_downsample_outrow)
      + max0(s V_fullsize_smooth_downsample_outrow) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_fullsize_smooth_downsample =>
    [mkPA Q (fun n z s => ai_fullsize_smooth_downsample n s /\ annot0_fullsize_smooth_downsample n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_fullsize_smooth_downsample (proc_start P_fullsize_smooth_downsample) s1 (proc_end P_fullsize_smooth_downsample) s2 ->
    (s2 V_fullsize_smooth_downsample_z <= max0(-2
                                               + 8 * s1 V_fullsize_smooth_downsample_compptr_dref_off28) * max0(s1 V_fullsize_smooth_downsample_compptr_dref_off12)
                                          + max0(s1 V_fullsize_smooth_downsample_compptr_dref_off12))%Q.
Proof.
  prove_bound ipa admissible_ipa P_fullsize_smooth_downsample.
Qed.
