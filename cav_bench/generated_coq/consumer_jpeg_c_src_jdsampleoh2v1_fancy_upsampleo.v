Require Import pasta.Pasta.

Inductive proc: Type :=
  P_h2v1_fancy_upsample.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_h2v1_fancy_upsample_z := 1%positive.
Notation V_h2v1_fancy_upsample_cinfo_dref_off392 := 2%positive.
Notation V_h2v1_fancy_upsample_colctr := 3%positive.
Notation V_h2v1_fancy_upsample_compptr_dref_off40 := 4%positive.
Notation V_h2v1_fancy_upsample_inrow := 5%positive.
Notation V_h2v1_fancy_upsample_invalue := 6%positive.
Notation V_h2v1_fancy_upsample_cinfo := 7%positive.
Notation V_h2v1_fancy_upsample_compptr := 8%positive.
Notation V_h2v1_fancy_upsample_input_data := 9%positive.
Notation V_h2v1_fancy_upsample_output_data_ptr := 10%positive.
Definition Pedges_h2v1_fancy_upsample: list (edge proc) :=
  (EA 1 (AAssign V_h2v1_fancy_upsample_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_h2v1_fancy_upsample_colctr) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 AWeaken 4)::(EA 4 (AAssign V_h2v1_fancy_upsample_inrow
  (Some (ENum (0)))) 5)::(EA 5 ANone 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_h2v1_fancy_upsample_inrow) s) <
  (eval (EVar V_h2v1_fancy_upsample_cinfo_dref_off392) s))%Z)) 10)::
  (EA 7 (AGuard (fun s => ((eval (EVar V_h2v1_fancy_upsample_inrow) s) >=
  (eval (EVar V_h2v1_fancy_upsample_cinfo_dref_off392) s))%Z)) 8)::
  (EA 8 AWeaken 9)::(EA 10 AWeaken 11)::(EA 11 (AAssign
  V_h2v1_fancy_upsample_invalue None) 12)::(EA 12 (AAssign
  V_h2v1_fancy_upsample_colctr
  (Some (ESub (EVar V_h2v1_fancy_upsample_compptr_dref_off40)
  (ENum (2))))) 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_h2v1_fancy_upsample_colctr) s) > (eval (ENum (0))
  s))%Z)) 24)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_h2v1_fancy_upsample_colctr) s) <= (eval (ENum (0))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 (AAssign
  V_h2v1_fancy_upsample_invalue None) 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_h2v1_fancy_upsample_inrow (Some (EAdd (EVar V_h2v1_fancy_upsample_inrow)
  (ENum (1))))) 20)::(EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 (AAssign
  V_h2v1_fancy_upsample_z (Some (EAdd (ENum (1))
  (EVar V_h2v1_fancy_upsample_z)))) 23)::(EA 23 AWeaken 7)::
  (EA 24 AWeaken 25)::(EA 25 (AAssign V_h2v1_fancy_upsample_invalue
  None) 26)::(EA 26 ANone 27)::(EA 27 (AAssign V_h2v1_fancy_upsample_colctr
  (Some (EAdd (EVar V_h2v1_fancy_upsample_colctr) (ENum (-1))))) 28)::
  (EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign V_h2v1_fancy_upsample_z
  (Some (EAdd (ENum (1)) (EVar V_h2v1_fancy_upsample_z)))) 31)::
  (EA 31 AWeaken 15)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_h2v1_fancy_upsample => Pedges_h2v1_fancy_upsample
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_h2v1_fancy_upsample => 9
     end)%positive;
  var_global := var_global
}.

Definition ai_h2v1_fancy_upsample (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0)%Z
   | 3 => (-1 * s V_h2v1_fancy_upsample_z <= 0 /\ 1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_colctr <= 0)%Z
   | 4 => (-1 * s V_h2v1_fancy_upsample_colctr <= 0 /\ 1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0)%Z
   | 5 => (-1 * s V_h2v1_fancy_upsample_z <= 0 /\ 1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_colctr <= 0 /\ 1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0)%Z
   | 6 => (-1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ 1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_colctr <= 0 /\ 1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0)%Z
   | 7 => (-1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0)%Z
   | 8 => (-1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ 1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ -1 * s V_h2v1_fancy_upsample_inrow <= 0)%Z
   | 9 => (1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0)%Z
   | 10 => (-1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0)%Z
   | 11 => (-1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0)%Z
   | 12 => (-1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0)%Z
   | 13 => (-1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0)%Z
   | 14 => (-1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0)%Z
   | 15 => (-1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0)%Z
   | 16 => (-1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ 1 * s V_h2v1_fancy_upsample_colctr <= 0)%Z
   | 17 => (1 * s V_h2v1_fancy_upsample_colctr <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0)%Z
   | 18 => (-1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ 1 * s V_h2v1_fancy_upsample_colctr <= 0)%Z
   | 19 => (1 * s V_h2v1_fancy_upsample_colctr <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0)%Z
   | 20 => (-1 * s V_h2v1_fancy_upsample_z <= 0 /\ 1 * s V_h2v1_fancy_upsample_colctr <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow <= 0)%Z
   | 21 => (-1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ 1 * s V_h2v1_fancy_upsample_colctr <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0)%Z
   | 22 => (-1 * s V_h2v1_fancy_upsample_z <= 0 /\ 1 * s V_h2v1_fancy_upsample_colctr <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow <= 0)%Z
   | 23 => (-1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ 1 * s V_h2v1_fancy_upsample_colctr <= 0 /\ -1 * s V_h2v1_fancy_upsample_z + 1 <= 0)%Z
   | 24 => (-1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_colctr + 1 <= 0)%Z
   | 25 => (-1 * s V_h2v1_fancy_upsample_colctr + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0)%Z
   | 26 => (-1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_colctr + 1 <= 0)%Z
   | 27 => (-1 * s V_h2v1_fancy_upsample_colctr + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0)%Z
   | 28 => (-1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_colctr <= 0)%Z
   | 29 => (-1 * s V_h2v1_fancy_upsample_colctr <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0)%Z
   | 30 => (-1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_z <= 0 /\ -1 * s V_h2v1_fancy_upsample_colctr <= 0)%Z
   | 31 => (-1 * s V_h2v1_fancy_upsample_colctr <= 0 /\ -1 * s V_h2v1_fancy_upsample_inrow <= 0 /\ -1 * s V_h2v1_fancy_upsample_cinfo_dref_off392+ 1 * s V_h2v1_fancy_upsample_inrow + 1 <= 0 /\ -1 * s V_h2v1_fancy_upsample_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_h2v1_fancy_upsample (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
           + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392) <= z)%Q
   | 2 => (s V_h2v1_fancy_upsample_z
           + max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
           + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392) <= z)%Q
   | 3 => (s V_h2v1_fancy_upsample_z
           + max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
           + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392) <= z)%Q
   | 4 => (s V_h2v1_fancy_upsample_z
           + max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
           + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392) <= z)%Q
   | 5 => (s V_h2v1_fancy_upsample_z
           + max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
           + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                  - s V_h2v1_fancy_upsample_inrow) <= z)%Q
   | 6 => (s V_h2v1_fancy_upsample_z
           + max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
           + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                  - s V_h2v1_fancy_upsample_inrow) <= z)%Q
   | 7 => (s V_h2v1_fancy_upsample_z
           + max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
           + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                  - s V_h2v1_fancy_upsample_inrow) <= z)%Q
   | 8 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                             - s V_h2v1_fancy_upsample_inrow) (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow));
      (*-1 0*) F_max0_ge_0 (-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                            - s V_h2v1_fancy_upsample_inrow);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                            + s V_h2v1_fancy_upsample_compptr_dref_off40)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)) (F_check_ge (0) (0)))]
     (s V_h2v1_fancy_upsample_z
      + max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - 
                                                                    s V_h2v1_fancy_upsample_inrow)
      + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
             - s V_h2v1_fancy_upsample_inrow) <= z)%Q
   | 9 => (s V_h2v1_fancy_upsample_z <= z)%Q
   | 10 => hints
     [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                               + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                               - s V_h2v1_fancy_upsample_inrow)) (F_check_ge (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)) (F_check_ge (0) (0)))]
     (s V_h2v1_fancy_upsample_z
      + max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - 
                                                                    s V_h2v1_fancy_upsample_inrow)
      + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
             - s V_h2v1_fancy_upsample_inrow) <= z)%Q
   | 11 => ((1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                             - s V_h2v1_fancy_upsample_inrow)
            + s V_h2v1_fancy_upsample_z
            + max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)^2
            + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow) <= z)%Q
   | 12 => ((1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                             - s V_h2v1_fancy_upsample_inrow)
            + s V_h2v1_fancy_upsample_z
            + max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)^2
            + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow) <= z)%Q
   | 13 => (-(1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-2
                                                                    + s V_h2v1_fancy_upsample_compptr_dref_off40)
            + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                             - s V_h2v1_fancy_upsample_inrow)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)^2
            + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 14 => (-(1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-2
                                                                    + s V_h2v1_fancy_upsample_compptr_dref_off40)
            + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                             - s V_h2v1_fancy_upsample_inrow)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)^2
            + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 15 => (-(1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-2
                                                                    + s V_h2v1_fancy_upsample_compptr_dref_off40)
            + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                             - s V_h2v1_fancy_upsample_inrow)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)^2
            + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 16 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                      - s V_h2v1_fancy_upsample_inrow) (1);
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow) (0))) (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                - s V_h2v1_fancy_upsample_inrow)) (F_check_ge (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_h2v1_fancy_upsample_compptr_dref_off40)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                - s V_h2v1_fancy_upsample_inrow)) (F_check_ge (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_colctr)) (F_check_ge (0) (0)))]
     (-(1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-2
                                                                  + s V_h2v1_fancy_upsample_compptr_dref_off40)
      + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                   + 
                                                                   s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                   - 
                                                                   s V_h2v1_fancy_upsample_inrow)
      + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
      - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                       + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                       - s V_h2v1_fancy_upsample_inrow)
      + s V_h2v1_fancy_upsample_z
      + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
      + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
      + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
      - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
      - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                       - s V_h2v1_fancy_upsample_inrow)
      - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                       - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr)
      - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                       - s V_h2v1_fancy_upsample_inrow)^2
      + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
             - s V_h2v1_fancy_upsample_inrow)
      + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                       - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 17 => ((1 # 1)
            + s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-2
                                                             + s V_h2v1_fancy_upsample_compptr_dref_off40)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(s V_h2v1_fancy_upsample_colctr)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
            + max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 18 => ((1 # 1)
            + s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-2
                                                             + s V_h2v1_fancy_upsample_compptr_dref_off40)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(s V_h2v1_fancy_upsample_colctr)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
            + max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 19 => ((1 # 1)
            + s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-2
                                                             + s V_h2v1_fancy_upsample_compptr_dref_off40)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(s V_h2v1_fancy_upsample_colctr)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
            + max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 20 => ((1 # 1)
            + s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-2
                                                             + s V_h2v1_fancy_upsample_compptr_dref_off40)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(s V_h2v1_fancy_upsample_colctr)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
            + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 21 => ((1 # 1)
            + s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-2
                                                             + s V_h2v1_fancy_upsample_compptr_dref_off40)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(s V_h2v1_fancy_upsample_colctr)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
            + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 22 => ((1 # 1)
            + s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-2
                                                             + s V_h2v1_fancy_upsample_compptr_dref_off40)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(s V_h2v1_fancy_upsample_colctr)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
            + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 23 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392) (0))) (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_colctr)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_h2v1_fancy_upsample_compptr_dref_off40)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow) (0))) (F_max0_ge_0 (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_h2v1_fancy_upsample_compptr_dref_off40)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow) (0))) (F_max0_ge_0 (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_colctr)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_colctr)) (F_check_ge (0) (0))]
     (s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
      - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-2
                                                       + s V_h2v1_fancy_upsample_compptr_dref_off40)
      - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(s V_h2v1_fancy_upsample_colctr)
      + s V_h2v1_fancy_upsample_z
      + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40)
      + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
      + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
      - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
      + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
             - s V_h2v1_fancy_upsample_inrow)
      - (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                       - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr)
      + (1 # 2) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 24 => hints
     [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow) (0))) (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_colctr)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_h2v1_fancy_upsample_colctr) (0))) (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_colctr))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                               + s V_h2v1_fancy_upsample_colctr)) (F_check_ge (-1
                                                                    + s V_h2v1_fancy_upsample_colctr) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_h2v1_fancy_upsample_cinfo_dref_off392) (0))) (F_max0_ge_0 (s V_h2v1_fancy_upsample_cinfo_dref_off392))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_colctr)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_h2v1_fancy_upsample_colctr) (0))) (F_max0_ge_0 (s V_h2v1_fancy_upsample_colctr))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (0) (0)));
      (*-1.66678e-12 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_h2v1_fancy_upsample_colctr) (0))) (F_max0_ge_0 (s V_h2v1_fancy_upsample_colctr))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_h2v1_fancy_upsample_colctr)) (F_check_ge (s V_h2v1_fancy_upsample_colctr) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_h2v1_fancy_upsample_colctr)) (F_check_ge (s V_h2v1_fancy_upsample_colctr) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)) (F_check_ge (0) (0)))]
     (-(1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-2
                                                                  + s V_h2v1_fancy_upsample_compptr_dref_off40)
      + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                   + 
                                                                   s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                   - 
                                                                   s V_h2v1_fancy_upsample_inrow)
      + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
      - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                       + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                       - s V_h2v1_fancy_upsample_inrow)
      + s V_h2v1_fancy_upsample_z
      + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
      + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
      + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
      - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
      - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                       - s V_h2v1_fancy_upsample_inrow)
      - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                       - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr)
      - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                       - s V_h2v1_fancy_upsample_inrow)^2
      + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
             - s V_h2v1_fancy_upsample_inrow)
      + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                       - s V_h2v1_fancy_upsample_inrow) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 25 => (-(1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-2
                                                                    + s V_h2v1_fancy_upsample_compptr_dref_off40)
            + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(-1
                                                              + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                              - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                             - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_colctr)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)^2
            + (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_colctr)
            + (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_colctr) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow) <= z)%Q
   | 26 => (-(1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-2
                                                                    + s V_h2v1_fancy_upsample_compptr_dref_off40)
            + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(-1
                                                              + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                              - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                             - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_colctr)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)^2
            + (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_colctr)
            + (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_colctr) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow) <= z)%Q
   | 27 => (-(1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-2
                                                                    + s V_h2v1_fancy_upsample_compptr_dref_off40)
            + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(-1
                                                              + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                              - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                             - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_colctr)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)^2
            + (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_colctr)
            + (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_colctr) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow) <= z)%Q
   | 28 => (-(1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-2
                                                                    + s V_h2v1_fancy_upsample_compptr_dref_off40)
            + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(-1
                                                              + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                              - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                             - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(s V_h2v1_fancy_upsample_colctr)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            - max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)^2
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
            + (3 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 29 => (-(1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-2
                                                                    + s V_h2v1_fancy_upsample_compptr_dref_off40)
            + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(-1
                                                              + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                              - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                             - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(s V_h2v1_fancy_upsample_colctr)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            - max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)^2
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
            + (3 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 30 => (-(1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-2
                                                                    + s V_h2v1_fancy_upsample_compptr_dref_off40)
            + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
            - (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(-1
                                                              + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                              - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                             + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                             - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(s V_h2v1_fancy_upsample_colctr)
            + s V_h2v1_fancy_upsample_z
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392)
            - max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                   - s V_h2v1_fancy_upsample_inrow)
            - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)^2
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
            + (3 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                             - s V_h2v1_fancy_upsample_inrow)
            + (1 # 2) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | 31 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                         - s V_h2v1_fancy_upsample_inrow) (1);
      (*-0.25 0*) F_binom_monotonic 2 (F_max0_ge_arg (-1
                                                      + s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392) (0));
      (*-0.25 0*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_h2v1_fancy_upsample_cinfo_dref_off392) (0))) (F_max0_ge_0 (s V_h2v1_fancy_upsample_cinfo_dref_off392));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392) (0))) (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392) (0))) (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                - s V_h2v1_fancy_upsample_inrow)) (F_check_ge (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_colctr)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (s V_h2v1_fancy_upsample_cinfo_dref_off392) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (0) (0)));
      (*-0.25 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (s V_h2v1_fancy_upsample_cinfo_dref_off392) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (s V_h2v1_fancy_upsample_cinfo_dref_off392) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_colctr)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_h2v1_fancy_upsample_colctr) (0))) (F_max0_ge_0 (s V_h2v1_fancy_upsample_colctr))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_h2v1_fancy_upsample_colctr)) (F_check_ge (s V_h2v1_fancy_upsample_colctr) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_h2v1_fancy_upsample_cinfo_dref_off392)) (F_check_ge (s V_h2v1_fancy_upsample_cinfo_dref_off392) (0))]
     (-(1 # 1)
      - (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-2
                                                                   + 
                                                                   s V_h2v1_fancy_upsample_compptr_dref_off40)
      + (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(-1
                                                                   + 
                                                                   s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                   - 
                                                                   s V_h2v1_fancy_upsample_inrow)
      - (1 # 2) * s V_h2v1_fancy_upsample_cinfo_dref_off392 * max0(s V_h2v1_fancy_upsample_colctr)
      - (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(-1
                                                        + s V_h2v1_fancy_upsample_cinfo_dref_off392)
      + (1 # 2) * s V_h2v1_fancy_upsample_colctr * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                        - s V_h2v1_fancy_upsample_inrow)
      - (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(-1
                                                       + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                       - s V_h2v1_fancy_upsample_inrow)
      + (1 # 2) * s V_h2v1_fancy_upsample_inrow * max0(s V_h2v1_fancy_upsample_colctr)
      + s V_h2v1_fancy_upsample_z
      + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392)
      + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(-1
                                                                    + s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
      + (1 # 2) * max0(-2 + s V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                                                                    - s V_h2v1_fancy_upsample_inrow)
      - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392)
      - max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
             - s V_h2v1_fancy_upsample_inrow)
      - (1 # 2) * max0(-1 + s V_h2v1_fancy_upsample_cinfo_dref_off392
                       - s V_h2v1_fancy_upsample_inrow)^2
      + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392)
      + (1 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392) * max0(s V_h2v1_fancy_upsample_colctr)
      + (3 # 2) * max0(s V_h2v1_fancy_upsample_cinfo_dref_off392
                       - s V_h2v1_fancy_upsample_inrow)
      + (1 # 2) * max0(s V_h2v1_fancy_upsample_colctr) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_h2v1_fancy_upsample =>
    [mkPA Q (fun n z s => ai_h2v1_fancy_upsample n s /\ annot0_h2v1_fancy_upsample n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_h2v1_fancy_upsample (proc_start P_h2v1_fancy_upsample) s1 (proc_end P_h2v1_fancy_upsample) s2 ->
    (s2 V_h2v1_fancy_upsample_z <= max0(-2
                                        + s1 V_h2v1_fancy_upsample_compptr_dref_off40) * max0(s1 V_h2v1_fancy_upsample_cinfo_dref_off392)
                                   + max0(s1 V_h2v1_fancy_upsample_cinfo_dref_off392))%Q.
Proof.
  prove_bound ipa admissible_ipa P_h2v1_fancy_upsample.
Qed.
