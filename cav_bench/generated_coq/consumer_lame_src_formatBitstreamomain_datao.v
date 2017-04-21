Require Import pasta.Pasta.

Inductive proc: Type :=
  P_main_data.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_main_data_z := 1%positive.
Notation V_main_data_bits := 2%positive.
Notation V_main_data_ch := 3%positive.
Notation V_main_data_fi_dref_off4 := 4%positive.
Notation V_main_data_fi_dref_off8 := 5%positive.
Notation V_main_data_gr := 6%positive.
Notation V_main_data_fi := 7%positive.
Notation V_main_data_results := 8%positive.
Definition Pedges_main_data: list (edge proc) :=
  (EA 1 (AAssign V_main_data_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_main_data_bits (Some (ENum (0)))) 3)::(EA 3 (AAssign V_main_data_gr
  (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_main_data_gr) s) <
  (eval (EVar V_main_data_fi_dref_off4) s))%Z)) 11)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_main_data_gr) s) >=
  (eval (EVar V_main_data_fi_dref_off4) s))%Z)) 7)::(EA 7 AWeaken 8)::
  (EA 8 (AAssign V_main_data_bits None) 9)::(EA 9 AWeaken 10)::
  (EA 11 AWeaken 12)::(EA 12 (AAssign V_main_data_ch (Some (ENum (0)))) 13)::
  (EA 13 ANone 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_main_data_ch) s) <
  (eval (EVar V_main_data_fi_dref_off8) s))%Z)) 23)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_main_data_ch) s) >=
  (eval (EVar V_main_data_fi_dref_off8) s))%Z)) 16)::(EA 16 AWeaken 17)::
  (EA 17 ANone 18)::(EA 18 (AAssign V_main_data_gr
  (Some (EAdd (EVar V_main_data_gr) (ENum (1))))) 19)::(EA 19 ANone 20)::
  (EA 20 ANone 21)::(EA 21 (AAssign V_main_data_z (Some (EAdd (ENum (1))
  (EVar V_main_data_z)))) 22)::(EA 22 AWeaken 6)::(EA 23 AWeaken 24)::
  (EA 24 (AAssign V_main_data_bits None) 25)::(EA 25 (AAssign
  V_main_data_bits None) 26)::(EA 26 (AAssign V_main_data_bits None) 27)::
  (EA 27 ANone 28)::(EA 28 (AAssign V_main_data_ch
  (Some (EAdd (EVar V_main_data_ch) (ENum (1))))) 29)::(EA 29 ANone 30)::
  (EA 30 ANone 31)::(EA 31 (AAssign V_main_data_z (Some (EAdd (ENum (1))
  (EVar V_main_data_z)))) 32)::(EA 32 AWeaken 15)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_main_data => Pedges_main_data
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_main_data => 10
     end)%positive;
  var_global := var_global
}.

Definition ai_main_data (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_z <= 0)%Z
   | 3 => (-1 * s V_main_data_z <= 0 /\ 1 * s V_main_data_z <= 0 /\ 1 * s V_main_data_bits <= 0 /\ -1 * s V_main_data_bits <= 0)%Z
   | 4 => (-1 * s V_main_data_bits <= 0 /\ 1 * s V_main_data_bits <= 0 /\ 1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_z <= 0 /\ 1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_gr <= 0)%Z
   | 5 => (-1 * s V_main_data_gr <= 0 /\ 1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_z <= 0 /\ 1 * s V_main_data_z <= 0 /\ 1 * s V_main_data_bits <= 0 /\ -1 * s V_main_data_bits <= 0)%Z
   | 6 => (-1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_gr <= 0)%Z
   | 7 => (-1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_z <= 0 /\ 1 * s V_main_data_fi_dref_off4+ -1 * s V_main_data_gr <= 0)%Z
   | 8 => (1 * s V_main_data_fi_dref_off4+ -1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_gr <= 0)%Z
   | 9 => (-1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_z <= 0 /\ 1 * s V_main_data_fi_dref_off4+ -1 * s V_main_data_gr <= 0)%Z
   | 10 => (1 * s V_main_data_fi_dref_off4+ -1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_gr <= 0)%Z
   | 11 => (-1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0)%Z
   | 12 => (-1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_gr <= 0)%Z
   | 13 => (-1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ 1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_ch <= 0)%Z
   | 14 => (-1 * s V_main_data_ch <= 0 /\ 1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_gr <= 0)%Z
   | 15 => (-1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_gr <= 0)%Z
   | 16 => (-1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_ch+ 1 * s V_main_data_fi_dref_off8 <= 0)%Z
   | 17 => (-1 * s V_main_data_ch+ 1 * s V_main_data_fi_dref_off8 <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_gr <= 0)%Z
   | 18 => (-1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_ch+ 1 * s V_main_data_fi_dref_off8 <= 0)%Z
   | 19 => (-1 * s V_main_data_ch+ 1 * s V_main_data_fi_dref_off8 <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr <= 0)%Z
   | 20 => (-1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_ch+ 1 * s V_main_data_fi_dref_off8 <= 0)%Z
   | 21 => (-1 * s V_main_data_ch+ 1 * s V_main_data_fi_dref_off8 <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr <= 0)%Z
   | 22 => (-1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_ch+ 1 * s V_main_data_fi_dref_off8 <= 0 /\ -1 * s V_main_data_z + 1 <= 0)%Z
   | 23 => (-1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_z <= 0 /\ 1 * s V_main_data_ch+ -1 * s V_main_data_fi_dref_off8 + 1 <= 0)%Z
   | 24 => (1 * s V_main_data_ch+ -1 * s V_main_data_fi_dref_off8 + 1 <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_gr <= 0)%Z
   | 25 => (-1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_z <= 0 /\ 1 * s V_main_data_ch+ -1 * s V_main_data_fi_dref_off8 + 1 <= 0)%Z
   | 26 => (1 * s V_main_data_ch+ -1 * s V_main_data_fi_dref_off8 + 1 <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_gr <= 0)%Z
   | 27 => (-1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_z <= 0 /\ 1 * s V_main_data_ch+ -1 * s V_main_data_fi_dref_off8 + 1 <= 0)%Z
   | 28 => (1 * s V_main_data_ch+ -1 * s V_main_data_fi_dref_off8 + 1 <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_ch <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_gr <= 0)%Z
   | 29 => (-1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_z <= 0 /\ 1 * s V_main_data_ch+ -1 * s V_main_data_fi_dref_off8 <= 0 /\ -1 * s V_main_data_ch + 1 <= 0)%Z
   | 30 => (-1 * s V_main_data_ch + 1 <= 0 /\ 1 * s V_main_data_ch+ -1 * s V_main_data_fi_dref_off8 <= 0 /\ -1 * s V_main_data_z <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_gr <= 0)%Z
   | 31 => (-1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_z <= 0 /\ 1 * s V_main_data_ch+ -1 * s V_main_data_fi_dref_off8 <= 0 /\ -1 * s V_main_data_ch + 1 <= 0)%Z
   | 32 => (-1 * s V_main_data_ch + 1 <= 0 /\ 1 * s V_main_data_ch+ -1 * s V_main_data_fi_dref_off8 <= 0 /\ -1 * s V_main_data_fi_dref_off4+ 1 * s V_main_data_gr + 1 <= 0 /\ -1 * s V_main_data_gr <= 0 /\ -1 * s V_main_data_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_main_data (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_main_data_fi_dref_off4)
           + max0(s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 2 => (s V_main_data_z + max0(s V_main_data_fi_dref_off4)
           + max0(s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 3 => (s V_main_data_z + max0(s V_main_data_fi_dref_off4)
           + max0(s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 4 => (s V_main_data_z
           + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
           + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 5 => (s V_main_data_z
           + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
           + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 6 => (s V_main_data_z
           + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
           + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 7 => (s V_main_data_z
           + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
           + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 8 => (s V_main_data_z
           + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
           + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_main_data_fi_dref_off4
                                             - s V_main_data_gr) (-1
                                                                  + s V_main_data_fi_dref_off4
                                                                  - s V_main_data_gr));
      (*-1 0*) F_max0_ge_0 (-1 + s V_main_data_fi_dref_off4
                            - s V_main_data_gr);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_main_data_fi_dref_off4
                                                            - s V_main_data_gr)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_main_data_fi_dref_off8)) (F_check_ge (0) (0)))]
     (s V_main_data_z + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
      + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 10 => (s V_main_data_z <= z)%Q
   | 11 => (s V_main_data_z
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 12 => (s V_main_data_z
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 13 => (s V_main_data_fi_dref_off4 * max0(-s V_main_data_ch
                                              + s V_main_data_fi_dref_off8)
            - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
            + s V_main_data_z
            - max0(-1 + s V_main_data_fi_dref_off4) * max0(-s V_main_data_ch
                                                           + s V_main_data_fi_dref_off8)
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 14 => hints
     [(*-0.111111 0*) F_binom_monotonic 2 (F_max0_ge_arg (s V_main_data_fi_dref_off4
                                                          - s V_main_data_gr)) (F_check_ge (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr) (0));
      (*-0.222222 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr) (0))) (F_max0_ge_0 (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr))]
     (s V_main_data_fi_dref_off4 * max0(-s V_main_data_ch
                                        + s V_main_data_fi_dref_off8)
      - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
      + s V_main_data_z
      - max0(-1 + s V_main_data_fi_dref_off4) * max0(-s V_main_data_ch
                                                     + s V_main_data_fi_dref_off8)
      + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
      + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
      + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 15 => (-(1 # 3) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            + s V_main_data_fi_dref_off4 * max0(-s V_main_data_ch
                                                + s V_main_data_fi_dref_off8)
            - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 3) * s V_main_data_gr + (1 # 9) * s V_main_data_gr^2
            + s V_main_data_z
            - max0(-1 + s V_main_data_fi_dref_off4) * max0(-s V_main_data_ch
                                                           + s V_main_data_fi_dref_off8)
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + (4 # 3) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8)
            - (1 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)^2 <= z)%Q
   | 16 => hints
     [(*0 1.22222*) F_max0_pre_decrement 1 (s V_main_data_fi_dref_off4
                                            - s V_main_data_gr) (1);
      (*-0.111111 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    + 
                                                                    s V_main_data_fi_dref_off4
                                                                    - 
                                                                    s V_main_data_gr)) (F_check_ge (-1
                                                                    + s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr)) (F_check_ge (0) (0)));
      (*-0.111111 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr) (0))) (F_max0_ge_0 (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    + s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr)) (F_check_ge (0) (0)));
      (*-0.111111 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr) (0))) (F_max0_ge_0 (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_main_data_fi_dref_off4
                                                              - s V_main_data_gr)) (F_check_ge (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_main_data_fi_dref_off8)) (F_check_ge (0) (0)))]
     (-(1 # 3) * s V_main_data_fi_dref_off4
      - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
      + s V_main_data_fi_dref_off4 * max0(-s V_main_data_ch
                                          + s V_main_data_fi_dref_off8)
      - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
      + (1 # 9) * s V_main_data_fi_dref_off4^2 + (1 # 3) * s V_main_data_gr
      + (1 # 9) * s V_main_data_gr^2 + s V_main_data_z
      - max0(-1 + s V_main_data_fi_dref_off4) * max0(-s V_main_data_ch
                                                     + s V_main_data_fi_dref_off8)
      + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
      + (4 # 3) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
      + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8)
      - (1 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)^2 <= z)%Q
   | 17 => ((11 # 9) - (1 # 3) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            - (1 # 9) * s V_main_data_fi_dref_off4 * max0(-1
                                                          + s V_main_data_fi_dref_off4
                                                          - s V_main_data_gr)
            + s V_main_data_fi_dref_off4 * max0(-s V_main_data_ch
                                                + s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 3) * s V_main_data_gr
            + (1 # 9) * s V_main_data_gr * max0(-1
                                                + s V_main_data_fi_dref_off4
                                                - s V_main_data_gr)
            - s V_main_data_gr * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_gr^2 + s V_main_data_z
            - max0(-1 + s V_main_data_fi_dref_off4) * max0(-s V_main_data_ch
                                                           + s V_main_data_fi_dref_off8)
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + (11 # 9) * max0(-1 + s V_main_data_fi_dref_off4
                              - s V_main_data_gr) <= z)%Q
   | 18 => ((11 # 9) - (1 # 3) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            - (1 # 9) * s V_main_data_fi_dref_off4 * max0(-1
                                                          + s V_main_data_fi_dref_off4
                                                          - s V_main_data_gr)
            + s V_main_data_fi_dref_off4 * max0(-s V_main_data_ch
                                                + s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 3) * s V_main_data_gr
            + (1 # 9) * s V_main_data_gr * max0(-1
                                                + s V_main_data_fi_dref_off4
                                                - s V_main_data_gr)
            - s V_main_data_gr * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_gr^2 + s V_main_data_z
            - max0(-1 + s V_main_data_fi_dref_off4) * max0(-s V_main_data_ch
                                                           + s V_main_data_fi_dref_off8)
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + (11 # 9) * max0(-1 + s V_main_data_fi_dref_off4
                              - s V_main_data_gr) <= z)%Q
   | 19 => ((1 # 1) - (1 # 9) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            + s V_main_data_fi_dref_off4 * max0(-s V_main_data_ch
                                                + s V_main_data_fi_dref_off8)
            - (1 # 9) * s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off4
                                                          - s V_main_data_gr)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 9) * s V_main_data_gr
            + (1 # 9) * s V_main_data_gr * max0(s V_main_data_fi_dref_off4
                                                - s V_main_data_gr)
            - s V_main_data_gr * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_gr^2 + s V_main_data_z
            - max0(-1 + s V_main_data_fi_dref_off4) * max0(-s V_main_data_ch
                                                           + s V_main_data_fi_dref_off8)
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + (10 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 20 => ((1 # 1) - (1 # 9) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            + s V_main_data_fi_dref_off4 * max0(-s V_main_data_ch
                                                + s V_main_data_fi_dref_off8)
            - (1 # 9) * s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off4
                                                          - s V_main_data_gr)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 9) * s V_main_data_gr
            + (1 # 9) * s V_main_data_gr * max0(s V_main_data_fi_dref_off4
                                                - s V_main_data_gr)
            - s V_main_data_gr * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_gr^2 + s V_main_data_z
            - max0(-1 + s V_main_data_fi_dref_off4) * max0(-s V_main_data_ch
                                                           + s V_main_data_fi_dref_off8)
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + (10 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 21 => ((1 # 1) - (1 # 9) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            + s V_main_data_fi_dref_off4 * max0(-s V_main_data_ch
                                                + s V_main_data_fi_dref_off8)
            - (1 # 9) * s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off4
                                                          - s V_main_data_gr)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 9) * s V_main_data_gr
            + (1 # 9) * s V_main_data_gr * max0(s V_main_data_fi_dref_off4
                                                - s V_main_data_gr)
            - s V_main_data_gr * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_gr^2 + s V_main_data_z
            - max0(-1 + s V_main_data_fi_dref_off4) * max0(-s V_main_data_ch
                                                           + s V_main_data_fi_dref_off8)
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + (10 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 22 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (-s V_main_data_ch
                                            + s V_main_data_fi_dref_off8) (-1
                                                                    - s V_main_data_ch
                                                                    + s V_main_data_fi_dref_off8));
      (*0 0.111111*) F_binom_monotonic 2 (F_max0_le_arg (F_check_ge (s V_main_data_fi_dref_off4
                                                                    - 
                                                                    s V_main_data_gr) (0))) (F_max0_ge_0 (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_main_data_fi_dref_off4) (0))) (F_max0_ge_0 (-1
                                                                    + s V_main_data_fi_dref_off4))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_main_data_ch
                                                                    + s V_main_data_fi_dref_off8)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                              + s V_main_data_fi_dref_off4)) (F_check_ge (-1
                                                                    + s V_main_data_fi_dref_off4) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_main_data_fi_dref_off8)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr) (0))) (F_max0_ge_0 (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_main_data_fi_dref_off8)) (F_check_ge (0) (0)));
      (*-0.111111 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_main_data_fi_dref_off4
                                                                    - 
                                                                    s V_main_data_gr)) (F_check_ge (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_main_data_fi_dref_off4
                                                                    - s V_main_data_gr)) (F_check_ge (0) (0)));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1 - s V_main_data_ch
                                                 + s V_main_data_fi_dref_off8)) (F_check_ge (0) (0))]
     (-(1 # 9) * s V_main_data_fi_dref_off4
      - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
      + s V_main_data_fi_dref_off4 * max0(-s V_main_data_ch
                                          + s V_main_data_fi_dref_off8)
      - (1 # 9) * s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off4
                                                    - s V_main_data_gr)
      + (1 # 9) * s V_main_data_fi_dref_off4^2 + (1 # 9) * s V_main_data_gr
      + (1 # 9) * s V_main_data_gr * max0(s V_main_data_fi_dref_off4
                                          - s V_main_data_gr)
      - s V_main_data_gr * max0(s V_main_data_fi_dref_off8)
      + (1 # 9) * s V_main_data_gr^2 + s V_main_data_z
      - max0(-1 + s V_main_data_fi_dref_off4) * max0(-s V_main_data_ch
                                                     + s V_main_data_fi_dref_off8)
      + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
      + (10 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
      + max0(s V_main_data_fi_dref_off8) <= z)%Q
   | 23 => hints
     [(*0 1*) F_max0_pre_decrement 1 (-s V_main_data_ch
                                      + s V_main_data_fi_dref_off8) (1);
      (*0 0.333333*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    - s V_main_data_ch
                                                                    + s V_main_data_fi_dref_off8) (0))) (F_max0_ge_0 (-1
                                                                    - s V_main_data_ch
                                                                    + s V_main_data_fi_dref_off8))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_main_data_fi_dref_off4)) (F_check_ge (0) (0)));
      (*0 0.333333*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                    - 
                                                                    s V_main_data_ch
                                                                    + 
                                                                    s V_main_data_fi_dref_off8)) (F_check_ge (-1
                                                                    - s V_main_data_ch
                                                                    + s V_main_data_fi_dref_off8) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_main_data_fi_dref_off4)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_main_data_fi_dref_off4) (0))) (F_max0_ge_0 (-1
                                                                    + s V_main_data_fi_dref_off4))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_main_data_ch
                                                                    + s V_main_data_fi_dref_off8)) (F_check_ge (0) (0)))]
     (-(1 # 3) * s V_main_data_fi_dref_off4
      - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
      + s V_main_data_fi_dref_off4 * max0(-s V_main_data_ch
                                          + s V_main_data_fi_dref_off8)
      - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
      + (1 # 9) * s V_main_data_fi_dref_off4^2 + (1 # 3) * s V_main_data_gr
      + (1 # 9) * s V_main_data_gr^2 + s V_main_data_z
      - max0(-1 + s V_main_data_fi_dref_off4) * max0(-s V_main_data_ch
                                                     + s V_main_data_fi_dref_off8)
      + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
      + (4 # 3) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
      + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8)
      - (1 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)^2 <= z)%Q
   | 24 => ((1 # 1) - (1 # 3) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 3) * s V_main_data_gr + (1 # 9) * s V_main_data_gr^2
            + s V_main_data_z
            + max0(-1 - s V_main_data_ch + s V_main_data_fi_dref_off8)
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + (4 # 3) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8)
            - (1 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)^2 <= z)%Q
   | 25 => ((1 # 1) - (1 # 3) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 3) * s V_main_data_gr + (1 # 9) * s V_main_data_gr^2
            + s V_main_data_z
            + max0(-1 - s V_main_data_ch + s V_main_data_fi_dref_off8)
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + (4 # 3) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8)
            - (1 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)^2 <= z)%Q
   | 26 => ((1 # 1) - (1 # 3) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 3) * s V_main_data_gr + (1 # 9) * s V_main_data_gr^2
            + s V_main_data_z
            + max0(-1 - s V_main_data_ch + s V_main_data_fi_dref_off8)
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + (4 # 3) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8)
            - (1 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)^2 <= z)%Q
   | 27 => ((1 # 1) - (1 # 3) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 3) * s V_main_data_gr + (1 # 9) * s V_main_data_gr^2
            + s V_main_data_z
            + max0(-1 - s V_main_data_ch + s V_main_data_fi_dref_off8)
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + (4 # 3) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8)
            - (1 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)^2 <= z)%Q
   | 28 => ((1 # 1) - (1 # 3) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 3) * s V_main_data_gr + (1 # 9) * s V_main_data_gr^2
            + s V_main_data_z
            + max0(-1 - s V_main_data_ch + s V_main_data_fi_dref_off8)
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + (4 # 3) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8)
            - (1 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)^2 <= z)%Q
   | 29 => ((1 # 1) - (1 # 3) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 3) * s V_main_data_gr + (1 # 9) * s V_main_data_gr^2
            + s V_main_data_z
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + max0(-s V_main_data_ch + s V_main_data_fi_dref_off8)
            + (4 # 3) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8)
            - (1 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)^2 <= z)%Q
   | 30 => ((1 # 1) - (1 # 3) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 3) * s V_main_data_gr + (1 # 9) * s V_main_data_gr^2
            + s V_main_data_z
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + max0(-s V_main_data_ch + s V_main_data_fi_dref_off8)
            + (4 # 3) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8)
            - (1 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)^2 <= z)%Q
   | 31 => ((1 # 1) - (1 # 3) * s V_main_data_fi_dref_off4
            - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
            - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
            + (1 # 9) * s V_main_data_fi_dref_off4^2
            + (1 # 3) * s V_main_data_gr + (1 # 9) * s V_main_data_gr^2
            + s V_main_data_z
            + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
            + max0(-s V_main_data_ch + s V_main_data_fi_dref_off8)
            + (4 # 3) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
            + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8)
            - (1 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)^2 <= z)%Q
   | 32 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                              + s V_main_data_fi_dref_off4)) (F_check_ge (-1
                                                                    + s V_main_data_fi_dref_off4) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_main_data_ch
                                                                    + s V_main_data_fi_dref_off8)) (F_check_ge (0) (0)))]
     (-(1 # 3) * s V_main_data_fi_dref_off4
      - (2 # 9) * s V_main_data_fi_dref_off4 * s V_main_data_gr
      - s V_main_data_fi_dref_off4 * max0(s V_main_data_fi_dref_off8)
      + (1 # 9) * s V_main_data_fi_dref_off4^2 + (1 # 3) * s V_main_data_gr
      + (1 # 9) * s V_main_data_gr^2 + s V_main_data_z
      + max0(-1 + s V_main_data_fi_dref_off4) * max0(s V_main_data_fi_dref_off8)
      + max0(-s V_main_data_ch + s V_main_data_fi_dref_off8)
      + (4 # 3) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)
      + max0(s V_main_data_fi_dref_off4 - s V_main_data_gr) * max0(s V_main_data_fi_dref_off8)
      - (1 # 9) * max0(s V_main_data_fi_dref_off4 - s V_main_data_gr)^2 <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_main_data =>
    [mkPA Q (fun n z s => ai_main_data n s /\ annot0_main_data n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_main_data (proc_start P_main_data) s1 (proc_end P_main_data) s2 ->
    (s2 V_main_data_z <= max0(s1 V_main_data_fi_dref_off4)
                         + max0(s1 V_main_data_fi_dref_off4) * max0(s1 V_main_data_fi_dref_off8))%Q.
Proof.
  prove_bound ipa admissible_ipa P_main_data.
Qed.
