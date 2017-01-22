Require Import pasta.Pasta.

Notation IDpdf_reset_page_z := 1%positive.
Notation IDpdf_reset_page_i := 2%positive.
Notation IDpdf_reset_page_pdev := 3%positive.
Definition pdf_reset_page : graph := {|
  g_start := 1%positive;
  g_end := 7%positive;
  g_edges := (1%positive,(AAssign IDpdf_reset_page_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDpdf_reset_page_i (Some (ENum (0)))),
             3%positive)::(3%positive,ANone,4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDpdf_reset_page_i)
             s) < (eval (ENum (5)) s))%Z)),8%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDpdf_reset_page_i)
             s) >= (eval (ENum (5)) s))%Z)),6%positive)::
             (6%positive,AWeaken,7%positive)::
             (8%positive,AWeaken,9%positive)::
             (9%positive,ANone,10%positive)::
             (10%positive,(AAssign IDpdf_reset_page_i
             (Some (EAdd (EVar IDpdf_reset_page_i) (ENum (1))))),11%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDpdf_reset_page_z (Some (EAdd (ENum (1))
             (EVar IDpdf_reset_page_z)))),14%positive)::
             (14%positive,AWeaken,5%positive)::nil
|}.

Definition pdf_reset_page_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDpdf_reset_page_z) <= 0 /\ -1 * (s IDpdf_reset_page_z) <= 0)%Z
    | 3%positive => (-1 * (s IDpdf_reset_page_z) <= 0 /\ 1 * (s IDpdf_reset_page_z) <= 0 /\ 1 * (s IDpdf_reset_page_i) <= 0 /\ -1 * (s IDpdf_reset_page_i) <= 0)%Z
    | 4%positive => (-1 * (s IDpdf_reset_page_i) <= 0 /\ 1 * (s IDpdf_reset_page_i) <= 0 /\ 1 * (s IDpdf_reset_page_z) <= 0 /\ -1 * (s IDpdf_reset_page_z) <= 0)%Z
    | 5%positive => (-1 * (s IDpdf_reset_page_z) <= 0 /\ -1 * (s IDpdf_reset_page_i) <= 0 /\ 1 * (s IDpdf_reset_page_i) + -5 <= 0)%Z
    | 6%positive => (1 * (s IDpdf_reset_page_i) + -5 <= 0 /\ -1 * (s IDpdf_reset_page_z) <= 0 /\ -1 * (s IDpdf_reset_page_i) + 5 <= 0)%Z
    | 7%positive => (-1 * (s IDpdf_reset_page_i) + 5 <= 0 /\ -1 * (s IDpdf_reset_page_z) <= 0 /\ 1 * (s IDpdf_reset_page_i) + -5 <= 0)%Z
    | 8%positive => (-1 * (s IDpdf_reset_page_i) <= 0 /\ -1 * (s IDpdf_reset_page_z) <= 0 /\ 1 * (s IDpdf_reset_page_i) + -4 <= 0)%Z
    | 9%positive => (1 * (s IDpdf_reset_page_i) + -4 <= 0 /\ -1 * (s IDpdf_reset_page_z) <= 0 /\ -1 * (s IDpdf_reset_page_i) <= 0)%Z
    | 10%positive => (-1 * (s IDpdf_reset_page_i) <= 0 /\ -1 * (s IDpdf_reset_page_z) <= 0 /\ 1 * (s IDpdf_reset_page_i) + -4 <= 0)%Z
    | 11%positive => (-1 * (s IDpdf_reset_page_z) <= 0 /\ -1 * (s IDpdf_reset_page_i) + 1 <= 0 /\ 1 * (s IDpdf_reset_page_i) + -5 <= 0)%Z
    | 12%positive => (1 * (s IDpdf_reset_page_i) + -5 <= 0 /\ -1 * (s IDpdf_reset_page_i) + 1 <= 0 /\ -1 * (s IDpdf_reset_page_z) <= 0)%Z
    | 13%positive => (-1 * (s IDpdf_reset_page_z) <= 0 /\ -1 * (s IDpdf_reset_page_i) + 1 <= 0 /\ 1 * (s IDpdf_reset_page_i) + -5 <= 0)%Z
    | 14%positive => (1 * (s IDpdf_reset_page_i) + -5 <= 0 /\ -1 * (s IDpdf_reset_page_i) + 1 <= 0 /\ -1 * (s IDpdf_reset_page_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition pdf_reset_page_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((5 # 1))%Q
    | 2%positive => ((5 # 1) + (s IDpdf_reset_page_z))%Q
    | 3%positive => ((s IDpdf_reset_page_z)
                     + max0(5 - (s IDpdf_reset_page_i)))%Q
    | 4%positive => ((s IDpdf_reset_page_z)
                     + max0(5 - (s IDpdf_reset_page_i)))%Q
    | 5%positive => ((s IDpdf_reset_page_z)
                     + max0(5 - (s IDpdf_reset_page_i)))%Q
    | 6%positive => ((s IDpdf_reset_page_z)
                     + max0(5 - (s IDpdf_reset_page_i)))%Q
    | 7%positive => ((s IDpdf_reset_page_z))%Q
    | 8%positive => ((s IDpdf_reset_page_z)
                     + max0(5 - (s IDpdf_reset_page_i)))%Q
    | 9%positive => ((1 # 1) + (s IDpdf_reset_page_z)
                     + max0(4 - (s IDpdf_reset_page_i)))%Q
    | 10%positive => ((1 # 1) + (s IDpdf_reset_page_z)
                      + max0(4 - (s IDpdf_reset_page_i)))%Q
    | 11%positive => ((1 # 1) + (s IDpdf_reset_page_z)
                      + max0(5 - (s IDpdf_reset_page_i)))%Q
    | 12%positive => ((1 # 1) + (s IDpdf_reset_page_z)
                      + max0(5 - (s IDpdf_reset_page_i)))%Q
    | 13%positive => ((1 # 1) + (s IDpdf_reset_page_z)
                      + max0(5 - (s IDpdf_reset_page_i)))%Q
    | 14%positive => ((s IDpdf_reset_page_z)
                      + max0(5 - (s IDpdf_reset_page_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition pdf_reset_page_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (5
                                                            - (s IDpdf_reset_page_i)) (4
                                                                    - (s IDpdf_reset_page_i)));
                     (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                                - (s IDpdf_reset_page_i))) (F_check_ge (0) (0))]
    | 7%positive => []
    | 8%positive => [(*-1 0*) F_max0_pre_decrement (5
                                                    - (s IDpdf_reset_page_i)) (1)]
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | _ => []
  end.


Theorem pdf_reset_page_ai_correct:
  forall s p' s', steps (g_start pdf_reset_page) s (g_edges pdf_reset_page) p' s' -> pdf_reset_page_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem pdf_reset_page_pot_correct:
  forall s p' s',
    steps (g_start pdf_reset_page) s (g_edges pdf_reset_page) p' s' ->
    (pdf_reset_page_pot (g_start pdf_reset_page) s >= pdf_reset_page_pot p' s')%Q.
Proof.
  check_lp pdf_reset_page_ai_correct pdf_reset_page_hints.
Qed.

