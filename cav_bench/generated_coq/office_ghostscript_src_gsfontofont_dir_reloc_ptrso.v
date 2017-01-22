Require Import pasta.Pasta.

Notation IDfont_dir_reloc_ptrs_z := 1%positive.
Notation IDfont_dir_reloc_ptrs__tmp := 2%positive.
Notation IDfont_dir_reloc_ptrs_chi := 3%positive.
Notation IDfont_dir_reloc_ptrs_vptr_dref_off48_off40 := 4%positive.
Notation IDfont_dir_reloc_ptrs_gcst := 5%positive.
Notation IDfont_dir_reloc_ptrs_size := 6%positive.
Notation IDfont_dir_reloc_ptrs_vptr := 7%positive.
Definition font_dir_reloc_ptrs : graph := {|
  g_start := 1%positive;
  g_end := 8%positive;
  g_edges := (1%positive,(AAssign IDfont_dir_reloc_ptrs_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDfont_dir_reloc_ptrs__tmp
             (Some (EVar IDfont_dir_reloc_ptrs_size))),3%positive)::
             (3%positive,(AAssign IDfont_dir_reloc_ptrs_chi
             (Some (EVar IDfont_dir_reloc_ptrs_vptr_dref_off48_off40))),
             4%positive)::(4%positive,ANone,5%positive)::
             (5%positive,AWeaken,6%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDfont_dir_reloc_ptrs_chi) s) >=
             (eval (ENum (0)) s))%Z)),9%positive)::
             (6%positive,(AGuard
             (fun s => ((eval (EVar IDfont_dir_reloc_ptrs_chi) s) <
             (eval (ENum (0)) s))%Z)),7%positive)::
             (7%positive,AWeaken,8%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,ANone,11%positive)::
             (10%positive,ANone,12%positive)::
             (11%positive,ANone,12%positive)::
             (12%positive,ANone,13%positive)::
             (13%positive,(AAssign IDfont_dir_reloc_ptrs_chi
             (Some (EAdd (EVar IDfont_dir_reloc_ptrs_chi) (ENum (-1))))),
             14%positive)::(14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDfont_dir_reloc_ptrs_z
             (Some (EAdd (ENum (1)) (EVar IDfont_dir_reloc_ptrs_z)))),
             17%positive)::(17%positive,AWeaken,6%positive)::nil
|}.

Definition font_dir_reloc_ptrs_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDfont_dir_reloc_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_reloc_ptrs_z) <= 0)%Z
    | 3%positive => (-1 * (s IDfont_dir_reloc_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_reloc_ptrs_z) <= 0)%Z
    | 4%positive => (1 * (s IDfont_dir_reloc_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_reloc_ptrs_z) <= 0)%Z
    | 5%positive => (-1 * (s IDfont_dir_reloc_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_reloc_ptrs_z) <= 0)%Z
    | 6%positive => (-1 * (s IDfont_dir_reloc_ptrs_z) <= 0)%Z
    | 7%positive => (-1 * (s IDfont_dir_reloc_ptrs_z) <= 0 /\ 1 * (s IDfont_dir_reloc_ptrs_chi) + 1 <= 0)%Z
    | 8%positive => (1 * (s IDfont_dir_reloc_ptrs_chi) + 1 <= 0 /\ -1 * (s IDfont_dir_reloc_ptrs_z) <= 0)%Z
    | 9%positive => (-1 * (s IDfont_dir_reloc_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_reloc_ptrs_chi) <= 0)%Z
    | 10%positive => (-1 * (s IDfont_dir_reloc_ptrs_chi) <= 0 /\ -1 * (s IDfont_dir_reloc_ptrs_z) <= 0)%Z
    | 11%positive => (-1 * (s IDfont_dir_reloc_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_reloc_ptrs_chi) <= 0)%Z
    | 12%positive => (-1 * (s IDfont_dir_reloc_ptrs_chi) <= 0 /\ -1 * (s IDfont_dir_reloc_ptrs_z) <= 0)%Z
    | 13%positive => (-1 * (s IDfont_dir_reloc_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_reloc_ptrs_chi) <= 0)%Z
    | 14%positive => (-1 * (s IDfont_dir_reloc_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_reloc_ptrs_chi) + -1 <= 0)%Z
    | 15%positive => (-1 * (s IDfont_dir_reloc_ptrs_chi) + -1 <= 0 /\ -1 * (s IDfont_dir_reloc_ptrs_z) <= 0)%Z
    | 16%positive => (-1 * (s IDfont_dir_reloc_ptrs_z) <= 0 /\ -1 * (s IDfont_dir_reloc_ptrs_chi) + -1 <= 0)%Z
    | 17%positive => (-1 * (s IDfont_dir_reloc_ptrs_chi) + -1 <= 0 /\ -1 * (s IDfont_dir_reloc_ptrs_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition font_dir_reloc_ptrs_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(1 + (s IDfont_dir_reloc_ptrs_vptr_dref_off48_off40)))%Q
    | 2%positive => ((s IDfont_dir_reloc_ptrs_z)
                     + max0(1
                            + (s IDfont_dir_reloc_ptrs_vptr_dref_off48_off40)))%Q
    | 3%positive => ((s IDfont_dir_reloc_ptrs_z)
                     + max0(1
                            + (s IDfont_dir_reloc_ptrs_vptr_dref_off48_off40)))%Q
    | 4%positive => ((s IDfont_dir_reloc_ptrs_z)
                     + max0(1 + (s IDfont_dir_reloc_ptrs_chi)))%Q
    | 5%positive => ((s IDfont_dir_reloc_ptrs_z)
                     + max0(1 + (s IDfont_dir_reloc_ptrs_chi)))%Q
    | 6%positive => ((s IDfont_dir_reloc_ptrs_z)
                     + max0(1 + (s IDfont_dir_reloc_ptrs_chi)))%Q
    | 7%positive => ((s IDfont_dir_reloc_ptrs_z)
                     + max0(1 + (s IDfont_dir_reloc_ptrs_chi)))%Q
    | 8%positive => ((s IDfont_dir_reloc_ptrs_z))%Q
    | 9%positive => ((s IDfont_dir_reloc_ptrs_z)
                     + max0(1 + (s IDfont_dir_reloc_ptrs_chi)))%Q
    | 10%positive => ((1 # 1) + (s IDfont_dir_reloc_ptrs_z)
                      + max0((s IDfont_dir_reloc_ptrs_chi)))%Q
    | 11%positive => ((1 # 1) + (s IDfont_dir_reloc_ptrs_z)
                      + max0((s IDfont_dir_reloc_ptrs_chi)))%Q
    | 12%positive => ((1 # 1) + (s IDfont_dir_reloc_ptrs_z)
                      + max0((s IDfont_dir_reloc_ptrs_chi)))%Q
    | 13%positive => ((1 # 1) + (s IDfont_dir_reloc_ptrs_z)
                      + max0((s IDfont_dir_reloc_ptrs_chi)))%Q
    | 14%positive => ((1 # 1) + (s IDfont_dir_reloc_ptrs_z)
                      + max0(1 + (s IDfont_dir_reloc_ptrs_chi)))%Q
    | 15%positive => ((1 # 1) + (s IDfont_dir_reloc_ptrs_z)
                      + max0(1 + (s IDfont_dir_reloc_ptrs_chi)))%Q
    | 16%positive => ((1 # 1) + (s IDfont_dir_reloc_ptrs_z)
                      + max0(1 + (s IDfont_dir_reloc_ptrs_chi)))%Q
    | 17%positive => ((s IDfont_dir_reloc_ptrs_z)
                      + max0(1 + (s IDfont_dir_reloc_ptrs_chi)))%Q
    | _ => (0 # 1)%Q
  end.

Definition font_dir_reloc_ptrs_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (1
                                                            + (s IDfont_dir_reloc_ptrs_chi)) ((s IDfont_dir_reloc_ptrs_chi)));
                     (*-1 0*) F_max0_ge_0 ((s IDfont_dir_reloc_ptrs_chi))]
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_pre_decrement (1
                                                    + (s IDfont_dir_reloc_ptrs_chi)) (1)]
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | _ => []
  end.


Theorem font_dir_reloc_ptrs_ai_correct:
  forall s p' s', steps (g_start font_dir_reloc_ptrs) s (g_edges font_dir_reloc_ptrs) p' s' -> font_dir_reloc_ptrs_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem font_dir_reloc_ptrs_pot_correct:
  forall s p' s',
    steps (g_start font_dir_reloc_ptrs) s (g_edges font_dir_reloc_ptrs) p' s' ->
    (font_dir_reloc_ptrs_pot (g_start font_dir_reloc_ptrs) s >= font_dir_reloc_ptrs_pot p' s')%Q.
Proof.
  check_lp font_dir_reloc_ptrs_ai_correct font_dir_reloc_ptrs_hints.
Qed.

