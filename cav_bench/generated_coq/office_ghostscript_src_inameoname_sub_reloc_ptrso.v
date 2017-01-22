Require Import pasta.Pasta.

Notation IDname_sub_reloc_ptrs_z := 1%positive.
Notation IDname_sub_reloc_ptrs__tmp := 2%positive.
Notation IDname_sub_reloc_ptrs_i := 3%positive.
Notation IDname_sub_reloc_ptrs_gcst := 4%positive.
Notation IDname_sub_reloc_ptrs_size := 5%positive.
Notation IDname_sub_reloc_ptrs_vptr := 6%positive.
Definition name_sub_reloc_ptrs : graph := {|
  g_start := 1%positive;
  g_end := 10%positive;
  g_edges := (1%positive,(AAssign IDname_sub_reloc_ptrs_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard
             (fun s => ((eval (EVar IDname_sub_reloc_ptrs_i) s) >=
             (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,AWeaken,4%positive)::
             (4%positive,(AAssign IDname_sub_reloc_ptrs__tmp
             (Some (EVar IDname_sub_reloc_ptrs_size))),5%positive)::
             (5%positive,(AAssign IDname_sub_reloc_ptrs_i (Some (ENum (0)))),
             6%positive)::(6%positive,ANone,7%positive)::
             (7%positive,AWeaken,8%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDname_sub_reloc_ptrs_i) s) <
             (eval (ENum (128)) s))%Z)),11%positive)::
             (8%positive,(AGuard
             (fun s => ((eval (EVar IDname_sub_reloc_ptrs_i) s) >=
             (eval (ENum (128)) s))%Z)),9%positive)::
             (9%positive,AWeaken,10%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,ANone,13%positive)::
             (12%positive,ANone,16%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,ANone,16%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDname_sub_reloc_ptrs_i
             (Some (EAdd (EVar IDname_sub_reloc_ptrs_i) (ENum (1))))),
             18%positive)::(18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDname_sub_reloc_ptrs_z
             (Some (EAdd (ENum (1)) (EVar IDname_sub_reloc_ptrs_z)))),
             21%positive)::(21%positive,AWeaken,8%positive)::nil
|}.

Definition name_sub_reloc_ptrs_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0)%Z
    | 3%positive => (-1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_i) <= 0)%Z
    | 4%positive => (-1 * (s IDname_sub_reloc_ptrs_i) <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0)%Z
    | 5%positive => (-1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_i) <= 0)%Z
    | 6%positive => (1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_i) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_i) <= 0)%Z
    | 7%positive => (-1 * (s IDname_sub_reloc_ptrs_i) <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_i) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_z) <= 0)%Z
    | 8%positive => (-1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_i) <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_i) + -128 <= 0)%Z
    | 9%positive => (1 * (s IDname_sub_reloc_ptrs_i) + -128 <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_i) + 128 <= 0)%Z
    | 10%positive => (-1 * (s IDname_sub_reloc_ptrs_i) + 128 <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_i) + -128 <= 0)%Z
    | 11%positive => (-1 * (s IDname_sub_reloc_ptrs_i) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_i) + -127 <= 0)%Z
    | 12%positive => (1 * (s IDname_sub_reloc_ptrs_i) + -127 <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_i) <= 0)%Z
    | 13%positive => (-1 * (s IDname_sub_reloc_ptrs_i) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_i) + -127 <= 0)%Z
    | 14%positive => (1 * (s IDname_sub_reloc_ptrs_i) + -127 <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_i) <= 0)%Z
    | 15%positive => (-1 * (s IDname_sub_reloc_ptrs_i) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_i) + -127 <= 0)%Z
    | 16%positive => (1 * (s IDname_sub_reloc_ptrs_i) + -127 <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_i) <= 0)%Z
    | 17%positive => (-1 * (s IDname_sub_reloc_ptrs_i) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_i) + -127 <= 0)%Z
    | 18%positive => (-1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_i) + 1 <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_i) + -128 <= 0)%Z
    | 19%positive => (1 * (s IDname_sub_reloc_ptrs_i) + -128 <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_i) + 1 <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) <= 0)%Z
    | 20%positive => (-1 * (s IDname_sub_reloc_ptrs_z) <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_i) + 1 <= 0 /\ 1 * (s IDname_sub_reloc_ptrs_i) + -128 <= 0)%Z
    | 21%positive => (1 * (s IDname_sub_reloc_ptrs_i) + -128 <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_i) + 1 <= 0 /\ -1 * (s IDname_sub_reloc_ptrs_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition name_sub_reloc_ptrs_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => ((128 # 1))%Q
    | 2%positive => ((128 # 1) + (s IDname_sub_reloc_ptrs_z))%Q
    | 3%positive => ((128 # 1) + (s IDname_sub_reloc_ptrs_z))%Q
    | 4%positive => ((128 # 1) + (s IDname_sub_reloc_ptrs_z))%Q
    | 5%positive => ((128 # 1) + (s IDname_sub_reloc_ptrs_z))%Q
    | 6%positive => ((s IDname_sub_reloc_ptrs_z)
                     + max0(128 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 7%positive => ((s IDname_sub_reloc_ptrs_z)
                     + max0(128 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 8%positive => ((s IDname_sub_reloc_ptrs_z)
                     + max0(128 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 9%positive => ((s IDname_sub_reloc_ptrs_z)
                     + max0(128 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 10%positive => ((s IDname_sub_reloc_ptrs_z))%Q
    | 11%positive => ((s IDname_sub_reloc_ptrs_z)
                      + max0(128 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 12%positive => ((1 # 1) + (s IDname_sub_reloc_ptrs_z)
                      + max0(127 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 13%positive => ((1 # 1) + (s IDname_sub_reloc_ptrs_z)
                      + max0(127 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 14%positive => ((1 # 1) + (s IDname_sub_reloc_ptrs_z)
                      + max0(127 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 15%positive => ((1 # 1) + (s IDname_sub_reloc_ptrs_z)
                      + max0(127 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 16%positive => ((1 # 1) + (s IDname_sub_reloc_ptrs_z)
                      + max0(127 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 17%positive => ((1 # 1) + (s IDname_sub_reloc_ptrs_z)
                      + max0(127 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 18%positive => ((1 # 1) + (s IDname_sub_reloc_ptrs_z)
                      + max0(128 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 19%positive => ((1 # 1) + (s IDname_sub_reloc_ptrs_z)
                      + max0(128 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 20%positive => ((1 # 1) + (s IDname_sub_reloc_ptrs_z)
                      + max0(128 - (s IDname_sub_reloc_ptrs_i)))%Q
    | 21%positive => ((s IDname_sub_reloc_ptrs_z)
                      + max0(128 - (s IDname_sub_reloc_ptrs_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition name_sub_reloc_ptrs_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => [(*-1 0*) F_max0_monotonic (F_check_ge (128
                                                            - (s IDname_sub_reloc_ptrs_i)) (127
                                                                    - (s IDname_sub_reloc_ptrs_i)));
                     (*-1 0*) F_max0_ge_0 (127 - (s IDname_sub_reloc_ptrs_i))]
    | 10%positive => []
    | 11%positive => [(*-1 0*) F_max0_pre_decrement (128
                                                     - (s IDname_sub_reloc_ptrs_i)) (1)]
    | 12%positive => []
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | _ => []
  end.


Theorem name_sub_reloc_ptrs_ai_correct:
  forall s p' s', steps (g_start name_sub_reloc_ptrs) s (g_edges name_sub_reloc_ptrs) p' s' -> name_sub_reloc_ptrs_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem name_sub_reloc_ptrs_pot_correct:
  forall s p' s',
    steps (g_start name_sub_reloc_ptrs) s (g_edges name_sub_reloc_ptrs) p' s' ->
    (name_sub_reloc_ptrs_pot (g_start name_sub_reloc_ptrs) s >= name_sub_reloc_ptrs_pot p' s')%Q.
Proof.
  check_lp name_sub_reloc_ptrs_ai_correct name_sub_reloc_ptrs_hints.
Qed.

