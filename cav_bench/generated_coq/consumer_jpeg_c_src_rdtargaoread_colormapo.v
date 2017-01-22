Require Import pasta.Pasta.

Notation IDread_colormap_z := 1%positive.
Notation IDread_colormap__tmp := 2%positive.
Notation IDread_colormap__tmp1 := 3%positive.
Notation IDread_colormap_i := 4%positive.
Notation IDread_colormap_cmaplen := 5%positive.
Notation IDread_colormap_mapentrysize := 6%positive.
Notation IDread_colormap_sinfo := 7%positive.
Definition read_colormap : graph := {|
  g_start := 1%positive;
  g_end := 14%positive;
  g_edges := (1%positive,(AAssign IDread_colormap_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AAssign IDread_colormap__tmp1
             (Some (EVar IDread_colormap_cmaplen))),3%positive)::
             (3%positive,(AAssign IDread_colormap__tmp
             (Some (EVar IDread_colormap_mapentrysize))),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDread_colormap__tmp)
             s) <> (eval (ENum (24)) s))%Z)),7%positive)::
             (5%positive,(AGuard (fun s => ((eval (EVar IDread_colormap__tmp)
             s) = (eval (ENum (24)) s))%Z)),6%positive)::
             (6%positive,AWeaken,9%positive)::
             (7%positive,AWeaken,8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,(AAssign IDread_colormap_i (Some (ENum (0)))),
             10%positive)::(10%positive,ANone,11%positive)::
             (11%positive,AWeaken,12%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDread_colormap_i)
             s) < (eval (EVar IDread_colormap__tmp1) s))%Z)),15%positive)::
             (12%positive,(AGuard (fun s => ((eval (EVar IDread_colormap_i)
             s) >= (eval (EVar IDread_colormap__tmp1) s))%Z)),13%positive)::
             (13%positive,AWeaken,14%positive)::
             (15%positive,AWeaken,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,(AAssign IDread_colormap_i
             (Some (EAdd (EVar IDread_colormap_i) (ENum (1))))),18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,(AAssign IDread_colormap_z (Some (EAdd (ENum (1))
             (EVar IDread_colormap_z)))),21%positive)::
             (21%positive,AWeaken,12%positive)::nil
|}.

Definition read_colormap_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 3%positive => (-1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap_z) <= 0)%Z
    | 4%positive => (1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 5%positive => (-1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap_z) <= 0)%Z
    | 6%positive => (1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap__tmp) + -24 <= 0 /\ -1 * (s IDread_colormap__tmp) + 24 <= 0)%Z
    | 7%positive => (1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 8%positive => (-1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap_z) <= 0)%Z
    | 9%positive => (1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 10%positive => (-1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_i) <= 0)%Z
    | 11%positive => (-1 * (s IDread_colormap_i) <= 0 /\ 1 * (s IDread_colormap_i) <= 0 /\ 1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 12%positive => (-1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) <= 0)%Z
    | 13%positive => (-1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ 1 * (s IDread_colormap__tmp1)+ -1 * (s IDread_colormap_i) <= 0)%Z
    | 14%positive => (1 * (s IDread_colormap__tmp1)+ -1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) <= 0)%Z
    | 15%positive => (-1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap__tmp1)+ 1 * (s IDread_colormap_i) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDread_colormap__tmp1)+ 1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) <= 0)%Z
    | 17%positive => (-1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap__tmp1)+ 1 * (s IDread_colormap_i) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap__tmp1)+ 1 * (s IDread_colormap_i) <= 0)%Z
    | 19%positive => (-1 * (s IDread_colormap__tmp1)+ 1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap_z) <= 0)%Z
    | 20%positive => (-1 * (s IDread_colormap_z) <= 0 /\ -1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap__tmp1)+ 1 * (s IDread_colormap_i) <= 0)%Z
    | 21%positive => (-1 * (s IDread_colormap__tmp1)+ 1 * (s IDread_colormap_i) <= 0 /\ -1 * (s IDread_colormap_i) + 1 <= 0 /\ -1 * (s IDread_colormap_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition read_colormap_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDread_colormap_cmaplen)))%Q
    | 2%positive => ((s IDread_colormap_z)
                     + max0((s IDread_colormap_cmaplen)))%Q
    | 3%positive => ((s IDread_colormap_z) + max0((s IDread_colormap__tmp1)))%Q
    | 4%positive => ((s IDread_colormap_z) + max0((s IDread_colormap__tmp1)))%Q
    | 5%positive => ((s IDread_colormap_z) + max0((s IDread_colormap__tmp1)))%Q
    | 6%positive => ((s IDread_colormap_z) + max0((s IDread_colormap__tmp1)))%Q
    | 7%positive => ((s IDread_colormap_z) + max0((s IDread_colormap__tmp1)))%Q
    | 8%positive => ((s IDread_colormap_z) + max0((s IDread_colormap__tmp1)))%Q
    | 9%positive => ((s IDread_colormap_z) + max0((s IDread_colormap__tmp1)))%Q
    | 10%positive => ((s IDread_colormap_z)
                      + max0((s IDread_colormap__tmp1)
                             - (s IDread_colormap_i)))%Q
    | 11%positive => ((s IDread_colormap_z)
                      + max0((s IDread_colormap__tmp1)
                             - (s IDread_colormap_i)))%Q
    | 12%positive => ((s IDread_colormap_z)
                      + max0((s IDread_colormap__tmp1)
                             - (s IDread_colormap_i)))%Q
    | 13%positive => ((s IDread_colormap_z)
                      + max0((s IDread_colormap__tmp1)
                             - (s IDread_colormap_i)))%Q
    | 14%positive => ((s IDread_colormap_z))%Q
    | 15%positive => ((s IDread_colormap_z)
                      + max0((s IDread_colormap__tmp1)
                             - (s IDread_colormap_i)))%Q
    | 16%positive => ((1 # 1) + (s IDread_colormap_z)
                      + max0(-1 + (s IDread_colormap__tmp1)
                             - (s IDread_colormap_i)))%Q
    | 17%positive => ((1 # 1) + (s IDread_colormap_z)
                      + max0(-1 + (s IDread_colormap__tmp1)
                             - (s IDread_colormap_i)))%Q
    | 18%positive => ((1 # 1) + (s IDread_colormap_z)
                      + max0((s IDread_colormap__tmp1)
                             - (s IDread_colormap_i)))%Q
    | 19%positive => ((1 # 1) + (s IDread_colormap_z)
                      + max0((s IDread_colormap__tmp1)
                             - (s IDread_colormap_i)))%Q
    | 20%positive => ((1 # 1) + (s IDread_colormap_z)
                      + max0((s IDread_colormap__tmp1)
                             - (s IDread_colormap_i)))%Q
    | 21%positive => ((s IDread_colormap_z)
                      + max0((s IDread_colormap__tmp1)
                             - (s IDread_colormap_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition read_colormap_hints (p : node) (s : state) := 
  match p with
    | 1%positive => []
    | 2%positive => []
    | 3%positive => []
    | 4%positive => []
    | 5%positive => []
    | 6%positive => []
    | 7%positive => []
    | 8%positive => []
    | 9%positive => []
    | 10%positive => []
    | 11%positive => []
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDread_colormap__tmp1)
                                                             - (s IDread_colormap_i)) (-1
                                                                    + (s IDread_colormap__tmp1)
                                                                    - (s IDread_colormap_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDread_colormap__tmp1)
                                            - (s IDread_colormap_i))]
    | 14%positive => []
    | 15%positive => [(*-1 0*) F_max0_pre_decrement ((s IDread_colormap__tmp1)
                                                     - (s IDread_colormap_i)) (1)]
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | _ => []
  end.


Theorem read_colormap_ai_correct:
  forall s p' s', steps (g_start read_colormap) s (g_edges read_colormap) p' s' -> read_colormap_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem read_colormap_pot_correct:
  forall s p' s',
    steps (g_start read_colormap) s (g_edges read_colormap) p' s' ->
    (read_colormap_pot (g_start read_colormap) s >= read_colormap_pot p' s')%Q.
Proof.
  check_lp read_colormap_ai_correct read_colormap_hints.
Qed.

