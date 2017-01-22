Require Import pasta.Pasta.

Notation IDrender_data_z := 1%positive.
Notation IDrender_data__tmp := 2%positive.
Notation IDrender_data_i := 3%positive.
Notation IDrender_data_size := 4%positive.
Notation IDrender_data_fields := 5%positive.
Notation IDrender_data_length := 6%positive.
Notation IDrender_data_ptr := 7%positive.
Definition render_data : graph := {|
  g_start := 1%positive;
  g_end := 12%positive;
  g_edges := (1%positive,(AAssign IDrender_data_z (Some (ENum (0)))),
             2%positive)::
             (2%positive,(AGuard (fun s => ((eval (EVar IDrender_data_i)
             s) >= (eval (ENum (0)) s))%Z)),3%positive)::
             (3%positive,(AGuard (fun s => ((eval (EVar IDrender_data__tmp)
             s) >= (eval (ENum (0)) s))%Z)),4%positive)::
             (4%positive,AWeaken,5%positive)::
             (5%positive,(AAssign IDrender_data__tmp
             (Some (EVar IDrender_data_length))),6%positive)::
             (6%positive,(AAssign IDrender_data_size (Some (ENum (0)))),
             7%positive)::
             (7%positive,(AAssign IDrender_data_i (Some (ENum (0)))),
             8%positive)::(8%positive,ANone,9%positive)::
             (9%positive,AWeaken,10%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDrender_data_i)
             s) < (eval (EVar IDrender_data__tmp) s))%Z)),13%positive)::
             (10%positive,(AGuard (fun s => ((eval (EVar IDrender_data_i)
             s) >= (eval (EVar IDrender_data__tmp) s))%Z)),11%positive)::
             (11%positive,AWeaken,12%positive)::
             (13%positive,AWeaken,14%positive)::
             (14%positive,(AAssign IDrender_data_size None),15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,(AAssign IDrender_data_i
             (Some (EAdd (EVar IDrender_data_i) (ENum (1))))),17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,(AAssign IDrender_data_z (Some (EAdd (ENum (1))
             (EVar IDrender_data_z)))),20%positive)::
             (20%positive,AWeaken,10%positive)::nil
|}.

Definition render_data_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data_z) <= 0)%Z
    | 3%positive => (-1 * (s IDrender_data_z) <= 0 /\ 1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data_i) <= 0)%Z
    | 4%positive => (-1 * (s IDrender_data_i) <= 0 /\ 1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data__tmp) <= 0)%Z
    | 5%positive => (-1 * (s IDrender_data__tmp) <= 0 /\ -1 * (s IDrender_data_z) <= 0 /\ 1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data_i) <= 0)%Z
    | 6%positive => (-1 * (s IDrender_data_i) <= 0 /\ 1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data_z) <= 0)%Z
    | 7%positive => (-1 * (s IDrender_data_z) <= 0 /\ 1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data_i) <= 0 /\ 1 * (s IDrender_data_size) <= 0 /\ -1 * (s IDrender_data_size) <= 0)%Z
    | 8%positive => (-1 * (s IDrender_data_size) <= 0 /\ 1 * (s IDrender_data_size) <= 0 /\ 1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data_z) <= 0 /\ 1 * (s IDrender_data_i) <= 0 /\ -1 * (s IDrender_data_i) <= 0)%Z
    | 9%positive => (-1 * (s IDrender_data_i) <= 0 /\ 1 * (s IDrender_data_i) <= 0 /\ -1 * (s IDrender_data_z) <= 0 /\ 1 * (s IDrender_data_z) <= 0 /\ 1 * (s IDrender_data_size) <= 0 /\ -1 * (s IDrender_data_size) <= 0)%Z
    | 10%positive => (-1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data_i) <= 0)%Z
    | 11%positive => (-1 * (s IDrender_data_i) <= 0 /\ -1 * (s IDrender_data_z) <= 0 /\ 1 * (s IDrender_data__tmp)+ -1 * (s IDrender_data_i) <= 0)%Z
    | 12%positive => (1 * (s IDrender_data__tmp)+ -1 * (s IDrender_data_i) <= 0 /\ -1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data_i) <= 0)%Z
    | 13%positive => (-1 * (s IDrender_data_i) <= 0 /\ -1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data__tmp)+ 1 * (s IDrender_data_i) + 1 <= 0)%Z
    | 14%positive => (-1 * (s IDrender_data__tmp)+ 1 * (s IDrender_data_i) + 1 <= 0 /\ -1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data_i) <= 0)%Z
    | 15%positive => (-1 * (s IDrender_data_i) <= 0 /\ -1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data__tmp)+ 1 * (s IDrender_data_i) + 1 <= 0)%Z
    | 16%positive => (-1 * (s IDrender_data__tmp)+ 1 * (s IDrender_data_i) + 1 <= 0 /\ -1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data_i) <= 0)%Z
    | 17%positive => (-1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data__tmp)+ 1 * (s IDrender_data_i) <= 0 /\ -1 * (s IDrender_data_i) + 1 <= 0)%Z
    | 18%positive => (-1 * (s IDrender_data_i) + 1 <= 0 /\ -1 * (s IDrender_data__tmp)+ 1 * (s IDrender_data_i) <= 0 /\ -1 * (s IDrender_data_z) <= 0)%Z
    | 19%positive => (-1 * (s IDrender_data_z) <= 0 /\ -1 * (s IDrender_data__tmp)+ 1 * (s IDrender_data_i) <= 0 /\ -1 * (s IDrender_data_i) + 1 <= 0)%Z
    | 20%positive => (-1 * (s IDrender_data_i) + 1 <= 0 /\ -1 * (s IDrender_data__tmp)+ 1 * (s IDrender_data_i) <= 0 /\ -1 * (s IDrender_data_z) + 1 <= 0)%Z
    | _ => False
  end.

Definition render_data_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0((s IDrender_data_length)))%Q
    | 2%positive => ((s IDrender_data_z) + max0((s IDrender_data_length)))%Q
    | 3%positive => ((s IDrender_data_z) + max0((s IDrender_data_length)))%Q
    | 4%positive => ((s IDrender_data_z) + max0((s IDrender_data_length)))%Q
    | 5%positive => ((s IDrender_data_z) + max0((s IDrender_data_length)))%Q
    | 6%positive => ((s IDrender_data_z) + max0((s IDrender_data__tmp)))%Q
    | 7%positive => ((s IDrender_data_z) + max0((s IDrender_data__tmp)))%Q
    | 8%positive => ((s IDrender_data_z)
                     + max0((s IDrender_data__tmp) - (s IDrender_data_i)))%Q
    | 9%positive => ((s IDrender_data_z)
                     + max0((s IDrender_data__tmp) - (s IDrender_data_i)))%Q
    | 10%positive => ((s IDrender_data_z)
                      + max0((s IDrender_data__tmp) - (s IDrender_data_i)))%Q
    | 11%positive => ((s IDrender_data_z)
                      + max0((s IDrender_data__tmp) - (s IDrender_data_i)))%Q
    | 12%positive => ((s IDrender_data_z))%Q
    | 13%positive => ((s IDrender_data_z)
                      + max0((s IDrender_data__tmp) - (s IDrender_data_i)))%Q
    | 14%positive => ((1 # 1) + (s IDrender_data_z)
                      + max0(-1 + (s IDrender_data__tmp)
                             - (s IDrender_data_i)))%Q
    | 15%positive => ((1 # 1) + (s IDrender_data_z)
                      + max0(-1 + (s IDrender_data__tmp)
                             - (s IDrender_data_i)))%Q
    | 16%positive => ((1 # 1) + (s IDrender_data_z)
                      + max0(-1 + (s IDrender_data__tmp)
                             - (s IDrender_data_i)))%Q
    | 17%positive => ((1 # 1) + (s IDrender_data_z)
                      + max0((s IDrender_data__tmp) - (s IDrender_data_i)))%Q
    | 18%positive => ((1 # 1) + (s IDrender_data_z)
                      + max0((s IDrender_data__tmp) - (s IDrender_data_i)))%Q
    | 19%positive => ((1 # 1) + (s IDrender_data_z)
                      + max0((s IDrender_data__tmp) - (s IDrender_data_i)))%Q
    | 20%positive => ((s IDrender_data_z)
                      + max0((s IDrender_data__tmp) - (s IDrender_data_i)))%Q
    | _ => (0 # 1)%Q
  end.

Definition render_data_hints (p : node) (s : state) := 
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
    | 11%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDrender_data__tmp)
                                                             - (s IDrender_data_i)) (-1
                                                                    + (s IDrender_data__tmp)
                                                                    - (s IDrender_data_i)));
                      (*-1 0*) F_max0_ge_0 (-1 + (s IDrender_data__tmp)
                                            - (s IDrender_data_i))]
    | 12%positive => []
    | 13%positive => [(*-1 0*) F_max0_pre_decrement ((s IDrender_data__tmp)
                                                     - (s IDrender_data_i)) (1)]
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | _ => []
  end.


Theorem render_data_ai_correct:
  forall s p' s', steps (g_start render_data) s (g_edges render_data) p' s' -> render_data_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem render_data_pot_correct:
  forall s p' s',
    steps (g_start render_data) s (g_edges render_data) p' s' ->
    (render_data_pot (g_start render_data) s >= render_data_pot p' s')%Q.
Proof.
  check_lp render_data_ai_correct render_data_hints.
Qed.

