Require Import pasta.Pasta.

Notation IDclist_unpack_short_bits_z := 1%positive.
Notation IDclist_unpack_short_bits__tmp := 2%positive.
Notation IDclist_unpack_short_bits__tmp1 := 3%positive.
Notation IDclist_unpack_short_bits__tmp2 := 4%positive.
Notation IDclist_unpack_short_bits_bytes := 5%positive.
Notation IDclist_unpack_short_bits_dest := 6%positive.
Notation IDclist_unpack_short_bits_height := 7%positive.
Notation IDclist_unpack_short_bits_raster := 8%positive.
Notation IDclist_unpack_short_bits_src := 9%positive.
Notation IDclist_unpack_short_bits_width_bytes := 10%positive.
Definition clist_unpack_short_bits : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDclist_unpack_short_bits_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDclist_unpack_short_bits__tmp1
             (Some (EVar IDclist_unpack_short_bits_width_bytes))),3%positive)::
             (3%positive,(AAssign IDclist_unpack_short_bits__tmp
             (Some (EVar IDclist_unpack_short_bits_height))),4%positive)::
             (4%positive,(AAssign IDclist_unpack_short_bits__tmp2
             (Some (EVar IDclist_unpack_short_bits_raster))),5%positive)::
             (5%positive,(AAssign IDclist_unpack_short_bits_bytes
             (Some (EMul (EVar IDclist_unpack_short_bits__tmp1)
             (EVar IDclist_unpack_short_bits__tmp)))),6%positive)::
             (6%positive,ANone,7%positive)::
             (7%positive,(AAssign IDclist_unpack_short_bits__tmp
             (Some (EAdd (EVar IDclist_unpack_short_bits__tmp)
             (ENum (-1))))),8%positive)::(8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDclist_unpack_short_bits__tmp)
             (ENum (-1))) s) >= (eval (ENum (0)) s))%Z)),12%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EAdd (EVar IDclist_unpack_short_bits__tmp)
             (ENum (-1))) s) < (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,21%positive)::
             (13%positive,ANone,14%positive)::
             (13%positive,ANone,15%positive)::
             (13%positive,ANone,16%positive)::
             (13%positive,ANone,17%positive)::
             (13%positive,ANone,18%positive)::
             (13%positive,ANone,19%positive)::
             (13%positive,ANone,20%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,ANone,16%positive)::
             (16%positive,ANone,17%positive)::
             (17%positive,ANone,18%positive)::
             (18%positive,ANone,19%positive)::
             (19%positive,ANone,20%positive)::
             (20%positive,ANone,22%positive)::
             (21%positive,ANone,22%positive)::
             (22%positive,ANone,23%positive)::
             (23%positive,ANone,24%positive)::
             (24%positive,(AAssign IDclist_unpack_short_bits_z
             (Some (EAdd (ENum (1)) (EVar IDclist_unpack_short_bits_z)))),
             7%positive)::nil
|}.

Definition clist_unpack_short_bits_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDclist_unpack_short_bits_z) <= 0 /\ -1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 3%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0 /\ 1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 4%positive => (1 * (s IDclist_unpack_short_bits_z) <= 0 /\ -1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 5%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0 /\ 1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 6%positive => (1 * (s IDclist_unpack_short_bits_z) <= 0 /\ -1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 7%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 8%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 9%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 10%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0 /\ 1 * (s IDclist_unpack_short_bits__tmp) <= 0)%Z
    | 11%positive => (1 * (s IDclist_unpack_short_bits__tmp) <= 0 /\ -1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 12%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0 /\ -1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0 /\ -1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 14%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0 /\ -1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0 /\ -1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 16%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0 /\ -1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0)%Z
    | 17%positive => (-1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0 /\ -1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 18%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0 /\ -1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0)%Z
    | 19%positive => (-1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0 /\ -1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 20%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0 /\ -1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0)%Z
    | 21%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0 /\ -1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0)%Z
    | 22%positive => (-1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0 /\ -1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | 23%positive => (-1 * (s IDclist_unpack_short_bits_z) <= 0 /\ -1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0)%Z
    | 24%positive => (-1 * (s IDclist_unpack_short_bits__tmp) + 1 <= 0 /\ -1 * (s IDclist_unpack_short_bits_z) <= 0)%Z
    | _ => False
  end.

Definition clist_unpack_short_bits_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDclist_unpack_short_bits_height)))%Q
    | 2%positive => ((s IDclist_unpack_short_bits_z)
                     + max0(-1 + (s IDclist_unpack_short_bits_height)))%Q
    | 3%positive => ((s IDclist_unpack_short_bits_z)
                     + max0(-1 + (s IDclist_unpack_short_bits_height)))%Q
    | 4%positive => ((s IDclist_unpack_short_bits_z)
                     + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 5%positive => ((s IDclist_unpack_short_bits_z)
                     + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 6%positive => ((s IDclist_unpack_short_bits_z)
                     + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 7%positive => ((s IDclist_unpack_short_bits_z)
                     + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 8%positive => ((s IDclist_unpack_short_bits_z)
                     + max0((s IDclist_unpack_short_bits__tmp)))%Q
    | 9%positive => ((s IDclist_unpack_short_bits_z)
                     + max0((s IDclist_unpack_short_bits__tmp)))%Q
    | 10%positive => ((s IDclist_unpack_short_bits_z)
                      + max0((s IDclist_unpack_short_bits__tmp)))%Q
    | 11%positive => ((s IDclist_unpack_short_bits_z))%Q
    | 12%positive => ((s IDclist_unpack_short_bits_z)
                      + max0((s IDclist_unpack_short_bits__tmp)))%Q
    | 13%positive => ((1 # 1) + (s IDclist_unpack_short_bits_z)
                      + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 14%positive => ((1 # 1) + (s IDclist_unpack_short_bits_z)
                      + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDclist_unpack_short_bits_z)
                      + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 16%positive => ((1 # 1) + (s IDclist_unpack_short_bits_z)
                      + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 17%positive => ((1 # 1) + (s IDclist_unpack_short_bits_z)
                      + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 18%positive => ((1 # 1) + (s IDclist_unpack_short_bits_z)
                      + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 19%positive => ((1 # 1) + (s IDclist_unpack_short_bits_z)
                      + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 20%positive => ((1 # 1) + (s IDclist_unpack_short_bits_z)
                      + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 21%positive => ((1 # 1) + (s IDclist_unpack_short_bits_z)
                      + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 22%positive => ((1 # 1) + (s IDclist_unpack_short_bits_z)
                      + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 23%positive => ((1 # 1) + (s IDclist_unpack_short_bits_z)
                      + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | 24%positive => ((1 # 1) + (s IDclist_unpack_short_bits_z)
                      + max0(-1 + (s IDclist_unpack_short_bits__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition clist_unpack_short_bits_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDclist_unpack_short_bits__tmp)) (-1
                                                                    + (s IDclist_unpack_short_bits__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDclist_unpack_short_bits__tmp))]
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_pre_decrement ((s IDclist_unpack_short_bits__tmp)) (1)]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | 16%positive => []
    | 17%positive => []
    | 18%positive => []
    | 19%positive => []
    | 20%positive => []
    | 21%positive => []
    | 22%positive => []
    | 23%positive => []
    | 24%positive => []
    | _ => []
  end.


Theorem clist_unpack_short_bits_ai_correct:
  forall s p' s', steps (g_start clist_unpack_short_bits) s (g_edges clist_unpack_short_bits) p' s' -> clist_unpack_short_bits_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem clist_unpack_short_bits_pot_correct:
  forall s p' s',
    steps (g_start clist_unpack_short_bits) s (g_edges clist_unpack_short_bits) p' s' ->
    (clist_unpack_short_bits_pot (g_start clist_unpack_short_bits) s >= clist_unpack_short_bits_pot p' s')%Q.
Proof.
  check_lp clist_unpack_short_bits_ai_correct clist_unpack_short_bits_hints.
Qed.

