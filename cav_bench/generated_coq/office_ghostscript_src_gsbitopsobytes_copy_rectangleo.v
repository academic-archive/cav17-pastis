Require Import pasta.Pasta.

Notation IDbytes_copy_rectangle_z := 1%positive.
Notation IDbytes_copy_rectangle__tmp := 2%positive.
Notation IDbytes_copy_rectangle__tmp1 := 3%positive.
Notation IDbytes_copy_rectangle__tmp2 := 4%positive.
Notation IDbytes_copy_rectangle__tmp3 := 5%positive.
Notation IDbytes_copy_rectangle_dest := 6%positive.
Notation IDbytes_copy_rectangle_dest_raster := 7%positive.
Notation IDbytes_copy_rectangle_height := 8%positive.
Notation IDbytes_copy_rectangle_src := 9%positive.
Notation IDbytes_copy_rectangle_src_raster := 10%positive.
Notation IDbytes_copy_rectangle_width_bytes := 11%positive.
Definition bytes_copy_rectangle : graph := {|
  g_start := 1%positive;
  g_end := 11%positive;
  g_edges := (1%positive,(AAssign IDbytes_copy_rectangle_z
             (Some (ENum (0)))),2%positive)::
             (2%positive,(AAssign IDbytes_copy_rectangle__tmp3
             (Some (EVar IDbytes_copy_rectangle_dest_raster))),3%positive)::
             (3%positive,(AAssign IDbytes_copy_rectangle__tmp2
             (Some (EVar IDbytes_copy_rectangle_src_raster))),4%positive)::
             (4%positive,(AAssign IDbytes_copy_rectangle__tmp1
             (Some (EVar IDbytes_copy_rectangle_width_bytes))),5%positive)::
             (5%positive,(AAssign IDbytes_copy_rectangle__tmp
             (Some (EVar IDbytes_copy_rectangle_height))),6%positive)::
             (6%positive,ANone,7%positive)::
             (7%positive,(AAssign IDbytes_copy_rectangle__tmp
             (Some (EAdd (EVar IDbytes_copy_rectangle__tmp) (ENum (-1))))),
             8%positive)::(8%positive,AWeaken,9%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDbytes_copy_rectangle__tmp) s) >
             (eval (ENum (0)) s))%Z)),12%positive)::
             (9%positive,(AGuard
             (fun s => ((eval (EVar IDbytes_copy_rectangle__tmp) s) <=
             (eval (ENum (0)) s))%Z)),10%positive)::
             (10%positive,AWeaken,11%positive)::
             (12%positive,AWeaken,13%positive)::
             (13%positive,ANone,14%positive)::
             (14%positive,ANone,15%positive)::
             (15%positive,(AAssign IDbytes_copy_rectangle_z
             (Some (EAdd (ENum (1)) (EVar IDbytes_copy_rectangle_z)))),
             7%positive)::nil
|}.

Definition bytes_copy_rectangle_ai (p: node) (s: state) := 
  match p with
    | 1%positive => (True)%Z
    | 2%positive => (1 * (s IDbytes_copy_rectangle_z) <= 0 /\ -1 * (s IDbytes_copy_rectangle_z) <= 0)%Z
    | 3%positive => (-1 * (s IDbytes_copy_rectangle_z) <= 0 /\ 1 * (s IDbytes_copy_rectangle_z) <= 0)%Z
    | 4%positive => (1 * (s IDbytes_copy_rectangle_z) <= 0 /\ -1 * (s IDbytes_copy_rectangle_z) <= 0)%Z
    | 5%positive => (-1 * (s IDbytes_copy_rectangle_z) <= 0 /\ 1 * (s IDbytes_copy_rectangle_z) <= 0)%Z
    | 6%positive => (1 * (s IDbytes_copy_rectangle_z) <= 0 /\ -1 * (s IDbytes_copy_rectangle_z) <= 0)%Z
    | 7%positive => (-1 * (s IDbytes_copy_rectangle_z) <= 0)%Z
    | 8%positive => (-1 * (s IDbytes_copy_rectangle_z) <= 0)%Z
    | 9%positive => (-1 * (s IDbytes_copy_rectangle_z) <= 0)%Z
    | 10%positive => (-1 * (s IDbytes_copy_rectangle_z) <= 0 /\ 1 * (s IDbytes_copy_rectangle__tmp) <= 0)%Z
    | 11%positive => (1 * (s IDbytes_copy_rectangle__tmp) <= 0 /\ -1 * (s IDbytes_copy_rectangle_z) <= 0)%Z
    | 12%positive => (-1 * (s IDbytes_copy_rectangle_z) <= 0 /\ -1 * (s IDbytes_copy_rectangle__tmp) + 1 <= 0)%Z
    | 13%positive => (-1 * (s IDbytes_copy_rectangle__tmp) + 1 <= 0 /\ -1 * (s IDbytes_copy_rectangle_z) <= 0)%Z
    | 14%positive => (-1 * (s IDbytes_copy_rectangle_z) <= 0 /\ -1 * (s IDbytes_copy_rectangle__tmp) + 1 <= 0)%Z
    | 15%positive => (-1 * (s IDbytes_copy_rectangle__tmp) + 1 <= 0 /\ -1 * (s IDbytes_copy_rectangle_z) <= 0)%Z
    | _ => False
  end.

Definition bytes_copy_rectangle_pot (p : node) (s : state): Q := 
  match p with
    | 1%positive => (max0(-1 + (s IDbytes_copy_rectangle_height)))%Q
    | 2%positive => ((s IDbytes_copy_rectangle_z)
                     + max0(-1 + (s IDbytes_copy_rectangle_height)))%Q
    | 3%positive => ((s IDbytes_copy_rectangle_z)
                     + max0(-1 + (s IDbytes_copy_rectangle_height)))%Q
    | 4%positive => ((s IDbytes_copy_rectangle_z)
                     + max0(-1 + (s IDbytes_copy_rectangle_height)))%Q
    | 5%positive => ((s IDbytes_copy_rectangle_z)
                     + max0(-1 + (s IDbytes_copy_rectangle_height)))%Q
    | 6%positive => ((s IDbytes_copy_rectangle_z)
                     + max0(-1 + (s IDbytes_copy_rectangle__tmp)))%Q
    | 7%positive => ((s IDbytes_copy_rectangle_z)
                     + max0(-1 + (s IDbytes_copy_rectangle__tmp)))%Q
    | 8%positive => ((s IDbytes_copy_rectangle_z)
                     + max0((s IDbytes_copy_rectangle__tmp)))%Q
    | 9%positive => ((s IDbytes_copy_rectangle_z)
                     + max0((s IDbytes_copy_rectangle__tmp)))%Q
    | 10%positive => ((s IDbytes_copy_rectangle_z)
                      + max0((s IDbytes_copy_rectangle__tmp)))%Q
    | 11%positive => ((s IDbytes_copy_rectangle_z))%Q
    | 12%positive => ((s IDbytes_copy_rectangle_z)
                      + max0((s IDbytes_copy_rectangle__tmp)))%Q
    | 13%positive => ((1 # 1) + (s IDbytes_copy_rectangle_z)
                      + max0(-1 + (s IDbytes_copy_rectangle__tmp)))%Q
    | 14%positive => ((1 # 1) + (s IDbytes_copy_rectangle_z)
                      + max0(-1 + (s IDbytes_copy_rectangle__tmp)))%Q
    | 15%positive => ((1 # 1) + (s IDbytes_copy_rectangle_z)
                      + max0(-1 + (s IDbytes_copy_rectangle__tmp)))%Q
    | _ => (0 # 1)%Q
  end.

Definition bytes_copy_rectangle_hints (p : node) (s : state) := 
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
    | 10%positive => [(*-1 0*) F_max0_monotonic (F_check_ge ((s IDbytes_copy_rectangle__tmp)) (-1
                                                                    + (s IDbytes_copy_rectangle__tmp)));
                      (*-1 0*) F_max0_ge_0 (-1
                                            + (s IDbytes_copy_rectangle__tmp))]
    | 11%positive => []
    | 12%positive => [(*-1 0*) F_max0_pre_decrement ((s IDbytes_copy_rectangle__tmp)) (1)]
    | 13%positive => []
    | 14%positive => []
    | 15%positive => []
    | _ => []
  end.


Theorem bytes_copy_rectangle_ai_correct:
  forall s p' s', steps (g_start bytes_copy_rectangle) s (g_edges bytes_copy_rectangle) p' s' -> bytes_copy_rectangle_ai p' s'.
Proof.
  check_ai.
Qed.

Theorem bytes_copy_rectangle_pot_correct:
  forall s p' s',
    steps (g_start bytes_copy_rectangle) s (g_edges bytes_copy_rectangle) p' s' ->
    (bytes_copy_rectangle_pot (g_start bytes_copy_rectangle) s >= bytes_copy_rectangle_pot p' s')%Q.
Proof.
  check_lp bytes_copy_rectangle_ai_correct bytes_copy_rectangle_hints.
Qed.

