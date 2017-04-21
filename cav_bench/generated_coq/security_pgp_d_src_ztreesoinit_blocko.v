Require Import pasta.Pasta.

Inductive proc: Type :=
  P_init_block.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_init_block_z := 1%positive.
Notation V_init_block_flag_bit := 2%positive.
Notation V_init_block_flags := 3%positive.
Notation V_init_block_last_dist := 4%positive.
Notation V_init_block_last_flags := 5%positive.
Notation V_init_block_last_lit := 6%positive.
Notation V_init_block_n := 7%positive.
Notation V_init_block_opt_len := 8%positive.
Notation V_init_block_static_len := 9%positive.
Definition Pedges_init_block: list (edge proc) :=
  (EA 1 (AAssign V_init_block_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_init_block_n (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_init_block_n) s) <
  (eval (ENum (286)) s))%Z)) 40)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_init_block_n) s) >= (eval (ENum (286))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign V_init_block_n
  (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_init_block_n) s) < (eval (ENum (30)) s))%Z)) 33)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_init_block_n) s) >=
  (eval (ENum (30)) s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 12 (AAssign
  V_init_block_n (Some (ENum (0)))) 13)::(EA 13 ANone 14)::
  (EA 14 AWeaken 15)::(EA 15 (AGuard (fun s => ((eval (EVar V_init_block_n)
  s) < (eval (ENum (19)) s))%Z)) 26)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_init_block_n) s) >= (eval (ENum (19))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 (AAssign V_init_block_static_len
  (Some (ENum (0)))) 18)::(EA 18 (AAssign V_init_block_opt_len
  (Some (ENum (0)))) 19)::(EA 19 (AAssign V_init_block_last_flags
  (Some (ENum (0)))) 20)::(EA 20 (AAssign V_init_block_last_dist
  (Some (ENum (0)))) 21)::(EA 21 (AAssign V_init_block_last_lit
  (Some (ENum (0)))) 22)::(EA 22 (AAssign V_init_block_flags
  (Some (ENum (0)))) 23)::(EA 23 (AAssign V_init_block_flag_bit
  (Some (ENum (1)))) 24)::(EA 24 AWeaken 25)::(EA 26 AWeaken 27)::
  (EA 27 ANone 28)::(EA 28 (AAssign V_init_block_n
  (Some (EAdd (EVar V_init_block_n) (ENum (1))))) 29)::(EA 29 ANone 30)::
  (EA 30 ANone 31)::(EA 31 (AAssign V_init_block_z (Some (EAdd (ENum (1))
  (EVar V_init_block_z)))) 32)::(EA 32 AWeaken 15)::(EA 33 AWeaken 34)::
  (EA 34 ANone 35)::(EA 35 (AAssign V_init_block_n
  (Some (EAdd (EVar V_init_block_n) (ENum (1))))) 36)::(EA 36 ANone 37)::
  (EA 37 ANone 38)::(EA 38 (AAssign V_init_block_z (Some (EAdd (ENum (1))
  (EVar V_init_block_z)))) 39)::(EA 39 AWeaken 10)::(EA 40 AWeaken 41)::
  (EA 41 ANone 42)::(EA 42 (AAssign V_init_block_n
  (Some (EAdd (EVar V_init_block_n) (ENum (1))))) 43)::(EA 43 ANone 44)::
  (EA 44 ANone 45)::(EA 45 (AAssign V_init_block_z (Some (EAdd (ENum (1))
  (EVar V_init_block_z)))) 46)::(EA 46 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_init_block => Pedges_init_block
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_init_block => 25
     end)%positive;
  var_global := var_global
}.

Definition ai_init_block (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_z <= 0)%Z
   | 3 => (-1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n <= 0 /\ -1 * s V_init_block_n <= 0)%Z
   | 4 => (-1 * s V_init_block_n <= 0 /\ 1 * s V_init_block_n <= 0 /\ 1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_z <= 0)%Z
   | 5 => (-1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n <= 0 /\ 1 * s V_init_block_n + -286 <= 0)%Z
   | 6 => (1 * s V_init_block_n + -286 <= 0 /\ -1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 286 <= 0)%Z
   | 7 => (-1 * s V_init_block_n + 286 <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -286 <= 0)%Z
   | 8 => (-1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n <= 0 /\ -1 * s V_init_block_n <= 0)%Z
   | 9 => (-1 * s V_init_block_n <= 0 /\ 1 * s V_init_block_n <= 0 /\ -1 * s V_init_block_z <= 0)%Z
   | 10 => (-1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n <= 0 /\ 1 * s V_init_block_n + -30 <= 0)%Z
   | 11 => (1 * s V_init_block_n + -30 <= 0 /\ -1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 30 <= 0)%Z
   | 12 => (-1 * s V_init_block_n + 30 <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -30 <= 0)%Z
   | 13 => (-1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n <= 0 /\ -1 * s V_init_block_n <= 0)%Z
   | 14 => (-1 * s V_init_block_n <= 0 /\ 1 * s V_init_block_n <= 0 /\ -1 * s V_init_block_z <= 0)%Z
   | 15 => (-1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n <= 0 /\ 1 * s V_init_block_n + -19 <= 0)%Z
   | 16 => (1 * s V_init_block_n + -19 <= 0 /\ -1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 19 <= 0)%Z
   | 17 => (-1 * s V_init_block_n + 19 <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -19 <= 0)%Z
   | 18 => (1 * s V_init_block_n + -19 <= 0 /\ -1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 19 <= 0 /\ 1 * s V_init_block_static_len <= 0 /\ -1 * s V_init_block_static_len <= 0)%Z
   | 19 => (-1 * s V_init_block_static_len <= 0 /\ 1 * s V_init_block_static_len <= 0 /\ -1 * s V_init_block_n + 19 <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -19 <= 0 /\ 1 * s V_init_block_opt_len <= 0 /\ -1 * s V_init_block_opt_len <= 0)%Z
   | 20 => (-1 * s V_init_block_opt_len <= 0 /\ 1 * s V_init_block_opt_len <= 0 /\ 1 * s V_init_block_n + -19 <= 0 /\ -1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 19 <= 0 /\ 1 * s V_init_block_static_len <= 0 /\ -1 * s V_init_block_static_len <= 0 /\ 1 * s V_init_block_last_flags <= 0 /\ -1 * s V_init_block_last_flags <= 0)%Z
   | 21 => (-1 * s V_init_block_last_flags <= 0 /\ 1 * s V_init_block_last_flags <= 0 /\ -1 * s V_init_block_static_len <= 0 /\ 1 * s V_init_block_static_len <= 0 /\ -1 * s V_init_block_n + 19 <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -19 <= 0 /\ 1 * s V_init_block_opt_len <= 0 /\ -1 * s V_init_block_opt_len <= 0 /\ 1 * s V_init_block_last_dist <= 0 /\ -1 * s V_init_block_last_dist <= 0)%Z
   | 22 => (-1 * s V_init_block_last_dist <= 0 /\ 1 * s V_init_block_last_dist <= 0 /\ -1 * s V_init_block_opt_len <= 0 /\ 1 * s V_init_block_opt_len <= 0 /\ 1 * s V_init_block_n + -19 <= 0 /\ -1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 19 <= 0 /\ 1 * s V_init_block_static_len <= 0 /\ -1 * s V_init_block_static_len <= 0 /\ 1 * s V_init_block_last_flags <= 0 /\ -1 * s V_init_block_last_flags <= 0 /\ 1 * s V_init_block_last_lit <= 0 /\ -1 * s V_init_block_last_lit <= 0)%Z
   | 23 => (-1 * s V_init_block_last_lit <= 0 /\ 1 * s V_init_block_last_lit <= 0 /\ -1 * s V_init_block_last_flags <= 0 /\ 1 * s V_init_block_last_flags <= 0 /\ -1 * s V_init_block_static_len <= 0 /\ 1 * s V_init_block_static_len <= 0 /\ -1 * s V_init_block_n + 19 <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -19 <= 0 /\ 1 * s V_init_block_opt_len <= 0 /\ -1 * s V_init_block_opt_len <= 0 /\ 1 * s V_init_block_last_dist <= 0 /\ -1 * s V_init_block_last_dist <= 0 /\ 1 * s V_init_block_flags <= 0 /\ -1 * s V_init_block_flags <= 0)%Z
   | 24 => (-1 * s V_init_block_flags <= 0 /\ 1 * s V_init_block_flags <= 0 /\ -1 * s V_init_block_last_dist <= 0 /\ 1 * s V_init_block_last_dist <= 0 /\ -1 * s V_init_block_opt_len <= 0 /\ 1 * s V_init_block_opt_len <= 0 /\ 1 * s V_init_block_n + -19 <= 0 /\ -1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 19 <= 0 /\ 1 * s V_init_block_static_len <= 0 /\ -1 * s V_init_block_static_len <= 0 /\ 1 * s V_init_block_last_flags <= 0 /\ -1 * s V_init_block_last_flags <= 0 /\ 1 * s V_init_block_last_lit <= 0 /\ -1 * s V_init_block_last_lit <= 0 /\ 1 * s V_init_block_flag_bit + -1 <= 0 /\ -1 * s V_init_block_flag_bit + 1 <= 0)%Z
   | 25 => (-1 * s V_init_block_flag_bit + 1 <= 0 /\ 1 * s V_init_block_flag_bit + -1 <= 0 /\ -1 * s V_init_block_last_lit <= 0 /\ 1 * s V_init_block_last_lit <= 0 /\ -1 * s V_init_block_last_flags <= 0 /\ 1 * s V_init_block_last_flags <= 0 /\ -1 * s V_init_block_static_len <= 0 /\ 1 * s V_init_block_static_len <= 0 /\ -1 * s V_init_block_n + 19 <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -19 <= 0 /\ 1 * s V_init_block_opt_len <= 0 /\ -1 * s V_init_block_opt_len <= 0 /\ 1 * s V_init_block_last_dist <= 0 /\ -1 * s V_init_block_last_dist <= 0 /\ 1 * s V_init_block_flags <= 0 /\ -1 * s V_init_block_flags <= 0)%Z
   | 26 => (-1 * s V_init_block_n <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -18 <= 0)%Z
   | 27 => (1 * s V_init_block_n + -18 <= 0 /\ -1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n <= 0)%Z
   | 28 => (-1 * s V_init_block_n <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -18 <= 0)%Z
   | 29 => (-1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 1 <= 0 /\ 1 * s V_init_block_n + -19 <= 0)%Z
   | 30 => (1 * s V_init_block_n + -19 <= 0 /\ -1 * s V_init_block_n + 1 <= 0 /\ -1 * s V_init_block_z <= 0)%Z
   | 31 => (-1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 1 <= 0 /\ 1 * s V_init_block_n + -19 <= 0)%Z
   | 32 => (1 * s V_init_block_n + -19 <= 0 /\ -1 * s V_init_block_n + 1 <= 0 /\ -1 * s V_init_block_z + 1 <= 0)%Z
   | 33 => (-1 * s V_init_block_n <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -29 <= 0)%Z
   | 34 => (1 * s V_init_block_n + -29 <= 0 /\ -1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n <= 0)%Z
   | 35 => (-1 * s V_init_block_n <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -29 <= 0)%Z
   | 36 => (-1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 1 <= 0 /\ 1 * s V_init_block_n + -30 <= 0)%Z
   | 37 => (1 * s V_init_block_n + -30 <= 0 /\ -1 * s V_init_block_n + 1 <= 0 /\ -1 * s V_init_block_z <= 0)%Z
   | 38 => (-1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 1 <= 0 /\ 1 * s V_init_block_n + -30 <= 0)%Z
   | 39 => (1 * s V_init_block_n + -30 <= 0 /\ -1 * s V_init_block_n + 1 <= 0 /\ -1 * s V_init_block_z + 1 <= 0)%Z
   | 40 => (-1 * s V_init_block_n <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -285 <= 0)%Z
   | 41 => (1 * s V_init_block_n + -285 <= 0 /\ -1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n <= 0)%Z
   | 42 => (-1 * s V_init_block_n <= 0 /\ -1 * s V_init_block_z <= 0 /\ 1 * s V_init_block_n + -285 <= 0)%Z
   | 43 => (-1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 1 <= 0 /\ 1 * s V_init_block_n + -286 <= 0)%Z
   | 44 => (1 * s V_init_block_n + -286 <= 0 /\ -1 * s V_init_block_n + 1 <= 0 /\ -1 * s V_init_block_z <= 0)%Z
   | 45 => (-1 * s V_init_block_z <= 0 /\ -1 * s V_init_block_n + 1 <= 0 /\ 1 * s V_init_block_n + -286 <= 0)%Z
   | 46 => (1 * s V_init_block_n + -286 <= 0 /\ -1 * s V_init_block_n + 1 <= 0 /\ -1 * s V_init_block_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_init_block (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((335 # 1) <= z)%Q
   | 2 => ((335 # 1) + s V_init_block_z <= z)%Q
   | 3 => ((335 # 1) - s V_init_block_n + s V_init_block_z <= z)%Q
   | 4 => ((335 # 1) - s V_init_block_n + s V_init_block_z <= z)%Q
   | 5 => ((335 # 1) - s V_init_block_n + s V_init_block_z <= z)%Q
   | 6 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (286 - s V_init_block_n) (285
                                                                    - s V_init_block_n));
      (*-1 0*) F_max0_ge_0 (285 - s V_init_block_n);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (286
                                                               - s V_init_block_n) (0))) (F_max0_ge_0 (286
                                                                    - s V_init_block_n))]
     ((335 # 1) - s V_init_block_n + s V_init_block_z <= z)%Q
   | 7 => ((49 # 1) + s V_init_block_z <= z)%Q
   | 8 => ((19 # 1) + s V_init_block_z + max0(30 - s V_init_block_n) <= z)%Q
   | 9 => ((19 # 1) + s V_init_block_z + max0(30 - s V_init_block_n) <= z)%Q
   | 10 => ((19 # 1) + s V_init_block_z + max0(30 - s V_init_block_n) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (30 - s V_init_block_n) (29
                                                                    - 
                                                                    s V_init_block_n));
      (*-1 0*) F_max0_ge_0 (29 - s V_init_block_n)]
     ((19 # 1) + s V_init_block_z + max0(30 - s V_init_block_n) <= z)%Q
   | 12 => ((19 # 1) + s V_init_block_z <= z)%Q
   | 13 => (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 14 => (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 15 => (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 16 => (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 17 => (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 18 => (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 19 => (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 20 => (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 21 => (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 22 => (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 23 => (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (19 - s V_init_block_n) (18
                                                                    - 
                                                                    s V_init_block_n));
      (*-1 0*) F_max0_ge_0 (18 - s V_init_block_n)]
     (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 25 => (s V_init_block_z <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (19 - s V_init_block_n) (1)]
     (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 27 => ((1 # 1) + s V_init_block_z + max0(18 - s V_init_block_n) <= z)%Q
   | 28 => ((1 # 1) + s V_init_block_z + max0(18 - s V_init_block_n) <= z)%Q
   | 29 => ((1 # 1) + s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 30 => ((1 # 1) + s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 31 => ((1 # 1) + s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 32 => (s V_init_block_z + max0(19 - s V_init_block_n) <= z)%Q
   | 33 => hints
     [(*0 1*) F_max0_pre_decrement 1 (30 - s V_init_block_n) (1)]
     ((19 # 1) + s V_init_block_z + max0(30 - s V_init_block_n) <= z)%Q
   | 34 => ((20 # 1) + s V_init_block_z + max0(29 - s V_init_block_n) <= z)%Q
   | 35 => ((20 # 1) + s V_init_block_z + max0(29 - s V_init_block_n) <= z)%Q
   | 36 => ((20 # 1) + s V_init_block_z + max0(30 - s V_init_block_n) <= z)%Q
   | 37 => ((20 # 1) + s V_init_block_z + max0(30 - s V_init_block_n) <= z)%Q
   | 38 => ((20 # 1) + s V_init_block_z + max0(30 - s V_init_block_n) <= z)%Q
   | 39 => ((19 # 1) + s V_init_block_z + max0(30 - s V_init_block_n) <= z)%Q
   | 40 => ((335 # 1) - s V_init_block_n + s V_init_block_z <= z)%Q
   | 41 => ((335 # 1) - s V_init_block_n + s V_init_block_z <= z)%Q
   | 42 => ((335 # 1) - s V_init_block_n + s V_init_block_z <= z)%Q
   | 43 => ((336 # 1) - s V_init_block_n + s V_init_block_z <= z)%Q
   | 44 => ((336 # 1) - s V_init_block_n + s V_init_block_z <= z)%Q
   | 45 => ((336 # 1) - s V_init_block_n + s V_init_block_z <= z)%Q
   | 46 => ((335 # 1) - s V_init_block_n + s V_init_block_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_init_block =>
    [mkPA Q (fun n z s => ai_init_block n s /\ annot0_init_block n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_init_block (proc_start P_init_block) s1 (proc_end P_init_block) s2 ->
    (s2 V_init_block_z <= (335 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_init_block.
Qed.
