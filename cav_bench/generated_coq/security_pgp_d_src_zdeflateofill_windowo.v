Require Import pasta.Pasta.

Inductive proc: Type :=
  P_fill_window.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_fill_window_z := 1%positive.
Notation V_fill_window_block_start := 2%positive.
Notation V_fill_window_eofile := 3%positive.
Notation V_fill_window_lookahead := 4%positive.
Notation V_fill_window_m := 5%positive.
Notation V_fill_window_match_start := 6%positive.
Notation V_fill_window_more := 7%positive.
Notation V_fill_window_n := 8%positive.
Notation V_fill_window_strstart := 9%positive.
Definition Pedges_fill_window: list (edge proc) :=
  (EA 1 (AAssign V_fill_window_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_fill_window_n) s) >= (eval (ENum (0)) s))%Z)) 3)::
  (EA 3 (AGuard (fun s => ((eval (EVar V_fill_window_more) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_fill_window_m) s) >= (eval (ENum (0)) s))%Z)) 5)::
  (EA 5 AWeaken 6)::(EA 6 (AAssign V_fill_window_more None) 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard (fun s => ((eval (EVar V_fill_window_more)
  s) = (eval (ENum (-1)) s))%Z)) 55)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_fill_window_more) s) <> (eval (ENum (-1))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_fill_window_more) s) <= (eval (ENum (1))
  s))%Z)) 12)::(EA 10 (AGuard (fun s => ((eval (EVar V_fill_window_more) s) >
  (eval (ENum (1)) s))%Z)) 11)::(EA 11 AWeaken 27)::(EA 12 AWeaken 13)::
  (EA 13 (AAssign V_fill_window_match_start
  (Some (ESub (EVar V_fill_window_match_start) (ENum (8192))))) 14)::
  (EA 14 (AAssign V_fill_window_strstart
  (Some (ESub (EVar V_fill_window_strstart) (ENum (8192))))) 15)::
  (EA 15 (AAssign V_fill_window_block_start
  (Some (ESub (EVar V_fill_window_block_start) (ENum (8192))))) 16)::
  (EA 16 (AAssign V_fill_window_n (Some (ENum (0)))) 17)::(EA 17 ANone 18)::
  (EA 18 AWeaken 19)::(EA 19 ANone 42)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_fill_window_n (Some (ENum (0)))) 21)::(EA 21 ANone 22)::
  (EA 22 AWeaken 23)::(EA 23 (AGuard (fun s => ((eval (EVar V_fill_window_n)
  s) < (eval (ENum (8192)) s))%Z)) 28)::(EA 23 (AGuard
  (fun s => ((eval (EVar V_fill_window_n) s) >= (eval (ENum (8192))
  s))%Z)) 24)::(EA 24 AWeaken 25)::(EA 25 (AAssign V_fill_window_more
  (Some (EAdd (EVar V_fill_window_more) (ENum (8192))))) 26)::
  (EA 26 ANone 27)::(EA 27 ANone 58)::(EA 28 AWeaken 29)::(EA 29 (AAssign
  V_fill_window_m None) 30)::(EA 30 AWeaken 31)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_fill_window_m) s) >= (eval (ENum (8192))
  s))%Z)) 34)::(EA 31 (AGuard (fun s => ((eval (EVar V_fill_window_m) s) <
  (eval (ENum (8192)) s))%Z)) 32)::(EA 32 AWeaken 33)::(EA 33 ANone 36)::
  (EA 34 AWeaken 35)::(EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign
  V_fill_window_n (Some (EAdd (EVar V_fill_window_n) (ENum (1))))) 38)::
  (EA 38 ANone 39)::(EA 39 ANone 40)::(EA 40 (AAssign V_fill_window_z
  (Some (EAdd (ENum (1)) (EVar V_fill_window_z)))) 41)::(EA 41 AWeaken 23)::
  (EA 42 (AAssign V_fill_window_m None) 43)::(EA 43 AWeaken 44)::
  (EA 44 (AGuard (fun s => ((eval (EVar V_fill_window_m) s) >=
  (eval (ENum (8192)) s))%Z)) 47)::(EA 44 (AGuard
  (fun s => ((eval (EVar V_fill_window_m) s) < (eval (ENum (8192))
  s))%Z)) 45)::(EA 45 AWeaken 46)::(EA 46 ANone 49)::(EA 47 AWeaken 48)::
  (EA 48 ANone 49)::(EA 49 ANone 50)::(EA 50 (AAssign V_fill_window_n
  (Some (EAdd (EVar V_fill_window_n) (ENum (1))))) 51)::(EA 51 ANone 52)::
  (EA 52 ANone 53)::(EA 53 (AAssign V_fill_window_z (Some (EAdd (ENum (1))
  (EVar V_fill_window_z)))) 54)::(EA 54 AWeaken 19)::(EA 55 AWeaken 56)::
  (EA 56 (AAssign V_fill_window_more (Some (EAdd (EVar V_fill_window_more)
  (ENum (-1))))) 57)::(EA 57 ANone 58)::(EA 58 (AAssign V_fill_window_n
  None) 59)::(EA 59 AWeaken 60)::(EA 60 (AGuard
  (fun s => ((eval (EVar V_fill_window_n) s) = (eval (ENum (0)) s))%Z)) 68)::
  (EA 60 (AGuard (fun s => ((eval (EVar V_fill_window_n) s) <>
  (eval (ENum (0)) s))%Z)) 61)::(EA 61 AWeaken 62)::(EA 62 (AGuard
  (fun s => ((eval (EVar V_fill_window_n) s) = (eval (ENum (-1))
  s))%Z)) 67)::(EA 62 (AGuard (fun s => ((eval (EVar V_fill_window_n) s) <>
  (eval (ENum (-1)) s))%Z)) 63)::(EA 63 AWeaken 64)::(EA 64 (AAssign
  V_fill_window_lookahead (Some (EAdd (EVar V_fill_window_lookahead)
  (EVar V_fill_window_n)))) 65)::(EA 65 ANone 66)::(EA 66 AWeaken 72)::
  (EA 67 AWeaken 69)::(EA 68 AWeaken 69)::(EA 69 (AAssign
  V_fill_window_eofile (Some (ENum (1)))) 70)::(EA 70 ANone 71)::
  (EA 71 AWeaken 72)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_fill_window => Pedges_fill_window
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_fill_window => 72
     end)%positive;
  var_global := var_global
}.

Definition ai_fill_window (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 3 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0)%Z
   | 4 => (-1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_more <= 0)%Z
   | 5 => (-1 * s V_fill_window_more <= 0 /\ -1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_m <= 0)%Z
   | 6 => (-1 * s V_fill_window_m <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_more <= 0)%Z
   | 7 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_m <= 0)%Z
   | 8 => (-1 * s V_fill_window_m <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 9 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_m <= 0)%Z
   | 10 => (-1 * s V_fill_window_m <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 11 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_m <= 0 /\ -1 * s V_fill_window_more + 2 <= 0)%Z
   | 12 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_m <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 13 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_m <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 14 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_m <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 15 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_m <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 16 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_m <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 17 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_m <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_n <= 0)%Z
   | 18 => (-1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_m <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 19 => (-1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 20 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 21 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ 1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_n <= 0)%Z
   | 22 => (-1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 23 => (-1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ 1 * s V_fill_window_n + -8192 <= 0)%Z
   | 24 => (1 * s V_fill_window_n + -8192 <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n + 8192 <= 0)%Z
   | 25 => (-1 * s V_fill_window_n + 8192 <= 0 /\ -1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ 1 * s V_fill_window_n + -8192 <= 0)%Z
   | 26 => (1 * s V_fill_window_n + -8192 <= 0 /\ -1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n + 8192 <= 0 /\ 1 * s V_fill_window_more + -8193 <= 0)%Z
   | 27 => (-1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 28 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_n + -8191 <= 0)%Z
   | 29 => (1 * s V_fill_window_n + -8191 <= 0 /\ -1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 30 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_n + -8191 <= 0)%Z
   | 31 => (1 * s V_fill_window_n + -8191 <= 0 /\ -1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 32 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_n + -8191 <= 0 /\ 1 * s V_fill_window_m + -8191 <= 0)%Z
   | 33 => (1 * s V_fill_window_m + -8191 <= 0 /\ 1 * s V_fill_window_n + -8191 <= 0 /\ -1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 34 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_n + -8191 <= 0 /\ -1 * s V_fill_window_m + 8192 <= 0)%Z
   | 35 => (-1 * s V_fill_window_m + 8192 <= 0 /\ 1 * s V_fill_window_n + -8191 <= 0 /\ -1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 36 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_n + -8191 <= 0)%Z
   | 37 => (1 * s V_fill_window_n + -8191 <= 0 /\ -1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 38 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_n + -8192 <= 0 /\ -1 * s V_fill_window_n + 1 <= 0)%Z
   | 39 => (-1 * s V_fill_window_n + 1 <= 0 /\ 1 * s V_fill_window_n + -8192 <= 0 /\ -1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 40 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_n + -8192 <= 0 /\ -1 * s V_fill_window_n + 1 <= 0)%Z
   | 41 => (-1 * s V_fill_window_n + 1 <= 0 /\ 1 * s V_fill_window_n + -8192 <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_z + 1 <= 0)%Z
   | 42 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 43 => (-1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 44 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 45 => (-1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ 1 * s V_fill_window_m + -8191 <= 0)%Z
   | 46 => (1 * s V_fill_window_m + -8191 <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 47 => (-1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_m + 8192 <= 0)%Z
   | 48 => (-1 * s V_fill_window_m + 8192 <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 49 => (-1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_more + -1 <= 0)%Z
   | 50 => (1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 51 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n + 1 <= 0)%Z
   | 52 => (-1 * s V_fill_window_n + 1 <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 53 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_n + 1 <= 0)%Z
   | 54 => (-1 * s V_fill_window_n + 1 <= 0 /\ 1 * s V_fill_window_more + -1 <= 0 /\ -1 * s V_fill_window_z + 1 <= 0)%Z
   | 55 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_m <= 0 /\ 1 * s V_fill_window_more + 1 <= 0 /\ -1 * s V_fill_window_more + -1 <= 0)%Z
   | 56 => (-1 * s V_fill_window_more + -1 <= 0 /\ 1 * s V_fill_window_more + 1 <= 0 /\ -1 * s V_fill_window_m <= 0 /\ -1 * s V_fill_window_n <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 57 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_z <= 0 /\ -1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_m <= 0 /\ -1 * s V_fill_window_more + -2 <= 0 /\ 1 * s V_fill_window_more + 2 <= 0)%Z
   | 58 => (-1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 59 => (-1 * s V_fill_window_z <= 0)%Z
   | 60 => (-1 * s V_fill_window_z <= 0)%Z
   | 61 => (-1 * s V_fill_window_z <= 0)%Z
   | 62 => (-1 * s V_fill_window_z <= 0)%Z
   | 63 => (-1 * s V_fill_window_z <= 0)%Z
   | 64 => (-1 * s V_fill_window_z <= 0)%Z
   | 65 => (-1 * s V_fill_window_z <= 0)%Z
   | 66 => (-1 * s V_fill_window_z <= 0)%Z
   | 67 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_n + 1 <= 0 /\ -1 * s V_fill_window_n + -1 <= 0)%Z
   | 68 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_n <= 0)%Z
   | 69 => (-1 * s V_fill_window_n + -1 <= 0 /\ 1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 70 => (-1 * s V_fill_window_z <= 0 /\ 1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_n + -1 <= 0 /\ 1 * s V_fill_window_eofile + -1 <= 0 /\ -1 * s V_fill_window_eofile + 1 <= 0)%Z
   | 71 => (-1 * s V_fill_window_eofile + 1 <= 0 /\ 1 * s V_fill_window_eofile + -1 <= 0 /\ -1 * s V_fill_window_n + -1 <= 0 /\ 1 * s V_fill_window_n <= 0 /\ -1 * s V_fill_window_z <= 0)%Z
   | 72 => (-1 * s V_fill_window_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_fill_window (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (0 <= z)%Q
   | 2 => ((1 # 2) * s V_fill_window_fresh_dot_7 * max0(s V_fill_window_z)
           - (1 # 2) * max0(-2 + s V_fill_window_fresh_dot_7) * max0(s V_fill_window_z) <= z)%Q
   | 3 => ((1 # 2) * s V_fill_window_fresh_dot_7 * max0(s V_fill_window_z)
           - (1 # 2) * max0(-2 + s V_fill_window_fresh_dot_7) * max0(s V_fill_window_z) <= z)%Q
   | 4 => ((1 # 2) * s V_fill_window_fresh_dot_7 * max0(s V_fill_window_z)
           - (1 # 2) * max0(-2 + s V_fill_window_fresh_dot_7) * max0(s V_fill_window_z) <= z)%Q
   | 5 => ((1 # 2) * s V_fill_window_fresh_dot_7 * max0(s V_fill_window_z)
           - (1 # 2) * max0(-2 + s V_fill_window_fresh_dot_7) * max0(s V_fill_window_z) <= z)%Q
   | 6 => ((1 # 2) * s V_fill_window_fresh_dot_7 * max0(s V_fill_window_z)
           - (1 # 2) * max0(-2 + s V_fill_window_fresh_dot_7) * max0(s V_fill_window_z) <= z)%Q
   | 7 => ((1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
           - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z) <= z)%Q
   | 8 => ((1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
           - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z) <= z)%Q
   | 9 => ((1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
           - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z) <= z)%Q
   | 10 => ((1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
            - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z) <= z)%Q
   | 11 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-2
                                                                    + s V_fill_window_more) (0))) (F_max0_ge_0 (-2
                                                                    + s V_fill_window_more))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0)))]
     ((1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
      - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z) <= z)%Q
   | 12 => ((1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
            - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z) <= z)%Q
   | 13 => ((1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
            - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z) <= z)%Q
   | 14 => ((1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
            - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z) <= z)%Q
   | 15 => ((1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
            - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z) <= z)%Q
   | 16 => ((1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
            - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z) <= z)%Q
   | 17 => ((1 # 2)
            + (1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
            + (0 # 1) * s V_fill_window_z
            - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z)
            + (0 # 1) * max0(8192 - s V_fill_window_n) * max0(s V_fill_window_z)
            - (1 # 2) * max0(s V_fill_window_z) <= z)%Q
   | 18 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                              + s V_fill_window_more)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_fill_window_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                                - s V_fill_window_more)) (F_check_ge (1
                                                                    - s V_fill_window_more) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                              - s V_fill_window_more)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_fill_window_z)) (F_check_ge (0) (0)));
      (*-6.10352e-05 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (8192
                                                                    - s V_fill_window_n)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_fill_window_z) (0))) (F_max0_ge_0 (-
                                                                    s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_fill_window_more)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_fill_window_z) (0))) (F_max0_ge_0 (-
                                                                    s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_fill_window_more)) (F_check_ge (0) (0)))]
     ((1 # 2) + (1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
      + (0 # 1) * s V_fill_window_z
      - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z)
      + (0 # 1) * max0(8192 - s V_fill_window_n) * max0(s V_fill_window_z)
      - (1 # 2) * max0(s V_fill_window_z) <= z)%Q
   | 19 => ((1 # 2) + (0 # 1) * s V_fill_window_z
            + (1 # 2) * s V_fill_window_z * max0(-2 + s V_fill_window_more)
            + (1 # 2) * s V_fill_window_z * max0(1 - s V_fill_window_more)
            - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z)
            - (1 # 2) * max0(1 - s V_fill_window_more) * max0(s V_fill_window_z)
            - (0 # 1) * max0(s V_fill_window_z) <= z)%Q
   | 20 => ((1 # 2) + (0 # 1) * s V_fill_window_z
            + (1 # 2) * s V_fill_window_z * max0(-2 + s V_fill_window_more)
            + (1 # 2) * s V_fill_window_z * max0(1 - s V_fill_window_more)
            - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z)
            - (1 # 2) * max0(1 - s V_fill_window_more) * max0(s V_fill_window_z)
            - (0 # 1) * max0(s V_fill_window_z) <= z)%Q
   | 21 => ((1 # 2) + (0 # 1) * s V_fill_window_n * max0(s V_fill_window_z)
            + (1 # 2) * s V_fill_window_z * max0(-2 + s V_fill_window_more)
            + (1 # 2) * s V_fill_window_z * max0(1 - s V_fill_window_more)
            + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n)
            - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z)
            - (1 # 2) * max0(1 - s V_fill_window_more) * max0(s V_fill_window_z)
            - (0 # 1) * max0(1 + s V_fill_window_n) * max0(s V_fill_window_z)
            - (0 # 1) * max0(s V_fill_window_n)
            + (0 # 1) * max0(s V_fill_window_z) <= z)%Q
   | 22 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fill_window_z) (0))) (F_max0_ge_0 (s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_fill_window_more)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fill_window_z) (0))) (F_max0_ge_0 (s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_fill_window_more)) (F_check_ge (0) (0)))]
     ((1 # 2) + (0 # 1) * s V_fill_window_n * max0(s V_fill_window_z)
      + (1 # 2) * s V_fill_window_z * max0(-2 + s V_fill_window_more)
      + (1 # 2) * s V_fill_window_z * max0(1 - s V_fill_window_more)
      + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n)
      - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z)
      - (1 # 2) * max0(1 - s V_fill_window_more) * max0(s V_fill_window_z)
      - (0 # 1) * max0(1 + s V_fill_window_n) * max0(s V_fill_window_z)
      - (0 # 1) * max0(s V_fill_window_n) + (0 # 1) * max0(s V_fill_window_z) <= z)%Q
   | 23 => ((1 # 2) + (0 # 1) * s V_fill_window_n * max0(s V_fill_window_z)
            + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n)
            - (0 # 1) * max0(1 + s V_fill_window_n) * max0(s V_fill_window_z)
            - (0 # 1) * max0(s V_fill_window_n)
            + (0 # 1) * max0(s V_fill_window_z) <= z)%Q
   | 24 => hints
     [(*-0.000122055 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-8192
                                                                    + s V_fill_window_n) (0))) (F_max0_ge_0 (-8192
                                                                    + s V_fill_window_n))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0)));
      (*-0.000122055 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-8192
                                                                    + s V_fill_window_n)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0)));
      (*-0.000122055 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fill_window_z) (0))) (F_max0_ge_0 (s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_fill_window_n)) (F_check_ge (0) (0)));
      (*-0.000122063 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fill_window_n) (0))) (F_max0_ge_0 (s V_fill_window_n))]
     ((1 # 2) + (0 # 1) * s V_fill_window_n * max0(s V_fill_window_z)
      + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n)
      - (0 # 1) * max0(1 + s V_fill_window_n) * max0(s V_fill_window_z)
      - (0 # 1) * max0(s V_fill_window_n) + (0 # 1) * max0(s V_fill_window_z) <= z)%Q
   | 25 => (max0(s V_fill_window_z) <= z)%Q
   | 26 => (max0(s V_fill_window_z) <= z)%Q
   | 27 => (max0(s V_fill_window_z) <= z)%Q
   | 28 => hints
     [(*4.75762e-07 0*) F_one;
      (*0 0.000122055*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + s V_fill_window_n) (0))) (F_max0_ge_0 (1
                                                                    + s V_fill_window_n))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0)))]
     ((1 # 2) + (0 # 1) * s V_fill_window_n * max0(s V_fill_window_z)
      + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n)
      - (0 # 1) * max0(1 + s V_fill_window_n) * max0(s V_fill_window_z)
      - (0 # 1) * max0(s V_fill_window_n) + (0 # 1) * max0(s V_fill_window_z) <= z)%Q
   | 29 => ((1 # 2)
            + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n) <= z)%Q
   | 30 => ((1 # 2)
            + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n) <= z)%Q
   | 31 => ((1 # 2)
            + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n) <= z)%Q
   | 32 => hints
     [(*-4.75697e-07 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_window_z)) (F_check_ge (s V_fill_window_z) (0))]
     ((1 # 2) + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n) <= z)%Q
   | 33 => ((1 # 2) + (0 # 1) * s V_fill_window_z
            + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n) <= z)%Q
   | 34 => hints
     [(*0 -4.75697e-07*) F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0));
      (*0 4.75697e-07*) F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_window_z)) (F_check_ge (s V_fill_window_z) (0))]
     ((1 # 2) + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n) <= z)%Q
   | 35 => ((1 # 2) + (0 # 1) * s V_fill_window_z
            + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n) <= z)%Q
   | 36 => ((1 # 2) + (0 # 1) * s V_fill_window_z
            + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n) <= z)%Q
   | 37 => ((1 # 2) + (0 # 1) * s V_fill_window_z
            + (0 # 1) * s V_fill_window_z * max0(1 + s V_fill_window_n) <= z)%Q
   | 38 => ((1 # 2) + (0 # 1) * s V_fill_window_z
            + (0 # 1) * s V_fill_window_z * max0(s V_fill_window_n) <= z)%Q
   | 39 => ((1 # 2) + (0 # 1) * s V_fill_window_z
            + (0 # 1) * s V_fill_window_z * max0(s V_fill_window_n) <= z)%Q
   | 40 => ((1 # 2) + (0 # 1) * s V_fill_window_z
            + (0 # 1) * s V_fill_window_z * max0(s V_fill_window_n) <= z)%Q
   | 41 => hints
     [(*-0.000122055 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_window_n)) (F_check_ge (s V_fill_window_n) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0)));
      (*-0.000122055 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fill_window_z) (0))) (F_max0_ge_0 (s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_n)) (F_check_ge (0) (0)));
      (*-0.000122055 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_window_z)) (F_check_ge (s V_fill_window_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    + s V_fill_window_n)) (F_check_ge (0) (0)));
      (*4.75697e-07 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_window_z)) (F_check_ge (s V_fill_window_z) (0))]
     ((1 # 2) + (0 # 1) * s V_fill_window_z
      + (0 # 1) * s V_fill_window_z * max0(s V_fill_window_n)
      - (0 # 1) * max0(s V_fill_window_n) <= z)%Q
   | 42 => ((1 # 2) + (0 # 1) * s V_fill_window_z
            + (1 # 2) * s V_fill_window_z * max0(-2 + s V_fill_window_more)
            + (1 # 2) * s V_fill_window_z * max0(1 - s V_fill_window_more)
            - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z)
            - (1 # 2) * max0(1 - s V_fill_window_more) * max0(s V_fill_window_z)
            - (0 # 1) * max0(s V_fill_window_z) <= z)%Q
   | 43 => hints
     [(*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fill_window_z) (0))) (F_max0_ge_0 (s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_fill_window_more)) (F_check_ge (0) (0)));
      (*0 0.5*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fill_window_z) (0))) (F_max0_ge_0 (s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_fill_window_more)) (F_check_ge (0) (0)));
      (*0 2.65597e-07*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fill_window_z) (0))) (F_max0_ge_0 (s V_fill_window_z))]
     ((1 # 2) + (0 # 1) * s V_fill_window_z
      + (1 # 2) * s V_fill_window_z * max0(-2 + s V_fill_window_more)
      + (1 # 2) * s V_fill_window_z * max0(1 - s V_fill_window_more)
      - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z)
      - (1 # 2) * max0(1 - s V_fill_window_more) * max0(s V_fill_window_z)
      - (0 # 1) * max0(s V_fill_window_z) <= z)%Q
   | 44 => ((1 # 2) <= z)%Q
   | 45 => hints
     [(*2.65597e-07 0*) F_one;
      (*2.65597e-07 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0));
      (*-2.65597e-07 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_window_z)) (F_check_ge (s V_fill_window_z) (0))]
     ((1 # 2) <= z)%Q
   | 46 => ((1 # 2) + (0 # 1) * s V_fill_window_z <= z)%Q
   | 47 => hints
     [(*0 -2.65597e-07*) F_one;
      (*0 -2.65597e-07*) F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0));
      (*0 2.65597e-07*) F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_window_z)) (F_check_ge (s V_fill_window_z) (0))]
     ((1 # 2) <= z)%Q
   | 48 => ((1 # 2) + (0 # 1) * s V_fill_window_z <= z)%Q
   | 49 => ((1 # 2) + (0 # 1) * s V_fill_window_z <= z)%Q
   | 50 => ((1 # 2) + (0 # 1) * s V_fill_window_z <= z)%Q
   | 51 => ((1 # 2) + (0 # 1) * s V_fill_window_z <= z)%Q
   | 52 => ((1 # 2) + (0 # 1) * s V_fill_window_z <= z)%Q
   | 53 => ((1 # 2) + (0 # 1) * s V_fill_window_z <= z)%Q
   | 54 => hints
     [(*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_window_z)) (F_check_ge (s V_fill_window_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_fill_window_more)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_window_z)) (F_check_ge (s V_fill_window_z) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                                    - s V_fill_window_more)) (F_check_ge (0) (0)));
      (*0 2.65597e-07*) F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0))]
     ((1 # 2) + (0 # 1) * s V_fill_window_z <= z)%Q
   | 55 => hints
     [(*-1.5 0*) F_binom_monotonic 2 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                              + s V_fill_window_more)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_fill_window_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                                - s V_fill_window_more)) (F_check_ge (-1
                                                                    - s V_fill_window_more) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                              - s V_fill_window_more)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (-
                                                                    s V_fill_window_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_fill_window_z) (0))) (F_max0_ge_0 (-
                                                                    s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_fill_window_more)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_fill_window_z) (0))) (F_max0_ge_0 (-
                                                                    s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_fill_window_more)) (F_check_ge (0) (0)));
      (*-1.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-
                                                                    s V_fill_window_z) (0))) (F_max0_ge_0 (-
                                                                    s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0)));
      (*-1.5 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (-s V_fill_window_z)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fill_window_z) (0))) (F_max0_ge_0 (s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-2
                                                                    + s V_fill_window_more)) (F_check_ge (0) (0)));
      (*-0.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fill_window_z) (0))) (F_max0_ge_0 (s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                                    - s V_fill_window_more)) (F_check_ge (0) (0)));
      (*-1.5 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_fill_window_z) (0))) (F_max0_ge_0 (s V_fill_window_z))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_fill_window_z)) (F_check_ge (0) (0)))]
     ((1 # 2) * s V_fill_window_more * max0(s V_fill_window_z)
      - (1 # 2) * max0(-2 + s V_fill_window_more) * max0(s V_fill_window_z) <= z)%Q
   | 56 => (max0(s V_fill_window_z) <= z)%Q
   | 57 => (max0(s V_fill_window_z) <= z)%Q
   | 58 => (max0(s V_fill_window_z) <= z)%Q
   | 59 => (max0(s V_fill_window_z) <= z)%Q
   | 60 => (max0(s V_fill_window_z) <= z)%Q
   | 61 => (max0(s V_fill_window_z) <= z)%Q
   | 62 => (max0(s V_fill_window_z) <= z)%Q
   | 63 => (max0(s V_fill_window_z) <= z)%Q
   | 64 => (max0(s V_fill_window_z) <= z)%Q
   | 65 => (max0(s V_fill_window_z) <= z)%Q
   | 66 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_window_z)) (F_check_ge (s V_fill_window_z) (0))]
     (max0(s V_fill_window_z) <= z)%Q
   | 67 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_window_z)) (F_check_ge (s V_fill_window_z) (0))]
     (max0(s V_fill_window_z) <= z)%Q
   | 68 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_fill_window_z)) (F_check_ge (s V_fill_window_z) (0))]
     (max0(s V_fill_window_z) <= z)%Q
   | 69 => (s V_fill_window_z <= z)%Q
   | 70 => (s V_fill_window_z <= z)%Q
   | 71 => (s V_fill_window_z <= z)%Q
   | 72 => (s V_fill_window_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_fill_window =>
    [mkPA Q (fun n z s => ai_fill_window n s /\ annot0_fill_window n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_fill_window (proc_start P_fill_window) s1 (proc_end P_fill_window) s2 ->
    (s2 V_fill_window_z <= 0)%Q.
Proof.
  prove_bound ipa admissible_ipa P_fill_window.
Qed.
