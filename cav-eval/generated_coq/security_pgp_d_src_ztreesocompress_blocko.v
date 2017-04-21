Require Import pasta.Pasta.

Inductive proc: Type :=
  P_compress_block.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_compress_block_z := 1%positive.
Notation V_compress_block_code := 2%positive.
Notation V_compress_block_dist := 3%positive.
Notation V_compress_block_dx := 4%positive.
Notation V_compress_block_extra := 5%positive.
Notation V_compress_block_flag := 6%positive.
Notation V_compress_block_fx := 7%positive.
Notation V_compress_block_last_lit := 8%positive.
Notation V_compress_block_lc := 9%positive.
Notation V_compress_block_lx := 10%positive.
Notation V_compress_block_dtree := 11%positive.
Notation V_compress_block_ltree := 12%positive.
Definition Pedges_compress_block: list (edge proc) :=
  (EA 1 (AAssign V_compress_block_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_compress_block_lx) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_compress_block_last_lit)
  s) >= (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_compress_block_dist) s) >= (eval (ENum (0))
  s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 (AAssign V_compress_block_lx
  (Some (ENum (0)))) 7)::(EA 7 (AAssign V_compress_block_dx
  (Some (ENum (0)))) 8)::(EA 8 (AAssign V_compress_block_fx
  (Some (ENum (0)))) 9)::(EA 9 (AAssign V_compress_block_flag
  (Some (ENum (0)))) 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_compress_block_last_lit) s) <> (eval (ENum (0))
  s))%Z)) 13)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_compress_block_last_lit) s) = (eval (ENum (0))
  s))%Z)) 12)::(EA 12 AWeaken 57)::(EA 13 AWeaken 14)::(EA 14 ANone 15)::
  (EA 15 AWeaken 16)::(EA 16 ANone 17)::(EA 16 ANone 20)::(EA 17 (AAssign
  V_compress_block_fx (Some (EAdd (EVar V_compress_block_fx)
  (ENum (1))))) 18)::(EA 18 (AAssign V_compress_block_flag None) 19)::
  (EA 19 ANone 20)::(EA 20 (AAssign V_compress_block_lx
  (Some (EAdd (EVar V_compress_block_lx) (ENum (1))))) 21)::(EA 21 (AAssign
  V_compress_block_lc None) 22)::(EA 22 AWeaken 23)::(EA 23 ANone 49)::
  (EA 23 ANone 24)::(EA 24 (AAssign V_compress_block_code None) 25)::
  (EA 25 (AAssign V_compress_block_extra None) 26)::(EA 26 AWeaken 27)::
  (EA 27 (AGuard (fun s => ((eval (EVar V_compress_block_extra) s) <>
  (eval (ENum (0)) s))%Z)) 29)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_compress_block_extra) s) = (eval (ENum (0))
  s))%Z)) 28)::(EA 28 AWeaken 32)::(EA 29 AWeaken 30)::(EA 30 (AAssign
  V_compress_block_lc None) 31)::(EA 31 ANone 32)::(EA 32 (AAssign
  V_compress_block_dx (Some (EAdd (EVar V_compress_block_dx)
  (ENum (1))))) 33)::(EA 33 (AAssign V_compress_block_dist None) 34)::
  (EA 34 AWeaken 35)::(EA 35 (AGuard
  (fun s => ((eval (EVar V_compress_block_dist) s) < (eval (ENum (256))
  s))%Z)) 38)::(EA 35 (AGuard (fun s => ((eval (EVar V_compress_block_dist)
  s) >= (eval (ENum (256)) s))%Z)) 36)::(EA 36 AWeaken 37)::
  (EA 37 ANone 40)::(EA 38 AWeaken 39)::(EA 39 ANone 40)::(EA 40 (AAssign
  V_compress_block_code None) 41)::(EA 41 (AAssign V_compress_block_extra
  None) 42)::(EA 42 AWeaken 43)::(EA 43 (AGuard
  (fun s => ((eval (EVar V_compress_block_extra) s) <> (eval (ENum (0))
  s))%Z)) 45)::(EA 43 (AGuard (fun s => ((eval (EVar V_compress_block_extra)
  s) = (eval (ENum (0)) s))%Z)) 44)::(EA 44 AWeaken 48)::(EA 45 AWeaken 46)::
  (EA 46 (AAssign V_compress_block_dist None) 47)::(EA 47 ANone 48)::
  (EA 48 ANone 50)::(EA 49 ANone 50)::(EA 50 (AAssign V_compress_block_flag
  None) 51)::(EA 51 ANone 52)::(EA 52 AWeaken 53)::(EA 53 (AGuard
  (fun s => ((eval (EVar V_compress_block_lx) s) <
  (eval (EVar V_compress_block_last_lit) s))%Z)) 58)::(EA 53 (AGuard
  (fun s => ((eval (EVar V_compress_block_lx) s) >=
  (eval (EVar V_compress_block_last_lit) s))%Z)) 54)::(EA 54 AWeaken 55)::
  (EA 55 ANone 56)::(EA 56 AWeaken 57)::(EA 58 AWeaken 59)::
  (EA 59 ANone 60)::(EA 60 (AAssign V_compress_block_z (Some (EAdd (ENum (1))
  (EVar V_compress_block_z)))) 61)::(EA 61 AWeaken 16)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_compress_block => Pedges_compress_block
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_compress_block => 57
     end)%positive;
  var_global := var_global
}.

Definition ai_compress_block (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_z <= 0)%Z
   | 3 => (-1 * s V_compress_block_z <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_lx <= 0)%Z
   | 4 => (-1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_last_lit <= 0)%Z
   | 5 => (-1 * s V_compress_block_last_lit <= 0 /\ -1 * s V_compress_block_z <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_dist <= 0)%Z
   | 6 => (-1 * s V_compress_block_dist <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_last_lit <= 0)%Z
   | 7 => (-1 * s V_compress_block_last_lit <= 0 /\ -1 * s V_compress_block_z <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dist <= 0 /\ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx <= 0)%Z
   | 8 => (-1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_dist <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_last_lit <= 0 /\ 1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_dx <= 0)%Z
   | 9 => (-1 * s V_compress_block_dx <= 0 /\ 1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_last_lit <= 0 /\ -1 * s V_compress_block_z <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dist <= 0 /\ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_fx <= 0)%Z
   | 10 => (-1 * s V_compress_block_fx <= 0 /\ 1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_dist <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_last_lit <= 0 /\ 1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ 1 * s V_compress_block_flag <= 0 /\ -1 * s V_compress_block_flag <= 0)%Z
   | 11 => (-1 * s V_compress_block_flag <= 0 /\ 1 * s V_compress_block_flag <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ 1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_last_lit <= 0 /\ -1 * s V_compress_block_z <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dist <= 0 /\ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_fx <= 0)%Z
   | 12 => (-1 * s V_compress_block_fx <= 0 /\ 1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_dist <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_last_lit <= 0 /\ 1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ 1 * s V_compress_block_flag <= 0 /\ -1 * s V_compress_block_flag <= 0 /\ 1 * s V_compress_block_last_lit <= 0)%Z
   | 13 => (-1 * s V_compress_block_fx <= 0 /\ 1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_dist <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_z <= 0 /\ 1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ 1 * s V_compress_block_flag <= 0 /\ -1 * s V_compress_block_flag <= 0 /\ -1 * s V_compress_block_last_lit + 1 <= 0)%Z
   | 14 => (-1 * s V_compress_block_last_lit + 1 <= 0 /\ -1 * s V_compress_block_flag <= 0 /\ 1 * s V_compress_block_flag <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ 1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dist <= 0 /\ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_fx <= 0)%Z
   | 15 => (-1 * s V_compress_block_fx <= 0 /\ 1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_dist <= 0 /\ 1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_z <= 0 /\ 1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ 1 * s V_compress_block_flag <= 0 /\ -1 * s V_compress_block_flag <= 0 /\ -1 * s V_compress_block_last_lit + 1 <= 0)%Z
   | 16 => (-1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx + 1 <= 0)%Z
   | 17 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_z <= 0)%Z
   | 18 => (-1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx + 1 <= 0)%Z
   | 19 => (-1 * s V_compress_block_fx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_z <= 0)%Z
   | 20 => (-1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx + 1 <= 0)%Z
   | 21 => (-1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 22 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx <= 0)%Z
   | 23 => (-1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 24 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx <= 0)%Z
   | 25 => (-1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 26 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx <= 0)%Z
   | 27 => (-1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 28 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ 1 * s V_compress_block_extra <= 0 /\ -1 * s V_compress_block_extra <= 0)%Z
   | 29 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx <= 0)%Z
   | 30 => (-1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 31 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx <= 0)%Z
   | 32 => (-1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 33 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx + 1 <= 0)%Z
   | 34 => (-1 * s V_compress_block_dx + 1 <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 35 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx + 1 <= 0)%Z
   | 36 => (-1 * s V_compress_block_dx + 1 <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_dist + 256 <= 0)%Z
   | 37 => (-1 * s V_compress_block_dist + 256 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx + 1 <= 0)%Z
   | 38 => (-1 * s V_compress_block_dx + 1 <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_dist + -255 <= 0)%Z
   | 39 => (1 * s V_compress_block_dist + -255 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx + 1 <= 0)%Z
   | 40 => (-1 * s V_compress_block_dx + 1 <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 41 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx + 1 <= 0)%Z
   | 42 => (-1 * s V_compress_block_dx + 1 <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 43 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx + 1 <= 0)%Z
   | 44 => (-1 * s V_compress_block_dx + 1 <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_extra <= 0 /\ -1 * s V_compress_block_extra <= 0)%Z
   | 45 => (-1 * s V_compress_block_dx + 1 <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 46 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx + 1 <= 0)%Z
   | 47 => (-1 * s V_compress_block_dx + 1 <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 48 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx + 1 <= 0)%Z
   | 49 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx <= 0)%Z
   | 50 => (-1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 51 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx <= 0)%Z
   | 52 => (-1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0)%Z
   | 53 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx <= 0)%Z
   | 54 => (-1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_last_lit+ -1 * s V_compress_block_lx <= 0)%Z
   | 55 => (1 * s V_compress_block_last_lit+ -1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx <= 0)%Z
   | 56 => (-1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_last_lit+ -1 * s V_compress_block_lx <= 0)%Z
   | 57 => (-1 * s V_compress_block_lx <= 0 /\ 1 * s V_compress_block_last_lit+ -1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx <= 0)%Z
   | 58 => (-1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx + 1 <= 0)%Z
   | 59 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_dx <= 0)%Z
   | 60 => (-1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx + 1 <= 0)%Z
   | 61 => (-1 * s V_compress_block_last_lit+ 1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_lx + 1 <= 0 /\ -1 * s V_compress_block_fx <= 0 /\ -1 * s V_compress_block_dx <= 0 /\ -1 * s V_compress_block_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_compress_block (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 2 => (max0(-1 + s V_compress_block_last_lit)
           + max0(s V_compress_block_z) <= z)%Q
   | 3 => (max0(-1 + s V_compress_block_last_lit)
           + max0(s V_compress_block_z) <= z)%Q
   | 4 => (max0(-1 + s V_compress_block_last_lit)
           + max0(s V_compress_block_z) <= z)%Q
   | 5 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_compress_block_z)) (F_check_ge (s V_compress_block_z) (0))]
     (max0(-1 + s V_compress_block_last_lit) + max0(s V_compress_block_z) <= z)%Q
   | 6 => (s V_compress_block_z + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 7 => (s V_compress_block_z + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 8 => (s V_compress_block_z + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 9 => (s V_compress_block_z + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 10 => (s V_compress_block_z + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 11 => (s V_compress_block_z + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_compress_block_last_lit)) (F_check_ge (0) (0))]
     (s V_compress_block_z + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 13 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (s V_compress_block_lx)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_compress_block_lx) (0))) (F_max0_ge_0 (s V_compress_block_lx))]
     (s V_compress_block_z + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 14 => (-s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 15 => (-s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 16 => (-s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 17 => (-s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 18 => (-s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 19 => (-s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 20 => (-s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 21 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 22 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 23 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 24 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 25 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 26 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 27 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 28 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 29 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 30 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 31 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 32 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 33 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 34 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 35 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 36 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 37 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 38 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 39 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 40 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 41 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 42 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 43 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 44 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 45 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 46 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 47 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 48 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 49 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 50 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 51 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 52 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 53 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 54 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 55 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 56 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_compress_block_last_lit
                                             - s V_compress_block_lx) (-1
                                                                    + s V_compress_block_last_lit
                                                                    - s V_compress_block_lx));
      (*-1 0*) F_max0_ge_0 (-1 + s V_compress_block_last_lit
                            - s V_compress_block_lx);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_compress_block_last_lit
                                                              - s V_compress_block_lx) (0))) (F_max0_ge_0 (s V_compress_block_last_lit
                                                                    - s V_compress_block_lx));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-1
                                                   + s V_compress_block_last_lit)) (F_check_ge (-1
                                                                    + s V_compress_block_last_lit) (0))]
     ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
      + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 57 => (s V_compress_block_z <= z)%Q
   | 58 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 59 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 60 => ((1 # 1) - s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | 61 => (-s V_compress_block_lx + s V_compress_block_z
            + max0(-1 + s V_compress_block_last_lit) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_compress_block =>
    [mkPA Q (fun n z s => ai_compress_block n s /\ annot0_compress_block n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_compress_block (proc_start P_compress_block) s1 (proc_end P_compress_block) s2 ->
    (s2 V_compress_block_z <= max0(-1 + s1 V_compress_block_last_lit))%Q.
Proof.
  prove_bound ipa admissible_ipa P_compress_block.
Qed.
