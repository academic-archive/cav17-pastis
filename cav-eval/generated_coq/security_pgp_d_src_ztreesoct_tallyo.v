Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ct_tally.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ct_tally_z := 1%positive.
Notation V_ct_tally__tmp := 2%positive.
Notation V_ct_tally__tmp1 := 3%positive.
Notation V_ct_tally__tmp2 := 4%positive.
Notation V_ct_tally_block_start := 5%positive.
Notation V_ct_tally_dcode := 6%positive.
Notation V_ct_tally_flag_bit := 7%positive.
Notation V_ct_tally_flags := 8%positive.
Notation V_ct_tally_in_length := 9%positive.
Notation V_ct_tally_last_dist := 10%positive.
Notation V_ct_tally_last_flags := 11%positive.
Notation V_ct_tally_last_lit := 12%positive.
Notation V_ct_tally_level := 13%positive.
Notation V_ct_tally_out_length := 14%positive.
Notation V_ct_tally_strstart := 15%positive.
Notation V_ct_tally_dist := 16%positive.
Notation V_ct_tally_lc := 17%positive.
Definition Pedges_ct_tally: list (edge proc) :=
  (EA 1 (AAssign V_ct_tally_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_ct_tally_out_length) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_ct_tally_last_dist)
  s) >= (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_ct_tally__tmp (Some (EVar V_ct_tally_dist))) 6)::(EA 6 (AAssign
  V_ct_tally__tmp2 (Some (EVar V_ct_tally_lc))) 7)::(EA 7 (AAssign
  V_ct_tally_last_lit (Some (EAdd (EVar V_ct_tally_last_lit)
  (ENum (1))))) 8)::(EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_ct_tally__tmp) s) = (eval (ENum (0)) s))%Z)) 21)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_ct_tally__tmp) s) <>
  (eval (ENum (0)) s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 (AAssign
  V_ct_tally__tmp (Some (EAdd (EVar V_ct_tally__tmp) (ENum (-1))))) 12)::
  (EA 12 AWeaken 13)::(EA 13 (AGuard (fun s => ((eval (EVar V_ct_tally__tmp)
  s) < (eval (ENum (256)) s))%Z)) 16)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_ct_tally__tmp) s) >= (eval (ENum (256))
  s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 ANone 18)::(EA 16 AWeaken 17)::
  (EA 17 ANone 18)::(EA 18 (AAssign V_ct_tally_last_dist
  (Some (EAdd (EVar V_ct_tally_last_dist) (ENum (1))))) 19)::(EA 19 (AAssign
  V_ct_tally_flags None) 20)::(EA 20 ANone 23)::(EA 21 AWeaken 22)::
  (EA 22 ANone 23)::(EA 23 (AAssign V_ct_tally_flag_bit None) 24)::
  (EA 24 AWeaken 25)::(EA 25 ANone 27)::(EA 25 ANone 26)::
  (EA 26 AWeaken 32)::(EA 27 (AAssign V_ct_tally_last_flags
  (Some (EAdd (EVar V_ct_tally_last_flags) (ENum (1))))) 28)::(EA 28 (AAssign
  V_ct_tally_flags (Some (ENum (0)))) 29)::(EA 29 (AAssign
  V_ct_tally_flag_bit (Some (ENum (1)))) 30)::(EA 30 ANone 31)::
  (EA 31 AWeaken 32)::(EA 32 (AGuard (fun s => ((eval (EVar V_ct_tally_level)
  s) > (eval (ENum (2)) s))%Z)) 34)::(EA 32 (AGuard
  (fun s => ((eval (EVar V_ct_tally_level) s) <= (eval (ENum (2))
  s))%Z)) 33)::(EA 33 AWeaken 51)::(EA 34 AWeaken 35)::(EA 35 ANone 37)::
  (EA 35 ANone 36)::(EA 36 AWeaken 51)::(EA 37 (AAssign V_ct_tally_out_length
  (Some (EMul (EVar V_ct_tally_last_lit) (ENum (8))))) 38)::(EA 38 (AAssign
  V_ct_tally_in_length (Some (ESub (EVar V_ct_tally_strstart)
  (EVar V_ct_tally_block_start)))) 39)::(EA 39 (AAssign V_ct_tally_dcode
  (Some (ENum (0)))) 40)::(EA 40 ANone 41)::(EA 41 AWeaken 42)::
  (EA 42 (AGuard (fun s => ((eval (EVar V_ct_tally_dcode) s) <
  (eval (ENum (30)) s))%Z)) 61)::(EA 42 (AGuard
  (fun s => ((eval (EVar V_ct_tally_dcode) s) >= (eval (ENum (30))
  s))%Z)) 43)::(EA 43 AWeaken 44)::(EA 44 (AAssign V_ct_tally_out_length
  None) 45)::(EA 45 AWeaken 46)::(EA 46 ANone 47)::(EA 46 ANone 49)::
  (EA 47 AWeaken 48)::(EA 48 ANone 57)::(EA 48 ANone 49)::(EA 49 ANone 50)::
  (EA 50 AWeaken 51)::(EA 51 ANone 56)::(EA 51 ANone 52)::(EA 52 ANone 53)::
  (EA 53 (AAssign V_ct_tally__tmp1 None) 54)::(EA 54 ANone 55)::
  (EA 55 AWeaken 60)::(EA 56 AWeaken 60)::(EA 57 (AAssign V_ct_tally__tmp1
  (Some (ENum (1)))) 58)::(EA 58 ANone 59)::(EA 59 AWeaken 60)::
  (EA 61 AWeaken 62)::(EA 62 (AAssign V_ct_tally_out_length None) 63)::
  (EA 63 ANone 64)::(EA 64 (AAssign V_ct_tally_dcode
  (Some (EAdd (EVar V_ct_tally_dcode) (ENum (1))))) 65)::(EA 65 ANone 66)::
  (EA 66 ANone 67)::(EA 67 (AAssign V_ct_tally_z (Some (EAdd (ENum (1))
  (EVar V_ct_tally_z)))) 68)::(EA 68 AWeaken 42)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ct_tally => Pedges_ct_tally
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ct_tally => 60
     end)%positive;
  var_global := var_global
}.

Definition ai_ct_tally (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0)%Z
   | 3 => (-1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0)%Z
   | 4 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 5 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0)%Z
   | 6 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 7 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0)%Z
   | 8 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 9 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0)%Z
   | 10 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 11 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0)%Z
   | 12 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 13 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0)%Z
   | 14 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally__tmp + 256 <= 0)%Z
   | 15 => (-1 * s V_ct_tally__tmp + 256 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0)%Z
   | 16 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ 1 * s V_ct_tally__tmp + -255 <= 0)%Z
   | 17 => (1 * s V_ct_tally__tmp + -255 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0)%Z
   | 18 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 19 => (-1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0 /\ -1 * s V_ct_tally_last_dist + 1 <= 0)%Z
   | 20 => (-1 * s V_ct_tally_last_dist + 1 <= 0 /\ -1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0)%Z
   | 21 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ 1 * s V_ct_tally__tmp <= 0 /\ -1 * s V_ct_tally__tmp <= 0)%Z
   | 22 => (-1 * s V_ct_tally__tmp <= 0 /\ 1 * s V_ct_tally__tmp <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0)%Z
   | 23 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 24 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0)%Z
   | 25 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 26 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0)%Z
   | 27 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0)%Z
   | 28 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 29 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_flags <= 0 /\ -1 * s V_ct_tally_flags <= 0)%Z
   | 30 => (-1 * s V_ct_tally_flags <= 0 /\ 1 * s V_ct_tally_flags <= 0 /\ -1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ 1 * s V_ct_tally_flag_bit + -1 <= 0 /\ -1 * s V_ct_tally_flag_bit + 1 <= 0)%Z
   | 31 => (-1 * s V_ct_tally_flag_bit + 1 <= 0 /\ 1 * s V_ct_tally_flag_bit + -1 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_flags <= 0 /\ -1 * s V_ct_tally_flags <= 0)%Z
   | 32 => (-1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 33 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_level + -2 <= 0)%Z
   | 34 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0)%Z
   | 35 => (-1 * s V_ct_tally_level + 3 <= 0 /\ -1 * s V_ct_tally_out_length <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 36 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0)%Z
   | 37 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_out_length <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0)%Z
   | 38 => (-1 * s V_ct_tally_level + 3 <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 39 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0)%Z
   | 40 => (-1 * s V_ct_tally_level + 3 <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ 1 * s V_ct_tally_dcode <= 0 /\ -1 * s V_ct_tally_dcode <= 0)%Z
   | 41 => (-1 * s V_ct_tally_dcode <= 0 /\ 1 * s V_ct_tally_dcode <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0)%Z
   | 42 => (-1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_dcode <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ 1 * s V_ct_tally_dcode + -30 <= 0)%Z
   | 43 => (1 * s V_ct_tally_dcode + -30 <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_dcode + 30 <= 0)%Z
   | 44 => (-1 * s V_ct_tally_dcode + 30 <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ 1 * s V_ct_tally_dcode + -30 <= 0)%Z
   | 45 => (1 * s V_ct_tally_dcode + -30 <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_dcode + 30 <= 0)%Z
   | 46 => (-1 * s V_ct_tally_dcode + 30 <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ 1 * s V_ct_tally_dcode + -30 <= 0)%Z
   | 47 => (1 * s V_ct_tally_dcode + -30 <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_dcode + 30 <= 0)%Z
   | 48 => (-1 * s V_ct_tally_dcode + 30 <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ 1 * s V_ct_tally_dcode + -30 <= 0)%Z
   | 49 => (1 * s V_ct_tally_dcode + -30 <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_dcode + 30 <= 0)%Z
   | 50 => (-1 * s V_ct_tally_dcode + 30 <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ 1 * s V_ct_tally_dcode + -30 <= 0)%Z
   | 51 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0)%Z
   | 52 => (-1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 53 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0)%Z
   | 54 => (-1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 55 => (-1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0)%Z
   | 56 => (-1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 57 => (1 * s V_ct_tally_dcode + -30 <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_dcode + 30 <= 0)%Z
   | 58 => (-1 * s V_ct_tally_dcode + 30 <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ 1 * s V_ct_tally_dcode + -30 <= 0 /\ 1 * s V_ct_tally__tmp1 + -1 <= 0 /\ -1 * s V_ct_tally__tmp1 + 1 <= 0)%Z
   | 59 => (-1 * s V_ct_tally__tmp1 + 1 <= 0 /\ 1 * s V_ct_tally__tmp1 + -1 <= 0 /\ 1 * s V_ct_tally_dcode + -30 <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_dcode + 30 <= 0)%Z
   | 60 => (-1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0)%Z
   | 61 => (-1 * s V_ct_tally_level + 3 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_dcode <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_dcode + -29 <= 0)%Z
   | 62 => (1 * s V_ct_tally_dcode + -29 <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_dcode <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0)%Z
   | 63 => (-1 * s V_ct_tally_level + 3 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_dcode <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_dcode + -29 <= 0)%Z
   | 64 => (1 * s V_ct_tally_dcode + -29 <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_dcode <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0)%Z
   | 65 => (-1 * s V_ct_tally_level + 3 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_dcode + -30 <= 0 /\ -1 * s V_ct_tally_dcode + 1 <= 0)%Z
   | 66 => (-1 * s V_ct_tally_dcode + 1 <= 0 /\ 1 * s V_ct_tally_dcode + -30 <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0)%Z
   | 67 => (-1 * s V_ct_tally_level + 3 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_z <= 0 /\ 1 * s V_ct_tally_dcode + -30 <= 0 /\ -1 * s V_ct_tally_dcode + 1 <= 0)%Z
   | 68 => (-1 * s V_ct_tally_dcode + 1 <= 0 /\ 1 * s V_ct_tally_dcode + -30 <= 0 /\ -1 * s V_ct_tally_last_dist <= 0 /\ -1 * s V_ct_tally_level + 3 <= 0 /\ -1 * s V_ct_tally_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ct_tally (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((30 # 1) <= z)%Q
   | 2 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 3 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 4 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 5 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 6 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 7 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 8 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 9 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 10 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 11 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 12 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 13 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 14 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 15 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 16 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 17 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 18 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 19 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 20 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 21 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 22 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 23 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 24 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 25 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 26 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 27 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 28 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 29 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 30 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 31 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 32 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 33 => hints
     [(*-30 0*) F_one]
     ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 34 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 35 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 36 => hints
     [(*-30 0*) F_one]
     ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 37 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 38 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 39 => ((30 # 1) + s V_ct_tally_z <= z)%Q
   | 40 => (s V_ct_tally_z + max0(30 - s V_ct_tally_dcode) <= z)%Q
   | 41 => (s V_ct_tally_z + max0(30 - s V_ct_tally_dcode) <= z)%Q
   | 42 => (s V_ct_tally_z + max0(30 - s V_ct_tally_dcode) <= z)%Q
   | 43 => (s V_ct_tally_z + max0(30 - s V_ct_tally_dcode) <= z)%Q
   | 44 => (s V_ct_tally_z + max0(30 - s V_ct_tally_dcode) <= z)%Q
   | 45 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (30 - s V_ct_tally_dcode) (29
                                                                    - s V_ct_tally_dcode))]
     (s V_ct_tally_z + max0(30 - s V_ct_tally_dcode) <= z)%Q
   | 46 => (s V_ct_tally_z + max0(29 - s V_ct_tally_dcode) <= z)%Q
   | 47 => (s V_ct_tally_z + max0(29 - s V_ct_tally_dcode) <= z)%Q
   | 48 => (s V_ct_tally_z + max0(29 - s V_ct_tally_dcode) <= z)%Q
   | 49 => (s V_ct_tally_z + max0(29 - s V_ct_tally_dcode) <= z)%Q
   | 50 => hints
     [(*-1 0*) F_max0_ge_0 (29 - s V_ct_tally_dcode)]
     (s V_ct_tally_z + max0(29 - s V_ct_tally_dcode) <= z)%Q
   | 51 => (s V_ct_tally_z <= z)%Q
   | 52 => (s V_ct_tally_z <= z)%Q
   | 53 => (s V_ct_tally_z <= z)%Q
   | 54 => (s V_ct_tally_z <= z)%Q
   | 55 => (s V_ct_tally_z <= z)%Q
   | 56 => (s V_ct_tally_z <= z)%Q
   | 57 => (s V_ct_tally_z + max0(29 - s V_ct_tally_dcode) <= z)%Q
   | 58 => (s V_ct_tally_z + max0(29 - s V_ct_tally_dcode) <= z)%Q
   | 59 => hints
     [(*-1 0*) F_max0_ge_0 (29 - s V_ct_tally_dcode)]
     (s V_ct_tally_z + max0(29 - s V_ct_tally_dcode) <= z)%Q
   | 60 => (s V_ct_tally_z <= z)%Q
   | 61 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (30 - s V_ct_tally_dcode) (1)]
     (s V_ct_tally_z + max0(30 - s V_ct_tally_dcode) <= z)%Q
   | 62 => ((1 # 1) + s V_ct_tally_z + max0(29 - s V_ct_tally_dcode) <= z)%Q
   | 63 => ((1 # 1) + s V_ct_tally_z + max0(29 - s V_ct_tally_dcode) <= z)%Q
   | 64 => ((1 # 1) + s V_ct_tally_z + max0(29 - s V_ct_tally_dcode) <= z)%Q
   | 65 => ((1 # 1) + s V_ct_tally_z + max0(30 - s V_ct_tally_dcode) <= z)%Q
   | 66 => ((1 # 1) + s V_ct_tally_z + max0(30 - s V_ct_tally_dcode) <= z)%Q
   | 67 => ((1 # 1) + s V_ct_tally_z + max0(30 - s V_ct_tally_dcode) <= z)%Q
   | 68 => (s V_ct_tally_z + max0(30 - s V_ct_tally_dcode) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ct_tally =>
    [mkPA Q (fun n z s => ai_ct_tally n s /\ annot0_ct_tally n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ct_tally (proc_start P_ct_tally) s1 (proc_end P_ct_tally) s2 ->
    (s2 V_ct_tally_z <= (30 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_ct_tally.
Qed.
