Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cmd_compress_bitmap.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cmd_compress_bitmap_z := 1%positive.
Notation V_cmd_compress_bitmap__tmp := 2%positive.
Notation V_cmd_compress_bitmap__tmp1 := 3%positive.
Notation V_cmd_compress_bitmap__tmp2 := 4%positive.
Notation V_cmd_compress_bitmap_status := 5%positive.
Notation V_cmd_compress_bitmap_width_bytes := 6%positive.
Notation V_cmd_compress_bitmap_y := 7%positive.
Notation V_cmd_compress_bitmap_data := 8%positive.
Notation V_cmd_compress_bitmap_height := 9%positive.
Notation V_cmd_compress_bitmap_pw := 10%positive.
Notation V_cmd_compress_bitmap_raster := 11%positive.
Notation V_cmd_compress_bitmap_st := 12%positive.
Notation V_cmd_compress_bitmap_width_bits := 13%positive.
Definition Pedges_cmd_compress_bitmap: list (edge proc) :=
  (EA 1 (AAssign V_cmd_compress_bitmap_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_cmd_compress_bitmap_y) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_cmd_compress_bitmap__tmp1) s) >= (eval (ENum (0))
  s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign V_cmd_compress_bitmap__tmp2
  (Some (EVar V_cmd_compress_bitmap_width_bits))) 6)::(EA 6 (AAssign
  V_cmd_compress_bitmap__tmp (Some (EVar V_cmd_compress_bitmap_raster))) 7)::
  (EA 7 (AAssign V_cmd_compress_bitmap__tmp1
  (Some (EVar V_cmd_compress_bitmap_height))) 8)::(EA 8 (AAssign
  V_cmd_compress_bitmap_width_bytes None) 9)::(EA 9 (AAssign
  V_cmd_compress_bitmap_status (Some (ENum (0)))) 10)::(EA 10 AWeaken 11)::
  (EA 11 (AGuard (fun s => ((eval (EVar V_cmd_compress_bitmap__tmp) s) =
  (eval (EVar V_cmd_compress_bitmap_width_bytes) s))%Z)) 43)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_cmd_compress_bitmap__tmp) s) <>
  (eval (EVar V_cmd_compress_bitmap_width_bytes) s))%Z)) 12)::
  (EA 12 AWeaken 13)::(EA 13 (AAssign V_cmd_compress_bitmap_y
  (Some (ENum (1)))) 14)::(EA 14 ANone 15)::(EA 15 AWeaken 16)::
  (EA 16 (AGuard (fun s => ((eval (EVar V_cmd_compress_bitmap_y) s) <
  (eval (EVar V_cmd_compress_bitmap__tmp1) s))%Z)) 18)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_cmd_compress_bitmap_y) s) >=
  (eval (EVar V_cmd_compress_bitmap__tmp1) s))%Z)) 17)::(EA 17 AWeaken 36)::
  (EA 18 AWeaken 19)::(EA 19 (AAssign V_cmd_compress_bitmap_status
  None) 20)::(EA 20 AWeaken 21)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_cmd_compress_bitmap_status) s) <> (eval (ENum (0))
  s))%Z)) 33)::(EA 21 (AGuard
  (fun s => ((eval (EVar V_cmd_compress_bitmap_status) s) = (eval (ENum (0))
  s))%Z)) 22)::(EA 22 AWeaken 23)::(EA 23 ANone 30)::(EA 23 ANone 24)::
  (EA 24 ANone 25)::(EA 25 (AAssign V_cmd_compress_bitmap_y
  (Some (EAdd (EVar V_cmd_compress_bitmap_y) (ENum (1))))) 26)::
  (EA 26 ANone 27)::(EA 27 ANone 28)::(EA 28 (AAssign V_cmd_compress_bitmap_z
  (Some (EAdd (ENum (1)) (EVar V_cmd_compress_bitmap_z)))) 29)::
  (EA 29 AWeaken 16)::(EA 30 (AAssign V_cmd_compress_bitmap_status
  (Some (ENum (-1)))) 31)::(EA 31 ANone 32)::(EA 32 AWeaken 36)::
  (EA 33 AWeaken 34)::(EA 34 ANone 35)::(EA 35 AWeaken 36)::(EA 36 (AGuard
  (fun s => ((eval (EVar V_cmd_compress_bitmap_status) s) = (eval (ENum (0))
  s))%Z)) 38)::(EA 36 (AGuard
  (fun s => ((eval (EVar V_cmd_compress_bitmap_status) s) <> (eval (ENum (0))
  s))%Z)) 37)::(EA 37 AWeaken 41)::(EA 38 AWeaken 39)::(EA 39 (AAssign
  V_cmd_compress_bitmap_status None) 40)::(EA 40 ANone 41)::
  (EA 41 ANone 42)::(EA 42 AWeaken 47)::(EA 43 AWeaken 44)::(EA 44 (AAssign
  V_cmd_compress_bitmap_status None) 45)::(EA 45 ANone 46)::
  (EA 46 AWeaken 47)::(EA 47 ANone 49)::(EA 47 ANone 48)::
  (EA 48 AWeaken 51)::(EA 49 ANone 50)::(EA 50 AWeaken 51)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cmd_compress_bitmap => Pedges_cmd_compress_bitmap
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cmd_compress_bitmap => 51
     end)%positive;
  var_global := var_global
}.

Definition ai_cmd_compress_bitmap (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0)%Z
   | 3 => (-1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y <= 0)%Z
   | 4 => (-1 * s V_cmd_compress_bitmap_y <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1 <= 0)%Z
   | 5 => (-1 * s V_cmd_compress_bitmap__tmp1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y <= 0)%Z
   | 6 => (-1 * s V_cmd_compress_bitmap_y <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1 <= 0)%Z
   | 7 => (-1 * s V_cmd_compress_bitmap__tmp1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y <= 0)%Z
   | 8 => (-1 * s V_cmd_compress_bitmap_y <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0)%Z
   | 9 => (-1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y <= 0)%Z
   | 10 => (-1 * s V_cmd_compress_bitmap_y <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0)%Z
   | 11 => (-1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y <= 0)%Z
   | 12 => (-1 * s V_cmd_compress_bitmap_y <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0)%Z
   | 13 => (-1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y <= 0)%Z
   | 14 => (1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap_y + -1 <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 15 => (-1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ 1 * s V_cmd_compress_bitmap_y + -1 <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0)%Z
   | 16 => (-1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0)%Z
   | 17 => (-1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap__tmp1+ -1 * s V_cmd_compress_bitmap_y <= 0)%Z
   | 18 => (-1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 19 => (-1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0)%Z
   | 20 => (-1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 21 => (-1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 22 => (-1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0)%Z
   | 23 => (-1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 24 => (-1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0)%Z
   | 25 => (-1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 26 => (-1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 2 <= 0)%Z
   | 27 => (-1 * s V_cmd_compress_bitmap_y + 2 <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0)%Z
   | 28 => (-1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 2 <= 0)%Z
   | 29 => (-1 * s V_cmd_compress_bitmap_y + 2 <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_z + 1 <= 0)%Z
   | 30 => (-1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0)%Z
   | 31 => (-1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ 1 * s V_cmd_compress_bitmap_status + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_status + -1 <= 0)%Z
   | 32 => (-1 * s V_cmd_compress_bitmap_status + -1 <= 0 /\ 1 * s V_cmd_compress_bitmap_status + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 33 => (-1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 34 => (-1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 35 => (-1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp1+ 1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 36 => (-1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 37 => (-1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0)%Z
   | 38 => (-1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0)%Z
   | 39 => (-1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 40 => (-1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0)%Z
   | 41 => (-1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y + 1 <= 0)%Z
   | 42 => (-1 * s V_cmd_compress_bitmap_y + 1 <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0)%Z
   | 43 => (-1 * s V_cmd_compress_bitmap_y <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap__tmp+ -1 * s V_cmd_compress_bitmap_width_bytes <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp+ 1 * s V_cmd_compress_bitmap_width_bytes <= 0)%Z
   | 44 => (-1 * s V_cmd_compress_bitmap__tmp+ 1 * s V_cmd_compress_bitmap_width_bytes <= 0 /\ 1 * s V_cmd_compress_bitmap__tmp+ -1 * s V_cmd_compress_bitmap_width_bytes <= 0 /\ -1 * s V_cmd_compress_bitmap_status <= 0 /\ 1 * s V_cmd_compress_bitmap_status <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y <= 0)%Z
   | 45 => (-1 * s V_cmd_compress_bitmap_y <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap__tmp+ -1 * s V_cmd_compress_bitmap_width_bytes <= 0 /\ -1 * s V_cmd_compress_bitmap__tmp+ 1 * s V_cmd_compress_bitmap_width_bytes <= 0)%Z
   | 46 => (-1 * s V_cmd_compress_bitmap__tmp+ 1 * s V_cmd_compress_bitmap_width_bytes <= 0 /\ 1 * s V_cmd_compress_bitmap__tmp+ -1 * s V_cmd_compress_bitmap_width_bytes <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0 /\ 1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y <= 0)%Z
   | 47 => (-1 * s V_cmd_compress_bitmap_y <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0)%Z
   | 48 => (-1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y <= 0)%Z
   | 49 => (-1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y <= 0)%Z
   | 50 => (-1 * s V_cmd_compress_bitmap_y <= 0 /\ -1 * s V_cmd_compress_bitmap_z <= 0)%Z
   | 51 => (-1 * s V_cmd_compress_bitmap_z <= 0 /\ -1 * s V_cmd_compress_bitmap_y <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cmd_compress_bitmap (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(-1 + s V_cmd_compress_bitmap_height) <= z)%Q
   | 2 => (s V_cmd_compress_bitmap_z
           + max0(-1 + s V_cmd_compress_bitmap_height) <= z)%Q
   | 3 => (s V_cmd_compress_bitmap_z
           + max0(-1 + s V_cmd_compress_bitmap_height) <= z)%Q
   | 4 => (s V_cmd_compress_bitmap_z
           + max0(-1 + s V_cmd_compress_bitmap_height) <= z)%Q
   | 5 => (s V_cmd_compress_bitmap_z
           + max0(-1 + s V_cmd_compress_bitmap_height) <= z)%Q
   | 6 => (s V_cmd_compress_bitmap_z
           + max0(-1 + s V_cmd_compress_bitmap_height) <= z)%Q
   | 7 => (s V_cmd_compress_bitmap_z
           + max0(-1 + s V_cmd_compress_bitmap_height) <= z)%Q
   | 8 => (s V_cmd_compress_bitmap_z
           + max0(-1 + s V_cmd_compress_bitmap__tmp1) <= z)%Q
   | 9 => (s V_cmd_compress_bitmap_z
           + max0(-1 + s V_cmd_compress_bitmap__tmp1) <= z)%Q
   | 10 => (s V_cmd_compress_bitmap_z
            + max0(-1 + s V_cmd_compress_bitmap__tmp1) <= z)%Q
   | 11 => (s V_cmd_compress_bitmap_z
            + max0(-1 + s V_cmd_compress_bitmap__tmp1) <= z)%Q
   | 12 => (s V_cmd_compress_bitmap_z
            + max0(-1 + s V_cmd_compress_bitmap__tmp1) <= z)%Q
   | 13 => (s V_cmd_compress_bitmap_z
            + max0(-1 + s V_cmd_compress_bitmap__tmp1) <= z)%Q
   | 14 => (s V_cmd_compress_bitmap_z
            + max0(s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y) <= z)%Q
   | 15 => (s V_cmd_compress_bitmap_z
            + max0(s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y) <= z)%Q
   | 16 => (s V_cmd_compress_bitmap_z
            + max0(s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_cmd_compress_bitmap__tmp1
                                             - s V_cmd_compress_bitmap_y) (-1
                                                                    + s V_cmd_compress_bitmap__tmp1
                                                                    - s V_cmd_compress_bitmap_y));
      (*-1 0*) F_max0_ge_0 (-1 + s V_cmd_compress_bitmap__tmp1
                            - s V_cmd_compress_bitmap_y)]
     (s V_cmd_compress_bitmap_z
      + max0(s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y) <= z)%Q
   | 18 => (s V_cmd_compress_bitmap_z
            + max0(s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y) <= z)%Q
   | 19 => (s V_cmd_compress_bitmap_z
            + max0(s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y) <= z)%Q
   | 20 => (s V_cmd_compress_bitmap_z
            + max0(s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y) <= z)%Q
   | 21 => (s V_cmd_compress_bitmap_z
            + max0(s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y) <= z)%Q
   | 22 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cmd_compress_bitmap__tmp1
                                                   - s V_cmd_compress_bitmap_y)) (F_check_ge (s V_cmd_compress_bitmap__tmp1
                                                                    - s V_cmd_compress_bitmap_y) (0))]
     (s V_cmd_compress_bitmap_z
      + max0(s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y) <= z)%Q
   | 23 => (s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y
            + s V_cmd_compress_bitmap_z <= z)%Q
   | 24 => (s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y
            + s V_cmd_compress_bitmap_z <= z)%Q
   | 25 => (s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y
            + s V_cmd_compress_bitmap_z <= z)%Q
   | 26 => ((1 # 1) + s V_cmd_compress_bitmap__tmp1
            - s V_cmd_compress_bitmap_y + s V_cmd_compress_bitmap_z <= z)%Q
   | 27 => ((1 # 1) + s V_cmd_compress_bitmap__tmp1
            - s V_cmd_compress_bitmap_y + s V_cmd_compress_bitmap_z <= z)%Q
   | 28 => ((1 # 1) + s V_cmd_compress_bitmap__tmp1
            - s V_cmd_compress_bitmap_y + s V_cmd_compress_bitmap_z <= z)%Q
   | 29 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_cmd_compress_bitmap__tmp1
                                                               - s V_cmd_compress_bitmap_y) (0))) (F_max0_ge_0 (s V_cmd_compress_bitmap__tmp1
                                                                    - s V_cmd_compress_bitmap_y))]
     (s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y
      + s V_cmd_compress_bitmap_z <= z)%Q
   | 30 => (s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y
            + s V_cmd_compress_bitmap_z <= z)%Q
   | 31 => (s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y
            + s V_cmd_compress_bitmap_z <= z)%Q
   | 32 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_cmd_compress_bitmap__tmp1
                                       - s V_cmd_compress_bitmap_y) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_cmd_compress_bitmap__tmp1
                            - s V_cmd_compress_bitmap_y);
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_cmd_compress_bitmap__tmp1
                                                              - s V_cmd_compress_bitmap_y) (0))) (F_max0_ge_0 (s V_cmd_compress_bitmap__tmp1
                                                                    - s V_cmd_compress_bitmap_y))]
     (s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y
      + s V_cmd_compress_bitmap_z <= z)%Q
   | 33 => (s V_cmd_compress_bitmap_z
            + max0(s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y) <= z)%Q
   | 34 => (s V_cmd_compress_bitmap_z
            + max0(s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_cmd_compress_bitmap__tmp1
                                       - s V_cmd_compress_bitmap_y) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_cmd_compress_bitmap__tmp1
                            - s V_cmd_compress_bitmap_y)]
     (s V_cmd_compress_bitmap_z
      + max0(s V_cmd_compress_bitmap__tmp1 - s V_cmd_compress_bitmap_y) <= z)%Q
   | 36 => (s V_cmd_compress_bitmap_z <= z)%Q
   | 37 => (s V_cmd_compress_bitmap_z <= z)%Q
   | 38 => (s V_cmd_compress_bitmap_z <= z)%Q
   | 39 => (s V_cmd_compress_bitmap_z <= z)%Q
   | 40 => (s V_cmd_compress_bitmap_z <= z)%Q
   | 41 => (s V_cmd_compress_bitmap_z <= z)%Q
   | 42 => (s V_cmd_compress_bitmap_z <= z)%Q
   | 43 => (s V_cmd_compress_bitmap_z
            + max0(-1 + s V_cmd_compress_bitmap__tmp1) <= z)%Q
   | 44 => (s V_cmd_compress_bitmap_z
            + max0(-1 + s V_cmd_compress_bitmap__tmp1) <= z)%Q
   | 45 => (s V_cmd_compress_bitmap_z
            + max0(-1 + s V_cmd_compress_bitmap__tmp1) <= z)%Q
   | 46 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_cmd_compress_bitmap__tmp1)]
     (s V_cmd_compress_bitmap_z + max0(-1 + s V_cmd_compress_bitmap__tmp1) <= z)%Q
   | 47 => (s V_cmd_compress_bitmap_z <= z)%Q
   | 48 => (s V_cmd_compress_bitmap_z <= z)%Q
   | 49 => (s V_cmd_compress_bitmap_z <= z)%Q
   | 50 => (s V_cmd_compress_bitmap_z <= z)%Q
   | 51 => (s V_cmd_compress_bitmap_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cmd_compress_bitmap =>
    [mkPA Q (fun n z s => ai_cmd_compress_bitmap n s /\ annot0_cmd_compress_bitmap n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cmd_compress_bitmap (proc_start P_cmd_compress_bitmap) s1 (proc_end P_cmd_compress_bitmap) s2 ->
    (s2 V_cmd_compress_bitmap_z <= max0(-1 + s1 V_cmd_compress_bitmap_height))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cmd_compress_bitmap.
Qed.
