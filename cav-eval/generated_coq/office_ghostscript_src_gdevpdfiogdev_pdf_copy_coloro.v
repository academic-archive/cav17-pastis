Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gdev_pdf_copy_color.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gdev_pdf_copy_color_z := 1%positive.
Notation V_gdev_pdf_copy_color__tmp := 2%positive.
Notation V_gdev_pdf_copy_color__tmp1 := 3%positive.
Notation V_gdev_pdf_copy_color__tmp2 := 4%positive.
Notation V_gdev_pdf_copy_color__tmp3 := 5%positive.
Notation V_gdev_pdf_copy_color__tmp4 := 6%positive.
Notation V_gdev_pdf_copy_color__tmp5 := 7%positive.
Notation V_gdev_pdf_copy_color__tmp6 := 8%positive.
Notation V_gdev_pdf_copy_color__tmp7 := 9%positive.
Notation V_gdev_pdf_copy_color_bytes_per_pixel := 10%positive.
Notation V_gdev_pdf_copy_color_code := 11%positive.
Notation V_gdev_pdf_copy_color_depth := 12%positive.
Notation V_gdev_pdf_copy_color_nbytes := 13%positive.
Notation V_gdev_pdf_copy_color_yi := 14%positive.
Notation V_gdev_pdf_copy_color_base := 15%positive.
Notation V_gdev_pdf_copy_color_dev := 16%positive.
Notation V_gdev_pdf_copy_color_h := 17%positive.
Notation V_gdev_pdf_copy_color_id := 18%positive.
Notation V_gdev_pdf_copy_color_raster := 19%positive.
Notation V_gdev_pdf_copy_color_sourcex := 20%positive.
Notation V_gdev_pdf_copy_color_w := 21%positive.
Notation V_gdev_pdf_copy_color_x := 22%positive.
Notation V_gdev_pdf_copy_color_y := 23%positive.
Definition Pedges_gdev_pdf_copy_color: list (edge proc) :=
  (EA 1 (AAssign V_gdev_pdf_copy_color_z (Some (ENum (0)))) 2)::
  (EA 2 (AAssign V_gdev_pdf_copy_color__tmp7
  (Some (EVar V_gdev_pdf_copy_color_sourcex))) 3)::(EA 3 (AAssign
  V_gdev_pdf_copy_color__tmp6
  (Some (EVar V_gdev_pdf_copy_color_raster))) 4)::(EA 4 (AAssign
  V_gdev_pdf_copy_color__tmp5 (Some (EVar V_gdev_pdf_copy_color_id))) 5)::
  (EA 5 (AAssign V_gdev_pdf_copy_color__tmp4
  (Some (EVar V_gdev_pdf_copy_color_x))) 6)::(EA 6 (AAssign
  V_gdev_pdf_copy_color__tmp3 (Some (EVar V_gdev_pdf_copy_color_y))) 7)::
  (EA 7 (AAssign V_gdev_pdf_copy_color__tmp1
  (Some (EVar V_gdev_pdf_copy_color_w))) 8)::(EA 8 (AAssign
  V_gdev_pdf_copy_color__tmp2 (Some (EVar V_gdev_pdf_copy_color_h))) 9)::
  (EA 9 (AAssign V_gdev_pdf_copy_color_depth None) 10)::(EA 10 (AAssign
  V_gdev_pdf_copy_color_bytes_per_pixel None) 11)::(EA 11 (AAssign
  V_gdev_pdf_copy_color_code None) 12)::(EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color_code) s) < (eval (ENum (0))
  s))%Z)) 71)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color_code) s) >= (eval (ENum (0))
  s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color__tmp1) s) <= (eval (ENum (0))
  s))%Z)) 67)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color__tmp1) s) > (eval (ENum (0))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color__tmp2) s) <= (eval (ENum (0))
  s))%Z)) 66)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color__tmp2) s) > (eval (ENum (0))
  s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color_bytes_per_pixel) s) =
  (eval (ENum (3)) s))%Z)) 22)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color_bytes_per_pixel) s) <>
  (eval (ENum (3)) s))%Z)) 20)::(EA 20 AWeaken 21)::(EA 21 ANone 24)::
  (EA 22 AWeaken 23)::(EA 23 ANone 24)::(EA 24 (AAssign
  V_gdev_pdf_copy_color_nbytes
  (Some (EMul (EMul (EVar V_gdev_pdf_copy_color__tmp1)
  (EVar V_gdev_pdf_copy_color_bytes_per_pixel))
  (EVar V_gdev_pdf_copy_color__tmp2)))) 25)::(EA 25 (AAssign
  V_gdev_pdf_copy_color_code None) 26)::(EA 26 AWeaken 27)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color_code) s) < (eval (ENum (0))
  s))%Z)) 62)::(EA 27 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color_code) s) >= (eval (ENum (0))
  s))%Z)) 28)::(EA 28 AWeaken 29)::(EA 29 (AAssign V_gdev_pdf_copy_color_code
  None) 30)::(EA 30 AWeaken 31)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color_code) s) < (eval (ENum (0))
  s))%Z)) 58)::(EA 31 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color_code) s) >= (eval (ENum (0))
  s))%Z)) 32)::(EA 32 AWeaken 33)::(EA 33 (AAssign V_gdev_pdf_copy_color_yi
  (Some (ENum (0)))) 34)::(EA 34 ANone 35)::(EA 35 AWeaken 36)::
  (EA 36 (AGuard (fun s => ((eval (EVar V_gdev_pdf_copy_color_yi) s) <
  (eval (EVar V_gdev_pdf_copy_color__tmp2) s))%Z)) 51)::(EA 36 (AGuard
  (fun s => ((eval (EVar V_gdev_pdf_copy_color_yi) s) >=
  (eval (EVar V_gdev_pdf_copy_color__tmp2) s))%Z)) 37)::(EA 37 AWeaken 38)::
  (EA 38 (AAssign V_gdev_pdf_copy_color_code None) 39)::(EA 39 AWeaken 40)::
  (EA 40 ANone 48)::(EA 40 ANone 45)::(EA 40 ANone 41)::(EA 41 ANone 42)::
  (EA 42 (AAssign V_gdev_pdf_copy_color__tmp None) 43)::(EA 43 ANone 44)::
  (EA 44 AWeaken 75)::(EA 45 (AAssign V_gdev_pdf_copy_color__tmp
  (Some (ENum (0)))) 46)::(EA 46 ANone 47)::(EA 47 AWeaken 75)::
  (EA 48 (AAssign V_gdev_pdf_copy_color__tmp
  (Some (EVar V_gdev_pdf_copy_color_code))) 49)::(EA 49 ANone 50)::
  (EA 50 AWeaken 75)::(EA 51 AWeaken 52)::(EA 52 ANone 53)::(EA 53 (AAssign
  V_gdev_pdf_copy_color_yi (Some (EAdd (EVar V_gdev_pdf_copy_color_yi)
  (ENum (1))))) 54)::(EA 54 ANone 55)::(EA 55 ANone 56)::(EA 56 (AAssign
  V_gdev_pdf_copy_color_z (Some (EAdd (ENum (1))
  (EVar V_gdev_pdf_copy_color_z)))) 57)::(EA 57 AWeaken 36)::
  (EA 58 AWeaken 59)::(EA 59 (AAssign V_gdev_pdf_copy_color__tmp
  (Some (EVar V_gdev_pdf_copy_color_code))) 60)::(EA 60 ANone 61)::
  (EA 61 AWeaken 75)::(EA 62 AWeaken 63)::(EA 63 (AAssign
  V_gdev_pdf_copy_color__tmp (Some (EVar V_gdev_pdf_copy_color_code))) 64)::
  (EA 64 ANone 65)::(EA 65 AWeaken 75)::(EA 66 AWeaken 68)::
  (EA 67 AWeaken 68)::(EA 68 (AAssign V_gdev_pdf_copy_color__tmp
  (Some (ENum (0)))) 69)::(EA 69 ANone 70)::(EA 70 AWeaken 75)::
  (EA 71 AWeaken 72)::(EA 72 (AAssign V_gdev_pdf_copy_color__tmp
  (Some (EVar V_gdev_pdf_copy_color_code))) 73)::(EA 73 ANone 74)::
  (EA 74 AWeaken 75)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gdev_pdf_copy_color => Pedges_gdev_pdf_copy_color
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gdev_pdf_copy_color => 75
     end)%positive;
  var_global := var_global
}.

Definition ai_gdev_pdf_copy_color (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 3 => (-1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 4 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 5 => (-1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 6 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 7 => (-1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 8 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 9 => (-1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 10 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 11 => (-1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 12 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 13 => (-1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 14 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0)%Z
   | 15 => (-1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 16 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0)%Z
   | 17 => (-1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 18 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0)%Z
   | 19 => (-1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 20 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0)%Z
   | 21 => (-1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 22 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ 1 * s V_gdev_pdf_copy_color_bytes_per_pixel + -3 <= 0 /\ -1 * s V_gdev_pdf_copy_color_bytes_per_pixel + 3 <= 0)%Z
   | 23 => (-1 * s V_gdev_pdf_copy_color_bytes_per_pixel + 3 <= 0 /\ 1 * s V_gdev_pdf_copy_color_bytes_per_pixel + -3 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 24 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0)%Z
   | 25 => (-1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 26 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0)%Z
   | 27 => (-1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 28 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0)%Z
   | 29 => (-1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 30 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0)%Z
   | 31 => (-1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 32 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0)%Z
   | 33 => (-1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 34 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 35 => (-1 * s V_gdev_pdf_copy_color_yi <= 0 /\ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 36 => (-1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 37 => (-1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 38 => (1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 39 => (-1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 40 => (1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 41 => (-1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 42 => (1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 43 => (-1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 44 => (1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 45 => (-1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 46 => (1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp <= 0)%Z
   | 47 => (-1 * s V_gdev_pdf_copy_color__tmp <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 48 => (-1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 49 => (1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 50 => (-1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp2+ -1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 51 => (-1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi + 1 <= 0)%Z
   | 52 => (-1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0)%Z
   | 53 => (-1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi + 1 <= 0)%Z
   | 54 => (-1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color_yi + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 55 => (-1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color_yi + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 56 => (-1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color_yi + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0)%Z
   | 57 => (-1 * s V_gdev_pdf_copy_color__tmp2+ 1 * s V_gdev_pdf_copy_color_yi <= 0 /\ -1 * s V_gdev_pdf_copy_color_yi + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z + 1 <= 0)%Z
   | 58 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ 1 * s V_gdev_pdf_copy_color_code + 1 <= 0)%Z
   | 59 => (1 * s V_gdev_pdf_copy_color_code + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 60 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ 1 * s V_gdev_pdf_copy_color_code + 1 <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp + 1 <= 0)%Z
   | 61 => (1 * s V_gdev_pdf_copy_color__tmp + 1 <= 0 /\ 1 * s V_gdev_pdf_copy_color_code + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 62 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ 1 * s V_gdev_pdf_copy_color_code + 1 <= 0)%Z
   | 63 => (1 * s V_gdev_pdf_copy_color_code + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 64 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ 1 * s V_gdev_pdf_copy_color_code + 1 <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp + 1 <= 0)%Z
   | 65 => (1 * s V_gdev_pdf_copy_color__tmp + 1 <= 0 /\ 1 * s V_gdev_pdf_copy_color_code + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp2 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 66 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp1 + 1 <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp2 <= 0)%Z
   | 67 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp1 <= 0)%Z
   | 68 => (-1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 69 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp <= 0 /\ -1 * s V_gdev_pdf_copy_color__tmp <= 0)%Z
   | 70 => (-1 * s V_gdev_pdf_copy_color__tmp <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp <= 0 /\ -1 * s V_gdev_pdf_copy_color_code <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 71 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_code + 1 <= 0)%Z
   | 72 => (1 * s V_gdev_pdf_copy_color_code + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 73 => (1 * s V_gdev_pdf_copy_color_z <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_code + 1 <= 0 /\ 1 * s V_gdev_pdf_copy_color__tmp + 1 <= 0)%Z
   | 74 => (1 * s V_gdev_pdf_copy_color__tmp + 1 <= 0 /\ 1 * s V_gdev_pdf_copy_color_code + 1 <= 0 /\ -1 * s V_gdev_pdf_copy_color_z <= 0 /\ 1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | 75 => (-1 * s V_gdev_pdf_copy_color_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gdev_pdf_copy_color (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_gdev_pdf_copy_color_h) <= z)%Q
   | 2 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color_h) <= z)%Q
   | 3 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color_h) <= z)%Q
   | 4 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color_h) <= z)%Q
   | 5 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color_h) <= z)%Q
   | 6 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color_h) <= z)%Q
   | 7 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color_h) <= z)%Q
   | 8 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color_h) <= z)%Q
   | 9 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 10 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 11 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 12 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 13 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 14 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gdev_pdf_copy_color_z) (0))) (F_max0_ge_0 (s V_gdev_pdf_copy_color_z))]
     (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 15 => (max0(s V_gdev_pdf_copy_color__tmp2)
            + max0(s V_gdev_pdf_copy_color_z) <= z)%Q
   | 16 => (max0(s V_gdev_pdf_copy_color__tmp2)
            + max0(s V_gdev_pdf_copy_color_z) <= z)%Q
   | 17 => (max0(s V_gdev_pdf_copy_color__tmp2)
            + max0(s V_gdev_pdf_copy_color_z) <= z)%Q
   | 18 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (s V_gdev_pdf_copy_color_z)) (F_check_ge (0) (0))]
     (max0(s V_gdev_pdf_copy_color__tmp2) + max0(s V_gdev_pdf_copy_color_z) <= z)%Q
   | 19 => (max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 20 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gdev_pdf_copy_color__tmp2)) (F_check_ge (s V_gdev_pdf_copy_color__tmp2) (0))]
     (max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 21 => (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 22 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gdev_pdf_copy_color__tmp2)) (F_check_ge (s V_gdev_pdf_copy_color__tmp2) (0))]
     (max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 23 => (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 24 => (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 25 => (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 26 => (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 27 => (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 28 => (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 29 => (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 30 => (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 31 => (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 32 => (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 33 => (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 34 => (s V_gdev_pdf_copy_color__tmp2
            - max0(s V_gdev_pdf_copy_color__tmp2)
            + max0(s V_gdev_pdf_copy_color__tmp2 - s V_gdev_pdf_copy_color_yi) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_gdev_pdf_copy_color_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_gdev_pdf_copy_color_z) (0))) (F_max0_ge_0 (-
                                                                    s V_gdev_pdf_copy_color_z))]
     (s V_gdev_pdf_copy_color__tmp2 - max0(s V_gdev_pdf_copy_color__tmp2)
      + max0(s V_gdev_pdf_copy_color__tmp2 - s V_gdev_pdf_copy_color_yi) <= z)%Q
   | 36 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            - max0(s V_gdev_pdf_copy_color__tmp2)
            + max0(s V_gdev_pdf_copy_color__tmp2 - s V_gdev_pdf_copy_color_yi) <= z)%Q
   | 37 => hints
     [(*-1 0*) F_max0_ge_0 (s V_gdev_pdf_copy_color__tmp2
                            - s V_gdev_pdf_copy_color_yi)]
     (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
      - max0(s V_gdev_pdf_copy_color__tmp2)
      + max0(s V_gdev_pdf_copy_color__tmp2 - s V_gdev_pdf_copy_color_yi) <= z)%Q
   | 38 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 39 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 40 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 41 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 42 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 43 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 44 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gdev_pdf_copy_color__tmp2) (0))) (F_max0_ge_0 (s V_gdev_pdf_copy_color__tmp2))]
     (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
      - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 45 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 46 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gdev_pdf_copy_color__tmp2) (0))) (F_max0_ge_0 (s V_gdev_pdf_copy_color__tmp2))]
     (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
      - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 48 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 49 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 50 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gdev_pdf_copy_color__tmp2) (0))) (F_max0_ge_0 (s V_gdev_pdf_copy_color__tmp2))]
     (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
      - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 51 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_gdev_pdf_copy_color__tmp2
                                      - s V_gdev_pdf_copy_color_yi) (1)]
     (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
      - max0(s V_gdev_pdf_copy_color__tmp2)
      + max0(s V_gdev_pdf_copy_color__tmp2 - s V_gdev_pdf_copy_color_yi) <= z)%Q
   | 52 => ((1 # 1) + s V_gdev_pdf_copy_color__tmp2
            + s V_gdev_pdf_copy_color_z
            + max0(-1 + s V_gdev_pdf_copy_color__tmp2
                   - s V_gdev_pdf_copy_color_yi)
            - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 53 => ((1 # 1) + s V_gdev_pdf_copy_color__tmp2
            + s V_gdev_pdf_copy_color_z
            + max0(-1 + s V_gdev_pdf_copy_color__tmp2
                   - s V_gdev_pdf_copy_color_yi)
            - max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 54 => ((1 # 1) + s V_gdev_pdf_copy_color__tmp2
            + s V_gdev_pdf_copy_color_z - max0(s V_gdev_pdf_copy_color__tmp2)
            + max0(s V_gdev_pdf_copy_color__tmp2 - s V_gdev_pdf_copy_color_yi) <= z)%Q
   | 55 => ((1 # 1) + s V_gdev_pdf_copy_color__tmp2
            + s V_gdev_pdf_copy_color_z - max0(s V_gdev_pdf_copy_color__tmp2)
            + max0(s V_gdev_pdf_copy_color__tmp2 - s V_gdev_pdf_copy_color_yi) <= z)%Q
   | 56 => ((1 # 1) + s V_gdev_pdf_copy_color__tmp2
            + s V_gdev_pdf_copy_color_z - max0(s V_gdev_pdf_copy_color__tmp2)
            + max0(s V_gdev_pdf_copy_color__tmp2 - s V_gdev_pdf_copy_color_yi) <= z)%Q
   | 57 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            - max0(s V_gdev_pdf_copy_color__tmp2)
            + max0(s V_gdev_pdf_copy_color__tmp2 - s V_gdev_pdf_copy_color_yi) <= z)%Q
   | 58 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_gdev_pdf_copy_color_z) (0))) (F_max0_ge_0 (-
                                                                    s V_gdev_pdf_copy_color_z))]
     (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 59 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            + max0(-s V_gdev_pdf_copy_color_z) <= z)%Q
   | 60 => (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
            + max0(-s V_gdev_pdf_copy_color_z) <= z)%Q
   | 61 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_gdev_pdf_copy_color_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_gdev_pdf_copy_color__tmp2)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_gdev_pdf_copy_color__tmp2) (0))) (F_max0_ge_0 (-1
                                                                    + s V_gdev_pdf_copy_color__tmp2))]
     (s V_gdev_pdf_copy_color__tmp2 + s V_gdev_pdf_copy_color_z
      + max0(-s V_gdev_pdf_copy_color_z) <= z)%Q
   | 62 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              + s V_gdev_pdf_copy_color__tmp2) (0))) (F_max0_ge_0 (-1
                                                                    + s V_gdev_pdf_copy_color__tmp2))]
     (s V_gdev_pdf_copy_color__tmp2 <= z)%Q
   | 63 => ((1 # 1) + max0(-1 + s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 64 => ((1 # 1) + max0(-1 + s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 65 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_gdev_pdf_copy_color_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_gdev_pdf_copy_color_z) (0))) (F_max0_ge_0 (-
                                                                    s V_gdev_pdf_copy_color_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_gdev_pdf_copy_color__tmp2)) (F_check_ge (0) (0))]
     ((1 # 1) + max0(-1 + s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 66 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_gdev_pdf_copy_color__tmp2)) (F_check_ge (0) (0))]
     (max0(s V_gdev_pdf_copy_color__tmp2) + max0(s V_gdev_pdf_copy_color_z) <= z)%Q
   | 67 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_gdev_pdf_copy_color__tmp2)) (F_check_ge (0) (0))]
     (max0(s V_gdev_pdf_copy_color__tmp2) + max0(s V_gdev_pdf_copy_color_z) <= z)%Q
   | 68 => (max0(s V_gdev_pdf_copy_color_z) <= z)%Q
   | 69 => (max0(s V_gdev_pdf_copy_color_z) <= z)%Q
   | 70 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gdev_pdf_copy_color_z)) (F_check_ge (s V_gdev_pdf_copy_color_z) (0))]
     (max0(s V_gdev_pdf_copy_color_z) <= z)%Q
   | 71 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 72 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 73 => (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 74 => hints
     [(*-1 0*) F_max0_ge_0 (s V_gdev_pdf_copy_color__tmp2)]
     (s V_gdev_pdf_copy_color_z + max0(s V_gdev_pdf_copy_color__tmp2) <= z)%Q
   | 75 => (s V_gdev_pdf_copy_color_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gdev_pdf_copy_color =>
    [mkPA Q (fun n z s => ai_gdev_pdf_copy_color n s /\ annot0_gdev_pdf_copy_color n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gdev_pdf_copy_color (proc_start P_gdev_pdf_copy_color) s1 (proc_end P_gdev_pdf_copy_color) s2 ->
    (s2 V_gdev_pdf_copy_color_z <= max0(s1 V_gdev_pdf_copy_color_h))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gdev_pdf_copy_color.
Qed.
