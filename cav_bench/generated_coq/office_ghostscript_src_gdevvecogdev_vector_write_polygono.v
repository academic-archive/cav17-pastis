Require Import pasta.Pasta.

Inductive proc: Type :=
  P_gdev_vector_write_polygon.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_gdev_vector_write_polygon_z := 1%positive.
Notation V_gdev_vector_write_polygon__tmp := 2%positive.
Notation V_gdev_vector_write_polygon__tmp1 := 3%positive.
Notation V_gdev_vector_write_polygon__tmp2 := 4%positive.
Notation V_gdev_vector_write_polygon__tmp3 := 5%positive.
Notation V_gdev_vector_write_polygon_code := 6%positive.
Notation V_gdev_vector_write_polygon_i := 7%positive.
Notation V_gdev_vector_write_polygon_close := 8%positive.
Notation V_gdev_vector_write_polygon_count := 9%positive.
Notation V_gdev_vector_write_polygon_points := 10%positive.
Notation V_gdev_vector_write_polygon_type := 11%positive.
Notation V_gdev_vector_write_polygon_vdev := 12%positive.
Definition Pedges_gdev_vector_write_polygon: list (edge proc) :=
  (EA 1 (AAssign V_gdev_vector_write_polygon_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard (fun s => ((eval (EVar V_gdev_vector_write_polygon_i) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon__tmp2) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_gdev_vector_write_polygon__tmp2
  (Some (EVar V_gdev_vector_write_polygon_count))) 6)::(EA 6 (AAssign
  V_gdev_vector_write_polygon__tmp3
  (Some (EVar V_gdev_vector_write_polygon_close))) 7)::(EA 7 (AAssign
  V_gdev_vector_write_polygon__tmp
  (Some (EVar V_gdev_vector_write_polygon_type))) 8)::(EA 8 (AAssign
  V_gdev_vector_write_polygon_code (Some (ENum (0)))) 9)::(EA 9 AWeaken 10)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_gdev_vector_write_polygon__tmp)
  s) <> (eval (ENum (0)) s))%Z)) 12)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon__tmp) s) =
  (eval (ENum (0)) s))%Z)) 11)::(EA 11 AWeaken 17)::(EA 12 AWeaken 13)::
  (EA 13 (AAssign V_gdev_vector_write_polygon_code None) 14)::
  (EA 14 AWeaken 15)::(EA 15 ANone 66)::(EA 15 ANone 16)::
  (EA 16 AWeaken 17)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon__tmp2) s) >
  (eval (ENum (0)) s))%Z)) 19)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon__tmp2) s) <=
  (eval (ENum (0)) s))%Z)) 18)::(EA 18 AWeaken 47)::(EA 19 AWeaken 20)::
  (EA 20 (AAssign V_gdev_vector_write_polygon_code None) 21)::
  (EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon_code) s) >=
  (eval (ENum (0)) s))%Z)) 24)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon_code) s) <
  (eval (ENum (0)) s))%Z)) 23)::(EA 23 AWeaken 37)::(EA 24 AWeaken 25)::
  (EA 25 (AAssign V_gdev_vector_write_polygon_i (Some (ENum (1)))) 26)::
  (EA 26 ANone 27)::(EA 27 AWeaken 28)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon_i) s) <
  (eval (EVar V_gdev_vector_write_polygon__tmp2) s))%Z)) 30)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon_i) s) >=
  (eval (EVar V_gdev_vector_write_polygon__tmp2) s))%Z)) 29)::
  (EA 29 AWeaken 35)::(EA 30 AWeaken 31)::(EA 31 ANone 32)::
  (EA 32 AWeaken 33)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon_code) s) >=
  (eval (ENum (0)) s))%Z)) 58)::(EA 33 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon_code) s) <
  (eval (ENum (0)) s))%Z)) 34)::(EA 34 AWeaken 35)::(EA 35 ANone 36)::
  (EA 36 AWeaken 37)::(EA 37 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon_code) s) >=
  (eval (ENum (0)) s))%Z)) 39)::(EA 37 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon_code) s) <
  (eval (ENum (0)) s))%Z)) 38)::(EA 38 AWeaken 45)::(EA 39 AWeaken 40)::
  (EA 40 (AGuard (fun s => ((eval (EVar V_gdev_vector_write_polygon__tmp3)
  s) <> (eval (ENum (0)) s))%Z)) 42)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon__tmp3) s) =
  (eval (ENum (0)) s))%Z)) 41)::(EA 41 AWeaken 45)::(EA 42 AWeaken 43)::
  (EA 43 (AAssign V_gdev_vector_write_polygon_code None) 44)::
  (EA 44 ANone 45)::(EA 45 ANone 46)::(EA 46 AWeaken 47)::(EA 47 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon_code) s) >=
  (eval (ENum (0)) s))%Z)) 49)::(EA 47 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon_code) s) <
  (eval (ENum (0)) s))%Z)) 48)::(EA 48 AWeaken 52)::(EA 49 AWeaken 50)::
  (EA 50 (AGuard (fun s => ((eval (EVar V_gdev_vector_write_polygon__tmp)
  s) <> (eval (ENum (0)) s))%Z)) 53)::(EA 50 (AGuard
  (fun s => ((eval (EVar V_gdev_vector_write_polygon__tmp) s) =
  (eval (ENum (0)) s))%Z)) 51)::(EA 51 AWeaken 52)::(EA 52 ANone 55)::
  (EA 53 AWeaken 54)::(EA 54 ANone 55)::(EA 55 (AAssign
  V_gdev_vector_write_polygon__tmp1 None) 56)::(EA 56 ANone 57)::
  (EA 57 AWeaken 69)::(EA 58 AWeaken 59)::(EA 59 (AAssign
  V_gdev_vector_write_polygon_code None) 60)::(EA 60 ANone 61)::
  (EA 61 (AAssign V_gdev_vector_write_polygon_i
  (Some (EAdd (EVar V_gdev_vector_write_polygon_i) (ENum (1))))) 62)::
  (EA 62 ANone 63)::(EA 63 ANone 64)::(EA 64 (AAssign
  V_gdev_vector_write_polygon_z (Some (EAdd (ENum (1))
  (EVar V_gdev_vector_write_polygon_z)))) 65)::(EA 65 AWeaken 28)::
  (EA 66 (AAssign V_gdev_vector_write_polygon__tmp1
  (Some (EVar V_gdev_vector_write_polygon_code))) 67)::(EA 67 ANone 68)::
  (EA 68 AWeaken 69)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_gdev_vector_write_polygon => Pedges_gdev_vector_write_polygon
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_gdev_vector_write_polygon => 69
     end)%positive;
  var_global := var_global
}.

Definition ai_gdev_vector_write_polygon (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 3 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0)%Z
   | 4 => (-1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 <= 0)%Z
   | 5 => (-1 * s V_gdev_vector_write_polygon__tmp2 <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0)%Z
   | 6 => (-1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 7 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0)%Z
   | 8 => (-1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 9 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_code <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0)%Z
   | 10 => (-1 * s V_gdev_vector_write_polygon_code <= 0 /\ 1 * s V_gdev_vector_write_polygon_code <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 11 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_code <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0 /\ 1 * s V_gdev_vector_write_polygon__tmp <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp <= 0)%Z
   | 12 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_code <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0)%Z
   | 13 => (-1 * s V_gdev_vector_write_polygon_code <= 0 /\ 1 * s V_gdev_vector_write_polygon_code <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 14 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0)%Z
   | 15 => (-1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 16 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0)%Z
   | 17 => (-1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 18 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon__tmp2 <= 0)%Z
   | 19 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0)%Z
   | 20 => (-1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 21 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0)%Z
   | 22 => (-1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 23 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ 1 * s V_gdev_vector_write_polygon_code + 1 <= 0)%Z
   | 24 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0)%Z
   | 25 => (-1 * s V_gdev_vector_write_polygon_code <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 26 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0 /\ 1 * s V_gdev_vector_write_polygon_i + -1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_i + 1 <= 0)%Z
   | 27 => (-1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ 1 * s V_gdev_vector_write_polygon_i + -1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 28 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i <= 0)%Z
   | 29 => (-1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon__tmp2+ -1 * s V_gdev_vector_write_polygon_i <= 0)%Z
   | 30 => (-1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i + 1 <= 0)%Z
   | 31 => (-1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i + 1 <= 0)%Z
   | 32 => (-1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i + 1 <= 0)%Z
   | 33 => (-1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i + 1 <= 0)%Z
   | 34 => (-1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ 1 * s V_gdev_vector_write_polygon_code + 1 <= 0)%Z
   | 35 => (-1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i + 1 <= 0)%Z
   | 36 => (-1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i <= 0)%Z
   | 37 => (-1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 38 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ 1 * s V_gdev_vector_write_polygon_code + 1 <= 0)%Z
   | 39 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0)%Z
   | 40 => (-1 * s V_gdev_vector_write_polygon_code <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 41 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0 /\ 1 * s V_gdev_vector_write_polygon__tmp3 <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp3 <= 0)%Z
   | 42 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0)%Z
   | 43 => (-1 * s V_gdev_vector_write_polygon_code <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 44 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0)%Z
   | 45 => (-1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 46 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2 + 1 <= 0)%Z
   | 47 => (-1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 48 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_code + 1 <= 0)%Z
   | 49 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0)%Z
   | 50 => (-1 * s V_gdev_vector_write_polygon_code <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 51 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0 /\ 1 * s V_gdev_vector_write_polygon__tmp <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp <= 0)%Z
   | 52 => (-1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 53 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0)%Z
   | 54 => (-1 * s V_gdev_vector_write_polygon_code <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 55 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0)%Z
   | 56 => (-1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 57 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0)%Z
   | 58 => (-1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_code <= 0)%Z
   | 59 => (-1 * s V_gdev_vector_write_polygon_code <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i + 1 <= 0)%Z
   | 60 => (-1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i + 1 <= 0)%Z
   | 61 => (-1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i + 1 <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i + 1 <= 0)%Z
   | 62 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_i + 2 <= 0)%Z
   | 63 => (-1 * s V_gdev_vector_write_polygon_i + 2 <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 64 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_i + 2 <= 0)%Z
   | 65 => (-1 * s V_gdev_vector_write_polygon_i + 2 <= 0 /\ -1 * s V_gdev_vector_write_polygon__tmp2+ 1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z + 1 <= 0)%Z
   | 66 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0)%Z
   | 67 => (-1 * s V_gdev_vector_write_polygon_i <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | 68 => (-1 * s V_gdev_vector_write_polygon_z <= 0 /\ 1 * s V_gdev_vector_write_polygon_z <= 0 /\ -1 * s V_gdev_vector_write_polygon_i <= 0)%Z
   | 69 => (-1 * s V_gdev_vector_write_polygon_i <= 0 /\ -1 * s V_gdev_vector_write_polygon_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_gdev_vector_write_polygon (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_gdev_vector_write_polygon_count) <= z)%Q
   | 2 => (s V_gdev_vector_write_polygon_z
           + max0(s V_gdev_vector_write_polygon_count) <= z)%Q
   | 3 => (s V_gdev_vector_write_polygon_z
           + max0(s V_gdev_vector_write_polygon_count) <= z)%Q
   | 4 => (s V_gdev_vector_write_polygon_z
           + max0(s V_gdev_vector_write_polygon_count) <= z)%Q
   | 5 => (s V_gdev_vector_write_polygon_z
           + max0(s V_gdev_vector_write_polygon_count) <= z)%Q
   | 6 => (s V_gdev_vector_write_polygon_z
           + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 7 => (s V_gdev_vector_write_polygon_z
           + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 8 => (s V_gdev_vector_write_polygon_z
           + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 9 => (s V_gdev_vector_write_polygon_z
           + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 10 => (s V_gdev_vector_write_polygon_z
            + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 11 => (s V_gdev_vector_write_polygon_z
            + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 12 => (s V_gdev_vector_write_polygon_z
            + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 13 => (s V_gdev_vector_write_polygon_z
            + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 14 => (s V_gdev_vector_write_polygon_z
            + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 15 => (s V_gdev_vector_write_polygon_z
            + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 16 => (s V_gdev_vector_write_polygon_z
            + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 17 => (s V_gdev_vector_write_polygon_z
            + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 18 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_gdev_vector_write_polygon__tmp2)) (F_check_ge (0) (0))]
     (s V_gdev_vector_write_polygon_z
      + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 19 => (s V_gdev_vector_write_polygon_z
            + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 20 => (s V_gdev_vector_write_polygon_z
            + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 21 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gdev_vector_write_polygon__tmp2)) (F_check_ge (s V_gdev_vector_write_polygon__tmp2) (0))]
     (s V_gdev_vector_write_polygon_z
      + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 22 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z <= z)%Q
   | 23 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_gdev_vector_write_polygon__tmp2)]
     (s V_gdev_vector_write_polygon__tmp2 + s V_gdev_vector_write_polygon_z <= z)%Q
   | 24 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z <= z)%Q
   | 25 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z <= z)%Q
   | 26 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2)
            + max0(s V_gdev_vector_write_polygon__tmp2
                   - s V_gdev_vector_write_polygon_i) <= z)%Q
   | 27 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2)
            + max0(s V_gdev_vector_write_polygon__tmp2
                   - s V_gdev_vector_write_polygon_i) <= z)%Q
   | 28 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2)
            + max0(s V_gdev_vector_write_polygon__tmp2
                   - s V_gdev_vector_write_polygon_i) <= z)%Q
   | 29 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_gdev_vector_write_polygon__tmp2
                                            - s V_gdev_vector_write_polygon_i) (-1
                                                                    + s V_gdev_vector_write_polygon__tmp2
                                                                    - s V_gdev_vector_write_polygon_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_gdev_vector_write_polygon__tmp2
                                                 - s V_gdev_vector_write_polygon_i)) (F_check_ge (0) (0))]
     (s V_gdev_vector_write_polygon__tmp2 + s V_gdev_vector_write_polygon_z
      - max0(-1 + s V_gdev_vector_write_polygon__tmp2)
      + max0(s V_gdev_vector_write_polygon__tmp2
             - s V_gdev_vector_write_polygon_i) <= z)%Q
   | 30 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2)
            + max0(s V_gdev_vector_write_polygon__tmp2
                   - s V_gdev_vector_write_polygon_i) <= z)%Q
   | 31 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2)
            + max0(s V_gdev_vector_write_polygon__tmp2
                   - s V_gdev_vector_write_polygon_i) <= z)%Q
   | 32 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2)
            + max0(s V_gdev_vector_write_polygon__tmp2
                   - s V_gdev_vector_write_polygon_i) <= z)%Q
   | 33 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2)
            + max0(s V_gdev_vector_write_polygon__tmp2
                   - s V_gdev_vector_write_polygon_i) <= z)%Q
   | 34 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_gdev_vector_write_polygon__tmp2
                                       - s V_gdev_vector_write_polygon_i) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_gdev_vector_write_polygon__tmp2
                            - s V_gdev_vector_write_polygon_i)]
     (s V_gdev_vector_write_polygon__tmp2 + s V_gdev_vector_write_polygon_z
      - max0(-1 + s V_gdev_vector_write_polygon__tmp2)
      + max0(s V_gdev_vector_write_polygon__tmp2
             - s V_gdev_vector_write_polygon_i) <= z)%Q
   | 35 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 36 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 37 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 38 => hints
     [(*-1 0*) F_one]
     (s V_gdev_vector_write_polygon__tmp2 + s V_gdev_vector_write_polygon_z
      - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 39 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 40 => (s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 41 => hints
     [(*-1 0*) F_one]
     (s V_gdev_vector_write_polygon__tmp2 + s V_gdev_vector_write_polygon_z
      - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 42 => hints
     [(*-1 0*) F_one]
     (s V_gdev_vector_write_polygon__tmp2 + s V_gdev_vector_write_polygon_z
      - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 43 => (-(1 # 1) + s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 44 => (-(1 # 1) + s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 45 => (-(1 # 1) + s V_gdev_vector_write_polygon__tmp2
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 46 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                              + s V_gdev_vector_write_polygon__tmp2) (0))) (F_max0_ge_0 (-1
                                                                    + s V_gdev_vector_write_polygon__tmp2))]
     (-(1 # 1) + s V_gdev_vector_write_polygon__tmp2
      + s V_gdev_vector_write_polygon_z
      - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 47 => (s V_gdev_vector_write_polygon_z <= z)%Q
   | 48 => (s V_gdev_vector_write_polygon_z <= z)%Q
   | 49 => (s V_gdev_vector_write_polygon_z <= z)%Q
   | 50 => (s V_gdev_vector_write_polygon_z <= z)%Q
   | 51 => (s V_gdev_vector_write_polygon_z <= z)%Q
   | 52 => (s V_gdev_vector_write_polygon_z <= z)%Q
   | 53 => (s V_gdev_vector_write_polygon_z <= z)%Q
   | 54 => (s V_gdev_vector_write_polygon_z <= z)%Q
   | 55 => (s V_gdev_vector_write_polygon_z <= z)%Q
   | 56 => (s V_gdev_vector_write_polygon_z <= z)%Q
   | 57 => (s V_gdev_vector_write_polygon_z <= z)%Q
   | 58 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_gdev_vector_write_polygon__tmp2
                                                   - s V_gdev_vector_write_polygon_i)) (F_check_ge (s V_gdev_vector_write_polygon__tmp2
                                                                    - s V_gdev_vector_write_polygon_i) (0))]
     (s V_gdev_vector_write_polygon__tmp2 + s V_gdev_vector_write_polygon_z
      - max0(-1 + s V_gdev_vector_write_polygon__tmp2)
      + max0(s V_gdev_vector_write_polygon__tmp2
             - s V_gdev_vector_write_polygon_i) <= z)%Q
   | 59 => ((2 # 1) * s V_gdev_vector_write_polygon__tmp2
            - s V_gdev_vector_write_polygon_i
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 60 => ((2 # 1) * s V_gdev_vector_write_polygon__tmp2
            - s V_gdev_vector_write_polygon_i
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 61 => ((2 # 1) * s V_gdev_vector_write_polygon__tmp2
            - s V_gdev_vector_write_polygon_i
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 62 => ((1 # 1) + (2 # 1) * s V_gdev_vector_write_polygon__tmp2
            - s V_gdev_vector_write_polygon_i
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 63 => ((1 # 1) + (2 # 1) * s V_gdev_vector_write_polygon__tmp2
            - s V_gdev_vector_write_polygon_i
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 64 => ((1 # 1) + (2 # 1) * s V_gdev_vector_write_polygon__tmp2
            - s V_gdev_vector_write_polygon_i
            + s V_gdev_vector_write_polygon_z
            - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 65 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_gdev_vector_write_polygon__tmp2
                                                               - s V_gdev_vector_write_polygon_i) (0))) (F_max0_ge_0 (s V_gdev_vector_write_polygon__tmp2
                                                                    - s V_gdev_vector_write_polygon_i))]
     ((2 # 1) * s V_gdev_vector_write_polygon__tmp2
      - s V_gdev_vector_write_polygon_i + s V_gdev_vector_write_polygon_z
      - max0(-1 + s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 66 => (s V_gdev_vector_write_polygon_z
            + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 67 => (s V_gdev_vector_write_polygon_z
            + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 68 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_gdev_vector_write_polygon__tmp2)) (F_check_ge (0) (0))]
     (s V_gdev_vector_write_polygon_z
      + max0(s V_gdev_vector_write_polygon__tmp2) <= z)%Q
   | 69 => (s V_gdev_vector_write_polygon_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_gdev_vector_write_polygon =>
    [mkPA Q (fun n z s => ai_gdev_vector_write_polygon n s /\ annot0_gdev_vector_write_polygon n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_gdev_vector_write_polygon (proc_start P_gdev_vector_write_polygon) s1 (proc_end P_gdev_vector_write_polygon) s2 ->
    (s2 V_gdev_vector_write_polygon_z <= max0(s1 V_gdev_vector_write_polygon_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_gdev_vector_write_polygon.
Qed.
