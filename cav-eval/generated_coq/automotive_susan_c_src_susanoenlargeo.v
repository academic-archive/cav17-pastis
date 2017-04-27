Require Import pasta.Pasta.

Inductive proc: Type :=
  P_enlarge.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_enlarge_z := 1%positive.
Notation V_enlarge__tmp := 2%positive.
Notation V_enlarge_i := 3%positive.
Notation V_enlarge_j := 4%positive.
Notation V_enlarge_x_size_dref := 5%positive.
Notation V_enlarge_y_size_dref := 6%positive.
Notation V_enlarge_border := 7%positive.
Notation V_enlarge_in := 8%positive.
Notation V_enlarge_tmp_image := 9%positive.
Notation V_enlarge_x_size := 10%positive.
Notation V_enlarge_y_size := 11%positive.
Definition Pedges_enlarge: list (edge proc) :=
  (EA 1 (AAssign V_enlarge_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_enlarge__tmp (Some (EVar V_enlarge_border))) 3)::(EA 3 (AAssign
  V_enlarge_i (Some (ENum (0)))) 4)::(EA 4 ANone 5)::(EA 5 AWeaken 6)::
  (EA 6 (AGuard (fun s => ((eval (EVar V_enlarge_i) s) <
  (eval (EVar V_enlarge_y_size_dref) s))%Z)) 48)::(EA 6 (AGuard
  (fun s => ((eval (EVar V_enlarge_i) s) >=
  (eval (EVar V_enlarge_y_size_dref) s))%Z)) 7)::(EA 7 AWeaken 8)::
  (EA 8 (AAssign V_enlarge_i (Some (ENum (0)))) 9)::(EA 9 ANone 10)::
  (EA 10 AWeaken 11)::(EA 11 (AGuard (fun s => ((eval (EVar V_enlarge_i) s) <
  (eval (EVar V_enlarge__tmp) s))%Z)) 41)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_enlarge_i) s) >= (eval (EVar V_enlarge__tmp)
  s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 (AAssign V_enlarge_i
  (Some (ENum (0)))) 14)::(EA 14 ANone 15)::(EA 15 AWeaken 16)::
  (EA 16 (AGuard (fun s => ((eval (EVar V_enlarge_i) s) <
  (eval (EVar V_enlarge__tmp) s))%Z)) 22)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_enlarge_i) s) >= (eval (EVar V_enlarge__tmp)
  s))%Z)) 17)::(EA 17 AWeaken 18)::(EA 18 (AAssign V_enlarge_x_size_dref
  (Some (EAdd (EVar V_enlarge_x_size_dref) (EMul (ENum (2))
  (EVar V_enlarge__tmp))))) 19)::(EA 19 (AAssign V_enlarge_y_size_dref
  (Some (EAdd (EVar V_enlarge_y_size_dref) (EMul (ENum (2))
  (EVar V_enlarge__tmp))))) 20)::(EA 20 AWeaken 21)::(EA 22 AWeaken 23)::
  (EA 23 (AAssign V_enlarge_j (Some (ENum (0)))) 24)::(EA 24 ANone 25)::
  (EA 25 AWeaken 26)::(EA 26 (AGuard (fun s => ((eval (EVar V_enlarge_j) s) <
  (eval (EAdd (EVar V_enlarge_y_size_dref) (EMul (ENum (2))
  (EVar V_enlarge__tmp))) s))%Z)) 34)::(EA 26 (AGuard
  (fun s => ((eval (EVar V_enlarge_j) s) >=
  (eval (EAdd (EVar V_enlarge_y_size_dref) (EMul (ENum (2))
  (EVar V_enlarge__tmp))) s))%Z)) 27)::(EA 27 AWeaken 28)::(EA 28 ANone 29)::
  (EA 29 (AAssign V_enlarge_i (Some (EAdd (EVar V_enlarge_i)
  (ENum (1))))) 30)::(EA 30 ANone 31)::(EA 31 ANone 32)::(EA 32 (AAssign
  V_enlarge_z (Some (EAdd (ENum (1)) (EVar V_enlarge_z)))) 33)::
  (EA 33 AWeaken 16)::(EA 34 AWeaken 35)::(EA 35 ANone 36)::(EA 36 (AAssign
  V_enlarge_j (Some (EAdd (EVar V_enlarge_j) (ENum (1))))) 37)::
  (EA 37 ANone 38)::(EA 38 ANone 39)::(EA 39 (AAssign V_enlarge_z
  (Some (EAdd (ENum (1)) (EVar V_enlarge_z)))) 40)::(EA 40 AWeaken 26)::
  (EA 41 AWeaken 42)::(EA 42 ANone 43)::(EA 43 (AAssign V_enlarge_i
  (Some (EAdd (EVar V_enlarge_i) (ENum (1))))) 44)::(EA 44 ANone 45)::
  (EA 45 ANone 46)::(EA 46 (AAssign V_enlarge_z (Some (EAdd (ENum (1))
  (EVar V_enlarge_z)))) 47)::(EA 47 AWeaken 11)::(EA 48 AWeaken 49)::
  (EA 49 ANone 50)::(EA 50 (AAssign V_enlarge_i
  (Some (EAdd (EVar V_enlarge_i) (ENum (1))))) 51)::(EA 51 ANone 52)::
  (EA 52 ANone 53)::(EA 53 (AAssign V_enlarge_z (Some (EAdd (ENum (1))
  (EVar V_enlarge_z)))) 54)::(EA 54 AWeaken 6)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_enlarge => Pedges_enlarge
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_enlarge => 21
     end)%positive;
  var_global := var_global
}.

Definition ai_enlarge (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_z <= 0)%Z
   | 3 => (-1 * s V_enlarge_z <= 0 /\ 1 * s V_enlarge_z <= 0)%Z
   | 4 => (1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_z <= 0 /\ 1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 5 => (-1 * s V_enlarge_i <= 0 /\ 1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ 1 * s V_enlarge_z <= 0)%Z
   | 6 => (-1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 7 => (-1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i+ 1 * s V_enlarge_y_size_dref <= 0)%Z
   | 8 => (-1 * s V_enlarge_i+ 1 * s V_enlarge_y_size_dref <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 9 => (-1 * s V_enlarge_z <= 0 /\ 1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 10 => (-1 * s V_enlarge_i <= 0 /\ 1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0)%Z
   | 11 => (-1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 12 => (-1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ 1 * s V_enlarge__tmp+ -1 * s V_enlarge_i <= 0)%Z
   | 13 => (1 * s V_enlarge__tmp+ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 14 => (-1 * s V_enlarge_z <= 0 /\ 1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 15 => (-1 * s V_enlarge_i <= 0 /\ 1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0)%Z
   | 16 => (-1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 17 => (-1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ 1 * s V_enlarge__tmp+ -1 * s V_enlarge_i <= 0)%Z
   | 18 => (1 * s V_enlarge__tmp+ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 19 => (-1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ 1 * s V_enlarge__tmp+ -1 * s V_enlarge_i <= 0)%Z
   | 20 => (1 * s V_enlarge__tmp+ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 21 => (-1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ 1 * s V_enlarge__tmp+ -1 * s V_enlarge_i <= 0)%Z
   | 22 => (-1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0)%Z
   | 23 => (-1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 24 => (-1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0 /\ 1 * s V_enlarge_j <= 0 /\ -1 * s V_enlarge_j <= 0)%Z
   | 25 => (-1 * s V_enlarge_j <= 0 /\ 1 * s V_enlarge_j <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 26 => (-1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_j <= 0 /\ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0)%Z
   | 27 => (-1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_j <= 0 /\ -1 * s V_enlarge_z <= 0 /\ 2 * s V_enlarge__tmp+ -1 * s V_enlarge_j+ 1 * s V_enlarge_y_size_dref <= 0)%Z
   | 28 => (2 * s V_enlarge__tmp+ -1 * s V_enlarge_j+ 1 * s V_enlarge_y_size_dref <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_j <= 0 /\ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0)%Z
   | 29 => (-1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_j <= 0 /\ -1 * s V_enlarge_z <= 0 /\ 2 * s V_enlarge__tmp+ -1 * s V_enlarge_j+ 1 * s V_enlarge_y_size_dref <= 0)%Z
   | 30 => (2 * s V_enlarge__tmp+ -1 * s V_enlarge_j+ 1 * s V_enlarge_y_size_dref <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_j <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_i + 1 <= 0)%Z
   | 31 => (-1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_j <= 0 /\ -1 * s V_enlarge_z <= 0 /\ 2 * s V_enlarge__tmp+ -1 * s V_enlarge_j+ 1 * s V_enlarge_y_size_dref <= 0)%Z
   | 32 => (2 * s V_enlarge__tmp+ -1 * s V_enlarge_j+ 1 * s V_enlarge_y_size_dref <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_j <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_i + 1 <= 0)%Z
   | 33 => (-1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_j <= 0 /\ 2 * s V_enlarge__tmp+ -1 * s V_enlarge_j+ 1 * s V_enlarge_y_size_dref <= 0 /\ -1 * s V_enlarge_z + 1 <= 0)%Z
   | 34 => (-1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_j <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -2 * s V_enlarge__tmp+ 1 * s V_enlarge_j+ -1 * s V_enlarge_y_size_dref + 1 <= 0)%Z
   | 35 => (-2 * s V_enlarge__tmp+ 1 * s V_enlarge_j+ -1 * s V_enlarge_y_size_dref + 1 <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_j <= 0 /\ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0)%Z
   | 36 => (-1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_j <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -2 * s V_enlarge__tmp+ 1 * s V_enlarge_j+ -1 * s V_enlarge_y_size_dref + 1 <= 0)%Z
   | 37 => (-1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_j + 1 <= 0 /\ -2 * s V_enlarge__tmp+ 1 * s V_enlarge_j+ -1 * s V_enlarge_y_size_dref <= 0)%Z
   | 38 => (-2 * s V_enlarge__tmp+ 1 * s V_enlarge_j+ -1 * s V_enlarge_y_size_dref <= 0 /\ -1 * s V_enlarge_j + 1 <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0)%Z
   | 39 => (-1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_j + 1 <= 0 /\ -2 * s V_enlarge__tmp+ 1 * s V_enlarge_j+ -1 * s V_enlarge_y_size_dref <= 0)%Z
   | 40 => (-2 * s V_enlarge__tmp+ 1 * s V_enlarge_j+ -1 * s V_enlarge_y_size_dref <= 0 /\ -1 * s V_enlarge_j + 1 <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z + 1 <= 0)%Z
   | 41 => (-1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0)%Z
   | 42 => (-1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 43 => (-1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i + 1 <= 0)%Z
   | 44 => (-1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i <= 0)%Z
   | 45 => (-1 * s V_enlarge__tmp+ 1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_z <= 0)%Z
   | 46 => (-1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge__tmp+ 1 * s V_enlarge_i <= 0)%Z
   | 47 => (-1 * s V_enlarge__tmp+ 1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_z + 1 <= 0)%Z
   | 48 => (-1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ 1 * s V_enlarge_i+ -1 * s V_enlarge_y_size_dref + 1 <= 0)%Z
   | 49 => (1 * s V_enlarge_i+ -1 * s V_enlarge_y_size_dref + 1 <= 0 /\ -1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i <= 0)%Z
   | 50 => (-1 * s V_enlarge_i <= 0 /\ -1 * s V_enlarge_z <= 0 /\ 1 * s V_enlarge_i+ -1 * s V_enlarge_y_size_dref + 1 <= 0)%Z
   | 51 => (-1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i + 1 <= 0 /\ 1 * s V_enlarge_i+ -1 * s V_enlarge_y_size_dref <= 0)%Z
   | 52 => (1 * s V_enlarge_i+ -1 * s V_enlarge_y_size_dref <= 0 /\ -1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_z <= 0)%Z
   | 53 => (-1 * s V_enlarge_z <= 0 /\ -1 * s V_enlarge_i + 1 <= 0 /\ 1 * s V_enlarge_i+ -1 * s V_enlarge_y_size_dref <= 0)%Z
   | 54 => (1 * s V_enlarge_i+ -1 * s V_enlarge_y_size_dref <= 0 /\ -1 * s V_enlarge_i + 1 <= 0 /\ -1 * s V_enlarge_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_enlarge (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((2 # 1) * max0(s V_enlarge_border)
           + max0(s V_enlarge_border) * max0(2 * s V_enlarge_border
                                             + s V_enlarge_y_size_dref)
           + max0(s V_enlarge_y_size_dref) <= z)%Q
   | 2 => (s V_enlarge_z + (2 # 1) * max0(s V_enlarge_border)
           + max0(s V_enlarge_border) * max0(2 * s V_enlarge_border
                                             + s V_enlarge_y_size_dref)
           + max0(s V_enlarge_y_size_dref) <= z)%Q
   | 3 => (s V_enlarge_z + (2 # 1) * max0(s V_enlarge__tmp)
           + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                           + s V_enlarge_y_size_dref)
           + max0(s V_enlarge_y_size_dref) <= z)%Q
   | 4 => (s V_enlarge_z + (2 # 1) * max0(s V_enlarge__tmp)
           + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                           + s V_enlarge_y_size_dref)
           + max0(-s V_enlarge_i + s V_enlarge_y_size_dref) <= z)%Q
   | 5 => (s V_enlarge_z + (2 # 1) * max0(s V_enlarge__tmp)
           + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                           + s V_enlarge_y_size_dref)
           + max0(-s V_enlarge_i + s V_enlarge_y_size_dref) <= z)%Q
   | 6 => (s V_enlarge_z + (2 # 1) * max0(s V_enlarge__tmp)
           + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                           + s V_enlarge_y_size_dref)
           + max0(-s V_enlarge_i + s V_enlarge_y_size_dref) <= z)%Q
   | 7 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_enlarge_i
                                             + s V_enlarge_y_size_dref) (-1
                                                                    - s V_enlarge_i
                                                                    + s V_enlarge_y_size_dref));
      (*-1 0*) F_max0_ge_0 (-1 - s V_enlarge_i + s V_enlarge_y_size_dref)]
     (s V_enlarge_z + (2 # 1) * max0(s V_enlarge__tmp)
      + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                      + s V_enlarge_y_size_dref)
      + max0(-s V_enlarge_i + s V_enlarge_y_size_dref) <= z)%Q
   | 8 => (s V_enlarge_z + (2 # 1) * max0(s V_enlarge__tmp)
           + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                           + s V_enlarge_y_size_dref) <= z)%Q
   | 9 => (s V_enlarge_z + max0(s V_enlarge__tmp)
           + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                           + s V_enlarge_y_size_dref)
           + max0(s V_enlarge__tmp - s V_enlarge_i) <= z)%Q
   | 10 => (s V_enlarge_z + max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i) <= z)%Q
   | 11 => (s V_enlarge_z + max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i) <= z)%Q
   | 12 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_enlarge__tmp - s V_enlarge_i) (-1
                                                                    + s V_enlarge__tmp
                                                                    - s V_enlarge_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_enlarge__tmp - s V_enlarge_i)]
     (s V_enlarge_z + max0(s V_enlarge__tmp)
      + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                      + s V_enlarge_y_size_dref)
      + max0(s V_enlarge__tmp - s V_enlarge_i) <= z)%Q
   | 13 => (s V_enlarge_z + max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref) <= z)%Q
   | 14 => (s V_enlarge_z + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                            + s V_enlarge_y_size_dref) <= z)%Q
   | 15 => (s V_enlarge_z + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                            + s V_enlarge_y_size_dref) <= z)%Q
   | 16 => (s V_enlarge_z + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                            + s V_enlarge_y_size_dref) <= z)%Q
   | 17 => (s V_enlarge_z + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                            + s V_enlarge_y_size_dref) <= z)%Q
   | 18 => (s V_enlarge_z + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                            + s V_enlarge_y_size_dref) <= z)%Q
   | 19 => (s V_enlarge_z + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                            + s V_enlarge_y_size_dref) <= z)%Q
   | 20 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_enlarge__tmp - s V_enlarge_i) (-1
                                                                    + s V_enlarge__tmp
                                                                    - s V_enlarge_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_enlarge__tmp - s V_enlarge_i);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (s V_enlarge__tmp
                                                            - s V_enlarge_i)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_enlarge_y_size_dref)) (F_check_ge (0) (0)))]
     (s V_enlarge_z + max0(s V_enlarge__tmp - s V_enlarge_i)
      + max0(s V_enlarge__tmp - s V_enlarge_i) * max0(s V_enlarge_y_size_dref) <= z)%Q
   | 21 => (s V_enlarge_z <= z)%Q
   | 22 => hints
     [(*0 1*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_enlarge__tmp
                                                                    - s V_enlarge_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_enlarge__tmp
                                                                    - s V_enlarge_i))) (F_binom_monotonic 1 (F_max0_ge_0 (2 * s V_enlarge__tmp
                                                                    + s V_enlarge_y_size_dref)) (F_check_ge (0) (0)));
      (*0 1*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_enlarge__tmp
                                                             - s V_enlarge_i)) (F_check_ge (s V_enlarge__tmp
                                                                    - s V_enlarge_i) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (2 * s V_enlarge__tmp
                                                                    + s V_enlarge_y_size_dref)) (F_check_ge (0) (0)))]
     (s V_enlarge_z + max0(s V_enlarge__tmp - s V_enlarge_i)
      + max0(s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                      + s V_enlarge_y_size_dref) <= z)%Q
   | 23 => (s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                                 + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(2 * s V_enlarge__tmp + s V_enlarge_y_size_dref) <= z)%Q
   | 24 => (s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                                 + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref) <= z)%Q
   | 25 => (s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                                 + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref) <= z)%Q
   | 26 => (s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                                 + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref) <= z)%Q
   | 27 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_enlarge__tmp - s V_enlarge_i) (1)]
     (s V_enlarge_z
      + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                           + s V_enlarge_y_size_dref)
      + max0(s V_enlarge__tmp - s V_enlarge_i)
      + max0(2 * s V_enlarge__tmp - s V_enlarge_j + s V_enlarge_y_size_dref) <= z)%Q
   | 28 => ((1 # 1) + s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i)
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                                 + s V_enlarge_y_size_dref)
            + max0(2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref) <= z)%Q
   | 29 => ((1 # 1) + s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i)
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                                 + s V_enlarge_y_size_dref)
            + max0(2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref) <= z)%Q
   | 30 => ((1 # 1) + s V_enlarge_z + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                            + s V_enlarge_y_size_dref)
            + max0(2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref) <= z)%Q
   | 31 => ((1 # 1) + s V_enlarge_z + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                            + s V_enlarge_y_size_dref)
            + max0(2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref) <= z)%Q
   | 32 => ((1 # 1) + s V_enlarge_z + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                            + s V_enlarge_y_size_dref)
            + max0(2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref) <= z)%Q
   | 33 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (2 * s V_enlarge__tmp
                                                 - s V_enlarge_j
                                                 + s V_enlarge_y_size_dref)) (F_check_ge (0) (0))]
     (s V_enlarge_z + max0(s V_enlarge__tmp - s V_enlarge_i)
      + max0(s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                      + s V_enlarge_y_size_dref)
      + max0(2 * s V_enlarge__tmp - s V_enlarge_j + s V_enlarge_y_size_dref) <= z)%Q
   | 34 => hints
     [(*0 1*) F_max0_pre_decrement 1 (2 * s V_enlarge__tmp - s V_enlarge_j
                                      + s V_enlarge_y_size_dref) (1)]
     (s V_enlarge_z
      + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                           + s V_enlarge_y_size_dref)
      + max0(s V_enlarge__tmp - s V_enlarge_i)
      + max0(2 * s V_enlarge__tmp - s V_enlarge_j + s V_enlarge_y_size_dref) <= z)%Q
   | 35 => ((1 # 1) + s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                                 + s V_enlarge_y_size_dref)
            + max0(-1 + 2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i) <= z)%Q
   | 36 => ((1 # 1) + s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                                 + s V_enlarge_y_size_dref)
            + max0(-1 + 2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i) <= z)%Q
   | 37 => ((1 # 1) + s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                                 + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref) <= z)%Q
   | 38 => ((1 # 1) + s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                                 + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref) <= z)%Q
   | 39 => ((1 # 1) + s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                                 + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref) <= z)%Q
   | 40 => (s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i) * max0(2 * s V_enlarge__tmp
                                                                 + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i)
            + max0(2 * s V_enlarge__tmp - s V_enlarge_j
                   + s V_enlarge_y_size_dref) <= z)%Q
   | 41 => hints
     [(*0 1*) F_max0_pre_decrement 1 (s V_enlarge__tmp - s V_enlarge_i) (1)]
     (s V_enlarge_z + max0(s V_enlarge__tmp)
      + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                      + s V_enlarge_y_size_dref)
      + max0(s V_enlarge__tmp - s V_enlarge_i) <= z)%Q
   | 42 => ((1 # 1) + s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i)
            + max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref) <= z)%Q
   | 43 => ((1 # 1) + s V_enlarge_z
            + max0(-1 + s V_enlarge__tmp - s V_enlarge_i)
            + max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref) <= z)%Q
   | 44 => ((1 # 1) + s V_enlarge_z + max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i) <= z)%Q
   | 45 => ((1 # 1) + s V_enlarge_z + max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i) <= z)%Q
   | 46 => ((1 # 1) + s V_enlarge_z + max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i) <= z)%Q
   | 47 => (s V_enlarge_z + max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref)
            + max0(s V_enlarge__tmp - s V_enlarge_i) <= z)%Q
   | 48 => hints
     [(*0 1*) F_max0_pre_decrement 1 (-s V_enlarge_i
                                      + s V_enlarge_y_size_dref) (1)]
     (s V_enlarge_z + (2 # 1) * max0(s V_enlarge__tmp)
      + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                      + s V_enlarge_y_size_dref)
      + max0(-s V_enlarge_i + s V_enlarge_y_size_dref) <= z)%Q
   | 49 => ((1 # 1) + s V_enlarge_z
            + max0(-1 - s V_enlarge_i + s V_enlarge_y_size_dref)
            + (2 # 1) * max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref) <= z)%Q
   | 50 => ((1 # 1) + s V_enlarge_z
            + max0(-1 - s V_enlarge_i + s V_enlarge_y_size_dref)
            + (2 # 1) * max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref) <= z)%Q
   | 51 => ((1 # 1) + s V_enlarge_z + (2 # 1) * max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref)
            + max0(-s V_enlarge_i + s V_enlarge_y_size_dref) <= z)%Q
   | 52 => ((1 # 1) + s V_enlarge_z + (2 # 1) * max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref)
            + max0(-s V_enlarge_i + s V_enlarge_y_size_dref) <= z)%Q
   | 53 => ((1 # 1) + s V_enlarge_z + (2 # 1) * max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref)
            + max0(-s V_enlarge_i + s V_enlarge_y_size_dref) <= z)%Q
   | 54 => (s V_enlarge_z + (2 # 1) * max0(s V_enlarge__tmp)
            + max0(s V_enlarge__tmp) * max0(2 * s V_enlarge__tmp
                                            + s V_enlarge_y_size_dref)
            + max0(-s V_enlarge_i + s V_enlarge_y_size_dref) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_enlarge =>
    [mkPA Q (fun n z s => ai_enlarge n s /\ annot0_enlarge n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_enlarge (proc_start P_enlarge) s1 (proc_end P_enlarge) s2 ->
    (s2 V_enlarge_z <= (2 # 1) * max0(s1 V_enlarge_border)
                       + max0(s1 V_enlarge_border) * max0(2 * s1 V_enlarge_border
                                                          + s1 V_enlarge_y_size_dref)
                       + max0(s1 V_enlarge_y_size_dref))%Q.
Proof.
  prove_bound ipa admissible_ipa P_enlarge.
Qed.
