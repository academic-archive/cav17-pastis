Require Import pasta.Pasta.

Inductive proc: Type :=
  P_fill_inverse_cmap.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_fill_inverse_cmap_z := 1%positive.
Notation V_fill_inverse_cmap__tmp := 2%positive.
Notation V_fill_inverse_cmap__tmp1 := 3%positive.
Notation V_fill_inverse_cmap__tmp2 := 4%positive.
Notation V_fill_inverse_cmap_ic0 := 5%positive.
Notation V_fill_inverse_cmap_ic1 := 6%positive.
Notation V_fill_inverse_cmap_ic2 := 7%positive.
Notation V_fill_inverse_cmap_minc0 := 8%positive.
Notation V_fill_inverse_cmap_minc1 := 9%positive.
Notation V_fill_inverse_cmap_minc2 := 10%positive.
Notation V_fill_inverse_cmap_numcolors := 11%positive.
Notation V_fill_inverse_cmap_c0 := 12%positive.
Notation V_fill_inverse_cmap_c1 := 13%positive.
Notation V_fill_inverse_cmap_c2 := 14%positive.
Notation V_fill_inverse_cmap_cinfo := 15%positive.
Definition Pedges_fill_inverse_cmap: list (edge proc) :=
  (EA 1 (AAssign V_fill_inverse_cmap_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_fill_inverse_cmap__tmp2 (Some (EVar V_fill_inverse_cmap_c0))) 3)::
  (EA 3 (AAssign V_fill_inverse_cmap__tmp1
  (Some (EVar V_fill_inverse_cmap_c1))) 4)::(EA 4 (AAssign
  V_fill_inverse_cmap__tmp (Some (EVar V_fill_inverse_cmap_c2))) 5)::
  (EA 5 (AAssign V_fill_inverse_cmap__tmp2 None) 6)::(EA 6 (AAssign
  V_fill_inverse_cmap__tmp1 None) 7)::(EA 7 (AAssign V_fill_inverse_cmap__tmp
  None) 8)::(EA 8 (AAssign V_fill_inverse_cmap_minc0 None) 9)::(EA 9 (AAssign
  V_fill_inverse_cmap_minc1 None) 10)::(EA 10 (AAssign
  V_fill_inverse_cmap_minc2 None) 11)::(EA 11 (AAssign
  V_fill_inverse_cmap_numcolors None) 12)::(EA 12 (AAssign
  V_fill_inverse_cmap__tmp2 None) 13)::(EA 13 (AAssign
  V_fill_inverse_cmap__tmp1 None) 14)::(EA 14 (AAssign
  V_fill_inverse_cmap__tmp None) 15)::(EA 15 (AAssign V_fill_inverse_cmap_ic0
  (Some (ENum (0)))) 16)::(EA 16 ANone 17)::(EA 17 AWeaken 18)::
  (EA 18 (AGuard (fun s => ((eval (EVar V_fill_inverse_cmap_ic0) s) <
  (eval (ENum (4)) s))%Z)) 21)::(EA 18 (AGuard
  (fun s => ((eval (EVar V_fill_inverse_cmap_ic0) s) >= (eval (ENum (4))
  s))%Z)) 19)::(EA 19 AWeaken 20)::(EA 21 AWeaken 22)::(EA 22 (AAssign
  V_fill_inverse_cmap_ic1 (Some (ENum (0)))) 23)::(EA 23 ANone 24)::
  (EA 24 AWeaken 25)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_fill_inverse_cmap_ic1) s) < (eval (ENum (8))
  s))%Z)) 33)::(EA 25 (AGuard (fun s => ((eval (EVar V_fill_inverse_cmap_ic1)
  s) >= (eval (ENum (8)) s))%Z)) 26)::(EA 26 AWeaken 27)::(EA 27 ANone 28)::
  (EA 28 (AAssign V_fill_inverse_cmap_ic0
  (Some (EAdd (EVar V_fill_inverse_cmap_ic0) (ENum (1))))) 29)::
  (EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 (AAssign V_fill_inverse_cmap_z
  (Some (EAdd (ENum (1)) (EVar V_fill_inverse_cmap_z)))) 32)::
  (EA 32 AWeaken 18)::(EA 33 AWeaken 34)::(EA 34 (AAssign
  V_fill_inverse_cmap_ic2 (Some (ENum (0)))) 35)::(EA 35 ANone 36)::
  (EA 36 AWeaken 37)::(EA 37 (AGuard
  (fun s => ((eval (EVar V_fill_inverse_cmap_ic2) s) < (eval (ENum (4))
  s))%Z)) 45)::(EA 37 (AGuard (fun s => ((eval (EVar V_fill_inverse_cmap_ic2)
  s) >= (eval (ENum (4)) s))%Z)) 38)::(EA 38 AWeaken 39)::(EA 39 ANone 40)::
  (EA 40 (AAssign V_fill_inverse_cmap_ic1
  (Some (EAdd (EVar V_fill_inverse_cmap_ic1) (ENum (1))))) 41)::
  (EA 41 ANone 42)::(EA 42 ANone 43)::(EA 43 (AAssign V_fill_inverse_cmap_z
  (Some (EAdd (ENum (1)) (EVar V_fill_inverse_cmap_z)))) 44)::
  (EA 44 AWeaken 25)::(EA 45 AWeaken 46)::(EA 46 ANone 47)::(EA 47 (AAssign
  V_fill_inverse_cmap_ic2 (Some (EAdd (EVar V_fill_inverse_cmap_ic2)
  (ENum (1))))) 48)::(EA 48 ANone 49)::(EA 49 ANone 50)::(EA 50 (AAssign
  V_fill_inverse_cmap_z (Some (EAdd (ENum (1))
  (EVar V_fill_inverse_cmap_z)))) 51)::(EA 51 AWeaken 37)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_fill_inverse_cmap => Pedges_fill_inverse_cmap
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_fill_inverse_cmap => 20
     end)%positive;
  var_global := var_global
}.

Definition ai_fill_inverse_cmap (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 3 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 4 => (1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 5 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 6 => (1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 7 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 8 => (1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 9 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 10 => (1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 11 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 12 => (1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 13 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 14 => (1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 15 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 16 => (1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0)%Z
   | 17 => (-1 * s V_fill_inverse_cmap_ic0 <= 0 /\ 1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 18 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0)%Z
   | 19 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 + 4 <= 0)%Z
   | 20 => (-1 * s V_fill_inverse_cmap_ic0 + 4 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 21 => (-1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_ic0 + -3 <= 0)%Z
   | 22 => (1 * s V_fill_inverse_cmap_ic0 + -3 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0)%Z
   | 23 => (-1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_ic0 + -3 <= 0 /\ 1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0)%Z
   | 24 => (-1 * s V_fill_inverse_cmap_ic1 <= 0 /\ 1 * s V_fill_inverse_cmap_ic1 <= 0 /\ 1 * s V_fill_inverse_cmap_ic0 + -3 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0)%Z
   | 25 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0)%Z
   | 26 => (-1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 + 8 <= 0)%Z
   | 27 => (-1 * s V_fill_inverse_cmap_ic1 + 8 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0)%Z
   | 28 => (-1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 + 8 <= 0)%Z
   | 29 => (-1 * s V_fill_inverse_cmap_ic1 + 8 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 + 1 <= 0)%Z
   | 30 => (-1 * s V_fill_inverse_cmap_ic0 + 1 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 + 8 <= 0)%Z
   | 31 => (-1 * s V_fill_inverse_cmap_ic1 + 8 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 + 1 <= 0)%Z
   | 32 => (-1 * s V_fill_inverse_cmap_ic0 + 1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 + 8 <= 0 /\ -1 * s V_fill_inverse_cmap_z + 1 <= 0)%Z
   | 33 => (-1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_ic1 + -7 <= 0)%Z
   | 34 => (1 * s V_fill_inverse_cmap_ic1 + -7 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0)%Z
   | 35 => (-1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_ic1 + -7 <= 0 /\ 1 * s V_fill_inverse_cmap_ic2 <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 <= 0)%Z
   | 36 => (-1 * s V_fill_inverse_cmap_ic2 <= 0 /\ 1 * s V_fill_inverse_cmap_ic2 <= 0 /\ 1 * s V_fill_inverse_cmap_ic1 + -7 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0)%Z
   | 37 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0 /\ 1 * s V_fill_inverse_cmap_ic2 + -4 <= 0)%Z
   | 38 => (1 * s V_fill_inverse_cmap_ic2 + -4 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 + 4 <= 0)%Z
   | 39 => (-1 * s V_fill_inverse_cmap_ic2 + 4 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0 /\ 1 * s V_fill_inverse_cmap_ic2 + -4 <= 0)%Z
   | 40 => (1 * s V_fill_inverse_cmap_ic2 + -4 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 + 4 <= 0)%Z
   | 41 => (-1 * s V_fill_inverse_cmap_ic2 + 4 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0 /\ 1 * s V_fill_inverse_cmap_ic2 + -4 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 + 1 <= 0)%Z
   | 42 => (-1 * s V_fill_inverse_cmap_ic1 + 1 <= 0 /\ 1 * s V_fill_inverse_cmap_ic2 + -4 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 + 4 <= 0)%Z
   | 43 => (-1 * s V_fill_inverse_cmap_ic2 + 4 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0 /\ 1 * s V_fill_inverse_cmap_ic2 + -4 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 + 1 <= 0)%Z
   | 44 => (-1 * s V_fill_inverse_cmap_ic1 + 1 <= 0 /\ 1 * s V_fill_inverse_cmap_ic2 + -4 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 + 4 <= 0 /\ -1 * s V_fill_inverse_cmap_z + 1 <= 0)%Z
   | 45 => (-1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_ic2 + -3 <= 0)%Z
   | 46 => (1 * s V_fill_inverse_cmap_ic2 + -3 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0)%Z
   | 47 => (-1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0 /\ 1 * s V_fill_inverse_cmap_ic2 + -3 <= 0)%Z
   | 48 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 + 1 <= 0 /\ 1 * s V_fill_inverse_cmap_ic2 + -4 <= 0)%Z
   | 49 => (1 * s V_fill_inverse_cmap_ic2 + -4 <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 + 1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_z <= 0)%Z
   | 50 => (-1 * s V_fill_inverse_cmap_z <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 + 1 <= 0 /\ 1 * s V_fill_inverse_cmap_ic2 + -4 <= 0)%Z
   | 51 => (1 * s V_fill_inverse_cmap_ic2 + -4 <= 0 /\ -1 * s V_fill_inverse_cmap_ic2 + 1 <= 0 /\ -1 * s V_fill_inverse_cmap_ic0 <= 0 /\ -1 * s V_fill_inverse_cmap_ic1 <= 0 /\ -1 * s V_fill_inverse_cmap_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_fill_inverse_cmap (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((164 # 1) <= z)%Q
   | 2 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 3 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 4 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 5 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 6 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 7 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 8 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 9 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 10 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 11 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 12 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 13 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 14 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 15 => ((164 # 1) + s V_fill_inverse_cmap_z <= z)%Q
   | 16 => (s V_fill_inverse_cmap_z
            + (41 # 1) * max0(4 - s V_fill_inverse_cmap_ic0) <= z)%Q
   | 17 => (s V_fill_inverse_cmap_z
            + (41 # 1) * max0(4 - s V_fill_inverse_cmap_ic0) <= z)%Q
   | 18 => (s V_fill_inverse_cmap_z
            + (41 # 1) * max0(4 - s V_fill_inverse_cmap_ic0) <= z)%Q
   | 19 => hints
     [(*-41 0*) F_max0_monotonic (F_check_ge (4 - s V_fill_inverse_cmap_ic0) (3
                                                                    - s V_fill_inverse_cmap_ic0));
      (*-41 0*) F_max0_ge_0 (3 - s V_fill_inverse_cmap_ic0)]
     (s V_fill_inverse_cmap_z
      + (41 # 1) * max0(4 - s V_fill_inverse_cmap_ic0) <= z)%Q
   | 20 => (s V_fill_inverse_cmap_z <= z)%Q
   | 21 => (s V_fill_inverse_cmap_z
            + (41 # 1) * max0(4 - s V_fill_inverse_cmap_ic0) <= z)%Q
   | 22 => (s V_fill_inverse_cmap_z
            + (41 # 1) * max0(4 - s V_fill_inverse_cmap_ic0) <= z)%Q
   | 23 => (-(40 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(4 - s V_fill_inverse_cmap_ic0)
            + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 24 => hints
     [(*-41 0*) F_max0_pre_decrement 1 (4 - s V_fill_inverse_cmap_ic0) (1)]
     (-(40 # 1) + s V_fill_inverse_cmap_z
      + (41 # 1) * max0(4 - s V_fill_inverse_cmap_ic0)
      + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 25 => ((1 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 26 => ((1 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 27 => ((1 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 28 => ((1 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 29 => ((1 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(4 - s V_fill_inverse_cmap_ic0)
            + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 30 => ((1 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(4 - s V_fill_inverse_cmap_ic0)
            + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 31 => ((1 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(4 - s V_fill_inverse_cmap_ic0)
            + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 32 => hints
     [(*-5 0*) F_max0_monotonic (F_check_ge (8 - s V_fill_inverse_cmap_ic1) (7
                                                                    - s V_fill_inverse_cmap_ic1));
      (*-5 0*) F_max0_ge_0 (7 - s V_fill_inverse_cmap_ic1)]
     (s V_fill_inverse_cmap_z
      + (41 # 1) * max0(4 - s V_fill_inverse_cmap_ic0)
      + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 33 => hints
     [(*0 5*) F_max0_pre_decrement 1 (8 - s V_fill_inverse_cmap_ic1) (1)]
     ((1 # 1) + s V_fill_inverse_cmap_z
      + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
      + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 34 => ((6 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 35 => ((2 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 36 => ((2 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 37 => ((2 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 38 => ((2 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 39 => ((2 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 40 => ((2 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 41 => ((2 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 42 => ((2 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 43 => ((2 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 44 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (4
                                                 - s V_fill_inverse_cmap_ic2)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_fill_inverse_cmap_z
      + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
      + max0(4 - s V_fill_inverse_cmap_ic2)
      + (5 # 1) * max0(8 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 45 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (4 - s V_fill_inverse_cmap_ic2) (1)]
     ((2 # 1) + s V_fill_inverse_cmap_z
      + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
      + max0(4 - s V_fill_inverse_cmap_ic2)
      + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 46 => ((3 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(3 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 47 => ((3 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(3 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 48 => ((3 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 49 => ((3 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 50 => ((3 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | 51 => ((2 # 1) + s V_fill_inverse_cmap_z
            + (41 # 1) * max0(3 - s V_fill_inverse_cmap_ic0)
            + max0(4 - s V_fill_inverse_cmap_ic2)
            + (5 # 1) * max0(7 - s V_fill_inverse_cmap_ic1) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_fill_inverse_cmap =>
    [mkPA Q (fun n z s => ai_fill_inverse_cmap n s /\ annot0_fill_inverse_cmap n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_fill_inverse_cmap (proc_start P_fill_inverse_cmap) s1 (proc_end P_fill_inverse_cmap) s2 ->
    (s2 V_fill_inverse_cmap_z <= (164 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_fill_inverse_cmap.
Qed.
