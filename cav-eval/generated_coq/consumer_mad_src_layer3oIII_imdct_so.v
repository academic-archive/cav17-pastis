Require Import pasta.Pasta.

Inductive proc: Type :=
  P_III_imdct_s.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_III_imdct_s_z := 1%positive.
Notation V_III_imdct_s_i := 2%positive.
Notation V_III_imdct_s_lo := 3%positive.
Notation V_III_imdct_s_w := 4%positive.
Notation V_III_imdct_s_X := 5%positive.
Notation V_III_imdct_s_z := 6%positive.
Definition Pedges_III_imdct_s: list (edge proc) :=
  (EA 1 (AAssign V_III_imdct_s_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_III_imdct_s_w (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_III_imdct_s_w) s) < (eval (ENum (3))
  s))%Z)) 24)::(EA 5 (AGuard (fun s => ((eval (EVar V_III_imdct_s_w) s) >=
  (eval (ENum (3)) s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 (AAssign
  V_III_imdct_s_i (Some (ENum (0)))) 8)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::
  (EA 10 (AGuard (fun s => ((eval (EVar V_III_imdct_s_i) s) <
  (eval (ENum (6)) s))%Z)) 13)::(EA 10 (AGuard
  (fun s => ((eval (EVar V_III_imdct_s_i) s) >= (eval (ENum (6))
  s))%Z)) 11)::(EA 11 AWeaken 12)::(EA 13 AWeaken 14)::(EA 14 (AAssign
  V_III_imdct_s_lo None) 15)::(EA 15 (AAssign V_III_imdct_s_lo None) 16)::
  (EA 16 (AAssign V_III_imdct_s_lo None) 17)::(EA 17 (AAssign
  V_III_imdct_s_lo None) 18)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_III_imdct_s_i (Some (EAdd (EVar V_III_imdct_s_i) (ENum (1))))) 20)::
  (EA 20 ANone 21)::(EA 21 ANone 22)::(EA 22 (AAssign V_III_imdct_s_z
  (Some (EAdd (ENum (1)) (EVar V_III_imdct_s_z)))) 23)::(EA 23 AWeaken 10)::
  (EA 24 AWeaken 25)::(EA 25 (AAssign V_III_imdct_s_i
  (Some (ENum (0)))) 26)::(EA 26 ANone 27)::(EA 27 AWeaken 28)::
  (EA 28 (AGuard (fun s => ((eval (EVar V_III_imdct_s_i) s) <
  (eval (ENum (3)) s))%Z)) 36)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_III_imdct_s_i) s) >= (eval (ENum (3))
  s))%Z)) 29)::(EA 29 AWeaken 30)::(EA 30 ANone 31)::(EA 31 (AAssign
  V_III_imdct_s_w (Some (EAdd (EVar V_III_imdct_s_w) (ENum (1))))) 32)::
  (EA 32 ANone 33)::(EA 33 ANone 34)::(EA 34 (AAssign V_III_imdct_s_z
  (Some (EAdd (ENum (1)) (EVar V_III_imdct_s_z)))) 35)::(EA 35 AWeaken 5)::
  (EA 36 AWeaken 37)::(EA 37 (AAssign V_III_imdct_s_lo None) 38)::
  (EA 38 (AAssign V_III_imdct_s_lo None) 39)::(EA 39 (AAssign
  V_III_imdct_s_lo None) 40)::(EA 40 (AAssign V_III_imdct_s_lo None) 41)::
  (EA 41 (AAssign V_III_imdct_s_lo None) 42)::(EA 42 (AAssign
  V_III_imdct_s_lo None) 43)::(EA 43 (AAssign V_III_imdct_s_lo None) 44)::
  (EA 44 (AAssign V_III_imdct_s_lo None) 45)::(EA 45 (AAssign
  V_III_imdct_s_lo None) 46)::(EA 46 (AAssign V_III_imdct_s_lo None) 47)::
  (EA 47 (AAssign V_III_imdct_s_lo None) 48)::(EA 48 (AAssign
  V_III_imdct_s_lo None) 49)::(EA 49 ANone 50)::(EA 50 (AAssign
  V_III_imdct_s_i (Some (EAdd (EVar V_III_imdct_s_i) (ENum (1))))) 51)::
  (EA 51 ANone 52)::(EA 52 ANone 53)::(EA 53 (AAssign V_III_imdct_s_z
  (Some (EAdd (ENum (1)) (EVar V_III_imdct_s_z)))) 54)::(EA 54 AWeaken 28)::
  nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_III_imdct_s => Pedges_III_imdct_s
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_III_imdct_s => 12
     end)%positive;
  var_global := var_global
}.

Definition ai_III_imdct_s (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_z <= 0)%Z
   | 3 => (-1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_w <= 0)%Z
   | 4 => (-1 * s V_III_imdct_s_w <= 0 /\ 1 * s V_III_imdct_s_w <= 0 /\ 1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_z <= 0)%Z
   | 5 => (-1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_w <= 0)%Z
   | 6 => (-1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0)%Z
   | 7 => (-1 * s V_III_imdct_s_w + 3 <= 0 /\ -1 * s V_III_imdct_s_z <= 0)%Z
   | 8 => (-1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0 /\ 1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_i <= 0)%Z
   | 9 => (-1 * s V_III_imdct_s_i <= 0 /\ 1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0 /\ -1 * s V_III_imdct_s_z <= 0)%Z
   | 10 => (-1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0 /\ 1 * s V_III_imdct_s_i + -6 <= 0)%Z
   | 11 => (1 * s V_III_imdct_s_i + -6 <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i + 6 <= 0)%Z
   | 12 => (-1 * s V_III_imdct_s_i + 6 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0 /\ 1 * s V_III_imdct_s_i + -6 <= 0)%Z
   | 13 => (-1 * s V_III_imdct_s_w + 3 <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -5 <= 0)%Z
   | 14 => (1 * s V_III_imdct_s_i + -5 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0)%Z
   | 15 => (-1 * s V_III_imdct_s_w + 3 <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -5 <= 0)%Z
   | 16 => (1 * s V_III_imdct_s_i + -5 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0)%Z
   | 17 => (-1 * s V_III_imdct_s_w + 3 <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -5 <= 0)%Z
   | 18 => (1 * s V_III_imdct_s_i + -5 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0)%Z
   | 19 => (-1 * s V_III_imdct_s_w + 3 <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -5 <= 0)%Z
   | 20 => (-1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0 /\ -1 * s V_III_imdct_s_i + 1 <= 0 /\ 1 * s V_III_imdct_s_i + -6 <= 0)%Z
   | 21 => (1 * s V_III_imdct_s_i + -6 <= 0 /\ -1 * s V_III_imdct_s_i + 1 <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0 /\ -1 * s V_III_imdct_s_z <= 0)%Z
   | 22 => (-1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0 /\ -1 * s V_III_imdct_s_i + 1 <= 0 /\ 1 * s V_III_imdct_s_i + -6 <= 0)%Z
   | 23 => (1 * s V_III_imdct_s_i + -6 <= 0 /\ -1 * s V_III_imdct_s_i + 1 <= 0 /\ -1 * s V_III_imdct_s_w + 3 <= 0 /\ -1 * s V_III_imdct_s_z + 1 <= 0)%Z
   | 24 => (-1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_w + -2 <= 0)%Z
   | 25 => (1 * s V_III_imdct_s_w + -2 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_w <= 0)%Z
   | 26 => (-1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_w + -2 <= 0 /\ 1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_i <= 0)%Z
   | 27 => (-1 * s V_III_imdct_s_i <= 0 /\ 1 * s V_III_imdct_s_i <= 0 /\ 1 * s V_III_imdct_s_w + -2 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_w <= 0)%Z
   | 28 => (-1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w <= 0 /\ 1 * s V_III_imdct_s_i + -3 <= 0)%Z
   | 29 => (1 * s V_III_imdct_s_i + -3 <= 0 /\ -1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i + 3 <= 0)%Z
   | 30 => (-1 * s V_III_imdct_s_i + 3 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_w <= 0 /\ 1 * s V_III_imdct_s_i + -3 <= 0)%Z
   | 31 => (1 * s V_III_imdct_s_i + -3 <= 0 /\ -1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i + 3 <= 0)%Z
   | 32 => (-1 * s V_III_imdct_s_i + 3 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -3 <= 0 /\ -1 * s V_III_imdct_s_w + 1 <= 0)%Z
   | 33 => (-1 * s V_III_imdct_s_w + 1 <= 0 /\ 1 * s V_III_imdct_s_i + -3 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i + 3 <= 0)%Z
   | 34 => (-1 * s V_III_imdct_s_i + 3 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -3 <= 0 /\ -1 * s V_III_imdct_s_w + 1 <= 0)%Z
   | 35 => (-1 * s V_III_imdct_s_w + 1 <= 0 /\ 1 * s V_III_imdct_s_i + -3 <= 0 /\ -1 * s V_III_imdct_s_i + 3 <= 0 /\ -1 * s V_III_imdct_s_z + 1 <= 0)%Z
   | 36 => (-1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -2 <= 0)%Z
   | 37 => (1 * s V_III_imdct_s_i + -2 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w <= 0)%Z
   | 38 => (-1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -2 <= 0)%Z
   | 39 => (1 * s V_III_imdct_s_i + -2 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w <= 0)%Z
   | 40 => (-1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -2 <= 0)%Z
   | 41 => (1 * s V_III_imdct_s_i + -2 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w <= 0)%Z
   | 42 => (-1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -2 <= 0)%Z
   | 43 => (1 * s V_III_imdct_s_i + -2 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w <= 0)%Z
   | 44 => (-1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -2 <= 0)%Z
   | 45 => (1 * s V_III_imdct_s_i + -2 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w <= 0)%Z
   | 46 => (-1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -2 <= 0)%Z
   | 47 => (1 * s V_III_imdct_s_i + -2 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w <= 0)%Z
   | 48 => (-1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -2 <= 0)%Z
   | 49 => (1 * s V_III_imdct_s_i + -2 <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_w <= 0)%Z
   | 50 => (-1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_i <= 0 /\ -1 * s V_III_imdct_s_z <= 0 /\ 1 * s V_III_imdct_s_i + -2 <= 0)%Z
   | 51 => (-1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_i + 1 <= 0 /\ 1 * s V_III_imdct_s_i + -3 <= 0)%Z
   | 52 => (1 * s V_III_imdct_s_i + -3 <= 0 /\ -1 * s V_III_imdct_s_i + 1 <= 0 /\ -1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_z <= 0)%Z
   | 53 => (-1 * s V_III_imdct_s_z <= 0 /\ -1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_i + 1 <= 0 /\ 1 * s V_III_imdct_s_i + -3 <= 0)%Z
   | 54 => (1 * s V_III_imdct_s_i + -3 <= 0 /\ -1 * s V_III_imdct_s_i + 1 <= 0 /\ -1 * s V_III_imdct_s_w <= 0 /\ -1 * s V_III_imdct_s_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_III_imdct_s (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((18 # 1) <= z)%Q
   | 2 => ((18 # 1) + s V_III_imdct_s_z <= z)%Q
   | 3 => ((6 # 1) + s V_III_imdct_s_z
           + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 4 => ((6 # 1) + s V_III_imdct_s_z
           + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 5 => ((6 # 1) + s V_III_imdct_s_z
           + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 6 => ((6 # 1) + s V_III_imdct_s_z
           + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 7 => ((6 # 1) + s V_III_imdct_s_z
           + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 8 => (s V_III_imdct_s_z + (4 # 1) * max0(3 - s V_III_imdct_s_w)
           + max0(6 - s V_III_imdct_s_i) <= z)%Q
   | 9 => (s V_III_imdct_s_z + (4 # 1) * max0(3 - s V_III_imdct_s_w)
           + max0(6 - s V_III_imdct_s_i) <= z)%Q
   | 10 => (s V_III_imdct_s_z + (4 # 1) * max0(3 - s V_III_imdct_s_w)
            + max0(6 - s V_III_imdct_s_i) <= z)%Q
   | 11 => hints
     [(*-4 0*) F_max0_monotonic (F_check_ge (3 - s V_III_imdct_s_w) (2
                                                                    - 
                                                                    s V_III_imdct_s_w));
      (*-4 0*) F_max0_ge_0 (2 - s V_III_imdct_s_w);
      (*-1 0*) F_max0_monotonic (F_check_ge (6 - s V_III_imdct_s_i) (5
                                                                    - 
                                                                    s V_III_imdct_s_i));
      (*-1 0*) F_max0_ge_0 (5 - s V_III_imdct_s_i)]
     (s V_III_imdct_s_z + (4 # 1) * max0(3 - s V_III_imdct_s_w)
      + max0(6 - s V_III_imdct_s_i) <= z)%Q
   | 12 => (s V_III_imdct_s_z <= z)%Q
   | 13 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (6 - s V_III_imdct_s_i)) (F_check_ge (6
                                                                    - s V_III_imdct_s_i) (0))]
     (s V_III_imdct_s_z + (4 # 1) * max0(3 - s V_III_imdct_s_w)
      + max0(6 - s V_III_imdct_s_i) <= z)%Q
   | 14 => ((6 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 15 => ((6 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 16 => ((6 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 17 => ((6 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 18 => ((6 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 19 => ((6 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 20 => ((7 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 21 => ((7 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 22 => ((7 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 23 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (6
                                                               - s V_III_imdct_s_i) (0))) (F_max0_ge_0 (6
                                                                    - s V_III_imdct_s_i))]
     ((6 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
      + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 24 => ((6 # 1) + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 25 => ((6 # 1) + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 26 => ((6 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 27 => hints
     [(*-4 0*) F_max0_pre_decrement 1 (3 - s V_III_imdct_s_w) (1)]
     ((6 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
      + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 28 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 29 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 30 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 31 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 32 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 33 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 34 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 35 => hints
     [(*-1 0*) F_max0_ge_0 (3 - s V_III_imdct_s_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                               - s V_III_imdct_s_i) (0))) (F_max0_ge_0 (3
                                                                    - s V_III_imdct_s_i))]
     ((9 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
      + (4 # 1) * max0(3 - s V_III_imdct_s_w) <= z)%Q
   | 36 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 37 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 38 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 39 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 40 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 41 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 42 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 43 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 44 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 45 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 46 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 47 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 48 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 49 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 50 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 51 => ((11 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 52 => ((11 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 53 => ((11 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | 54 => ((10 # 1) - s V_III_imdct_s_i + s V_III_imdct_s_z
            + (4 # 1) * max0(2 - s V_III_imdct_s_w) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_III_imdct_s =>
    [mkPA Q (fun n z s => ai_III_imdct_s n s /\ annot0_III_imdct_s n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_III_imdct_s (proc_start P_III_imdct_s) s1 (proc_end P_III_imdct_s) s2 ->
    (s2 V_III_imdct_s_z <= (18 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_III_imdct_s.
Qed.
