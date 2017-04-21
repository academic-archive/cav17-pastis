Require Import pasta.Pasta.

Inductive proc: Type :=
  P_zsetcacheparams.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_zsetcacheparams_z := 1%positive.
Notation V_zsetcacheparams__tmp := 2%positive.
Notation V_zsetcacheparams_code := 3%positive.
Notation V_zsetcacheparams_i := 4%positive.
Notation V_zsetcacheparams_op := 5%positive.
Definition Pedges_zsetcacheparams: list (edge proc) :=
  (EA 1 (AAssign V_zsetcacheparams_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_zsetcacheparams_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::
  (EA 4 AWeaken 5)::(EA 5 (AGuard (fun s => ((eval (EVar V_zsetcacheparams_i)
  s) < (eval (ENum (3)) s))%Z)) 7)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_zsetcacheparams_i) s) >= (eval (ENum (3))
  s))%Z)) 6)::(EA 6 AWeaken 12)::(EA 7 AWeaken 8)::(EA 8 ANone 9)::
  (EA 9 AWeaken 10)::(EA 10 (AGuard (fun s => True)) 38)::(EA 10 (AGuard
  (fun s => True)) 11)::(EA 11 AWeaken 12)::(EA 12 ANone 35)::
  (EA 12 ANone 13)::(EA 12 ANone 17)::(EA 12 ANone 21)::(EA 12 ANone 25)::
  (EA 13 (AAssign V_zsetcacheparams_code None) 14)::(EA 14 AWeaken 15)::
  (EA 15 ANone 32)::(EA 15 ANone 16)::(EA 16 ANone 17)::(EA 17 (AAssign
  V_zsetcacheparams_code None) 18)::(EA 18 AWeaken 19)::(EA 19 ANone 29)::
  (EA 19 ANone 20)::(EA 20 ANone 21)::(EA 21 (AAssign V_zsetcacheparams_code
  None) 22)::(EA 22 AWeaken 23)::(EA 23 ANone 26)::(EA 23 ANone 24)::
  (EA 24 ANone 25)::(EA 25 ANone 35)::(EA 26 (AAssign V_zsetcacheparams__tmp
  (Some (EVar V_zsetcacheparams_code))) 27)::(EA 27 ANone 28)::
  (EA 28 AWeaken 54)::(EA 29 (AAssign V_zsetcacheparams__tmp
  (Some (EVar V_zsetcacheparams_code))) 30)::(EA 30 ANone 31)::
  (EA 31 AWeaken 54)::(EA 32 (AAssign V_zsetcacheparams__tmp
  (Some (EVar V_zsetcacheparams_code))) 33)::(EA 33 ANone 34)::
  (EA 34 AWeaken 54)::(EA 35 (AAssign V_zsetcacheparams__tmp None) 36)::
  (EA 36 ANone 37)::(EA 37 AWeaken 54)::(EA 38 AWeaken 39)::
  (EA 39 ANone 43)::(EA 39 ANone 40)::(EA 40 (AAssign V_zsetcacheparams__tmp
  None) 41)::(EA 41 ANone 42)::(EA 42 AWeaken 54)::(EA 43 AWeaken 44)::
  (EA 44 ANone 51)::(EA 44 ANone 45)::(EA 45 ANone 46)::(EA 46 (AAssign
  V_zsetcacheparams_i (Some (EAdd (EVar V_zsetcacheparams_i)
  (ENum (1))))) 47)::(EA 47 ANone 48)::(EA 48 ANone 49)::(EA 49 (AAssign
  V_zsetcacheparams_z (Some (EAdd (ENum (1))
  (EVar V_zsetcacheparams_z)))) 50)::(EA 50 AWeaken 5)::(EA 51 (AAssign
  V_zsetcacheparams__tmp (Some (ENum (-15)))) 52)::(EA 52 ANone 53)::
  (EA 53 AWeaken 54)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_zsetcacheparams => Pedges_zsetcacheparams
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_zsetcacheparams => 54
     end)%positive;
  var_global := var_global
}.

Definition ai_zsetcacheparams (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_z <= 0)%Z
   | 3 => (-1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 4 => (-1 * s V_zsetcacheparams_i <= 0 /\ 1 * s V_zsetcacheparams_i <= 0 /\ 1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_z <= 0)%Z
   | 5 => (-1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0)%Z
   | 6 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i + 3 <= 0)%Z
   | 7 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -2 <= 0)%Z
   | 8 => (1 * s V_zsetcacheparams_i + -2 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 9 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -2 <= 0)%Z
   | 10 => (1 * s V_zsetcacheparams_i + -2 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 11 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -2 <= 0)%Z
   | 12 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 13 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0)%Z
   | 14 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 15 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0)%Z
   | 16 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 17 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0)%Z
   | 18 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 19 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0)%Z
   | 20 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 21 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0)%Z
   | 22 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 23 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0)%Z
   | 24 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 25 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0)%Z
   | 26 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 27 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0)%Z
   | 28 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 29 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 30 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0)%Z
   | 31 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 32 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 33 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0)%Z
   | 34 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 35 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 36 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0)%Z
   | 37 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 38 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -2 <= 0)%Z
   | 39 => (1 * s V_zsetcacheparams_i + -2 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 40 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -2 <= 0)%Z
   | 41 => (1 * s V_zsetcacheparams_i + -2 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 42 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -2 <= 0)%Z
   | 43 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -2 <= 0)%Z
   | 44 => (1 * s V_zsetcacheparams_i + -2 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 45 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -2 <= 0)%Z
   | 46 => (1 * s V_zsetcacheparams_i + -2 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | 47 => (-1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_i + 1 <= 0)%Z
   | 48 => (-1 * s V_zsetcacheparams_i + 1 <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0)%Z
   | 49 => (-1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_i + 1 <= 0)%Z
   | 50 => (-1 * s V_zsetcacheparams_i + 1 <= 0 /\ 1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z + 1 <= 0)%Z
   | 51 => (-1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -2 <= 0)%Z
   | 52 => (1 * s V_zsetcacheparams_i + -2 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0 /\ 1 * s V_zsetcacheparams__tmp + 15 <= 0 /\ -1 * s V_zsetcacheparams__tmp + -15 <= 0)%Z
   | 53 => (-1 * s V_zsetcacheparams__tmp + -15 <= 0 /\ 1 * s V_zsetcacheparams__tmp + 15 <= 0 /\ -1 * s V_zsetcacheparams_i <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ 1 * s V_zsetcacheparams_i + -2 <= 0)%Z
   | 54 => (1 * s V_zsetcacheparams_i + -3 <= 0 /\ -1 * s V_zsetcacheparams_z <= 0 /\ -1 * s V_zsetcacheparams_i <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_zsetcacheparams (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((3 # 1) <= z)%Q
   | 2 => ((3 # 1) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 3 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 4 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 5 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 6 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 7 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 8 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 9 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsetcacheparams_z)) (F_check_ge (s V_zsetcacheparams_z) (0))]
     (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 10 => (s V_zsetcacheparams_z + max0(3 - s V_zsetcacheparams_i) <= z)%Q
   | 11 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsetcacheparams_z) (0))) (F_max0_ge_0 (s V_zsetcacheparams_z))]
     (s V_zsetcacheparams_z + max0(3 - s V_zsetcacheparams_i) <= z)%Q
   | 12 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 13 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 14 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 15 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 16 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 17 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 18 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 19 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 20 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 21 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 22 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 23 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 24 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 25 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 26 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 27 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 28 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3 - s V_zsetcacheparams_i) (2
                                                                    - s V_zsetcacheparams_i));
      (*-1 0*) F_max0_ge_0 (2 - s V_zsetcacheparams_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsetcacheparams_z)) (F_check_ge (s V_zsetcacheparams_z) (0))]
     (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 29 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 30 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 31 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3 - s V_zsetcacheparams_i) (2
                                                                    - s V_zsetcacheparams_i));
      (*-1 0*) F_max0_ge_0 (2 - s V_zsetcacheparams_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsetcacheparams_z)) (F_check_ge (s V_zsetcacheparams_z) (0))]
     (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 32 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 33 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 34 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3 - s V_zsetcacheparams_i) (2
                                                                    - s V_zsetcacheparams_i));
      (*-1 0*) F_max0_ge_0 (2 - s V_zsetcacheparams_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsetcacheparams_z)) (F_check_ge (s V_zsetcacheparams_z) (0))]
     (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 35 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 36 => (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 37 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3 - s V_zsetcacheparams_i) (2
                                                                    - s V_zsetcacheparams_i));
      (*-1 0*) F_max0_ge_0 (2 - s V_zsetcacheparams_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_zsetcacheparams_z)) (F_check_ge (s V_zsetcacheparams_z) (0))]
     (max0(3 - s V_zsetcacheparams_i) + max0(s V_zsetcacheparams_z) <= z)%Q
   | 38 => (s V_zsetcacheparams_z + max0(3 - s V_zsetcacheparams_i) <= z)%Q
   | 39 => (s V_zsetcacheparams_z + max0(3 - s V_zsetcacheparams_i) <= z)%Q
   | 40 => (s V_zsetcacheparams_z + max0(3 - s V_zsetcacheparams_i) <= z)%Q
   | 41 => (s V_zsetcacheparams_z + max0(3 - s V_zsetcacheparams_i) <= z)%Q
   | 42 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (3 - s V_zsetcacheparams_i) (1);
      (*-1 0*) F_max0_ge_0 (2 - s V_zsetcacheparams_i)]
     (s V_zsetcacheparams_z + max0(3 - s V_zsetcacheparams_i) <= z)%Q
   | 43 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (3 - s V_zsetcacheparams_i)) (F_check_ge (3
                                                                    - s V_zsetcacheparams_i) (0))]
     (s V_zsetcacheparams_z + max0(3 - s V_zsetcacheparams_i) <= z)%Q
   | 44 => ((3 # 1) - s V_zsetcacheparams_i + s V_zsetcacheparams_z <= z)%Q
   | 45 => ((3 # 1) - s V_zsetcacheparams_i + s V_zsetcacheparams_z <= z)%Q
   | 46 => ((3 # 1) - s V_zsetcacheparams_i + s V_zsetcacheparams_z <= z)%Q
   | 47 => ((4 # 1) - s V_zsetcacheparams_i + s V_zsetcacheparams_z <= z)%Q
   | 48 => ((4 # 1) - s V_zsetcacheparams_i + s V_zsetcacheparams_z <= z)%Q
   | 49 => ((4 # 1) - s V_zsetcacheparams_i + s V_zsetcacheparams_z <= z)%Q
   | 50 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_zsetcacheparams_z) (0))) (F_max0_ge_0 (s V_zsetcacheparams_z));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                               - s V_zsetcacheparams_i) (0))) (F_max0_ge_0 (3
                                                                    - s V_zsetcacheparams_i))]
     ((3 # 1) - s V_zsetcacheparams_i + s V_zsetcacheparams_z <= z)%Q
   | 51 => ((3 # 1) - s V_zsetcacheparams_i + s V_zsetcacheparams_z <= z)%Q
   | 52 => ((3 # 1) - s V_zsetcacheparams_i + s V_zsetcacheparams_z <= z)%Q
   | 53 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (3 - s V_zsetcacheparams_i) (2
                                                                    - s V_zsetcacheparams_i));
      (*-1 0*) F_max0_ge_0 (2 - s V_zsetcacheparams_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (3
                                                               - s V_zsetcacheparams_i) (0))) (F_max0_ge_0 (3
                                                                    - s V_zsetcacheparams_i))]
     ((3 # 1) - s V_zsetcacheparams_i + s V_zsetcacheparams_z <= z)%Q
   | 54 => (s V_zsetcacheparams_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_zsetcacheparams =>
    [mkPA Q (fun n z s => ai_zsetcacheparams n s /\ annot0_zsetcacheparams n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_zsetcacheparams (proc_start P_zsetcacheparams) s1 (proc_end P_zsetcacheparams) s2 ->
    (s2 V_zsetcacheparams_z <= (3 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_zsetcacheparams.
Qed.
