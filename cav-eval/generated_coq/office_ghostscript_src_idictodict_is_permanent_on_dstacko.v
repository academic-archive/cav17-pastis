Require Import pasta.Pasta.

Inductive proc: Type :=
  P_dict_is_permanent_on_dstack.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_dict_is_permanent_on_dstack_z := 1%positive.
Notation V_dict_is_permanent_on_dstack__tmp := 2%positive.
Notation V_dict_is_permanent_on_dstack_count := 3%positive.
Notation V_dict_is_permanent_on_dstack_i := 4%positive.
Notation V_dict_is_permanent_on_dstack_min_dstack_size := 5%positive.
Notation V_dict_is_permanent_on_dstack_pdref := 6%positive.
Definition Pedges_dict_is_permanent_on_dstack: list (edge proc) :=
  (EA 1 (AAssign V_dict_is_permanent_on_dstack_z (Some (ENum (0)))) 2)::
  (EA 2 (AGuard
  (fun s => ((eval (EVar V_dict_is_permanent_on_dstack_min_dstack_size) s) >=
  (eval (ENum (0)) s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_dict_is_permanent_on_dstack_i) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_dict_is_permanent_on_dstack_count) s) >=
  (eval (ENum (0)) s))%Z)) 5)::(EA 5 AWeaken 6)::(EA 6 ANone 25)::
  (EA 6 ANone 7)::(EA 7 (AAssign V_dict_is_permanent_on_dstack_count
  None) 8)::(EA 8 (AAssign V_dict_is_permanent_on_dstack_i
  (Some (ESub (EVar V_dict_is_permanent_on_dstack_count)
  (EVar V_dict_is_permanent_on_dstack_min_dstack_size)))) 9)::
  (EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_dict_is_permanent_on_dstack_i) s) <
  (eval (EVar V_dict_is_permanent_on_dstack_count) s))%Z)) 14)::
  (EA 11 (AGuard (fun s => ((eval (EVar V_dict_is_permanent_on_dstack_i)
  s) >= (eval (EVar V_dict_is_permanent_on_dstack_count) s))%Z)) 12)::
  (EA 12 AWeaken 13)::(EA 13 ANone 31)::(EA 14 AWeaken 15)::
  (EA 15 ANone 22)::(EA 15 ANone 16)::(EA 16 ANone 17)::(EA 17 (AAssign
  V_dict_is_permanent_on_dstack_i
  (Some (EAdd (EVar V_dict_is_permanent_on_dstack_i) (ENum (1))))) 18)::
  (EA 18 ANone 19)::(EA 19 ANone 20)::(EA 20 (AAssign
  V_dict_is_permanent_on_dstack_z (Some (EAdd (ENum (1))
  (EVar V_dict_is_permanent_on_dstack_z)))) 21)::(EA 21 AWeaken 11)::
  (EA 22 (AAssign V_dict_is_permanent_on_dstack__tmp (Some (ENum (1)))) 23)::
  (EA 23 ANone 24)::(EA 24 AWeaken 45)::(EA 25 (AAssign
  V_dict_is_permanent_on_dstack_i (Some (ENum (0)))) 26)::(EA 26 ANone 27)::
  (EA 27 AWeaken 28)::(EA 28 (AGuard
  (fun s => ((eval (EVar V_dict_is_permanent_on_dstack_i) s) <
  (eval (EVar V_dict_is_permanent_on_dstack_min_dstack_size) s))%Z)) 34)::
  (EA 28 (AGuard (fun s => ((eval (EVar V_dict_is_permanent_on_dstack_i)
  s) >= (eval (EVar V_dict_is_permanent_on_dstack_min_dstack_size)
  s))%Z)) 29)::(EA 29 AWeaken 30)::(EA 30 ANone 31)::(EA 31 (AAssign
  V_dict_is_permanent_on_dstack__tmp (Some (ENum (0)))) 32)::
  (EA 32 ANone 33)::(EA 33 AWeaken 45)::(EA 34 AWeaken 35)::
  (EA 35 ANone 42)::(EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign
  V_dict_is_permanent_on_dstack_i
  (Some (EAdd (EVar V_dict_is_permanent_on_dstack_i) (ENum (1))))) 38)::
  (EA 38 ANone 39)::(EA 39 ANone 40)::(EA 40 (AAssign
  V_dict_is_permanent_on_dstack_z (Some (EAdd (ENum (1))
  (EVar V_dict_is_permanent_on_dstack_z)))) 41)::(EA 41 AWeaken 28)::
  (EA 42 (AAssign V_dict_is_permanent_on_dstack__tmp (Some (ENum (1)))) 43)::
  (EA 43 ANone 44)::(EA 44 AWeaken 45)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_dict_is_permanent_on_dstack => Pedges_dict_is_permanent_on_dstack
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_dict_is_permanent_on_dstack => 45
     end)%positive;
  var_global := var_global
}.

Definition ai_dict_is_permanent_on_dstack (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0)%Z
   | 3 => (-1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0)%Z
   | 4 => (-1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0)%Z
   | 5 => (-1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0)%Z
   | 6 => (-1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0)%Z
   | 7 => (-1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0)%Z
   | 8 => (-1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0)%Z
   | 9 => (-1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0)%Z
   | 10 => (-1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0)%Z
   | 11 => (-1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0)%Z
   | 12 => (-1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_count+ -1 * s V_dict_is_permanent_on_dstack_i <= 0)%Z
   | 13 => (1 * s V_dict_is_permanent_on_dstack_count+ -1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0)%Z
   | 14 => (-1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count+ 1 * s V_dict_is_permanent_on_dstack_i + 1 <= 0)%Z
   | 15 => (-1 * s V_dict_is_permanent_on_dstack_count+ 1 * s V_dict_is_permanent_on_dstack_i + 1 <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0)%Z
   | 16 => (-1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count+ 1 * s V_dict_is_permanent_on_dstack_i + 1 <= 0)%Z
   | 17 => (-1 * s V_dict_is_permanent_on_dstack_count+ 1 * s V_dict_is_permanent_on_dstack_i + 1 <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0)%Z
   | 18 => (-1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count+ 1 * s V_dict_is_permanent_on_dstack_i <= 0)%Z
   | 19 => (-1 * s V_dict_is_permanent_on_dstack_count+ 1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0)%Z
   | 20 => (-1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count+ 1 * s V_dict_is_permanent_on_dstack_i <= 0)%Z
   | 21 => (-1 * s V_dict_is_permanent_on_dstack_count+ 1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z + 1 <= 0)%Z
   | 22 => (-1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count+ 1 * s V_dict_is_permanent_on_dstack_i + 1 <= 0)%Z
   | 23 => (-1 * s V_dict_is_permanent_on_dstack_count+ 1 * s V_dict_is_permanent_on_dstack_i + 1 <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ 1 * s V_dict_is_permanent_on_dstack__tmp + -1 <= 0 /\ -1 * s V_dict_is_permanent_on_dstack__tmp + 1 <= 0)%Z
   | 24 => (-1 * s V_dict_is_permanent_on_dstack__tmp + 1 <= 0 /\ 1 * s V_dict_is_permanent_on_dstack__tmp + -1 <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count+ 1 * s V_dict_is_permanent_on_dstack_i + 1 <= 0)%Z
   | 25 => (-1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0)%Z
   | 26 => (-1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0)%Z
   | 27 => (-1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0)%Z
   | 28 => (-1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0)%Z
   | 29 => (1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i+ 1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0)%Z
   | 30 => (-1 * s V_dict_is_permanent_on_dstack_i+ 1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0)%Z
   | 31 => (-1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0)%Z
   | 32 => (-1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ 1 * s V_dict_is_permanent_on_dstack__tmp <= 0 /\ -1 * s V_dict_is_permanent_on_dstack__tmp <= 0)%Z
   | 33 => (-1 * s V_dict_is_permanent_on_dstack__tmp <= 0 /\ 1 * s V_dict_is_permanent_on_dstack__tmp <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0)%Z
   | 34 => (-1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size + 1 <= 0)%Z
   | 35 => (1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size + 1 <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0)%Z
   | 36 => (-1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size + 1 <= 0)%Z
   | 37 => (1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size + 1 <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0)%Z
   | 38 => (-1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i + 1 <= 0)%Z
   | 39 => (-1 * s V_dict_is_permanent_on_dstack_i + 1 <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0)%Z
   | 40 => (-1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i + 1 <= 0)%Z
   | 41 => (-1 * s V_dict_is_permanent_on_dstack_i + 1 <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z + 1 <= 0)%Z
   | 42 => (-1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size + 1 <= 0)%Z
   | 43 => (1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size + 1 <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ 1 * s V_dict_is_permanent_on_dstack__tmp + -1 <= 0 /\ -1 * s V_dict_is_permanent_on_dstack__tmp + 1 <= 0)%Z
   | 44 => (-1 * s V_dict_is_permanent_on_dstack__tmp + 1 <= 0 /\ 1 * s V_dict_is_permanent_on_dstack__tmp + -1 <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_count <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_i <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack_i+ -1 * s V_dict_is_permanent_on_dstack_min_dstack_size + 1 <= 0)%Z
   | 45 => (-1 * s V_dict_is_permanent_on_dstack_min_dstack_size <= 0 /\ -1 * s V_dict_is_permanent_on_dstack__tmp <= 0 /\ -1 * s V_dict_is_permanent_on_dstack_z <= 0 /\ 1 * s V_dict_is_permanent_on_dstack__tmp + -1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_dict_is_permanent_on_dstack (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 2 => (s V_dict_is_permanent_on_dstack_z
           + max0(s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 3 => (s V_dict_is_permanent_on_dstack_z
           + max0(s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 4 => (s V_dict_is_permanent_on_dstack_z
           + max0(s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 5 => (s V_dict_is_permanent_on_dstack_z
           + max0(s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 6 => (s V_dict_is_permanent_on_dstack_z
           + max0(s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 7 => (s V_dict_is_permanent_on_dstack_z
           + max0(s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 8 => (s V_dict_is_permanent_on_dstack_z
           + max0(s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 9 => (s V_dict_is_permanent_on_dstack_z
           + max0(s V_dict_is_permanent_on_dstack_count
                  - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 10 => (s V_dict_is_permanent_on_dstack_z
            + max0(s V_dict_is_permanent_on_dstack_count
                   - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 11 => (s V_dict_is_permanent_on_dstack_z
            + max0(s V_dict_is_permanent_on_dstack_count
                   - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 12 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (s V_dict_is_permanent_on_dstack_count
                                            - s V_dict_is_permanent_on_dstack_i) (-1
                                                                    + s V_dict_is_permanent_on_dstack_count
                                                                    - s V_dict_is_permanent_on_dstack_i));
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                 + s V_dict_is_permanent_on_dstack_count
                                                 - s V_dict_is_permanent_on_dstack_i)) (F_check_ge (0) (0))]
     (s V_dict_is_permanent_on_dstack_z
      + max0(s V_dict_is_permanent_on_dstack_count
             - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 13 => (s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_dict_is_permanent_on_dstack_count
                                       - s V_dict_is_permanent_on_dstack_i) (1)]
     (s V_dict_is_permanent_on_dstack_z
      + max0(s V_dict_is_permanent_on_dstack_count
             - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 15 => ((1 # 1) + s V_dict_is_permanent_on_dstack_z
            + max0(-1 + s V_dict_is_permanent_on_dstack_count
                   - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 16 => ((1 # 1) + s V_dict_is_permanent_on_dstack_z
            + max0(-1 + s V_dict_is_permanent_on_dstack_count
                   - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 17 => ((1 # 1) + s V_dict_is_permanent_on_dstack_z
            + max0(-1 + s V_dict_is_permanent_on_dstack_count
                   - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 18 => ((1 # 1) + s V_dict_is_permanent_on_dstack_z
            + max0(s V_dict_is_permanent_on_dstack_count
                   - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 19 => ((1 # 1) + s V_dict_is_permanent_on_dstack_z
            + max0(s V_dict_is_permanent_on_dstack_count
                   - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 20 => ((1 # 1) + s V_dict_is_permanent_on_dstack_z
            + max0(s V_dict_is_permanent_on_dstack_count
                   - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 21 => (s V_dict_is_permanent_on_dstack_z
            + max0(s V_dict_is_permanent_on_dstack_count
                   - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 22 => ((1 # 1) + s V_dict_is_permanent_on_dstack_z
            + max0(-1 + s V_dict_is_permanent_on_dstack_count
                   - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 23 => ((1 # 1) + s V_dict_is_permanent_on_dstack_z
            + max0(-1 + s V_dict_is_permanent_on_dstack_count
                   - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 + s V_dict_is_permanent_on_dstack_count
                            - s V_dict_is_permanent_on_dstack_i)]
     ((1 # 1) + s V_dict_is_permanent_on_dstack_z
      + max0(-1 + s V_dict_is_permanent_on_dstack_count
             - s V_dict_is_permanent_on_dstack_i) <= z)%Q
   | 25 => (s V_dict_is_permanent_on_dstack_z
            + max0(s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 26 => (s V_dict_is_permanent_on_dstack_z
            + max0(-s V_dict_is_permanent_on_dstack_i
                   + s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 27 => (s V_dict_is_permanent_on_dstack_z
            + max0(-s V_dict_is_permanent_on_dstack_i
                   + s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 28 => (s V_dict_is_permanent_on_dstack_z
            + max0(-s V_dict_is_permanent_on_dstack_i
                   + s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 29 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-s V_dict_is_permanent_on_dstack_i
                                             + s V_dict_is_permanent_on_dstack_min_dstack_size) (-1
                                                                    - s V_dict_is_permanent_on_dstack_i
                                                                    + s V_dict_is_permanent_on_dstack_min_dstack_size));
      (*-1 0*) F_max0_ge_0 (-1 - s V_dict_is_permanent_on_dstack_i
                            + s V_dict_is_permanent_on_dstack_min_dstack_size)]
     (s V_dict_is_permanent_on_dstack_z
      + max0(-s V_dict_is_permanent_on_dstack_i
             + s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 30 => (s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 31 => (s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 32 => (s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 33 => (s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 34 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (-s V_dict_is_permanent_on_dstack_i
                                                   + s V_dict_is_permanent_on_dstack_min_dstack_size)) (F_check_ge (-
                                                                    s V_dict_is_permanent_on_dstack_i
                                                                    + s V_dict_is_permanent_on_dstack_min_dstack_size) (0))]
     (s V_dict_is_permanent_on_dstack_z
      + max0(-s V_dict_is_permanent_on_dstack_i
             + s V_dict_is_permanent_on_dstack_min_dstack_size) <= z)%Q
   | 35 => (-s V_dict_is_permanent_on_dstack_i
            + s V_dict_is_permanent_on_dstack_min_dstack_size
            + s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 36 => (-s V_dict_is_permanent_on_dstack_i
            + s V_dict_is_permanent_on_dstack_min_dstack_size
            + s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 37 => (-s V_dict_is_permanent_on_dstack_i
            + s V_dict_is_permanent_on_dstack_min_dstack_size
            + s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 38 => ((1 # 1) - s V_dict_is_permanent_on_dstack_i
            + s V_dict_is_permanent_on_dstack_min_dstack_size
            + s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 39 => ((1 # 1) - s V_dict_is_permanent_on_dstack_i
            + s V_dict_is_permanent_on_dstack_min_dstack_size
            + s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 40 => ((1 # 1) - s V_dict_is_permanent_on_dstack_i
            + s V_dict_is_permanent_on_dstack_min_dstack_size
            + s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 41 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_dict_is_permanent_on_dstack_i
                                                               + s V_dict_is_permanent_on_dstack_min_dstack_size) (0))) (F_max0_ge_0 (-
                                                                    s V_dict_is_permanent_on_dstack_i
                                                                    + s V_dict_is_permanent_on_dstack_min_dstack_size))]
     (-s V_dict_is_permanent_on_dstack_i
      + s V_dict_is_permanent_on_dstack_min_dstack_size
      + s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 42 => (-s V_dict_is_permanent_on_dstack_i
            + s V_dict_is_permanent_on_dstack_min_dstack_size
            + s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 43 => (-s V_dict_is_permanent_on_dstack_i
            + s V_dict_is_permanent_on_dstack_min_dstack_size
            + s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 44 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (-s V_dict_is_permanent_on_dstack_i
                                       + s V_dict_is_permanent_on_dstack_min_dstack_size) (1);
      (*-1 0*) F_max0_ge_0 (-1 - s V_dict_is_permanent_on_dstack_i
                            + s V_dict_is_permanent_on_dstack_min_dstack_size);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_dict_is_permanent_on_dstack_i
                                                               + s V_dict_is_permanent_on_dstack_min_dstack_size) (0))) (F_max0_ge_0 (-
                                                                    s V_dict_is_permanent_on_dstack_i
                                                                    + s V_dict_is_permanent_on_dstack_min_dstack_size))]
     (-s V_dict_is_permanent_on_dstack_i
      + s V_dict_is_permanent_on_dstack_min_dstack_size
      + s V_dict_is_permanent_on_dstack_z <= z)%Q
   | 45 => (s V_dict_is_permanent_on_dstack_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_dict_is_permanent_on_dstack =>
    [mkPA Q (fun n z s => ai_dict_is_permanent_on_dstack n s /\ annot0_dict_is_permanent_on_dstack n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_dict_is_permanent_on_dstack (proc_start P_dict_is_permanent_on_dstack) s1 (proc_end P_dict_is_permanent_on_dstack) s2 ->
    (s2 V_dict_is_permanent_on_dstack_z <= max0(s1 V_dict_is_permanent_on_dstack_min_dstack_size))%Q.
Proof.
  prove_bound ipa admissible_ipa P_dict_is_permanent_on_dstack.
Qed.
