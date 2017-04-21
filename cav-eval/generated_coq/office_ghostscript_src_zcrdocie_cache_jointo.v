Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cie_cache_joint.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cie_cache_joint_z := 1%positive.
Notation V_cie_cache_joint__tmp := 2%positive.
Notation V_cie_cache_joint_code := 3%positive.
Notation V_cie_cache_joint_es_code_ := 4%positive.
Notation V_cie_cache_joint_i := 5%positive.
Notation V_cie_cache_joint_j := 6%positive.
Notation V_cie_cache_joint_space := 7%positive.
Notation V_cie_cache_joint_pcrprocs := 8%positive.
Notation V_cie_cache_joint_pgs := 9%positive.
Definition Pedges_cie_cache_joint: list (edge proc) :=
  (EA 1 (AAssign V_cie_cache_joint_z (Some (ENum (0)))) 2)::
  (EA 2 AWeaken 3)::(EA 3 ANone 55)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 ANone 52)::(EA 5 ANone 6)::(EA 6 (AAssign V_cie_cache_joint_code
  None) 7)::(EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_cie_cache_joint_code) s) < (eval (ENum (0))
  s))%Z)) 48)::(EA 8 (AGuard (fun s => ((eval (EVar V_cie_cache_joint_code)
  s) >= (eval (ENum (0)) s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 ANone 11)::
  (EA 10 ANone 16)::(EA 11 (AAssign V_cie_cache_joint_es_code_ None) 12)::
  (EA 12 AWeaken 13)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_cie_cache_joint_es_code_) s) < (eval (ENum (0))
  s))%Z)) 44)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_cie_cache_joint_es_code_) s) >= (eval (ENum (0))
  s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 ANone 16)::(EA 16 (AAssign
  V_cie_cache_joint_space None) 17)::(EA 17 (AAssign V_cie_cache_joint_i
  (Some (ENum (0)))) 18)::(EA 18 ANone 19)::(EA 19 AWeaken 20)::
  (EA 20 (AGuard (fun s => ((eval (EVar V_cie_cache_joint_i) s) <
  (eval (ENum (3)) s))%Z)) 25)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_cie_cache_joint_i) s) >= (eval (ENum (3))
  s))%Z)) 21)::(EA 21 AWeaken 22)::(EA 22 (AAssign V_cie_cache_joint__tmp
  None) 23)::(EA 23 ANone 24)::(EA 24 AWeaken 58)::(EA 25 AWeaken 26)::
  (EA 26 (AAssign V_cie_cache_joint_j (Some (ENum (0)))) 27)::
  (EA 27 ANone 28)::(EA 28 AWeaken 29)::(EA 29 (AGuard
  (fun s => ((eval (EVar V_cie_cache_joint_j) s) < (eval (ENum (24))
  s))%Z)) 37)::(EA 29 (AGuard (fun s => ((eval (EVar V_cie_cache_joint_j)
  s) >= (eval (ENum (24)) s))%Z)) 30)::(EA 30 AWeaken 31)::(EA 31 ANone 32)::
  (EA 32 (AAssign V_cie_cache_joint_i (Some (EAdd (EVar V_cie_cache_joint_i)
  (ENum (1))))) 33)::(EA 33 ANone 34)::(EA 34 ANone 35)::(EA 35 (AAssign
  V_cie_cache_joint_z (Some (EAdd (ENum (1))
  (EVar V_cie_cache_joint_z)))) 36)::(EA 36 AWeaken 20)::(EA 37 AWeaken 38)::
  (EA 38 ANone 39)::(EA 39 (AAssign V_cie_cache_joint_j
  (Some (EAdd (EVar V_cie_cache_joint_j) (ENum (1))))) 40)::
  (EA 40 ANone 41)::(EA 41 ANone 42)::(EA 42 (AAssign V_cie_cache_joint_z
  (Some (EAdd (ENum (1)) (EVar V_cie_cache_joint_z)))) 43)::
  (EA 43 AWeaken 29)::(EA 44 AWeaken 45)::(EA 45 (AAssign
  V_cie_cache_joint__tmp (Some (EVar V_cie_cache_joint_es_code_))) 46)::
  (EA 46 ANone 47)::(EA 47 AWeaken 58)::(EA 48 AWeaken 49)::(EA 49 (AAssign
  V_cie_cache_joint__tmp (Some (EVar V_cie_cache_joint_code))) 50)::
  (EA 50 ANone 51)::(EA 51 AWeaken 58)::(EA 52 (AAssign
  V_cie_cache_joint__tmp (Some (ENum (-25)))) 53)::(EA 53 ANone 54)::
  (EA 54 AWeaken 58)::(EA 55 (AAssign V_cie_cache_joint__tmp
  (Some (ENum (0)))) 56)::(EA 56 ANone 57)::(EA 57 AWeaken 58)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cie_cache_joint => Pedges_cie_cache_joint
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cie_cache_joint => 58
     end)%positive;
  var_global := var_global
}.

Definition ai_cie_cache_joint (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 3 => (-1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0)%Z
   | 4 => (1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 5 => (-1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0)%Z
   | 6 => (1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 7 => (-1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0)%Z
   | 8 => (1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 9 => (-1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 10 => (-1 * s V_cie_cache_joint_code <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 11 => (-1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 12 => (-1 * s V_cie_cache_joint_code <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 13 => (-1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 14 => (-1 * s V_cie_cache_joint_code <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_es_code_ <= 0)%Z
   | 15 => (-1 * s V_cie_cache_joint_es_code_ <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 16 => (-1 * s V_cie_cache_joint_code <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 17 => (-1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 18 => (-1 * s V_cie_cache_joint_code <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_i <= 0)%Z
   | 19 => (-1 * s V_cie_cache_joint_i <= 0 /\ 1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 20 => (-1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 21 => (-1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_i + 3 <= 0)%Z
   | 22 => (-1 * s V_cie_cache_joint_i + 3 <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 23 => (-1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_i + 3 <= 0)%Z
   | 24 => (-1 * s V_cie_cache_joint_i + 3 <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 25 => (-1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_i + -2 <= 0)%Z
   | 26 => (1 * s V_cie_cache_joint_i + -2 <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 27 => (-1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_i + -2 <= 0 /\ 1 * s V_cie_cache_joint_j <= 0 /\ -1 * s V_cie_cache_joint_j <= 0)%Z
   | 28 => (-1 * s V_cie_cache_joint_j <= 0 /\ 1 * s V_cie_cache_joint_j <= 0 /\ 1 * s V_cie_cache_joint_i + -2 <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 29 => (-1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_j <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_code <= 0 /\ 1 * s V_cie_cache_joint_j + -24 <= 0)%Z
   | 30 => (1 * s V_cie_cache_joint_j + -24 <= 0 /\ -1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_j + 24 <= 0)%Z
   | 31 => (-1 * s V_cie_cache_joint_j + 24 <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_code <= 0 /\ 1 * s V_cie_cache_joint_j + -24 <= 0)%Z
   | 32 => (1 * s V_cie_cache_joint_j + -24 <= 0 /\ -1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_j + 24 <= 0)%Z
   | 33 => (-1 * s V_cie_cache_joint_j + 24 <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_code <= 0 /\ 1 * s V_cie_cache_joint_j + -24 <= 0 /\ -1 * s V_cie_cache_joint_i + 1 <= 0)%Z
   | 34 => (-1 * s V_cie_cache_joint_i + 1 <= 0 /\ 1 * s V_cie_cache_joint_j + -24 <= 0 /\ -1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_j + 24 <= 0)%Z
   | 35 => (-1 * s V_cie_cache_joint_j + 24 <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_code <= 0 /\ 1 * s V_cie_cache_joint_j + -24 <= 0 /\ -1 * s V_cie_cache_joint_i + 1 <= 0)%Z
   | 36 => (-1 * s V_cie_cache_joint_i + 1 <= 0 /\ 1 * s V_cie_cache_joint_j + -24 <= 0 /\ -1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_j + 24 <= 0 /\ -1 * s V_cie_cache_joint_z + 1 <= 0)%Z
   | 37 => (-1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_j <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_j + -23 <= 0)%Z
   | 38 => (1 * s V_cie_cache_joint_j + -23 <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_j <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 39 => (-1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_j <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_j + -23 <= 0)%Z
   | 40 => (-1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_j + 1 <= 0 /\ 1 * s V_cie_cache_joint_j + -24 <= 0)%Z
   | 41 => (1 * s V_cie_cache_joint_j + -24 <= 0 /\ -1 * s V_cie_cache_joint_j + 1 <= 0 /\ -1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 42 => (-1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_j + 1 <= 0 /\ 1 * s V_cie_cache_joint_j + -24 <= 0)%Z
   | 43 => (1 * s V_cie_cache_joint_j + -24 <= 0 /\ -1 * s V_cie_cache_joint_j + 1 <= 0 /\ -1 * s V_cie_cache_joint_code <= 0 /\ -1 * s V_cie_cache_joint_i <= 0 /\ -1 * s V_cie_cache_joint_z + 1 <= 0)%Z
   | 44 => (-1 * s V_cie_cache_joint_code <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_es_code_ + 1 <= 0)%Z
   | 45 => (1 * s V_cie_cache_joint_es_code_ + 1 <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 46 => (-1 * s V_cie_cache_joint_code <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_es_code_ + 1 <= 0 /\ 1 * s V_cie_cache_joint__tmp + 1 <= 0)%Z
   | 47 => (1 * s V_cie_cache_joint__tmp + 1 <= 0 /\ 1 * s V_cie_cache_joint_es_code_ + 1 <= 0 /\ -1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_code <= 0)%Z
   | 48 => (-1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_code + 1 <= 0)%Z
   | 49 => (1 * s V_cie_cache_joint_code + 1 <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 50 => (-1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_code + 1 <= 0 /\ 1 * s V_cie_cache_joint__tmp + 1 <= 0)%Z
   | 51 => (1 * s V_cie_cache_joint__tmp + 1 <= 0 /\ 1 * s V_cie_cache_joint_code + 1 <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 52 => (1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 53 => (-1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint__tmp + 25 <= 0 /\ -1 * s V_cie_cache_joint__tmp + -25 <= 0)%Z
   | 54 => (-1 * s V_cie_cache_joint__tmp + -25 <= 0 /\ 1 * s V_cie_cache_joint__tmp + 25 <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 55 => (1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 56 => (-1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ 1 * s V_cie_cache_joint__tmp <= 0 /\ -1 * s V_cie_cache_joint__tmp <= 0)%Z
   | 57 => (-1 * s V_cie_cache_joint__tmp <= 0 /\ 1 * s V_cie_cache_joint__tmp <= 0 /\ 1 * s V_cie_cache_joint_z <= 0 /\ -1 * s V_cie_cache_joint_z <= 0)%Z
   | 58 => (-1 * s V_cie_cache_joint_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cie_cache_joint (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((75 # 1) <= z)%Q
   | 2 => ((75 # 1) + max0(s V_cie_cache_joint_z) <= z)%Q
   | 3 => ((75 # 1) + max0(s V_cie_cache_joint_z) <= z)%Q
   | 4 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (s V_cie_cache_joint_z)) (F_check_ge (0) (0))]
     ((75 # 1) + max0(s V_cie_cache_joint_z) <= z)%Q
   | 5 => ((75 # 1) <= z)%Q
   | 6 => ((75 # 1) <= z)%Q
   | 7 => ((75 # 1) <= z)%Q
   | 8 => ((75 # 1) <= z)%Q
   | 9 => ((75 # 1) <= z)%Q
   | 10 => ((75 # 1) <= z)%Q
   | 11 => ((75 # 1) <= z)%Q
   | 12 => ((75 # 1) <= z)%Q
   | 13 => ((75 # 1) <= z)%Q
   | 14 => ((75 # 1) <= z)%Q
   | 15 => ((75 # 1) <= z)%Q
   | 16 => ((75 # 1) <= z)%Q
   | 17 => ((75 # 1) <= z)%Q
   | 18 => ((25 # 1) * max0(3 - s V_cie_cache_joint_i) <= z)%Q
   | 19 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_cie_cache_joint_z)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_cie_cache_joint_z) (0))) (F_max0_ge_0 (-
                                                                    s V_cie_cache_joint_z))]
     ((25 # 1) * max0(3 - s V_cie_cache_joint_i) <= z)%Q
   | 20 => (s V_cie_cache_joint_z
            + (25 # 1) * max0(3 - s V_cie_cache_joint_i) <= z)%Q
   | 21 => (s V_cie_cache_joint_z
            + (25 # 1) * max0(3 - s V_cie_cache_joint_i) <= z)%Q
   | 22 => (s V_cie_cache_joint_z
            + (25 # 1) * max0(3 - s V_cie_cache_joint_i) <= z)%Q
   | 23 => (s V_cie_cache_joint_z
            + (25 # 1) * max0(3 - s V_cie_cache_joint_i) <= z)%Q
   | 24 => hints
     [(*-25 0*) F_max0_monotonic (F_check_ge (3 - s V_cie_cache_joint_i) (2
                                                                    - s V_cie_cache_joint_i));
      (*-25 0*) F_max0_ge_0 (2 - s V_cie_cache_joint_i)]
     (s V_cie_cache_joint_z + (25 # 1) * max0(3 - s V_cie_cache_joint_i) <= z)%Q
   | 25 => (s V_cie_cache_joint_z
            + (25 # 1) * max0(3 - s V_cie_cache_joint_i) <= z)%Q
   | 26 => (s V_cie_cache_joint_z
            + (25 # 1) * max0(3 - s V_cie_cache_joint_i) <= z)%Q
   | 27 => (-(24 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(3 - s V_cie_cache_joint_i)
            + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 28 => hints
     [(*-25 0*) F_max0_pre_decrement 1 (3 - s V_cie_cache_joint_i) (1)]
     (-(24 # 1) + s V_cie_cache_joint_z
      + (25 # 1) * max0(3 - s V_cie_cache_joint_i)
      + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 29 => ((1 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(2 - s V_cie_cache_joint_i)
            + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 30 => ((1 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(2 - s V_cie_cache_joint_i)
            + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 31 => ((1 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(2 - s V_cie_cache_joint_i)
            + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 32 => ((1 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(2 - s V_cie_cache_joint_i)
            + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 33 => ((1 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(3 - s V_cie_cache_joint_i)
            + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 34 => ((1 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(3 - s V_cie_cache_joint_i)
            + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 35 => ((1 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(3 - s V_cie_cache_joint_i)
            + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 36 => hints
     [(*0 1*) F_max0_monotonic (F_check_ge (24 - s V_cie_cache_joint_j) (23
                                                                    - s V_cie_cache_joint_j));
      (*-1 0*) F_max0_ge_0 (23 - s V_cie_cache_joint_j)]
     (s V_cie_cache_joint_z + (25 # 1) * max0(3 - s V_cie_cache_joint_i)
      + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 37 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (24 - s V_cie_cache_joint_j) (1)]
     ((1 # 1) + s V_cie_cache_joint_z
      + (25 # 1) * max0(2 - s V_cie_cache_joint_i)
      + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 38 => ((2 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(2 - s V_cie_cache_joint_i)
            + max0(23 - s V_cie_cache_joint_j) <= z)%Q
   | 39 => ((2 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(2 - s V_cie_cache_joint_i)
            + max0(23 - s V_cie_cache_joint_j) <= z)%Q
   | 40 => ((2 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(2 - s V_cie_cache_joint_i)
            + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 41 => ((2 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(2 - s V_cie_cache_joint_i)
            + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 42 => ((2 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(2 - s V_cie_cache_joint_i)
            + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 43 => ((1 # 1) + s V_cie_cache_joint_z
            + (25 # 1) * max0(2 - s V_cie_cache_joint_i)
            + max0(24 - s V_cie_cache_joint_j) <= z)%Q
   | 44 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_cie_cache_joint_z) (0))) (F_max0_ge_0 (-
                                                                    s V_cie_cache_joint_z))]
     ((75 # 1) <= z)%Q
   | 45 => ((75 # 1) + s V_cie_cache_joint_z + max0(-s V_cie_cache_joint_z) <= z)%Q
   | 46 => ((75 # 1) + s V_cie_cache_joint_z + max0(-s V_cie_cache_joint_z) <= z)%Q
   | 47 => hints
     [(*-75 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_cie_cache_joint_z)) (F_check_ge (0) (0))]
     ((75 # 1) + s V_cie_cache_joint_z + max0(-s V_cie_cache_joint_z) <= z)%Q
   | 48 => hints
     [(*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_cie_cache_joint_z) (0))) (F_max0_ge_0 (-
                                                                    s V_cie_cache_joint_z))]
     ((75 # 1) <= z)%Q
   | 49 => ((75 # 1) + s V_cie_cache_joint_z + max0(-s V_cie_cache_joint_z) <= z)%Q
   | 50 => ((75 # 1) + s V_cie_cache_joint_z + max0(-s V_cie_cache_joint_z) <= z)%Q
   | 51 => hints
     [(*-75 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_cie_cache_joint_z)) (F_check_ge (0) (0))]
     ((75 # 1) + s V_cie_cache_joint_z + max0(-s V_cie_cache_joint_z) <= z)%Q
   | 52 => ((75 # 1) <= z)%Q
   | 53 => ((25 # 8) * max0(-1 - s V_cie_cache_joint__tmp) <= z)%Q
   | 54 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_cie_cache_joint_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_cie_cache_joint_z) (0))) (F_max0_ge_0 (-
                                                                    s V_cie_cache_joint_z));
      (*-3.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                     - s V_cie_cache_joint__tmp)) (F_check_ge (0) (0))]
     ((25 # 8) * max0(-1 - s V_cie_cache_joint__tmp) <= z)%Q
   | 55 => ((75 # 1) + max0(s V_cie_cache_joint_z) <= z)%Q
   | 56 => ((3 # 1) * max0(25 + s V_cie_cache_joint__tmp)
            + max0(s V_cie_cache_joint_z) <= z)%Q
   | 57 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_cie_cache_joint_z)) (F_check_ge (s V_cie_cache_joint_z) (0));
      (*-3 0*) F_binom_monotonic 1 (F_max0_ge_0 (25
                                                 + s V_cie_cache_joint__tmp)) (F_check_ge (0) (0))]
     ((3 # 1) * max0(25 + s V_cie_cache_joint__tmp)
      + max0(s V_cie_cache_joint_z) <= z)%Q
   | 58 => (s V_cie_cache_joint_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cie_cache_joint =>
    [mkPA Q (fun n z s => ai_cie_cache_joint n s /\ annot0_cie_cache_joint n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cie_cache_joint (proc_start P_cie_cache_joint) s1 (proc_end P_cie_cache_joint) s2 ->
    (s2 V_cie_cache_joint_z <= (75 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cie_cache_joint.
Qed.
