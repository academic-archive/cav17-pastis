Require Import pasta.Pasta.

Inductive proc: Type :=
  P_BZ2_hbAssignCodes.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_BZ2_hbAssignCodes_z := 1%positive.
Notation V_BZ2_hbAssignCodes__tmp := 2%positive.
Notation V_BZ2_hbAssignCodes__tmp1 := 3%positive.
Notation V_BZ2_hbAssignCodes__tmp2 := 4%positive.
Notation V_BZ2_hbAssignCodes_i := 5%positive.
Notation V_BZ2_hbAssignCodes_n := 6%positive.
Notation V_BZ2_hbAssignCodes_vec := 7%positive.
Notation V_BZ2_hbAssignCodes_alphaSize := 8%positive.
Notation V_BZ2_hbAssignCodes_code := 9%positive.
Notation V_BZ2_hbAssignCodes_length := 10%positive.
Notation V_BZ2_hbAssignCodes_maxLen := 11%positive.
Notation V_BZ2_hbAssignCodes_minLen := 12%positive.
Definition Pedges_BZ2_hbAssignCodes: list (edge proc) :=
  (EA 1 (AAssign V_BZ2_hbAssignCodes_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_BZ2_hbAssignCodes__tmp2 (Some (EVar V_BZ2_hbAssignCodes_minLen))) 3)::
  (EA 3 (AAssign V_BZ2_hbAssignCodes__tmp
  (Some (EVar V_BZ2_hbAssignCodes_maxLen))) 4)::(EA 4 (AAssign
  V_BZ2_hbAssignCodes__tmp1 (Some (EVar V_BZ2_hbAssignCodes_alphaSize))) 5)::
  (EA 5 (AAssign V_BZ2_hbAssignCodes_vec (Some (ENum (0)))) 6)::
  (EA 6 (AAssign V_BZ2_hbAssignCodes_n
  (Some (EVar V_BZ2_hbAssignCodes__tmp2))) 7)::(EA 7 ANone 8)::
  (EA 8 AWeaken 9)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbAssignCodes_n) s) <=
  (eval (EVar V_BZ2_hbAssignCodes__tmp) s))%Z)) 12)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbAssignCodes_n) s) >
  (eval (EVar V_BZ2_hbAssignCodes__tmp) s))%Z)) 10)::(EA 10 AWeaken 11)::
  (EA 12 AWeaken 13)::(EA 13 (AAssign V_BZ2_hbAssignCodes_i
  (Some (ENum (0)))) 14)::(EA 14 ANone 15)::(EA 15 AWeaken 16)::
  (EA 16 (AGuard (fun s => ((eval (EVar V_BZ2_hbAssignCodes_i) s) <
  (eval (EVar V_BZ2_hbAssignCodes__tmp1) s))%Z)) 25)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_BZ2_hbAssignCodes_i) s) >=
  (eval (EVar V_BZ2_hbAssignCodes__tmp1) s))%Z)) 17)::(EA 17 AWeaken 18)::
  (EA 18 (AAssign V_BZ2_hbAssignCodes_vec None) 19)::(EA 19 ANone 20)::
  (EA 20 (AAssign V_BZ2_hbAssignCodes_n
  (Some (EAdd (EVar V_BZ2_hbAssignCodes_n) (ENum (1))))) 21)::
  (EA 21 ANone 22)::(EA 22 ANone 23)::(EA 23 (AAssign V_BZ2_hbAssignCodes_z
  (Some (EAdd (ENum (1)) (EVar V_BZ2_hbAssignCodes_z)))) 24)::
  (EA 24 AWeaken 9)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::(EA 26 ANone 29)::
  (EA 27 (AAssign V_BZ2_hbAssignCodes_vec
  (Some (EAdd (EVar V_BZ2_hbAssignCodes_vec) (ENum (1))))) 28)::
  (EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign V_BZ2_hbAssignCodes_i
  (Some (EAdd (EVar V_BZ2_hbAssignCodes_i) (ENum (1))))) 31)::
  (EA 31 ANone 32)::(EA 32 ANone 33)::(EA 33 (AAssign V_BZ2_hbAssignCodes_z
  (Some (EAdd (ENum (1)) (EVar V_BZ2_hbAssignCodes_z)))) 34)::
  (EA 34 AWeaken 16)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_BZ2_hbAssignCodes => Pedges_BZ2_hbAssignCodes
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_BZ2_hbAssignCodes => 11
     end)%positive;
  var_global := var_global
}.

Definition ai_BZ2_hbAssignCodes (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0)%Z
   | 3 => (-1 * s V_BZ2_hbAssignCodes_z <= 0 /\ 1 * s V_BZ2_hbAssignCodes_z <= 0)%Z
   | 4 => (1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0)%Z
   | 5 => (-1 * s V_BZ2_hbAssignCodes_z <= 0 /\ 1 * s V_BZ2_hbAssignCodes_z <= 0)%Z
   | 6 => (1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ 1 * s V_BZ2_hbAssignCodes_vec <= 0 /\ -1 * s V_BZ2_hbAssignCodes_vec <= 0)%Z
   | 7 => (-1 * s V_BZ2_hbAssignCodes_vec <= 0 /\ 1 * s V_BZ2_hbAssignCodes_vec <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ 1 * s V_BZ2_hbAssignCodes_z <= 0)%Z
   | 8 => (1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ 1 * s V_BZ2_hbAssignCodes_vec <= 0 /\ -1 * s V_BZ2_hbAssignCodes_vec <= 0)%Z
   | 9 => (-1 * s V_BZ2_hbAssignCodes_z <= 0)%Z
   | 10 => (-1 * s V_BZ2_hbAssignCodes_z <= 0 /\ 1 * s V_BZ2_hbAssignCodes__tmp+ -1 * s V_BZ2_hbAssignCodes_n + 1 <= 0)%Z
   | 11 => (1 * s V_BZ2_hbAssignCodes__tmp+ -1 * s V_BZ2_hbAssignCodes_n + 1 <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0)%Z
   | 12 => (-1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0)%Z
   | 13 => (-1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0)%Z
   | 14 => (-1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0 /\ 1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0)%Z
   | 15 => (-1 * s V_BZ2_hbAssignCodes_i <= 0 /\ 1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0)%Z
   | 16 => (-1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0)%Z
   | 17 => (-1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ 1 * s V_BZ2_hbAssignCodes__tmp1+ -1 * s V_BZ2_hbAssignCodes_i <= 0)%Z
   | 18 => (1 * s V_BZ2_hbAssignCodes__tmp1+ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0)%Z
   | 19 => (-1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ 1 * s V_BZ2_hbAssignCodes__tmp1+ -1 * s V_BZ2_hbAssignCodes_i <= 0)%Z
   | 20 => (1 * s V_BZ2_hbAssignCodes__tmp1+ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0)%Z
   | 21 => (-1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ 1 * s V_BZ2_hbAssignCodes__tmp1+ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n + -1 <= 0)%Z
   | 22 => (-1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n + -1 <= 0 /\ 1 * s V_BZ2_hbAssignCodes__tmp1+ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0)%Z
   | 23 => (-1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ 1 * s V_BZ2_hbAssignCodes__tmp1+ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n + -1 <= 0)%Z
   | 24 => (-1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n + -1 <= 0 /\ 1 * s V_BZ2_hbAssignCodes__tmp1+ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z + 1 <= 0)%Z
   | 25 => (-1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp1+ 1 * s V_BZ2_hbAssignCodes_i + 1 <= 0)%Z
   | 26 => (-1 * s V_BZ2_hbAssignCodes__tmp1+ 1 * s V_BZ2_hbAssignCodes_i + 1 <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0)%Z
   | 27 => (-1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp1+ 1 * s V_BZ2_hbAssignCodes_i + 1 <= 0)%Z
   | 28 => (-1 * s V_BZ2_hbAssignCodes__tmp1+ 1 * s V_BZ2_hbAssignCodes_i + 1 <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0)%Z
   | 29 => (-1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp1+ 1 * s V_BZ2_hbAssignCodes_i + 1 <= 0)%Z
   | 30 => (-1 * s V_BZ2_hbAssignCodes__tmp1+ 1 * s V_BZ2_hbAssignCodes_i + 1 <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0)%Z
   | 31 => (-1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp1+ 1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i + 1 <= 0)%Z
   | 32 => (-1 * s V_BZ2_hbAssignCodes_i + 1 <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp1+ 1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0)%Z
   | 33 => (-1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp1+ 1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes_i + 1 <= 0)%Z
   | 34 => (-1 * s V_BZ2_hbAssignCodes_i + 1 <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp1+ 1 * s V_BZ2_hbAssignCodes_i <= 0 /\ -1 * s V_BZ2_hbAssignCodes__tmp+ 1 * s V_BZ2_hbAssignCodes_n <= 0 /\ -1 * s V_BZ2_hbAssignCodes_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_BZ2_hbAssignCodes (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(1 + s V_BZ2_hbAssignCodes_maxLen
                - s V_BZ2_hbAssignCodes_minLen)
           + max0(1 + s V_BZ2_hbAssignCodes_maxLen
                  - s V_BZ2_hbAssignCodes_minLen) * max0(s V_BZ2_hbAssignCodes_alphaSize) <= z)%Q
   | 2 => (s V_BZ2_hbAssignCodes_z
           + max0(1 + s V_BZ2_hbAssignCodes_maxLen
                  - s V_BZ2_hbAssignCodes_minLen)
           + max0(1 + s V_BZ2_hbAssignCodes_maxLen
                  - s V_BZ2_hbAssignCodes_minLen) * max0(s V_BZ2_hbAssignCodes_alphaSize) <= z)%Q
   | 3 => (s V_BZ2_hbAssignCodes_z
           + max0(1 - s V_BZ2_hbAssignCodes__tmp2
                  + s V_BZ2_hbAssignCodes_maxLen)
           + max0(1 - s V_BZ2_hbAssignCodes__tmp2
                  + s V_BZ2_hbAssignCodes_maxLen) * max0(s V_BZ2_hbAssignCodes_alphaSize) <= z)%Q
   | 4 => (s V_BZ2_hbAssignCodes_z
           + max0(1 + s V_BZ2_hbAssignCodes__tmp
                  - s V_BZ2_hbAssignCodes__tmp2)
           + max0(1 + s V_BZ2_hbAssignCodes__tmp
                  - s V_BZ2_hbAssignCodes__tmp2) * max0(s V_BZ2_hbAssignCodes_alphaSize) <= z)%Q
   | 5 => (s V_BZ2_hbAssignCodes_z
           + max0(1 + s V_BZ2_hbAssignCodes__tmp
                  - s V_BZ2_hbAssignCodes__tmp2)
           + max0(1 + s V_BZ2_hbAssignCodes__tmp
                  - s V_BZ2_hbAssignCodes__tmp2) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 6 => (s V_BZ2_hbAssignCodes_z
           + max0(1 + s V_BZ2_hbAssignCodes__tmp
                  - s V_BZ2_hbAssignCodes__tmp2)
           + max0(1 + s V_BZ2_hbAssignCodes__tmp
                  - s V_BZ2_hbAssignCodes__tmp2) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 7 => (s V_BZ2_hbAssignCodes_z
           + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
           + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 8 => (s V_BZ2_hbAssignCodes_z
           + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
           + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 9 => (s V_BZ2_hbAssignCodes_z
           + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
           + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (1 + s V_BZ2_hbAssignCodes__tmp
                                             - s V_BZ2_hbAssignCodes_n) (s V_BZ2_hbAssignCodes__tmp
                                                                    - s V_BZ2_hbAssignCodes_n));
      (*-1 0*) F_max0_ge_0 (s V_BZ2_hbAssignCodes__tmp
                            - s V_BZ2_hbAssignCodes_n);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_0 (1
                                                            + s V_BZ2_hbAssignCodes__tmp
                                                            - s V_BZ2_hbAssignCodes_n)) (F_check_ge (0) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbAssignCodes__tmp1)) (F_check_ge (0) (0)))]
     (s V_BZ2_hbAssignCodes_z
      + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
      + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 11 => (s V_BZ2_hbAssignCodes_z <= z)%Q
   | 12 => (s V_BZ2_hbAssignCodes_z
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 13 => (s V_BZ2_hbAssignCodes_z
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 14 => (s V_BZ2_hbAssignCodes_z
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
            - max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 15 => (s V_BZ2_hbAssignCodes_z
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
            - max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 16 => (s V_BZ2_hbAssignCodes_z
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
            - max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 17 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (1 + s V_BZ2_hbAssignCodes__tmp
                                       - s V_BZ2_hbAssignCodes_n) (1);
      (*-1 0*) F_max0_ge_0 (s V_BZ2_hbAssignCodes__tmp1
                            - s V_BZ2_hbAssignCodes_i);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                              + s V_BZ2_hbAssignCodes__tmp
                                                              - s V_BZ2_hbAssignCodes_n)) (F_check_ge (1
                                                                    + s V_BZ2_hbAssignCodes__tmp
                                                                    - s V_BZ2_hbAssignCodes_n) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)) (F_check_ge (0) (0)))]
     (s V_BZ2_hbAssignCodes_z
      + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
      + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)
      + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
      - max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 18 => ((1 # 1)
            + s V_BZ2_hbAssignCodes__tmp * max0(s V_BZ2_hbAssignCodes__tmp1
                                                - s V_BZ2_hbAssignCodes_i)
            - s V_BZ2_hbAssignCodes_n * max0(s V_BZ2_hbAssignCodes__tmp1
                                             - s V_BZ2_hbAssignCodes_i)
            + s V_BZ2_hbAssignCodes_z
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
            - max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 19 => ((1 # 1)
            + s V_BZ2_hbAssignCodes__tmp * max0(s V_BZ2_hbAssignCodes__tmp1
                                                - s V_BZ2_hbAssignCodes_i)
            - s V_BZ2_hbAssignCodes_n * max0(s V_BZ2_hbAssignCodes__tmp1
                                             - s V_BZ2_hbAssignCodes_i)
            + s V_BZ2_hbAssignCodes_z
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
            - max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 20 => ((1 # 1)
            + s V_BZ2_hbAssignCodes__tmp * max0(s V_BZ2_hbAssignCodes__tmp1
                                                - s V_BZ2_hbAssignCodes_i)
            - s V_BZ2_hbAssignCodes_n * max0(s V_BZ2_hbAssignCodes__tmp1
                                             - s V_BZ2_hbAssignCodes_i)
            + s V_BZ2_hbAssignCodes_z
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
            - max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 21 => ((1 # 1)
            + s V_BZ2_hbAssignCodes__tmp * max0(s V_BZ2_hbAssignCodes__tmp1
                                                - s V_BZ2_hbAssignCodes_i)
            - s V_BZ2_hbAssignCodes_n * max0(s V_BZ2_hbAssignCodes__tmp1
                                             - s V_BZ2_hbAssignCodes_i)
            + s V_BZ2_hbAssignCodes_z
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
            - max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)
            + max0(s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 22 => ((1 # 1)
            + s V_BZ2_hbAssignCodes__tmp * max0(s V_BZ2_hbAssignCodes__tmp1
                                                - s V_BZ2_hbAssignCodes_i)
            - s V_BZ2_hbAssignCodes_n * max0(s V_BZ2_hbAssignCodes__tmp1
                                             - s V_BZ2_hbAssignCodes_i)
            + s V_BZ2_hbAssignCodes_z
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
            - max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)
            + max0(s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 23 => ((1 # 1)
            + s V_BZ2_hbAssignCodes__tmp * max0(s V_BZ2_hbAssignCodes__tmp1
                                                - s V_BZ2_hbAssignCodes_i)
            - s V_BZ2_hbAssignCodes_n * max0(s V_BZ2_hbAssignCodes__tmp1
                                             - s V_BZ2_hbAssignCodes_i)
            + s V_BZ2_hbAssignCodes_z
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
            - max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)
            + max0(s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 24 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + s V_BZ2_hbAssignCodes__tmp
                                                                    - s V_BZ2_hbAssignCodes_n) (0))) (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbAssignCodes__tmp
                                                                    - s V_BZ2_hbAssignCodes_n))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)) (F_check_ge (0) (0)))]
     (s V_BZ2_hbAssignCodes__tmp * max0(s V_BZ2_hbAssignCodes__tmp1
                                        - s V_BZ2_hbAssignCodes_i)
      - s V_BZ2_hbAssignCodes_n * max0(s V_BZ2_hbAssignCodes__tmp1
                                       - s V_BZ2_hbAssignCodes_i)
      + s V_BZ2_hbAssignCodes_z
      + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
      + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
      - max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)
      + max0(s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 25 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_BZ2_hbAssignCodes__tmp1
                                       - s V_BZ2_hbAssignCodes_i) (1);
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (1
                                                              + s V_BZ2_hbAssignCodes__tmp
                                                              - s V_BZ2_hbAssignCodes_n)) (F_check_ge (1
                                                                    + s V_BZ2_hbAssignCodes__tmp
                                                                    - s V_BZ2_hbAssignCodes_n) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BZ2_hbAssignCodes__tmp
                                                                    - s V_BZ2_hbAssignCodes_n) (0))) (F_max0_ge_0 (s V_BZ2_hbAssignCodes__tmp
                                                                    - s V_BZ2_hbAssignCodes_n))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)) (F_check_ge (0) (0)))]
     (s V_BZ2_hbAssignCodes_z
      + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
      + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)
      + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
      - max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 26 => ((1 # 1) + s V_BZ2_hbAssignCodes_z
            + max0(-1 + s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 27 => ((1 # 1) + s V_BZ2_hbAssignCodes_z
            + max0(-1 + s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 28 => ((1 # 1) + s V_BZ2_hbAssignCodes_z
            + max0(-1 + s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 29 => ((1 # 1) + s V_BZ2_hbAssignCodes_z
            + max0(-1 + s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 30 => ((1 # 1) + s V_BZ2_hbAssignCodes_z
            + max0(-1 + s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i)
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1) <= z)%Q
   | 31 => ((1 # 1) + s V_BZ2_hbAssignCodes_z
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
            + max0(s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 32 => ((1 # 1) + s V_BZ2_hbAssignCodes_z
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
            + max0(s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 33 => ((1 # 1) + s V_BZ2_hbAssignCodes_z
            + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
            + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
            + max0(s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | 34 => hints
     [(*-1 0*) F_product (F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (1
                                                                    + s V_BZ2_hbAssignCodes__tmp
                                                                    - s V_BZ2_hbAssignCodes_n) (0))) (F_max0_ge_0 (1
                                                                    + s V_BZ2_hbAssignCodes__tmp
                                                                    - s V_BZ2_hbAssignCodes_n))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)) (F_check_ge (0) (0)));
      (*-1 0*) F_product (F_binom_monotonic 1 (F_max0_ge_arg (s V_BZ2_hbAssignCodes__tmp
                                                              - s V_BZ2_hbAssignCodes_n)) (F_check_ge (s V_BZ2_hbAssignCodes__tmp
                                                                    - s V_BZ2_hbAssignCodes_n) (0))) (F_binom_monotonic 1 (F_max0_ge_0 (s V_BZ2_hbAssignCodes__tmp1
                                                                    - s V_BZ2_hbAssignCodes_i)) (F_check_ge (0) (0)))]
     (s V_BZ2_hbAssignCodes_z
      + max0(1 + s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n)
      + max0(s V_BZ2_hbAssignCodes__tmp - s V_BZ2_hbAssignCodes_n) * max0(s V_BZ2_hbAssignCodes__tmp1)
      + max0(s V_BZ2_hbAssignCodes__tmp1 - s V_BZ2_hbAssignCodes_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_BZ2_hbAssignCodes =>
    [mkPA Q (fun n z s => ai_BZ2_hbAssignCodes n s /\ annot0_BZ2_hbAssignCodes n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_BZ2_hbAssignCodes (proc_start P_BZ2_hbAssignCodes) s1 (proc_end P_BZ2_hbAssignCodes) s2 ->
    (s2 V_BZ2_hbAssignCodes_z <= max0(1 + s1 V_BZ2_hbAssignCodes_maxLen
                                      - s1 V_BZ2_hbAssignCodes_minLen)
                                 + max0(1 + s1 V_BZ2_hbAssignCodes_maxLen
                                        - s1 V_BZ2_hbAssignCodes_minLen) * max0(s1 V_BZ2_hbAssignCodes_alphaSize))%Q.
Proof.
  prove_bound ipa admissible_ipa P_BZ2_hbAssignCodes.
Qed.
