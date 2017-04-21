Require Import pasta.Pasta.

Inductive proc: Type :=
  P_cie_3d_table_param.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_cie_3d_table_param_z := 1%positive.
Notation V_cie_3d_table_param__tmp := 2%positive.
Notation V_cie_3d_table_param__tmp1 := 3%positive.
Notation V_cie_3d_table_param__tmp2 := 4%positive.
Notation V_cie_3d_table_param_i := 5%positive.
Notation V_cie_3d_table_param_count := 6%positive.
Notation V_cie_3d_table_param_nbytes := 7%positive.
Notation V_cie_3d_table_param_ptable := 8%positive.
Notation V_cie_3d_table_param_strings := 9%positive.
Definition Pedges_cie_3d_table_param: list (edge proc) :=
  (EA 1 (AAssign V_cie_3d_table_param_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_cie_3d_table_param_i) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard (fun s => ((eval (EVar V_cie_3d_table_param__tmp)
  s) >= (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_cie_3d_table_param__tmp (Some (EVar V_cie_3d_table_param_count))) 6)::
  (EA 6 (AAssign V_cie_3d_table_param__tmp2
  (Some (EVar V_cie_3d_table_param_nbytes))) 7)::(EA 7 AWeaken 8)::
  (EA 8 ANone 17)::(EA 8 ANone 9)::(EA 9 AWeaken 10)::(EA 10 ANone 15)::
  (EA 10 ANone 11)::(EA 11 ANone 12)::(EA 12 (AAssign
  V_cie_3d_table_param__tmp1 None) 13)::(EA 13 ANone 14)::
  (EA 14 AWeaken 51)::(EA 15 ANone 16)::(EA 16 AWeaken 51)::
  (EA 17 AWeaken 18)::(EA 18 ANone 48)::(EA 18 ANone 19)::(EA 19 (AAssign
  V_cie_3d_table_param_i (Some (ENum (0)))) 20)::(EA 20 ANone 21)::
  (EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_cie_3d_table_param_i) s) <
  (eval (EVar V_cie_3d_table_param__tmp) s))%Z)) 27)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_cie_3d_table_param_i) s) >=
  (eval (EVar V_cie_3d_table_param__tmp) s))%Z)) 23)::(EA 23 AWeaken 24)::
  (EA 24 (AAssign V_cie_3d_table_param__tmp1 (Some (ENum (0)))) 25)::
  (EA 25 ANone 26)::(EA 26 AWeaken 51)::(EA 27 AWeaken 28)::
  (EA 28 ANone 37)::(EA 28 ANone 29)::(EA 29 AWeaken 30)::(EA 30 ANone 35)::
  (EA 30 ANone 31)::(EA 31 ANone 32)::(EA 32 (AAssign
  V_cie_3d_table_param__tmp1 None) 33)::(EA 33 ANone 34)::
  (EA 34 AWeaken 51)::(EA 35 ANone 36)::(EA 36 AWeaken 51)::
  (EA 37 AWeaken 38)::(EA 38 ANone 45)::(EA 38 ANone 39)::(EA 39 ANone 40)::
  (EA 40 (AAssign V_cie_3d_table_param_i
  (Some (EAdd (EVar V_cie_3d_table_param_i) (ENum (1))))) 41)::
  (EA 41 ANone 42)::(EA 42 ANone 43)::(EA 43 (AAssign V_cie_3d_table_param_z
  (Some (EAdd (ENum (1)) (EVar V_cie_3d_table_param_z)))) 44)::
  (EA 44 AWeaken 22)::(EA 45 (AAssign V_cie_3d_table_param__tmp1
  (Some (ENum (-15)))) 46)::(EA 46 ANone 47)::(EA 47 AWeaken 51)::
  (EA 48 (AAssign V_cie_3d_table_param__tmp1 (Some (ENum (-15)))) 49)::
  (EA 49 ANone 50)::(EA 50 AWeaken 51)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_cie_3d_table_param => Pedges_cie_3d_table_param
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_cie_3d_table_param => 51
     end)%positive;
  var_global := var_global
}.

Definition ai_cie_3d_table_param (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0)%Z
   | 3 => (-1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 4 => (-1 * s V_cie_3d_table_param_i <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param__tmp <= 0)%Z
   | 5 => (-1 * s V_cie_3d_table_param__tmp <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 6 => (-1 * s V_cie_3d_table_param_i <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0)%Z
   | 7 => (-1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 8 => (-1 * s V_cie_3d_table_param_i <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0)%Z
   | 9 => (-1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 10 => (-1 * s V_cie_3d_table_param_i <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0)%Z
   | 11 => (-1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 12 => (-1 * s V_cie_3d_table_param_i <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0)%Z
   | 13 => (-1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 14 => (-1 * s V_cie_3d_table_param_i <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0)%Z
   | 15 => (-1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 16 => (-1 * s V_cie_3d_table_param_i <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0)%Z
   | 17 => (-1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 18 => (-1 * s V_cie_3d_table_param_i <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0)%Z
   | 19 => (-1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 20 => (1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 21 => (-1 * s V_cie_3d_table_param_i <= 0 /\ 1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0)%Z
   | 22 => (-1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 23 => (-1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param__tmp+ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 24 => (1 * s V_cie_3d_table_param__tmp+ -1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 25 => (-1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param__tmp+ -1 * s V_cie_3d_table_param_i <= 0 /\ 1 * s V_cie_3d_table_param__tmp1 <= 0 /\ -1 * s V_cie_3d_table_param__tmp1 <= 0)%Z
   | 26 => (-1 * s V_cie_3d_table_param__tmp1 <= 0 /\ 1 * s V_cie_3d_table_param__tmp1 <= 0 /\ 1 * s V_cie_3d_table_param__tmp+ -1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 27 => (-1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0)%Z
   | 28 => (-1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 29 => (-1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0)%Z
   | 30 => (-1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 31 => (-1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0)%Z
   | 32 => (-1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 33 => (-1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0)%Z
   | 34 => (-1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 35 => (-1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0)%Z
   | 36 => (-1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 37 => (-1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0)%Z
   | 38 => (-1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 39 => (-1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0)%Z
   | 40 => (-1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 41 => (-1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_i + 1 <= 0)%Z
   | 42 => (-1 * s V_cie_3d_table_param_i + 1 <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0)%Z
   | 43 => (-1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_i + 1 <= 0)%Z
   | 44 => (-1 * s V_cie_3d_table_param_i + 1 <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z + 1 <= 0)%Z
   | 45 => (-1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0)%Z
   | 46 => (-1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0 /\ 1 * s V_cie_3d_table_param__tmp1 + 15 <= 0 /\ -1 * s V_cie_3d_table_param__tmp1 + -15 <= 0)%Z
   | 47 => (-1 * s V_cie_3d_table_param__tmp1 + -15 <= 0 /\ 1 * s V_cie_3d_table_param__tmp1 + 15 <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param__tmp+ 1 * s V_cie_3d_table_param_i + 1 <= 0)%Z
   | 48 => (-1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 49 => (-1 * s V_cie_3d_table_param_i <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param__tmp1 + 15 <= 0 /\ -1 * s V_cie_3d_table_param__tmp1 + -15 <= 0)%Z
   | 50 => (-1 * s V_cie_3d_table_param__tmp1 + -15 <= 0 /\ 1 * s V_cie_3d_table_param__tmp1 + 15 <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0 /\ 1 * s V_cie_3d_table_param_z <= 0 /\ -1 * s V_cie_3d_table_param_i <= 0)%Z
   | 51 => (-1 * s V_cie_3d_table_param_i <= 0 /\ -1 * s V_cie_3d_table_param_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_cie_3d_table_param (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (max0(s V_cie_3d_table_param_count) <= z)%Q
   | 2 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param_count) <= z)%Q
   | 3 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param_count) <= z)%Q
   | 4 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param_count) <= z)%Q
   | 5 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param_count) <= z)%Q
   | 6 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 7 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 8 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 9 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 10 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 11 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 12 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 13 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 14 => hints
     [(*-1 0*) F_max0_ge_0 (s V_cie_3d_table_param__tmp)]
     (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 15 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 16 => hints
     [(*-1 0*) F_max0_ge_0 (s V_cie_3d_table_param__tmp)]
     (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 17 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 18 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 19 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 20 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 21 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 22 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 23 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 24 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 25 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_cie_3d_table_param__tmp
                                             - s V_cie_3d_table_param_i) (-1
                                                                    + s V_cie_3d_table_param__tmp
                                                                    - s V_cie_3d_table_param_i));
      (*-1 0*) F_max0_ge_0 (-1 + s V_cie_3d_table_param__tmp
                            - s V_cie_3d_table_param_i)]
     (s V_cie_3d_table_param_z
      + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 27 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 28 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 29 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 30 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 31 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 32 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 33 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 34 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_cie_3d_table_param__tmp
                                       - s V_cie_3d_table_param_i) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_cie_3d_table_param__tmp
                            - s V_cie_3d_table_param_i)]
     (s V_cie_3d_table_param_z
      + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 35 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 36 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_pre_decrement 1 (s V_cie_3d_table_param__tmp
                                       - s V_cie_3d_table_param_i) (1);
      (*-1 0*) F_max0_ge_0 (-1 + s V_cie_3d_table_param__tmp
                            - s V_cie_3d_table_param_i)]
     (s V_cie_3d_table_param_z
      + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 37 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_cie_3d_table_param__tmp
                                       - s V_cie_3d_table_param_i) (1)]
     (s V_cie_3d_table_param_z
      + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 38 => ((1 # 1) + s V_cie_3d_table_param_z
            + max0(-1 + s V_cie_3d_table_param__tmp
                   - s V_cie_3d_table_param_i) <= z)%Q
   | 39 => ((1 # 1) + s V_cie_3d_table_param_z
            + max0(-1 + s V_cie_3d_table_param__tmp
                   - s V_cie_3d_table_param_i) <= z)%Q
   | 40 => ((1 # 1) + s V_cie_3d_table_param_z
            + max0(-1 + s V_cie_3d_table_param__tmp
                   - s V_cie_3d_table_param_i) <= z)%Q
   | 41 => ((1 # 1) + s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 42 => ((1 # 1) + s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 43 => ((1 # 1) + s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 44 => (s V_cie_3d_table_param_z
            + max0(s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i) <= z)%Q
   | 45 => ((1 # 1) + s V_cie_3d_table_param_z
            + max0(-1 + s V_cie_3d_table_param__tmp
                   - s V_cie_3d_table_param_i) <= z)%Q
   | 46 => (s V_cie_3d_table_param_z
            + max0(-1 + s V_cie_3d_table_param__tmp
                   - s V_cie_3d_table_param_i)
            + (1 # 15) * max0(-s V_cie_3d_table_param__tmp1) <= z)%Q
   | 47 => hints
     [(*-1 0*) F_max0_ge_0 (-1 + s V_cie_3d_table_param__tmp
                            - s V_cie_3d_table_param_i);
      (*-0.0666667 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_cie_3d_table_param__tmp1)) (F_check_ge (0) (0))]
     (s V_cie_3d_table_param_z
      + max0(-1 + s V_cie_3d_table_param__tmp - s V_cie_3d_table_param_i)
      + (1 # 15) * max0(-s V_cie_3d_table_param__tmp1) <= z)%Q
   | 48 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 49 => (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 50 => hints
     [(*-1 0*) F_max0_ge_0 (s V_cie_3d_table_param__tmp)]
     (s V_cie_3d_table_param_z + max0(s V_cie_3d_table_param__tmp) <= z)%Q
   | 51 => (s V_cie_3d_table_param_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_cie_3d_table_param =>
    [mkPA Q (fun n z s => ai_cie_3d_table_param n s /\ annot0_cie_3d_table_param n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_cie_3d_table_param (proc_start P_cie_3d_table_param) s1 (proc_end P_cie_3d_table_param) s2 ->
    (s2 V_cie_3d_table_param_z <= max0(s1 V_cie_3d_table_param_count))%Q.
Proof.
  prove_bound ipa admissible_ipa P_cie_3d_table_param.
Qed.
