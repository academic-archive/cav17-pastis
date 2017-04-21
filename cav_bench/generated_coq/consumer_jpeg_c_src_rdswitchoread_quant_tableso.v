Require Import pasta.Pasta.

Inductive proc: Type :=
  P_read_quant_tables.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_read_quant_tables_z := 1%positive.
Notation V_read_quant_tables__tmp := 2%positive.
Notation V_read_quant_tables__tmp1 := 3%positive.
Notation V_read_quant_tables__tmp2 := 4%positive.
Notation V_read_quant_tables_i := 5%positive.
Notation V_read_quant_tables_tblno := 6%positive.
Notation V_read_quant_tables_cinfo := 7%positive.
Notation V_read_quant_tables_filename := 8%positive.
Notation V_read_quant_tables_force_baseline := 9%positive.
Notation V_read_quant_tables_scale_factor := 10%positive.
Definition Pedges_read_quant_tables: list (edge proc) :=
  (EA 1 (AAssign V_read_quant_tables_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_read_quant_tables__tmp2
  (Some (EVar V_read_quant_tables_scale_factor))) 3)::(EA 3 (AAssign
  V_read_quant_tables__tmp1
  (Some (EVar V_read_quant_tables_force_baseline))) 4)::(EA 4 AWeaken 5)::
  (EA 5 ANone 46)::(EA 5 ANone 6)::(EA 6 (AAssign V_read_quant_tables_tblno
  (Some (ENum (0)))) 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::(EA 9 ANone 18)::
  (EA 9 ANone 10)::(EA 10 AWeaken 11)::(EA 11 ANone 15)::(EA 11 ANone 12)::
  (EA 12 (AAssign V_read_quant_tables__tmp (Some (ENum (1)))) 13)::
  (EA 13 ANone 14)::(EA 14 AWeaken 49)::(EA 15 (AAssign
  V_read_quant_tables__tmp (Some (ENum (0)))) 16)::(EA 16 ANone 17)::
  (EA 17 AWeaken 49)::(EA 18 AWeaken 19)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_read_quant_tables_tblno) s) >= (eval (ENum (4))
  s))%Z)) 42)::(EA 19 (AGuard
  (fun s => ((eval (EVar V_read_quant_tables_tblno) s) < (eval (ENum (4))
  s))%Z)) 20)::(EA 20 AWeaken 21)::(EA 21 (AAssign V_read_quant_tables_i
  (Some (ENum (1)))) 22)::(EA 22 ANone 23)::(EA 23 AWeaken 24)::
  (EA 24 (AGuard (fun s => ((eval (EVar V_read_quant_tables_i) s) <
  (eval (ENum (64)) s))%Z)) 31)::(EA 24 (AGuard
  (fun s => ((eval (EVar V_read_quant_tables_i) s) >= (eval (ENum (64))
  s))%Z)) 25)::(EA 25 AWeaken 26)::(EA 26 (AAssign V_read_quant_tables_tblno
  (Some (EAdd (EVar V_read_quant_tables_tblno) (ENum (1))))) 27)::
  (EA 27 ANone 28)::(EA 28 ANone 29)::(EA 29 (AAssign V_read_quant_tables_z
  (Some (EAdd (ENum (1)) (EVar V_read_quant_tables_z)))) 30)::
  (EA 30 AWeaken 9)::(EA 31 AWeaken 32)::(EA 32 ANone 36)::(EA 32 ANone 33)::
  (EA 33 (AAssign V_read_quant_tables__tmp (Some (ENum (0)))) 34)::
  (EA 34 ANone 35)::(EA 35 AWeaken 49)::(EA 36 ANone 37)::(EA 37 (AAssign
  V_read_quant_tables_i (Some (EAdd (EVar V_read_quant_tables_i)
  (ENum (1))))) 38)::(EA 38 ANone 39)::(EA 39 ANone 40)::(EA 40 (AAssign
  V_read_quant_tables_z (Some (EAdd (ENum (1))
  (EVar V_read_quant_tables_z)))) 41)::(EA 41 AWeaken 24)::
  (EA 42 AWeaken 43)::(EA 43 (AAssign V_read_quant_tables__tmp
  (Some (ENum (0)))) 44)::(EA 44 ANone 45)::(EA 45 AWeaken 49)::
  (EA 46 (AAssign V_read_quant_tables__tmp (Some (ENum (0)))) 47)::
  (EA 47 ANone 48)::(EA 48 AWeaken 49)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_read_quant_tables => Pedges_read_quant_tables
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_read_quant_tables => 49
     end)%positive;
  var_global := var_global
}.

Definition ai_read_quant_tables (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 3 => (-1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_z <= 0)%Z
   | 4 => (1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 5 => (-1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_z <= 0)%Z
   | 6 => (1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 7 => (-1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0)%Z
   | 8 => (-1 * s V_read_quant_tables_tblno <= 0 /\ 1 * s V_read_quant_tables_tblno <= 0 /\ 1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 9 => (-1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0)%Z
   | 10 => (-1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 11 => (-1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0)%Z
   | 12 => (-1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 13 => (-1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0 /\ 1 * s V_read_quant_tables__tmp + -1 <= 0 /\ -1 * s V_read_quant_tables__tmp + 1 <= 0)%Z
   | 14 => (-1 * s V_read_quant_tables__tmp + 1 <= 0 /\ 1 * s V_read_quant_tables__tmp + -1 <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 15 => (-1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 16 => (-1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0 /\ 1 * s V_read_quant_tables__tmp <= 0 /\ -1 * s V_read_quant_tables__tmp <= 0)%Z
   | 17 => (-1 * s V_read_quant_tables__tmp <= 0 /\ 1 * s V_read_quant_tables__tmp <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 18 => (-1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 19 => (-1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0)%Z
   | 20 => (-1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_tblno + -3 <= 0)%Z
   | 21 => (1 * s V_read_quant_tables_tblno + -3 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0)%Z
   | 22 => (-1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_tblno + -3 <= 0 /\ 1 * s V_read_quant_tables_i + -1 <= 0 /\ -1 * s V_read_quant_tables_i + 1 <= 0)%Z
   | 23 => (-1 * s V_read_quant_tables_i + 1 <= 0 /\ 1 * s V_read_quant_tables_i + -1 <= 0 /\ 1 * s V_read_quant_tables_tblno + -3 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0)%Z
   | 24 => (-1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_i + 1 <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0 /\ 1 * s V_read_quant_tables_i + -64 <= 0)%Z
   | 25 => (1 * s V_read_quant_tables_i + -64 <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_i + 64 <= 0)%Z
   | 26 => (-1 * s V_read_quant_tables_i + 64 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0 /\ 1 * s V_read_quant_tables_i + -64 <= 0)%Z
   | 27 => (1 * s V_read_quant_tables_i + -64 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_i + 64 <= 0 /\ -1 * s V_read_quant_tables_tblno + 1 <= 0)%Z
   | 28 => (-1 * s V_read_quant_tables_tblno + 1 <= 0 /\ -1 * s V_read_quant_tables_i + 64 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_i + -64 <= 0)%Z
   | 29 => (1 * s V_read_quant_tables_i + -64 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_i + 64 <= 0 /\ -1 * s V_read_quant_tables_tblno + 1 <= 0)%Z
   | 30 => (-1 * s V_read_quant_tables_tblno + 1 <= 0 /\ -1 * s V_read_quant_tables_i + 64 <= 0 /\ 1 * s V_read_quant_tables_i + -64 <= 0 /\ -1 * s V_read_quant_tables_z + 1 <= 0)%Z
   | 31 => (-1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_i + 1 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_i + -63 <= 0)%Z
   | 32 => (1 * s V_read_quant_tables_i + -63 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_i + 1 <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0)%Z
   | 33 => (-1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_i + 1 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_i + -63 <= 0)%Z
   | 34 => (1 * s V_read_quant_tables_i + -63 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_i + 1 <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0 /\ 1 * s V_read_quant_tables__tmp <= 0 /\ -1 * s V_read_quant_tables__tmp <= 0)%Z
   | 35 => (-1 * s V_read_quant_tables__tmp <= 0 /\ 1 * s V_read_quant_tables__tmp <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_i + 1 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_i + -63 <= 0)%Z
   | 36 => (-1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_i + 1 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_i + -63 <= 0)%Z
   | 37 => (1 * s V_read_quant_tables_i + -63 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_i + 1 <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0)%Z
   | 38 => (-1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_i + -64 <= 0 /\ -1 * s V_read_quant_tables_i + 2 <= 0)%Z
   | 39 => (-1 * s V_read_quant_tables_i + 2 <= 0 /\ 1 * s V_read_quant_tables_i + -64 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0)%Z
   | 40 => (-1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_i + -64 <= 0 /\ -1 * s V_read_quant_tables_i + 2 <= 0)%Z
   | 41 => (-1 * s V_read_quant_tables_i + 2 <= 0 /\ 1 * s V_read_quant_tables_i + -64 <= 0 /\ -1 * s V_read_quant_tables_tblno <= 0 /\ -1 * s V_read_quant_tables_z + 1 <= 0)%Z
   | 42 => (-1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_tblno + 4 <= 0)%Z
   | 43 => (-1 * s V_read_quant_tables_tblno + 4 <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 44 => (-1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_tblno + 4 <= 0 /\ 1 * s V_read_quant_tables__tmp <= 0 /\ -1 * s V_read_quant_tables__tmp <= 0)%Z
   | 45 => (-1 * s V_read_quant_tables__tmp <= 0 /\ 1 * s V_read_quant_tables__tmp <= 0 /\ -1 * s V_read_quant_tables_tblno + 4 <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 46 => (1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 47 => (-1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables_z <= 0 /\ 1 * s V_read_quant_tables__tmp <= 0 /\ -1 * s V_read_quant_tables__tmp <= 0)%Z
   | 48 => (-1 * s V_read_quant_tables__tmp <= 0 /\ 1 * s V_read_quant_tables__tmp <= 0 /\ 1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables_z <= 0)%Z
   | 49 => (1 * s V_read_quant_tables__tmp + -1 <= 0 /\ -1 * s V_read_quant_tables_z <= 0 /\ -1 * s V_read_quant_tables__tmp <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_read_quant_tables (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((256 # 1) <= z)%Q
   | 2 => ((256 # 1) + s V_read_quant_tables_z <= z)%Q
   | 3 => ((256 # 1) + s V_read_quant_tables_z <= z)%Q
   | 4 => ((256 # 1) + s V_read_quant_tables_z <= z)%Q
   | 5 => ((256 # 1) + s V_read_quant_tables_z <= z)%Q
   | 6 => ((256 # 1) + s V_read_quant_tables_z <= z)%Q
   | 7 => (s V_read_quant_tables_z
           + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 8 => (s V_read_quant_tables_z
           + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 9 => (s V_read_quant_tables_z
           + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 10 => (s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 11 => (s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 12 => (s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 13 => (s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 14 => hints
     [(*-64 0*) F_max0_monotonic (F_check_ge (4 - s V_read_quant_tables_tblno) (3
                                                                    - s V_read_quant_tables_tblno));
      (*-64 0*) F_max0_ge_0 (3 - s V_read_quant_tables_tblno)]
     (s V_read_quant_tables_z
      + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 15 => (s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 16 => (s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 17 => hints
     [(*-64 0*) F_max0_monotonic (F_check_ge (4 - s V_read_quant_tables_tblno) (3
                                                                    - s V_read_quant_tables_tblno));
      (*-64 0*) F_max0_ge_0 (3 - s V_read_quant_tables_tblno)]
     (s V_read_quant_tables_z
      + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 18 => (s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 19 => (s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 20 => hints
     [(*0 64*) F_max0_pre_decrement 1 (4 - s V_read_quant_tables_tblno) (1)]
     (s V_read_quant_tables_z
      + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 21 => ((64 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno) <= z)%Q
   | 22 => ((1 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 23 => ((1 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 24 => ((1 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 25 => ((1 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 26 => ((1 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 27 => ((1 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno)
            + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 28 => ((1 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno)
            + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 29 => ((1 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno)
            + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 30 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (64 - s V_read_quant_tables_i)) (F_check_ge (0) (0))]
     (s V_read_quant_tables_z
      + (64 # 1) * max0(4 - s V_read_quant_tables_tblno)
      + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 31 => hints
     [(*0 1*) F_max0_pre_decrement 1 (64 - s V_read_quant_tables_i) (1)]
     ((1 # 1) + s V_read_quant_tables_z
      + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
      + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 32 => ((2 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(63 - s V_read_quant_tables_i) <= z)%Q
   | 33 => ((2 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(63 - s V_read_quant_tables_i) <= z)%Q
   | 34 => ((2 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(63 - s V_read_quant_tables_i) <= z)%Q
   | 35 => hints
     [(*-64 0*) F_max0_ge_0 (3 - s V_read_quant_tables_tblno);
      (*-1.03226 0*) F_max0_ge_0 (63 - s V_read_quant_tables_i);
      (*-0.0322581 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (63
                                                                    - s V_read_quant_tables_i) (0))) (F_max0_ge_0 (63
                                                                    - s V_read_quant_tables_i));
      (*-0.0322581 0*) F_binom_monotonic 1 (F_max0_ge_0 (-1
                                                         + s V_read_quant_tables_i)) (F_check_ge (0) (0));
      (*-0.0322581 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                    + s V_read_quant_tables_i) (0))) (F_max0_ge_0 (-1
                                                                    + s V_read_quant_tables_i))]
     ((2 # 1) + s V_read_quant_tables_z
      + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
      + max0(63 - s V_read_quant_tables_i) <= z)%Q
   | 36 => ((2 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(63 - s V_read_quant_tables_i) <= z)%Q
   | 37 => ((2 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(63 - s V_read_quant_tables_i) <= z)%Q
   | 38 => ((2 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 39 => ((2 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 40 => ((2 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 41 => ((1 # 1) + s V_read_quant_tables_z
            + (64 # 1) * max0(3 - s V_read_quant_tables_tblno)
            + max0(64 - s V_read_quant_tables_i) <= z)%Q
   | 42 => (s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 43 => (s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 44 => (s V_read_quant_tables_z
            + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 45 => hints
     [(*-64 0*) F_max0_monotonic (F_check_ge (4 - s V_read_quant_tables_tblno) (3
                                                                    - s V_read_quant_tables_tblno));
      (*-64 0*) F_max0_ge_0 (3 - s V_read_quant_tables_tblno)]
     (s V_read_quant_tables_z
      + (64 # 1) * max0(4 - s V_read_quant_tables_tblno) <= z)%Q
   | 46 => ((256 # 1) + s V_read_quant_tables_z <= z)%Q
   | 47 => ((256 # 1) + s V_read_quant_tables_z <= z)%Q
   | 48 => hints
     [(*-256 0*) F_one]
     ((256 # 1) + s V_read_quant_tables_z <= z)%Q
   | 49 => (s V_read_quant_tables_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_read_quant_tables =>
    [mkPA Q (fun n z s => ai_read_quant_tables n s /\ annot0_read_quant_tables n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_read_quant_tables (proc_start P_read_quant_tables) s1 (proc_end P_read_quant_tables) s2 ->
    (s2 V_read_quant_tables_z <= (256 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_read_quant_tables.
Qed.
