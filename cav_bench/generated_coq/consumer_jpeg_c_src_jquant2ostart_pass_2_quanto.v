Require Import pasta.Pasta.

Inductive proc: Type :=
  P_start_pass_2_quant.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_start_pass_2_quant_z := 1%positive.
Notation V_start_pass_2_quant__tmp := 2%positive.
Notation V_start_pass_2_quant_arraysize := 3%positive.
Notation V_start_pass_2_quant_i := 4%positive.
Notation V_start_pass_2_quant_cinfo := 5%positive.
Notation V_start_pass_2_quant_is_pre_scan := 6%positive.
Definition Pedges_start_pass_2_quant: list (edge proc) :=
  (EA 1 (AAssign V_start_pass_2_quant_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_start_pass_2_quant__tmp
  (Some (EVar V_start_pass_2_quant_is_pre_scan))) 3)::(EA 3 AWeaken 4)::
  (EA 4 ANone 6)::(EA 4 ANone 5)::(EA 5 AWeaken 8)::(EA 6 ANone 7)::
  (EA 7 AWeaken 8)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_start_pass_2_quant__tmp) s) <> (eval (ENum (0))
  s))%Z)) 37)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_start_pass_2_quant__tmp) s) = (eval (ENum (0))
  s))%Z)) 9)::(EA 9 AWeaken 10)::(EA 10 ANone 12)::(EA 10 ANone 11)::
  (EA 11 ANone 13)::(EA 12 ANone 13)::(EA 13 (AAssign V_start_pass_2_quant_i
  None) 14)::(EA 14 AWeaken 15)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_start_pass_2_quant_i) s) < (eval (ENum (1))
  s))%Z)) 17)::(EA 15 (AGuard (fun s => ((eval (EVar V_start_pass_2_quant_i)
  s) >= (eval (ENum (1)) s))%Z)) 16)::(EA 16 AWeaken 20)::
  (EA 17 AWeaken 18)::(EA 18 ANone 19)::(EA 19 AWeaken 20)::(EA 20 (AGuard
  (fun s => ((eval (EVar V_start_pass_2_quant_i) s) > (eval (ENum (256))
  s))%Z)) 22)::(EA 20 (AGuard (fun s => ((eval (EVar V_start_pass_2_quant_i)
  s) <= (eval (ENum (256)) s))%Z)) 21)::(EA 21 AWeaken 25)::
  (EA 22 AWeaken 23)::(EA 23 ANone 24)::(EA 24 AWeaken 25)::
  (EA 25 ANone 26)::(EA 25 ANone 35)::(EA 26 (AAssign
  V_start_pass_2_quant_arraysize None) 27)::(EA 27 AWeaken 28)::
  (EA 28 ANone 30)::(EA 28 ANone 29)::(EA 29 AWeaken 32)::(EA 30 ANone 31)::
  (EA 31 AWeaken 32)::(EA 32 ANone 33)::(EA 32 ANone 34)::(EA 33 ANone 34)::
  (EA 34 ANone 35)::(EA 35 ANone 36)::(EA 36 AWeaken 40)::
  (EA 37 AWeaken 38)::(EA 38 ANone 39)::(EA 39 AWeaken 40)::
  (EA 40 ANone 42)::(EA 40 ANone 41)::(EA 41 AWeaken 49)::(EA 42 (AAssign
  V_start_pass_2_quant_i (Some (ENum (0)))) 43)::(EA 43 ANone 44)::
  (EA 44 AWeaken 45)::(EA 45 (AGuard
  (fun s => ((eval (EVar V_start_pass_2_quant_i) s) < (eval (ENum (32))
  s))%Z)) 50)::(EA 45 (AGuard (fun s => ((eval (EVar V_start_pass_2_quant_i)
  s) >= (eval (ENum (32)) s))%Z)) 46)::(EA 46 AWeaken 47)::(EA 47 ANone 48)::
  (EA 48 AWeaken 49)::(EA 50 AWeaken 51)::(EA 51 ANone 52)::(EA 52 (AAssign
  V_start_pass_2_quant_i (Some (EAdd (EVar V_start_pass_2_quant_i)
  (ENum (1))))) 53)::(EA 53 ANone 54)::(EA 54 ANone 55)::(EA 55 (AAssign
  V_start_pass_2_quant_z (Some (EAdd (ENum (1))
  (EVar V_start_pass_2_quant_z)))) 56)::(EA 56 AWeaken 45)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_start_pass_2_quant => Pedges_start_pass_2_quant
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_start_pass_2_quant => 49
     end)%positive;
  var_global := var_global
}.

Definition ai_start_pass_2_quant (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0)%Z
   | 3 => (-1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 4 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0)%Z
   | 5 => (-1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 6 => (-1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 7 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0)%Z
   | 8 => (-1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 9 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0)%Z
   | 10 => (-1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 11 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0)%Z
   | 12 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0)%Z
   | 13 => (-1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 14 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0)%Z
   | 15 => (-1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 16 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_i + 1 <= 0)%Z
   | 17 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant_i <= 0)%Z
   | 18 => (1 * s V_start_pass_2_quant_i <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 19 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant_i <= 0)%Z
   | 20 => (-1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 21 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant_i + -256 <= 0)%Z
   | 22 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_i + 257 <= 0)%Z
   | 23 => (-1 * s V_start_pass_2_quant_i + 257 <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 24 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_i + 257 <= 0)%Z
   | 25 => (-1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 26 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0)%Z
   | 27 => (-1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 28 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0)%Z
   | 29 => (-1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 30 => (-1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 31 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0)%Z
   | 32 => (-1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 33 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0)%Z
   | 34 => (-1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 35 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant__tmp <= 0)%Z
   | 36 => (-1 * s V_start_pass_2_quant__tmp <= 0 /\ 1 * s V_start_pass_2_quant__tmp <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 37 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0)%Z
   | 38 => (-1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 39 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0)%Z
   | 40 => (-1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0)%Z
   | 41 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0)%Z
   | 42 => (1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0)%Z
   | 43 => (-1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_i <= 0 /\ -1 * s V_start_pass_2_quant_i <= 0)%Z
   | 44 => (-1 * s V_start_pass_2_quant_i <= 0 /\ 1 * s V_start_pass_2_quant_i <= 0 /\ 1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0)%Z
   | 45 => (-1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_i <= 0 /\ 1 * s V_start_pass_2_quant_i + -32 <= 0)%Z
   | 46 => (1 * s V_start_pass_2_quant_i + -32 <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_i + 32 <= 0)%Z
   | 47 => (-1 * s V_start_pass_2_quant_i + 32 <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_i + -32 <= 0)%Z
   | 48 => (1 * s V_start_pass_2_quant_i + -32 <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_i + 32 <= 0)%Z
   | 49 => (-1 * s V_start_pass_2_quant_z <= 0)%Z
   | 50 => (-1 * s V_start_pass_2_quant_i <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_i + -31 <= 0)%Z
   | 51 => (1 * s V_start_pass_2_quant_i + -31 <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_i <= 0)%Z
   | 52 => (-1 * s V_start_pass_2_quant_i <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0 /\ 1 * s V_start_pass_2_quant_i + -31 <= 0)%Z
   | 53 => (-1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_i + 1 <= 0 /\ 1 * s V_start_pass_2_quant_i + -32 <= 0)%Z
   | 54 => (1 * s V_start_pass_2_quant_i + -32 <= 0 /\ -1 * s V_start_pass_2_quant_i + 1 <= 0 /\ -1 * s V_start_pass_2_quant_z <= 0)%Z
   | 55 => (-1 * s V_start_pass_2_quant_z <= 0 /\ -1 * s V_start_pass_2_quant_i + 1 <= 0 /\ 1 * s V_start_pass_2_quant_i + -32 <= 0)%Z
   | 56 => (1 * s V_start_pass_2_quant_i + -32 <= 0 /\ -1 * s V_start_pass_2_quant_i + 1 <= 0 /\ -1 * s V_start_pass_2_quant_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_start_pass_2_quant (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((32 # 1) <= z)%Q
   | 2 => ((32 # 1) <= z)%Q
   | 3 => ((32 # 1) <= z)%Q
   | 4 => ((32 # 1) <= z)%Q
   | 5 => ((32 # 1) <= z)%Q
   | 6 => ((32 # 1) <= z)%Q
   | 7 => ((32 # 1) <= z)%Q
   | 8 => ((32 # 1) <= z)%Q
   | 9 => ((32 # 1) <= z)%Q
   | 10 => ((32 # 1) <= z)%Q
   | 11 => ((32 # 1) <= z)%Q
   | 12 => ((32 # 1) <= z)%Q
   | 13 => ((32 # 1) <= z)%Q
   | 14 => ((32 # 1) <= z)%Q
   | 15 => ((32 # 1) <= z)%Q
   | 16 => ((32 # 1) <= z)%Q
   | 17 => ((32 # 1) <= z)%Q
   | 18 => ((32 # 1) <= z)%Q
   | 19 => ((32 # 1) <= z)%Q
   | 20 => ((32 # 1) <= z)%Q
   | 21 => ((32 # 1) <= z)%Q
   | 22 => ((32 # 1) <= z)%Q
   | 23 => ((32 # 1) <= z)%Q
   | 24 => ((32 # 1) <= z)%Q
   | 25 => ((32 # 1) <= z)%Q
   | 26 => ((32 # 1) <= z)%Q
   | 27 => ((32 # 1) <= z)%Q
   | 28 => ((32 # 1) <= z)%Q
   | 29 => ((32 # 1) <= z)%Q
   | 30 => ((32 # 1) <= z)%Q
   | 31 => ((32 # 1) <= z)%Q
   | 32 => ((32 # 1) <= z)%Q
   | 33 => ((32 # 1) <= z)%Q
   | 34 => ((32 # 1) <= z)%Q
   | 35 => ((32 # 1) <= z)%Q
   | 36 => ((32 # 1) <= z)%Q
   | 37 => ((32 # 1) <= z)%Q
   | 38 => ((32 # 1) <= z)%Q
   | 39 => ((32 # 1) <= z)%Q
   | 40 => ((32 # 1) <= z)%Q
   | 41 => hints
     [(*-32 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_start_pass_2_quant_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_start_pass_2_quant_z) (0))) (F_max0_ge_0 (-
                                                                    s V_start_pass_2_quant_z))]
     ((32 # 1) <= z)%Q
   | 42 => ((32 # 1) <= z)%Q
   | 43 => (max0(32 - s V_start_pass_2_quant_i) <= z)%Q
   | 44 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (-s V_start_pass_2_quant_z)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_start_pass_2_quant_z) (0))) (F_max0_ge_0 (-
                                                                    s V_start_pass_2_quant_z))]
     (max0(32 - s V_start_pass_2_quant_i) <= z)%Q
   | 45 => (s V_start_pass_2_quant_z + max0(32 - s V_start_pass_2_quant_i) <= z)%Q
   | 46 => (s V_start_pass_2_quant_z + max0(32 - s V_start_pass_2_quant_i) <= z)%Q
   | 47 => (s V_start_pass_2_quant_z + max0(32 - s V_start_pass_2_quant_i) <= z)%Q
   | 48 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (32 - s V_start_pass_2_quant_i) (31
                                                                    - s V_start_pass_2_quant_i));
      (*-1 0*) F_max0_ge_0 (31 - s V_start_pass_2_quant_i)]
     (s V_start_pass_2_quant_z + max0(32 - s V_start_pass_2_quant_i) <= z)%Q
   | 49 => (s V_start_pass_2_quant_z <= z)%Q
   | 50 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (32 - s V_start_pass_2_quant_i) (1)]
     (s V_start_pass_2_quant_z + max0(32 - s V_start_pass_2_quant_i) <= z)%Q
   | 51 => ((1 # 1) + s V_start_pass_2_quant_z
            + max0(31 - s V_start_pass_2_quant_i) <= z)%Q
   | 52 => ((1 # 1) + s V_start_pass_2_quant_z
            + max0(31 - s V_start_pass_2_quant_i) <= z)%Q
   | 53 => ((1 # 1) + s V_start_pass_2_quant_z
            + max0(32 - s V_start_pass_2_quant_i) <= z)%Q
   | 54 => ((1 # 1) + s V_start_pass_2_quant_z
            + max0(32 - s V_start_pass_2_quant_i) <= z)%Q
   | 55 => ((1 # 1) + s V_start_pass_2_quant_z
            + max0(32 - s V_start_pass_2_quant_i) <= z)%Q
   | 56 => (s V_start_pass_2_quant_z + max0(32 - s V_start_pass_2_quant_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_start_pass_2_quant =>
    [mkPA Q (fun n z s => ai_start_pass_2_quant n s /\ annot0_start_pass_2_quant n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_start_pass_2_quant (proc_start P_start_pass_2_quant) s1 (proc_end P_start_pass_2_quant) s2 ->
    (s2 V_start_pass_2_quant_z <= (32 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_start_pass_2_quant.
Qed.
