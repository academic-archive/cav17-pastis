Require Import pasta.Pasta.

Inductive proc: Type :=
  P_set_file_type.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_set_file_type_z := 1%positive.
Notation V_set_file_type_ascii_freq := 2%positive.
Notation V_set_file_type_bin_freq := 3%positive.
Notation V_set_file_type_file_type_dref := 4%positive.
Notation V_set_file_type_n := 5%positive.
Definition Pedges_set_file_type: list (edge proc) :=
  (EA 1 (AAssign V_set_file_type_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_set_file_type_n (Some (ENum (0)))) 3)::(EA 3 (AAssign
  V_set_file_type_ascii_freq (Some (ENum (0)))) 4)::(EA 4 (AAssign
  V_set_file_type_bin_freq (Some (ENum (0)))) 5)::(EA 5 ANone 6)::
  (EA 6 AWeaken 7)::(EA 7 (AGuard (fun s => ((eval (EVar V_set_file_type_n)
  s) < (eval (ENum (7)) s))%Z)) 34)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_set_file_type_n) s) >= (eval (ENum (7))
  s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 ANone 10)::(EA 10 AWeaken 11)::
  (EA 11 (AGuard (fun s => ((eval (EVar V_set_file_type_n) s) <
  (eval (ENum (128)) s))%Z)) 27)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_set_file_type_n) s) >= (eval (ENum (128))
  s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_set_file_type_n) s) <
  (eval (ENum (256)) s))%Z)) 20)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_set_file_type_n) s) >= (eval (ENum (256))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 (AAssign
  V_set_file_type_file_type_dref None) 18)::(EA 18 AWeaken 19)::
  (EA 20 AWeaken 21)::(EA 21 (AAssign V_set_file_type_n
  (Some (EAdd (EVar V_set_file_type_n) (ENum (1))))) 22)::(EA 22 (AAssign
  V_set_file_type_bin_freq None) 23)::(EA 23 ANone 24)::(EA 24 ANone 25)::
  (EA 25 (AAssign V_set_file_type_z (Some (EAdd (ENum (1))
  (EVar V_set_file_type_z)))) 26)::(EA 26 AWeaken 15)::(EA 27 AWeaken 28)::
  (EA 28 (AAssign V_set_file_type_n (Some (EAdd (EVar V_set_file_type_n)
  (ENum (1))))) 29)::(EA 29 (AAssign V_set_file_type_ascii_freq None) 30)::
  (EA 30 ANone 31)::(EA 31 ANone 32)::(EA 32 (AAssign V_set_file_type_z
  (Some (EAdd (ENum (1)) (EVar V_set_file_type_z)))) 33)::
  (EA 33 AWeaken 11)::(EA 34 AWeaken 35)::(EA 35 (AAssign V_set_file_type_n
  (Some (EAdd (EVar V_set_file_type_n) (ENum (1))))) 36)::(EA 36 (AAssign
  V_set_file_type_bin_freq None) 37)::(EA 37 ANone 38)::(EA 38 ANone 39)::
  (EA 39 (AAssign V_set_file_type_z (Some (EAdd (ENum (1))
  (EVar V_set_file_type_z)))) 40)::(EA 40 AWeaken 7)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_set_file_type => Pedges_set_file_type
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_set_file_type => 19
     end)%positive;
  var_global := var_global
}.

Definition ai_set_file_type (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_z <= 0)%Z
   | 3 => (-1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n <= 0 /\ -1 * s V_set_file_type_n <= 0)%Z
   | 4 => (-1 * s V_set_file_type_n <= 0 /\ 1 * s V_set_file_type_n <= 0 /\ 1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_ascii_freq <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0)%Z
   | 5 => (-1 * s V_set_file_type_ascii_freq <= 0 /\ 1 * s V_set_file_type_ascii_freq <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n <= 0 /\ -1 * s V_set_file_type_n <= 0 /\ 1 * s V_set_file_type_bin_freq <= 0 /\ -1 * s V_set_file_type_bin_freq <= 0)%Z
   | 6 => (-1 * s V_set_file_type_bin_freq <= 0 /\ 1 * s V_set_file_type_bin_freq <= 0 /\ -1 * s V_set_file_type_n <= 0 /\ 1 * s V_set_file_type_n <= 0 /\ 1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_ascii_freq <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0)%Z
   | 7 => (-1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_n <= 0 /\ 1 * s V_set_file_type_n + -7 <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0 /\ 1 * s V_set_file_type_ascii_freq <= 0)%Z
   | 8 => (1 * s V_set_file_type_ascii_freq <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0 /\ 1 * s V_set_file_type_n + -7 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_n + 7 <= 0)%Z
   | 9 => (-1 * s V_set_file_type_n + 7 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -7 <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0 /\ 1 * s V_set_file_type_ascii_freq <= 0)%Z
   | 10 => (1 * s V_set_file_type_ascii_freq <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0 /\ 1 * s V_set_file_type_n + -7 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_n + 7 <= 0)%Z
   | 11 => (-1 * s V_set_file_type_n + 7 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -128 <= 0)%Z
   | 12 => (1 * s V_set_file_type_n + -128 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_n + 128 <= 0)%Z
   | 13 => (-1 * s V_set_file_type_n + 128 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -128 <= 0)%Z
   | 14 => (1 * s V_set_file_type_n + -128 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_n + 128 <= 0)%Z
   | 15 => (-1 * s V_set_file_type_n + 128 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -256 <= 0)%Z
   | 16 => (1 * s V_set_file_type_n + -256 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_n + 256 <= 0)%Z
   | 17 => (-1 * s V_set_file_type_n + 256 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -256 <= 0)%Z
   | 18 => (1 * s V_set_file_type_n + -256 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_n + 256 <= 0)%Z
   | 19 => (-1 * s V_set_file_type_n + 256 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -256 <= 0)%Z
   | 20 => (-1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_n + 128 <= 0 /\ 1 * s V_set_file_type_n + -255 <= 0)%Z
   | 21 => (1 * s V_set_file_type_n + -255 <= 0 /\ -1 * s V_set_file_type_n + 128 <= 0 /\ -1 * s V_set_file_type_z <= 0)%Z
   | 22 => (-1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -256 <= 0 /\ -1 * s V_set_file_type_n + 129 <= 0)%Z
   | 23 => (-1 * s V_set_file_type_n + 129 <= 0 /\ 1 * s V_set_file_type_n + -256 <= 0 /\ -1 * s V_set_file_type_z <= 0)%Z
   | 24 => (-1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -256 <= 0 /\ -1 * s V_set_file_type_n + 129 <= 0)%Z
   | 25 => (-1 * s V_set_file_type_n + 129 <= 0 /\ 1 * s V_set_file_type_n + -256 <= 0 /\ -1 * s V_set_file_type_z <= 0)%Z
   | 26 => (1 * s V_set_file_type_n + -256 <= 0 /\ -1 * s V_set_file_type_n + 129 <= 0 /\ -1 * s V_set_file_type_z + 1 <= 0)%Z
   | 27 => (-1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_n + 7 <= 0 /\ 1 * s V_set_file_type_n + -127 <= 0)%Z
   | 28 => (1 * s V_set_file_type_n + -127 <= 0 /\ -1 * s V_set_file_type_n + 7 <= 0 /\ -1 * s V_set_file_type_z <= 0)%Z
   | 29 => (-1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -128 <= 0 /\ -1 * s V_set_file_type_n + 8 <= 0)%Z
   | 30 => (-1 * s V_set_file_type_n + 8 <= 0 /\ 1 * s V_set_file_type_n + -128 <= 0 /\ -1 * s V_set_file_type_z <= 0)%Z
   | 31 => (-1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -128 <= 0 /\ -1 * s V_set_file_type_n + 8 <= 0)%Z
   | 32 => (-1 * s V_set_file_type_n + 8 <= 0 /\ 1 * s V_set_file_type_n + -128 <= 0 /\ -1 * s V_set_file_type_z <= 0)%Z
   | 33 => (1 * s V_set_file_type_n + -128 <= 0 /\ -1 * s V_set_file_type_n + 8 <= 0 /\ -1 * s V_set_file_type_z + 1 <= 0)%Z
   | 34 => (1 * s V_set_file_type_ascii_freq <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0 /\ -1 * s V_set_file_type_n <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -6 <= 0)%Z
   | 35 => (1 * s V_set_file_type_n + -6 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_n <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0 /\ 1 * s V_set_file_type_ascii_freq <= 0)%Z
   | 36 => (1 * s V_set_file_type_ascii_freq <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -7 <= 0 /\ -1 * s V_set_file_type_n + 1 <= 0)%Z
   | 37 => (-1 * s V_set_file_type_n + 1 <= 0 /\ 1 * s V_set_file_type_n + -7 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0 /\ 1 * s V_set_file_type_ascii_freq <= 0)%Z
   | 38 => (1 * s V_set_file_type_ascii_freq <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ 1 * s V_set_file_type_n + -7 <= 0 /\ -1 * s V_set_file_type_n + 1 <= 0)%Z
   | 39 => (-1 * s V_set_file_type_n + 1 <= 0 /\ 1 * s V_set_file_type_n + -7 <= 0 /\ -1 * s V_set_file_type_z <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0 /\ 1 * s V_set_file_type_ascii_freq <= 0)%Z
   | 40 => (1 * s V_set_file_type_ascii_freq <= 0 /\ -1 * s V_set_file_type_ascii_freq <= 0 /\ 1 * s V_set_file_type_n + -7 <= 0 /\ -1 * s V_set_file_type_n + 1 <= 0 /\ -1 * s V_set_file_type_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_set_file_type (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((256 # 1) <= z)%Q
   | 2 => ((256 # 1) + max0(s V_set_file_type_z) <= z)%Q
   | 3 => ((257 # 2) + (1 # 2) * max0(127 - s V_set_file_type_n)
           + (1 # 2) * max0(128 - s V_set_file_type_n)
           + max0(s V_set_file_type_z) <= z)%Q
   | 4 => ((257 # 2) + (1 # 2) * max0(127 - s V_set_file_type_n)
           + (1 # 2) * max0(128 - s V_set_file_type_n)
           + max0(s V_set_file_type_z) <= z)%Q
   | 5 => ((257 # 2) + (1 # 2) * max0(127 - s V_set_file_type_n)
           + (1 # 2) * max0(128 - s V_set_file_type_n)
           + max0(s V_set_file_type_z) <= z)%Q
   | 6 => ((257 # 2) + (1 # 2) * max0(127 - s V_set_file_type_n)
           + (1 # 2) * max0(128 - s V_set_file_type_n)
           + max0(s V_set_file_type_z) <= z)%Q
   | 7 => ((257 # 2) + (1 # 2) * max0(127 - s V_set_file_type_n)
           + (1 # 2) * max0(128 - s V_set_file_type_n)
           + max0(s V_set_file_type_z) <= z)%Q
   | 8 => ((257 # 2) + (1 # 2) * max0(127 - s V_set_file_type_n)
           + (1 # 2) * max0(128 - s V_set_file_type_n)
           + max0(s V_set_file_type_z) <= z)%Q
   | 9 => ((257 # 2) + (1 # 2) * max0(127 - s V_set_file_type_n)
           + (1 # 2) * max0(128 - s V_set_file_type_n)
           + max0(s V_set_file_type_z) <= z)%Q
   | 10 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_arg (s V_set_file_type_z)) (F_check_ge (s V_set_file_type_z) (0));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (127
                                                     - s V_set_file_type_n)) (F_check_ge (127
                                                                    - s V_set_file_type_n) (0))]
     ((257 # 2) + (1 # 2) * max0(127 - s V_set_file_type_n)
      + (1 # 2) * max0(128 - s V_set_file_type_n) + max0(s V_set_file_type_z) <= z)%Q
   | 11 => ((192 # 1) - (1 # 2) * s V_set_file_type_n + s V_set_file_type_z
            + (1 # 2) * max0(128 - s V_set_file_type_n) <= z)%Q
   | 12 => ((192 # 1) - (1 # 2) * s V_set_file_type_n + s V_set_file_type_z
            + (1 # 2) * max0(128 - s V_set_file_type_n) <= z)%Q
   | 13 => ((192 # 1) - (1 # 2) * s V_set_file_type_n + s V_set_file_type_z
            + (1 # 2) * max0(128 - s V_set_file_type_n) <= z)%Q
   | 14 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (128
                                                     - s V_set_file_type_n)) (F_check_ge (128
                                                                    - s V_set_file_type_n) (0))]
     ((192 # 1) - (1 # 2) * s V_set_file_type_n + s V_set_file_type_z
      + (1 # 2) * max0(128 - s V_set_file_type_n) <= z)%Q
   | 15 => ((256 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 16 => ((256 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 17 => ((256 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (256 - s V_set_file_type_n) (255
                                                                    - s V_set_file_type_n));
      (*-1 0*) F_max0_ge_0 (255 - s V_set_file_type_n);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (256
                                                               - s V_set_file_type_n) (0))) (F_max0_ge_0 (256
                                                                    - s V_set_file_type_n))]
     ((256 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 19 => (s V_set_file_type_z <= z)%Q
   | 20 => ((256 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 21 => ((256 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 22 => ((257 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 23 => ((257 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 24 => ((257 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 25 => ((257 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 26 => ((256 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 27 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (128
                                                     - s V_set_file_type_n)) (F_check_ge (128
                                                                    - s V_set_file_type_n) (0))]
     ((192 # 1) - (1 # 2) * s V_set_file_type_n + s V_set_file_type_z
      + (1 # 2) * max0(128 - s V_set_file_type_n) <= z)%Q
   | 28 => ((256 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 29 => ((257 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 30 => ((257 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 31 => ((257 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 32 => ((257 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 33 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (128
                                                                 - s V_set_file_type_n) (0))) (F_max0_ge_0 (128
                                                                    - s V_set_file_type_n))]
     ((256 # 1) - s V_set_file_type_n + s V_set_file_type_z <= z)%Q
   | 34 => hints
     [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (128 - s V_set_file_type_n)) (F_check_ge (128
                                                                    - s V_set_file_type_n) (0))]
     ((257 # 2) + (1 # 2) * max0(127 - s V_set_file_type_n)
      + (1 # 2) * max0(128 - s V_set_file_type_n) + max0(s V_set_file_type_z) <= z)%Q
   | 35 => ((385 # 2) - (1 # 2) * s V_set_file_type_n
            + (1 # 2) * max0(127 - s V_set_file_type_n)
            + max0(s V_set_file_type_z) <= z)%Q
   | 36 => ((193 # 1) - (1 # 2) * s V_set_file_type_n
            + (1 # 2) * max0(128 - s V_set_file_type_n)
            + max0(s V_set_file_type_z) <= z)%Q
   | 37 => ((193 # 1) - (1 # 2) * s V_set_file_type_n
            + (1 # 2) * max0(128 - s V_set_file_type_n)
            + max0(s V_set_file_type_z) <= z)%Q
   | 38 => ((193 # 1) - (1 # 2) * s V_set_file_type_n
            + (1 # 2) * max0(128 - s V_set_file_type_n)
            + max0(s V_set_file_type_z) <= z)%Q
   | 39 => ((193 # 1) - (1 # 2) * s V_set_file_type_n
            + (1 # 2) * max0(128 - s V_set_file_type_n)
            + max0(s V_set_file_type_z) <= z)%Q
   | 40 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_set_file_type_z) (0))) (F_max0_ge_0 (s V_set_file_type_z));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (127
                                                                 - s V_set_file_type_n) (0))) (F_max0_ge_0 (127
                                                                    - s V_set_file_type_n));
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_arg (-1 + s V_set_file_type_z)) (F_check_ge (-1
                                                                    + s V_set_file_type_z) (0))]
     ((193 # 1) - (1 # 2) * s V_set_file_type_n
      + max0(-1 + s V_set_file_type_z)
      + (1 # 2) * max0(128 - s V_set_file_type_n) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_set_file_type =>
    [mkPA Q (fun n z s => ai_set_file_type n s /\ annot0_set_file_type n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_set_file_type (proc_start P_set_file_type) s1 (proc_end P_set_file_type) s2 ->
    (s2 V_set_file_type_z <= (256 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_set_file_type.
Qed.
