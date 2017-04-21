Require Import pasta.Pasta.

Inductive proc: Type :=
  P_loop_break.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_loop_break_z := 1%positive.
Notation V_loop_break__tmp := 2%positive.
Notation V_loop_break_cod_info_dref_off80 := 3%positive.
Notation V_loop_break_cod_info_dref_off84 := 4%positive.
Notation V_loop_break_i := 5%positive.
Notation V_loop_break_sfb := 6%positive.
Notation V_loop_break_cod_info := 7%positive.
Notation V_loop_break_scalefac := 8%positive.
Definition Pedges_loop_break: list (edge proc) :=
  (EA 1 (AAssign V_loop_break_z (Some (ENum (0)))) 2)::(EA 2 (AGuard
  (fun s => ((eval (EVar V_loop_break_sfb) s) >= (eval (ENum (0))
  s))%Z)) 3)::(EA 3 (AGuard
  (fun s => ((eval (EVar V_loop_break_cod_info_dref_off80) s) >=
  (eval (ENum (0)) s))%Z)) 4)::(EA 4 AWeaken 5)::(EA 5 (AAssign
  V_loop_break_sfb (Some (ENum (0)))) 6)::(EA 6 ANone 7)::(EA 7 AWeaken 8)::
  (EA 8 (AGuard (fun s => ((eval (EVar V_loop_break_sfb) s) <
  (eval (EVar V_loop_break_cod_info_dref_off80) s))%Z)) 41)::(EA 8 (AGuard
  (fun s => ((eval (EVar V_loop_break_sfb) s) >=
  (eval (EVar V_loop_break_cod_info_dref_off80) s))%Z)) 9)::
  (EA 9 AWeaken 10)::(EA 10 (AAssign V_loop_break_sfb
  (Some (EVar V_loop_break_cod_info_dref_off84))) 11)::(EA 11 ANone 12)::
  (EA 12 AWeaken 13)::(EA 13 (AGuard (fun s => ((eval (EVar V_loop_break_sfb)
  s) < (eval (ENum (12)) s))%Z)) 18)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_loop_break_sfb) s) >= (eval (ENum (12))
  s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 (AAssign V_loop_break__tmp
  (Some (ENum (1)))) 16)::(EA 16 ANone 17)::(EA 17 AWeaken 52)::
  (EA 18 AWeaken 19)::(EA 19 (AAssign V_loop_break_i (Some (ENum (0)))) 20)::
  (EA 20 ANone 21)::(EA 21 AWeaken 22)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_loop_break_i) s) < (eval (ENum (3)) s))%Z)) 30)::
  (EA 22 (AGuard (fun s => ((eval (EVar V_loop_break_i) s) >=
  (eval (ENum (3)) s))%Z)) 23)::(EA 23 AWeaken 24)::(EA 24 ANone 25)::
  (EA 25 (AAssign V_loop_break_sfb (Some (EAdd (EVar V_loop_break_sfb)
  (ENum (1))))) 26)::(EA 26 ANone 27)::(EA 27 ANone 28)::(EA 28 (AAssign
  V_loop_break_z (Some (EAdd (ENum (1)) (EVar V_loop_break_z)))) 29)::
  (EA 29 AWeaken 13)::(EA 30 AWeaken 31)::(EA 31 ANone 38)::
  (EA 31 ANone 32)::(EA 32 ANone 33)::(EA 33 (AAssign V_loop_break_i
  (Some (EAdd (EVar V_loop_break_i) (ENum (1))))) 34)::(EA 34 ANone 35)::
  (EA 35 ANone 36)::(EA 36 (AAssign V_loop_break_z (Some (EAdd (ENum (1))
  (EVar V_loop_break_z)))) 37)::(EA 37 AWeaken 22)::(EA 38 (AAssign
  V_loop_break__tmp (Some (ENum (0)))) 39)::(EA 39 ANone 40)::
  (EA 40 AWeaken 52)::(EA 41 AWeaken 42)::(EA 42 ANone 49)::
  (EA 42 ANone 43)::(EA 43 ANone 44)::(EA 44 (AAssign V_loop_break_sfb
  (Some (EAdd (EVar V_loop_break_sfb) (ENum (1))))) 45)::(EA 45 ANone 46)::
  (EA 46 ANone 47)::(EA 47 (AAssign V_loop_break_z (Some (EAdd (ENum (1))
  (EVar V_loop_break_z)))) 48)::(EA 48 AWeaken 8)::(EA 49 (AAssign
  V_loop_break__tmp (Some (ENum (0)))) 50)::(EA 50 ANone 51)::
  (EA 51 AWeaken 52)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_loop_break => Pedges_loop_break
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_loop_break => 52
     end)%positive;
  var_global := var_global
}.

Definition ai_loop_break (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_z <= 0)%Z
   | 3 => (-1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_sfb <= 0)%Z
   | 4 => (-1 * s V_loop_break_sfb <= 0 /\ 1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80 <= 0)%Z
   | 5 => (-1 * s V_loop_break_cod_info_dref_off80 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_sfb <= 0)%Z
   | 6 => (1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80 <= 0 /\ 1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_sfb <= 0)%Z
   | 7 => (-1 * s V_loop_break_sfb <= 0 /\ 1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_z <= 0)%Z
   | 8 => (-1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb <= 0)%Z
   | 9 => (-1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_cod_info_dref_off80+ -1 * s V_loop_break_sfb <= 0)%Z
   | 10 => (1 * s V_loop_break_cod_info_dref_off80+ -1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb <= 0)%Z
   | 11 => (-1 * s V_loop_break_z <= 0)%Z
   | 12 => (-1 * s V_loop_break_z <= 0)%Z
   | 13 => (-1 * s V_loop_break_z <= 0)%Z
   | 14 => (-1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_sfb + 12 <= 0)%Z
   | 15 => (-1 * s V_loop_break_sfb + 12 <= 0 /\ -1 * s V_loop_break_z <= 0)%Z
   | 16 => (-1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_sfb + 12 <= 0 /\ 1 * s V_loop_break__tmp + -1 <= 0 /\ -1 * s V_loop_break__tmp + 1 <= 0)%Z
   | 17 => (-1 * s V_loop_break__tmp + 1 <= 0 /\ 1 * s V_loop_break__tmp + -1 <= 0 /\ -1 * s V_loop_break_sfb + 12 <= 0 /\ -1 * s V_loop_break_z <= 0)%Z
   | 18 => (-1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0)%Z
   | 19 => (1 * s V_loop_break_sfb + -11 <= 0 /\ -1 * s V_loop_break_z <= 0)%Z
   | 20 => (-1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0 /\ 1 * s V_loop_break_i <= 0 /\ -1 * s V_loop_break_i <= 0)%Z
   | 21 => (-1 * s V_loop_break_i <= 0 /\ 1 * s V_loop_break_i <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0 /\ -1 * s V_loop_break_z <= 0)%Z
   | 22 => (-1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_i <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0 /\ 1 * s V_loop_break_i + -3 <= 0)%Z
   | 23 => (1 * s V_loop_break_i + -3 <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_i + 3 <= 0)%Z
   | 24 => (-1 * s V_loop_break_i + 3 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0 /\ 1 * s V_loop_break_i + -3 <= 0)%Z
   | 25 => (1 * s V_loop_break_i + -3 <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_i + 3 <= 0)%Z
   | 26 => (-1 * s V_loop_break_i + 3 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_i + -3 <= 0 /\ 1 * s V_loop_break_sfb + -12 <= 0)%Z
   | 27 => (1 * s V_loop_break_sfb + -12 <= 0 /\ 1 * s V_loop_break_i + -3 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_i + 3 <= 0)%Z
   | 28 => (-1 * s V_loop_break_i + 3 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_i + -3 <= 0 /\ 1 * s V_loop_break_sfb + -12 <= 0)%Z
   | 29 => (1 * s V_loop_break_sfb + -12 <= 0 /\ 1 * s V_loop_break_i + -3 <= 0 /\ -1 * s V_loop_break_i + 3 <= 0 /\ -1 * s V_loop_break_z + 1 <= 0)%Z
   | 30 => (1 * s V_loop_break_sfb + -11 <= 0 /\ -1 * s V_loop_break_i <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_i + -2 <= 0)%Z
   | 31 => (1 * s V_loop_break_i + -2 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_i <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0)%Z
   | 32 => (1 * s V_loop_break_sfb + -11 <= 0 /\ -1 * s V_loop_break_i <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_i + -2 <= 0)%Z
   | 33 => (1 * s V_loop_break_i + -2 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_i <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0)%Z
   | 34 => (1 * s V_loop_break_sfb + -11 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_i + -3 <= 0 /\ -1 * s V_loop_break_i + 1 <= 0)%Z
   | 35 => (-1 * s V_loop_break_i + 1 <= 0 /\ 1 * s V_loop_break_i + -3 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0)%Z
   | 36 => (1 * s V_loop_break_sfb + -11 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_i + -3 <= 0 /\ -1 * s V_loop_break_i + 1 <= 0)%Z
   | 37 => (-1 * s V_loop_break_i + 1 <= 0 /\ 1 * s V_loop_break_i + -3 <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0 /\ -1 * s V_loop_break_z + 1 <= 0)%Z
   | 38 => (1 * s V_loop_break_sfb + -11 <= 0 /\ -1 * s V_loop_break_i <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_i + -2 <= 0)%Z
   | 39 => (1 * s V_loop_break_i + -2 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_i <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0 /\ 1 * s V_loop_break__tmp <= 0 /\ -1 * s V_loop_break__tmp <= 0)%Z
   | 40 => (-1 * s V_loop_break__tmp <= 0 /\ 1 * s V_loop_break__tmp <= 0 /\ 1 * s V_loop_break_sfb + -11 <= 0 /\ -1 * s V_loop_break_i <= 0 /\ -1 * s V_loop_break_z <= 0 /\ 1 * s V_loop_break_i + -2 <= 0)%Z
   | 41 => (-1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb + 1 <= 0)%Z
   | 42 => (-1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb + 1 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_sfb <= 0)%Z
   | 43 => (-1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb + 1 <= 0)%Z
   | 44 => (-1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb + 1 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_sfb <= 0)%Z
   | 45 => (-1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_sfb + 1 <= 0)%Z
   | 46 => (-1 * s V_loop_break_sfb + 1 <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_z <= 0)%Z
   | 47 => (-1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_sfb + 1 <= 0)%Z
   | 48 => (-1 * s V_loop_break_sfb + 1 <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_z + 1 <= 0)%Z
   | 49 => (-1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb + 1 <= 0)%Z
   | 50 => (-1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb + 1 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_sfb <= 0 /\ 1 * s V_loop_break__tmp <= 0 /\ -1 * s V_loop_break__tmp <= 0)%Z
   | 51 => (-1 * s V_loop_break__tmp <= 0 /\ 1 * s V_loop_break__tmp <= 0 /\ -1 * s V_loop_break_sfb <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break_cod_info_dref_off80+ 1 * s V_loop_break_sfb + 1 <= 0)%Z
   | 52 => (1 * s V_loop_break__tmp + -1 <= 0 /\ -1 * s V_loop_break_z <= 0 /\ -1 * s V_loop_break__tmp <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_loop_break (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
           + max0(s V_loop_break_cod_info_dref_off80) <= z)%Q
   | 2 => (s V_loop_break_z
           + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
           + max0(s V_loop_break_cod_info_dref_off80) <= z)%Q
   | 3 => (s V_loop_break_z
           + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
           + max0(s V_loop_break_cod_info_dref_off80) <= z)%Q
   | 4 => (s V_loop_break_z
           + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
           + max0(s V_loop_break_cod_info_dref_off80) <= z)%Q
   | 5 => (s V_loop_break_z
           + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
           + max0(s V_loop_break_cod_info_dref_off80) <= z)%Q
   | 6 => (s V_loop_break_z
           + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
           + max0(s V_loop_break_cod_info_dref_off80 - s V_loop_break_sfb) <= z)%Q
   | 7 => (s V_loop_break_z
           + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
           + max0(s V_loop_break_cod_info_dref_off80 - s V_loop_break_sfb) <= z)%Q
   | 8 => (s V_loop_break_z
           + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
           + max0(s V_loop_break_cod_info_dref_off80 - s V_loop_break_sfb) <= z)%Q
   | 9 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (s V_loop_break_cod_info_dref_off80
                                             - s V_loop_break_sfb) (-1
                                                                    + 
                                                                    s V_loop_break_cod_info_dref_off80
                                                                    - 
                                                                    s V_loop_break_sfb));
      (*-1 0*) F_max0_ge_0 (-1 + s V_loop_break_cod_info_dref_off80
                            - s V_loop_break_sfb)]
     (s V_loop_break_z
      + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
      + max0(s V_loop_break_cod_info_dref_off80 - s V_loop_break_sfb) <= z)%Q
   | 10 => (s V_loop_break_z
            + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84) <= z)%Q
   | 11 => (s V_loop_break_z + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 12 => (s V_loop_break_z + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 13 => (s V_loop_break_z + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 14 => (s V_loop_break_z + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 15 => (s V_loop_break_z + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 16 => (s V_loop_break_z + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 17 => hints
     [(*-4 0*) F_max0_monotonic (F_check_ge (12 - s V_loop_break_sfb) (11
                                                                    - s V_loop_break_sfb));
      (*-4 0*) F_max0_ge_0 (11 - s V_loop_break_sfb)]
     (s V_loop_break_z + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 18 => (s V_loop_break_z + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 19 => (s V_loop_break_z + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 20 => (-(3 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
            + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 21 => hints
     [(*-4 0*) F_max0_pre_decrement 1 (12 - s V_loop_break_sfb) (1)]
     (-(3 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
      + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 22 => ((1 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 23 => ((1 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 24 => ((1 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 25 => ((1 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 26 => ((1 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
            + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 27 => ((1 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
            + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 28 => ((1 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
            + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 29 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (3 - s V_loop_break_i)) (F_check_ge (0) (0))]
     (s V_loop_break_z + max0(3 - s V_loop_break_i)
      + (4 # 1) * max0(12 - s V_loop_break_sfb) <= z)%Q
   | 30 => hints
     [(*0 1*) F_max0_pre_decrement 1 (3 - s V_loop_break_i) (1)]
     ((1 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
      + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 31 => ((2 # 1) + s V_loop_break_z + max0(2 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 32 => ((2 # 1) + s V_loop_break_z + max0(2 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 33 => ((2 # 1) + s V_loop_break_z + max0(2 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 34 => ((2 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 35 => ((2 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 36 => ((2 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 37 => ((1 # 1) + s V_loop_break_z + max0(3 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 38 => ((2 # 1) + s V_loop_break_z + max0(2 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 39 => ((2 # 1) + s V_loop_break_z + max0(2 - s V_loop_break_i)
            + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 40 => hints
     [(*-2 0*) F_one; (*-1 0*) F_max0_ge_0 (2 - s V_loop_break_i);
      (*-4 0*) F_max0_ge_0 (11 - s V_loop_break_sfb)]
     ((2 # 1) + s V_loop_break_z + max0(2 - s V_loop_break_i)
      + (4 # 1) * max0(11 - s V_loop_break_sfb) <= z)%Q
   | 41 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (s V_loop_break_cod_info_dref_off80
                                       - s V_loop_break_sfb) (1)]
     (s V_loop_break_z
      + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
      + max0(s V_loop_break_cod_info_dref_off80 - s V_loop_break_sfb) <= z)%Q
   | 42 => ((1 # 1) + s V_loop_break_z
            + max0(-1 + s V_loop_break_cod_info_dref_off80
                   - s V_loop_break_sfb)
            + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84) <= z)%Q
   | 43 => ((1 # 1) + s V_loop_break_z
            + max0(-1 + s V_loop_break_cod_info_dref_off80
                   - s V_loop_break_sfb)
            + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84) <= z)%Q
   | 44 => ((1 # 1) + s V_loop_break_z
            + max0(-1 + s V_loop_break_cod_info_dref_off80
                   - s V_loop_break_sfb)
            + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84) <= z)%Q
   | 45 => ((1 # 1) + s V_loop_break_z
            + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
            + max0(s V_loop_break_cod_info_dref_off80 - s V_loop_break_sfb) <= z)%Q
   | 46 => ((1 # 1) + s V_loop_break_z
            + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
            + max0(s V_loop_break_cod_info_dref_off80 - s V_loop_break_sfb) <= z)%Q
   | 47 => ((1 # 1) + s V_loop_break_z
            + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
            + max0(s V_loop_break_cod_info_dref_off80 - s V_loop_break_sfb) <= z)%Q
   | 48 => (s V_loop_break_z
            + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84)
            + max0(s V_loop_break_cod_info_dref_off80 - s V_loop_break_sfb) <= z)%Q
   | 49 => ((1 # 1) + s V_loop_break_z
            + max0(-1 + s V_loop_break_cod_info_dref_off80
                   - s V_loop_break_sfb)
            + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84) <= z)%Q
   | 50 => ((1 # 1) + s V_loop_break_z
            + max0(-1 + s V_loop_break_cod_info_dref_off80
                   - s V_loop_break_sfb)
            + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84) <= z)%Q
   | 51 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_max0_ge_0 (-1 + s V_loop_break_cod_info_dref_off80
                            - s V_loop_break_sfb);
      (*-4 0*) F_max0_ge_0 (12 - s V_loop_break_cod_info_dref_off84)]
     ((1 # 1) + s V_loop_break_z
      + max0(-1 + s V_loop_break_cod_info_dref_off80 - s V_loop_break_sfb)
      + (4 # 1) * max0(12 - s V_loop_break_cod_info_dref_off84) <= z)%Q
   | 52 => (s V_loop_break_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_loop_break =>
    [mkPA Q (fun n z s => ai_loop_break n s /\ annot0_loop_break n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_loop_break (proc_start P_loop_break) s1 (proc_end P_loop_break) s2 ->
    (s2 V_loop_break_z <= (4 # 1) * max0(12
                                         - s1 V_loop_break_cod_info_dref_off84)
                          + max0(s1 V_loop_break_cod_info_dref_off80))%Q.
Proof.
  prove_bound ipa admissible_ipa P_loop_break.
Qed.
