Require Import pasta.Pasta.

Inductive proc: Type :=
  P_inflate_fixed.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_inflate_fixed_z := 1%positive.
Notation V_inflate_fixed__tmp := 2%positive.
Notation V_inflate_fixed_i := 3%positive.
Definition Pedges_inflate_fixed: list (edge proc) :=
  (EA 1 (AAssign V_inflate_fixed_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_inflate_fixed_i (Some (ENum (0)))) 3)::(EA 3 ANone 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_inflate_fixed_i) s) <
  (eval (ENum (144)) s))%Z)) 73)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_inflate_fixed_i) s) >= (eval (ENum (144))
  s))%Z)) 6)::(EA 6 AWeaken 7)::(EA 7 ANone 8)::(EA 8 AWeaken 9)::
  (EA 9 (AGuard (fun s => ((eval (EVar V_inflate_fixed_i) s) <
  (eval (ENum (256)) s))%Z)) 66)::(EA 9 (AGuard
  (fun s => ((eval (EVar V_inflate_fixed_i) s) >= (eval (ENum (256))
  s))%Z)) 10)::(EA 10 AWeaken 11)::(EA 11 ANone 12)::(EA 12 AWeaken 13)::
  (EA 13 (AGuard (fun s => ((eval (EVar V_inflate_fixed_i) s) <
  (eval (ENum (280)) s))%Z)) 59)::(EA 13 (AGuard
  (fun s => ((eval (EVar V_inflate_fixed_i) s) >= (eval (ENum (280))
  s))%Z)) 14)::(EA 14 AWeaken 15)::(EA 15 ANone 16)::(EA 16 AWeaken 17)::
  (EA 17 (AGuard (fun s => ((eval (EVar V_inflate_fixed_i) s) <
  (eval (ENum (288)) s))%Z)) 52)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_inflate_fixed_i) s) >= (eval (ENum (288))
  s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 (AAssign V_inflate_fixed_i
  None) 20)::(EA 20 AWeaken 21)::(EA 21 ANone 48)::(EA 21 ANone 22)::
  (EA 22 (AAssign V_inflate_fixed_i (Some (ENum (0)))) 23)::
  (EA 23 ANone 24)::(EA 24 AWeaken 25)::(EA 25 (AGuard
  (fun s => ((eval (EVar V_inflate_fixed_i) s) < (eval (ENum (30))
  s))%Z)) 41)::(EA 25 (AGuard (fun s => ((eval (EVar V_inflate_fixed_i) s) >=
  (eval (ENum (30)) s))%Z)) 26)::(EA 26 AWeaken 27)::(EA 27 (AAssign
  V_inflate_fixed_i None) 28)::(EA 28 AWeaken 29)::(EA 29 ANone 38)::
  (EA 29 ANone 30)::(EA 30 AWeaken 31)::(EA 31 ANone 35)::(EA 31 ANone 32)::
  (EA 32 (AAssign V_inflate_fixed__tmp (Some (ENum (0)))) 33)::
  (EA 33 ANone 34)::(EA 34 AWeaken 51)::(EA 35 (AAssign V_inflate_fixed__tmp
  (Some (ENum (1)))) 36)::(EA 36 ANone 37)::(EA 37 AWeaken 51)::
  (EA 38 (AAssign V_inflate_fixed__tmp (Some (EVar V_inflate_fixed_i))) 39)::
  (EA 39 ANone 40)::(EA 40 AWeaken 51)::(EA 41 AWeaken 42)::
  (EA 42 ANone 43)::(EA 43 (AAssign V_inflate_fixed_i
  (Some (EAdd (EVar V_inflate_fixed_i) (ENum (1))))) 44)::(EA 44 ANone 45)::
  (EA 45 ANone 46)::(EA 46 (AAssign V_inflate_fixed_z (Some (EAdd (ENum (1))
  (EVar V_inflate_fixed_z)))) 47)::(EA 47 AWeaken 25)::(EA 48 (AAssign
  V_inflate_fixed__tmp (Some (EVar V_inflate_fixed_i))) 49)::
  (EA 49 ANone 50)::(EA 50 AWeaken 51)::(EA 52 AWeaken 53)::
  (EA 53 ANone 54)::(EA 54 (AAssign V_inflate_fixed_i
  (Some (EAdd (EVar V_inflate_fixed_i) (ENum (1))))) 55)::(EA 55 ANone 56)::
  (EA 56 ANone 57)::(EA 57 (AAssign V_inflate_fixed_z (Some (EAdd (ENum (1))
  (EVar V_inflate_fixed_z)))) 58)::(EA 58 AWeaken 17)::(EA 59 AWeaken 60)::
  (EA 60 ANone 61)::(EA 61 (AAssign V_inflate_fixed_i
  (Some (EAdd (EVar V_inflate_fixed_i) (ENum (1))))) 62)::(EA 62 ANone 63)::
  (EA 63 ANone 64)::(EA 64 (AAssign V_inflate_fixed_z (Some (EAdd (ENum (1))
  (EVar V_inflate_fixed_z)))) 65)::(EA 65 AWeaken 13)::(EA 66 AWeaken 67)::
  (EA 67 ANone 68)::(EA 68 (AAssign V_inflate_fixed_i
  (Some (EAdd (EVar V_inflate_fixed_i) (ENum (1))))) 69)::(EA 69 ANone 70)::
  (EA 70 ANone 71)::(EA 71 (AAssign V_inflate_fixed_z (Some (EAdd (ENum (1))
  (EVar V_inflate_fixed_z)))) 72)::(EA 72 AWeaken 9)::(EA 73 AWeaken 74)::
  (EA 74 ANone 75)::(EA 75 (AAssign V_inflate_fixed_i
  (Some (EAdd (EVar V_inflate_fixed_i) (ENum (1))))) 76)::(EA 76 ANone 77)::
  (EA 77 ANone 78)::(EA 78 (AAssign V_inflate_fixed_z (Some (EAdd (ENum (1))
  (EVar V_inflate_fixed_z)))) 79)::(EA 79 AWeaken 5)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_inflate_fixed => Pedges_inflate_fixed
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_inflate_fixed => 51
     end)%positive;
  var_global := var_global
}.

Definition ai_inflate_fixed (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 3 => (-1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i <= 0 /\ -1 * s V_inflate_fixed_i <= 0)%Z
   | 4 => (-1 * s V_inflate_fixed_i <= 0 /\ 1 * s V_inflate_fixed_i <= 0 /\ 1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 5 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i <= 0 /\ 1 * s V_inflate_fixed_i + -144 <= 0)%Z
   | 6 => (1 * s V_inflate_fixed_i + -144 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 144 <= 0)%Z
   | 7 => (-1 * s V_inflate_fixed_i + 144 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i + -144 <= 0)%Z
   | 8 => (1 * s V_inflate_fixed_i + -144 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 144 <= 0)%Z
   | 9 => (-1 * s V_inflate_fixed_i + 144 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i + -256 <= 0)%Z
   | 10 => (1 * s V_inflate_fixed_i + -256 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 256 <= 0)%Z
   | 11 => (-1 * s V_inflate_fixed_i + 256 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i + -256 <= 0)%Z
   | 12 => (1 * s V_inflate_fixed_i + -256 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 256 <= 0)%Z
   | 13 => (-1 * s V_inflate_fixed_i + 256 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i + -280 <= 0)%Z
   | 14 => (1 * s V_inflate_fixed_i + -280 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 280 <= 0)%Z
   | 15 => (-1 * s V_inflate_fixed_i + 280 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i + -280 <= 0)%Z
   | 16 => (1 * s V_inflate_fixed_i + -280 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 280 <= 0)%Z
   | 17 => (-1 * s V_inflate_fixed_i + 280 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i + -288 <= 0)%Z
   | 18 => (1 * s V_inflate_fixed_i + -288 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 288 <= 0)%Z
   | 19 => (-1 * s V_inflate_fixed_i + 288 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i + -288 <= 0)%Z
   | 20 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 21 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 22 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 23 => (-1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i <= 0 /\ -1 * s V_inflate_fixed_i <= 0)%Z
   | 24 => (-1 * s V_inflate_fixed_i <= 0 /\ 1 * s V_inflate_fixed_i <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 25 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i <= 0 /\ 1 * s V_inflate_fixed_i + -30 <= 0)%Z
   | 26 => (1 * s V_inflate_fixed_i + -30 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 30 <= 0)%Z
   | 27 => (-1 * s V_inflate_fixed_i + 30 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i + -30 <= 0)%Z
   | 28 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 29 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 30 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 31 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 32 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 33 => (-1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed__tmp <= 0 /\ -1 * s V_inflate_fixed__tmp <= 0)%Z
   | 34 => (-1 * s V_inflate_fixed__tmp <= 0 /\ 1 * s V_inflate_fixed__tmp <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 35 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 36 => (-1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed__tmp + -1 <= 0 /\ -1 * s V_inflate_fixed__tmp + 1 <= 0)%Z
   | 37 => (-1 * s V_inflate_fixed__tmp + 1 <= 0 /\ 1 * s V_inflate_fixed__tmp + -1 <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 38 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 39 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 40 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 41 => (-1 * s V_inflate_fixed_i <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i + -29 <= 0)%Z
   | 42 => (1 * s V_inflate_fixed_i + -29 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i <= 0)%Z
   | 43 => (-1 * s V_inflate_fixed_i <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i + -29 <= 0)%Z
   | 44 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 1 <= 0 /\ 1 * s V_inflate_fixed_i + -30 <= 0)%Z
   | 45 => (1 * s V_inflate_fixed_i + -30 <= 0 /\ -1 * s V_inflate_fixed_i + 1 <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 46 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 1 <= 0 /\ 1 * s V_inflate_fixed_i + -30 <= 0)%Z
   | 47 => (1 * s V_inflate_fixed_i + -30 <= 0 /\ -1 * s V_inflate_fixed_i + 1 <= 0 /\ -1 * s V_inflate_fixed_z + 1 <= 0)%Z
   | 48 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 49 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 50 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 51 => (-1 * s V_inflate_fixed_z <= 0)%Z
   | 52 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 280 <= 0 /\ 1 * s V_inflate_fixed_i + -287 <= 0)%Z
   | 53 => (1 * s V_inflate_fixed_i + -287 <= 0 /\ -1 * s V_inflate_fixed_i + 280 <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 54 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 280 <= 0 /\ 1 * s V_inflate_fixed_i + -287 <= 0)%Z
   | 55 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 281 <= 0 /\ 1 * s V_inflate_fixed_i + -288 <= 0)%Z
   | 56 => (1 * s V_inflate_fixed_i + -288 <= 0 /\ -1 * s V_inflate_fixed_i + 281 <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 57 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 281 <= 0 /\ 1 * s V_inflate_fixed_i + -288 <= 0)%Z
   | 58 => (1 * s V_inflate_fixed_i + -288 <= 0 /\ -1 * s V_inflate_fixed_i + 281 <= 0 /\ -1 * s V_inflate_fixed_z + 1 <= 0)%Z
   | 59 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 256 <= 0 /\ 1 * s V_inflate_fixed_i + -279 <= 0)%Z
   | 60 => (1 * s V_inflate_fixed_i + -279 <= 0 /\ -1 * s V_inflate_fixed_i + 256 <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 61 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 256 <= 0 /\ 1 * s V_inflate_fixed_i + -279 <= 0)%Z
   | 62 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 257 <= 0 /\ 1 * s V_inflate_fixed_i + -280 <= 0)%Z
   | 63 => (1 * s V_inflate_fixed_i + -280 <= 0 /\ -1 * s V_inflate_fixed_i + 257 <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 64 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 257 <= 0 /\ 1 * s V_inflate_fixed_i + -280 <= 0)%Z
   | 65 => (1 * s V_inflate_fixed_i + -280 <= 0 /\ -1 * s V_inflate_fixed_i + 257 <= 0 /\ -1 * s V_inflate_fixed_z + 1 <= 0)%Z
   | 66 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 144 <= 0 /\ 1 * s V_inflate_fixed_i + -255 <= 0)%Z
   | 67 => (1 * s V_inflate_fixed_i + -255 <= 0 /\ -1 * s V_inflate_fixed_i + 144 <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 68 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 144 <= 0 /\ 1 * s V_inflate_fixed_i + -255 <= 0)%Z
   | 69 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 145 <= 0 /\ 1 * s V_inflate_fixed_i + -256 <= 0)%Z
   | 70 => (1 * s V_inflate_fixed_i + -256 <= 0 /\ -1 * s V_inflate_fixed_i + 145 <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 71 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 145 <= 0 /\ 1 * s V_inflate_fixed_i + -256 <= 0)%Z
   | 72 => (1 * s V_inflate_fixed_i + -256 <= 0 /\ -1 * s V_inflate_fixed_i + 145 <= 0 /\ -1 * s V_inflate_fixed_z + 1 <= 0)%Z
   | 73 => (-1 * s V_inflate_fixed_i <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i + -143 <= 0)%Z
   | 74 => (1 * s V_inflate_fixed_i + -143 <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i <= 0)%Z
   | 75 => (-1 * s V_inflate_fixed_i <= 0 /\ -1 * s V_inflate_fixed_z <= 0 /\ 1 * s V_inflate_fixed_i + -143 <= 0)%Z
   | 76 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 1 <= 0 /\ 1 * s V_inflate_fixed_i + -144 <= 0)%Z
   | 77 => (1 * s V_inflate_fixed_i + -144 <= 0 /\ -1 * s V_inflate_fixed_i + 1 <= 0 /\ -1 * s V_inflate_fixed_z <= 0)%Z
   | 78 => (-1 * s V_inflate_fixed_z <= 0 /\ -1 * s V_inflate_fixed_i + 1 <= 0 /\ 1 * s V_inflate_fixed_i + -144 <= 0)%Z
   | 79 => (1 * s V_inflate_fixed_i + -144 <= 0 /\ -1 * s V_inflate_fixed_i + 1 <= 0 /\ -1 * s V_inflate_fixed_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_inflate_fixed (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((318 # 1) <= z)%Q
   | 2 => ((318 # 1) + s V_inflate_fixed_z <= z)%Q
   | 3 => ((30 # 1) + s V_inflate_fixed_z + max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 4 => ((30 # 1) + s V_inflate_fixed_z + max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 5 => ((30 # 1) + s V_inflate_fixed_z + max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 6 => ((30 # 1) + s V_inflate_fixed_z + max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 7 => ((30 # 1) + s V_inflate_fixed_z + max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 8 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (288 - s V_inflate_fixed_i) (1)]
     ((30 # 1) + s V_inflate_fixed_z + max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 9 => ((61 # 2) + s V_inflate_fixed_z
           + (1 # 2) * max0(287 - s V_inflate_fixed_i)
           + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 10 => ((61 # 2) + s V_inflate_fixed_z
            + (1 # 2) * max0(287 - s V_inflate_fixed_i)
            + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 11 => ((61 # 2) + s V_inflate_fixed_z
            + (1 # 2) * max0(287 - s V_inflate_fixed_i)
            + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 12 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (287
                                                     - s V_inflate_fixed_i)) (F_check_ge (287
                                                                    - s V_inflate_fixed_i) (0))]
     ((61 # 2) + s V_inflate_fixed_z
      + (1 # 2) * max0(287 - s V_inflate_fixed_i)
      + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 13 => ((174 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 14 => ((174 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 15 => ((174 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 16 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (288
                                                     - s V_inflate_fixed_i)) (F_check_ge (288
                                                                    - s V_inflate_fixed_i) (0))]
     ((174 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
      + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 17 => ((318 # 1) - s V_inflate_fixed_i + s V_inflate_fixed_z <= z)%Q
   | 18 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (288 - s V_inflate_fixed_i) (287
                                                                    - s V_inflate_fixed_i));
      (*-1 0*) F_max0_ge_0 (287 - s V_inflate_fixed_i);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (288
                                                               - s V_inflate_fixed_i) (0))) (F_max0_ge_0 (288
                                                                    - s V_inflate_fixed_i))]
     ((318 # 1) - s V_inflate_fixed_i + s V_inflate_fixed_z <= z)%Q
   | 19 => ((30 # 1) + s V_inflate_fixed_z <= z)%Q
   | 20 => ((30 # 1) + s V_inflate_fixed_z <= z)%Q
   | 21 => ((30 # 1) + s V_inflate_fixed_z <= z)%Q
   | 22 => ((30 # 1) + s V_inflate_fixed_z <= z)%Q
   | 23 => (s V_inflate_fixed_z + max0(30 - s V_inflate_fixed_i) <= z)%Q
   | 24 => (s V_inflate_fixed_z + max0(30 - s V_inflate_fixed_i) <= z)%Q
   | 25 => (s V_inflate_fixed_z + max0(30 - s V_inflate_fixed_i) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (30 - s V_inflate_fixed_i) (29
                                                                    - s V_inflate_fixed_i));
      (*-1 0*) F_max0_ge_0 (29 - s V_inflate_fixed_i)]
     (s V_inflate_fixed_z + max0(30 - s V_inflate_fixed_i) <= z)%Q
   | 27 => (s V_inflate_fixed_z <= z)%Q
   | 28 => (s V_inflate_fixed_z <= z)%Q
   | 29 => (s V_inflate_fixed_z <= z)%Q
   | 30 => (s V_inflate_fixed_z <= z)%Q
   | 31 => (s V_inflate_fixed_z <= z)%Q
   | 32 => (s V_inflate_fixed_z <= z)%Q
   | 33 => (s V_inflate_fixed_z <= z)%Q
   | 34 => (s V_inflate_fixed_z <= z)%Q
   | 35 => (s V_inflate_fixed_z <= z)%Q
   | 36 => (s V_inflate_fixed_z <= z)%Q
   | 37 => (s V_inflate_fixed_z <= z)%Q
   | 38 => (s V_inflate_fixed_z <= z)%Q
   | 39 => (s V_inflate_fixed_z <= z)%Q
   | 40 => (s V_inflate_fixed_z <= z)%Q
   | 41 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (30 - s V_inflate_fixed_i) (1)]
     (s V_inflate_fixed_z + max0(30 - s V_inflate_fixed_i) <= z)%Q
   | 42 => ((1 # 1) + s V_inflate_fixed_z + max0(29 - s V_inflate_fixed_i) <= z)%Q
   | 43 => ((1 # 1) + s V_inflate_fixed_z + max0(29 - s V_inflate_fixed_i) <= z)%Q
   | 44 => ((1 # 1) + s V_inflate_fixed_z + max0(30 - s V_inflate_fixed_i) <= z)%Q
   | 45 => ((1 # 1) + s V_inflate_fixed_z + max0(30 - s V_inflate_fixed_i) <= z)%Q
   | 46 => ((1 # 1) + s V_inflate_fixed_z + max0(30 - s V_inflate_fixed_i) <= z)%Q
   | 47 => (s V_inflate_fixed_z + max0(30 - s V_inflate_fixed_i) <= z)%Q
   | 48 => ((30 # 1) + s V_inflate_fixed_z <= z)%Q
   | 49 => ((30 # 1) + s V_inflate_fixed_z <= z)%Q
   | 50 => hints
     [(*-30 0*) F_one]
     ((30 # 1) + s V_inflate_fixed_z <= z)%Q
   | 51 => (s V_inflate_fixed_z <= z)%Q
   | 52 => ((318 # 1) - s V_inflate_fixed_i + s V_inflate_fixed_z <= z)%Q
   | 53 => ((318 # 1) - s V_inflate_fixed_i + s V_inflate_fixed_z <= z)%Q
   | 54 => ((318 # 1) - s V_inflate_fixed_i + s V_inflate_fixed_z <= z)%Q
   | 55 => ((319 # 1) - s V_inflate_fixed_i + s V_inflate_fixed_z <= z)%Q
   | 56 => ((319 # 1) - s V_inflate_fixed_i + s V_inflate_fixed_z <= z)%Q
   | 57 => ((319 # 1) - s V_inflate_fixed_i + s V_inflate_fixed_z <= z)%Q
   | 58 => ((318 # 1) - s V_inflate_fixed_i + s V_inflate_fixed_z <= z)%Q
   | 59 => hints
     [(*0 0.5*) F_max0_pre_decrement 1 (288 - s V_inflate_fixed_i) (1)]
     ((174 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
      + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 60 => ((349 # 2) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(287 - s V_inflate_fixed_i) <= z)%Q
   | 61 => ((349 # 2) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(287 - s V_inflate_fixed_i) <= z)%Q
   | 62 => ((175 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 63 => ((175 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 64 => ((175 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 65 => ((174 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 66 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_ge_arg (288
                                                     - s V_inflate_fixed_i)) (F_check_ge (288
                                                                    - s V_inflate_fixed_i) (0))]
     ((61 # 2) + s V_inflate_fixed_z
      + (1 # 2) * max0(287 - s V_inflate_fixed_i)
      + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 67 => ((349 # 2) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(287 - s V_inflate_fixed_i) <= z)%Q
   | 68 => ((349 # 2) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(287 - s V_inflate_fixed_i) <= z)%Q
   | 69 => ((175 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 70 => ((175 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 71 => ((175 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
            + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 72 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (287
                                                                 - s V_inflate_fixed_i) (0))) (F_max0_ge_0 (287
                                                                    - s V_inflate_fixed_i))]
     ((174 # 1) - (1 # 2) * s V_inflate_fixed_i + s V_inflate_fixed_z
      + (1 # 2) * max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 73 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (288 - s V_inflate_fixed_i) (1)]
     ((30 # 1) + s V_inflate_fixed_z + max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 74 => ((31 # 1) + s V_inflate_fixed_z + max0(287 - s V_inflate_fixed_i) <= z)%Q
   | 75 => ((31 # 1) + s V_inflate_fixed_z + max0(287 - s V_inflate_fixed_i) <= z)%Q
   | 76 => ((31 # 1) + s V_inflate_fixed_z + max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 77 => ((31 # 1) + s V_inflate_fixed_z + max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 78 => ((31 # 1) + s V_inflate_fixed_z + max0(288 - s V_inflate_fixed_i) <= z)%Q
   | 79 => ((30 # 1) + s V_inflate_fixed_z + max0(288 - s V_inflate_fixed_i) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_inflate_fixed =>
    [mkPA Q (fun n z s => ai_inflate_fixed n s /\ annot0_inflate_fixed n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_inflate_fixed (proc_start P_inflate_fixed) s1 (proc_end P_inflate_fixed) s2 ->
    (s2 V_inflate_fixed_z <= (318 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_inflate_fixed.
Qed.
