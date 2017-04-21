Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ideaCfbEncrypt.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ideaCfbEncrypt_z := 1%positive.
Notation V_ideaCfbEncrypt__tmp := 2%positive.
Notation V_ideaCfbEncrypt_bufleft := 3%positive.
Notation V_ideaCfbEncrypt_context := 4%positive.
Notation V_ideaCfbEncrypt_count := 5%positive.
Notation V_ideaCfbEncrypt_dest := 6%positive.
Notation V_ideaCfbEncrypt_src := 7%positive.
Definition Pedges_ideaCfbEncrypt: list (edge proc) :=
  (EA 1 (AAssign V_ideaCfbEncrypt_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_ideaCfbEncrypt__tmp (Some (EVar V_ideaCfbEncrypt_count))) 3)::
  (EA 3 (AAssign V_ideaCfbEncrypt_bufleft None) 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_ideaCfbEncrypt__tmp) s) <=
  (eval (EVar V_ideaCfbEncrypt_bufleft) s))%Z)) 46)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_ideaCfbEncrypt__tmp) s) >
  (eval (EVar V_ideaCfbEncrypt_bufleft) s))%Z)) 6)::(EA 6 AWeaken 7)::
  (EA 7 (AAssign V_ideaCfbEncrypt__tmp
  (Some (ESub (EVar V_ideaCfbEncrypt__tmp)
  (EVar V_ideaCfbEncrypt_bufleft)))) 8)::(EA 8 ANone 9)::(EA 9 (AAssign
  V_ideaCfbEncrypt_bufleft (Some (EAdd (EVar V_ideaCfbEncrypt_bufleft)
  (ENum (-1))))) 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_ideaCfbEncrypt_bufleft) s) <> (eval (ENum (0))
  s))%Z)) 42)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_ideaCfbEncrypt_bufleft) s) = (eval (ENum (0))
  s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_ideaCfbEncrypt__tmp) s) >
  (eval (ENum (8)) s))%Z)) 26)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_ideaCfbEncrypt__tmp) s) <= (eval (ENum (8))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 18)::(EA 18 ANone 19)::
  (EA 19 (AAssign V_ideaCfbEncrypt__tmp
  (Some (EAdd (EVar V_ideaCfbEncrypt__tmp) (ENum (-1))))) 20)::
  (EA 20 AWeaken 21)::(EA 21 (AGuard
  (fun s => ((eval (EAdd (EVar V_ideaCfbEncrypt__tmp) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 23)::(EA 21 (AGuard
  (fun s => ((eval (EAdd (EVar V_ideaCfbEncrypt__tmp) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 22)::(EA 22 AWeaken 54)::(EA 23 AWeaken 24)::
  (EA 24 ANone 25)::(EA 25 (AAssign V_ideaCfbEncrypt_z (Some (EAdd (ENum (1))
  (EVar V_ideaCfbEncrypt_z)))) 18)::(EA 26 AWeaken 27)::(EA 27 (AAssign
  V_ideaCfbEncrypt_bufleft (Some (ENum (8)))) 28)::(EA 28 (AAssign
  V_ideaCfbEncrypt__tmp (Some (ESub (EVar V_ideaCfbEncrypt__tmp)
  (ENum (8))))) 29)::(EA 29 ANone 30)::(EA 30 ANone 31)::(EA 31 (AAssign
  V_ideaCfbEncrypt_bufleft (Some (EAdd (EVar V_ideaCfbEncrypt_bufleft)
  (ENum (-1))))) 32)::(EA 32 AWeaken 33)::(EA 33 (AGuard
  (fun s => ((eval (EAdd (EVar V_ideaCfbEncrypt_bufleft) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 39)::(EA 33 (AGuard
  (fun s => ((eval (EAdd (EVar V_ideaCfbEncrypt_bufleft) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 34)::(EA 34 AWeaken 35)::(EA 35 ANone 36)::
  (EA 36 ANone 37)::(EA 37 (AAssign V_ideaCfbEncrypt_z (Some (EAdd (ENum (1))
  (EVar V_ideaCfbEncrypt_z)))) 38)::(EA 38 AWeaken 15)::(EA 39 AWeaken 40)::
  (EA 40 ANone 41)::(EA 41 (AAssign V_ideaCfbEncrypt_z (Some (EAdd (ENum (1))
  (EVar V_ideaCfbEncrypt_z)))) 30)::(EA 42 AWeaken 43)::(EA 43 ANone 44)::
  (EA 44 ANone 45)::(EA 45 (AAssign V_ideaCfbEncrypt_z (Some (EAdd (ENum (1))
  (EVar V_ideaCfbEncrypt_z)))) 9)::(EA 46 AWeaken 47)::(EA 47 ANone 48)::
  (EA 48 (AAssign V_ideaCfbEncrypt__tmp
  (Some (EAdd (EVar V_ideaCfbEncrypt__tmp) (ENum (-1))))) 49)::
  (EA 49 AWeaken 50)::(EA 50 (AGuard
  (fun s => ((eval (EVar V_ideaCfbEncrypt__tmp) s) <> (eval (ENum (0))
  s))%Z)) 55)::(EA 50 (AGuard (fun s => ((eval (EVar V_ideaCfbEncrypt__tmp)
  s) = (eval (ENum (0)) s))%Z)) 51)::(EA 51 AWeaken 52)::(EA 52 ANone 53)::
  (EA 53 AWeaken 54)::(EA 55 AWeaken 56)::(EA 56 ANone 57)::
  (EA 57 ANone 58)::(EA 58 (AAssign V_ideaCfbEncrypt_z (Some (EAdd (ENum (1))
  (EVar V_ideaCfbEncrypt_z)))) 48)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ideaCfbEncrypt => Pedges_ideaCfbEncrypt
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ideaCfbEncrypt => 54
     end)%positive;
  var_global := var_global
}.

Definition ai_ideaCfbEncrypt (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 3 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 4 => (1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 5 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 6 => (1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp+ 1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0)%Z
   | 7 => (-1 * s V_ideaCfbEncrypt__tmp+ 1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 8 => (1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0)%Z
   | 9 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0)%Z
   | 10 => (-1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 11 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0)%Z
   | 12 => (-1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0)%Z
   | 13 => (-1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0)%Z
   | 14 => (-1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0)%Z
   | 15 => (-1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0)%Z
   | 16 => (-1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp + -8 <= 0)%Z
   | 17 => (1 * s V_ideaCfbEncrypt__tmp + -8 <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0)%Z
   | 18 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp + -8 <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0)%Z
   | 19 => (1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp + -8 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 20 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp + -7 <= 0)%Z
   | 21 => (1 * s V_ideaCfbEncrypt__tmp + -7 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 22 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0)%Z
   | 23 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp + -7 <= 0)%Z
   | 24 => (1 * s V_ideaCfbEncrypt__tmp + -7 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 25 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp + -7 <= 0)%Z
   | 26 => (1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 9 <= 0)%Z
   | 27 => (-1 * s V_ideaCfbEncrypt__tmp + 9 <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0)%Z
   | 28 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 9 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -8 <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft + 8 <= 0)%Z
   | 29 => (-1 * s V_ideaCfbEncrypt_bufleft + 8 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -8 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0)%Z
   | 30 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -8 <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0)%Z
   | 31 => (-1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -8 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 32 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -7 <= 0)%Z
   | 33 => (1 * s V_ideaCfbEncrypt_bufleft + -7 <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 34 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0)%Z
   | 35 => (-1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 36 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0)%Z
   | 37 => (-1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 38 => (-1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z + 1 <= 0)%Z
   | 39 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -7 <= 0)%Z
   | 40 => (1 * s V_ideaCfbEncrypt_bufleft + -7 <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 41 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbEncrypt_bufleft + -7 <= 0)%Z
   | 42 => (-1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 43 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0)%Z
   | 44 => (-1 * s V_ideaCfbEncrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 45 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp + 1 <= 0)%Z
   | 46 => (1 * s V_ideaCfbEncrypt_z <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp+ -1 * s V_ideaCfbEncrypt_bufleft <= 0)%Z
   | 47 => (1 * s V_ideaCfbEncrypt__tmp+ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 48 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp+ -1 * s V_ideaCfbEncrypt_bufleft <= 0)%Z
   | 49 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp+ -1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0)%Z
   | 50 => (1 * s V_ideaCfbEncrypt__tmp+ -1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 51 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp+ -1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp <= 0)%Z
   | 52 => (-1 * s V_ideaCfbEncrypt__tmp <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp+ -1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 53 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp+ -1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp <= 0)%Z
   | 54 => (1 * s V_ideaCfbEncrypt__tmp + -1 <= 0 /\ -1 * s V_ideaCfbEncrypt_bufleft <= 0 /\ -1 * s V_ideaCfbEncrypt__tmp <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 55 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp+ -1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0)%Z
   | 56 => (1 * s V_ideaCfbEncrypt__tmp+ -1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | 57 => (-1 * s V_ideaCfbEncrypt_z <= 0 /\ 1 * s V_ideaCfbEncrypt__tmp+ -1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0)%Z
   | 58 => (1 * s V_ideaCfbEncrypt__tmp+ -1 * s V_ideaCfbEncrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbEncrypt_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ideaCfbEncrypt (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_ideaCfbEncrypt_count <= z)%Q
   | 2 => (s V_ideaCfbEncrypt_count - max0(-s V_ideaCfbEncrypt_z) <= z)%Q
   | 3 => (s V_ideaCfbEncrypt__tmp - max0(-s V_ideaCfbEncrypt_z) <= z)%Q
   | 4 => (s V_ideaCfbEncrypt__tmp - max0(-s V_ideaCfbEncrypt_z) <= z)%Q
   | 5 => (s V_ideaCfbEncrypt__tmp - max0(-s V_ideaCfbEncrypt_z) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_ideaCfbEncrypt_z) (0))) (F_max0_ge_0 (-
                                                                    s V_ideaCfbEncrypt_z))]
     (s V_ideaCfbEncrypt__tmp - max0(-s V_ideaCfbEncrypt_z) <= z)%Q
   | 7 => (s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_z <= z)%Q
   | 8 => (s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
           + s V_ideaCfbEncrypt_z <= z)%Q
   | 9 => (s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
           + s V_ideaCfbEncrypt_z <= z)%Q
   | 10 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 11 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 12 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 13 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 14 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 15 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 16 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_ideaCfbEncrypt_bufleft)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ideaCfbEncrypt_bufleft) (0))) (F_max0_ge_0 (s V_ideaCfbEncrypt_bufleft));
      (*-0.375 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_ideaCfbEncrypt__tmp)) (F_check_ge (0) (0));
      (*-0.375 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ideaCfbEncrypt__tmp) (0))) (F_max0_ge_0 (s V_ideaCfbEncrypt__tmp));
      (*-0.375 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                     - s V_ideaCfbEncrypt__tmp)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
      + s V_ideaCfbEncrypt_z <= z)%Q
   | 17 => ((1 # 1) + (5 # 8) * s V_ideaCfbEncrypt__tmp
            + s V_ideaCfbEncrypt_z
            - (3 # 8) * max0(8 - s V_ideaCfbEncrypt__tmp) <= z)%Q
   | 18 => ((1 # 1) + (5 # 8) * s V_ideaCfbEncrypt__tmp
            + s V_ideaCfbEncrypt_z
            - (3 # 8) * max0(8 - s V_ideaCfbEncrypt__tmp) <= z)%Q
   | 19 => ((1 # 1) + (5 # 8) * s V_ideaCfbEncrypt__tmp
            + s V_ideaCfbEncrypt_z
            - (3 # 8) * max0(8 - s V_ideaCfbEncrypt__tmp) <= z)%Q
   | 20 => hints
     [(*-0.375 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                   - 
                                                                   s V_ideaCfbEncrypt__tmp) (0))) (F_max0_ge_0 (7
                                                                    - s V_ideaCfbEncrypt__tmp))]
     ((13 # 8) + (5 # 8) * s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_z
      - (3 # 8) * max0(7 - s V_ideaCfbEncrypt__tmp) <= z)%Q
   | 21 => (-(1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_z <= z)%Q
   | 22 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-1 + s V_ideaCfbEncrypt__tmp) (-9
                                                                    + s V_ideaCfbEncrypt__tmp));
      (*-1 0*) F_max0_ge_0 (-9 + s V_ideaCfbEncrypt__tmp);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_ideaCfbEncrypt__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_ideaCfbEncrypt__tmp))]
     (-(1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_z <= z)%Q
   | 23 => hints
     [(*-0.375 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                       - s V_ideaCfbEncrypt__tmp)) (F_check_ge (8
                                                                    - s V_ideaCfbEncrypt__tmp) (0))]
     (-(1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_z <= z)%Q
   | 24 => ((2 # 1) + (5 # 8) * s V_ideaCfbEncrypt__tmp
            + s V_ideaCfbEncrypt_z
            - (3 # 8) * max0(8 - s V_ideaCfbEncrypt__tmp) <= z)%Q
   | 25 => ((2 # 1) + (5 # 8) * s V_ideaCfbEncrypt__tmp
            + s V_ideaCfbEncrypt_z
            - (3 # 8) * max0(8 - s V_ideaCfbEncrypt__tmp) <= z)%Q
   | 26 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_ideaCfbEncrypt_bufleft)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ideaCfbEncrypt_bufleft) (0))) (F_max0_ge_0 (s V_ideaCfbEncrypt_bufleft))]
     ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
      + s V_ideaCfbEncrypt_z <= z)%Q
   | 27 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_z <= z)%Q
   | 28 => (-(7 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 29 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 30 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 31 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 32 => ((2 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 33 => ((2 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 34 => ((2 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 35 => ((2 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 36 => ((2 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 37 => ((2 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 38 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 39 => ((2 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 40 => ((2 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 41 => ((2 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 42 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 43 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 44 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 45 => ((1 # 1) + s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft
            + s V_ideaCfbEncrypt_z <= z)%Q
   | 46 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_ideaCfbEncrypt_z) (0))) (F_max0_ge_0 (-
                                                                    s V_ideaCfbEncrypt_z));
      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-s V_ideaCfbEncrypt__tmp
                                                    + s V_ideaCfbEncrypt_bufleft)) (F_check_ge (-
                                                                    s V_ideaCfbEncrypt__tmp
                                                                    + s V_ideaCfbEncrypt_bufleft) (0))]
     (s V_ideaCfbEncrypt__tmp - max0(-s V_ideaCfbEncrypt_z) <= z)%Q
   | 47 => ((1 # 2) * s V_ideaCfbEncrypt__tmp
            + (1 # 2) * s V_ideaCfbEncrypt_bufleft + s V_ideaCfbEncrypt_z
            - (1 # 2) * max0(-s V_ideaCfbEncrypt__tmp
                             + s V_ideaCfbEncrypt_bufleft) <= z)%Q
   | 48 => ((1 # 2) * s V_ideaCfbEncrypt__tmp
            + (1 # 2) * s V_ideaCfbEncrypt_bufleft + s V_ideaCfbEncrypt_z
            - (1 # 2) * max0(-s V_ideaCfbEncrypt__tmp
                             + s V_ideaCfbEncrypt_bufleft) <= z)%Q
   | 49 => hints
     [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-s V_ideaCfbEncrypt__tmp
                                                    + s V_ideaCfbEncrypt_bufleft)) (F_check_ge (-
                                                                    s V_ideaCfbEncrypt__tmp
                                                                    + s V_ideaCfbEncrypt_bufleft) (0))]
     ((1 # 2) + (1 # 2) * s V_ideaCfbEncrypt__tmp
      + (1 # 2) * s V_ideaCfbEncrypt_bufleft + s V_ideaCfbEncrypt_z
      - (1 # 2) * max0(-1 - s V_ideaCfbEncrypt__tmp
                       + s V_ideaCfbEncrypt_bufleft) <= z)%Q
   | 50 => ((1 # 2) + s V_ideaCfbEncrypt_bufleft + s V_ideaCfbEncrypt_z
            - (1 # 2) * max0(-1 - s V_ideaCfbEncrypt__tmp
                             + s V_ideaCfbEncrypt_bufleft)
            - (1 # 2) * max0(-s V_ideaCfbEncrypt__tmp
                             + s V_ideaCfbEncrypt_bufleft) <= z)%Q
   | 51 => ((1 # 2) + s V_ideaCfbEncrypt_bufleft + s V_ideaCfbEncrypt_z
            - (1 # 2) * max0(-1 - s V_ideaCfbEncrypt__tmp
                             + s V_ideaCfbEncrypt_bufleft)
            - (1 # 2) * max0(-s V_ideaCfbEncrypt__tmp
                             + s V_ideaCfbEncrypt_bufleft) <= z)%Q
   | 52 => ((1 # 2) + s V_ideaCfbEncrypt_bufleft + s V_ideaCfbEncrypt_z
            - (1 # 2) * max0(-1 - s V_ideaCfbEncrypt__tmp
                             + s V_ideaCfbEncrypt_bufleft)
            - (1 # 2) * max0(-s V_ideaCfbEncrypt__tmp
                             + s V_ideaCfbEncrypt_bufleft) <= z)%Q
   | 53 => hints
     [(*-1.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_ideaCfbEncrypt__tmp)) (F_check_ge (0) (0));
      (*-1.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ideaCfbEncrypt__tmp) (0))) (F_max0_ge_0 (s V_ideaCfbEncrypt__tmp));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_ideaCfbEncrypt__tmp
                                                                 + s V_ideaCfbEncrypt_bufleft) (0))) (F_max0_ge_0 (-
                                                                    s V_ideaCfbEncrypt__tmp
                                                                    + s V_ideaCfbEncrypt_bufleft));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                     - s V_ideaCfbEncrypt__tmp)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                   - 
                                                                   s V_ideaCfbEncrypt__tmp) (0))) (F_max0_ge_0 (8
                                                                    - s V_ideaCfbEncrypt__tmp));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                 - s V_ideaCfbEncrypt__tmp
                                                                 + s V_ideaCfbEncrypt_bufleft) (0))) (F_max0_ge_0 (-1
                                                                    - s V_ideaCfbEncrypt__tmp
                                                                    + s V_ideaCfbEncrypt_bufleft))]
     ((1 # 2) + s V_ideaCfbEncrypt_bufleft + s V_ideaCfbEncrypt_z
      - (1 # 2) * max0(-1 - s V_ideaCfbEncrypt__tmp
                       + s V_ideaCfbEncrypt_bufleft)
      - (1 # 2) * max0(-s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft) <= z)%Q
   | 54 => (s V_ideaCfbEncrypt_z <= z)%Q
   | 55 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                 - s V_ideaCfbEncrypt__tmp
                                                                 + s V_ideaCfbEncrypt_bufleft) (0))) (F_max0_ge_0 (-1
                                                                    - s V_ideaCfbEncrypt__tmp
                                                                    + s V_ideaCfbEncrypt_bufleft))]
     ((1 # 2) + s V_ideaCfbEncrypt_bufleft + s V_ideaCfbEncrypt_z
      - (1 # 2) * max0(-1 - s V_ideaCfbEncrypt__tmp
                       + s V_ideaCfbEncrypt_bufleft)
      - (1 # 2) * max0(-s V_ideaCfbEncrypt__tmp + s V_ideaCfbEncrypt_bufleft) <= z)%Q
   | 56 => ((1 # 1) + (1 # 2) * s V_ideaCfbEncrypt__tmp
            + (1 # 2) * s V_ideaCfbEncrypt_bufleft + s V_ideaCfbEncrypt_z
            - (1 # 2) * max0(-s V_ideaCfbEncrypt__tmp
                             + s V_ideaCfbEncrypt_bufleft) <= z)%Q
   | 57 => ((1 # 1) + (1 # 2) * s V_ideaCfbEncrypt__tmp
            + (1 # 2) * s V_ideaCfbEncrypt_bufleft + s V_ideaCfbEncrypt_z
            - (1 # 2) * max0(-s V_ideaCfbEncrypt__tmp
                             + s V_ideaCfbEncrypt_bufleft) <= z)%Q
   | 58 => ((1 # 1) + (1 # 2) * s V_ideaCfbEncrypt__tmp
            + (1 # 2) * s V_ideaCfbEncrypt_bufleft + s V_ideaCfbEncrypt_z
            - (1 # 2) * max0(-s V_ideaCfbEncrypt__tmp
                             + s V_ideaCfbEncrypt_bufleft) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ideaCfbEncrypt =>
    [mkPA Q (fun n z s => ai_ideaCfbEncrypt n s /\ annot0_ideaCfbEncrypt n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ideaCfbEncrypt (proc_start P_ideaCfbEncrypt) s1 (proc_end P_ideaCfbEncrypt) s2 ->
    (s2 V_ideaCfbEncrypt_z <= s1 V_ideaCfbEncrypt_count)%Q.
Proof.
  prove_bound ipa admissible_ipa P_ideaCfbEncrypt.
Qed.
