Require Import pasta.Pasta.

Inductive proc: Type :=
  P_ideaCfbDecrypt.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_ideaCfbDecrypt_z := 1%positive.
Notation V_ideaCfbDecrypt__tmp := 2%positive.
Notation V_ideaCfbDecrypt_bufleft := 3%positive.
Notation V_ideaCfbDecrypt_ideaCfbDecrypt_dot_bufptr_dref := 4%positive.
Notation V_ideaCfbDecrypt_t := 5%positive.
Notation V_ideaCfbDecrypt_context := 6%positive.
Notation V_ideaCfbDecrypt_count := 7%positive.
Notation V_ideaCfbDecrypt_dest := 8%positive.
Notation V_ideaCfbDecrypt_src := 9%positive.
Definition Pedges_ideaCfbDecrypt: list (edge proc) :=
  (EA 1 (AAssign V_ideaCfbDecrypt_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_ideaCfbDecrypt__tmp (Some (EVar V_ideaCfbDecrypt_count))) 3)::
  (EA 3 (AAssign V_ideaCfbDecrypt_bufleft None) 4)::(EA 4 AWeaken 5)::
  (EA 5 (AGuard (fun s => ((eval (EVar V_ideaCfbDecrypt__tmp) s) <=
  (eval (EVar V_ideaCfbDecrypt_bufleft) s))%Z)) 52)::(EA 5 (AGuard
  (fun s => ((eval (EVar V_ideaCfbDecrypt__tmp) s) >
  (eval (EVar V_ideaCfbDecrypt_bufleft) s))%Z)) 6)::(EA 6 AWeaken 7)::
  (EA 7 (AAssign V_ideaCfbDecrypt__tmp
  (Some (ESub (EVar V_ideaCfbDecrypt__tmp)
  (EVar V_ideaCfbDecrypt_bufleft)))) 8)::(EA 8 ANone 9)::(EA 9 (AAssign
  V_ideaCfbDecrypt_bufleft (Some (EAdd (EVar V_ideaCfbDecrypt_bufleft)
  (ENum (-1))))) 10)::(EA 10 AWeaken 11)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_ideaCfbDecrypt_bufleft) s) <> (eval (ENum (0))
  s))%Z)) 46)::(EA 11 (AGuard
  (fun s => ((eval (EVar V_ideaCfbDecrypt_bufleft) s) = (eval (ENum (0))
  s))%Z)) 12)::(EA 12 AWeaken 13)::(EA 13 ANone 14)::(EA 14 AWeaken 15)::
  (EA 15 (AGuard (fun s => ((eval (EVar V_ideaCfbDecrypt__tmp) s) >
  (eval (ENum (8)) s))%Z)) 28)::(EA 15 (AGuard
  (fun s => ((eval (EVar V_ideaCfbDecrypt__tmp) s) <= (eval (ENum (8))
  s))%Z)) 16)::(EA 16 AWeaken 17)::(EA 17 ANone 18)::(EA 18 (AAssign
  V_ideaCfbDecrypt_t
  (Some (EVar V_ideaCfbDecrypt_ideaCfbDecrypt_dot_bufptr_dref))) 19)::
  (EA 19 (AAssign V_ideaCfbDecrypt_ideaCfbDecrypt_dot_bufptr_dref None) 20)::
  (EA 20 ANone 21)::(EA 21 (AAssign V_ideaCfbDecrypt__tmp
  (Some (EAdd (EVar V_ideaCfbDecrypt__tmp) (ENum (-1))))) 22)::
  (EA 22 AWeaken 23)::(EA 23 (AGuard
  (fun s => ((eval (EAdd (EVar V_ideaCfbDecrypt__tmp) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 25)::(EA 23 (AGuard
  (fun s => ((eval (EAdd (EVar V_ideaCfbDecrypt__tmp) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 24)::(EA 24 AWeaken 60)::(EA 25 AWeaken 26)::
  (EA 26 ANone 27)::(EA 27 (AAssign V_ideaCfbDecrypt_z (Some (EAdd (ENum (1))
  (EVar V_ideaCfbDecrypt_z)))) 18)::(EA 28 AWeaken 29)::(EA 29 (AAssign
  V_ideaCfbDecrypt_bufleft (Some (ENum (8)))) 30)::(EA 30 (AAssign
  V_ideaCfbDecrypt__tmp (Some (ESub (EVar V_ideaCfbDecrypt__tmp)
  (ENum (8))))) 31)::(EA 31 ANone 32)::(EA 32 (AAssign V_ideaCfbDecrypt_t
  (Some (EVar V_ideaCfbDecrypt_ideaCfbDecrypt_dot_bufptr_dref))) 33)::
  (EA 33 (AAssign V_ideaCfbDecrypt_ideaCfbDecrypt_dot_bufptr_dref None) 34)::
  (EA 34 ANone 35)::(EA 35 (AAssign V_ideaCfbDecrypt_bufleft
  (Some (EAdd (EVar V_ideaCfbDecrypt_bufleft) (ENum (-1))))) 36)::
  (EA 36 AWeaken 37)::(EA 37 (AGuard
  (fun s => ((eval (EAdd (EVar V_ideaCfbDecrypt_bufleft) (ENum (-1))) s) <>
  (eval (ENum (0)) s))%Z)) 43)::(EA 37 (AGuard
  (fun s => ((eval (EAdd (EVar V_ideaCfbDecrypt_bufleft) (ENum (-1))) s) =
  (eval (ENum (0)) s))%Z)) 38)::(EA 38 AWeaken 39)::(EA 39 ANone 40)::
  (EA 40 ANone 41)::(EA 41 (AAssign V_ideaCfbDecrypt_z (Some (EAdd (ENum (1))
  (EVar V_ideaCfbDecrypt_z)))) 42)::(EA 42 AWeaken 15)::(EA 43 AWeaken 44)::
  (EA 44 ANone 45)::(EA 45 (AAssign V_ideaCfbDecrypt_z (Some (EAdd (ENum (1))
  (EVar V_ideaCfbDecrypt_z)))) 32)::(EA 46 AWeaken 47)::(EA 47 (AAssign
  V_ideaCfbDecrypt_t
  (Some (EVar V_ideaCfbDecrypt_ideaCfbDecrypt_dot_bufptr_dref))) 48)::
  (EA 48 (AAssign V_ideaCfbDecrypt_ideaCfbDecrypt_dot_bufptr_dref None) 49)::
  (EA 49 ANone 50)::(EA 50 ANone 51)::(EA 51 (AAssign V_ideaCfbDecrypt_z
  (Some (EAdd (ENum (1)) (EVar V_ideaCfbDecrypt_z)))) 9)::
  (EA 52 AWeaken 53)::(EA 53 ANone 54)::(EA 54 (AAssign V_ideaCfbDecrypt__tmp
  (Some (EAdd (EVar V_ideaCfbDecrypt__tmp) (ENum (-1))))) 55)::
  (EA 55 AWeaken 56)::(EA 56 (AGuard
  (fun s => ((eval (EVar V_ideaCfbDecrypt__tmp) s) <> (eval (ENum (0))
  s))%Z)) 61)::(EA 56 (AGuard (fun s => ((eval (EVar V_ideaCfbDecrypt__tmp)
  s) = (eval (ENum (0)) s))%Z)) 57)::(EA 57 AWeaken 58)::(EA 58 ANone 59)::
  (EA 59 AWeaken 60)::(EA 61 AWeaken 62)::(EA 62 (AAssign V_ideaCfbDecrypt_t
  (Some (EVar V_ideaCfbDecrypt_ideaCfbDecrypt_dot_bufptr_dref))) 63)::
  (EA 63 (AAssign V_ideaCfbDecrypt_ideaCfbDecrypt_dot_bufptr_dref None) 64)::
  (EA 64 ANone 65)::(EA 65 ANone 66)::(EA 66 (AAssign V_ideaCfbDecrypt_z
  (Some (EAdd (ENum (1)) (EVar V_ideaCfbDecrypt_z)))) 54)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_ideaCfbDecrypt => Pedges_ideaCfbDecrypt
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_ideaCfbDecrypt => 60
     end)%positive;
  var_global := var_global
}.

Definition ai_ideaCfbDecrypt (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 3 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 4 => (1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 5 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 6 => (1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp+ 1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0)%Z
   | 7 => (-1 * s V_ideaCfbDecrypt__tmp+ 1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 8 => (1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 9 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 10 => (-1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 11 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 12 => (-1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0)%Z
   | 13 => (-1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 14 => (-1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0)%Z
   | 15 => (-1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 16 => (-1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp + -8 <= 0)%Z
   | 17 => (1 * s V_ideaCfbDecrypt__tmp + -8 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 18 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp + -8 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0)%Z
   | 19 => (1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp + -8 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 20 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp + -8 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0)%Z
   | 21 => (1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp + -8 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 22 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp + -7 <= 0)%Z
   | 23 => (1 * s V_ideaCfbDecrypt__tmp + -7 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 24 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 25 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp + -7 <= 0)%Z
   | 26 => (1 * s V_ideaCfbDecrypt__tmp + -7 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 27 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp + -7 <= 0)%Z
   | 28 => (1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 9 <= 0)%Z
   | 29 => (-1 * s V_ideaCfbDecrypt__tmp + 9 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0)%Z
   | 30 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 9 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -8 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft + 8 <= 0)%Z
   | 31 => (-1 * s V_ideaCfbDecrypt_bufleft + 8 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -8 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 32 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -8 <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 33 => (-1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -8 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 34 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -8 <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 35 => (-1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -8 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 36 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -7 <= 0)%Z
   | 37 => (1 * s V_ideaCfbDecrypt_bufleft + -7 <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 38 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0)%Z
   | 39 => (-1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 40 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0)%Z
   | 41 => (-1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 42 => (-1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z + 1 <= 0)%Z
   | 43 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -7 <= 0)%Z
   | 44 => (1 * s V_ideaCfbDecrypt_bufleft + -7 <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 45 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt_bufleft + -7 <= 0)%Z
   | 46 => (-1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 47 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 48 => (-1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 49 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 50 => (-1 * s V_ideaCfbDecrypt__tmp + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 51 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp + 1 <= 0)%Z
   | 52 => (1 * s V_ideaCfbDecrypt_z <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft <= 0)%Z
   | 53 => (1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 54 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft <= 0)%Z
   | 55 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0)%Z
   | 56 => (1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 57 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp <= 0)%Z
   | 58 => (-1 * s V_ideaCfbDecrypt__tmp <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 59 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp <= 0)%Z
   | 60 => (1 * s V_ideaCfbDecrypt__tmp + -1 <= 0 /\ -1 * s V_ideaCfbDecrypt_bufleft <= 0 /\ -1 * s V_ideaCfbDecrypt__tmp <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 61 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0)%Z
   | 62 => (1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 63 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0)%Z
   | 64 => (1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | 65 => (-1 * s V_ideaCfbDecrypt_z <= 0 /\ 1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0)%Z
   | 66 => (1 * s V_ideaCfbDecrypt__tmp+ -1 * s V_ideaCfbDecrypt_bufleft + 1 <= 0 /\ -1 * s V_ideaCfbDecrypt_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_ideaCfbDecrypt (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_ideaCfbDecrypt_count <= z)%Q
   | 2 => (s V_ideaCfbDecrypt_count - max0(-s V_ideaCfbDecrypt_z) <= z)%Q
   | 3 => (s V_ideaCfbDecrypt__tmp - max0(-s V_ideaCfbDecrypt_z) <= z)%Q
   | 4 => (s V_ideaCfbDecrypt__tmp - max0(-s V_ideaCfbDecrypt_z) <= z)%Q
   | 5 => (s V_ideaCfbDecrypt__tmp - max0(-s V_ideaCfbDecrypt_z) <= z)%Q
   | 6 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_ideaCfbDecrypt_z) (0))) (F_max0_ge_0 (-
                                                                    s V_ideaCfbDecrypt_z))]
     (s V_ideaCfbDecrypt__tmp - max0(-s V_ideaCfbDecrypt_z) <= z)%Q
   | 7 => (s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_z <= z)%Q
   | 8 => (s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
           + s V_ideaCfbDecrypt_z <= z)%Q
   | 9 => (s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
           + s V_ideaCfbDecrypt_z <= z)%Q
   | 10 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 11 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 12 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 13 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 14 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 15 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 16 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_ideaCfbDecrypt_bufleft)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ideaCfbDecrypt_bufleft) (0))) (F_max0_ge_0 (s V_ideaCfbDecrypt_bufleft));
      (*-0.375 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_ideaCfbDecrypt__tmp)) (F_check_ge (0) (0));
      (*-0.375 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ideaCfbDecrypt__tmp) (0))) (F_max0_ge_0 (s V_ideaCfbDecrypt__tmp));
      (*-0.375 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                     - s V_ideaCfbDecrypt__tmp)) (F_check_ge (0) (0))]
     ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
      + s V_ideaCfbDecrypt_z <= z)%Q
   | 17 => ((1 # 1) + (5 # 8) * s V_ideaCfbDecrypt__tmp
            + s V_ideaCfbDecrypt_z
            - (3 # 8) * max0(8 - s V_ideaCfbDecrypt__tmp) <= z)%Q
   | 18 => ((1 # 1) + (5 # 8) * s V_ideaCfbDecrypt__tmp
            + s V_ideaCfbDecrypt_z
            - (3 # 8) * max0(8 - s V_ideaCfbDecrypt__tmp) <= z)%Q
   | 19 => ((1 # 1) + (5 # 8) * s V_ideaCfbDecrypt__tmp
            + s V_ideaCfbDecrypt_z
            - (3 # 8) * max0(8 - s V_ideaCfbDecrypt__tmp) <= z)%Q
   | 20 => ((1 # 1) + (5 # 8) * s V_ideaCfbDecrypt__tmp
            + s V_ideaCfbDecrypt_z
            - (3 # 8) * max0(8 - s V_ideaCfbDecrypt__tmp) <= z)%Q
   | 21 => ((1 # 1) + (5 # 8) * s V_ideaCfbDecrypt__tmp
            + s V_ideaCfbDecrypt_z
            - (3 # 8) * max0(8 - s V_ideaCfbDecrypt__tmp) <= z)%Q
   | 22 => hints
     [(*-0.375 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (7
                                                                   - 
                                                                   s V_ideaCfbDecrypt__tmp) (0))) (F_max0_ge_0 (7
                                                                    - s V_ideaCfbDecrypt__tmp))]
     ((13 # 8) + (5 # 8) * s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_z
      - (3 # 8) * max0(7 - s V_ideaCfbDecrypt__tmp) <= z)%Q
   | 23 => (-(1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_z <= z)%Q
   | 24 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (-1 + s V_ideaCfbDecrypt__tmp) (-9
                                                                    + s V_ideaCfbDecrypt__tmp));
      (*-1 0*) F_max0_ge_0 (-9 + s V_ideaCfbDecrypt__tmp);
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                               + s V_ideaCfbDecrypt__tmp) (0))) (F_max0_ge_0 (-1
                                                                    + s V_ideaCfbDecrypt__tmp))]
     (-(1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_z <= z)%Q
   | 25 => hints
     [(*-0.375 0*) F_binom_monotonic 1 (F_max0_ge_arg (8
                                                       - s V_ideaCfbDecrypt__tmp)) (F_check_ge (8
                                                                    - s V_ideaCfbDecrypt__tmp) (0))]
     (-(1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_z <= z)%Q
   | 26 => ((2 # 1) + (5 # 8) * s V_ideaCfbDecrypt__tmp
            + s V_ideaCfbDecrypt_z
            - (3 # 8) * max0(8 - s V_ideaCfbDecrypt__tmp) <= z)%Q
   | 27 => ((2 # 1) + (5 # 8) * s V_ideaCfbDecrypt__tmp
            + s V_ideaCfbDecrypt_z
            - (3 # 8) * max0(8 - s V_ideaCfbDecrypt__tmp) <= z)%Q
   | 28 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_ideaCfbDecrypt_bufleft)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ideaCfbDecrypt_bufleft) (0))) (F_max0_ge_0 (s V_ideaCfbDecrypt_bufleft))]
     ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
      + s V_ideaCfbDecrypt_z <= z)%Q
   | 29 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_z <= z)%Q
   | 30 => (-(7 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 31 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 32 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 33 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 34 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 35 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 36 => ((2 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 37 => ((2 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 38 => ((2 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 39 => ((2 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 40 => ((2 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 41 => ((2 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 42 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 43 => ((2 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 44 => ((2 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 45 => ((2 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 46 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 47 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 48 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 49 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 50 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 51 => ((1 # 1) + s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft
            + s V_ideaCfbDecrypt_z <= z)%Q
   | 52 => hints
     [(*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_ideaCfbDecrypt_z) (0))) (F_max0_ge_0 (-
                                                                    s V_ideaCfbDecrypt_z));
      (*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-s V_ideaCfbDecrypt__tmp
                                                    + s V_ideaCfbDecrypt_bufleft)) (F_check_ge (-
                                                                    s V_ideaCfbDecrypt__tmp
                                                                    + s V_ideaCfbDecrypt_bufleft) (0))]
     (s V_ideaCfbDecrypt__tmp - max0(-s V_ideaCfbDecrypt_z) <= z)%Q
   | 53 => ((1 # 2) * s V_ideaCfbDecrypt__tmp
            + (1 # 2) * s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
            - (1 # 2) * max0(-s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | 54 => ((1 # 2) * s V_ideaCfbDecrypt__tmp
            + (1 # 2) * s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
            - (1 # 2) * max0(-s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | 55 => hints
     [(*0 0.5*) F_binom_monotonic 1 (F_max0_ge_arg (-s V_ideaCfbDecrypt__tmp
                                                    + s V_ideaCfbDecrypt_bufleft)) (F_check_ge (-
                                                                    s V_ideaCfbDecrypt__tmp
                                                                    + s V_ideaCfbDecrypt_bufleft) (0))]
     ((1 # 2) + (1 # 2) * s V_ideaCfbDecrypt__tmp
      + (1 # 2) * s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
      - (1 # 2) * max0(-1 - s V_ideaCfbDecrypt__tmp
                       + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | 56 => ((1 # 2) + s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
            - (1 # 2) * max0(-1 - s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft)
            - (1 # 2) * max0(-s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | 57 => ((1 # 2) + s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
            - (1 # 2) * max0(-1 - s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft)
            - (1 # 2) * max0(-s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | 58 => ((1 # 2) + s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
            - (1 # 2) * max0(-1 - s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft)
            - (1 # 2) * max0(-s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | 59 => hints
     [(*-1.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_ideaCfbDecrypt__tmp)) (F_check_ge (0) (0));
      (*-1.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_ideaCfbDecrypt__tmp) (0))) (F_max0_ge_0 (s V_ideaCfbDecrypt__tmp));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-s V_ideaCfbDecrypt__tmp
                                                                 + s V_ideaCfbDecrypt_bufleft) (0))) (F_max0_ge_0 (-
                                                                    s V_ideaCfbDecrypt__tmp
                                                                    + s V_ideaCfbDecrypt_bufleft));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_ge_0 (8
                                                     - s V_ideaCfbDecrypt__tmp)) (F_check_ge (0) (0));
      (*-0.125 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (8
                                                                   - 
                                                                   s V_ideaCfbDecrypt__tmp) (0))) (F_max0_ge_0 (8
                                                                    - s V_ideaCfbDecrypt__tmp));
      (*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                 - s V_ideaCfbDecrypt__tmp
                                                                 + s V_ideaCfbDecrypt_bufleft) (0))) (F_max0_ge_0 (-1
                                                                    - s V_ideaCfbDecrypt__tmp
                                                                    + s V_ideaCfbDecrypt_bufleft))]
     ((1 # 2) + s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
      - (1 # 2) * max0(-1 - s V_ideaCfbDecrypt__tmp
                       + s V_ideaCfbDecrypt_bufleft)
      - (1 # 2) * max0(-s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | 60 => (s V_ideaCfbDecrypt_z <= z)%Q
   | 61 => hints
     [(*-0.5 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (-1
                                                                 - s V_ideaCfbDecrypt__tmp
                                                                 + s V_ideaCfbDecrypt_bufleft) (0))) (F_max0_ge_0 (-1
                                                                    - s V_ideaCfbDecrypt__tmp
                                                                    + s V_ideaCfbDecrypt_bufleft))]
     ((1 # 2) + s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
      - (1 # 2) * max0(-1 - s V_ideaCfbDecrypt__tmp
                       + s V_ideaCfbDecrypt_bufleft)
      - (1 # 2) * max0(-s V_ideaCfbDecrypt__tmp + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | 62 => ((1 # 1) + (1 # 2) * s V_ideaCfbDecrypt__tmp
            + (1 # 2) * s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
            - (1 # 2) * max0(-s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | 63 => ((1 # 1) + (1 # 2) * s V_ideaCfbDecrypt__tmp
            + (1 # 2) * s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
            - (1 # 2) * max0(-s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | 64 => ((1 # 1) + (1 # 2) * s V_ideaCfbDecrypt__tmp
            + (1 # 2) * s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
            - (1 # 2) * max0(-s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | 65 => ((1 # 1) + (1 # 2) * s V_ideaCfbDecrypt__tmp
            + (1 # 2) * s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
            - (1 # 2) * max0(-s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | 66 => ((1 # 1) + (1 # 2) * s V_ideaCfbDecrypt__tmp
            + (1 # 2) * s V_ideaCfbDecrypt_bufleft + s V_ideaCfbDecrypt_z
            - (1 # 2) * max0(-s V_ideaCfbDecrypt__tmp
                             + s V_ideaCfbDecrypt_bufleft) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_ideaCfbDecrypt =>
    [mkPA Q (fun n z s => ai_ideaCfbDecrypt n s /\ annot0_ideaCfbDecrypt n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_ideaCfbDecrypt (proc_start P_ideaCfbDecrypt) s1 (proc_end P_ideaCfbDecrypt) s2 ->
    (s2 V_ideaCfbDecrypt_z <= s1 V_ideaCfbDecrypt_count)%Q.
Proof.
  prove_bound ipa admissible_ipa P_ideaCfbDecrypt.
Qed.
