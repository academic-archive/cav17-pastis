Require Import pasta.Pasta.

Inductive proc: Type :=
  P_BF_cfb64_encrypt.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_BF_cfb64_encrypt_z := 1%positive.
Notation V_BF_cfb64_encrypt__tmp := 2%positive.
Notation V_BF_cfb64_encrypt__tmp1 := 3%positive.
Notation V_BF_cfb64_encrypt_c := 4%positive.
Notation V_BF_cfb64_encrypt_cc := 5%positive.
Notation V_BF_cfb64_encrypt_l := 6%positive.
Notation V_BF_cfb64_encrypt_n := 7%positive.
Notation V_BF_cfb64_encrypt_num_dref := 8%positive.
Notation V_BF_cfb64_encrypt_t := 9%positive.
Notation V_BF_cfb64_encrypt_v0 := 10%positive.
Notation V_BF_cfb64_encrypt_v1 := 11%positive.
Notation V_BF_cfb64_encrypt_encrypt := 12%positive.
Notation V_BF_cfb64_encrypt_in := 13%positive.
Notation V_BF_cfb64_encrypt_ivec := 14%positive.
Notation V_BF_cfb64_encrypt_length := 15%positive.
Notation V_BF_cfb64_encrypt_num := 16%positive.
Notation V_BF_cfb64_encrypt_out := 17%positive.
Notation V_BF_cfb64_encrypt_schedule := 18%positive.
Definition Pedges_BF_cfb64_encrypt: list (edge proc) :=
  (EA 1 (AAssign V_BF_cfb64_encrypt_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_BF_cfb64_encrypt__tmp1 (Some (EVar V_BF_cfb64_encrypt_length))) 3)::
  (EA 3 (AAssign V_BF_cfb64_encrypt__tmp
  (Some (EVar V_BF_cfb64_encrypt_encrypt))) 4)::(EA 4 (AAssign
  V_BF_cfb64_encrypt_n (Some (EVar V_BF_cfb64_encrypt_num_dref))) 5)::
  (EA 5 (AAssign V_BF_cfb64_encrypt_l
  (Some (EVar V_BF_cfb64_encrypt__tmp1))) 6)::(EA 6 AWeaken 7)::(EA 7 (AGuard
  (fun s => ((eval (EVar V_BF_cfb64_encrypt__tmp) s) <> (eval (ENum (0))
  s))%Z)) 36)::(EA 7 (AGuard (fun s => ((eval (EVar V_BF_cfb64_encrypt__tmp)
  s) = (eval (ENum (0)) s))%Z)) 8)::(EA 8 AWeaken 9)::(EA 9 ANone 10)::
  (EA 10 (AAssign V_BF_cfb64_encrypt_l
  (Some (EAdd (EVar V_BF_cfb64_encrypt_l) (ENum (-1))))) 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_BF_cfb64_encrypt_l) s) <> (eval (ENum (0))
  s))%Z)) 15)::(EA 12 (AGuard (fun s => ((eval (EVar V_BF_cfb64_encrypt_l)
  s) = (eval (ENum (0)) s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 ANone 43)::
  (EA 15 AWeaken 16)::(EA 16 (AGuard
  (fun s => ((eval (EVar V_BF_cfb64_encrypt_n) s) = (eval (ENum (0))
  s))%Z)) 18)::(EA 16 (AGuard (fun s => ((eval (EVar V_BF_cfb64_encrypt_n)
  s) <> (eval (ENum (0)) s))%Z)) 17)::(EA 17 AWeaken 30)::
  (EA 18 AWeaken 19)::(EA 19 (AAssign V_BF_cfb64_encrypt_v0 None) 20)::
  (EA 20 (AAssign V_BF_cfb64_encrypt_v0 None) 21)::(EA 21 (AAssign
  V_BF_cfb64_encrypt_v0 None) 22)::(EA 22 (AAssign V_BF_cfb64_encrypt_v0
  None) 23)::(EA 23 (AAssign V_BF_cfb64_encrypt_v1 None) 24)::(EA 24 (AAssign
  V_BF_cfb64_encrypt_v1 None) 25)::(EA 25 (AAssign V_BF_cfb64_encrypt_v1
  None) 26)::(EA 26 (AAssign V_BF_cfb64_encrypt_v1 None) 27)::(EA 27 (AAssign
  V_BF_cfb64_encrypt_t None) 28)::(EA 28 (AAssign V_BF_cfb64_encrypt_t
  None) 29)::(EA 29 ANone 30)::(EA 30 (AAssign V_BF_cfb64_encrypt_cc
  None) 31)::(EA 31 (AAssign V_BF_cfb64_encrypt_c None) 32)::(EA 32 (AAssign
  V_BF_cfb64_encrypt_n None) 33)::(EA 33 ANone 34)::(EA 34 ANone 35)::
  (EA 35 (AAssign V_BF_cfb64_encrypt_z (Some (EAdd (ENum (1))
  (EVar V_BF_cfb64_encrypt_z)))) 10)::(EA 36 AWeaken 37)::(EA 37 ANone 38)::
  (EA 38 (AAssign V_BF_cfb64_encrypt_l
  (Some (EAdd (EVar V_BF_cfb64_encrypt_l) (ENum (-1))))) 39)::
  (EA 39 AWeaken 40)::(EA 40 (AGuard
  (fun s => ((eval (EVar V_BF_cfb64_encrypt_l) s) <> (eval (ENum (0))
  s))%Z)) 51)::(EA 40 (AGuard (fun s => ((eval (EVar V_BF_cfb64_encrypt_l)
  s) = (eval (ENum (0)) s))%Z)) 41)::(EA 41 AWeaken 42)::(EA 42 ANone 43)::
  (EA 43 (AAssign V_BF_cfb64_encrypt_cc (Some (ENum (0)))) 44)::
  (EA 44 (AAssign V_BF_cfb64_encrypt_c (Some (ENum (0)))) 45)::
  (EA 45 (AAssign V_BF_cfb64_encrypt_t (Some (ENum (0)))) 46)::
  (EA 46 (AAssign V_BF_cfb64_encrypt_v1 (Some (ENum (0)))) 47)::
  (EA 47 (AAssign V_BF_cfb64_encrypt_v0 (Some (ENum (0)))) 48)::
  (EA 48 (AAssign V_BF_cfb64_encrypt_num_dref
  (Some (EVar V_BF_cfb64_encrypt_n))) 49)::(EA 49 AWeaken 50)::
  (EA 51 AWeaken 52)::(EA 52 (AGuard
  (fun s => ((eval (EVar V_BF_cfb64_encrypt_n) s) = (eval (ENum (0))
  s))%Z)) 54)::(EA 52 (AGuard (fun s => ((eval (EVar V_BF_cfb64_encrypt_n)
  s) <> (eval (ENum (0)) s))%Z)) 53)::(EA 53 AWeaken 66)::
  (EA 54 AWeaken 55)::(EA 55 (AAssign V_BF_cfb64_encrypt_v0 None) 56)::
  (EA 56 (AAssign V_BF_cfb64_encrypt_v0 None) 57)::(EA 57 (AAssign
  V_BF_cfb64_encrypt_v0 None) 58)::(EA 58 (AAssign V_BF_cfb64_encrypt_v0
  None) 59)::(EA 59 (AAssign V_BF_cfb64_encrypt_v1 None) 60)::(EA 60 (AAssign
  V_BF_cfb64_encrypt_v1 None) 61)::(EA 61 (AAssign V_BF_cfb64_encrypt_v1
  None) 62)::(EA 62 (AAssign V_BF_cfb64_encrypt_v1 None) 63)::(EA 63 (AAssign
  V_BF_cfb64_encrypt_t None) 64)::(EA 64 (AAssign V_BF_cfb64_encrypt_t
  None) 65)::(EA 65 ANone 66)::(EA 66 (AAssign V_BF_cfb64_encrypt_c
  None) 67)::(EA 67 (AAssign V_BF_cfb64_encrypt_n None) 68)::
  (EA 68 ANone 69)::(EA 69 ANone 70)::(EA 70 (AAssign V_BF_cfb64_encrypt_z
  (Some (EAdd (ENum (1)) (EVar V_BF_cfb64_encrypt_z)))) 38)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_BF_cfb64_encrypt => Pedges_BF_cfb64_encrypt
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_BF_cfb64_encrypt => 50
     end)%positive;
  var_global := var_global
}.

Definition ai_BF_cfb64_encrypt (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 3 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 4 => (1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 5 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 6 => (1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 7 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 8 => (1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 9 => (-1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 10 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 11 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 12 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 13 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_l <= 0 /\ -1 * s V_BF_cfb64_encrypt_l <= 0)%Z
   | 14 => (-1 * s V_BF_cfb64_encrypt_l <= 0 /\ 1 * s V_BF_cfb64_encrypt_l <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 15 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 16 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 17 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 18 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_n <= 0)%Z
   | 19 => (-1 * s V_BF_cfb64_encrypt_n <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 20 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_n <= 0)%Z
   | 21 => (-1 * s V_BF_cfb64_encrypt_n <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 22 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_n <= 0)%Z
   | 23 => (-1 * s V_BF_cfb64_encrypt_n <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 24 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_n <= 0)%Z
   | 25 => (-1 * s V_BF_cfb64_encrypt_n <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 26 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_n <= 0)%Z
   | 27 => (-1 * s V_BF_cfb64_encrypt_n <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 28 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_n <= 0)%Z
   | 29 => (-1 * s V_BF_cfb64_encrypt_n <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 30 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 31 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 32 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 33 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 34 => (1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 35 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt__tmp <= 0 /\ 1 * s V_BF_cfb64_encrypt__tmp <= 0)%Z
   | 36 => (1 * s V_BF_cfb64_encrypt_z <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 37 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 38 => (-1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 39 => (-1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 40 => (-1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 41 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_l <= 0 /\ -1 * s V_BF_cfb64_encrypt_l <= 0)%Z
   | 42 => (-1 * s V_BF_cfb64_encrypt_l <= 0 /\ 1 * s V_BF_cfb64_encrypt_l <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 43 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_l <= 0 /\ -1 * s V_BF_cfb64_encrypt_l <= 0)%Z
   | 44 => (-1 * s V_BF_cfb64_encrypt_l <= 0 /\ 1 * s V_BF_cfb64_encrypt_l <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_cc <= 0 /\ -1 * s V_BF_cfb64_encrypt_cc <= 0)%Z
   | 45 => (-1 * s V_BF_cfb64_encrypt_cc <= 0 /\ 1 * s V_BF_cfb64_encrypt_cc <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_l <= 0 /\ -1 * s V_BF_cfb64_encrypt_l <= 0 /\ 1 * s V_BF_cfb64_encrypt_c <= 0 /\ -1 * s V_BF_cfb64_encrypt_c <= 0)%Z
   | 46 => (-1 * s V_BF_cfb64_encrypt_c <= 0 /\ 1 * s V_BF_cfb64_encrypt_c <= 0 /\ -1 * s V_BF_cfb64_encrypt_l <= 0 /\ 1 * s V_BF_cfb64_encrypt_l <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_cc <= 0 /\ -1 * s V_BF_cfb64_encrypt_cc <= 0 /\ 1 * s V_BF_cfb64_encrypt_t <= 0 /\ -1 * s V_BF_cfb64_encrypt_t <= 0)%Z
   | 47 => (-1 * s V_BF_cfb64_encrypt_t <= 0 /\ 1 * s V_BF_cfb64_encrypt_t <= 0 /\ -1 * s V_BF_cfb64_encrypt_cc <= 0 /\ 1 * s V_BF_cfb64_encrypt_cc <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_l <= 0 /\ -1 * s V_BF_cfb64_encrypt_l <= 0 /\ 1 * s V_BF_cfb64_encrypt_c <= 0 /\ -1 * s V_BF_cfb64_encrypt_c <= 0 /\ 1 * s V_BF_cfb64_encrypt_v1 <= 0 /\ -1 * s V_BF_cfb64_encrypt_v1 <= 0)%Z
   | 48 => (-1 * s V_BF_cfb64_encrypt_v1 <= 0 /\ 1 * s V_BF_cfb64_encrypt_v1 <= 0 /\ -1 * s V_BF_cfb64_encrypt_c <= 0 /\ 1 * s V_BF_cfb64_encrypt_c <= 0 /\ -1 * s V_BF_cfb64_encrypt_l <= 0 /\ 1 * s V_BF_cfb64_encrypt_l <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_cc <= 0 /\ -1 * s V_BF_cfb64_encrypt_cc <= 0 /\ 1 * s V_BF_cfb64_encrypt_t <= 0 /\ -1 * s V_BF_cfb64_encrypt_t <= 0 /\ 1 * s V_BF_cfb64_encrypt_v0 <= 0 /\ -1 * s V_BF_cfb64_encrypt_v0 <= 0)%Z
   | 49 => (-1 * s V_BF_cfb64_encrypt_v0 <= 0 /\ 1 * s V_BF_cfb64_encrypt_v0 <= 0 /\ -1 * s V_BF_cfb64_encrypt_t <= 0 /\ 1 * s V_BF_cfb64_encrypt_t <= 0 /\ -1 * s V_BF_cfb64_encrypt_cc <= 0 /\ 1 * s V_BF_cfb64_encrypt_cc <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_l <= 0 /\ -1 * s V_BF_cfb64_encrypt_l <= 0 /\ 1 * s V_BF_cfb64_encrypt_c <= 0 /\ -1 * s V_BF_cfb64_encrypt_c <= 0 /\ 1 * s V_BF_cfb64_encrypt_v1 <= 0 /\ -1 * s V_BF_cfb64_encrypt_v1 <= 0)%Z
   | 50 => (-1 * s V_BF_cfb64_encrypt_v1 <= 0 /\ 1 * s V_BF_cfb64_encrypt_v1 <= 0 /\ -1 * s V_BF_cfb64_encrypt_c <= 0 /\ 1 * s V_BF_cfb64_encrypt_c <= 0 /\ -1 * s V_BF_cfb64_encrypt_l <= 0 /\ 1 * s V_BF_cfb64_encrypt_l <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_cc <= 0 /\ -1 * s V_BF_cfb64_encrypt_cc <= 0 /\ 1 * s V_BF_cfb64_encrypt_t <= 0 /\ -1 * s V_BF_cfb64_encrypt_t <= 0 /\ 1 * s V_BF_cfb64_encrypt_v0 <= 0 /\ -1 * s V_BF_cfb64_encrypt_v0 <= 0)%Z
   | 51 => (-1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 52 => (-1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 53 => (-1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 54 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_n <= 0)%Z
   | 55 => (-1 * s V_BF_cfb64_encrypt_n <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 56 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_n <= 0)%Z
   | 57 => (-1 * s V_BF_cfb64_encrypt_n <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 58 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_n <= 0)%Z
   | 59 => (-1 * s V_BF_cfb64_encrypt_n <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 60 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_n <= 0)%Z
   | 61 => (-1 * s V_BF_cfb64_encrypt_n <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 62 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_n <= 0)%Z
   | 63 => (-1 * s V_BF_cfb64_encrypt_n <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 64 => (-1 * s V_BF_cfb64_encrypt_z <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_n <= 0)%Z
   | 65 => (-1 * s V_BF_cfb64_encrypt_n <= 0 /\ 1 * s V_BF_cfb64_encrypt_n <= 0 /\ -1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 66 => (-1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 67 => (-1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 68 => (-1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 69 => (-1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | 70 => (-1 * s V_BF_cfb64_encrypt_z <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_BF_cfb64_encrypt (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => (s V_BF_cfb64_encrypt_length <= z)%Q
   | 2 => (s V_BF_cfb64_encrypt_length + s V_BF_cfb64_encrypt_z <= z)%Q
   | 3 => (s V_BF_cfb64_encrypt__tmp1 + s V_BF_cfb64_encrypt_z <= z)%Q
   | 4 => (s V_BF_cfb64_encrypt__tmp1 + s V_BF_cfb64_encrypt_z <= z)%Q
   | 5 => (s V_BF_cfb64_encrypt__tmp1 + s V_BF_cfb64_encrypt_z <= z)%Q
   | 6 => (s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 7 => (s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 8 => (s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 9 => (s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 10 => (s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 11 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 12 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 13 => hints
     [(*0 1*) F_one;
      (*0 1*) F_binom_monotonic 1 (F_max0_ge_0 (s V_BF_cfb64_encrypt_l)) (F_check_ge (0) (0));
      (*0 1*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BF_cfb64_encrypt_l) (0))) (F_max0_ge_0 (s V_BF_cfb64_encrypt_l))]
     ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 14 => (s V_BF_cfb64_encrypt_z <= z)%Q
   | 15 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 16 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 17 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 18 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 19 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 20 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 21 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 22 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 23 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 24 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 25 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 26 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 27 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 28 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 29 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 30 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 31 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 32 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 33 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 34 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 35 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 36 => (s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 37 => (s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 38 => (s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 39 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 40 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 41 => hints
     [(*-1 0*) F_one;
      (*-1 0*) F_binom_monotonic 1 (F_max0_ge_0 (s V_BF_cfb64_encrypt_l)) (F_check_ge (0) (0));
      (*-1 0*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (s V_BF_cfb64_encrypt_l) (0))) (F_max0_ge_0 (s V_BF_cfb64_encrypt_l))]
     ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 42 => (s V_BF_cfb64_encrypt_z <= z)%Q
   | 43 => (s V_BF_cfb64_encrypt_z <= z)%Q
   | 44 => (s V_BF_cfb64_encrypt_z <= z)%Q
   | 45 => (s V_BF_cfb64_encrypt_z <= z)%Q
   | 46 => (s V_BF_cfb64_encrypt_z <= z)%Q
   | 47 => (s V_BF_cfb64_encrypt_z <= z)%Q
   | 48 => (s V_BF_cfb64_encrypt_z <= z)%Q
   | 49 => (s V_BF_cfb64_encrypt_z <= z)%Q
   | 50 => (s V_BF_cfb64_encrypt_z <= z)%Q
   | 51 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 52 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 53 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 54 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 55 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 56 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 57 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 58 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 59 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 60 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 61 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 62 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 63 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 64 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 65 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 66 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 67 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 68 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 69 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | 70 => ((1 # 1) + s V_BF_cfb64_encrypt_l + s V_BF_cfb64_encrypt_z <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_BF_cfb64_encrypt =>
    [mkPA Q (fun n z s => ai_BF_cfb64_encrypt n s /\ annot0_BF_cfb64_encrypt n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_BF_cfb64_encrypt (proc_start P_BF_cfb64_encrypt) s1 (proc_end P_BF_cfb64_encrypt) s2 ->
    (s2 V_BF_cfb64_encrypt_z <= s1 V_BF_cfb64_encrypt_length)%Q.
Proof.
  prove_bound ipa admissible_ipa P_BF_cfb64_encrypt.
Qed.
