Require Import pasta.Pasta.

Inductive proc: Type :=
  P_BF_set_key.

Definition var_global (v: id): bool :=
  match v with
  | _ => false
  end.

Notation V_BF_set_key_z := 1%positive.
Notation V_BF_set_key__tmp := 2%positive.
Notation V_BF_set_key_i := 3%positive.
Notation V_BF_set_key_ri := 4%positive.
Notation V_BF_set_key_data := 5%positive.
Notation V_BF_set_key_key := 6%positive.
Notation V_BF_set_key_len := 7%positive.
Definition Pedges_BF_set_key: list (edge proc) :=
  (EA 1 (AAssign V_BF_set_key_z (Some (ENum (0)))) 2)::(EA 2 (AAssign
  V_BF_set_key__tmp (Some (EVar V_BF_set_key_len))) 3)::(EA 3 AWeaken 4)::
  (EA 4 (AGuard (fun s => ((eval (EVar V_BF_set_key__tmp) s) >
  (eval (ENum (72)) s))%Z)) 6)::(EA 4 (AGuard
  (fun s => ((eval (EVar V_BF_set_key__tmp) s) <= (eval (ENum (72))
  s))%Z)) 5)::(EA 5 AWeaken 9)::(EA 6 AWeaken 7)::(EA 7 (AAssign
  V_BF_set_key__tmp (Some (ENum (72)))) 8)::(EA 8 ANone 9)::(EA 9 (AAssign
  V_BF_set_key_i (Some (ENum (0)))) 10)::(EA 10 ANone 11)::
  (EA 11 AWeaken 12)::(EA 12 (AGuard (fun s => ((eval (EVar V_BF_set_key_i)
  s) < (eval (ENum (18)) s))%Z)) 39)::(EA 12 (AGuard
  (fun s => ((eval (EVar V_BF_set_key_i) s) >= (eval (ENum (18))
  s))%Z)) 13)::(EA 13 AWeaken 14)::(EA 14 (AAssign V_BF_set_key_i
  (Some (ENum (0)))) 15)::(EA 15 ANone 16)::(EA 16 AWeaken 17)::
  (EA 17 (AGuard (fun s => ((eval (EVar V_BF_set_key_i) s) <
  (eval (ENum (18)) s))%Z)) 32)::(EA 17 (AGuard
  (fun s => ((eval (EVar V_BF_set_key_i) s) >= (eval (ENum (18))
  s))%Z)) 18)::(EA 18 AWeaken 19)::(EA 19 (AAssign V_BF_set_key_i
  (Some (ENum (0)))) 20)::(EA 20 ANone 21)::(EA 21 AWeaken 22)::
  (EA 22 (AGuard (fun s => ((eval (EVar V_BF_set_key_i) s) <
  (eval (ENum (1024)) s))%Z)) 25)::(EA 22 (AGuard
  (fun s => ((eval (EVar V_BF_set_key_i) s) >= (eval (ENum (1024))
  s))%Z)) 23)::(EA 23 AWeaken 24)::(EA 25 AWeaken 26)::(EA 26 ANone 27)::
  (EA 27 (AAssign V_BF_set_key_i (Some (EAdd (EVar V_BF_set_key_i)
  (ENum (2))))) 28)::(EA 28 ANone 29)::(EA 29 ANone 30)::(EA 30 (AAssign
  V_BF_set_key_z (Some (EAdd (ENum (1)) (EVar V_BF_set_key_z)))) 31)::
  (EA 31 AWeaken 22)::(EA 32 AWeaken 33)::(EA 33 ANone 34)::(EA 34 (AAssign
  V_BF_set_key_i (Some (EAdd (EVar V_BF_set_key_i) (ENum (2))))) 35)::
  (EA 35 ANone 36)::(EA 36 ANone 37)::(EA 37 (AAssign V_BF_set_key_z
  (Some (EAdd (ENum (1)) (EVar V_BF_set_key_z)))) 38)::(EA 38 AWeaken 17)::
  (EA 39 AWeaken 40)::(EA 40 (AAssign V_BF_set_key_ri None) 41)::
  (EA 41 AWeaken 42)::(EA 42 ANone 43)::(EA 42 ANone 44)::(EA 43 ANone 44)::
  (EA 44 (AAssign V_BF_set_key_ri None) 45)::(EA 45 (AAssign V_BF_set_key_ri
  None) 46)::(EA 46 AWeaken 47)::(EA 47 ANone 48)::(EA 47 ANone 49)::
  (EA 48 ANone 49)::(EA 49 (AAssign V_BF_set_key_ri None) 50)::
  (EA 50 (AAssign V_BF_set_key_ri None) 51)::(EA 51 AWeaken 52)::
  (EA 52 ANone 53)::(EA 52 ANone 54)::(EA 53 ANone 54)::(EA 54 (AAssign
  V_BF_set_key_ri None) 55)::(EA 55 (AAssign V_BF_set_key_ri None) 56)::
  (EA 56 AWeaken 57)::(EA 57 ANone 58)::(EA 57 ANone 59)::(EA 58 ANone 59)::
  (EA 59 ANone 60)::(EA 60 (AAssign V_BF_set_key_i
  (Some (EAdd (EVar V_BF_set_key_i) (ENum (1))))) 61)::(EA 61 ANone 62)::
  (EA 62 ANone 63)::(EA 63 (AAssign V_BF_set_key_z (Some (EAdd (ENum (1))
  (EVar V_BF_set_key_z)))) 64)::(EA 64 AWeaken 12)::nil.

Instance PROG: Program proc := {
  proc_edges := fun p =>
    match p with
    | P_BF_set_key => Pedges_BF_set_key
    end;
  proc_start := fun p => 1%positive;
  proc_end := fun p =>
    (match p with
     | P_BF_set_key => 24
     end)%positive;
  var_global := var_global
}.

Definition ai_BF_set_key (p: node) (s: state): Prop := 
  (match p with
   | 1 => (True)%Z
   | 2 => (1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_z <= 0)%Z
   | 3 => (-1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_z <= 0)%Z
   | 4 => (1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_z <= 0)%Z
   | 5 => (-1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 6 => (-1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key__tmp + 73 <= 0)%Z
   | 7 => (-1 * s V_BF_set_key__tmp + 73 <= 0 /\ 1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_z <= 0)%Z
   | 8 => (-1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key__tmp + 72 <= 0)%Z
   | 9 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ 1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_z <= 0)%Z
   | 10 => (-1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ 1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_i <= 0)%Z
   | 11 => (-1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ 1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_z <= 0)%Z
   | 12 => (-1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ 1 * s V_BF_set_key_i + -18 <= 0)%Z
   | 13 => (1 * s V_BF_set_key_i + -18 <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i + 18 <= 0)%Z
   | 14 => (-1 * s V_BF_set_key_i + 18 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ 1 * s V_BF_set_key_i + -18 <= 0)%Z
   | 15 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_i <= 0)%Z
   | 16 => (-1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 17 => (-1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ 1 * s V_BF_set_key_i + -19 <= 0)%Z
   | 18 => (1 * s V_BF_set_key_i + -19 <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i + 18 <= 0)%Z
   | 19 => (-1 * s V_BF_set_key_i + 18 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ 1 * s V_BF_set_key_i + -19 <= 0)%Z
   | 20 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_i <= 0)%Z
   | 21 => (-1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 22 => (-1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ 1 * s V_BF_set_key_i + -1025 <= 0)%Z
   | 23 => (1 * s V_BF_set_key_i + -1025 <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i + 1024 <= 0)%Z
   | 24 => (-1 * s V_BF_set_key_i + 1024 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ 1 * s V_BF_set_key_i + -1025 <= 0)%Z
   | 25 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -1023 <= 0)%Z
   | 26 => (1 * s V_BF_set_key_i + -1023 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 27 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -1023 <= 0)%Z
   | 28 => (-1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i + 2 <= 0 /\ 1 * s V_BF_set_key_i + -1025 <= 0)%Z
   | 29 => (1 * s V_BF_set_key_i + -1025 <= 0 /\ -1 * s V_BF_set_key_i + 2 <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_z <= 0)%Z
   | 30 => (-1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i + 2 <= 0 /\ 1 * s V_BF_set_key_i + -1025 <= 0)%Z
   | 31 => (1 * s V_BF_set_key_i + -1025 <= 0 /\ -1 * s V_BF_set_key_i + 2 <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_z + 1 <= 0)%Z
   | 32 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 33 => (1 * s V_BF_set_key_i + -17 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 34 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 35 => (-1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i + 2 <= 0 /\ 1 * s V_BF_set_key_i + -19 <= 0)%Z
   | 36 => (1 * s V_BF_set_key_i + -19 <= 0 /\ -1 * s V_BF_set_key_i + 2 <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_z <= 0)%Z
   | 37 => (-1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i + 2 <= 0 /\ 1 * s V_BF_set_key_i + -19 <= 0)%Z
   | 38 => (1 * s V_BF_set_key_i + -19 <= 0 /\ -1 * s V_BF_set_key_i + 2 <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_z + 1 <= 0)%Z
   | 39 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 40 => (1 * s V_BF_set_key_i + -17 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 41 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 42 => (1 * s V_BF_set_key_i + -17 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 43 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 44 => (1 * s V_BF_set_key_i + -17 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 45 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 46 => (1 * s V_BF_set_key_i + -17 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 47 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 48 => (1 * s V_BF_set_key_i + -17 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 49 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 50 => (1 * s V_BF_set_key_i + -17 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 51 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 52 => (1 * s V_BF_set_key_i + -17 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 53 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 54 => (1 * s V_BF_set_key_i + -17 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 55 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 56 => (1 * s V_BF_set_key_i + -17 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 57 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 58 => (1 * s V_BF_set_key_i + -17 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 59 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -17 <= 0)%Z
   | 60 => (1 * s V_BF_set_key_i + -17 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ -1 * s V_BF_set_key_i <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 61 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -18 <= 0 /\ -1 * s V_BF_set_key_i + 1 <= 0)%Z
   | 62 => (-1 * s V_BF_set_key_i + 1 <= 0 /\ 1 * s V_BF_set_key_i + -18 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0)%Z
   | 63 => (1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_z <= 0 /\ 1 * s V_BF_set_key_i + -18 <= 0 /\ -1 * s V_BF_set_key_i + 1 <= 0)%Z
   | 64 => (-1 * s V_BF_set_key_i + 1 <= 0 /\ 1 * s V_BF_set_key_i + -18 <= 0 /\ 1 * s V_BF_set_key__tmp + -72 <= 0 /\ -1 * s V_BF_set_key_z + 1 <= 0)%Z
   | _ => False
   end)%positive.

Definition annot0_BF_set_key (p: node) (z: Q) (s: state): Prop := 
  (match p with
   | 1 => ((540 # 1) <= z)%Q
   | 2 => ((540 # 1) + s V_BF_set_key_z <= z)%Q
   | 3 => ((540 # 1) + s V_BF_set_key_z <= z)%Q
   | 4 => ((540 # 1) + s V_BF_set_key_z <= z)%Q
   | 5 => ((540 # 1) + s V_BF_set_key_z <= z)%Q
   | 6 => ((540 # 1) + s V_BF_set_key_z <= z)%Q
   | 7 => ((540 # 1) + s V_BF_set_key_z <= z)%Q
   | 8 => ((540 # 1) + s V_BF_set_key_z <= z)%Q
   | 9 => ((540 # 1) + s V_BF_set_key_z <= z)%Q
   | 10 => ((522 # 1) + s V_BF_set_key_z + max0(18 - s V_BF_set_key_i) <= z)%Q
   | 11 => ((522 # 1) + s V_BF_set_key_z + max0(18 - s V_BF_set_key_i) <= z)%Q
   | 12 => ((522 # 1) + s V_BF_set_key_z + max0(18 - s V_BF_set_key_i) <= z)%Q
   | 13 => hints
     [(*-1 0*) F_max0_monotonic (F_check_ge (18 - s V_BF_set_key_i) (17
                                                                    - 
                                                                    s V_BF_set_key_i));
      (*-1 0*) F_max0_ge_0 (17 - s V_BF_set_key_i);
      (*0 7.36806*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (72
                                                                    - 
                                                                    s V_BF_set_key__tmp) (0))) (F_max0_ge_0 (72
                                                                    - s V_BF_set_key__tmp))]
     ((522 # 1) + s V_BF_set_key_z + max0(18 - s V_BF_set_key_i) <= z)%Q
   | 14 => (-(17 # 2) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 15 => (-(18 # 1) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + (1 # 2) * max0(19 - s V_BF_set_key_i)
            + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 16 => (-(18 # 1) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + (1 # 2) * max0(19 - s V_BF_set_key_i)
            + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 17 => (-(18 # 1) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + (1 # 2) * max0(19 - s V_BF_set_key_i)
            + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 18 => hints
     [(*-0.5 0*) F_max0_monotonic (F_check_ge (18 - s V_BF_set_key_i) (17
                                                                    - s V_BF_set_key_i));
      (*-0.5 0*) F_max0_ge_0 (17 - s V_BF_set_key_i);
      (*-0.5 0*) F_max0_monotonic (F_check_ge (19 - s V_BF_set_key_i) (18
                                                                    - s V_BF_set_key_i))]
     (-(18 # 1) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
      + (1 # 2) * max0(19 - s V_BF_set_key_i)
      + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 19 => (-(18 # 1) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 20 => (-(1061 # 2) + (921 # 125) * s V_BF_set_key__tmp
            + s V_BF_set_key_z + (921 # 125) * max0(72 - s V_BF_set_key__tmp)
            + (1 # 2) * max0(1025 - s V_BF_set_key_i) <= z)%Q
   | 21 => (-(1061 # 2) + (921 # 125) * s V_BF_set_key__tmp
            + s V_BF_set_key_z + (921 # 125) * max0(72 - s V_BF_set_key__tmp)
            + (1 # 2) * max0(1025 - s V_BF_set_key_i) <= z)%Q
   | 22 => (-(1061 # 2) + (921 # 125) * s V_BF_set_key__tmp
            + s V_BF_set_key_z + (921 # 125) * max0(72 - s V_BF_set_key__tmp)
            + (1 # 2) * max0(1025 - s V_BF_set_key_i) <= z)%Q
   | 23 => hints
     [(*-0.5 0*) F_max0_monotonic (F_check_ge (1025 - s V_BF_set_key_i) (1024
                                                                    - s V_BF_set_key_i));
      (*-0.5 0*) F_max0_ge_0 (1024 - s V_BF_set_key_i);
      (*-7.36806 0*) F_binom_monotonic 1 (F_max0_ge_arg (72
                                                         - s V_BF_set_key__tmp)) (F_check_ge (72
                                                                    - s V_BF_set_key__tmp) (0))]
     (-(1061 # 2) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
      + (921 # 125) * max0(72 - s V_BF_set_key__tmp)
      + (1 # 2) * max0(1025 - s V_BF_set_key_i) <= z)%Q
   | 24 => (s V_BF_set_key_z <= z)%Q
   | 25 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (1025 - s V_BF_set_key_i) (2)]
     (-(1061 # 2) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
      + (921 # 125) * max0(72 - s V_BF_set_key__tmp)
      + (1 # 2) * max0(1025 - s V_BF_set_key_i) <= z)%Q
   | 26 => (-(1059 # 2) + (921 # 125) * s V_BF_set_key__tmp
            + s V_BF_set_key_z + (921 # 125) * max0(72 - s V_BF_set_key__tmp)
            + (1 # 2) * max0(1023 - s V_BF_set_key_i) <= z)%Q
   | 27 => (-(1059 # 2) + (921 # 125) * s V_BF_set_key__tmp
            + s V_BF_set_key_z + (921 # 125) * max0(72 - s V_BF_set_key__tmp)
            + (1 # 2) * max0(1023 - s V_BF_set_key_i) <= z)%Q
   | 28 => (-(1059 # 2) + (921 # 125) * s V_BF_set_key__tmp
            + s V_BF_set_key_z + (921 # 125) * max0(72 - s V_BF_set_key__tmp)
            + (1 # 2) * max0(1025 - s V_BF_set_key_i) <= z)%Q
   | 29 => (-(1059 # 2) + (921 # 125) * s V_BF_set_key__tmp
            + s V_BF_set_key_z + (921 # 125) * max0(72 - s V_BF_set_key__tmp)
            + (1 # 2) * max0(1025 - s V_BF_set_key_i) <= z)%Q
   | 30 => (-(1059 # 2) + (921 # 125) * s V_BF_set_key__tmp
            + s V_BF_set_key_z + (921 # 125) * max0(72 - s V_BF_set_key__tmp)
            + (1 # 2) * max0(1025 - s V_BF_set_key_i) <= z)%Q
   | 31 => (-(1061 # 2) + (921 # 125) * s V_BF_set_key__tmp
            + s V_BF_set_key_z + (921 # 125) * max0(72 - s V_BF_set_key__tmp)
            + (1 # 2) * max0(1025 - s V_BF_set_key_i) <= z)%Q
   | 32 => hints
     [(*-0.5 0*) F_max0_pre_decrement 1 (19 - s V_BF_set_key_i) (2)]
     (-(18 # 1) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
      + (1 # 2) * max0(19 - s V_BF_set_key_i)
      + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 33 => (-(17 # 1) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + (1 # 2) * max0(17 - s V_BF_set_key_i)
            + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 34 => (-(17 # 1) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + (1 # 2) * max0(17 - s V_BF_set_key_i)
            + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 35 => (-(17 # 1) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + (1 # 2) * max0(19 - s V_BF_set_key_i)
            + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 36 => (-(17 # 1) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + (1 # 2) * max0(19 - s V_BF_set_key_i)
            + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 37 => (-(17 # 1) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + (1 # 2) * max0(19 - s V_BF_set_key_i)
            + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 38 => (-(18 # 1) + (921 # 125) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + (1 # 2) * max0(19 - s V_BF_set_key_i)
            + (921 # 125) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 39 => hints
     [(*0 7.25*) F_binom_monotonic 1 (F_max0_le_arg (F_check_ge (72
                                                                 - s V_BF_set_key__tmp) (0))) (F_max0_ge_0 (72
                                                                    - s V_BF_set_key__tmp))]
     ((522 # 1) + s V_BF_set_key_z + max0(18 - s V_BF_set_key_i) <= z)%Q
   | 40 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 41 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 42 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 43 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 44 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 45 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 46 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 47 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 48 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 49 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 50 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 51 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 52 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 53 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 54 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 55 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 56 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 57 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 58 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 59 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 60 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(18 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 61 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(19 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 62 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(19 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 63 => ((29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
            + max0(19 - s V_BF_set_key_i)
            + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | 64 => hints
     [(*-1 0*) F_max0_pre_decrement 1 (19 - s V_BF_set_key_i) (1);
      (*-7.25 0*) F_binom_monotonic 1 (F_max0_ge_arg (72
                                                      - s V_BF_set_key__tmp)) (F_check_ge (72
                                                                    - s V_BF_set_key__tmp) (0))]
     (-(1 # 1) + (29 # 4) * s V_BF_set_key__tmp + s V_BF_set_key_z
      + max0(19 - s V_BF_set_key_i)
      + (29 # 4) * max0(72 - s V_BF_set_key__tmp) <= z)%Q
   | _ => False
   end)%positive.

Definition ipa: IPA := fun p =>
  match p with
  | P_BF_set_key =>
    [mkPA Q (fun n z s => ai_BF_set_key n s /\ annot0_BF_set_key n z s)]
  end.

Theorem admissible_ipa: IPA_VC ipa.
Proof.
  prove_ipa_vc.
Qed.

Theorem bound_valid:
  forall s1 s2, steps P_BF_set_key (proc_start P_BF_set_key) s1 (proc_end P_BF_set_key) s2 ->
    (s2 V_BF_set_key_z <= (540 # 1))%Q.
Proof.
  prove_bound ipa admissible_ipa P_BF_set_key.
Qed.
